section OdooConnector;

[DataSource.Kind="OdooConnector", Publish="OdooConnector.Publish"]
shared OdooConnector.Contents = (BaseUrl as text, Database as text, optional ModelName as nullable text) as table =>
    let
        // --- nettoyage url et api ---
        CleanUrl = Text.TrimEnd(BaseUrl, "/"),
        ApiUrl = CleanUrl & "/json/2",

        // --- credentials ---
        Credential = Extension.CurrentCredential(),
        ApiKey = if Record.HasFields(Credential, "Key") then Credential[Key] else (error Error.Record("Credential.Missing", "Clé API introuvable", "Vérifiez les paramètres d'authentification")),

        // --- fonctions internes ---
        // helper pour requêtes POST JSON avec gestion d'erreurs basique
        DoPost = (relativePath as text, bodyRecord as record) =>
            let
                Headers = [
                    Authorization = "bearer " & ApiKey,
                    #"X-Odoo-Database" = Database,
                    #"Content-Type" = "application/json"
                ],
                Body = Json.FromValue(bodyRecord),
                Response = Web.Contents(
                    ApiUrl & relativePath,
                    [
                        Headers = Headers,
                        Content = Body,
                        ManualStatusHandling = {400,401,403,404,500}
                    ]
                ),
                Status = try Value.Metadata(Response)[Response.Status] otherwise 200,
                JsonResp = if Status = 200 then Json.Document(Response) else error Error.Record("DataSource.Error", "Erreur API Odoo (Code: " & Number.ToText(Status) & ")", Text.From(Status))
            in
                JsonResp,

        // Récupérer la liste des modèles (sans leurs données)
        GetModels = () =>
            let
                Body = [
                    context = [lang = "en_US"],
                    domain = {},
                    fields = {"model","name","info"},
                    limit = 10000
                ],
                Json = DoPost("/ir.model/search_read", Body)
            in
                Json,

        // Récupérer les champs d'un modèle
        GetModelFields = (ModelName as text) =>
            let
                Body = [
                    context = [lang = "en_US"],
                    domain = {{"model","=", ModelName}},
                    fields = {"name","field_description","ttype","required","readonly"}
                ],
                Json = DoPost("/ir.model.fields/search_read", Body),
                FieldsTable = Table.FromList(Json, Splitter.SplitByNothing(), null, null, ExtraValues.Error),
                ExpandedFields = Table.ExpandRecordColumn(FieldsTable, "Column1", {"name","field_description","ttype","required","readonly"})
            in
                ExpandedFields,

        // Récupérer les données d'un modèle (support basique limit + offset)
        GetModelData = (ModelName as text, optional Fields as nullable list, optional Domain as nullable list, optional Limit as nullable number, optional Offset as nullable number) as table =>
            let
                ActualFields = if Fields = null then {} else Fields,
                ActualDomain = if Domain = null then {} else Domain,
                ActualLimit = if Limit = null then null else Limit,
                ActualOffset = if Offset = null then null else Offset,
                BaseBody = [
                    context = [lang = "en_US"],
                    domain = ActualDomain,
                    fields = ActualFields
                ],
                Body = if ActualLimit <> null and ActualOffset <> null then Record.AddField(Record.AddField(BaseBody, "limit", ActualLimit), "offset", ActualOffset)
                       else if ActualLimit <> null then Record.AddField(BaseBody, "limit", ActualLimit)
                       else if ActualOffset <> null then Record.AddField(BaseBody, "offset", ActualOffset)
                       else BaseBody,
                Json = DoPost("/" & ModelName & "/search_read", Body),

                // traitement: convertir [id, name] -> name (ou null), laisser autres valeurs inchangées
                ProcessedData = List.Transform(Json, each
                    let
                        rec = _,
                        fields = Record.FieldNames(rec),
                        proc = Record.TransformFields(rec,
                            List.Transform(fields, (f) => {f, (v) =>
                                if Value.Is(v, type list) and List.Count(v) = 2 and Value.Is(List.First(v), type number) then
                                    List.Last(v)
                                else if Value.Is(v, type list) and List.IsEmpty(v) then
                                    null
                                else
                                    v
                            })
                        )
                    in
                        proc
                ),
                TableFromList = if List.Count(ProcessedData) > 0 then
                    let tbl = Table.FromList(ProcessedData, Splitter.SplitByNothing(), null, null, ExtraValues.Error),
                        firstFields = Record.FieldNames(ProcessedData{0}),
                        expanded = Table.ExpandRecordColumn(tbl, "Column1", firstFields)
                    in expanded
                else
                    #table({}, {})
            in
                TableFromList,

        // --- Comportement : si ModelName est fourni -> retourner directement la table du modèle ---
        ResultDirect = if ModelName <> null and Text.Trim(ModelName) <> "" then
            let
                // on récupère uniquement les données demandées
                DataTable = GetModelData(Text.Trim(ModelName))
            in
                DataTable
        else
            // sinon on retourne la table de navigation (modèles) avec thunks pour la Data (différée)
            let
                ModelsJson = GetModels(),
                ModelsTable = Table.FromList(ModelsJson, Splitter.SplitByNothing(), null, null, ExtraValues.Error),
                ExpandedModels = Table.ExpandRecordColumn(ModelsTable, "Column1", {"model","name"}),
                
                // Data et Fields doivent être des thunks pour éviter le chargement immédiat
                AddedData = Table.AddColumn(ExpandedModels, "Data", each () => GetModelData([model]), type function),
                AddedFields = Table.AddColumn(AddedData, "Fields", each () => GetModelFields([model]), type function),

                Renamed = Table.RenameColumns(AddedFields, {{"model","Model"}, {"name","Description"}}),
                Typed = Table.TransformColumnTypes(Renamed, {{"Model", type text}, {"Description", type text}}),

                // marquer la table comme NavigationTable (Power BI)
                NavTable = Table.ToNavigationTable(Typed, {"Model"}, "Model", "Data", "Description", "Data")
            in
                NavTable
    in
        ResultDirect;

// Fonction helper : transformer une table en table de navigation (laisse inchangé ton impl)
Table.ToNavigationTable = (
    table as table,
    keyColumns as list,
    nameColumn as text,
    dataColumn as text,
    itemKindColumn as text,
    itemNameColumn as text
) as table =>
    let
        tableType = Value.Type(table),
        newTableType = Type.AddTableKey(tableType, keyColumns, true) meta [
            NavigationTable.NameColumn = nameColumn,
            NavigationTable.DataColumn = dataColumn,
            NavigationTable.ItemKindColumn = itemKindColumn,
            Preview.DelayColumn = itemNameColumn
        ],
        navigationTable = Value.ReplaceType(table, newTableType)
    in
        navigationTable;

// Exposer une fonction utilitaire pour charger directement un modèle (pratique)
shared Odoo.Model = (BaseUrl as text, Database as text, ModelName as text, optional Fields as nullable list, optional Domain as nullable list, optional Limit as nullable number, optional Offset as nullable number) as table =>
    let
        // appel direct du connecteur principal pour bénéficier de la même logique d'auth
        impl = OdooConnector.Contents(BaseUrl, Database),
        // si impl est une table de navigation on appelle GetModelData en recréant la request (plus simple)
        // mais pour robustesse, on duplique logiquement l'appel (on peut aussi factoriser)
        CleanUrl = Text.TrimEnd(BaseUrl, "/"),
        ApiUrl = CleanUrl & "/json/2",
        Credential = Extension.CurrentCredential(),
        ApiKey = if Record.HasFields(Credential, "Key") then Credential[Key] else (error Error.Record("Credential.Missing", "Clé API introuvable", "Vérifiez les paramètres d'authentification")),
        // DoPost minimal (duplication légère pour isolation)
        DoPostLocal = (relativePath as text, bodyRecord as record) =>
            let
                Headers = [
                    Authorization = "bearer " & ApiKey,
                    #"X-Odoo-Database" = Database,
                    #"Content-Type" = "application/json"
                ],
                Body = Json.FromValue(bodyRecord),
                Response = Web.Contents(
                    ApiUrl & relativePath,
                    [
                        Headers = Headers,
                        Content = Body,
                        ManualStatusHandling = {400,401,403,404,500}
                    ]
                ),
                Status = try Value.Metadata(Response)[Response.Status] otherwise 200,
                JsonResp = if Status = 200 then Json.Document(Response) else error Error.Record("DataSource.Error", "Erreur API Odoo (Code: " & Number.ToText(Status) & ")", Text.From(Status))
            in
                JsonResp,
        Body = [
            context = [lang = "en_US"],
            domain = if Domain = null then {} else Domain,
            fields = if Fields = null then {} else Fields
        ],
        BodyWithLimit = if Limit <> null and Offset <> null then Record.AddField(Record.AddField(Body, "limit", Limit), "offset", Offset)
                        else if Limit <> null then Record.AddField(Body, "limit", Limit)
                        else if Offset <> null then Record.AddField(Body, "offset", Offset)
                        else Body,
        Json = DoPostLocal("/" & Text.Trim(ModelName) & "/search_read", BodyWithLimit),

        // post-process identical to main impl
        ProcessedData = List.Transform(Json, each
            let
                rec = _,
                fields = Record.FieldNames(rec),
                proc = Record.TransformFields(rec,
                    List.Transform(fields, (f) => {f, (v) =>
                        if Value.Is(v, type list) and List.Count(v) = 2 and Value.Is(List.First(v), type number) then
                            List.Last(v)
                        else if Value.Is(v, type list) and List.IsEmpty(v) then
                            null
                        else
                            v
                    })
                )
            in
                proc
        ),
        TableFromList = if List.Count(ProcessedData) > 0 then
            let tbl = Table.FromList(ProcessedData, Splitter.SplitByNothing(), null, null, ExtraValues.Error),
                firstFields = Record.FieldNames(ProcessedData{0}),
                expanded = Table.ExpandRecordColumn(tbl, "Column1", firstFields)
            in expanded
        else
            #table({}, {})
    in
        TableFromList;

// Configuration de l'authentification
OdooConnector = [
    TestConnection = (dataSourcePath) =>
        let
            json = Json.Document(dataSourcePath),
            BaseUrl = json[BaseUrl],
            Database = json[Database]
        in
            {"OdooConnector.Contents", BaseUrl, Database},
    Authentication = [
        Key = [
            KeyLabel = "Clé API Odoo",
            Label = "Clé API"
        ]
    ],
    Label = "Odoo Connector"
];

// Configuration de publication (restée identique)
OdooConnector.Publish = [
    Beta = false,
    Category = "Online Services",
    ButtonText = {"Odoo Connector", "Se connecter à Odoo"},
    LearnMoreUrl = "https://www.odoo.com/documentation/",
    SourceImage = OdooConnector.Icons,
    SourceTypeImage = OdooConnector.Icons
];

OdooConnector.Icons = [
    Icon16 = {
        Extension.Contents("Odoo16.png"),
        Extension.Contents("Odoo20.png"),
        Extension.Contents("Odoo24.png"),
        Extension.Contents("Odoo32.png")
    },
    Icon32 = {
        Extension.Contents("Odoo32.png"),
        Extension.Contents("Odoo40.png"),
        Extension.Contents("Odoo48.png"),
        Extension.Contents("Odoo64.png")
    }
];
