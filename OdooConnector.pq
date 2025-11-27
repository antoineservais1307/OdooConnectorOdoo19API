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

        GetModelFields = (ModelName as text) =>
            let
                Body = [
                    context = [lang = "en_US"],
                    domain = {{"model","=", ModelName}},
                    fields = {"name","field_description","ttype","required","readonly","relation"}
                ],
                Json = DoPost("/ir.model.fields/search_read", Body),
                FieldsTable = Table.FromList(Json, Splitter.SplitByNothing(), null, null, ExtraValues.Error),
                ExpandedFields = Table.ExpandRecordColumn(FieldsTable, "Column1", {"name","field_description","ttype","required","readonly","relation"})
            in
                ExpandedFields,

        // Récupérer les données d'un modèle avec expansion automatique des relations
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

                // Conversion initiale en table
                InitialTable = if List.Count(Json) > 0 then
                    let 
                        tbl = Table.FromList(Json, Splitter.SplitByNothing(), null, null, ExtraValues.Error),
                        firstFields = Record.FieldNames(Json{0}),
                        expanded = Table.ExpandRecordColumn(tbl, "Column1", firstFields)
                    in expanded
                else
                    #table({}, {}),

                // Récupérer les métadonnées des champs pour identifier les relations
                FieldsMeta = try GetModelFields(ModelName) otherwise #table({"name","ttype","relation"}, {}),
                
                // Identifier les colonnes relationnelles
                RelationalColumns = if Table.RowCount(InitialTable) > 0 then
                    let
                        AllColumns = Table.ColumnNames(InitialTable),
                        RelCols = List.Transform(AllColumns, (colName) =>
                            let
                                // Vérifier le type de données dans la première ligne
                                SampleValue = try Table.Column(InitialTable, colName){0} otherwise null,
                                
                                // Récupérer le type de champ depuis les métadonnées
                                FieldInfo = try Table.SelectRows(FieldsMeta, each [name] = colName){0} otherwise null,
                                FieldType = if FieldInfo <> null then try FieldInfo[ttype] otherwise null else null,
                                RelationModel = if FieldInfo <> null then try FieldInfo[relation] otherwise null else null,
                                
                                // Déterminer si c'est une relation
                                IsManyToOne = FieldType = "many2one" and Value.Is(SampleValue, type list) and List.Count(SampleValue) = 2,
                                IsManyToMany = (FieldType = "many2many" or FieldType = "one2many") and Value.Is(SampleValue, type list),
                                
                                Result = if IsManyToOne then 
                                    [Column = colName, Type = "many2one", Model = RelationModel]
                                else if IsManyToMany then 
                                    [Column = colName, Type = "many2many", Model = RelationModel]
                                else 
                                    null
                            in
                                Result
                        )
                    in
                        List.RemoveNulls(RelCols)
                else
                    {},

                // Traiter les colonnes many2one (simple expansion)
                TableWithMany2One = List.Accumulate(
                    List.Select(RelationalColumns, each [Type] = "many2one"),
                    InitialTable,
                    (state, relCol) =>
                        let
                            colName = relCol[Column],
                            // Extraire l'ID et le nom
                            AddedId = Table.AddColumn(state, colName & ".id", each 
                                if Value.Is(Record.Field(_, colName), type list) and List.Count(Record.Field(_, colName)) = 2 
                                then List.First(Record.Field(_, colName)) 
                                else null, 
                                Int64.Type),
                            AddedName = Table.AddColumn(AddedId, colName & ".name", each 
                                if Value.Is(Record.Field(_, colName), type list) and List.Count(Record.Field(_, colName)) = 2 
                                then List.Last(Record.Field(_, colName)) 
                                else null,
                                type text),
                            Removed = Table.RemoveColumns(AddedName, {colName})
                        in
                            Removed
                ),

                // Traiter les colonnes many2many / one2many (conversion en texte)
                TableWithMany2Many = List.Accumulate(
                    List.Select(RelationalColumns, each [Type] = "many2many"),
                    TableWithMany2One,
                    (state, relCol) =>
                        let
                            colName = relCol[Column],
                            // Convertir la liste d'IDs en texte
                            Transformed = Table.TransformColumns(state, {{colName, each 
                                if Value.Is(_, type list) and List.Count(_) > 0 then
                                    Text.Combine(List.Transform(_, Number.ToText), ", ")
                                else if Value.Is(_, type list) and List.IsEmpty(_) then
                                    null
                                else
                                    _,
                                type text}})
                        in
                            Transformed
                ),

                // Nettoyer les autres valeurs (records, etc.)
                FinalTable = Table.TransformColumns(
                    TableWithMany2Many,
                    List.Transform(Table.ColumnNames(TableWithMany2Many), (col) => 
                        {col, each 
                            if Value.Is(_, type record) then 
                                null
                            else if Value.Is(_, type list) and List.IsEmpty(_) then
                                null
                            else
                                _
                        }
                    )
                )
            in
                FinalTable,

        // --- Comportement : si ModelName est fourni -> retourner directement la table du modèle ---
        ResultDirect = if ModelName <> null and Text.Trim(ModelName) <> "" then
            GetModelData(Text.Trim(ModelName))
        else
            // Table de navigation
            let
                ModelsJson = GetModels(),
                ModelsTable = Table.FromList(ModelsJson, Splitter.SplitByNothing(), null, null, ExtraValues.Error),
                ExpandedModels = Table.ExpandRecordColumn(ModelsTable, "Column1", {"model","name"}),
                
                AddedData = Table.AddColumn(ExpandedModels, "Data", each () => GetModelData([model]), type function),
                AddedFields = Table.AddColumn(AddedData, "Fields", each () => GetModelFields([model]), type function),

                Renamed = Table.RenameColumns(AddedFields, {{"model","Model"}, {"name","Description"}}),
                Typed = Table.TransformColumnTypes(Renamed, {{"Model", type text}, {"Description", type text}}),

                NavTable = Table.ToNavigationTable(Typed, {"Model"}, "Model", "Data", "Description", "Data")
            in
                NavTable
    in
        ResultDirect;

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

shared Odoo.Model = (BaseUrl as text, Database as text, ModelName as text, optional Fields as nullable list, optional Domain as nullable list, optional Limit as nullable number, optional Offset as nullable number) as table =>
    OdooConnector.Contents(BaseUrl, Database, ModelName);

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