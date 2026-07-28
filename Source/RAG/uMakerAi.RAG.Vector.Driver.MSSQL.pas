unit uMakerAi.RAG.Vector.Driver.MSSQL;

{
  TAiRAGVectorMSSQLDriver — Driver Microsoft SQL Server para TAiRAGVector (MakerAI 3.4)

  Requisitos del servidor:
  - SQL Server 2025 (17.x) o superior, Azure SQL Database, Azure SQL Managed
    Instance (política Always-up-to-date) o SQL database in Fabric.
    El tipo VECTOR y la función VECTOR_DISTANCE son nativos desde esas versiones.
  - Full-Text Search: componente opcional en instalaciones on-premise (siempre
    disponible en Azure SQL Database). Si no está instalado o la tabla no tiene
    índice full-text, el driver degrada automáticamente a búsqueda solo-vectorial.

  Características:
  - Búsqueda vectorial exacta (kNN) con VECTOR_DISTANCE('cosine', ...) — GA.
    Recomendada por Microsoft hasta ~50K vectores por entidad filtrada.
  - Búsqueda léxical BM25 real: FREETEXTTABLE rankea con la fórmula Okapi BM25.
    El RANK (entero 0-1000, no normalizado) se normaliza a [0,1] dividiendo
    por el máximo del conjunto de resultados.
  - Búsqueda híbrida en un solo query T-SQL (CTEs + FULL OUTER JOIN) con
    fusión RRF (K=60) o ponderada — misma estructura que el driver Postgres.
  - Filtrado de metadatos vía JSON_VALUE() con parámetros FireDAC.
  - Upsert con UPDATE + INSERT condicional (T-SQL no tiene ON CONFLICT).

  Esquema generado por CreateSchema(tabla, dim):
    rowid BIGINT IDENTITY   — clave del índice full-text (FTS exige un índice
                              único de una sola columna; la PK compuesta no sirve)
    entidad + id            — PK lógica compuesta (partición multi-tenant)
    embedding VECTOR(dim)   — máx 1998 dimensiones (límite SQL Server 2025)
    properties NVARCHAR(MAX)— metadata JSON (filtrable con JSON_VALUE)
    + FULLTEXT CATALOG makerai_ftcat y FULLTEXT INDEX sobre content

  Nota DiskANN: CREATE VECTOR INDEX / VECTOR_SEARCH siguen en preview en
  SQL Server 2025; este driver usa kNN exacto (GA). Cuando el índice DiskANN
  sea GA podrá agregarse como opción sin cambiar el contrato del driver.

  Transporte del embedding: viaja como JSON '[0.1,0.2,...]' en NVARCHAR.
  SQL Server expone el tipo VECTOR como varchar(max) a clientes sin TDS
  extendido (caso FireDAC/ODBC), y CAST(nvarchar AS VECTOR(n)) convierte
  en el servidor. El helper EmbeddingToString de la clase base ya genera
  exactamente ese formato.
}

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Variants,
  System.StrUtils,
  System.Math,
  System.Generics.Collections,
  Data.DB,
  FireDAC.Comp.Client,
  FireDAC.Stan.Param,
  FireDAC.DApt,
  uMakerAi.Embeddings.Core,
  uMakerAi.RAG.Vectors,
  uMakerAi.RAG.Vectors.Index,
  uMakerAi.RAG.MetaData;

type
  // ---------------------------------------------------------------------------
  // TMSSQLFilterBuilder — WHERE clause desde TAiFilterCriteria para SQL Server
  // Usa JSON_VALUE() sobre la columna properties, con parámetros FireDAC.
  // Valores numéricos se comparan con TRY_CAST(... AS FLOAT) porque JSON_VALUE
  // siempre devuelve NVARCHAR.
  // ---------------------------------------------------------------------------
  TMSSQLFilterBuilder = class
  private
    FParamCounter: Integer;
    FParamNames: TList<string>;
    FParamValues: TList<Variant>;
    function BuildPath(const AKey: string): string;
    function JsonPathOf(const AKey: string): string;
    function IsNumericValue(const V: Variant): Boolean;
    function Process(ACriteria: TAiFilterCriteria): string;
  public
    constructor Create;
    destructor Destroy; override;
    function BuildSQL(ACriteria: TAiFilterCriteria): string;
    procedure ApplyParams(AQuery: TFDQuery);
  end;

  // ---------------------------------------------------------------------------
  // TAiRAGVectorMSSQLDriver
  // ---------------------------------------------------------------------------
  TAiRAGVectorMSSQLDriver = class(TAiVectorStoreDriverBase)
  private
    FConnection: TFDConnection;
    FTableName: string;
    FCurrentEntidad: string;
    FLanguage: TAiLanguage;
    FLastSQL: string;
    FFtsAvailable: Boolean;
    FFtsCheckedFor: string;

    function NewQuery: TFDQuery;
    function PropertiesToJSON(AMeta: TAiEmbeddingMetaData): string;
    function GetLcid: Integer;
    function IsFullTextReady: Boolean;
    procedure SetTableName(const Value: string);
    procedure SetConnection(const Value: TFDConnection);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;

    { Contrato TAiVectorStoreDriverBase }
    procedure Add(const ANode: TAiEmbeddingNode; const AEntidad: string = ''); override;
    function Search(const ATarget: TAiEmbeddingNode; const AEntidad: string; ALimit: Integer; APrecision: Double; AFilter: TAiFilterCriteria;
      Options: TAiSearchOptions): TAiRAGVector; override;
    procedure Delete(const AID: string; const AEntidad: string); override;
    procedure Clear(const AEntidad: string); override;

    { Gestión del esquema }
    procedure CreateSchema(const ABaseTableName: string; ADim: Integer);
    procedure DropSchema(const ABaseTableName: string);

    { Helpers }
    function StringToEmbedding(const AStr: string): TAiEmbeddingData;

    property LastSQL: string read FLastSQL;
    // True si el servidor tiene Full-Text Search y la tabla tiene índice FTS
    property FullTextAvailable: Boolean read FFtsAvailable;
  published
    property Connection: TFDConnection read FConnection write SetConnection;
    property TableName: string read FTableName write SetTableName;
    property CurrentEntidad: string read FCurrentEntidad write FCurrentEntidad;
    property Language: TAiLanguage read FLanguage write FLanguage default alSpanish;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI.RAG.Drivers', [TAiRAGVectorMSSQLDriver]);
end;

// =============================================================================
// TMSSQLFilterBuilder
// =============================================================================

constructor TMSSQLFilterBuilder.Create;
begin
  inherited Create;
  FParamCounter := 0;
  FParamNames := TList<string>.Create;
  FParamValues := TList<Variant>.Create;
end;

destructor TMSSQLFilterBuilder.Destroy;
begin
  FParamNames.Free;
  FParamValues.Free;
  inherited;
end;

function TMSSQLFilterBuilder.JsonPathOf(const AKey: string): string;
begin
  // 'user.name' → '$.user.name' ; 'category' → '$.category'
  Result := '$.' + AKey;
end;

function TMSSQLFilterBuilder.BuildPath(const AKey: string): string;
begin
  // Campos virtuales / columnas directas
  if SameText(AKey, 'text') or SameText(AKey, 'content') then
    Exit('content');
  if SameText(AKey, 'model') then
    Exit('model');
  if SameText(AKey, 'id') then
    Exit('id');

  // JSON_VALUE soporta rutas con puntos de forma nativa: '$.user.name'
  Result := Format('JSON_VALUE(properties, ''%s'')', [JsonPathOf(AKey)]);
end;

function TMSSQLFilterBuilder.IsNumericValue(const V: Variant): Boolean;
begin
  case VarType(V) and varTypeMask of
    varSmallint, varInteger, varShortInt, varByte, varWord, varLongWord, varInt64, varSingle, varDouble, varCurrency:
      Result := True;
  else
    Result := False;
  end;
end;

function TMSSQLFilterBuilder.BuildSQL(ACriteria: TAiFilterCriteria): string;
begin
  FParamCounter := 0;
  FParamNames.Clear;
  FParamValues.Clear;
  Result := Process(ACriteria);
end;

function TMSSQLFilterBuilder.Process(ACriteria: TAiFilterCriteria): string;
var
  I: Integer;
  Criterion: TFilterCriterion;
  Parts: TList<string>;
  LogicStr, SubSQL, Path, PName: string;

  function NextParam: string;
  begin
    Inc(FParamCounter);
    Result := 'p_flt_' + IntToStr(FParamCounter);
  end;

  procedure AddParam(const AName: string; const AValue: Variant);
  begin
    FParamNames.Add(AName);
    FParamValues.Add(AValue);
  end;

  // JSON_VALUE devuelve NVARCHAR; para comparar números hay que castear.
  // Los booleanos JSON llegan como 'true'/'false' y las fechas como texto ISO.
  function TypedPath(const APath: string; const AValue: Variant): string;
  begin
    if IsNumericValue(AValue) and APath.StartsWith('JSON_VALUE') then
      Result := 'TRY_CAST(' + APath + ' AS FLOAT)'
    else
      Result := APath;
  end;

  function TypedValue(const AValue: Variant): Variant;
  begin
    if (VarType(AValue) and varTypeMask) = varBoolean then
      Result := IfThen(Boolean(AValue), 'true', 'false')
    else if (VarType(AValue) and varTypeMask) = varDate then
      Result := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', VarToDateTime(AValue))
    else
      Result := AValue;
  end;

begin
  Result := '';
  if (ACriteria = nil) or (ACriteria.Count = 0) then
    Exit;

  Parts := TList<string>.Create;
  try
    LogicStr := IfThen(ACriteria.LogicalOp = loAnd, ' AND ', ' OR ');

    for I := 0 to ACriteria.Count - 1 do
    begin
      Criterion := ACriteria.Items[I];

      if Criterion.IsGroup then
      begin
        SubSQL := Process(Criterion.SubCriteria);
        if SubSQL <> '' then
          Parts.Add('(' + SubSQL + ')');
        Continue;
      end;

      Path := BuildPath(Criterion.Key);
      PName := NextParam;

      case Criterion.Op of
        foEqual:
          begin
            Parts.Add(Format('(%s = :%s)', [TypedPath(Path, Criterion.Value), PName]));
            AddParam(PName, TypedValue(Criterion.Value));
          end;
        foNotEqual:
          begin
            Parts.Add(Format('(%s <> :%s)', [TypedPath(Path, Criterion.Value), PName]));
            AddParam(PName, TypedValue(Criterion.Value));
          end;
        foGreater:
          begin
            Parts.Add(Format('(%s > :%s)', [TypedPath(Path, Criterion.Value), PName]));
            AddParam(PName, TypedValue(Criterion.Value));
          end;
        foGreaterOrEqual:
          begin
            Parts.Add(Format('(%s >= :%s)', [TypedPath(Path, Criterion.Value), PName]));
            AddParam(PName, TypedValue(Criterion.Value));
          end;
        foLess:
          begin
            Parts.Add(Format('(%s < :%s)', [TypedPath(Path, Criterion.Value), PName]));
            AddParam(PName, TypedValue(Criterion.Value));
          end;
        foLessOrEqual:
          begin
            Parts.Add(Format('(%s <= :%s)', [TypedPath(Path, Criterion.Value), PName]));
            AddParam(PName, TypedValue(Criterion.Value));
          end;

        foLike, foILike: // LIKE es case-insensitive bajo la collation CI por defecto
          begin
            Parts.Add(Format('(%s LIKE :%s)', [Path, PName]));
            AddParam(PName, Criterion.Value);
          end;
        foStartsWith:
          begin
            Parts.Add(Format('(%s LIKE :%s)', [Path, PName]));
            AddParam(PName, VarToStr(Criterion.Value) + '%');
          end;
        foEndsWith:
          begin
            Parts.Add(Format('(%s LIKE :%s)', [Path, PName]));
            AddParam(PName, '%' + VarToStr(Criterion.Value));
          end;

        foIn, foNotIn:
          begin
            if VarIsArray(Criterion.Value) then
            begin
              var InList := TList<string>.Create;
              try
                var J: Integer;
                for J := VarArrayLowBound(Criterion.Value, 1) to VarArrayHighBound(Criterion.Value, 1) do
                begin
                  var SP := PName + '_in_' + IntToStr(J);
                  InList.Add(':' + SP);
                  AddParam(SP, TypedValue(VarArrayGet(Criterion.Value, [J])));
                end;
                var Op := IfThen(Criterion.Op = foIn, 'IN', 'NOT IN');
                Parts.Add(Format('(%s %s (%s))', [TypedPath(Path, Criterion.Value), Op, String.Join(',', InList.ToArray)]));
              finally
                InList.Free;
              end;
            end
            else
            begin
              var Op := IfThen(Criterion.Op = foIn, 'IN', 'NOT IN');
              Parts.Add(Format('(%s %s (:%s))', [TypedPath(Path, Criterion.Value), Op, PName]));
              AddParam(PName, TypedValue(Criterion.Value));
            end;
          end;

        foBetween:
          begin
            Parts.Add(Format('(%s BETWEEN :%s_1 AND :%s_2)', [TypedPath(Path, Criterion.Value), PName, PName]));
            AddParam(PName + '_1', TypedValue(Criterion.Value));
            AddParam(PName + '_2', TypedValue(Criterion.Value2));
          end;

        // JSON_VALUE devuelve NULL tanto si la clave falta como si es null JSON
        foIsNull:
          Parts.Add(Format('(%s IS NULL)', [Path]));
        foIsNotNull:
          Parts.Add(Format('(%s IS NOT NULL)', [Path]));

        foExists: // JSON_PATH_EXISTS disponible desde SQL Server 2022
          Parts.Add(Format('(JSON_PATH_EXISTS(properties, ''%s'') = 1)', [JsonPathOf(Criterion.Key)]));

        foContains: // Aproximación: valor exacto en la clave indicada
          begin
            Parts.Add(Format('(%s = :%s)', [TypedPath(Path, Criterion.Value), PName]));
            AddParam(PName, TypedValue(Criterion.Value));
          end;

      else
        Parts.Add('(1=1)');
      end;
    end;

    if Parts.Count > 0 then
    begin
      Result := Parts[0];
      for I := 1 to Parts.Count - 1 do
        Result := Result + LogicStr + Parts[I];
    end;
  finally
    Parts.Free;
  end;
end;

procedure TMSSQLFilterBuilder.ApplyParams(AQuery: TFDQuery);
var
  I: Integer;
begin
  for I := 0 to FParamNames.Count - 1 do
    AQuery.ParamByName(FParamNames[I]).Value := FParamValues[I];
end;

// =============================================================================
// TAiRAGVectorMSSQLDriver — Infraestructura
// =============================================================================

constructor TAiRAGVectorMSSQLDriver.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTableName := 'vector_store';
  FCurrentEntidad := 'DEFAULT';
  FLanguage := alSpanish;
  FLastSQL := '';
  FFtsAvailable := False;
  FFtsCheckedFor := '';
end;

procedure TAiRAGVectorMSSQLDriver.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FConnection) then
    FConnection := nil;
end;

procedure TAiRAGVectorMSSQLDriver.SetConnection(const Value: TFDConnection);
begin
  if FConnection = Value then
    Exit;
  FConnection := Value;
  if Assigned(FConnection) then
    FConnection.FreeNotification(Self);
end;

procedure TAiRAGVectorMSSQLDriver.SetTableName(const Value: string);
begin
  FTableName := Value.ToLower;
  FFtsCheckedFor := ''; // invalidar cache de disponibilidad full-text
end;

function TAiRAGVectorMSSQLDriver.NewQuery: TFDQuery;
begin
  if not Assigned(FConnection) then
    raise Exception.Create('TAiRAGVectorMSSQLDriver: propiedad Connection no asignada.');
  Result := TFDQuery.Create(nil);
  Result.Connection := FConnection;
end;

function TAiRAGVectorMSSQLDriver.GetLcid: Integer;
begin
  // LCID para FREETEXTTABLE / CREATE FULLTEXT INDEX
  case FLanguage of
    alSpanish:
      Result := 3082; // Español (moderno)
    alEnglish:
      Result := 1033; // Inglés (US)
    alPortuguese:
      Result := 2070; // Portugués (Portugal); Brasil = 1046
  else
    Result := 0; // Neutral (sin stemming)
  end;
end;

function TAiRAGVectorMSSQLDriver.IsFullTextReady: Boolean;
var
  Q: TFDQuery;
begin
  // Cache por tabla: el servicio FTS es opcional on-premise y el índice
  // puede no existir si el esquema no fue creado por CreateSchema.
  if FFtsCheckedFor = FTableName then
    Exit(FFtsAvailable);

  FFtsAvailable := False;
  Q := NewQuery;
  try
    try
      Q.SQL.Text := 'SELECT CASE WHEN FULLTEXTSERVICEPROPERTY(''IsFullTextInstalled'') = 1' +
        ' AND EXISTS (SELECT 1 FROM sys.fulltext_indexes WHERE object_id = OBJECT_ID(:t))' + ' THEN 1 ELSE 0 END AS ok';
      Q.ParamByName('t').AsString := FTableName;
      Q.Open;
      FFtsAvailable := Q.Fields[0].AsInteger = 1;
      Q.Close;
    except
      FFtsAvailable := False; // Silencioso: degradar a solo-vector
    end;
  finally
    Q.Free;
  end;
  FFtsCheckedFor := FTableName;
  Result := FFtsAvailable;
end;

// =============================================================================
// Helpers: embedding ↔ string y metadata ↔ JSON
// =============================================================================

function TAiRAGVectorMSSQLDriver.StringToEmbedding(const AStr: string): TAiEmbeddingData;
var
  CleanStr: string;
  Parts: TArray<string>;
  I: Integer;
  FS: TFormatSettings;
begin
  Result := nil;
  if (AStr = '') or (AStr = '[]') then
    Exit;
  CleanStr := AStr.Replace('[', '').Replace(']', '');
  Parts := CleanStr.Split([',']);
  SetLength(Result, Length(Parts));
  FS := TFormatSettings.Invariant;
  for I := 0 to High(Parts) do
    if not TryStrToFloat(Trim(Parts[I]), Result[I], FS) then
      Result[I] := 0.0;
end;

function TAiRAGVectorMSSQLDriver.PropertiesToJSON(AMeta: TAiEmbeddingMetaData): string;
var
  J: TJSONObject;
begin
  if not Assigned(AMeta) then
    Exit('{}');
  J := AMeta.ToJSON;
  try
    Result := J.ToJSON;
  finally
    J.Free;
  end;
end;

// =============================================================================
// Gestión del esquema
// =============================================================================

procedure TAiRAGVectorMSSQLDriver.CreateSchema(const ABaseTableName: string; ADim: Integer);
var
  Q: TFDQuery;
  FtsInstalled, HasFtsIndex: Boolean;
begin
  if (ADim <= 0) or (ADim > 1998) then
    raise Exception.CreateFmt('Dimensión inválida: %d. SQL Server 2025 soporta VECTOR(1..1998).', [ADim]);

  FTableName := ABaseTableName.ToLower;
  FFtsCheckedFor := '';

  Q := NewQuery;
  try
    // Tabla principal. rowid es la clave del índice full-text: FTS exige un
    // índice único, no nulo y de una sola columna — la PK compuesta no sirve.
    Q.ExecSQL(
      'IF OBJECT_ID(N''' + FTableName + ''', N''U'') IS NULL ' +
      'CREATE TABLE ' + FTableName + ' (' +
      '  rowid BIGINT IDENTITY(1,1) NOT NULL,' +
      '  entidad NVARCHAR(50) NOT NULL,' +
      '  id NVARCHAR(400) NOT NULL,' +
      '  model NVARCHAR(100) NULL,' +
      '  content NVARCHAR(MAX) NULL,' +
      '  properties NVARCHAR(MAX) NULL,' +
      '  embedding VECTOR(' + IntToStr(ADim) + ') NULL,' +
      '  CONSTRAINT pk_' + FTableName + ' PRIMARY KEY (entidad, id),' +
      '  CONSTRAINT uq_' + FTableName + '_rowid UNIQUE (rowid)' +
      ')');

    // Full-Text: mejor esfuerzo. Si el servicio no está instalado (componente
    // opcional on-premise) el driver queda en modo solo-vector.
    try
      Q.Open('SELECT ISNULL(FULLTEXTSERVICEPROPERTY(''IsFullTextInstalled''), 0) AS v');
      FtsInstalled := Q.Fields[0].AsInteger = 1;
      Q.Close;
    except
      FtsInstalled := False;
    end;

    if FtsInstalled then
    begin
      try
        Q.ExecSQL('IF NOT EXISTS (SELECT 1 FROM sys.fulltext_catalogs WHERE name = N''makerai_ftcat'') ' +
          'CREATE FULLTEXT CATALOG makerai_ftcat');

        Q.Open('SELECT COUNT(*) AS c FROM sys.fulltext_indexes WHERE object_id = OBJECT_ID(N''' + FTableName + ''')');
        HasFtsIndex := Q.Fields[0].AsInteger > 0;
        Q.Close;

        // Nota: el idioma del índice se fija al crearlo; cambiar Language
        // después requiere recrear el índice full-text manualmente.
        if not HasFtsIndex then
          Q.ExecSQL('CREATE FULLTEXT INDEX ON ' + FTableName + ' (content LANGUAGE ' + IntToStr(GetLcid) + ') ' +
            'KEY INDEX uq_' + FTableName + '_rowid ON makerai_ftcat WITH CHANGE_TRACKING AUTO');
      except
        // Silencioso: sin FTS la búsqueda léxical se desactiva en Search
      end;
    end;
  finally
    Q.Free;
  end;

  IsFullTextReady; // refrescar cache
end;

procedure TAiRAGVectorMSSQLDriver.DropSchema(const ABaseTableName: string);
var
  Q: TFDQuery;
  TN: string;
begin
  TN := ABaseTableName.ToLower;
  Q := NewQuery;
  try
    // DROP TABLE elimina también su índice full-text asociado
    Q.ExecSQL('DROP TABLE IF EXISTS ' + TN);
  finally
    Q.Free;
  end;
  FFtsCheckedFor := '';
end;

// =============================================================================
// CRUD
// =============================================================================

procedure TAiRAGVectorMSSQLDriver.Add(const ANode: TAiEmbeddingNode; const AEntidad: string);
var
  Q: TFDQuery;
  LEnt, LEmbExpr: string;

  procedure SetCommonParams;
  begin
    Q.ParamByName('ent').AsString := LEnt;
    Q.ParamByName('id').AsString := ANode.Tag;
    Q.ParamByName('model').AsString := ANode.Model;
    Q.ParamByName('content').AsWideMemo := ANode.Text;
    Q.ParamByName('prop').AsWideMemo := PropertiesToJSON(ANode.MetaData);
    if Length(ANode.Data) > 0 then
      Q.ParamByName('emb').AsWideMemo := EmbeddingToString(ANode.Data);
  end;

begin
  Q := NewQuery;
  try
    LEnt := IfThen(AEntidad = '', FCurrentEntidad, AEntidad);

    if Length(ANode.Data) > 0 then
      LEmbExpr := 'CAST(:emb AS VECTOR(' + IntToStr(Length(ANode.Data)) + '))'
    else
      LEmbExpr := 'NULL';

    // Upsert en dos pasos: T-SQL no tiene ON CONFLICT y MERGE tiene
    // demasiados casos borde — UPDATE primero, INSERT si no existía.
    Q.SQL.Text := 'UPDATE ' + FTableName + ' SET model = :model, content = :content, properties = :prop, embedding = ' + LEmbExpr +
      ' WHERE entidad = :ent AND id = :id';
    SetCommonParams;
    Q.ExecSQL;

    if Q.RowsAffected = 0 then
    begin
      Q.SQL.Text := 'INSERT INTO ' + FTableName + ' (entidad, id, model, content, properties, embedding) ' +
        'VALUES (:ent, :id, :model, :content, :prop, ' + LEmbExpr + ')';
      SetCommonParams;
      Q.ExecSQL;
    end;
  finally
    Q.Free;
  end;
end;

procedure TAiRAGVectorMSSQLDriver.Delete(const AID: string; const AEntidad: string);
var
  Q: TFDQuery;
begin
  Q := NewQuery;
  try
    Q.SQL.Text := 'DELETE FROM ' + FTableName + ' WHERE entidad = :ent AND id = :id';
    Q.ParamByName('ent').AsString := IfThen(AEntidad = '', FCurrentEntidad, AEntidad);
    Q.ParamByName('id').AsString := AID;
    Q.ExecSQL;
  finally
    Q.Free;
  end;
end;

procedure TAiRAGVectorMSSQLDriver.Clear(const AEntidad: string);
var
  Q: TFDQuery;
begin
  Q := NewQuery;
  try
    Q.SQL.Text := 'DELETE FROM ' + FTableName + ' WHERE entidad = :ent';
    Q.ParamByName('ent').AsString := IfThen(AEntidad = '', FCurrentEntidad, AEntidad);
    Q.ExecSQL;
  finally
    Q.Free;
  end;
end;

// =============================================================================
// Search — híbrida en un solo query T-SQL (CTEs), misma estructura que Postgres
// =============================================================================

function TAiRAGVectorMSSQLDriver.Search(const ATarget: TAiEmbeddingNode; const AEntidad: string; ALimit: Integer; APrecision: Double;
  AFilter: TAiFilterCriteria; Options: TAiSearchOptions): TAiRAGVector;
var
  Q: TFDQuery;
  SQL: TStringBuilder;
  LEnt: string;
  Node: TAiEmbeddingNode;
  JObj: TJSONObject;
  LOptions: TAiSearchOptions;
  FS: TFormatSettings;
  DoVector, DoLexical: Boolean;
  MinVectorScore, MinLexicalScore: Double;
  FilterSQL: string;
  HasFilter: Boolean;
  FilterBuilder: TMSSQLFilterBuilder;
  VWeight, LWeight: Double;
  Dim, Lim3: Integer;
  DistExpr: string;
begin
  Result := TAiRAGVector.Create(nil, True);
  LEnt := IfThen(AEntidad = '', FCurrentEntidad, AEntidad);
  FS := TFormatSettings.Invariant;

  LOptions := Options;
  if not Assigned(LOptions) and Assigned(Owner) and (Owner is TAiRAGVector) then
    LOptions := TAiRAGVector(Owner).SearchOptions;

  DoVector := (Length(ATarget.Data) > 0) and ((LOptions = nil) or LOptions.UseEmbeddings);
  DoLexical := Assigned(LOptions) and LOptions.UseBM25 and (Trim(ATarget.Text) <> '') and IsFullTextReady;

  if not(DoVector or DoLexical) then
    Exit;

  if Assigned(LOptions) then
  begin
    MinVectorScore := LOptions.MinAbsoluteScoreEmbedding;
    MinLexicalScore := LOptions.MinAbsoluteScoreBM25;
  end
  else
  begin
    MinVectorScore := 0.0;
    MinLexicalScore := 0.0;
  end;

  Dim := Length(ATarget.Data);
  Lim3 := ALimit * 3;

  Q := NewQuery;
  SQL := TStringBuilder.Create;
  FilterBuilder := TMSSQLFilterBuilder.Create;
  try
    FilterSQL := '';
    HasFilter := False;
    if Assigned(AFilter) and (AFilter.Count > 0) then
    begin
      FilterSQL := FilterBuilder.BuildSQL(AFilter);
      HasFilter := FilterSQL <> '';
    end;

    // El embedding objetivo se declara una sola vez como variable T-SQL
    if DoVector then
    begin
      SQL.AppendLine('DECLARE @qv VECTOR(' + IntToStr(Dim) + ') = CAST(:qv AS VECTOR(' + IntToStr(Dim) + '));');
      DistExpr := 'VECTOR_DISTANCE(''cosine'', embedding, @qv)';
    end;

    SQL.AppendLine('WITH');

    // --- BLOQUE VECTOR (kNN exacto, similitud = 1 - distancia coseno) ---
    SQL.AppendLine('vector_res AS (');
    if DoVector then
    begin
      SQL.AppendLine('  SELECT TOP (' + IntToStr(Lim3) + ') rowid, entidad, id, model, content, properties,');
      SQL.AppendLine('    CAST(embedding AS NVARCHAR(MAX)) AS emb_txt,');
      SQL.AppendLine('    (1.0 - ' + DistExpr + ') AS v_score,');
      SQL.AppendLine('    ROW_NUMBER() OVER (ORDER BY ' + DistExpr + ' ASC) AS v_rank');
      SQL.AppendLine('  FROM ' + FTableName);
      SQL.AppendLine('  WHERE entidad = :ent AND embedding IS NOT NULL');
      if Trim(ATarget.Model) <> '' then
        SQL.AppendLine('    AND (model = :mdl OR model IS NULL OR model = '''')');
      if MinVectorScore > 0 then
        SQL.AppendLine('    AND (1.0 - ' + DistExpr + ') >= ' + FloatToStr(MinVectorScore, FS));
      if HasFilter then
        SQL.AppendLine('    AND (' + FilterSQL + ')');
      SQL.AppendLine('  ORDER BY ' + DistExpr + ' ASC');
    end
    else
    begin
      SQL.AppendLine('  SELECT CAST(NULL AS BIGINT) AS rowid, CAST(NULL AS NVARCHAR(50)) AS entidad,');
      SQL.AppendLine('    CAST(NULL AS NVARCHAR(400)) AS id, CAST(NULL AS NVARCHAR(100)) AS model,');
      SQL.AppendLine('    CAST(NULL AS NVARCHAR(MAX)) AS content, CAST(NULL AS NVARCHAR(MAX)) AS properties,');
      SQL.AppendLine('    CAST(NULL AS NVARCHAR(MAX)) AS emb_txt, CAST(0.0 AS FLOAT) AS v_score,');
      SQL.AppendLine('    CAST(999999 AS BIGINT) AS v_rank');
      SQL.AppendLine('  WHERE 1 = 0');
    end;
    SQL.AppendLine('),');

    // --- BLOQUE LEXICAL (FREETEXTTABLE = Okapi BM25; RANK 0-1000) ---
    SQL.AppendLine('lexical_raw AS (');
    if DoLexical then
    begin
      SQL.AppendLine('  SELECT TOP (' + IntToStr(Lim3) + ') t.rowid, t.entidad, t.id, t.model, t.content, t.properties,');
      SQL.AppendLine('    CAST(t.embedding AS NVARCHAR(MAX)) AS emb_txt,');
      SQL.AppendLine('    CAST(ft.[RANK] AS FLOAT) AS l_raw');
      SQL.AppendLine('  FROM FREETEXTTABLE(' + FTableName + ', content, :q, LANGUAGE ' + IntToStr(GetLcid) + ', ' + IntToStr(Lim3 * 2) +
        ') AS ft');
      SQL.AppendLine('  INNER JOIN ' + FTableName + ' AS t ON t.rowid = ft.[KEY]');
      SQL.AppendLine('  WHERE t.entidad = :ent');
      if HasFilter then
        SQL.AppendLine('    AND (' + FilterSQL + ')');
      SQL.AppendLine('  ORDER BY ft.[RANK] DESC');
    end
    else
    begin
      SQL.AppendLine('  SELECT CAST(NULL AS BIGINT) AS rowid, CAST(NULL AS NVARCHAR(50)) AS entidad,');
      SQL.AppendLine('    CAST(NULL AS NVARCHAR(400)) AS id, CAST(NULL AS NVARCHAR(100)) AS model,');
      SQL.AppendLine('    CAST(NULL AS NVARCHAR(MAX)) AS content, CAST(NULL AS NVARCHAR(MAX)) AS properties,');
      SQL.AppendLine('    CAST(NULL AS NVARCHAR(MAX)) AS emb_txt, CAST(0.0 AS FLOAT) AS l_raw');
      SQL.AppendLine('  WHERE 1 = 0');
    end;
    SQL.AppendLine('),');

    // Normalización del RANK a [0,1] dividiendo por el máximo del conjunto
    SQL.AppendLine('lexical_norm AS (');
    SQL.AppendLine('  SELECT rowid, entidad, id, model, content, properties, emb_txt,');
    SQL.AppendLine('    l_raw / NULLIF(MAX(l_raw) OVER (), 0) AS l_score,');
    SQL.AppendLine('    ROW_NUMBER() OVER (ORDER BY l_raw DESC) AS l_rank');
    SQL.AppendLine('  FROM lexical_raw');
    SQL.AppendLine('),');

    SQL.AppendLine('lexical_res AS (');
    SQL.Append('  SELECT * FROM lexical_norm');
    if MinLexicalScore > 0 then
      SQL.Append(' WHERE l_score >= ' + FloatToStr(MinLexicalScore, FS));
    SQL.AppendLine;
    SQL.AppendLine('),');

    // --- COMBINE / SCORE ---
    SQL.AppendLine('combined AS (');
    SQL.AppendLine('  SELECT COALESCE(v.rowid, l.rowid) AS rowid, COALESCE(v.entidad, l.entidad) AS entidad,');
    SQL.AppendLine('    COALESCE(v.id, l.id) AS id, COALESCE(v.model, l.model) AS model,');
    SQL.AppendLine('    COALESCE(v.content, l.content) AS content, COALESCE(v.properties, l.properties) AS properties,');
    SQL.AppendLine('    COALESCE(v.emb_txt, l.emb_txt) AS emb_txt,');
    SQL.AppendLine('    COALESCE(v.v_score, 0) AS v_score, COALESCE(l.l_score, 0) AS l_score,');
    SQL.AppendLine('    COALESCE(v.v_rank, 999999) AS v_rank, COALESCE(l.l_rank, 999999) AS l_rank');
    SQL.AppendLine('  FROM vector_res AS v FULL OUTER JOIN lexical_res AS l ON v.rowid = l.rowid');
    SQL.AppendLine('),');

    SQL.AppendLine('scored AS (');
    SQL.AppendLine('  SELECT id, entidad, content, model, properties, emb_txt,');
    if DoVector and DoLexical then
    begin
      if Assigned(LOptions) and LOptions.UseRRF then
        SQL.AppendLine('    (1.0 / (60 + v_rank)) + (1.0 / (60 + l_rank)) AS raw_score')
      else
      begin
        if Assigned(LOptions) then
        begin
          VWeight := LOptions.EmbeddingWeight;
          LWeight := LOptions.BM25Weight;
        end
        else
        begin
          VWeight := 0.7;
          LWeight := 0.3;
        end;
        SQL.AppendLine('    (v_score * ' + FloatToStr(VWeight, FS) + ') + (l_score * ' + FloatToStr(LWeight, FS) + ') AS raw_score');
      end;
    end
    else if DoVector then
      SQL.AppendLine('    v_score AS raw_score')
    else
      SQL.AppendLine('    l_score AS raw_score');
    SQL.AppendLine('  FROM combined');
    SQL.AppendLine('),');

    SQL.AppendLine('final_scored AS (');
    SQL.AppendLine('  SELECT id, entidad, content, model, properties, emb_txt,');
    if Assigned(LOptions) and LOptions.UseRRF and DoVector and DoLexical then
      SQL.AppendLine('    (raw_score / ((1.0/61) + (1.0/61))) AS final_score')
    else
      SQL.AppendLine('    raw_score AS final_score');
    SQL.AppendLine('  FROM scored');
    SQL.AppendLine(')');

    SQL.AppendLine('SELECT TOP (' + IntToStr(ALimit) + ') id, entidad, content, model, properties, emb_txt, final_score');
    SQL.AppendLine('FROM final_scored');
    if APrecision > 0 then
      SQL.AppendLine('WHERE final_score >= ' + FloatToStr(APrecision, FS));
    SQL.AppendLine('ORDER BY final_score DESC');

    Q.SQL.Text := SQL.ToString;
    FLastSQL := Q.SQL.Text;

    // Parámetros (FireDAC vincula el mismo nombre en todas sus apariciones)
    Q.ParamByName('ent').AsString := LEnt;
    if DoVector then
    begin
      Q.ParamByName('qv').AsWideMemo := EmbeddingToString(ATarget.Data);
      if Trim(ATarget.Model) <> '' then
        Q.ParamByName('mdl').AsString := ATarget.Model;
    end;
    if DoLexical then
      Q.ParamByName('q').AsString := ATarget.Text;
    if HasFilter then
      FilterBuilder.ApplyParams(Q);

    try
      Q.Open;
    except
      on E: Exception do
        raise Exception.CreateFmt('MSSQL Error: %s'#13#10'SQL: %s', [E.Message, FLastSQL]);
    end;

    // Procesamiento de resultados
    while not Q.Eof do
    begin
      Node := TAiEmbeddingNode.Create(ATarget.Dim);
      try
        Node.Tag := Q.FieldByName('id').AsString;
        Node.Text := Q.FieldByName('content').AsString;
        Node.Model := Q.FieldByName('model').AsString;
        Node.Idx := Q.FieldByName('final_score').AsFloat;

        if not Q.FieldByName('emb_txt').IsNull then
        begin
          Node.Data := StringToEmbedding(Q.FieldByName('emb_txt').AsString);
          if Length(Node.Data) > 0 then
            Node.SetDataLength(Length(Node.Data));
        end;

        if not Q.FieldByName('properties').IsNull then
        begin
          JObj := TJSONObject.ParseJSONValue(Q.FieldByName('properties').AsString) as TJSONObject;
          if Assigned(JObj) then
            try
              Node.MetaData.FromJSON(JObj);
            finally
              JObj.Free;
            end;
        end;
        // Asignar después de FromJSON — FromJSON.Clear borraría lo anterior
        Node.MetaData['entidad'] := Q.FieldByName('entidad').AsString;
        Result.Items.Add(Node);
      except
        Node.Free;
        raise;
      end;
      Q.Next;
    end;
  finally
    Q.Free;
    SQL.Free;
    FilterBuilder.Free;
  end;
end;

end.
