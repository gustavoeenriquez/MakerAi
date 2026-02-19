unit uMakerAi.RAG.Vector.Driver.Postgres;

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
  uMakerAi.RAG.MetaData; // Ahora contiene TFilterOperator unificado

type
  // Tipos de datos para conversión correcta a SQL
  TJSONBDataType = (jdtString, jdtInteger, jdtFloat, jdtBoolean, jdtDate, jdtDateTime, jdtJSON, jdtArray);

  TQueryParamValue = record
    Name: string;
    Value: Variant;
  end;

  // Clase helper para construcción de condiciones SQL
  TJSONBFilterCondition = class
  private
    FPath: string;
    FOperator: TFilterOperator; // Usamos el enum unificado
    FValue: Variant;
    FSecondValue: Variant;
    FDataType: TJSONBDataType;
    FIsNegated: Boolean;
    function GetSQLOperator: string;
    function FormatValue(const AValue: Variant): string;
  public
    constructor Create(const APath: string; AOperator: TFilterOperator; const AValue: Variant; ADataType: TJSONBDataType = jdtString);
    function ToSQL(AParamIndex: Integer; AParams: TList<TQueryParamValue>): string;

    property SecondValue: Variant read FSecondValue write FSecondValue;
  end;

  // Builder para construir la cláusula WHERE
  TJSONBFilterBuilder = class
  private
    // FConditions: TObjectList<TJSONBFilterCondition>;
    FLogicalOperator: string; // 'AND' o 'OR'
    FParamCounter: Integer;
    FQuery: TFDQuery;
    FParams: TList<TQueryParamValue>;
    function BuildPath(const AKey: string; AOperator: TFilterOperator): string;
    function InferDataType(const AValue: Variant): TJSONBDataType;
    function Process(ACriteria: TAiFilterCriteria): string;
    procedure ApplyParams(AQuery: TFDQuery);
  public
    constructor Create(AQuery: TFDQuery);
    destructor Destroy; override;

    // Método principal que consume los criterios unificados
    function BuildSQL(ACriteria: TAiFilterCriteria): string;

    // Generación SQL
    property Params: TList<TQueryParamValue> read FParams;
  end;

  { TAiRAGVectorPostgresDriver }
  TAiRAGVectorPostgresDriver = class(TAiVectorStoreDriverBase)
  private
    FConnection: TFDConnection;
    FTableName: string;
    FCurrentEntidad: string;
    FLanguage: TAiLanguage;
    FLastSQL: string;

    function NewQuery: TFDQuery;
    function PropertiesToJSON(AMeta: TAiEmbeddingMetaData): string;
    procedure SetTableName(const Value: string);
    function GetPostgresLangConfig: string;
    procedure SetConnection(const Value: TFDConnection);

    // Helper actualizado para usar Criteria
    function BuildJSONBFilter(AFilter: TAiFilterCriteria; AQuery: TFDQuery; out HasFilter: Boolean): string;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;

    { Métodos Principales }
    procedure Add(const ANode: TAiEmbeddingNode; const AEntidad: string = ''); override;

    // Firma actualizada para coincidir con la clase base y usar TAiFilterCriteria
    function Search(const ATarget: TAiEmbeddingNode; const AEntidad: string; ALimit: Integer; APrecision: Double; AFilter: TAiFilterCriteria; Options: TAiSearchOptions): TAiRAGVector; override;

    procedure Delete(const AID: string; const AEntidad: string); override;
    procedure Clear(const AEntidad: string); override;

    procedure CreateSchema(const ABaseTableName: string; ADim: Integer);

    { Helpers }
    function EmbeddingToString(const AData: TAiEmbeddingData): string;
    function StringToEmbedding(const AStr: String): TAiEmbeddingData;

    property LastSQL: string read FLastSQL;
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
  RegisterComponents('MakerAI.RAG.Drivers', [TAiRAGVectorPostgresDriver]);
end;

{ TJSONBFilterCondition }

constructor TJSONBFilterCondition.Create(const APath: string; AOperator: TFilterOperator; const AValue: Variant; ADataType: TJSONBDataType);
begin
  FPath := APath;
  FOperator := AOperator;
  FValue := AValue;
  FDataType := ADataType;
  FIsNegated := False;
end;

function TJSONBFilterCondition.GetSQLOperator: string;
begin
  // Mapeo directo de TFilterOperator (MetaData) a SQL Postgres
  case FOperator of
    foEqual:
      Result := '=';
    foNotEqual:
      Result := '<>';
    foGreater:
      Result := '>';
    foGreaterOrEqual:
      Result := '>=';
    foLess:
      Result := '<';
    foLessOrEqual:
      Result := '<=';

    foContains:
      Result := '@>'; // JSONB contiene
    foExists:
      Result := '?'; // Clave existe
    foExistsAny:
      Result := '?|'; // Alguna clave
    foExistsAll:
      Result := '?&'; // Todas las claves

    foLike:
      Result := 'LIKE';
    foILike:
      Result := 'ILIKE';
    foIn:
      Result := 'IN';
    foNotIn:
      Result := 'NOT IN';
    foBetween:
      Result := 'BETWEEN';
    foIsNull:
      Result := 'IS NULL';
    foIsNotNull:
      Result := 'IS NOT NULL';

    // foContainedBy y foStartsWith/EndsWith se manejan con lógica especial o LIKE
    foStartsWith, foEndsWith:
      Result := 'LIKE';
  else
    Result := '=';
  end;
end;

function TJSONBFilterCondition.FormatValue(const AValue: Variant): string;
var
  FS: TFormatSettings;
begin
  FS := TFormatSettings.Invariant;
  if VarIsNull(AValue) then
    Exit('NULL');

  case FDataType of
    jdtString:
      Result := QuotedStr(VarToStr(AValue));
    jdtInteger:
      Result := IntToStr(AValue);
    jdtFloat:
      Result := FloatToStr(Double(AValue), FS);
    jdtBoolean:
      Result := IfThen(Boolean(AValue), 'true', 'false');
    jdtDate:
      Result := QuotedStr(FormatDateTime('yyyy-mm-dd', VarToDateTime(AValue)));
    jdtDateTime:
      Result := QuotedStr(FormatDateTime('yyyy-mm-dd hh:nn:ss', VarToDateTime(AValue)));
    jdtJSON:
      Result := QuotedStr(VarToStr(AValue)) + '::jsonb';
    jdtArray:
      Result := 'ARRAY[' + VarToStr(AValue) + ']';
  else
    Result := QuotedStr(VarToStr(AValue));
  end;
end;

function TJSONBFilterCondition.ToSQL(AParamIndex: Integer; AParams: TList<TQueryParamValue>): string;
var
  ParamName, CastType, ValStr: string;
  I: Integer;
  ParamList: TStringList;
  SQLOp: string;

  // Sub-rutina para añadir parámetros a la lista diferida
  procedure AddParam(const AName: string; const AValue: Variant);
  var
    P: TQueryParamValue;
  begin
    P.Name := AName;
    P.Value := AValue;
    AParams.Add(P);
  end;

begin
  ParamName := 'p_flt_' + IntToStr(AParamIndex);

  // 1. Mapear tipo para CAST Postgres
  case FDataType of
    jdtString:
      CastType := 'text';
    jdtInteger:
      CastType := 'integer';
    jdtFloat:
      CastType := 'numeric';
    jdtBoolean:
      CastType := 'boolean';
    jdtDate:
      CastType := 'date';
    jdtDateTime:
      CastType := 'timestamp';
  else
    CastType := 'text';
  end;

  // 2. Generar SQL y coleccionar parámetros
  case FOperator of
    // Comparaciones simples (=, <>, >, etc.)
    foEqual, foNotEqual, foGreater, foGreaterOrEqual, foLess, foLessOrEqual:
      begin
        Result := Format('((%s)::%s %s :%s)', [FPath, CastType, GetSQLOperator, ParamName]);
        AddParam(ParamName, FValue);
      end;

    // LIKE y derivados (ILIKE, STARTS_WITH, ENDS_WITH)
    foLike, foILike, foStartsWith, foEndsWith:
      begin
        SQLOp := IfThen(FOperator = foILike, 'ILIKE', 'LIKE');
        Result := Format('((%s)::text %s :%s)', [FPath, SQLOp, ParamName]);

        ValStr := VarToStr(FValue);
        if FOperator = foStartsWith then
          ValStr := ValStr + '%'
        else if FOperator = foEndsWith then
          ValStr := '%' + ValStr;

        AddParam(ParamName, ValStr);
      end;

    // Contiene JSON (Uso del operador @>)
    foContains:
      begin
        Result := Format('(properties @> :%s::jsonb)', [ParamName]);

        // 1. Extraer el nombre de la llave de forma limpia
        var
        KeyName := FPath.Replace('properties->>''', '').Replace('properties #>> ''{', '').Replace('}''', '').Replace('''', '');

        // 2. Formatear el valor según su tipo para que el JSON sea válido
        var
          JsonValue: string;

          // Verificación de tipo usando VarType
        if VarIsNumeric(FValue) then
          JsonValue := VarToStr(FValue).Replace(',', '.')
        else if (VarType(FValue) and varTypeMask) = varBoolean then
          JsonValue := IfThen(Boolean(FValue), 'true', 'false')
        else if VarIsNull(FValue) then
          JsonValue := 'null'
        else
          // Los strings DEBEN llevar comillas dobles para ser JSON válido
          JsonValue := '"' + VarToStr(FValue).Replace('"', '\"') + '"';

        AddParam(ParamName, '{"' + KeyName + '": ' + JsonValue + '}');
      end;

    // Existencia de clave (Uso del operador ?)
    foExists:
      begin
        Result := Format('(properties ? :%s)', [ParamName]);
        // Aquí el valor del parámetro es el nombre de la llave
        AddParam(ParamName, FPath.Replace('properties->>''', '').Replace('''', ''));
      end;

    // Existencia de cualquiera (?|) o todos (?&) los elementos
    foExistsAny, foExistsAll:
      begin
        SQLOp := IfThen(FOperator = foExistsAny, '?|', '?&');
        if VarIsArray(FValue) then
        begin
          ParamList := TStringList.Create;
          try
            for I := VarArrayLowBound(FValue, 1) to VarArrayHighBound(FValue, 1) do
            begin
              var
              SubParam := ParamName + '_ex_' + IntToStr(I);
              ParamList.Add(':' + SubParam);
              AddParam(SubParam, VarArrayGet(FValue, [I]));
            end;
            Result := Format('(properties %s ARRAY[%s]::text[])', [SQLOp, String.Join(',', ParamList.ToStringArray)]);
          finally
            ParamList.Free;
          end;
        end
        else
          raise Exception.Create('foExistsAny / foExistsAll requiere un array de valores.');
      end;

    // IN / NOT IN corregido
    foIn, foNotIn:
      begin
        if VarIsArray(FValue) then
        begin
          ParamList := TStringList.Create;
          try
            for I := VarArrayLowBound(FValue, 1) to VarArrayHighBound(FValue, 1) do
            begin
              var
              SubParam := ParamName + '_in_' + IntToStr(I);
              ParamList.Add(':' + SubParam);
              AddParam(SubParam, VarArrayGet(FValue, [I]));
            end;
            // USAR CastType AQUÍ
            Result := Format('((%s)::%s %s (%s))', [FPath, CastType, GetSQLOperator, String.Join(',', ParamList.ToStringArray)]);
          finally
            ParamList.Free;
          end;
        end
        else
        begin
          Result := Format('((%s)::%s %s (:%s))', [FPath, CastType, GetSQLOperator, ParamName]);
          AddParam(ParamName, FValue);
        end;
      end;

    // BETWEEN con dos parámetros
    foBetween:
      begin
        Result := Format('((%s)::%s BETWEEN :%s_1 AND :%s_2)', [FPath, CastType, ParamName, ParamName]);
        AddParam(ParamName + '_1', FValue);
        AddParam(ParamName + '_2', FSecondValue);
      end;

    // IS NULL / IS NOT NULL (No requieren parámetros)
    foIsNull:
      Result := Format('((%s) IS NULL)', [FPath]);
    foIsNotNull:
      Result := Format('((%s) IS NOT NULL)', [FPath]);

  else
    Result := '(1=1)';
  end;

  // 3. Negación si aplica
  if FIsNegated then
    Result := 'NOT (' + Result + ')';
end;

{ TJSONBFilterBuilder }

constructor TJSONBFilterBuilder.Create;
begin
  inherited Create;
  FParams := TList<TQueryParamValue>.Create;
  FParamCounter := 0;
end;

destructor TJSONBFilterBuilder.Destroy;
begin
  FParams.Free;
  inherited;
end;

procedure TJSONBFilterBuilder.ApplyParams(AQuery: TFDQuery);
var
  P: TQueryParamValue;
begin
  for P in FParams do
  begin
    // Ahora FireDAC encontrará los parámetros porque ya asignamos el SQL.Text antes
    AQuery.ParamByName(P.Name).Value := P.Value;
  end;
end;

function TJSONBFilterBuilder.BuildPath(const AKey: string; AOperator: TFilterOperator): string;
var
  Parts: TArray<string>;
  I: Integer;
  JsonPath: string;
begin
  // 1. Mapeo de campos virtuales / Columnas directas
  if SameText(AKey, 'text') then
    Exit('content'); // El campo 'text' en VGQL apunta a la columna 'content' en Postgres

  if SameText(AKey, 'model') then
    Exit('model'); // Opcional: permitir filtrar por el nombre del modelo directamente

  // 2. Si la llave tiene puntos (ej: user.name), usamos la sintaxis de rutas de Postgres #>>
  if AKey.Contains('.') then
  begin
    Parts := AKey.Split(['.']);
    JsonPath := '{';
    for I := 0 to High(Parts) do
    begin
      if I > 0 then
        JsonPath := JsonPath + ',';
      JsonPath := JsonPath + Parts[I];
    end;
    JsonPath := JsonPath + '}';

    // El operador #>> devuelve el valor como TEXT
    Result := Format('properties #>> ''%s''', [JsonPath]);
  end
  else
  begin
    // 3. Si es una llave simple, usamos el operador estándar ->> (devuelve TEXT)
    Result := Format('properties->>''%s''', [AKey]);
  end;
end;

function TJSONBFilterBuilder.BuildSQL(ACriteria: TAiFilterCriteria): string;
begin
  FParamCounter := 0;
  FParams.Clear;
  Result := Process(ACriteria);
end;

function TJSONBFilterBuilder.InferDataType(const AValue: Variant): TJSONBDataType;
var
  TestVal: Variant;
begin
  // Si es un Array (caso de foIn / foNotIn)
  if VarIsArray(AValue) then
  begin
    if VarArrayHighBound(AValue, 1) >= VarArrayLowBound(AValue, 1) then
    begin
      // Tomamos el primer elemento para inferir el tipo
      TestVal := VarArrayGet(AValue, [VarArrayLowBound(AValue, 1)]);
      Result := InferDataType(TestVal); // Llamada recursiva con un solo valor
      Exit;
    end;
  end;

  // Tipos atómicos
  case VarType(AValue) and varTypeMask of
    varSmallint, varInteger, varShortInt, varByte, varWord, varLongWord, varInt64:
      Result := jdtInteger;
    varSingle, varDouble, varCurrency:
      Result := jdtFloat;
    varBoolean:
      Result := jdtBoolean;
    varDate:
      Result := jdtDateTime;
  else
    Result := jdtString;
  end;
end;

function TJSONBFilterBuilder.Process(ACriteria: TAiFilterCriteria): string;
var
  I: Integer;
  Criterion: TFilterCriterion;
  SQLParts: TList<string>;
  Cond: TJSONBFilterCondition;
  LogicStr, SubSQL: string;
begin
  Result := '';
  if (ACriteria = nil) or (ACriteria.Count = 0) then
    Exit;

  SQLParts := TList<string>.Create;
  try
    LogicStr := IfThen(ACriteria.LogicalOp = loAnd, ' AND ', ' OR ');

    for I := 0 to ACriteria.Count - 1 do
    begin
      Criterion := ACriteria.Items[I];

      if Criterion.IsGroup then
      begin
        // RECURSIVIDAD: Llamamos a Process para el subgrupo
        SubSQL := Process(Criterion.SubCriteria);
        if SubSQL <> '' then
          SQLParts.Add('(' + SubSQL + ')');
      end
      else
      begin
        // CONDICIÓN ATÓMICA
        Inc(FParamCounter);

        // 1. Crear la condición
        Cond := TJSONBFilterCondition.Create(BuildPath(Criterion.Key, Criterion.Op), Criterion.Op, Criterion.Value, InferDataType(Criterion.Value));
        try
          if Criterion.Op = foBetween then
            Cond.SecondValue := Criterion.Value2;

          // 2. Llamada CORREGIDA: Pasamos FParams en lugar de FQuery
          // Esto genera el fragmento SQL y guarda el valor en la lista diferida
          SQLParts.Add(Cond.ToSQL(FParamCounter, FParams));
        finally
          Cond.Free;
        end;
      end;
    end;

    // Unir las partes con AND/OR
    if SQLParts.Count > 0 then
    begin
      Result := SQLParts[0];
      for I := 1 to SQLParts.Count - 1 do
        Result := Result + LogicStr + SQLParts[I];
    end;

  finally
    SQLParts.Free;
  end;
end;

{ TAiRAGVectorPostgresDriver }

constructor TAiRAGVectorPostgresDriver.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTableName := 'vector_store';
  FCurrentEntidad := 'DEFAULT';
  FLanguage := alSpanish;
  FLastSQL := '';
end;

function TAiRAGVectorPostgresDriver.NewQuery: TFDQuery;
begin
  if not Assigned(FConnection) then
    raise Exception.Create('Error Crítico: Driver Postgres sin conexión asignada.');
  Result := TFDQuery.Create(nil);
  Result.Connection := FConnection;
end;

procedure TAiRAGVectorPostgresDriver.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FConnection) then
    FConnection := nil;
end;

procedure TAiRAGVectorPostgresDriver.SetConnection(const Value: TFDConnection);
begin
  if FConnection <> Value then
  begin
    FConnection := Value;
    if Assigned(FConnection) then
      FConnection.FreeNotification(Self);
  end;
end;

procedure TAiRAGVectorPostgresDriver.SetTableName(const Value: string);
begin
  FTableName := Value.ToLower;
end;

function TAiRAGVectorPostgresDriver.GetPostgresLangConfig: string;
begin
  case FLanguage of
    alSpanish:
      Result := 'spanish';
    alEnglish:
      Result := 'english';
    alPortuguese:
      Result := 'portuguese';
  else
    Result := 'simple';
  end;
end;

function TAiRAGVectorPostgresDriver.EmbeddingToString(const AData: TAiEmbeddingData): string;
var
  I: Integer;
  FS: TFormatSettings;
  S: string;
  Parts: TArray<string>;
begin
  if Length(AData) = 0 then
    Exit('[]');

  FS := TFormatSettings.Invariant;
  SetLength(Parts, Length(AData));

  for I := 0 to High(AData) do
    Parts[I] := FloatToStr(AData[I], FS);

  Result := '[' + String.Join(',', Parts) + ']';
end;

function TAiRAGVectorPostgresDriver.StringToEmbedding(const AStr: string): TAiEmbeddingData;
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
    if not TryStrToFloat(Parts[I], Result[I], FS) then
      Result[I] := 0.0;
end;

function TAiRAGVectorPostgresDriver.PropertiesToJSON(AMeta: TAiEmbeddingMetaData): string;
var
  j: TJSONObject;
begin
  if not Assigned(AMeta) then
    Exit('{}');
  j := AMeta.ToJSON;
  try
    Result := j.ToJSON;
  finally
    j.Free;
  end;
end;

// HELPER PRINCIPAL DE FILTRADO
function TAiRAGVectorPostgresDriver.BuildJSONBFilter(AFilter: TAiFilterCriteria; AQuery: TFDQuery; out HasFilter: Boolean): string;
var
  Builder: TJSONBFilterBuilder;
begin
  Result := '';
  HasFilter := False;
  if not Assigned(AFilter) or (AFilter.Count = 0) then
    Exit;

  Builder := TJSONBFilterBuilder.Create(AQuery);
  try
    Result := Builder.BuildSQL(AFilter);
    HasFilter := Result <> '';
  finally
    Builder.Free;
  end;
end;

procedure TAiRAGVectorPostgresDriver.CreateSchema(const ABaseTableName: string; ADim: Integer);
var
  Q: TFDQuery;
  LangConfig: string;
begin
  FTableName := ABaseTableName.ToLower;
  LangConfig := GetPostgresLangConfig;
  Q := NewQuery;
  try
    try
      Q.ExecSQL('CREATE EXTENSION IF NOT EXISTS vector');
    except
    end;

    Q.SQL.Clear;
    Q.SQL.Add(Format('CREATE TABLE IF NOT EXISTS %s (', [FTableName]));
    Q.SQL.Add('  entidad VARCHAR(50) NOT NULL,');
    Q.SQL.Add('  id TEXT NOT NULL,');
    Q.SQL.Add('  model VARCHAR(100),');
    Q.SQL.Add('  content TEXT,');
    Q.SQL.Add('  properties JSONB,');
    Q.SQL.Add(Format('  embedding VECTOR(%d),', [ADim]));
    Q.SQL.Add(Format('  search_vector TSVECTOR GENERATED ALWAYS AS (to_tsvector(''%s'', coalesce(content, ''''))) STORED,', [LangConfig]));
    Q.SQL.Add(Format('  CONSTRAINT pk_%s PRIMARY KEY (entidad, id)', [FTableName]));
    Q.SQL.Add(')');
    Q.ExecSQL;

    Q.ExecSQL(Format('CREATE INDEX IF NOT EXISTS idx_%s_vector ON %s USING hnsw (embedding vector_cosine_ops)', [FTableName, FTableName]));
    Q.ExecSQL(Format('CREATE INDEX IF NOT EXISTS idx_%s_props ON %s USING gin (properties)', [FTableName, FTableName]));
    Q.ExecSQL(Format('CREATE INDEX IF NOT EXISTS idx_%s_fts ON %s USING gin (search_vector)', [FTableName, FTableName]));
  finally
    Q.Free;
  end;
end;

procedure TAiRAGVectorPostgresDriver.Add(const ANode: TAiEmbeddingNode; const AEntidad: string);
var
  Q: TFDQuery;
  LEnt, LEmbedding: string;
begin
  Q := NewQuery;
  try
    LEnt := IfThen(AEntidad = '', FCurrentEntidad, AEntidad);
    if Length(ANode.Data) > 0 then
      LEmbedding := '''' + EmbeddingToString(ANode.Data) + ''''
    else
      LEmbedding := 'NULL';

    Q.SQL.Clear;
    Q.SQL.Add('INSERT INTO ' + FTableName + ' (entidad, id, model, content, properties, embedding)');
    Q.SQL.Add('VALUES (:ent, :id, :model, :content, :prop::jsonb, ' + LEmbedding + '::vector)');
    Q.SQL.Add('ON CONFLICT (entidad, id) DO UPDATE SET');
    Q.SQL.Add('  model = EXCLUDED.model, content = EXCLUDED.content, properties = EXCLUDED.properties, embedding = EXCLUDED.embedding');

    Q.ParamByName('ent').AsString := LEnt;
    Q.ParamByName('id').AsString := ANode.Tag;
    Q.ParamByName('model').AsString := ANode.Model;
    Q.ParamByName('content').AsString := ANode.Text;
    Q.ParamByName('prop').AsString := PropertiesToJSON(ANode.MetaData);
    Q.ExecSQL;
  finally
    Q.Free;
  end;
end;

procedure TAiRAGVectorPostgresDriver.Delete(const AID: string; const AEntidad: string);
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

procedure TAiRAGVectorPostgresDriver.Clear(const AEntidad: string);
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

function TAiRAGVectorPostgresDriver.Search(const ATarget: TAiEmbeddingNode; const AEntidad: string; ALimit: Integer; APrecision: Double; AFilter: TAiFilterCriteria; Options: TAiSearchOptions): TAiRAGVector;
var
  Q: TFDQuery;
  SQL: TStringBuilder;
  LEnt: string;
  Node: TAiEmbeddingNode;
  JObj: TJSONObject;
  LOptions: TAiSearchOptions;
  LTargetEmbedding: string;
  FS: TFormatSettings;
  DoVector, DoLexical, HasValidEmbedding: Boolean;
  MinVectorScore, MinLexicalScore: Double;
  LangConfig, FilterSQL: string;
  HasFilter: Boolean;
  FilterBuilder: TJSONBFilterBuilder; // Builder para filtros dinámicos
  VWeight, LWeight: Double;
begin
  Result := TAiRAGVector.Create(nil, True);
  LEnt := IfThen(AEntidad = '', FCurrentEntidad, AEntidad);
  FS := TFormatSettings.Invariant;
  LangConfig := GetPostgresLangConfig;

  LOptions := Options;
  if not Assigned(LOptions) and Assigned(Owner) and (Owner is TAiRAGVector) then
    LOptions := TAiRAGVector(Owner).SearchOptions;

  HasValidEmbedding := (Length(ATarget.Data) > 0);
  DoVector := HasValidEmbedding and ((LOptions = nil) or LOptions.UseEmbeddings);
  DoLexical := (LOptions <> nil) and LOptions.UseBM25 and (Trim(ATarget.Text) <> '');

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

  if DoVector then
    LTargetEmbedding := QuotedStr(EmbeddingToString(ATarget.Data));

  Q := NewQuery;
  SQL := TStringBuilder.Create;
  FilterBuilder := TJSONBFilterBuilder.Create(Q); // Pasamos Q para inicializar contexto, pero no asignamos parámetros aún
  try
    // 1. GENERAR SQL DINÁMICO Y COLECCIONAR PARÁMETROS
    FilterSQL := '';
    HasFilter := False;
    if Assigned(AFilter) and (AFilter.Count > 0) then
    begin
      FilterSQL := FilterBuilder.BuildSQL(AFilter);
      HasFilter := FilterSQL <> '';
    end;

    // 2. CONSTRUIR EL CUERPO DEL SQL
    SQL.AppendLine('WITH ');

    // --- BLOQUE VECTOR ---
    SQL.AppendLine('vector_res AS (');
    if DoVector then
    begin
      SQL.AppendLine('  SELECT id, content, model, properties, embedding,');
      SQL.AppendLine('    (1 - (embedding <=> ' + LTargetEmbedding + '::vector)) as v_score,');
      SQL.AppendLine('    ROW_NUMBER() OVER (ORDER BY embedding <=> ' + LTargetEmbedding + '::vector ASC) as v_rank');
      SQL.AppendLine('  FROM ' + FTableName);
      SQL.AppendLine('  WHERE entidad = :ent');
      if Trim(ATarget.Model) <> '' then
        SQL.AppendLine('    AND (model = :model OR model IS NULL OR model = '''')');
      if MinVectorScore > 0 then
        SQL.AppendLine('    AND (1 - (embedding <=> ' + LTargetEmbedding + '::vector)) >= :min_v');
      if HasFilter then
        SQL.AppendLine('    AND ' + FilterSQL);
      SQL.AppendLine('  ORDER BY embedding <=> ' + LTargetEmbedding + '::vector');
      SQL.AppendLine('  LIMIT :prelim_limit');
    end
    else
      SQL.AppendLine('  SELECT NULL::text as id, NULL::text as content, NULL::text as model, NULL::jsonb as properties, NULL::vector as embedding, 0::float as v_score, 0::bigint as v_rank WHERE FALSE');
    SQL.AppendLine('),');

    // --- BLOQUE LEXICAL ---
    SQL.AppendLine('lexical_res AS (');
    if DoLexical then
    begin
      SQL.AppendLine('  SELECT id, content, model, properties, embedding,');
      SQL.AppendLine('    ts_rank_cd(search_vector, websearch_to_tsquery(''' + LangConfig + ''', :query), 32) as l_score,');
      SQL.AppendLine('    ROW_NUMBER() OVER (ORDER BY ts_rank_cd(search_vector, websearch_to_tsquery(''' + LangConfig + ''', :query), 32) DESC) as l_rank');
      SQL.AppendLine('  FROM ' + FTableName);
      SQL.AppendLine('  WHERE entidad = :ent');
      SQL.AppendLine('    AND search_vector @@ websearch_to_tsquery(''' + LangConfig + ''', :query)');
      if MinLexicalScore > 0 then
        SQL.AppendLine('    AND ts_rank_cd(search_vector, websearch_to_tsquery(''' + LangConfig + ''', :query), 32) >= :min_l');
      if HasFilter then
        SQL.AppendLine('    AND ' + FilterSQL);
      SQL.AppendLine('  LIMIT :prelim_limit');
    end
    else
      SQL.AppendLine('  SELECT NULL::text as id, NULL::text as content, NULL::text as model, NULL::jsonb as properties, NULL::vector as embedding, 0::float as l_score, 0::bigint as l_rank WHERE FALSE');
    SQL.AppendLine('),');

    // --- BLOQUE COMBINE / RRF / SCORE ---
    SQL.AppendLine('combined AS (');
    SQL.AppendLine('  SELECT COALESCE(v.id, l.id) as id, COALESCE(v.content, l.content) as content, COALESCE(v.model, l.model) as model, COALESCE(v.properties, l.properties) as properties, COALESCE(v.embedding, l.embedding) as embedding,');
    SQL.AppendLine('    COALESCE(v.v_score, 0) as v_score, COALESCE(l.l_score, 0) as l_score, COALESCE(v.v_rank, 999999) as v_rank, COALESCE(l.l_rank, 999999) as l_rank');
    SQL.AppendLine('  FROM vector_res v FULL OUTER JOIN lexical_res l ON v.id = l.id');
    SQL.AppendLine('),');

    SQL.AppendLine('scored AS (');
    SQL.AppendLine('  SELECT id, content, model, properties, embedding, v_score, l_score,');

    if DoVector and DoLexical then
    begin
      if Assigned(LOptions) and LOptions.UseRRF then
      begin
        SQL.AppendLine('    (1.0 / (60 + v_rank)) + (1.0 / (60 + l_rank)) as raw_score');
      end
      else
      begin
        // Cálculo de pesos con IF normales
        if Assigned(LOptions) then
        begin
          VWeight := LOptions.EmbeddingWeight;
          LWeight := LOptions.BM25Weight;
        end
        else
        begin
          // Valores por defecto si no hay opciones
          VWeight := 0.7;
          LWeight := 0.3;
        end;

        SQL.AppendFormat('    (v_score * %f) + (l_score * %f) as raw_score'#13#10, [VWeight, LWeight]);
      end;
    end

    else if DoVector then
      SQL.AppendLine('    v_score as raw_score')
    else
      SQL.AppendLine('    l_score as raw_score');
    SQL.AppendLine('  FROM combined');
    SQL.AppendLine(')');

    // --- SELECCIÓN FINAL ---
    SQL.AppendLine('SELECT id, content, model, properties, embedding,');
    if Assigned(LOptions) and LOptions.UseRRF and DoVector and DoLexical then
      SQL.AppendLine('  (raw_score / ((1.0/61) + (1.0/61))) as final_score')
    else
      SQL.AppendLine('  raw_score as final_score');
    SQL.AppendLine('FROM scored');
    if APrecision > 0 then
      SQL.AppendLine('WHERE raw_score >= :prec');
    SQL.AppendLine('ORDER BY raw_score DESC LIMIT :lim');

    // 3. ASIGNAR SQL A LA QUERY (Crucial: Primero el texto)
    Q.SQL.Text := SQL.ToString;
    FLastSQL := Q.SQL.Text;

    // 4. AHORA APLICAMOS LOS PARÁMETROS DINÁMICOS DEL FILTRO
    if HasFilter then
      FilterBuilder.ApplyParams(Q);

    // 5. ASIGNAR PARÁMETROS ESTÁNDAR
    Q.ParamByName('ent').AsString := LEnt;
    Q.ParamByName('lim').AsInteger := ALimit;
    Q.ParamByName('prelim_limit').AsInteger := ALimit * 3;

    if APrecision > 0 then
      Q.ParamByName('prec').AsFloat := APrecision;

    if DoVector then
    begin
      if Trim(ATarget.Model) <> '' then
        Q.ParamByName('model').AsString := ATarget.Model;
      if MinVectorScore > 0 then
        Q.ParamByName('min_v').AsFloat := MinVectorScore;
    end;

    if DoLexical then
    begin
      Q.ParamByName('query').AsString := ATarget.Text;
      if MinLexicalScore > 0 then
        Q.ParamByName('min_l').AsFloat := MinLexicalScore;
    end;

    // 6. EJECUCIÓN
    try
      Q.Open;
    except
      on E: Exception do
        raise Exception.CreateFmt('Postgres Error: %s'#13#10'SQL: %s', [E.Message, FLastSQL]);
    end;

    // 7. PROCESAMIENTO DE RESULTADOS
    while not Q.Eof do
    begin
      Node := TAiEmbeddingNode.Create(ATarget.Dim);
      try
        Node.Tag := Q.FieldByName('id').AsString;
        Node.Text := Q.FieldByName('content').AsString;
        Node.Model := Q.FieldByName('model').AsString;
        Node.Idx := Q.FieldByName('final_score').AsFloat;

        if not Q.FieldByName('embedding').IsNull then
        begin
          Node.Data := StringToEmbedding(Q.FieldByName('embedding').AsString);
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
