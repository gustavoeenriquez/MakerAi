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
  // Tipos de datos para conversi�n correcta a SQL
  TJSONBDataType = (jdtString, jdtInteger, jdtFloat, jdtBoolean, jdtDate, jdtDateTime, jdtJSON, jdtArray);

  TQueryParamValue = record
    Name: string;
    Value: Variant;
  end;

  // Clase helper para construcci�n de condiciones SQL
  TJSONBFilterCondition = class
  private
    FPath: string;
    FJsonPath: string; // properties->'k' / properties #> '{a,b}'; vacio en columnas (content, model)
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
    property JsonPath: string read FJsonPath write FJsonPath;
  end;

  // Builder para construir la cl�usula WHERE
  TJSONBFilterBuilder = class
  private
    // FConditions: TObjectList<TJSONBFilterCondition>;
    FParamCounter: Integer;
    FParams: TList<TQueryParamValue>;
    function BuildPath(const AKey: string; AOperator: TFilterOperator): string;
    function BuildJsonPath(const AKey: string): string;
    function InferDataType(const AValue: Variant): TJSONBDataType;
    function Process(ACriteria: TAiFilterCriteria): string;
  public
    constructor Create(AQuery: TFDQuery);
    destructor Destroy; override;

    // M�todo principal que consume los criterios unificados
    function BuildSQL(ACriteria: TAiFilterCriteria): string;

    // Generaci�n SQL
    procedure ApplyParams(AQuery: TFDQuery);
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

  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;

    { M�todos Principales }
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

uses
  uMakerAi.Localization;

{$R ..\Resources\uMakerAiRAGDriversIcons.res}  // iconos de paleta del paquete

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

    // foContainedBy y foStartsWith/EndsWith se manejan con l�gica especial o LIKE
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

// Traduce una condicion con la misma semantica que el filtro en memoria
// (TAiEmbeddingMetaData.Evaluate / AiCompareValues). Todo va como literal (sin parametros de
// FireDAC) y sin el operador '?' de JSONB, que FireDAC toma por un parametro:
// - Numeros: el texto del campo se convierte solo si parece un numero; uno que no (un 'N/A')
//   da NULL y no se cumple. Antes ::integer/::numeric directo hacia fallar la consulta entera
//   en cuanto una fila tenia otra cosa (y ::integer fallaba con 42000.0).
// - Un texto que parece numero ('10') se compara como numero si el campo tambien lo es.
// - Texto: '=' distingue mayusculas; el orden no (desempata con ellas), como CompareText.
// - CONTAINS, STARTS_WITH, ENDS_WITH: sin distinguir mayusculas y sin comodines (antes CONTAINS
//   era '@>' de JSONB: igualdad exacta del valor); CONTAINS sobre una lista es pertenencia.
// - EXISTS_ANY/EXISTS_ALL: elementos de la lista del campo (antes '?|', que mira claves).
// - NOT: un campo que falta no se cumple ni negado, como en memoria.
function TJSONBFilterCondition.ToSQL(AParamIndex: Integer; AParams: TList<TQueryParamValue>): string;
const
  NUM_RE = '^\s*[-+]?([0-9]+\.?[0-9]*|\.[0-9]+)([eE][-+]?[0-9]+)?\s*$';
  DATE_RE = '^[0-9]{4}-[0-9]{2}-[0-9]{2}';
var
  I: Integer;
  Parts: TStringList;
  Values: TArray<Variant>;
  Body: string;

  function Q(const S: string): string;
  begin
    Result := QuotedStr(S.Replace(#0, ''));
  end;

  function IsNumType(const V: Variant): Boolean;
  begin
    Result := VarIsNumeric(V) and ((VarType(V) and varTypeMask) <> varBoolean);
  end;

  // Literal numerico: de un numero o de un texto que parece numero
  function TryNumLit(const V: Variant; out ALit: string): Boolean;
  var
    D: Double;
  begin
    Result := False;
    if VarIsArray(V) or VarIsNull(V) or VarIsEmpty(V) then
      Exit;
    if IsNumType(V) then
    begin
      ALit := AiFilterValueToText(V);
      Exit(True);
    end;
    if ((VarType(V) and varTypeMask) <> varDate) and ((VarType(V) and varTypeMask) <> varBoolean) and
      TryStrToFloat(Trim(VarToStr(V)), D, TFormatSettings.Invariant) then
    begin
      ALit := FloatToStr(D, TFormatSettings.Invariant);
      Exit(True);
    end;
  end;

  function NumOf(const P: string): string;
  begin
    Result := '(CASE WHEN ' + P + ' ~ ' + Q(NUM_RE) + ' THEN (' + P + ')::numeric END)';
  end;

  function SqlOp(AOp: TFilterOperator): string;
  begin
    case AOp of
      foNotEqual: Result := '<>';
      foGreater: Result := '>';
      foGreaterOrEqual: Result := '>=';
      foLess: Result := '<';
      foLessOrEqual: Result := '<=';
    else
      Result := '=';
    end;
  end;

  // Texto contra texto: '=' y '<>' exactos; el orden como CompareText (ASCII sin mayusculas,
  // en orden de bytes) y, si empatan, con mayusculas
  function TextCmp(const P: string; AOp: TFilterOperator; const S: string): string;
  var
    UP, US, StrictOp: string;
  begin
    if AOp in [foEqual, foNotEqual] then
      Exit('(' + P + ' ' + SqlOp(AOp) + ' ' + Q(S) + ')');
    UP := 'upper(' + P + ') COLLATE "C"';
    US := 'upper(' + Q(S) + ') COLLATE "C"';
    if AOp in [foGreater, foGreaterOrEqual] then
      StrictOp := '>'
    else
      StrictOp := '<';
    Result := '((' + UP + ' ' + StrictOp + ' ' + US + ') OR (' + UP + ' = ' + US + ' AND ' + P + ' COLLATE "C" ' +
      SqlOp(AOp) + ' ' + Q(S) + '))';
  end;

  // Comparacion de un campo (texto P, JSON J) con un valor, como AiCompareValues
  function Cmp(const P, J: string; AOp: TFilterOperator; const V: Variant): string;
  var
    Lit: string;
  begin
    if VarIsNull(V) or VarIsEmpty(V) or VarIsArray(V) then
      Exit('(NULL::boolean)');
    case VarType(V) and varTypeMask of
      varBoolean:
        Exit('(lower(' + P + ') IN (''true'', ''false'') AND lower(' + P + ') ' + SqlOp(AOp) + ' ' +
          Q(LowerCase(BoolToStr(V, True))) + ')');
      varDate:
        Exit('((CASE WHEN ' + P + ' ~ ' + Q(DATE_RE) + ' THEN (' + P + ')::timestamp END) ' + SqlOp(AOp) + ' ' +
          Q(FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz', VarToDateTime(V))) + '::timestamp)');
    end;
    if TryNumLit(V, Lit) then
    begin
      if IsNumType(V) then
        Exit('(' + NumOf(P) + ' ' + SqlOp(AOp) + ' ' + Lit + ')');
      // Texto que parece numero: como numero si el campo lo es; si no, como texto
      Exit('(CASE WHEN ' + P + ' ~ ' + Q(NUM_RE) + ' THEN (' + P + ')::numeric ' + SqlOp(AOp) + ' ' + Lit + ' ELSE ' +
        TextCmp(P, AOp, VarToStr(V)) + ' END)');
    end;
    // Texto que no es numero: no se compara con un numero JSON (en memoria tampoco)
    if J <> '' then
      Result := '(coalesce(jsonb_typeof(' + J + '), ''string'') <> ''number'' AND ' + TextCmp(P, AOp, VarToStr(V)) + ')'
    else
      Result := TextCmp(P, AOp, VarToStr(V));
  end;

  // El campo es una lista JSON y algun elemento es igual a V
  function InList(const J: string; const V: Variant): string;
  begin
    Result := 'EXISTS (SELECT 1 FROM jsonb_array_elements_text(' + J + ') AS e(v) WHERE ' + Cmp('e.v', '', foEqual, V) + ')';
  end;

  function ValuesOf(const V: Variant): TArray<Variant>;
  var
    K: Integer;
  begin
    if VarIsArray(V) then
    begin
      SetLength(Result, VarArrayHighBound(V, 1) - VarArrayLowBound(V, 1) + 1);
      for K := 0 to High(Result) do
        Result[K] := VarArrayGet(V, [VarArrayLowBound(V, 1) + K]);
    end
    else
      Result := [V];
  end;

  function IsArrayJson(const J: string): string;
  begin
    Result := 'jsonb_typeof(' + J + ') = ''array''';
  end;

begin
  Parts := TStringList.Create;
  try
    case FOperator of
      foEqual, foNotEqual, foGreater, foGreaterOrEqual, foLess, foLessOrEqual:
        Body := Cmp(FPath, FJsonPath, FOperator, FValue);

      foBetween:
        Body := '(' + Cmp(FPath, FJsonPath, foGreaterOrEqual, FValue) + ' AND ' +
          Cmp(FPath, FJsonPath, foLessOrEqual, FSecondValue) + ')';

      foIn, foNotIn:
        begin
          Values := ValuesOf(FValue);
          for I := 0 to High(Values) do
            Parts.Add(Cmp(FPath, FJsonPath, foEqual, Values[I]));
          if Parts.Count = 0 then
            Body := '(FALSE)'
          else
            Body := '(' + String.Join(' OR ', Parts.ToStringArray) + ')';
          // NOT IN: el campo existe y ningun valor es igual (un NULL adentro cuenta como distinto)
          if FOperator = foNotIn then
            Body := '((' + FPath + ') IS NOT NULL AND NOT COALESCE(' + Body + ', FALSE))';
        end;

      foLike, foILike:
        // '\' escapa % y _ en los dos lados (en memoria, AiLikeMatch)
        Body := '((' + FPath + ') ' + IfThen(FOperator = foILike, 'ILIKE', 'LIKE') + ' ' + Q(AiFilterValueToText(FValue)) + ')';

      foContains:
        begin
          Body := '(position(lower(' + Q(AiFilterValueToText(FValue)) + ') in lower(' + FPath + ')) > 0)';
          if FJsonPath <> '' then
            Body := '(CASE WHEN ' + IsArrayJson(FJsonPath) + ' THEN ' + InList(FJsonPath, FValue) + ' ELSE ' + Body + ' END)';
        end;

      foStartsWith:
        Body := '(left(lower(' + FPath + '), length(' + Q(AiFilterValueToText(FValue)) + ')) = lower(' +
          Q(AiFilterValueToText(FValue)) + '))';
      foEndsWith:
        Body := '(right(lower(' + FPath + '), length(' + Q(AiFilterValueToText(FValue)) + ')) = lower(' +
          Q(AiFilterValueToText(FValue)) + '))';

      foExistsAny, foExistsAll:
        begin
          if FJsonPath = '' then
            Body := '(FALSE)'
          else
          begin
            Values := ValuesOf(FValue);
            for I := 0 to High(Values) do
              Parts.Add(InList(FJsonPath, Values[I]));
            if Parts.Count = 0 then
              Body := '(FALSE)'
            else
              Body := '(' + IsArrayJson(FJsonPath) + ' AND (' +
                String.Join(IfThen(FOperator = foExistsAny, ' OR ', ' AND '), Parts.ToStringArray) + '))';
          end;
        end;

      foExists:
        if FJsonPath = '' then
          Body := '(TRUE)' // content / model: siempre estan
        else
          Body := '((' + FJsonPath + ') IS NOT NULL)';

      foIsNull:
        Body := '((' + FPath + ') IS NULL)';
      foIsNotNull:
        Body := '((' + FPath + ') IS NOT NULL)';
    else
      Body := '(1=1)';
    end;

    // NOT de una condicion: como en memoria, un campo que falta (o NULL) no se cumple
    if FIsNegated then
    begin
      if FOperator in [foIsNull, foIsNotNull, foExists] then
        Body := 'NOT ' + Body
      else
        Body := '((' + FPath + ') IS NOT NULL AND NOT COALESCE(' + Body + ', FALSE))';
    end;
    Result := Body;
  finally
    Parts.Free;
  end;
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
    // Ahora FireDAC encontrar� los par�metros porque ya asignamos el SQL.Text antes
    AQuery.ParamByName(P.Name).Value := P.Value;
  end;
end;

// Ruta de texto del campo. 'text' es la columna content y 'model' la columna model; con
// puntos (user.name) es una ruta dentro del JSON. La clave va entre comillas escapadas: antes
// se pegaba tal cual y una clave con una comilla rompia (o cambiaba) el SQL
function TJSONBFilterBuilder.BuildPath(const AKey: string; AOperator: TFilterOperator): string;
begin
  if SameText(AKey, 'text') then
    Exit('content');
  if SameText(AKey, 'model') then
    Exit('model');
  if AKey.Contains('.') then
    Result := 'properties #>> ' + QuotedStr('{' + String.Join(',', AKey.Split(['.'])) + '}')
  else
    Result := 'properties->>' + QuotedStr(AKey);
end;

// Ruta JSON (para el tipo y las listas); vacia en las columnas
function TJSONBFilterBuilder.BuildJsonPath(const AKey: string): string;
begin
  if SameText(AKey, 'text') or SameText(AKey, 'model') then
    Exit('');
  if AKey.Contains('.') then
    Result := 'properties #> ' + QuotedStr('{' + String.Join(',', AKey.Split(['.'])) + '}')
  else
    Result := 'properties->' + QuotedStr(AKey);
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

  // Tipos at�micos
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
        // Grupo negado NOT ( ... ): CASE lo vuelve verdadero/falso (un NULL adentro cuenta como falso,
        // igual que en memoria) y despues se niega
        if Criterion.Negated then
        begin
          if SubSQL = '' then
            SQLParts.Add('(1=0)')
          else
            SQLParts.Add('(CASE WHEN (' + SubSQL + ') THEN 1 ELSE 0 END = 0)');
        end
        else if SubSQL <> '' then
          SQLParts.Add('(' + SubSQL + ')');
      end
      else
      begin
        // CONDICI�N AT�MICA
        Inc(FParamCounter);

        // 1. Crear la condici�n
        Cond := TJSONBFilterCondition.Create(BuildPath(Criterion.Key, Criterion.Op), Criterion.Op, Criterion.Value, InferDataType(Criterion.Value));
        try
          if Criterion.Op = foBetween then
            Cond.SecondValue := Criterion.Value2;
          Cond.JsonPath := BuildJsonPath(Criterion.Key);
          // NOT LIKE, NOT BETWEEN...: NOT ( ... ); un campo que falta da NULL y no se cumple
          Cond.FIsNegated := Criterion.Negated;

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
    raise Exception.Create(AiStr('rag.pg.no_connection'));
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
  LEnt, LEmbLiteral, LContent, LProp: string;

  function SQLS(const S: string): string;
  begin
    Result := '''' + S.Replace(#0, '').Replace('''', '''''') + '''';
  end;

begin
  Q := NewQuery;
  try
    LEnt     := IfThen(AEntidad = '', FCurrentEntidad, AEntidad);
    LContent := ANode.Text.Replace(#0, '');
    LProp    := PropertiesToJSON(ANode.MetaData);

    if Length(ANode.Data) > 0 then
      LEmbLiteral := 'CAST(' + SQLS(EmbeddingToString(ANode.Data)) + ' AS vector)'
    else
      LEmbLiteral := 'NULL';

    Q.SQL.Text :=
      'INSERT INTO ' + FTableName + ' (entidad, id, model, content, properties, embedding) VALUES (' +
      SQLS(LEnt) + ',' + SQLS(ANode.Tag) + ',' + SQLS(ANode.Model) + ',' +
      SQLS(LContent) + ',' + SQLS(LProp) + '::jsonb,' +
      LEmbLiteral + ') ON CONFLICT (entidad, id) DO UPDATE SET ' +
      'model=EXCLUDED.model,content=EXCLUDED.content,' +
      'properties=EXCLUDED.properties,embedding=EXCLUDED.embedding';
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
    Q.ParamByName('ent').AsWideString := IfThen(AEntidad = '', FCurrentEntidad, AEntidad);
    Q.ParamByName('id').AsWideString := AID;
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
    Q.ParamByName('ent').AsWideString := IfThen(AEntidad = '', FCurrentEntidad, AEntidad);
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
  LTargetEmbedding, LQueryLiteral: string;
  FS: TFormatSettings;
  DoVector, DoLexical, HasValidEmbedding: Boolean;
  MinVectorScore, MinLexicalScore: Double;
  LangConfig, FilterSQL: string;
  HasFilter: Boolean;
  FilterBuilder: TJSONBFilterBuilder;
  VWeight, LWeight: Double;

  function SQLS(const S: string): string;
  begin
    Result := '''' + S.Replace(#0, '').Replace('''', '''''') + '''';
  end;

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
  if DoLexical then
    LQueryLiteral := SQLS(ATarget.Text);

  Q := NewQuery;
  SQL := TStringBuilder.Create;
  FilterBuilder := TJSONBFilterBuilder.Create(Q);
  try
    FilterSQL := '';
    HasFilter := False;
    if Assigned(AFilter) and (AFilter.Count > 0) then
    begin
      FilterSQL := FilterBuilder.BuildSQL(AFilter);
      HasFilter := FilterSQL <> '';
    end;

    // Build SQL with all values inlined — avoids PQexecParams null-byte bug on Linux
    SQL.AppendLine('WITH ');

    // --- BLOQUE VECTOR ---
    SQL.AppendLine('vector_res AS (');
    if DoVector then
    begin
      SQL.AppendLine('  SELECT id, entidad, content, model, properties, embedding,');
      SQL.AppendLine('    (1 - (embedding <=> ' + LTargetEmbedding + '::vector)) as v_score,');
      SQL.AppendLine('    ROW_NUMBER() OVER (ORDER BY embedding <=> ' + LTargetEmbedding + '::vector ASC) as v_rank');
      SQL.AppendLine('  FROM ' + FTableName);
      if LEnt <> '' then
        SQL.AppendLine('  WHERE entidad = ' + SQLS(LEnt))
      else
        SQL.AppendLine('  WHERE 1=1');
      if Trim(ATarget.Model) <> '' then
        SQL.AppendLine('    AND (model = ' + SQLS(ATarget.Model) + ' OR model IS NULL OR model = '''')');
      if MinVectorScore > 0 then
        SQL.AppendLine('    AND (1 - (embedding <=> ' + LTargetEmbedding + '::vector)) >= ' + FloatToStr(MinVectorScore, FS));
      if HasFilter then
        SQL.AppendLine('    AND ' + FilterSQL);
      SQL.AppendLine('  ORDER BY embedding <=> ' + LTargetEmbedding + '::vector');
      SQL.AppendLine('  LIMIT ' + IntToStr(ALimit * 3));
    end
    else
      SQL.AppendLine('  SELECT NULL::text as id, NULL::text as entidad, NULL::text as content, NULL::text as model, NULL::jsonb as properties, NULL::vector as embedding, 0::float as v_score, 0::bigint as v_rank WHERE FALSE');
    SQL.AppendLine('),');

    // --- BLOQUE LEXICAL ---
    SQL.AppendLine('lexical_res AS (');
    if DoLexical then
    begin
      SQL.AppendLine('  SELECT id, entidad, content, model, properties, embedding,');
      SQL.AppendLine('    ts_rank_cd(search_vector, websearch_to_tsquery(''' + LangConfig + ''', ' + LQueryLiteral + '), 32) as l_score,');
      SQL.AppendLine('    ROW_NUMBER() OVER (ORDER BY ts_rank_cd(search_vector, websearch_to_tsquery(''' + LangConfig + ''', ' + LQueryLiteral + '), 32) DESC) as l_rank');
      SQL.AppendLine('  FROM ' + FTableName);
      if LEnt <> '' then
        SQL.AppendLine('  WHERE entidad = ' + SQLS(LEnt) + ' AND search_vector @@ websearch_to_tsquery(''' + LangConfig + ''', ' + LQueryLiteral + ')')
      else
        SQL.AppendLine('  WHERE search_vector @@ websearch_to_tsquery(''' + LangConfig + ''', ' + LQueryLiteral + ')');
      if MinLexicalScore > 0 then
        SQL.AppendLine('    AND ts_rank_cd(search_vector, websearch_to_tsquery(''' + LangConfig + ''', ' + LQueryLiteral + '), 32) >= ' + FloatToStr(MinLexicalScore, FS));
      if HasFilter then
        SQL.AppendLine('    AND ' + FilterSQL);
      SQL.AppendLine('  LIMIT ' + IntToStr(ALimit * 3));
    end
    else
      SQL.AppendLine('  SELECT NULL::text as id, NULL::text as entidad, NULL::text as content, NULL::text as model, NULL::jsonb as properties, NULL::vector as embedding, 0::float as l_score, 0::bigint as l_rank WHERE FALSE');
    SQL.AppendLine('),');

    // --- BLOQUE COMBINE / RRF / SCORE ---
    SQL.AppendLine('combined AS (');
    SQL.AppendLine('  SELECT COALESCE(v.id, l.id) as id, COALESCE(v.entidad, l.entidad) as entidad, COALESCE(v.content, l.content) as content, COALESCE(v.model, l.model) as model, ' +
      'COALESCE(v.properties, l.properties) as properties, COALESCE(v.embedding, l.embedding) as embedding,');
    SQL.AppendLine('    COALESCE(v.v_score, 0) as v_score, COALESCE(l.l_score, 0) as l_score, COALESCE(v.v_rank, 999999) as v_rank, COALESCE(l.l_rank, 999999) as l_rank');
    SQL.AppendLine('  FROM vector_res v FULL OUTER JOIN lexical_res l ON v.id = l.id');
    SQL.AppendLine('),');

    SQL.AppendLine('scored AS (');
    SQL.AppendLine('  SELECT id, entidad, content, model, properties, embedding, v_score, l_score,');

    if DoVector and DoLexical then
    begin
      if Assigned(LOptions) and LOptions.UseRRF then
        SQL.AppendLine('    (1.0 / (60 + v_rank)) + (1.0 / (60 + l_rank)) as raw_score')
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
        SQL.AppendLine('    (v_score * ' + FloatToStr(VWeight, FS) + ') + (l_score * ' + FloatToStr(LWeight, FS) + ') as raw_score');
      end;
    end
    else if DoVector then
      SQL.AppendLine('    v_score as raw_score')
    else
      SQL.AppendLine('    l_score as raw_score');
    SQL.AppendLine('  FROM combined');
    SQL.AppendLine('),');

    SQL.AppendLine('final_scored AS (');
    SQL.AppendLine('  SELECT id, entidad, content, model, properties, embedding,');
    if Assigned(LOptions) and LOptions.UseRRF and DoVector and DoLexical then
      SQL.AppendLine('    (raw_score / ((1.0/61) + (1.0/61))) as final_score')
    else
      SQL.AppendLine('    raw_score as final_score');
    SQL.AppendLine('  FROM scored');
    SQL.AppendLine(')');
    // embedding::text NO es cosmetico: FireDAC mapea el tipo vector de pgvector a
    // ftBlob y AsString devuelve los bytes crudos, que StringToEmbedding no sabe
    // leer (acaba produciendo un vector [0] de un elemento). Los nodos volvian
    // asi SIN vector, y con ellos cualquier calculo posterior sobre el resultado
    // (RERANK por coseno, la diversidad de MMR) operaba sobre ceros. Casteado a
    // texto vuelve como '[0.1,0.2,...]', que es justo lo que espera el parser.
    SQL.AppendLine('SELECT id, entidad, content, model, properties, embedding::text AS embedding, final_score');
    SQL.AppendLine('FROM final_scored');
    if APrecision > 0 then
      SQL.AppendLine('WHERE final_score >= ' + FloatToStr(APrecision, FS));
    SQL.AppendLine('ORDER BY final_score DESC LIMIT ' + IntToStr(ALimit));

    Q.SQL.Text := SQL.ToString;
    FLastSQL := Q.SQL.Text;

    // No parameters — FireDAC uses PQexec (not PQexecParams), avoiding null-byte injection on Linux
    try
      Q.Open;
    except
      on E: Exception do
        raise Exception.CreateFmt(AiStr('rag.pg.error') + sLineBreak + 'SQL: %s', [E.Message, FLastSQL]);
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
        // Set after FromJSON — FromJSON.Clear would erase anything set earlier
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
