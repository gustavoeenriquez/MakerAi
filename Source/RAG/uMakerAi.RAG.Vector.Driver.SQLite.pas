unit uMakerAi.RAG.Vector.Driver.SQLite;

{
  TAiRAGVectorSQLiteDriver — Driver SQLite para TAiRAGVector (MakerAI 3.3)

  Características:
  - Almacenamiento persistente en archivo .db (SQLite vía FireDAC)
  - Búsqueda vectorial por similitud coseno calculada en Delphi
  - Soporte opcional para sqlite-vec: si VecExtensionPath está definido se usa
    vec_distance_cosine() nativo para búsqueda vectorial más rápida y escalable
  - Búsqueda léxical BM25 vía FTS5 (nativo en SQLite 3.9+, sin extensiones)
  - Búsqueda híbrida (vector + BM25) con fusión RRF o ponderada
  - Filtrado de metadatos vía json_extract()
  - Upsert (DELETE+INSERT) con triggers que mantienen FTS5 sincronizado

  Esquema generado por CreateSchema():
    <tabla>        — entidad, id, model, content, properties(JSON TEXT), embedding(TEXT)
    <tabla>_fts    — virtual FTS5 sincronizada por triggers
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
  System.Generics.Defaults,
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
  // TSQLiteFilterBuilder — WHERE clause desde TAiFilterCriteria para SQLite
  // Usa json_extract() en lugar de los operadores JSONB de PostgreSQL.
  // ---------------------------------------------------------------------------
  TSQLiteFilterBuilder = class
  private
    FParamCounter: Integer;
    FTable: string; // tabla con la que se califican las columnas (el JOIN con FTS5 repite id y content)
    FParamNames: TList<string>;
    FParamValues: TList<Variant>;
    function Col(const AName: string): string;
    function BuildPath(const AKey: string): string;
    function BuildJsonPath(const AKey: string): string;
    function ConditionSQL(const C: TFilterCriterion): string;
    function Process(ACriteria: TAiFilterCriteria): string;
  public
    constructor Create;
    destructor Destroy; override;
    function BuildSQL(ACriteria: TAiFilterCriteria; const ATable: string = ''): string;
    procedure ApplyParams(AQuery: TFDQuery);
  end;

  // ---------------------------------------------------------------------------
  // TAiRAGVectorSQLiteDriver
  // ---------------------------------------------------------------------------
  TAiRAGVectorSQLiteDriver = class(TAiVectorStoreDriverBase)
  private
    FConnection: TFDConnection;
    FTableName: string;
    FCurrentEntidad: string;
    FLanguage: TAiLanguage;
    FVecExtensionPath: string;
    FVecExtLoaded: Boolean;
    FLastSQL: string;

    function NewQuery: TFDQuery;
    function FtsTableName: string;
    function PropertiesToJSON(AMeta: TAiEmbeddingMetaData): string;
    function GetFtsTokenizer: string;
    procedure SetTableName(const Value: string);
    procedure SetConnection(const Value: TFDConnection);
    procedure TryLoadVecExtension;

    function VectorSearchDelphi(const ATarget: TAiEmbeddingNode;
      const AEntidad: string; ALimit: Integer; AMinScore: Double;
      const AFilterSQL: string; FB: TSQLiteFilterBuilder): TAiRAGVector;

    function VectorSearchVecExt(const ATarget: TAiEmbeddingNode;
      const AEntidad: string; ALimit: Integer; AMinScore: Double;
      const AFilterSQL: string; FB: TSQLiteFilterBuilder): TAiRAGVector;

    function LexicalSearchFTS5(const ATarget: TAiEmbeddingNode;
      const AEntidad: string; ALimit: Integer; AMinScore: Double;
      const AFilterSQL: string; FB: TSQLiteFilterBuilder): TAiRAGVector;

    function FuseRRF(AVec, ALex: TAiRAGVector; ALimit: Integer): TAiRAGVector;
    function FuseWeighted(AVec, ALex: TAiRAGVector;
      AVW, ALW: Double; ALimit: Integer): TAiRAGVector;

    // Copia un nodo creando una instancia nueva con misma dimensión que ATarget
    function CopyNode(Src: TAiEmbeddingNode; ATargetDim: Integer): TAiEmbeddingNode;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Add(const ANode: TAiEmbeddingNode; const AEntidad: string = ''); override;
    function Search(const ATarget: TAiEmbeddingNode; const AEntidad: string;
      ALimit: Integer; APrecision: Double; AFilter: TAiFilterCriteria;
      Options: TAiSearchOptions): TAiRAGVector; override;
    procedure Delete(const AID: string; const AEntidad: string); override;
    procedure Clear(const AEntidad: string); override;

    procedure CreateSchema(const ABaseTableName: string);
    procedure DropSchema(const ABaseTableName: string);

    function EmbeddingToStr(const AData: TAiEmbeddingData): string;
    function StrToEmbedding(const AStr: string): TAiEmbeddingData;

    property LastSQL: string read FLastSQL;
    property VecExtLoaded: Boolean read FVecExtLoaded;
  published
    property Connection: TFDConnection read FConnection write SetConnection;
    property TableName: string read FTableName write SetTableName;
    property CurrentEntidad: string read FCurrentEntidad write FCurrentEntidad;
    property Language: TAiLanguage read FLanguage write FLanguage default alSpanish;
    // Ruta completa a sqlite_vec.dll/.so — vacío = modo Delphi (brute-force cosine)
    property VecExtensionPath: string read FVecExtensionPath write FVecExtensionPath;
  end;

procedure Register;

implementation

uses
  uMakerAi.Localization;

procedure Register;
begin
  RegisterComponents('MakerAI.RAG.Drivers', [TAiRAGVectorSQLiteDriver]);
end;

// =============================================================================
// TSQLiteFilterBuilder
// =============================================================================

constructor TSQLiteFilterBuilder.Create;
begin
  inherited Create;
  FParamCounter := 0;
  FParamNames  := TList<string>.Create;
  FParamValues := TList<Variant>.Create;
end;

destructor TSQLiteFilterBuilder.Destroy;
begin
  FParamNames.Free;
  FParamValues.Free;
  inherited;
end;

function TSQLiteFilterBuilder.BuildPath(const AKey: string): string;
begin
  if SameText(AKey, 'text') or SameText(AKey, 'content') then Exit(Col('content'));
  if SameText(AKey, 'model')  then Exit(Col('model'));
  if SameText(AKey, 'id')     then Exit(Col('id'));
  Result := 'json_extract(' + Col('properties') + ', ' + QuotedStr(BuildJsonPath(AKey)) + ')';
end;

// Ruta JSON de SQLite con cada parte entre comillas dobles ($."precio", $."a"."b"): una clave
// con espacios, puntos o comillas ya no rompe el SQL (antes se pegaba tal cual: $.mi clave)
function TSQLiteFilterBuilder.BuildJsonPath(const AKey: string): string;
var
  Part: string;
begin
  Result := '$';
  for Part in AKey.Split(['.']) do
    Result := Result + '."' + Part.Replace('"', '').Replace(#0, '') + '"';
end;

function TSQLiteFilterBuilder.Col(const AName: string): string;
begin
  if FTable <> '' then
    Result := FTable + '.' + AName
  else
    Result := AName;
end;

function TSQLiteFilterBuilder.BuildSQL(ACriteria: TAiFilterCriteria; const ATable: string): string;
begin
  FTable := ATable;
  FParamCounter := 0;
  FParamNames.Clear;
  FParamValues.Clear;
  Result := Process(ACriteria);
end;

// Traduce una condicion con la misma semantica que el filtro en memoria
// (TAiEmbeddingMetaData.Evaluate / AiCompareValues) y que el driver de PostgreSQL. Todo va como
// literal. Antes: CONTAINS era igualdad exacta; LIKE no distinguia mayusculas (el LIKE de SQLite
// no las distingue en ASCII); los textos que parecen numeros ('10', '9') se comparaban como
// texto y '10' > 9 no se cumplia; un 'N/A' en una columna de numeros cumplia cualquier '>'
// (en SQLite un texto es mayor que cualquier numero); EXISTS_ANY/EXISTS_ALL no se traducian y
// dejaban pasar todo.
// - Numeros: el campo cuenta como numero si es un numero JSON o un texto que parece numero
//   (GLOB, SQLite no trae expresiones regulares); si no, la condicion no se cumple.
// - Texto: '=' distingue mayusculas; el orden no (desempata con ellas), como CompareText.
// - LIKE con GLOB (distingue mayusculas, '%' y '_' traducidos); ILIKE con LIKE ... ESCAPE.
// - CONTAINS, STARTS_WITH, ENDS_WITH sin mayusculas ni comodines; CONTAINS sobre una lista es
//   pertenencia (json_each).
// - NOT: un campo que falta no se cumple ni negado.
// Diferencia conocida: lower/upper y LIKE de SQLite solo pliegan ASCII ('É' no es 'é'); en memoria
// ILIKE y CONTAINS pliegan todo Unicode.
function TSQLiteFilterBuilder.ConditionSQL(const C: TFilterCriterion): string;
var
  IsColumn: Boolean;
  V, JT, T, Body, S: string;
  Parts: TStringList;
  Values: TArray<Variant>;
  I: Integer;

  function Q(const AText: string): string;
  begin
    Result := QuotedStr(AText.Replace(#0, ''));
  end;

  function IsNumType(const AVal: Variant): Boolean;
  begin
    Result := VarIsNumeric(AVal) and ((VarType(AVal) and varTypeMask) <> varBoolean);
  end;

  // Literal numerico: de un numero o de un texto que parece numero (como AiCompareValues)
  function TryNumLit(const AVal: Variant; out ALit: string): Boolean;
  var
    D: Double;
  begin
    Result := False;
    if VarIsArray(AVal) or VarIsNull(AVal) or VarIsEmpty(AVal) then
      Exit;
    if IsNumType(AVal) then
    begin
      ALit := AiFilterValueToText(AVal);
      Exit(True);
    end;
    if ((VarType(AVal) and varTypeMask) <> varDate) and ((VarType(AVal) and varTypeMask) <> varBoolean) and
      TryStrToFloat(Trim(VarToStr(AVal)), D, TFormatSettings.Invariant) then
    begin
      ALit := FloatToStr(D, TFormatSettings.Invariant);
      Exit(True);
    end;
  end;

  // El texto X parece un numero: [+-] digitos [. digitos] [e [+-] digitos], con espacios a los lados
  function IsNumText(const X: string): string;
  var
    M: string;
  begin
    M := '(CASE WHEN substr(trim(' + X + '), 1, 1) IN (''+'', ''-'') THEN substr(trim(' + X + '), 2) ELSE trim(' + X + ') END)';
    Result := '(' + M + ' <> '''' AND ' + M + ' NOT GLOB ''*[^0-9.eE+-]*'' AND ' + M + ' GLOB ''*[0-9]*'' AND ' +
      M + ' NOT GLOB ''*.*.*'' AND ' + M + ' NOT GLOB ''*[eE]*[eE]*'' AND ' + M + ' NOT GLOB ''*[eE]*.*'' AND ' +
      M + ' NOT GLOB ''[eE.]*[eE]*'' AND ' + M + ' NOT GLOB ''[eE]*'' AND ' + M + ' NOT GLOB ''*[eE]'' AND ' +
      M + ' NOT GLOB ''*[eE][+-]'' AND ' + M + ' NOT GLOB ''[+-]*'' AND ' + M + ' NOT GLOB ''*[^eE][+-]*'')';
  end;

  // Valor numerico del campo (AV valor, AJT tipo JSON) o NULL si no es numero
  function NumOf(const AV, AJT: string): string;
  begin
    Result := '(CASE WHEN ' + AJT + ' IN (''integer'', ''real'') THEN ' + AV + ' WHEN ' + AJT + ' = ''text'' AND ' +
      IsNumText(AV) + ' THEN CAST(trim(' + AV + ') AS REAL) END)';
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

  // Texto contra texto: '=' y '<>' exactos; el orden como CompareText (upper de SQLite pliega
  // solo ASCII, igual que CompareText) y, si empatan, con mayusculas
  function TextCmp(const AT: string; AOp: TFilterOperator; const AS_: string): string;
  var
    StrictOp: string;
  begin
    if AOp in [foEqual, foNotEqual] then
      Exit('(' + AT + ' ' + SqlOp(AOp) + ' ' + Q(AS_) + ')');
    if AOp in [foGreater, foGreaterOrEqual] then
      StrictOp := '>'
    else
      StrictOp := '<';
    Result := '((upper(' + AT + ') ' + StrictOp + ' upper(' + Q(AS_) + ')) OR (upper(' + AT + ') = upper(' + Q(AS_) +
      ') AND ' + AT + ' ' + SqlOp(AOp) + ' ' + Q(AS_) + '))';
  end;

  // Comparacion de un valor (AV, tipo JSON AJT) con AVal, como AiCompareValues
  function Cmp(const AV, AJT: string; AOp: TFilterOperator; const AVal: Variant): string;
  var
    Lit, BoolOf: string;
  begin
    if VarIsNull(AVal) or VarIsEmpty(AVal) or VarIsArray(AVal) then
      Exit('(NULL)');
    case VarType(AVal) and varTypeMask of
      varBoolean:
        begin
          BoolOf := '(CASE WHEN ' + AJT + ' = ''true'' THEN 1 WHEN ' + AJT + ' = ''false'' THEN 0 WHEN ' + AJT +
            ' = ''text'' AND lower(trim(' + AV + ')) = ''true'' THEN 1 WHEN ' + AJT + ' = ''text'' AND lower(trim(' + AV +
            ')) = ''false'' THEN 0 END)';
          Exit('(' + BoolOf + ' ' + SqlOp(AOp) + ' ' + IfThen(Boolean(AVal), '1', '0') + ')');
        end;
      varDate:
        Exit('((CASE WHEN ' + AJT + ' = ''text'' AND ' + AV + ' GLOB ''[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]*'' ' +
          'THEN julianday(' + AV + ') END) ' + SqlOp(AOp) + ' julianday(' +
          Q(FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz', VarToDateTime(AVal))) + '))');
    end;
    if TryNumLit(AVal, Lit) then
    begin
      if IsNumType(AVal) then
        Exit('(' + NumOf(AV, AJT) + ' ' + SqlOp(AOp) + ' ' + Lit + ')');
      // Texto que parece numero: como numero si el campo lo es; si no, como texto
      Exit('(CASE WHEN ' + NumOf(AV, AJT) + ' IS NOT NULL THEN ' + NumOf(AV, AJT) + ' ' + SqlOp(AOp) + ' ' + Lit +
        ' WHEN ' + AJT + ' = ''text'' THEN ' + TextCmp(AV, AOp, VarToStr(AVal)) + ' END)');
    end;
    // Texto que no es numero: solo contra textos (en memoria un numero o una lista no se comparan)
    Result := '(' + AJT + ' = ''text'' AND ' + TextCmp(AV, AOp, VarToStr(AVal)) + ')';
  end;

  // El campo es una lista JSON y algun elemento es igual a AVal
  function InList(const AVal: Variant): string;
  begin
    Result := 'EXISTS (SELECT 1 FROM json_each(' + Col('properties') + ', ' + Q(BuildJsonPath(C.Key)) + ') AS e WHERE ' +
      Cmp('e.value', 'e.type', foEqual, AVal) + ')';
  end;

  function ValuesOf(const AVal: Variant): TArray<Variant>;
  var
    K: Integer;
  begin
    if VarIsArray(AVal) then
    begin
      SetLength(Result, VarArrayHighBound(AVal, 1) - VarArrayLowBound(AVal, 1) + 1);
      for K := 0 to High(Result) do
        Result[K] := VarArrayGet(AVal, [VarArrayLowBound(AVal, 1) + K]);
    end
    else
      Result := [AVal];
  end;

  // LIKE de SQL (con la barra invertida de escape) como GLOB, que distingue mayusculas
  function LikeToGlob(const APattern: string): string;
  var
    K: Integer;
    Ch: Char;
  begin
    Result := '';
    K := 1;
    while K <= Length(APattern) do
    begin
      Ch := APattern[K];
      if (Ch = #92) and (K < Length(APattern)) then
      begin
        Inc(K);
        Ch := APattern[K];
        if CharInSet(Ch, ['*', '?', '[']) then
          Result := Result + '[' + Ch + ']'
        else
          Result := Result + Ch;
      end
      else if Ch = '%' then
        Result := Result + '*'
      else if Ch = '_' then
        Result := Result + '?'
      else if CharInSet(Ch, ['*', '?', '[']) then
        Result := Result + '[' + Ch + ']'
      else
        Result := Result + Ch;
      Inc(K);
    end;
  end;

begin
  IsColumn := SameText(C.Key, 'text') or SameText(C.Key, 'content') or SameText(C.Key, 'model') or SameText(C.Key, 'id');
  if IsColumn then
  begin
    V := BuildPath(C.Key);
    JT := '''text''';
    T := V;
  end
  else
  begin
    V := BuildPath(C.Key);
    JT := 'json_type(' + Col('properties') + ', ' + Q(BuildJsonPath(C.Key)) + ')';
    // Texto del campo: los booleanos de JSON llegan como 1/0
    T := '(CASE ' + JT + ' WHEN ''true'' THEN ''true'' WHEN ''false'' THEN ''false'' ELSE ' + V + ' END)';
  end;

  Parts := TStringList.Create;
  try
    case C.Op of
      foEqual, foNotEqual, foGreater, foGreaterOrEqual, foLess, foLessOrEqual:
        Body := Cmp(V, JT, C.Op, C.Value);

      foBetween:
        Body := '(' + Cmp(V, JT, foGreaterOrEqual, C.Value) + ' AND ' + Cmp(V, JT, foLessOrEqual, C.Value2) + ')';

      foIn, foNotIn:
        begin
          Values := ValuesOf(C.Value);
          for I := 0 to High(Values) do
            Parts.Add(Cmp(V, JT, foEqual, Values[I]));
          if Parts.Count = 0 then
            Body := '(0)'
          else
            Body := '(' + String.Join(' OR ', Parts.ToStringArray) + ')';
          // NOT IN: el campo existe y ningun valor es igual (un NULL adentro cuenta como distinto)
          if C.Op = foNotIn then
            Body := '((' + V + ') IS NOT NULL AND NOT COALESCE(' + Body + ', 0))';
        end;

      foLike:
        Body := '(' + T + ' GLOB ' + Q(LikeToGlob(AiFilterValueToText(C.Value))) + ')';
      foILike:
        Body := '(' + T + ' LIKE ' + Q(AiFilterValueToText(C.Value)) + ' ESCAPE ' + QuotedStr(#92) + ')';

      foContains:
        begin
          S := AiFilterValueToText(C.Value);
          Body := '(instr(lower(' + T + '), lower(' + Q(S) + ')) > 0)';
          if not IsColumn then
            Body := '(CASE WHEN ' + JT + ' = ''array'' THEN ' + InList(C.Value) + ' ELSE ' + Body + ' END)';
        end;

      foStartsWith, foEndsWith:
        begin
          S := AiFilterValueToText(C.Value);
          if S = '' then
            Body := '((' + T + ') IS NOT NULL)'
          else if C.Op = foStartsWith then
            Body := '(substr(lower(' + T + '), 1, ' + IntToStr(Length(S)) + ') = lower(' + Q(S) + '))'
          else
            Body := '(substr(lower(' + T + '), -' + IntToStr(Length(S)) + ') = lower(' + Q(S) + '))';
        end;

      foExistsAny, foExistsAll:
        if IsColumn then
          Body := '(0)'
        else
        begin
          Values := ValuesOf(C.Value);
          for I := 0 to High(Values) do
            Parts.Add(InList(Values[I]));
          if Parts.Count = 0 then
            Body := '(0)'
          else
            Body := '(' + JT + ' = ''array'' AND (' +
              String.Join(IfThen(C.Op = foExistsAny, ' OR ', ' AND '), Parts.ToStringArray) + '))';
        end;

      foExists:
        if IsColumn then
          Body := '(1)'
        else
          Body := '(' + JT + ' IS NOT NULL)'; // la clave esta, aunque valga null

      foIsNull:
        Body := '(' + V + ' IS NULL)';
      foIsNotNull:
        Body := '(' + V + ' IS NOT NULL)';
    else
      Body := '(1=1)';
    end;

    // NOT de una condicion: como en memoria, un campo que falta (o NULL) no se cumple
    if C.Negated then
    begin
      if C.Op in [foIsNull, foIsNotNull, foExists] then
        Body := 'NOT ' + Body
      else
        Body := '((' + V + ') IS NOT NULL AND NOT COALESCE(' + Body + ', 0))';
    end;
    Result := Body;
  finally
    Parts.Free;
  end;
end;

function TSQLiteFilterBuilder.Process(ACriteria: TAiFilterCriteria): string;
var
  I: Integer;
  Criterion: TFilterCriterion;
  Parts: TList<string>;
  LogicStr, SubSQL: string;
begin
  Result := '';
  if (ACriteria = nil) or (ACriteria.Count = 0) then Exit;

  Parts := TList<string>.Create;
  try
    LogicStr := IfThen(ACriteria.LogicalOp = loAnd, ' AND ', ' OR ');

    for I := 0 to ACriteria.Count - 1 do
    begin
      Criterion := ACriteria.Items[I];

      if Criterion.IsGroup then
      begin
        SubSQL := Process(Criterion.SubCriteria);
        // Grupo negado NOT ( ... ): CASE lo vuelve verdadero/falso (un NULL adentro cuenta como falso,
        // igual que en memoria) y despues se niega
        if Criterion.Negated then
        begin
          if SubSQL = '' then
            Parts.Add('(1=0)')
          else
            Parts.Add('(CASE WHEN (' + SubSQL + ') THEN 1 ELSE 0 END = 0)');
        end
        else if SubSQL <> '' then
          Parts.Add('(' + SubSQL + ')');
        Continue;
      end;

      Parts.Add(ConditionSQL(Criterion));
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

procedure TSQLiteFilterBuilder.ApplyParams(AQuery: TFDQuery);
var
  I: Integer;
begin
  for I := 0 to FParamNames.Count - 1 do
    AQuery.ParamByName(FParamNames[I]).Value := FParamValues[I];
end;

// =============================================================================
// TAiRAGVectorSQLiteDriver — Infraestructura
// =============================================================================

constructor TAiRAGVectorSQLiteDriver.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTableName := 'vector_store';
  FCurrentEntidad := 'DEFAULT';
  FLanguage := alSpanish;
  FVecExtensionPath := '';
  FVecExtLoaded := False;
  FLastSQL := '';
end;

procedure TAiRAGVectorSQLiteDriver.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FConnection) then
    FConnection := nil;
end;

procedure TAiRAGVectorSQLiteDriver.SetConnection(const Value: TFDConnection);
begin
  if FConnection = Value then Exit;
  FConnection := Value;
  if Assigned(FConnection) then
    FConnection.FreeNotification(Self);
end;

procedure TAiRAGVectorSQLiteDriver.SetTableName(const Value: string);
begin
  FTableName := Value.ToLower;
end;

function TAiRAGVectorSQLiteDriver.FtsTableName: string;
begin
  Result := FTableName + '_fts';
end;

function TAiRAGVectorSQLiteDriver.GetFtsTokenizer: string;
begin
  case FLanguage of
    alEnglish: Result := 'porter ascii';
  else
    Result := 'unicode61 remove_diacritics 1';
  end;
end;

function TAiRAGVectorSQLiteDriver.NewQuery: TFDQuery;
begin
  if not Assigned(FConnection) then
    raise Exception.Create(AiStr('rag.sqlite.no_connection'));
  Result := TFDQuery.Create(nil);
  Result.Connection := FConnection;
end;

procedure TAiRAGVectorSQLiteDriver.TryLoadVecExtension;
var
  Q: TFDQuery;
begin
  FVecExtLoaded := False;
  if FVecExtensionPath = '' then Exit;
  Q := NewQuery;
  try
    try
      Q.SQL.Text := 'SELECT load_extension(:p)';
      Q.ParamByName('p').AsString := FVecExtensionPath;
      Q.Open;
      Q.Close;
      FVecExtLoaded := True;
    except
      FVecExtLoaded := False; // Silencioso: fallback a Delphi cosine
    end;
  finally
    Q.Free;
  end;
end;

// =============================================================================
// Helpers: embedding ↔ string y metadata ↔ JSON
// =============================================================================

function TAiRAGVectorSQLiteDriver.EmbeddingToStr(const AData: TAiEmbeddingData): string;
var
  I: Integer;
  Parts: TArray<string>;
  FS: TFormatSettings;
begin
  if Length(AData) = 0 then Exit('');
  FS := TFormatSettings.Invariant;
  SetLength(Parts, Length(AData));
  for I := 0 to High(AData) do
    Parts[I] := FloatToStr(AData[I], FS);
  Result := String.Join(',', Parts);
end;

function TAiRAGVectorSQLiteDriver.StrToEmbedding(const AStr: string): TAiEmbeddingData;
var
  Parts: TArray<string>;
  I: Integer;
  FS: TFormatSettings;
begin
  Result := nil;
  if AStr = '' then Exit;
  Parts := AStr.Split([',']);
  SetLength(Result, Length(Parts));
  FS := TFormatSettings.Invariant;
  for I := 0 to High(Parts) do
    if not TryStrToFloat(Trim(Parts[I]), Result[I], FS) then
      Result[I] := 0.0;
end;

function TAiRAGVectorSQLiteDriver.PropertiesToJSON(AMeta: TAiEmbeddingMetaData): string;
var
  J: TJSONObject;
begin
  if not Assigned(AMeta) then Exit('{}');
  J := AMeta.ToJSON;
  try
    Result := J.ToJSON;
  finally
    J.Free;
  end;
end;

function TAiRAGVectorSQLiteDriver.CopyNode(Src: TAiEmbeddingNode; ATargetDim: Integer): TAiEmbeddingNode;
var
  J: TJSONObject;
begin
  Result := TAiEmbeddingNode.Create(ATargetDim);
  try
    Result.Tag   := Src.Tag;
    Result.Text  := Src.Text;
    Result.Model := Src.Model;
    Result.Idx   := Src.Idx;
    if Length(Src.Data) > 0 then
    begin
      Result.Data := Copy(Src.Data);
      Result.SetDataLength(Length(Result.Data));
    end;
    J := Src.MetaData.ToJSON;
    try
      Result.MetaData.FromJSON(J);
    finally
      J.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

// =============================================================================
// Gestión del esquema
// =============================================================================

procedure TAiRAGVectorSQLiteDriver.CreateSchema(const ABaseTableName: string);
var
  Q: TFDQuery;
  TN, FN, Tok: string;
begin
  TN := ABaseTableName.ToLower;
  FN := TN + '_fts';
  FTableName := TN;
  Tok := GetFtsTokenizer;

  Q := NewQuery;
  try
    Q.ExecSQL('PRAGMA journal_mode=WAL');
    Q.ExecSQL('PRAGMA synchronous=NORMAL');

    // Tabla principal
    Q.ExecSQL(
      'CREATE TABLE IF NOT EXISTS ' + TN + ' (' +
      '  entidad    TEXT NOT NULL,' +
      '  id         TEXT NOT NULL,' +
      '  model      TEXT,' +
      '  content    TEXT,' +
      '  properties TEXT NOT NULL DEFAULT ''{}'',' +
      '  embedding  TEXT,' +
      '  PRIMARY KEY (entidad, id)' +
      ')');

    Q.ExecSQL('CREATE INDEX IF NOT EXISTS idx_' + TN + '_ent ON ' + TN + '(entidad)');

    // Tabla FTS5 (virtual)
    Q.ExecSQL(
      'CREATE VIRTUAL TABLE IF NOT EXISTS ' + FN + ' USING fts5(' +
      '  entidad UNINDEXED,' +
      '  id UNINDEXED,' +
      '  content,' +
      '  tokenize=''' + Tok + '''' +
      ')');

    // Triggers para mantener FTS5 sincronizado
    Q.ExecSQL(
      'CREATE TRIGGER IF NOT EXISTS ' + TN + '_ai AFTER INSERT ON ' + TN + ' BEGIN' +
      '  INSERT INTO ' + FN + '(entidad, id, content)' +
      '  VALUES (NEW.entidad, NEW.id, NEW.content);' +
      'END');

    Q.ExecSQL(
      'CREATE TRIGGER IF NOT EXISTS ' + TN + '_ad AFTER DELETE ON ' + TN + ' BEGIN' +
      '  DELETE FROM ' + FN + ' WHERE entidad=OLD.entidad AND id=OLD.id;' +
      'END');

    Q.ExecSQL(
      'CREATE TRIGGER IF NOT EXISTS ' + TN + '_au AFTER UPDATE OF content ON ' + TN + ' BEGIN' +
      '  DELETE FROM ' + FN + ' WHERE entidad=OLD.entidad AND id=OLD.id;' +
      '  INSERT INTO ' + FN + '(entidad, id, content) VALUES (NEW.entidad, NEW.id, NEW.content);' +
      'END');
  finally
    Q.Free;
  end;

  TryLoadVecExtension;
end;

procedure TAiRAGVectorSQLiteDriver.DropSchema(const ABaseTableName: string);
var
  Q: TFDQuery;
  TN: string;
begin
  TN := ABaseTableName.ToLower;
  Q := NewQuery;
  try
    Q.ExecSQL('DROP TRIGGER IF EXISTS ' + TN + '_ai');
    Q.ExecSQL('DROP TRIGGER IF EXISTS ' + TN + '_ad');
    Q.ExecSQL('DROP TRIGGER IF EXISTS ' + TN + '_au');
    Q.ExecSQL('DROP TABLE IF EXISTS '   + TN + '_fts');
    Q.ExecSQL('DROP TABLE IF EXISTS '   + TN);
  finally
    Q.Free;
  end;
end;

// =============================================================================
// CRUD
// =============================================================================

procedure TAiRAGVectorSQLiteDriver.Add(const ANode: TAiEmbeddingNode; const AEntidad: string);
var
  Q: TFDQuery;
  LEnt: string;
begin
  Q := NewQuery;
  try
    LEnt := IfThen(AEntidad = '', FCurrentEntidad, AEntidad);

    // Upsert: eliminar primero dispara trigger _ad en FTS5,
    // luego el INSERT dispara _ai — así FTS5 queda siempre consistente.
    Q.SQL.Text := 'DELETE FROM ' + FTableName + ' WHERE entidad=:ent AND id=:id';
    Q.ParamByName('ent').AsString := LEnt;
    Q.ParamByName('id').AsString  := ANode.Tag;
    Q.ExecSQL;

    Q.SQL.Text :=
      'INSERT INTO ' + FTableName +
      ' (entidad, id, model, content, properties, embedding)' +
      ' VALUES (:ent, :id, :model, :content, :prop, :emb)';
    Q.ParamByName('ent').AsString     := LEnt;
    Q.ParamByName('id').AsString      := ANode.Tag;
    Q.ParamByName('model').AsString   := ANode.Model;
    Q.ParamByName('content').AsString := ANode.Text;
    Q.ParamByName('prop').AsString    := PropertiesToJSON(ANode.MetaData);
    Q.ParamByName('emb').AsString     := EmbeddingToStr(ANode.Data);
    Q.ExecSQL;
  finally
    Q.Free;
  end;
end;

procedure TAiRAGVectorSQLiteDriver.Delete(const AID: string; const AEntidad: string);
var
  Q: TFDQuery;
begin
  Q := NewQuery;
  try
    Q.SQL.Text := 'DELETE FROM ' + FTableName + ' WHERE entidad=:ent AND id=:id';
    Q.ParamByName('ent').AsString := IfThen(AEntidad = '', FCurrentEntidad, AEntidad);
    Q.ParamByName('id').AsString  := AID;
    Q.ExecSQL;
  finally
    Q.Free;
  end;
end;

procedure TAiRAGVectorSQLiteDriver.Clear(const AEntidad: string);
var
  Q: TFDQuery;
begin
  Q := NewQuery;
  try
    Q.SQL.Text := 'DELETE FROM ' + FTableName + ' WHERE entidad=:ent';
    Q.ParamByName('ent').AsString := IfThen(AEntidad = '', FCurrentEntidad, AEntidad);
    Q.ExecSQL;
  finally
    Q.Free;
  end;
end;

// =============================================================================
// Búsqueda vectorial — similitud coseno en Delphi (brute-force, O(n))
// Adecuado para colecciones pequeñas/medianas (≤ 50K nodos).
// =============================================================================

function TAiRAGVectorSQLiteDriver.VectorSearchDelphi(
  const ATarget: TAiEmbeddingNode;
  const AEntidad: string;
  ALimit: Integer;
  AMinScore: Double;
  const AFilterSQL: string;
  FB: TSQLiteFilterBuilder): TAiRAGVector;

  function CosineSim(const A, B: TAiEmbeddingData): Double;
  var
    Dot, NA, NB: Double;
    I: Integer;
  begin
    Dot := 0; NA := 0; NB := 0;
    if Length(A) <> Length(B) then Exit(0.0);
    for I := 0 to High(A) do
    begin
      Dot := Dot + A[I] * B[I];
      NA  := NA  + A[I] * A[I];
      NB  := NB  + B[I] * B[I];
    end;
    NA := Sqrt(NA); NB := Sqrt(NB);
    if (NA < 1e-12) or (NB < 1e-12) then Exit(0.0);
    Result := Dot / (NA * NB);
  end;

type
  TNodeScore = record
    Node: TAiEmbeddingNode;
    Score: Double;
  end;

var
  Q: TFDQuery;
  SQL: string;
  Candidates: TList<TNodeScore>;
  NS: TNodeScore;
  I: Integer;
  PropStr: string;
  JObj: TJSONObject;
begin
  Result := TAiRAGVector.Create(nil, True);
  Candidates := TList<TNodeScore>.Create;
  Q := NewQuery;
  try
    SQL :=
      'SELECT id, content, model, properties, embedding ' +
      'FROM ' + FTableName +
      ' WHERE entidad=:ent AND (embedding IS NOT NULL) AND (embedding <> '''')';
    if AFilterSQL <> '' then
      SQL := SQL + ' AND (' + AFilterSQL + ')';

    Q.SQL.Text := SQL;
    FLastSQL := SQL;
    Q.ParamByName('ent').AsString := AEntidad;
    if AFilterSQL <> '' then FB.ApplyParams(Q);
    Q.Open;

    while not Q.Eof do
    begin
      NS.Node := TAiEmbeddingNode.Create(ATarget.Dim);
      try
        NS.Node.Tag   := Q.FieldByName('id').AsString;
        NS.Node.Text  := Q.FieldByName('content').AsString;
        NS.Node.Model := Q.FieldByName('model').AsString;
        NS.Node.Data  := StrToEmbedding(Q.FieldByName('embedding').AsString);
        if Length(NS.Node.Data) > 0 then
          NS.Node.SetDataLength(Length(NS.Node.Data));
        PropStr := Q.FieldByName('properties').AsString;
        if PropStr <> '' then
        begin
          JObj := TJSONObject.ParseJSONValue(PropStr) as TJSONObject;
          if Assigned(JObj) then
            try NS.Node.MetaData.FromJSON(JObj); finally JObj.Free; end;
        end;
        NS.Score := CosineSim(ATarget.Data, NS.Node.Data);
        Candidates.Add(NS);
      except
        NS.Node.Free;
        raise;
      end;
      Q.Next;
    end;

    // Ordenar por score desc
    Candidates.Sort(TComparer<TNodeScore>.Construct(
      function(const A, B: TNodeScore): Integer
      begin
        if A.Score > B.Score then Result := -1
        else if A.Score < B.Score then Result := 1
        else Result := 0;
      end));

    // Tomar top-ALimit respetando min score
    for I := 0 to Candidates.Count - 1 do
    begin
      if Result.Items.Count >= ALimit then Break;
      if (AMinScore > 0) and (Candidates[I].Score < AMinScore) then Break;
      Candidates[I].Node.Idx := Candidates[I].Score;
      Result.Items.Add(Candidates[I].Node);
      // Marcar como transferido (evitar Free en cleanup)
      var NSMod := Candidates[I];
      NSMod.Node := nil;
      Candidates[I] := NSMod;
    end;

  finally
    // Liberar nodos no transferidos al Result
    for I := 0 to Candidates.Count - 1 do
      Candidates[I].Node.Free;
    Candidates.Free;
    Q.Free;
  end;
end;

// =============================================================================
// Búsqueda vectorial con sqlite-vec (rápida, usa índice vectorial nativo)
// =============================================================================

function TAiRAGVectorSQLiteDriver.VectorSearchVecExt(
  const ATarget: TAiEmbeddingNode;
  const AEntidad: string;
  ALimit: Integer;
  AMinScore: Double;
  const AFilterSQL: string;
  FB: TSQLiteFilterBuilder): TAiRAGVector;
var
  Q: TFDQuery;
  SQL: string;
  Node: TAiEmbeddingNode;
  PropStr: string;
  JObj: TJSONObject;
begin
  Result := TAiRAGVector.Create(nil, True);
  Q := NewQuery;
  try
    // sqlite-vec almacena embeddings como BLOB; aquí asumimos la columna
    // 'embedding' ya fue creada como vec_f32 por el usuario, o usamos
    // la versión TEXT con conversión inline.
    // Distancia coseno sqlite-vec: vec_distance_cosine(a, b) ∈ [0, 2]
    // Similitud coseno ≈ 1 - distancia (rango [−1, 1], usamos max(0, ...))
    SQL :=
      'SELECT id, content, model, properties, embedding,' +
      '  max(0.0, 1.0 - vec_distance_cosine(embedding, vec_f32(:qvec))) AS final_score ' +
      'FROM ' + FTableName +
      ' WHERE entidad=:ent';
    if AMinScore > 0 then
      SQL := SQL + ' AND max(0.0, 1.0 - vec_distance_cosine(embedding, vec_f32(:qvec))) >= :min_s';
    if AFilterSQL <> '' then
      SQL := SQL + ' AND (' + AFilterSQL + ')';
    SQL := SQL +
      ' ORDER BY vec_distance_cosine(embedding, vec_f32(:qvec)) ASC' +
      ' LIMIT :lim';

    Q.SQL.Text := SQL;
    FLastSQL := SQL;
    Q.ParamByName('ent').AsString  := AEntidad;
    Q.ParamByName('qvec').AsString := '[' + EmbeddingToStr(ATarget.Data) + ']';
    Q.ParamByName('lim').AsInteger := ALimit;
    if AMinScore > 0 then Q.ParamByName('min_s').AsFloat := AMinScore;
    if AFilterSQL <> '' then FB.ApplyParams(Q);

    Q.Open;
    while not Q.Eof do
    begin
      Node := TAiEmbeddingNode.Create(ATarget.Dim);
      try
        Node.Tag   := Q.FieldByName('id').AsString;
        Node.Text  := Q.FieldByName('content').AsString;
        Node.Model := Q.FieldByName('model').AsString;
        Node.Idx   := Q.FieldByName('final_score').AsFloat;
        Node.Data  := StrToEmbedding(Q.FieldByName('embedding').AsString);
        if Length(Node.Data) > 0 then Node.SetDataLength(Length(Node.Data));
        PropStr := Q.FieldByName('properties').AsString;
        if PropStr <> '' then
        begin
          JObj := TJSONObject.ParseJSONValue(PropStr) as TJSONObject;
          if Assigned(JObj) then
            try Node.MetaData.FromJSON(JObj); finally JObj.Free; end;
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
  end;
end;

// =============================================================================
// Búsqueda léxical BM25 vía FTS5
// FTS5 bm25() devuelve valores ≤ 0 (más negativo = más relevante).
// Convertimos a score positivo normalizado al rango [0, 1].
// =============================================================================

function TAiRAGVectorSQLiteDriver.LexicalSearchFTS5(
  const ATarget: TAiEmbeddingNode;
  const AEntidad: string;
  ALimit: Integer;
  AMinScore: Double;
  const AFilterSQL: string;
  FB: TSQLiteFilterBuilder): TAiRAGVector;
var
  Q: TFDQuery;
  SQL: string;
  Node: TAiEmbeddingNode;
  PropStr: string;
  JObj: TJSONObject;
  RawScore, MaxRaw, NormScore: Double;
  Candidates: TList<TAiEmbeddingNode>;
  I: Integer;
begin
  Result := TAiRAGVector.Create(nil, True);
  Candidates := TList<TAiEmbeddingNode>.Create;
  Q := NewQuery;
  try
    // JOIN FTS5 con tabla principal para recuperar todos los campos.
    // bm25() negativo → usamos -bm25() para tener valor positivo (mayor = mejor).
    SQL :=
      'SELECT ' + FTableName + '.id, ' + FTableName + '.content, ' + FTableName + '.model, ' +
      FTableName + '.properties, ' + FTableName + '.embedding,' +
      '  (-bm25(' + FtsTableName + ')) AS raw_score ' +
      'FROM ' + FtsTableName + ' AS f ' +
      'JOIN ' + FTableName + ' ON ' + FTableName + '.entidad=f.entidad AND ' + FTableName + '.id=f.id ' +
      'WHERE f.' + FtsTableName + ' MATCH :query AND f.entidad=:ent';
    if AFilterSQL <> '' then
      SQL := SQL + ' AND (' + AFilterSQL + ')';
    SQL := SQL + ' ORDER BY bm25(' + FtsTableName + ') ASC LIMIT :lim';

    Q.SQL.Text := SQL;
    FLastSQL := SQL;
    Q.ParamByName('query').AsString := ATarget.Text;
    Q.ParamByName('ent').AsString   := AEntidad;
    Q.ParamByName('lim').AsInteger  := ALimit * 2; // extra para normalizar
    if AFilterSQL <> '' then FB.ApplyParams(Q);

    try
      Q.Open;
    except
      // Query FTS inválida (caracteres especiales sin escapar) — devolver vacío
      Exit;
    end;

    // Primera pasada: recopilar candidatos y encontrar máximo raw_score
    MaxRaw := 1e-12; // evitar división por cero
    while not Q.Eof do
    begin
      RawScore := Q.FieldByName('raw_score').AsFloat;
      if RawScore > MaxRaw then MaxRaw := RawScore;

      Node := TAiEmbeddingNode.Create(ATarget.Dim);
      try
        Node.Tag   := Q.FieldByName('id').AsString;
        Node.Text  := Q.FieldByName('content').AsString;
        Node.Model := Q.FieldByName('model').AsString;
        Node.Idx   := RawScore; // temporal — se normaliza después
        Node.Data  := StrToEmbedding(Q.FieldByName('embedding').AsString);
        if Length(Node.Data) > 0 then Node.SetDataLength(Length(Node.Data));
        PropStr := Q.FieldByName('properties').AsString;
        if PropStr <> '' then
        begin
          JObj := TJSONObject.ParseJSONValue(PropStr) as TJSONObject;
          if Assigned(JObj) then
            try Node.MetaData.FromJSON(JObj); finally JObj.Free; end;
        end;
        Candidates.Add(Node);
      except
        Node.Free;
        raise;
      end;
      Q.Next;
    end;

    // Segunda pasada: normalizar scores y filtrar
    for I := 0 to Candidates.Count - 1 do
    begin
      if Result.Items.Count >= ALimit then Break;
      NormScore := Candidates[I].Idx / MaxRaw; // [0, 1]
      if (AMinScore > 0) and (NormScore < AMinScore) then Continue;
      Candidates[I].Idx := NormScore;
      Result.Items.Add(Candidates[I]);
      Candidates[I] := nil; // Transferido, no liberar en cleanup
    end;

  finally
    for I := 0 to Candidates.Count - 1 do
      Candidates[I].Free; // Los no transferidos
    Candidates.Free;
    Q.Free;
  end;
end;

// =============================================================================
// Fusión RRF (Reciprocal Rank Fusion)
// =============================================================================

function TAiRAGVectorSQLiteDriver.FuseRRF(AVec, ALex: TAiRAGVector; ALimit: Integer): TAiRAGVector;
const
  K = 60;
var
  Scores: TDictionary<string, Double>;
  NodeMap: TDictionary<string, TAiEmbeddingNode>;
  Pairs: TList<TPair<string, Double>>;
  I: Integer;
  Tag: string;
  RRF: Double;
begin
  Result := TAiRAGVector.Create(nil, True);
  Scores  := TDictionary<string, Double>.Create;
  NodeMap := TDictionary<string, TAiEmbeddingNode>.Create;
  Pairs   := TList<TPair<string, Double>>.Create;
  try
    for I := 0 to AVec.Items.Count - 1 do
    begin
      Tag := AVec.Items[I].Tag;
      RRF := 1.0 / (K + I + 1);
      if Scores.ContainsKey(Tag) then Scores[Tag] := Scores[Tag] + RRF
      else begin Scores.Add(Tag, RRF); NodeMap.Add(Tag, AVec.Items[I]); end;
    end;
    for I := 0 to ALex.Items.Count - 1 do
    begin
      Tag := ALex.Items[I].Tag;
      RRF := 1.0 / (K + I + 1);
      if Scores.ContainsKey(Tag) then Scores[Tag] := Scores[Tag] + RRF
      else begin Scores.Add(Tag, RRF); NodeMap.Add(Tag, ALex.Items[I]); end;
    end;

    for var KV in Scores do
      Pairs.Add(TPair<string, Double>.Create(KV.Key, KV.Value));

    Pairs.Sort(TComparer<TPair<string, Double>>.Construct(
      function(const A, B: TPair<string, Double>): Integer
      begin Result := IfThen(A.Value > B.Value, -1, IfThen(A.Value < B.Value, 1, 0)); end));

    for I := 0 to Min(ALimit - 1, Pairs.Count - 1) do
    begin
      if not NodeMap.ContainsKey(Pairs[I].Key) then Continue;
      var N := CopyNode(NodeMap[Pairs[I].Key], NodeMap[Pairs[I].Key].Dim);
      N.Idx := Pairs[I].Value;
      Result.Items.Add(N);
    end;
  finally
    Scores.Free; NodeMap.Free; Pairs.Free;
  end;
end;

// =============================================================================
// Fusión ponderada (Weighted Score Fusion)
// =============================================================================

function TAiRAGVectorSQLiteDriver.FuseWeighted(AVec, ALex: TAiRAGVector;
  AVW, ALW: Double; ALimit: Integer): TAiRAGVector;
var
  Scores: TDictionary<string, Double>;
  NodeMap: TDictionary<string, TAiEmbeddingNode>;
  Pairs: TList<TPair<string, Double>>;
  I: Integer;
  Tag: string;
begin
  Result := TAiRAGVector.Create(nil, True);
  Scores  := TDictionary<string, Double>.Create;
  NodeMap := TDictionary<string, TAiEmbeddingNode>.Create;
  Pairs   := TList<TPair<string, Double>>.Create;
  try
    for I := 0 to AVec.Items.Count - 1 do
    begin
      Tag := AVec.Items[I].Tag;
      var S := AVec.Items[I].Idx * AVW;
      if Scores.ContainsKey(Tag) then Scores[Tag] := Scores[Tag] + S
      else begin Scores.Add(Tag, S); NodeMap.Add(Tag, AVec.Items[I]); end;
    end;
    for I := 0 to ALex.Items.Count - 1 do
    begin
      Tag := ALex.Items[I].Tag;
      var S := ALex.Items[I].Idx * ALW;
      if Scores.ContainsKey(Tag) then Scores[Tag] := Scores[Tag] + S
      else begin Scores.Add(Tag, S); NodeMap.Add(Tag, ALex.Items[I]); end;
    end;

    for var KV in Scores do
      Pairs.Add(TPair<string, Double>.Create(KV.Key, KV.Value));

    Pairs.Sort(TComparer<TPair<string, Double>>.Construct(
      function(const A, B: TPair<string, Double>): Integer
      begin Result := IfThen(A.Value > B.Value, -1, IfThen(A.Value < B.Value, 1, 0)); end));

    for I := 0 to Min(ALimit - 1, Pairs.Count - 1) do
    begin
      if not NodeMap.ContainsKey(Pairs[I].Key) then Continue;
      var N := CopyNode(NodeMap[Pairs[I].Key], NodeMap[Pairs[I].Key].Dim);
      N.Idx := Pairs[I].Value;
      Result.Items.Add(N);
    end;
  finally
    Scores.Free; NodeMap.Free; Pairs.Free;
  end;
end;

// =============================================================================
// Search — orquestador principal
// =============================================================================

function TAiRAGVectorSQLiteDriver.Search(
  const ATarget: TAiEmbeddingNode;
  const AEntidad: string;
  ALimit: Integer;
  APrecision: Double;
  AFilter: TAiFilterCriteria;
  Options: TAiSearchOptions): TAiRAGVector;
var
  LEnt: string;
  LOptions: TAiSearchOptions;
  FB: TSQLiteFilterBuilder;
  FilterSQL: string;
  DoVector, DoLexical: Boolean;
  MinVec, MinLex: Double;
  VecRes, LexRes: TAiRAGVector;
  Fused: TAiRAGVector;

  procedure TransferFiltered(Src: TAiRAGVector);
  var
    J: Integer;
    N: TAiEmbeddingNode;
  begin
    for J := 0 to Src.Items.Count - 1 do
    begin
      if Result.Items.Count >= ALimit then Break;
      if (APrecision > 0) and (Src.Items[J].Idx < APrecision) then Continue;
      N := CopyNode(Src.Items[J], ATarget.Dim);
      Result.Items.Add(N);
    end;
  end;

begin
  Result := TAiRAGVector.Create(nil, True);
  LEnt := IfThen(AEntidad = '', FCurrentEntidad, AEntidad);

  LOptions := Options;
  if not Assigned(LOptions) and Assigned(Owner) and (Owner is TAiRAGVector) then
    LOptions := TAiRAGVector(Owner).SearchOptions;

  DoVector  := (Length(ATarget.Data) > 0) and ((LOptions = nil) or LOptions.UseEmbeddings);
  DoLexical := Assigned(LOptions) and LOptions.UseBM25 and (Trim(ATarget.Text) <> '');
  if not (DoVector or DoLexical) then Exit;

  // IfThen evalua las dos ramas: con LOptions en nil daba AV.
  MinVec := 0.0;
  MinLex := 0.0;
  if Assigned(LOptions) then
  begin
    MinVec := LOptions.MinAbsoluteScoreEmbedding;
    MinLex := LOptions.MinAbsoluteScoreBM25;
  end;

  FB := TSQLiteFilterBuilder.Create;
  VecRes := nil;
  LexRes := nil;
  try
    FilterSQL := '';
    if Assigned(AFilter) and (AFilter.Count > 0) then
      FilterSQL := FB.BuildSQL(AFilter, FTableName);

    if DoVector then
    begin
      try
        if FVecExtLoaded then
          VecRes := VectorSearchVecExt(ATarget, LEnt, ALimit * 3, MinVec, FilterSQL, FB)
        else
          VecRes := VectorSearchDelphi(ATarget, LEnt, ALimit * 3, MinVec, FilterSQL, FB);
      except
        on E: Exception do
        begin
          FreeAndNil(VecRes);
          if FVecExtLoaded then
          begin
            FVecExtLoaded := False; // sqlite-vec falló en runtime → fallback
            VecRes := VectorSearchDelphi(ATarget, LEnt, ALimit * 3, MinVec, FilterSQL, FB);
          end
          else raise;
        end;
      end;
    end;

    if DoLexical then
      LexRes := LexicalSearchFTS5(ATarget, LEnt, ALimit * 3, MinLex, FilterSQL, FB);

    // Fusión
    if DoVector and DoLexical then
    begin
      if Assigned(LOptions) and LOptions.UseRRF then
        Fused := FuseRRF(VecRes, LexRes, ALimit)
      else
      begin
        // OJO: IfThen es una FUNCION, evalua SIEMPRE las dos ramas.
        // Con LOptions en nil (caso real: ver los Assigned de arriba)
        // IfThen(Assigned(LOptions), LOptions.X, ...) reventaba con AV.
        var VW: Double := 0.7;
        var LW: Double := 0.3;
        if Assigned(LOptions) then
        begin
          VW := LOptions.EmbeddingWeight;
          LW := LOptions.BM25Weight;
        end;
        Fused := FuseWeighted(VecRes, LexRes, VW, LW, ALimit);
      end;
      try
        TransferFiltered(Fused);
      finally
        Fused.Free;
      end;
    end
    else if DoVector and Assigned(VecRes) then
      TransferFiltered(VecRes)
    else if DoLexical and Assigned(LexRes) then
      TransferFiltered(LexRes);

  finally
    FB.Free;
    VecRes.Free;
    LexRes.Free;
  end;
end;

end.
