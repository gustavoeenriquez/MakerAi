unit uMakerAi.RAG.MetaData;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.JSON,
  System.Variants,
  System.StrUtils,
  System.DateUtils,
  System.Math,
  System.VarUtils;

type
  TAiLanguage = (alSpanish, alEnglish, alPortuguese, alCustom);

  { Operador l�gico para determinar c�mo se eval�an los �tems de la lista }
  TLogicalOperator = (loAnd, loOr);

  { Super-set de operadores unificados (Memoria + SQL) }
  TFilterOperator = (
    // B�sicos
    foEqual, // =
    foNotEqual, // <>
    foGreater, // >
    foGreaterOrEqual, // >=
    foLess, // <
    foLessOrEqual, // <=

    // Texto Avanzado
    foContains, // String contiene (LIKE %val%) / JSON @>
    foStartsWith, // String empieza (LIKE val%)
    foEndsWith, // String termina (LIKE %val)
    foLike, // Patr�n SQL est�ndar (con % y _)
    foILike, // Case insensitive Like

    // Listas y Rangos
    foIn, // IN (lista)
    foNotIn, // NOT IN (lista)
    foBetween, // Rango (requiere dos valores)

    // Existencia y Estructura
    foIsNull, // Valor es nulo o clave no existe
    foIsNotNull, // Valor no es nulo y clave existe
    foExists, // La clave existe (independiente del valor)

    // --- AGREGADOS FALTANTES ---
    foExistsAny, // ?| (Alguna de las claves existe en el JSON)
    foExistsAll // ?& (Todas las claves existen en el JSON)
    );

  TAiEmbeddingMetaData = class;
  TAiFilterCriteria = class; // Forward declaration


  { Estructura para almacenar una condici�n individual O un subgrupo }
  TFilterCriterion = record
    Key: string;
    Op: TFilterOperator;
    Value: Variant;
    Value2: Variant; // Usado solo para foBetween

    // --- SOPORTE PARA �RBOL DE DECISI�N (VGQL) ---
    IsGroup: Boolean;
    SubCriteria: TAiFilterCriteria;
    // NOT: en una condición (NOT LIKE, NOT BETWEEN...) se cumple cuando la condición no se
    // cumple y el campo existe (como en SQL, comparar contra un campo que falta no se cumple ni
    // negado); en un grupo, NOT ( ... ) se cumple cuando el grupo no se cumple
    Negated: Boolean;

    constructor Create(const AKey: string; AOp: TFilterOperator; const AVal: Variant); overload;
    constructor Create(const AKey: string; AOp: TFilterOperator; const AVal, AVal2: Variant); overload;
    constructor CreateGroup(ASub: TAiFilterCriteria);
  end;

  { Clase contenedora de criterios de b�squeda complejos (Recursiva) }
  TAiFilterCriteria = class
  private
    FCriteria: TList<TFilterCriterion>;
    FLogicalOp: TLogicalOperator;
    function GetCount: Integer;
    function GetItem(Index: Integer): TFilterCriterion;
  public
    { Constructor con opci�n de definir l�gica (Default = AND para compatibilidad) }
    constructor Create(ALogic: TLogicalOperator = loAnd);
    destructor Destroy; override;

    procedure Clear;

    // M�todos Fluent para construir filtros f�cilmente
    function Add(const Key: string; Op: TFilterOperator; const Value: Variant): TAiFilterCriteria; overload;
    function Add(const Key: string; Op: TFilterOperator; const Value, Value2: Variant): TAiFilterCriteria; overload;

    // M�todo para agregar subgrupos (Necesario para el VGQL Compiler)
    function AddGroup(ALogic: TLogicalOperator): TAiFilterCriteria;

    function AddEqual(const Key: string; const Value: Variant): TAiFilterCriteria;
    function AddGreater(const Key: string; const Value: Variant): TAiFilterCriteria;
    function AddLess(const Key: string; const Value: Variant): TAiFilterCriteria;
    function AddBetween(const Key: string; const Min, Max: Variant): TAiFilterCriteria;
    function AddIn(const Key: string; const Values: Variant): TAiFilterCriteria; // Values puede ser Array Variant
    function AddLike(const Key, Pattern: string; CaseInsensitive: Boolean = True): TAiFilterCriteria;

    // Condición negada (NOT LIKE, NOT BETWEEN...) y subgrupo negado NOT ( ... ). Ver Negated
    function AddNot(const Key: string; Op: TFilterOperator; const Value: Variant): TAiFilterCriteria; overload;
    function AddNot(const Key: string; Op: TFilterOperator; const Value, Value2: Variant): TAiFilterCriteria; overload;
    function AddNotGroup(ALogic: TLogicalOperator): TAiFilterCriteria;

    // Carga de compatibilidad desde MetaData antiguo (Todo foEqual)
    procedure LoadFromMetaData(AMeta: TAiEmbeddingMetaData);

    property LogicalOp: TLogicalOperator read FLogicalOp write FLogicalOp;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TFilterCriterion read GetItem; default;
    property List: TList<TFilterCriterion> read FCriteria;
  end;

  { TAiEmbeddingMetaData: Almac�n de atributos del Nodo }
  TAiEmbeddingMetaData = class
  private
    FProperties: TDictionary<string, Variant>;
    FTagString: string;
    FData: TStrings;
    FTagObject: TObject;

    function GetProperty(const Name: string): Variant;
    procedure SetProperty(const Name: string; const Value: Variant);
    procedure SetData(const Value: TStrings);

    // Helpers de comparaci�n internos
    function CheckInList(const Val: Variant; const List: Variant; Negate: Boolean): Boolean;
    function EvaluateCriterion(const ACriterion: TFilterCriterion; const ANodeText: string): Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Assign(Source: TAiEmbeddingMetaData);
    function Has(const Name: string): Boolean;
    function Get(const Name: string; Default: Variant): Variant;
    procedure Remove(const Name: string);

    { Evalua si este MetaData cumple con un criterio espec�fico }
    function Evaluate(const Name: string; Op: TFilterOperator; const Value: Variant): Boolean; Overload;
    function Evaluate(const Name: string; Op: TFilterOperator; const Value, Value2: Variant; const ANodeText: string = ''): Boolean; Overload;

    { Evalua si este MetaData cumple con el �rbol de criterios }
    function Matches(Criteria: TAiFilterCriteria; const ANodeText: string = ''): Boolean;

    { Serializaci�n JSON }
    function ToJSON: TJSONObject;
    procedure FromJSON(aJSON: TJSONObject);

    property Properties[const Name: string]: Variant read GetProperty write SetProperty; default;
    property InternalDictionary: TDictionary<string, Variant> read FProperties;
    property Data: TStrings read FData write SetData;
    property TagString: string read FTagString write FTagString;
    property TagObject: TObject read FTagObject write FTagObject;
  end;

// Compara dos valores de filtro (lo usan los filtros y el ORDER BY de VQL). Orden de prueba:
// booleanos, fechas (si alguno es TDateTime; el otro puede ser texto ISO 8601), números
// (también textos numéricos con punto decimal, en cualquier idioma de Windows: '10' > '9') y
// textos (sin distinguir mayúsculas y desempatando con ellas, así '=' sí las distingue).
// Devuelve False si no se pueden comparar: un NULL, o un número contra un texto no numérico.
function AiCompareValues(const A, B: Variant; out ACmp: Integer): Boolean;

// LIKE de SQL: '%' cualquier secuencia, '_' un carácter y '\' escapa el siguiente. Antes se
// simulaba con MatchesMask, que no distingue mayúsculas y toma '*', '?' y '[' como comodines.
function AiLikeMatch(const AText, APattern: string; ACaseInsensitive: Boolean): Boolean;

// Texto de un valor para CONTAINS/LIKE: números con punto decimal sin importar el idioma
function AiFilterValueToText(const V: Variant): string;

implementation

type
  TAiLikeToken = record
    Kind: Byte; // 0 = carácter literal, 1 = '_' (uno cualquiera), 2 = '%' (cualquier secuencia)
    Ch: Char;
  end;

function AiIsIntType(const V: Variant): Boolean;
begin
  case VarType(V) and varTypeMask of
    varSmallint, varInteger, varShortInt, varByte, varWord, varLongWord, varInt64:
      Result := True;
  else
    Result := False;
  end;
end;

function AiIsTextType(const V: Variant): Boolean;
var
  T: Word;
begin
  T := VarType(V) and varTypeMask;
  Result := (T = varUString) or (T = varString) or (T = varOleStr);
end;

function AiTryNumber(const V: Variant; out D: Double; out AIsNumberType: Boolean): Boolean;
var
  S: string;
begin
  D := 0;
  AIsNumberType := False;
  case VarType(V) and varTypeMask of
    varSmallint, varInteger, varShortInt, varByte, varWord, varLongWord, varInt64, varUInt64, varSingle, varDouble, varCurrency:
      begin
        AIsNumberType := True;
        D := V;
        Exit(True);
      end;
  end;
  if AiIsTextType(V) then
  begin
    S := Trim(VarToStr(V));
    Exit((S <> '') and TryStrToFloat(S, D, TFormatSettings.Invariant));
  end;
  Result := False;
end;

function AiTryDate(const V: Variant; out DT: TDateTime): Boolean;
begin
  if (VarType(V) and varTypeMask) = varDate then
  begin
    DT := VarToDateTime(V);
    Exit(True);
  end;
  Result := AiIsTextType(V) and TryISO8601ToDate(VarToStr(V), DT);
end;

function AiTryBool(const V: Variant; out B: Boolean): Boolean;
begin
  if (VarType(V) and varTypeMask) = varBoolean then
  begin
    B := V;
    Exit(True);
  end;
  Result := AiIsTextType(V) and TryStrToBool(VarToStr(V), B);
end;

function AiFilterValueToText(const V: Variant): string;
var
  D: Double;
  IsNum: Boolean;
begin
  if VarIsNull(V) or VarIsEmpty(V) then
    Exit('');
  if (not AiIsTextType(V)) and (not AiIsIntType(V)) and AiTryNumber(V, D, IsNum) and IsNum then
    Exit(FloatToStr(D, TFormatSettings.Invariant));
  Result := VarToStr(V);
end;

function AiCompareValues(const A, B: Variant; out ACmp: Integer): Boolean;
var
  DA, DB: Double;
  NumA, NumB, IsNumA, IsNumB: Boolean;
  TA, TB: TDateTime;
  BA, BB: Boolean;
  I1, I2: Int64;
  SA, SB: string;
begin
  ACmp := 0;
  if VarIsNull(A) or VarIsEmpty(A) or VarIsNull(B) or VarIsEmpty(B) or VarIsArray(A) or VarIsArray(B) then
    Exit(False);

  // Booleanos
  if ((VarType(A) and varTypeMask) = varBoolean) or ((VarType(B) and varTypeMask) = varBoolean) then
  begin
    if not(AiTryBool(A, BA) and AiTryBool(B, BB)) then
      Exit(False);
    ACmp := Ord(BA) - Ord(BB);
    Exit(True);
  end;

  // Fechas
  if ((VarType(A) and varTypeMask) = varDate) or ((VarType(B) and varTypeMask) = varDate) then
  begin
    if not(AiTryDate(A, TA) and AiTryDate(B, TB)) then
      Exit(False);
    ACmp := CompareDateTime(TA, TB);
    Exit(True);
  end;

  // Números (los enteros, sin pasar por Double, para no perder precisión en IDs grandes)
  NumA := AiTryNumber(A, DA, IsNumA);
  NumB := AiTryNumber(B, DB, IsNumB);
  if NumA and NumB then
  begin
    if AiIsIntType(A) and AiIsIntType(B) then
    begin
      I1 := A;
      I2 := B;
      ACmp := CompareValue(I1, I2);
    end
    else if SameValue(DA, DB) then
      ACmp := 0
    else
      ACmp := CompareValue(DA, DB);
    Exit(True);
  end;
  // Un número contra un texto que no es número: no se comparan (en SQL el cast falla)
  if IsNumA or IsNumB then
    Exit(False);

  // Textos
  SA := VarToStr(A);
  SB := VarToStr(B);
  ACmp := CompareText(SA, SB);
  if ACmp = 0 then
    ACmp := CompareStr(SA, SB);
  ACmp := Sign(ACmp);
  Result := True;
end;

function AiLikeMatch(const AText, APattern: string; ACaseInsensitive: Boolean): Boolean;
var
  T, P: string;
  Toks: TArray<TAiLikeToken>;
  Tok: TAiLikeToken;
  Prev, Cur, Tmp: TArray<Boolean>;
  I, J, N, M: Integer;
begin
  if ACaseInsensitive then
  begin
    T := AText.ToLower;
    P := APattern.ToLower;
  end
  else
  begin
    T := AText;
    P := APattern;
  end;

  // Patrón -> fichas
  Toks := [];
  I := 1;
  while I <= Length(P) do
  begin
    if (P[I] = '\') and (I < Length(P)) then
    begin
      Inc(I);
      Tok.Kind := 0;
      Tok.Ch := P[I];
    end
    else if P[I] = '%' then
    begin
      Tok.Kind := 2;
      Tok.Ch := #0;
      if (Length(Toks) > 0) and (Toks[High(Toks)].Kind = 2) then
      begin
        Inc(I);
        Continue; // '%%' es lo mismo que '%'
      end;
    end
    else if P[I] = '_' then
    begin
      Tok.Kind := 1;
      Tok.Ch := #0;
    end
    else
    begin
      Tok.Kind := 0;
      Tok.Ch := P[I];
    end;
    Toks := Toks + [Tok];
    Inc(I);
  end;

  // Programación dinámica: Prev[j] = las primeras j fichas cubren el texto leído hasta ahora
  N := Length(T);
  M := Length(Toks);
  SetLength(Prev, M + 1);
  SetLength(Cur, M + 1);
  Prev[0] := True;
  for J := 1 to M do
    Prev[J] := Prev[J - 1] and (Toks[J - 1].Kind = 2);
  for I := 1 to N do
  begin
    Cur[0] := False;
    for J := 1 to M do
      case Toks[J - 1].Kind of
        2:
          Cur[J] := Cur[J - 1] or Prev[J];
        1:
          Cur[J] := Prev[J - 1];
      else
        Cur[J] := Prev[J - 1] and (Toks[J - 1].Ch = T[I]);
      end;
    Tmp := Prev;
    Prev := Cur;
    Cur := Tmp;
  end;
  Result := Prev[M];
end;

{ TFilterCriterion }

constructor TFilterCriterion.Create(const AKey: string; AOp: TFilterOperator; const AVal: Variant);
begin
  Key := AKey;
  Op := AOp;
  Value := AVal;
  Value2 := Unassigned;
  IsGroup := False;
  SubCriteria := nil;
  Negated := False;
end;

constructor TFilterCriterion.Create(const AKey: string; AOp: TFilterOperator; const AVal, AVal2: Variant);
begin
  Key := AKey;
  Op := AOp;
  Value := AVal;
  Value2 := AVal2;
  IsGroup := False;
  SubCriteria := nil;
  Negated := False;
end;

constructor TFilterCriterion.CreateGroup(ASub: TAiFilterCriteria);
begin
  IsGroup := True;
  SubCriteria := ASub;
  Negated := False;
  Key := '';
  Op := foEqual; // No relevante para grupos
  Value := Unassigned;
  Value2 := Unassigned;
end;

{ TAiFilterCriteria }

constructor TAiFilterCriteria.Create(ALogic: TLogicalOperator);
begin
  FCriteria := TList<TFilterCriterion>.Create;
  FLogicalOp := ALogic;
end;

destructor TAiFilterCriteria.Destroy;
var
  C: TFilterCriterion;
begin
  // Liberaci�n recursiva de memoria para grupos anidados
  for C in FCriteria do
  begin
    if C.IsGroup and Assigned(C.SubCriteria) then
      C.SubCriteria.Free;
  end;
  FCriteria.Free;
  inherited;
end;

procedure TAiFilterCriteria.Clear;
var
  C: TFilterCriterion;
begin
  for C in FCriteria do
  begin
    if C.IsGroup and Assigned(C.SubCriteria) then
      C.SubCriteria.Free;
  end;
  FCriteria.Clear;
end;

function TAiFilterCriteria.GetCount: Integer;
begin
  Result := FCriteria.Count;
end;

function TAiFilterCriteria.GetItem(Index: Integer): TFilterCriterion;
begin
  Result := FCriteria[Index];
end;

function TAiFilterCriteria.Add(const Key: string; Op: TFilterOperator; const Value: Variant): TAiFilterCriteria;
begin
  FCriteria.Add(TFilterCriterion.Create(Key, Op, Value));
  Result := Self;
end;

function TAiFilterCriteria.Add(const Key: string; Op: TFilterOperator; const Value, Value2: Variant): TAiFilterCriteria;
begin
  FCriteria.Add(TFilterCriterion.Create(Key, Op, Value, Value2));
  Result := Self;
end;

function TAiFilterCriteria.AddGroup(ALogic: TLogicalOperator): TAiFilterCriteria;
var
  Sub: TAiFilterCriteria;
begin
  Sub := TAiFilterCriteria.Create(ALogic);
  FCriteria.Add(TFilterCriterion.CreateGroup(Sub));
  Result := Sub;
end;

function TAiFilterCriteria.AddEqual(const Key: string; const Value: Variant): TAiFilterCriteria;
begin
  Result := Add(Key, foEqual, Value);
end;

function TAiFilterCriteria.AddGreater(const Key: string; const Value: Variant): TAiFilterCriteria;
begin
  Result := Add(Key, foGreater, Value);
end;

function TAiFilterCriteria.AddLess(const Key: string; const Value: Variant): TAiFilterCriteria;
begin
  Result := Add(Key, foLess, Value);
end;

function TAiFilterCriteria.AddBetween(const Key: string; const Min, Max: Variant): TAiFilterCriteria;
begin
  Result := Add(Key, foBetween, Min, Max);
end;

function TAiFilterCriteria.AddIn(const Key: string; const Values: Variant): TAiFilterCriteria;
begin
  Result := Add(Key, foIn, Values);
end;

function TAiFilterCriteria.AddLike(const Key, Pattern: string; CaseInsensitive: Boolean): TAiFilterCriteria;
begin
  if CaseInsensitive then
    Result := Add(Key, foILike, Pattern)
  else
    Result := Add(Key, foLike, Pattern);
end;

function TAiFilterCriteria.AddNot(const Key: string; Op: TFilterOperator; const Value: Variant): TAiFilterCriteria;
begin
  Result := AddNot(Key, Op, Value, Unassigned);
end;

function TAiFilterCriteria.AddNot(const Key: string; Op: TFilterOperator; const Value, Value2: Variant): TAiFilterCriteria;
var
  C: TFilterCriterion;
begin
  C := TFilterCriterion.Create(Key, Op, Value, Value2);
  C.Negated := True;
  FCriteria.Add(C);
  Result := Self;
end;

function TAiFilterCriteria.AddNotGroup(ALogic: TLogicalOperator): TAiFilterCriteria;
var
  Sub: TAiFilterCriteria;
  C: TFilterCriterion;
begin
  Sub := TAiFilterCriteria.Create(ALogic);
  C := TFilterCriterion.CreateGroup(Sub);
  C.Negated := True;
  FCriteria.Add(C);
  Result := Sub;
end;

procedure TAiFilterCriteria.LoadFromMetaData(AMeta: TAiEmbeddingMetaData);
var
  Pair: TPair<string, Variant>;
begin
  if not Assigned(AMeta) then
    Exit;

  // Compatibilidad legacy
  for Pair in AMeta.InternalDictionary do
    AddEqual(Pair.Key, Pair.Value);
end;

{ TAiEmbeddingMetaData }

constructor TAiEmbeddingMetaData.Create;
begin
  inherited Create;
  FProperties := TDictionary<string, Variant>.Create(TStringComparer.Ordinal);
  FData := TStringList.Create;
  FTagString := '';
  FTagObject := nil;
end;

destructor TAiEmbeddingMetaData.Destroy;
begin
  FData.Free;
  FProperties.Free;
  inherited;
end;

procedure TAiEmbeddingMetaData.Clear;
begin
  FProperties.Clear;
  FData.Clear;
  FTagString := '';
  FTagObject := nil;
end;

procedure TAiEmbeddingMetaData.Assign(Source: TAiEmbeddingMetaData);
var
  Pair: TPair<string, Variant>;
begin
  if not Assigned(Source) then
    Exit;
  Clear;
  FTagString := Source.TagString;
  FTagObject := Source.TagObject;
  FData.Assign(Source.Data);
  for Pair in Source.InternalDictionary do
    SetProperty(Pair.Key, Pair.Value);
end;

function TAiEmbeddingMetaData.Has(const Name: string): Boolean;
begin
  Result := FProperties.ContainsKey(Name);
end;

function TAiEmbeddingMetaData.Get(const Name: string; Default: Variant): Variant;
begin
  if not FProperties.TryGetValue(Name, Result) then
    Result := Default;
end;

procedure TAiEmbeddingMetaData.Remove(const Name: string);
begin
  FProperties.Remove(Name);
end;

function TAiEmbeddingMetaData.GetProperty(const Name: string): Variant;
begin
  if not FProperties.TryGetValue(Name, Result) then
    Result := Null;
end;

procedure TAiEmbeddingMetaData.SetProperty(const Name: string; const Value: Variant);
begin
  FProperties.AddOrSetValue(Name, Value);
end;

procedure TAiEmbeddingMetaData.SetData(const Value: TStrings);
begin
  if Assigned(Value) then
    FData.Assign(Value)
  else
    FData.Clear;
end;

{ --- Helpers de Comparaci�n --- }

function TAiEmbeddingMetaData.CheckInList(const Val: Variant; const List: Variant; Negate: Boolean): Boolean;
var
  i, Cmp: Integer;
begin
  // Mismo criterio que '=' (números, fechas y textos distinguiendo mayúsculas)
  Result := False;
  if not VarIsArray(List) then
    Result := AiCompareValues(Val, List, Cmp) and (Cmp = 0)
  else
    for i := VarArrayLowBound(List, 1) to VarArrayHighBound(List, 1) do
      if AiCompareValues(Val, VarArrayGet(List, [i]), Cmp) and (Cmp = 0) then
      begin
        Result := True;
        Break;
      end;

  if Negate then
    Result := not Result;
end;

{ --- Evalauci�n Principal --- }

function TAiEmbeddingMetaData.Evaluate(const Name: string; Op: TFilterOperator; const Value: Variant): Boolean;
begin
  Result := Evaluate(Name, Op, Value, Unassigned);
end;

function TAiEmbeddingMetaData.Evaluate(const Name: string; Op: TFilterOperator; const Value, Value2: Variant; const ANodeText: string = ''): Boolean;
var
  PropValue: Variant;
  Exists: Boolean;
  i, Cmp, Cmp2: Integer;
begin
  // 1. Inyecci�n de campo virtual 'text' (Contenido del nodo)
  if SameText(Name, 'text') then
  begin
    PropValue := ANodeText;
    Exists := True;
  end
  else
    Exists := FProperties.TryGetValue(Name, PropValue);

  // 2. Evaluaci�n de existencia y nulidad
  case Op of
    foExists:
      Exit(Exists);
    foIsNull:
      Exit(not Exists or VarIsNull(PropValue));
    foIsNotNull:
      Exit(Exists and not VarIsNull(PropValue));
  end;

  // Si la propiedad no existe o es nula, no puede cumplir criterios de comparaci�n
  if not Exists or VarIsNull(PropValue) then
    Exit(False);

  // 3. Operadores de lista (IN / NOT IN)
  if Op in [foIn, foNotIn] then
    Exit(CheckInList(PropValue, Value, Op = foNotIn));

  // 4. Operadores de Conjuntos (Arrays JSON) - ExistsAny / ExistsAll
  if Op in [foExistsAny, foExistsAll] then
  begin
    // Solo soportamos si la propiedad es un Array (ej: Tags: ["A", "B"])
    if VarIsArray(PropValue) then
    begin
      if VarIsArray(Value) then
      begin
        if Op = foExistsAny then
        begin
          for i := VarArrayLowBound(Value, 1) to VarArrayHighBound(Value, 1) do
            if CheckInList(VarArrayGet(Value, [i]), PropValue, False) then
              Exit(True);
          Exit(False);
        end
        else // foExistsAll
        begin
          for i := VarArrayLowBound(Value, 1) to VarArrayHighBound(Value, 1) do
            if not CheckInList(VarArrayGet(Value, [i]), PropValue, False) then
              Exit(False);
          Exit(True);
        end;
      end
      else
        Exit(CheckInList(Value, PropValue, False)); // Valor simple: CONTAINS en la lista
    end;
    Exit(False);
  end;

  // 5. Patrones: LIKE distingue mayúsculas, ILIKE no (como en SQL)
  if Op in [foLike, foILike] then
    Exit(AiLikeMatch(AiFilterValueToText(PropValue), AiFilterValueToText(Value), Op = foILike));

  // 6. Texto parcial, sin distinguir mayúsculas
  case Op of
    foContains:
      Exit(ContainsText(AiFilterValueToText(PropValue), AiFilterValueToText(Value)));
    foStartsWith:
      Exit(StartsText(AiFilterValueToText(Value), AiFilterValueToText(PropValue)));
    foEndsWith:
      Exit(EndsText(AiFilterValueToText(Value), AiFilterValueToText(PropValue)));
  end;

  // 7. Rango
  if Op = foBetween then
    Exit(AiCompareValues(PropValue, Value, Cmp) and AiCompareValues(PropValue, Value2, Cmp2) and (Cmp >= 0) and (Cmp2 <= 0));

  // 8. Comparaciones: números (también en texto), fechas, booleanos y textos. Antes '>=' y '<='
  // sobre texto nunca se cumplían y '10' > 9 comparaba como texto
  if not AiCompareValues(PropValue, Value, Cmp) then
    Exit(False);
  case Op of
    foEqual:
      Result := Cmp = 0;
    foNotEqual:
      Result := Cmp <> 0;
    foGreater:
      Result := Cmp > 0;
    foGreaterOrEqual:
      Result := Cmp >= 0;
    foLess:
      Result := Cmp < 0;
    foLessOrEqual:
      Result := Cmp <= 0;
  else
    Result := False;
  end;
end;

function TAiEmbeddingMetaData.EvaluateCriterion(const ACriterion: TFilterCriterion; const ANodeText: string): Boolean;
var
  V: Variant;
begin
  Result := Evaluate(ACriterion.Key, ACriterion.Op, ACriterion.Value, ACriterion.Value2, ANodeText);
  if not ACriterion.Negated then
    Exit;
  // Como en SQL: comparar contra un campo que falta (o NULL) no se cumple, tampoco negado
  if not(ACriterion.Op in [foIsNull, foIsNotNull, foExists]) and not SameText(ACriterion.Key, 'text') then
    if not FProperties.TryGetValue(ACriterion.Key, V) or VarIsNull(V) then
      Exit(False);
  Result := not Result;
end;

{ --- Nuevo Matches Recursivo (Composite Pattern) --- }

function TAiEmbeddingMetaData.Matches(Criteria: TAiFilterCriteria; const ANodeText: string = ''): Boolean;
var
  Criterion: TFilterCriterion;
  CurrentMatch: Boolean;
begin
  if (Criteria = nil) or (Criteria.Count = 0) then
    Exit(True);

  // L�gica AND: Asumimos True y salimos al primer False
  if Criteria.LogicalOp = loAnd then
  begin
    Result := True;
    for Criterion in Criteria.List do
    begin
      // Si es grupo, recursividad
      if Criterion.IsGroup then
        CurrentMatch := Matches(Criterion.SubCriteria, ANodeText) xor Criterion.Negated
      else
        CurrentMatch := EvaluateCriterion(Criterion, ANodeText);

      if not CurrentMatch then
      begin
        Result := False;
        Break;
      end;
    end;
  end
  // L�gica OR: Asumimos False y salimos al primer True
  else
  begin
    Result := False;
    for Criterion in Criteria.List do
    begin
      if Criterion.IsGroup then
        CurrentMatch := Matches(Criterion.SubCriteria, ANodeText) xor Criterion.Negated
      else
        CurrentMatch := EvaluateCriterion(Criterion, ANodeText);

      if CurrentMatch then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

{ --- JSON --- }

function TAiEmbeddingMetaData.ToJSON: TJSONObject;
var
  Pair: TPair<string, Variant>;
  JVal: TJSONValue;
  V: Variant;
begin
  Result := TJSONObject.Create;
  try
    if not FTagString.IsEmpty then
      Result.AddPair('tagString', FTagString);

    if FData.Count > 0 then
      Result.AddPair('_data_text', FData.Text);

    for Pair in FProperties do
    begin
      V := Pair.Value;
      case VarType(V) and varTypeMask of
        varSmallint, varInteger, varByte, varShortInt, varWord, varLongWord, varInt64:
          JVal := TJSONNumber.Create(Int64(V));
        varSingle, varDouble, varCurrency:
          JVal := TJSONNumber.Create(Double(V));
        varBoolean:
          JVal := TJSONBool.Create(Boolean(V));
        varDate:
          JVal := TJSONString.Create(DateToISO8601(TDateTime(V), True));
        varNull, varEmpty:
          JVal := TJSONNull.Create;
      else
        JVal := TJSONString.Create(VarToStr(V));
      end;
      Result.AddPair(Pair.Key, JVal);
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure TAiEmbeddingMetaData.FromJSON(aJSON: TJSONObject);
var
  i: Integer;
  Pair: TJSONPair;
  S: string;
  DT: TDateTime;
begin
  if not Assigned(aJSON) then
    Exit;
  Clear;
  for i := 0 to aJSON.Count - 1 do
  begin
    Pair := aJSON.Pairs[i];
    S := Pair.JsonString.Value;

    if SameText(S, 'tagString') then
      FTagString := Pair.JsonValue.Value
    else if SameText(S, '_data_text') then
      FData.Text := Pair.JsonValue.Value
    else
    begin
      if Pair.JsonValue is TJSONNumber then
      begin
        if ContainsText(Pair.JsonValue.ToJSON, '.') then
          SetProperty(S, TJSONNumber(Pair.JsonValue).AsDouble)
        else
          SetProperty(S, TJSONNumber(Pair.JsonValue).AsInt64);
      end
      else if Pair.JsonValue is TJSONBool then
        SetProperty(S, TJSONBool(Pair.JsonValue).AsBoolean)
      else if Pair.JsonValue is TJSONNull then
        SetProperty(S, Null)
      else
      begin
        if TryISO8601ToDate(Pair.JsonValue.Value, DT) then
          SetProperty(S, DT)
        else
          SetProperty(S, Pair.JsonValue.Value);
      end;
    end;
  end;
end;

end.
