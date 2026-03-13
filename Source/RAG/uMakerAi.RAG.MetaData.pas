// MIT License - Copyright (c) 2024-2026 Gustavo Enriquez
// FPC PORT - uMakerAi.RAG.MetaData
// Tipos de metadatos, criterios de filtro y operadores para RAG.
unit uMakerAi.RAG.MetaData;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  SysUtils, Classes, Variants, Math, StrUtils, DateUtils, fpmasks,
  fpjson;

type
  TAiLanguage = (alSpanish, alEnglish, alPortuguese, alCustom);

  TLogicalOperator = (loAnd, loOr);

  TFilterOperator = (
    foEqual,          // =
    foNotEqual,       // <>
    foGreater,        // >
    foGreaterOrEqual, // >=
    foLess,           // <
    foLessOrEqual,    // <=
    // Texto avanzado
    foContains,       // LIKE %val%
    foStartsWith,     // LIKE val%
    foEndsWith,       // LIKE %val
    foLike,           // Patrón SQL estándar
    foILike,          // Case-insensitive LIKE
    // Listas y rangos
    foIn,             // IN (lista)
    foNotIn,          // NOT IN
    foBetween,        // Rango
    // Existencia
    foIsNull,
    foIsNotNull,
    foExists,
    // Conjuntos JSON
    foExistsAny,      // ?| alguna clave existe
    foExistsAll       // ?& todas las claves existen
  );

  TAiEmbeddingMetaData = class;
  TAiFilterCriteria = class;

  TFilterCriterion = record
  public
    Key        : string;
    Op         : TFilterOperator;
    Value      : Variant;
    Value2     : Variant;
    IsGroup    : Boolean;
    SubCriteria: TAiFilterCriteria;
    constructor Create(const AKey: string; AOp: TFilterOperator; const AVal: Variant); overload;
    constructor Create(const AKey: string; AOp: TFilterOperator; const AVal, AVal2: Variant); overload;
    constructor CreateGroup(ASub: TAiFilterCriteria);
  end;

  // Clase contenedora de criterios (árbol recursivo)
  TAiFilterCriteria = class
  private
    FCriteria  : array of TFilterCriterion;
    FCount     : Integer;
    FLogicalOp : TLogicalOperator;
    function GetCount: Integer;
    function GetItem(Index: Integer): TFilterCriterion;
    procedure InternalAdd(const C: TFilterCriterion);
  public
    constructor Create(ALogic: TLogicalOperator = loAnd);
    destructor Destroy; override;
    procedure Clear;
    function Add(const Key: string; Op: TFilterOperator; const Value: Variant): TAiFilterCriteria; overload;
    function Add(const Key: string; Op: TFilterOperator; const Value, Value2: Variant): TAiFilterCriteria; overload;
    function AddGroup(ALogic: TLogicalOperator): TAiFilterCriteria;
    function AddEqual(const Key: string; const Value: Variant): TAiFilterCriteria;
    function AddGreater(const Key: string; const Value: Variant): TAiFilterCriteria;
    function AddLess(const Key: string; const Value: Variant): TAiFilterCriteria;
    function AddBetween(const Key: string; const Min, Max: Variant): TAiFilterCriteria;
    function AddIn(const Key: string; const Values: Variant): TAiFilterCriteria;
    function AddLike(const Key, Pattern: string; CaseInsensitive: Boolean = True): TAiFilterCriteria;
    procedure LoadFromMetaData(AMeta: TAiEmbeddingMetaData);
    property LogicalOp: TLogicalOperator read FLogicalOp write FLogicalOp;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TFilterCriterion read GetItem; default;
  end;

  // Wrapper para almacenar Variant en TStringList.Objects
  TAiVariantItem = class
    Value: Variant;
    constructor Create(const V: Variant);
  end;

  // Almacén de atributos del nodo RAG
  TAiEmbeddingMetaData = class
  private
    FProperties: TStringList; // sorted, stores TAiVariantItem as Objects
    FTagString : string;
    FData      : TStrings;
    FTagObject : TObject;

    function GetProperty(const Name: string): Variant;
    procedure SetProperty(const Name: string; const Value: Variant);
    procedure SetData(const Value: TStrings);
    function TryGetPropValue(const Name: string; out Val: Variant): Boolean;
    procedure AddOrSetPropValue(const Name: string; const Val: Variant);

    // Helpers de comparación internos
    function CompareNumbers(const A, B: Variant; Op: TFilterOperator): Boolean;
    function CompareStrings(const A, B: string; Op: TFilterOperator): Boolean;
    function CheckInList(const Val: Variant; const List: Variant; Negate: Boolean): Boolean;
    function CheckLike(const Val, Pattern: string; CaseInsensitive: Boolean): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Source: TAiEmbeddingMetaData);
    function Has(const Name: string): Boolean;
    function Get(const Name: string; Default: Variant): Variant;
    procedure Remove(const Name: string);
    function Evaluate(const Name: string; Op: TFilterOperator; const Value: Variant): Boolean; overload;
    function Evaluate(const Name: string; Op: TFilterOperator; const Value, Value2: Variant; const ANodeText: string = ''): Boolean; overload;
    function Matches(Criteria: TAiFilterCriteria; const ANodeText: string = ''): Boolean;
    function ToJSON: TJSONObject;
    procedure FromJSON(aJSON: TJSONObject);
    // Acceso indexado a propiedades (reemplaza InternalDictionary)
    function GetPropCount: Integer;
    function GetPropKey(I: Integer): string;
    function GetPropValue(I: Integer): Variant;
    property Properties[const Name: string]: Variant read GetProperty write SetProperty; default;
    property Data     : TStrings read FData      write SetData;
    property TagString: string   read FTagString write FTagString;
    property TagObject: TObject  read FTagObject write FTagObject;
  end;

implementation

// ---------------------------------------------------------------------------
// TAiVariantItem
// ---------------------------------------------------------------------------

constructor TAiVariantItem.Create(const V: Variant);
begin
  inherited Create;
  Value := V;
end;

// ---------------------------------------------------------------------------
// TFilterCriterion (record con constructores)
// ---------------------------------------------------------------------------

constructor TFilterCriterion.Create(const AKey: string; AOp: TFilterOperator;
    const AVal: Variant);
begin
  Key         := AKey;
  Op          := AOp;
  Value       := AVal;
  Value2      := Unassigned;
  IsGroup     := False;
  SubCriteria := nil;
end;

constructor TFilterCriterion.Create(const AKey: string; AOp: TFilterOperator;
    const AVal, AVal2: Variant);
begin
  Key         := AKey;
  Op          := AOp;
  Value       := AVal;
  Value2      := AVal2;
  IsGroup     := False;
  SubCriteria := nil;
end;

constructor TFilterCriterion.CreateGroup(ASub: TAiFilterCriteria);
begin
  IsGroup     := True;
  SubCriteria := ASub;
  Key         := '';
  Op          := foEqual;
  Value       := Unassigned;
  Value2      := Unassigned;
end;

// ---------------------------------------------------------------------------
// TAiFilterCriteria
// ---------------------------------------------------------------------------

constructor TAiFilterCriteria.Create(ALogic: TLogicalOperator);
begin
  inherited Create;
  FLogicalOp := ALogic;
  FCount     := 0;
end;

destructor TAiFilterCriteria.Destroy;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    if FCriteria[I].IsGroup and Assigned(FCriteria[I].SubCriteria) then
      FCriteria[I].SubCriteria.Free;
  inherited;
end;

procedure TAiFilterCriteria.InternalAdd(const C: TFilterCriterion);
begin
  if FCount >= Length(FCriteria) then
    SetLength(FCriteria, FCount + 16);
  FCriteria[FCount] := C;
  Inc(FCount);
end;

procedure TAiFilterCriteria.Clear;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    if FCriteria[I].IsGroup and Assigned(FCriteria[I].SubCriteria) then
      FCriteria[I].SubCriteria.Free;
  FCount := 0;
  SetLength(FCriteria, 0);
end;

function TAiFilterCriteria.GetCount: Integer;
begin
  Result := FCount;
end;

function TAiFilterCriteria.GetItem(Index: Integer): TFilterCriterion;
begin
  Result := FCriteria[Index];
end;

function TAiFilterCriteria.Add(const Key: string; Op: TFilterOperator;
    const Value: Variant): TAiFilterCriteria;
begin
  InternalAdd(TFilterCriterion.Create(Key, Op, Value));
  Result := Self;
end;

function TAiFilterCriteria.Add(const Key: string; Op: TFilterOperator;
    const Value, Value2: Variant): TAiFilterCriteria;
begin
  InternalAdd(TFilterCriterion.Create(Key, Op, Value, Value2));
  Result := Self;
end;

function TAiFilterCriteria.AddGroup(ALogic: TLogicalOperator): TAiFilterCriteria;
var
  Sub: TAiFilterCriteria;
begin
  Sub := TAiFilterCriteria.Create(ALogic);
  InternalAdd(TFilterCriterion.CreateGroup(Sub));
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

procedure TAiFilterCriteria.LoadFromMetaData(AMeta: TAiEmbeddingMetaData);
var
  I: Integer;
begin
  if not Assigned(AMeta) then Exit;
  for I := 0 to AMeta.GetPropCount - 1 do
    AddEqual(AMeta.GetPropKey(I), AMeta.GetPropValue(I));
end;

// ---------------------------------------------------------------------------
// TAiEmbeddingMetaData — gestión de propiedades con TStringList
// ---------------------------------------------------------------------------

constructor TAiEmbeddingMetaData.Create;
begin
  inherited Create;
  FProperties := TStringList.Create;
  FProperties.Sorted   := True;
  FProperties.Duplicates := dupIgnore;
  FData      := TStringList.Create;
  FTagString := '';
  FTagObject := nil;
end;

destructor TAiEmbeddingMetaData.Destroy;
var
  I: Integer;
begin
  for I := 0 to FProperties.Count - 1 do
    FProperties.Objects[I].Free;
  FProperties.Free;
  FData.Free;
  inherited;
end;

procedure TAiEmbeddingMetaData.Clear;
var
  I: Integer;
begin
  for I := 0 to FProperties.Count - 1 do
    FProperties.Objects[I].Free;
  FProperties.Clear;
  FData.Clear;
  FTagString := '';
  FTagObject := nil;
end;

procedure TAiEmbeddingMetaData.Assign(Source: TAiEmbeddingMetaData);
var
  I: Integer;
begin
  if not Assigned(Source) then Exit;
  Clear;
  FTagString := Source.FTagString;
  FTagObject := Source.FTagObject;
  FData.Assign(Source.FData);
  for I := 0 to Source.GetPropCount - 1 do
    AddOrSetPropValue(Source.GetPropKey(I), Source.GetPropValue(I));
end;

function TAiEmbeddingMetaData.Has(const Name: string): Boolean;
begin
  Result := FProperties.IndexOf(Name) >= 0;
end;

function TAiEmbeddingMetaData.Get(const Name: string; Default: Variant): Variant;
var
  I: Integer;
begin
  I := FProperties.IndexOf(Name);
  if I >= 0 then
    Result := TAiVariantItem(FProperties.Objects[I]).Value
  else
    Result := Default;
end;

procedure TAiEmbeddingMetaData.Remove(const Name: string);
var
  I: Integer;
begin
  I := FProperties.IndexOf(Name);
  if I >= 0 then
  begin
    FProperties.Objects[I].Free;
    FProperties.Delete(I);
  end;
end;

function TAiEmbeddingMetaData.GetProperty(const Name: string): Variant;
var
  I: Integer;
begin
  I := FProperties.IndexOf(Name);
  if I >= 0 then
    Result := TAiVariantItem(FProperties.Objects[I]).Value
  else
    Result := Null;
end;

procedure TAiEmbeddingMetaData.SetProperty(const Name: string; const Value: Variant);
begin
  AddOrSetPropValue(Name, Value);
end;

procedure TAiEmbeddingMetaData.SetData(const Value: TStrings);
begin
  if Assigned(Value) then FData.Assign(Value)
  else FData.Clear;
end;

function TAiEmbeddingMetaData.TryGetPropValue(const Name: string; out Val: Variant): Boolean;
var
  I: Integer;
begin
  I := FProperties.IndexOf(Name);
  Result := I >= 0;
  if Result then
    Val := TAiVariantItem(FProperties.Objects[I]).Value;
end;

procedure TAiEmbeddingMetaData.AddOrSetPropValue(const Name: string; const Val: Variant);
var
  I: Integer;
begin
  I := FProperties.IndexOf(Name);
  if I >= 0 then
    TAiVariantItem(FProperties.Objects[I]).Value := Val
  else
    FProperties.AddObject(Name, TAiVariantItem.Create(Val));
end;

function TAiEmbeddingMetaData.GetPropCount: Integer;
begin
  Result := FProperties.Count;
end;

function TAiEmbeddingMetaData.GetPropKey(I: Integer): string;
begin
  Result := FProperties[I];
end;

function TAiEmbeddingMetaData.GetPropValue(I: Integer): Variant;
begin
  Result := TAiVariantItem(FProperties.Objects[I]).Value;
end;

// ---------------------------------------------------------------------------
// Helpers de comparación
// ---------------------------------------------------------------------------

function TAiEmbeddingMetaData.CompareNumbers(const A, B: Variant;
    Op: TFilterOperator): Boolean;
var
  D1, D2  : Double;
  I1, I2  : Int64;
  TypeA   : Word;
  TypeB   : Word;
  IsIntA  : Boolean;
  IsIntB  : Boolean;
begin
  TypeA  := VarType(A) and varTypeMask;
  TypeB  := VarType(B) and varTypeMask;
  IsIntA := (TypeA = varInt64) or (TypeA = varInteger) or (TypeA = varSmallint) or
            (TypeA = varByte)  or (TypeA = varShortInt) or (TypeA = varWord) or
            (TypeA = varLongWord);
  IsIntB := (TypeB = varInt64) or (TypeB = varInteger) or (TypeB = varSmallint) or
            (TypeB = varByte)  or (TypeB = varShortInt) or (TypeB = varWord) or
            (TypeB = varLongWord);

  if IsIntA and IsIntB then
  begin
    try
      I1 := A;
      I2 := B;
      case Op of
        foEqual:          Result := I1 = I2;
        foNotEqual:       Result := I1 <> I2;
        foGreater:        Result := I1 > I2;
        foGreaterOrEqual: Result := I1 >= I2;
        foLess:           Result := I1 < I2;
        foLessOrEqual:    Result := I1 <= I2;
      else
        Result := False;
      end;
      Exit;
    except
      // Caer a Double si falla la conversión entero
    end;
  end;

  try
    D1 := Double(A);
    D2 := Double(B);
  except
    Result := False;
    Exit;
  end;

  case Op of
    foEqual:          Result := SameValue(D1, D2, 0.00001);
    foNotEqual:       Result := not SameValue(D1, D2, 0.00001);
    foGreater:        Result := D1 > D2;
    foGreaterOrEqual: Result := D1 >= D2;
    foLess:           Result := D1 < D2;
    foLessOrEqual:    Result := D1 <= D2;
  else
    Result := False;
  end;
end;

function TAiEmbeddingMetaData.CompareStrings(const A, B: string;
    Op: TFilterOperator): Boolean;
begin
  case Op of
    foEqual:      Result := SameText(A, B);
    foNotEqual:   Result := not SameText(A, B);
    foContains:   Result := ContainsText(A, B);
    foStartsWith: Result := StartsText(B, A);
    foEndsWith:   Result := EndsText(B, A);
    foGreater:    Result := CompareText(A, B) > 0;
    foLess:       Result := CompareText(A, B) < 0;
  else
    Result := False;
  end;
end;

function TAiEmbeddingMetaData.CheckLike(const Val, Pattern: string;
    CaseInsensitive: Boolean): Boolean;
var
  LVal, LPat: string;
begin
  LPat := StringReplace(StringReplace(Pattern, '%', '*', [rfReplaceAll]),
                        '_', '?', [rfReplaceAll]);
  LVal := Val;
  if CaseInsensitive then
  begin
    LPat := LowerCase(LPat);
    LVal := LowerCase(LVal);
  end;
  Result := MatchesMask(LVal, LPat);
end;

function TAiEmbeddingMetaData.CheckInList(const Val: Variant;
    const List: Variant; Negate: Boolean): Boolean;
var
  I      : Integer;
  Item   : Variant;
  ValStr : string;
begin
  Result := False;
  if not VarIsArray(List) then
  begin
    Result := (Val = List);
    if Negate then Result := not Result;
    Exit;
  end;

  ValStr := VarToStr(Val);
  for I := VarArrayLowBound(List, 1) to VarArrayHighBound(List, 1) do
  begin
    Item := VarArrayGet(List, [I]);
    if SameText(ValStr, VarToStr(Item)) then
    begin
      Result := True;
      Break;
    end;
  end;

  if Negate then Result := not Result;
end;

// ---------------------------------------------------------------------------
// Evaluate
// ---------------------------------------------------------------------------

function TAiEmbeddingMetaData.Evaluate(const Name: string; Op: TFilterOperator;
    const Value: Variant): Boolean;
begin
  Result := Evaluate(Name, Op, Value, Unassigned);
end;

function TAiEmbeddingMetaData.Evaluate(const Name: string; Op: TFilterOperator;
    const Value, Value2: Variant; const ANodeText: string): Boolean;
var
  PropValue        : Variant;
  Exists           : Boolean;
  DTProp, DTVal1, DTVal2: TDateTime;
  IsDateComparison : Boolean;
  I, J             : Integer;
begin
  // Campo virtual 'text'
  if SameText(Name, 'text') then
  begin
    PropValue := ANodeText;
    Exists    := True;
  end
  else
    Exists := TryGetPropValue(Name, PropValue);

  // Existencia / nulidad
  case Op of
    foExists:    begin Result := Exists; Exit; end;
    foIsNull:    begin Result := not Exists or VarIsNull(PropValue); Exit; end;
    foIsNotNull: begin Result := Exists and not VarIsNull(PropValue); Exit; end;
  end;

  if not Exists or VarIsNull(PropValue) then
  begin
    Result := False;
    Exit;
  end;

  // IN / NOT IN
  if Op in [foIn, foNotIn] then
  begin
    Result := CheckInList(PropValue, Value, Op = foNotIn);
    Exit;
  end;

  // ExistsAny / ExistsAll
  if Op in [foExistsAny, foExistsAll] then
  begin
    if VarIsArray(PropValue) then
    begin
      if VarIsArray(Value) then
      begin
        if Op = foExistsAny then
        begin
          for I := VarArrayLowBound(Value, 1) to VarArrayHighBound(Value, 1) do
            if CheckInList(VarArrayGet(Value, [I]), PropValue, False) then
            begin
              Result := True;
              Exit;
            end;
          Result := False;
        end
        else
        begin
          Result := True;
          for I := VarArrayLowBound(Value, 1) to VarArrayHighBound(Value, 1) do
            if not CheckInList(VarArrayGet(Value, [I]), PropValue, False) then
            begin
              Result := False;
              Exit;
            end;
        end;
      end
      else
        Result := CheckInList(Value, PropValue, False);
    end
    else
      Result := False;
    Exit;
  end;

  // LIKE / ILIKE
  if Op in [foLike, foILike] then
  begin
    Result := CheckLike(VarToStr(PropValue), VarToStr(Value), Op = foILike);
    Exit;
  end;

  // Comparación de fechas
  IsDateComparison := (VarType(PropValue) = varDate) or
      ((VarType(PropValue) = varString) and
       TryISO8601ToDate(VarToStr(PropValue), DTProp));

  if IsDateComparison then
  begin
    if VarType(PropValue) = varDate then
      DTProp := TDateTime(Double(PropValue));

    if Op = foBetween then
    begin
      if TryISO8601ToDate(VarToStr(Value), DTVal1) and
         TryISO8601ToDate(VarToStr(Value2), DTVal2) then
      begin
        Result := (DTProp >= DTVal1) and (DTProp <= DTVal2);
        Exit;
      end
      else
      begin
        Result := False;
        Exit;
      end;
    end
    else if TryISO8601ToDate(VarToStr(Value), DTVal1) then
    begin
      case Op of
        foEqual:          begin Result := DTProp = DTVal1; Exit; end;
        foNotEqual:       begin Result := DTProp <> DTVal1; Exit; end;
        foGreater:        begin Result := DTProp > DTVal1; Exit; end;
        foGreaterOrEqual: begin Result := DTProp >= DTVal1; Exit; end;
        foLess:           begin Result := DTProp < DTVal1; Exit; end;
        foLessOrEqual:    begin Result := DTProp <= DTVal1; Exit; end;
      end;
    end;
  end;

  // Comparación numérica
  if VarIsNumeric(PropValue) and VarIsNumeric(Value) then
  begin
    if Op = foBetween then
    begin
      if VarIsNumeric(Value2) then
        Result := (Double(PropValue) >= Double(Value)) and
                  (Double(PropValue) <= Double(Value2))
      else
        Result := False;
      Exit;
    end;
    Result := CompareNumbers(PropValue, Value, Op);
    Exit;
  end;

  // Fallback: strings
  if Op = foBetween then
  begin
    Result := (VarToStr(PropValue) >= VarToStr(Value)) and
              (VarToStr(PropValue) <= VarToStr(Value2));
    Exit;
  end;

  Result := CompareStrings(VarToStr(PropValue), VarToStr(Value), Op);
end;

// ---------------------------------------------------------------------------
// Matches (recursivo)
// ---------------------------------------------------------------------------

function TAiEmbeddingMetaData.Matches(Criteria: TAiFilterCriteria;
    const ANodeText: string): Boolean;
var
  I            : Integer;
  Criterion    : TFilterCriterion;
  CurrentMatch : Boolean;
begin
  if (Criteria = nil) or (Criteria.Count = 0) then
  begin
    Result := True;
    Exit;
  end;

  if Criteria.LogicalOp = loAnd then
  begin
    Result := True;
    for I := 0 to Criteria.Count - 1 do
    begin
      Criterion := Criteria.Items[I];
      if Criterion.IsGroup then
        CurrentMatch := Matches(Criterion.SubCriteria, ANodeText)
      else
        CurrentMatch := Evaluate(Criterion.Key, Criterion.Op,
            Criterion.Value, Criterion.Value2, ANodeText);
      if not CurrentMatch then
      begin
        Result := False;
        Exit;
      end;
    end;
  end
  else
  begin
    Result := False;
    for I := 0 to Criteria.Count - 1 do
    begin
      Criterion := Criteria.Items[I];
      if Criterion.IsGroup then
        CurrentMatch := Matches(Criterion.SubCriteria, ANodeText)
      else
        CurrentMatch := Evaluate(Criterion.Key, Criterion.Op,
            Criterion.Value, Criterion.Value2, ANodeText);
      if CurrentMatch then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

// ---------------------------------------------------------------------------
// JSON
// ---------------------------------------------------------------------------

function TAiEmbeddingMetaData.ToJSON: TJSONObject;
var
  I   : Integer;
  K   : string;
  V   : Variant;
  JVal: TJSONData;
  VT  : Word;
begin
  Result := TJSONObject.Create;
  try
    if FTagString <> '' then
      Result.Add('tagString', FTagString);

    if FData.Count > 0 then
      Result.Add('_data_text', FData.Text);

    for I := 0 to FProperties.Count - 1 do
    begin
      K  := FProperties[I];
      V  := TAiVariantItem(FProperties.Objects[I]).Value;
      VT := VarType(V) and varTypeMask;
      case VT of
        varSmallint, varInteger, varByte, varShortInt, varWord, varLongWord, varInt64:
          JVal := TJSONInt64Number.Create(Int64(V));
        varSingle, varDouble, varCurrency:
          JVal := TJSONFloatNumber.Create(Double(V));
        varBoolean:
          JVal := TJSONBoolean.Create(Boolean(V));
        varDate:
          JVal := TJSONString.Create(FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"', TDateTime(Double(V))));
        varNull, varEmpty:
          JVal := TJSONNull.Create;
      else
        JVal := TJSONString.Create(VarToStr(V));
      end;
      Result.Add(K, JVal);
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure TAiEmbeddingMetaData.FromJSON(aJSON: TJSONObject);
var
  I    : Integer;
  K    : string;
  JVal : TJSONData;
  S    : string;
  DT   : TDateTime;
begin
  if not Assigned(aJSON) then Exit;
  Clear;
  for I := 0 to aJSON.Count - 1 do
  begin
    K    := aJSON.Names[I];
    JVal := aJSON.Items[I];

    if SameText(K, 'tagString') then
      FTagString := JVal.AsString
    else if SameText(K, '_data_text') then
      FData.Text := JVal.AsString
    else
    begin
      if JVal is TJSONIntegerNumber then
        AddOrSetPropValue(K, Int64(JVal.AsInt64))
      else if JVal is TJSONInt64Number then
        AddOrSetPropValue(K, Int64(JVal.AsInt64))
      else if JVal is TJSONFloatNumber then
      begin
        S := JVal.AsJSON;
        if Pos('.', S) > 0 then
          AddOrSetPropValue(K, JVal.AsFloat)
        else
          AddOrSetPropValue(K, Int64(JVal.AsInt64));
      end
      else if JVal is TJSONBoolean then
        AddOrSetPropValue(K, JVal.AsBoolean)
      else if JVal is TJSONNull then
        AddOrSetPropValue(K, Null)
      else
      begin
        // String: intentar parsear como fecha ISO8601
        S := JVal.AsString;
        if TryISO8601ToDate(S, DT) then
          AddOrSetPropValue(K, DT)
        else
          AddOrSetPropValue(K, S);
      end;
    end;
  end;
end;

end.
