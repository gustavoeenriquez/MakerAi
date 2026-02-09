// =============================================================================
// MakerAI Cross-Platform Compatibility Layer - Generics Helper
// =============================================================================
//
// Purpose: Provee THashSet<T>, TStringComparer y estructuras de
//          System.Generics faltantes en versiones antiguas de Delphi y FPC.
//
// Compatibility:
//   - Delphi 10.2 (Tokyo) - Delphi 13 (Athens)
//   - Free Pascal 3.2+ (Implementación manual de TStringComparer)
//
// Usage:
//   uses uGenericsHelper;
//   var
//     Dict: TDictionary<string, Variant>;
//   begin
//     Dict := TDictionary<string, Variant>.Create(TStringComparer.Ordinal);
//   end;
//
// =============================================================================

unit uGenericsHelper;

{$INCLUDE ../CompilerDirectives.inc}

interface

uses
  {$IFDEF FPC}
  SysUtils, Generics.Collections, Generics.Defaults;
  {$ELSE}
  System.SysUtils, System.Generics.Collections, System.Generics.Defaults;
  {$ENDIF}

type
  // ============================================================================
  // THashSet<T> - Implementación unificada para Delphi < 12 y FPC
  // ============================================================================
  // Delphi 12+: THashSet nativo en System.Generics.Collections
  // Delphi < 12: Wrapper basado en TDictionary
  // FPC: Ya existe en Generics.Collections (no se redefine)
  
  {$IFNDEF FPC}
    {$IF CompilerVersion < 36} // Delphi < 12
  THashSet<T> = class
  private
    FDict: TDictionary<T, Boolean>;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const AValue: T);
    procedure Remove(const AValue: T);
    function Contains(const AValue: T): Boolean;
    procedure Clear;
    property Count: Integer read GetCount;
  end;
    {$ELSE}
  // Delphi 12+: Usar nativo
  THashSet<T> = class(System.Generics.Collections.THashSet<T>);
    {$IFEND}

  // ============================================================================
  // TGenericsUtils - Utilidad para clonado de diccionarios (Delphi)
  // ============================================================================
  TGenericsUtils = class
  public
    class function CloneDictionary<K,V>(const Source: TDictionary<K,V>): TDictionary<K,V>; static;
  end;
  {$ENDIF}

  // ============================================================================
  // TStringComparer - Comparador de strings para FPC
  // ============================================================================
  // En FPC TStringComparer no tiene constructor estÃ¡tico Ordinal como Delphi.
  // Implementamos un comparador compatible con IEqualityComparer.
  
{$IFDEF FPC}
  TStringComparer = class(TInterfacedObject, IEqualityComparer<string>)
  private
    FCaseSensitive: Boolean;
  public
    constructor Create(ACaseSensitive: Boolean);
   
    function Equals(const Left, Right: string): Boolean;
    function GetHashCode(const Value: string): LongWord;
    
    { MÃ©todos estÃ¡ticos compatibles con Delphi }
    class function Ordinal: IEqualityComparer<string>; static;
    class function OrdinalIgnoreCase: IEqualityComparer<string>; static;
  end;

var
  { Instancias globales para evitar crear mÃºltiples comparadores }
  GOrdinalComparer: IEqualityComparer<string> = nil;
  GOrdinalIgnoreCaseComparer: IEqualityComparer<string> = nil;

type
  { Clase de utilidad para Generics }
  TGenericsUtils = class
  public
    class function CloneDictionary<K,V>(const Source: TDictionary<K,V>): TDictionary<K,V>; static;
  end;
{$ENDIF}

implementation

// ============================================================================
// THashSet<T> Implementation (Delphi < 12 only)
// ============================================================================
{$IFNDEF FPC}
{$IF CompilerVersion < 36}

constructor THashSet<T>.Create;
begin
  inherited Create;
  FDict := TDictionary<T, Boolean>.Create;
end;

destructor THashSet<T>.Destroy;
begin
  FDict.Free;
  inherited;
end;

procedure THashSet<T>.Add(const AValue: T);
begin
  if not FDict.ContainsKey(AValue) then
    FDict.Add(AValue, True);
end;

procedure THashSet<T>.Remove(const AValue: T);
begin
  FDict.Remove(AValue);
end;

function THashSet<T>.Contains(const AValue: T): Boolean;
begin
  Result := FDict.ContainsKey(AValue);
end;

procedure THashSet<T>.Clear;
begin
  FDict.Clear;
end;

function THashSet<T>.GetCount: Integer;
begin
  Result := FDict.Count;
end;

{$IFEND}
{$ENDIF}

// ============================================================================
// TGenericsUtils Implementation (Delphi only)
// ============================================================================
{$IFNDEF FPC}
class function TGenericsUtils.CloneDictionary<K,V>(const Source: TDictionary<K,V>): TDictionary<K,V>;
var
  Pair: TPair<K,V>;
begin
  Result := TDictionary<K,V>.Create;
  if Source <> nil then
  begin
    for Pair in Source do
      Result.AddOrSetValue(Pair.Key, Pair.Value);
  end;
end;
{$ENDIF}

// ============================================================================
// TStringComparer & TGenericsUtils Implementation (FPC only)
// ============================================================================
{$IFDEF FPC}

constructor TStringComparer.Create(ACaseSensitive: Boolean);
begin
  inherited Create;
  FCaseSensitive := ACaseSensitive;
end;

function TStringComparer.Equals(const Left, Right: string): Boolean;
begin
  if FCaseSensitive then
    Result := Left = Right
  else
    Result := SameText(Left, Right);
end;

function TStringComparer.GetHashCode(const Value: string): LongWord;
var
  I: Integer;
  S: string;
begin
  if FCaseSensitive then
    S := Value
  else
    S := LowerCase(Value);
    
  { Hash simple pero efectivo }
  Result := 0;
  for I := 1 to Length(S) do
    Result := ((Result shl 5) - Result) + Ord(S[I]);
end;

class function TStringComparer.Ordinal: IEqualityComparer<string>;
begin
  if GOrdinalComparer = nil then
    GOrdinalComparer := TStringComparer.Create(True);
  Result := GOrdinalComparer;
end;

class function TStringComparer.OrdinalIgnoreCase: IEqualityComparer<string>;
begin
  if GOrdinalIgnoreCaseComparer = nil then
    GOrdinalIgnoreCaseComparer := TStringComparer.Create(False);
  Result := GOrdinalIgnoreCaseComparer;
end;

class function TGenericsUtils.CloneDictionary<K,V>(const Source: TDictionary<K,V>): TDictionary<K,V>;
var
  Pair: TPair<K,V>;
begin
  Result := TDictionary<K,V>.Create;
  if Source <> nil then
  begin
    for Pair in Source do
      Result.AddOrSetValue(Pair.Key, Pair.Value);
  end;
end;

initialization

finalization
  GOrdinalComparer := nil;
  GOrdinalIgnoreCaseComparer := nil;

{$ENDIF}

end.

