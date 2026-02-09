// =============================================================================
// MakerAI Cross-Platform Compatibility Layer - SysUtils Helper
// =============================================================================
// 
// Purpose: Unifica funciones comunes de SysUtils/StrUtils con namespaces inconsistentes
//
// Compatibility:
//   - Delphi XE7+ (Primary development environment)
//   - Free Pascal 3.3+ (Full support)
//   - Free Pascal 3.2 (Full support)
//
// Usage:
//   uses uSysUtilsHelper;
//   var
//     Result: string;
//   begin
//     Result := CompatIfThen(Condition, 'True', 'False');
//   end;
//
// Developer Notes (Delphi):
//   - Esta unidad simplemente re-exporta funciones estándar de Delphi
//   - No afecta el rendimiento ni agrega overhead en Delphi
//   - Solo resuelve diferencias de namespace entre Delphi y FPC
//
// =============================================================================

unit uSysUtilsHelper;

{$include ../CompilerDirectives.inc}

interface

uses
  {$IFDEF FPC}
  SysUtils, StrUtils, Classes, Types;
  {$ELSE}
  System.SysUtils, System.StrUtils, System.Classes, System.IOUtils,
  System.Types, System.Generics.Collections;
  {$ENDIF}

type
  // Tipos comunes para ambos compiladores
  {$IFNDEF FPC}
  TStringArray = TArray<string>;
  TStringSplitOptions = (None, ExcludeEmpty);
  {$ELSE}
  UINT = Cardinal;  // Alias para compatibilidad con Winapi.Windows
  TStringArray = array of string;
  TStringSplitOptions = (None, ExcludeEmpty);
  {$ENDIF}

// Función de utilidad para Split con opciones - compatibilidad Delphi/FPC
function CompatSplit(const AStr: string; const ASeparators: array of Char; AOptions: TStringSplitOptions): TStringArray;

{$IFDEF FPC}
type
  { TStringHelper }

  TStringHelper = record helper for string
  public
    function Split(const ASeparator: Char): TStringArray; overload;
    function Split(const ASeparators: array of Char): TStringArray; overload;
    function Split(const ASeparators: array of Char; AOptions: TStringSplitOptions): TStringArray; overload; // Nueva sobrecarga
    function StartsWith(const AValue: string; AIgnoreCase: Boolean = False): Boolean;
    function EndsWith(const AValue: string; AIgnoreCase: Boolean = False): Boolean;
    function Replace(const OldPattern, NewPattern: string): string;
    function ToLower: string;
    function ToUpper: string;
    function IsEmpty: Boolean;
    function Contains(const ASubStr: string): Boolean;
    function Trim: string;
    function TrimRight(const TrimChars: array of Char): string;
    function Equals(const AValue: string): Boolean;
    function Substring(StartIndex: Integer): string; overload;
    function Substring(StartIndex: Integer; Length: Integer): string; overload;
    class function Join(const Separator: string; const Values: array of string): string; overload; static;
    class function Join(const Separator: string; const Values: TStringArray): string; overload; static;
  end;

  TStringsHelper = class helper for TStrings
  private
    function GetKeyNames(Index: Integer): string;
  public
    property KeyNames[Index: Integer]: string read GetKeyNames;
    function Contains(const AValue: string): Boolean;
  end;
  
  { TCharHelper }
  TCharHelper = record helper for Char
  public
    function IsWhiteSpace: Boolean;
    function IsDigit: Boolean;
    function IsLetter: Boolean;
    function IsLetterOrDigit: Boolean;
    function IsUpper: Boolean;
    function IsLower: Boolean;
  end;
  
  { TIntegerHelper - Permite usar Integer.ToString como en Delphi }
  TIntegerHelper = record helper for Integer
  public
    function ToString: string;
  end;
  
  { TInt64Helper - Permite usar Int64.ToString como en Delphi }
  TInt64Helper = record helper for Int64
  public
    function ToString: string;
  end;
  
{ Función helper para conversión ISO8601 }
function TryISO8601ToDate(const AString: string; out ADateTime: TDateTime): Boolean;
{$ENDIF}

{$IFDEF FPC}
type
  TFile = record
  public
    class procedure WriteAllText(const Path: string; const Contents: string); overload; static;
    class procedure WriteAllText(const Path: string; const Contents: string; Encoding: TEncoding); overload; static;
    class procedure WriteAllBytes(const Path: string; const Bytes: TBytes); static;
    class function ReadAllText(const Path: string): string; overload; static;
    class function ReadAllText(const Path: string; Encoding: TEncoding): string; overload; static;
    class function Exists(const Path: string): Boolean; static;
    class procedure Delete(const Path: string); static;
    class procedure Copy(const Source, Dest: string; Overwrite: Boolean); static;
  end;

  TDirectory = record
  public
    class function Exists(const Path: string): Boolean; static;
    class procedure CreateDirectory(const Path: string); static;
  end;

  { TStreamReader - Compatibilidad con System.Classes.TStreamReader de Delphi }
  { Permite leer streams como texto con encoding especificado }
  TStreamReader = class
  private
    FStream: TStream;
    FEncoding: TEncoding;
    FOwnsStream: Boolean;
  public
    constructor Create(AStream: TStream); overload;
    constructor Create(AStream: TStream; AEncoding: TEncoding); overload;
    constructor Create(const AFileName: string); overload;
    constructor Create(const AFileName: string; AEncoding: TEncoding); overload;
    destructor Destroy; override;
    function ReadToEnd: string;
    function ReadLine: string;
    function EndOfStream: Boolean;
  end;

  { TStreamWriter - Compatibilidad con System.Classes.TStreamWriter de Delphi }
  TStreamWriter = class
  private
    FStream: TStream;
    FEncoding: TEncoding;
    FOwnsStream: Boolean;
  public
    constructor Create(AStream: TStream); overload;
    constructor Create(AStream: TStream; AEncoding: TEncoding); overload;
    constructor Create(const AFileName: string); overload;
    constructor Create(const AFileName: string; AEncoding: TEncoding); overload;
    destructor Destroy; override;
    procedure Write(const Value: string); overload;
    procedure WriteLine(const Value: string); overload;
  end;
{$ENDIF}

// IfThen unificado (namespace difiere entre FPC y Delphi)
function CompatIfThen(AValue: Boolean; const ATrue: string; const AFalse: string): string; overload; inline;
function CompatIfThen(AValue: Boolean; const ATrue: Integer; const AFalse: Integer): Integer; overload; inline;
function CompatIfThen(AValue: Boolean; const ATrue: Int64; const AFalse: Int64): Int64; overload; inline;
function CompatIfThen(AValue: Boolean; const ATrue: Double; const AFalse: Double): Double; overload; inline;

// Ruta temporal unificada
function GetCompatTempPath: string;

// Función TrimSet para recortar caracteres de un string (compatibilidad)
function TrimSet(const S: string; const CharSet: TSysCharSet): string;

// Variable de entorno unificada (evita SysUtils vs System.SysUtils)
function CompatGetEnvVar(const AName: string): string;

{$IFDEF FPC}
type
  TPath = record
  public
    class function GetHomePath: string; static;
    class function GetTempPath: string; static;
    class function Combine(const Path1, Path2: string): string; static;
    class function GetRandomFileName: string; static;
    class function GetDirectoryName(const Path: string): string; static;
  end;
  
  { TStopwatch - Compatibilidad con System.Diagnostics.TStopwatch de Delphi }
  TStopwatch = record
  private
    FStartTime: QWord;
    FStopTime: QWord;
    FRunning: Boolean;
  public
    class function StartNew: TStopwatch; static;
    procedure Start;
    procedure Stop;
    procedure Reset;
    function ElapsedMilliseconds: Int64;
    function IsRunning: Boolean;
  end;
{$ELSE}
// Delphi: TStringsHelper para proporcionar Contains en TStrings (clase base)
// TStringList.Contains existe en Delphi moderno, pero TStrings no lo tiene
type
  TStringsHelper = class helper for TStrings
  public
    function Contains(const AValue: string): Boolean;
  end;
{$ENDIF}

// =============================================================================
// TArrayUtils - Helper multiplataforma para ordenamiento de arrays
// Delphi: Usa TArray.Sort<T> de System.Generics.Collections
// FPC: Usa TList<T>.Sort via implementación manual
// =============================================================================
  TArrayUtils = record
    class procedure Sort(var Values: TIntegerDynArray);
      {$IFDEF FPC}overload;{$ELSE}{$IF CompilerVersion < 33.0}overload;{$ENDIF}{$ENDIF} static;
    {$IFDEF FPC}
    // FPC: TArray<Integer> y TIntegerDynArray son tipos distintos
    class procedure Sort(var Values: TArray<Integer>); overload; static;
    {$ELSE}
    {$IF CompilerVersion < 33.0}
    // D10.2: TArray<Integer> y TIntegerDynArray son nominalmente distintos
    class procedure Sort(var Values: TArray<Integer>); overload; static;
    {$ENDIF}
    {$ENDIF}
  end;

// ============================================================================
// Funciones helper para Stream + String (evitan crear/destruir TStreamReader/Writer)
// ============================================================================
// Escribe un string a un stream con encoding UTF-8
procedure WriteStringToStream(AStream: TStream; const AValue: string);
// Lee todo el contenido de un stream como string UTF-8
function ReadStringFromStream(AStream: TStream): string;

implementation

{$IFDEF FPC}
uses
  Generics.Collections;
{$ENDIF}

// ============================================================================
// Implementación de funciones helper Stream + String
// ============================================================================

procedure WriteStringToStream(AStream: TStream; const AValue: string);
var
  LWriter: TStreamWriter;
begin
  LWriter := TStreamWriter.Create(AStream, TEncoding.UTF8);
  try
    LWriter.Write(AValue);
  finally
    LWriter.Free;
  end;
end;

function ReadStringFromStream(AStream: TStream): string;
var
  LReader: TStreamReader;
begin
  AStream.Position := 0;
  LReader := TStreamReader.Create(AStream, TEncoding.UTF8);
  try
    Result := LReader.ReadToEnd;
  finally
    LReader.Free;
  end;
end;

function CompatIfThen(AValue: Boolean; const ATrue: string; const AFalse: string): string;
begin
  {$IFDEF FPC}
  Result := StrUtils.IfThen(AValue, ATrue, AFalse);
  {$ELSE}
  Result := System.StrUtils.IfThen(AValue, ATrue, AFalse);
  {$ENDIF}
end;

function CompatSplit(const AStr: string; const ASeparators: array of Char; AOptions: TStringSplitOptions): TStringArray;
var
  TempArr: TStringArray;
  i, Count: Integer;
begin
  // Usar Split nativo
  {$IFDEF FPC}
  TempArr := AStr.Split(ASeparators, AOptions);
  {$ELSE}
  TempArr := AStr.Split(ASeparators);
  {$ENDIF}
  
  // Si no es ExcludeEmpty, retornar directamente
  {$IFNDEF FPC}
  if AOptions = TStringSplitOptions.None then
  begin
    Result := TempArr;
    Exit;
  end;
  
  // Para Delphi: filtrar vacíos manualmente si es ExcludeEmpty
  Count := 0;
  for i := 0 to High(TempArr) do
    if TempArr[i] <> '' then
      Inc(Count);
  
  SetLength(Result, Count);
  Count := 0;
  for i := 0 to High(TempArr) do
    if TempArr[i] <> '' then
    begin
      Result[Count] := TempArr[i];
      Inc(Count);
    end;
  {$ELSE}
  Result := TempArr;
  {$ENDIF}
end;

function CompatIfThen(AValue: Boolean; const ATrue: Integer; const AFalse: Integer): Integer;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

function CompatIfThen(AValue: Boolean; const ATrue: Int64; const AFalse: Int64): Int64;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

function CompatIfThen(AValue: Boolean; const ATrue: Double; const AFalse: Double): Double;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

function GetCompatTempPath: string;
begin
  {$IFDEF FPC}
  Result := GetTempDir;
  {$ELSE}
  Result := TPath.GetTempPath;
  {$ENDIF}
  Result := IncludeTrailingPathDelimiter(Result);
end;

function CompatGetEnvVar(const AName: string): string;
begin
  {$IFDEF FPC}
  Result := SysUtils.GetEnvironmentVariable(AName);
  {$ELSE}
  Result := System.SysUtils.GetEnvironmentVariable(AName);
  {$ENDIF}
end;

function TrimSet(const S: string; const CharSet: TSysCharSet): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  // Trim izquierda
  while (I <= L) and (S[I] in CharSet) do
    Inc(I);
  if I > L then
  begin
    Result := '';
    Exit;
  end;
  // Trim derecha
  while (L >= I) and (S[L] in CharSet) do
    Dec(L);
  Result := Copy(S, I, L - I + 1);
end;

{$IFDEF FPC}
{ TPath }

class function TPath.GetHomePath: string;
begin
  Result := GetUserDir;
end;


class function TPath.GetTempPath: string;
begin
  Result := GetTempDir;
  Result := IncludeTrailingPathDelimiter(Result);
end;

class function TPath.Combine(const Path1, Path2: string): string;
begin
  if Path2 = '' then
    Result := Path1
  else if Path1 = '' then
    Result := Path2
  else
  begin
    if Path2[1] = DirectorySeparator then
      Result := IncludeTrailingPathDelimiter(Path1) + Copy(Path2, 2, MaxInt)
    else
      Result := IncludeTrailingPathDelimiter(Path1) + Path2;
  end;
end;

{ TStringHelper }

function TStringHelper.Split(const ASeparator: Char): TStringArray;
var
  LList: TStringList;
  I: Integer;
begin
  if Self = '' then
  begin
    Result := nil;
    Exit;
  end;

  LList := TStringList.Create;
  try
    LList.Delimiter := ASeparator;
    LList.StrictDelimiter := True;
    LList.DelimitedText := Self;
    SetLength(Result, LList.Count);
    for I := 0 to LList.Count - 1 do
      Result[I] := LList[I];
  finally
    LList.Free;
  end;
end;

function TStringHelper.Split(const ASeparators: array of Char): TStringArray;
var
  LList: TStringList;
  I: Integer;
  LSeparator: Char;
  LTemp: string;
begin
  if Self = '' then
  begin
    Result := nil;
    Exit;
  end;

  // Si no hay separadores, devolvemos un array con el string original
  if Length(ASeparators) = 0 then
  begin
    SetLength(Result, 1);
    Result[0] := Self;
    Exit;
  end;

  // Usamos el primer separador para TStringList
  LSeparator := ASeparators[0];
  
  LList := TStringList.Create;
  try
    LList.Delimiter := LSeparator;
    LList.StrictDelimiter := True;
    
    // Si hay más de un separador, normalizamos el string primero
    if Length(ASeparators) > 1 then
    begin
      LTemp := Self;
      for I := 1 to High(ASeparators) do
        LTemp := StringReplace(LTemp, ASeparators[I], LSeparator, [rfReplaceAll]);
      LList.DelimitedText := LTemp;
    end
    else
      LList.DelimitedText := Self;

    SetLength(Result, LList.Count);
    for I := 0 to LList.Count - 1 do
      Result[I] := LList[I];
  finally
    LList.Free;
  end;
end;

function TStringHelper.Split(const ASeparators: array of Char; AOptions: TStringSplitOptions): TStringArray;
var
  LList: TStringList;
  I: Integer;
  LSeparator: Char;
  LTemp: string;
begin
  if Self = '' then
  begin
    Result := nil;
    Exit;
  end;

  // Si no hay separadores, devolvemos un array con el string original
  if Length(ASeparators) = 0 then
  begin
    SetLength(Result, 1);
    Result[0] := Self;
    Exit;
  end;

  // Usamos el primer separador para TStringList
  LSeparator := ASeparators[0];
  
  LList := TStringList.Create;
  try
    LList.Delimiter := LSeparator;
    LList.StrictDelimiter := True;
    
    // Si hay más de un separador, normalizamos el string primero
    if Length(ASeparators) > 1 then
    begin
      LTemp := Self;
      for I := 1 to High(ASeparators) do
        LTemp := StringReplace(LTemp, ASeparators[I], LSeparator, [rfReplaceAll]);
      LList.DelimitedText := LTemp;
    end
    else
      LList.DelimitedText := Self;

    if AOptions = ExcludeEmpty then
    begin
      // Filtrar elementos vacíos
      for I := LList.Count - 1 downto 0 do
        if SysUtils.Trim(LList[I]) = '' then
          LList.Delete(I);
    end;

    SetLength(Result, LList.Count);
    for I := 0 to LList.Count - 1 do
      Result[I] := LList[I];
  finally
    LList.Free;
  end;
end;

function TStringHelper.StartsWith(const AValue: string; AIgnoreCase: Boolean): Boolean;
begin
  if AIgnoreCase then
    Result := SameText(Copy(Self, 1, Length(AValue)), AValue)
  else
    Result := Copy(Self, 1, Length(AValue)) = AValue;
end;

function TStringHelper.EndsWith(const AValue: string; AIgnoreCase: Boolean): Boolean;
begin
  if AIgnoreCase then
    Result := AnsiEndsText(AValue, Self)
  else
    Result := AnsiEndsStr(AValue, Self);
end;



function TStringHelper.Replace(const OldPattern, NewPattern: string): string;
begin
  Result := StringReplace(Self, OldPattern, NewPattern, [rfReplaceAll]);
end;

function TStringHelper.ToLower: string;
begin
  Result := LowerCase(Self);
end;

function TStringHelper.ToUpper: string;
begin
  Result := UpCase(Self);
end;

function TStringHelper.IsEmpty: Boolean;
begin
  Result := (Self = '');
end;

function TStringHelper.Contains(const ASubStr: string): Boolean;
begin
  Result := Pos(ASubStr, Self) > 0;
end;

{ TryISO8601ToDate - Conversión básica de formato ISO8601 }
function TryISO8601ToDate(const AString: string; out ADateTime: TDateTime): Boolean;
var
  Y, M, D, H, Min, S: Integer;
  TempStr: string;
begin
  Result := False;
  TempStr := AString;
  
  { Formato básico: YYYY-MM-DDTHH:MM:SS o YYYY-MM-DD }
  try
    if Length(TempStr) >= 10 then
    begin
      Y := StrToInt(Copy(TempStr, 1, 4));
      M := StrToInt(Copy(TempStr, 6, 2));
      D := StrToInt(Copy(TempStr, 9, 2));
      
      if Length(TempStr) >= 19 then
      begin
        { Con hora }
        H := StrToInt(Copy(TempStr, 12, 2));
        Min := StrToInt(Copy(TempStr, 15, 2));
        S := StrToInt(Copy(TempStr, 18, 2));
        ADateTime := EncodeDate(Y, M, D) + EncodeTime(H, Min, S, 0);
      end
      else
      begin
        { Solo fecha }
        ADateTime := EncodeDate(Y, M, D);
      end;
      Result := True;
    end;
  except
    Result := False;
  end;
end;



function TStringHelper.Trim: string;
begin
  Result := SysUtils.Trim(Self);
end;

function TStringHelper.TrimRight(const TrimChars: array of Char): string;
var
  I, J: Integer;
  Found: Boolean;
begin
  Result := Self;
  while Length(Result) > 0 do
  begin
    Found := False;
    for I := 0 to High(TrimChars) do
    begin
      if Result[Length(Result)] = TrimChars[I] then
      begin
        Found := True;
        Break;
      end;
    end;
    if Found then
      SetLength(Result, Length(Result) - 1)
    else
      Break;
  end;
end;

function TStringHelper.Equals(const AValue: string): Boolean;
begin
  Result := Self = AValue;
end;

{ Substring: Delphi uses 0-based indexing for Substring }
function TStringHelper.Substring(StartIndex: Integer): string;
begin
  // Delphi Substring is 0-indexed, FPC Copy is 1-indexed
  Result := Copy(Self, StartIndex + 1, MaxInt);
end;

function TStringHelper.Substring(StartIndex: Integer; Length: Integer): string;
begin
  // Delphi Substring is 0-indexed, FPC Copy is 1-indexed
  Result := Copy(Self, StartIndex + 1, Length);
end;

class function TStringHelper.Join(const Separator: string; const Values: array of string): string;
var
  I: Integer;
begin
  Result := '';
  if Length(Values) = 0 then Exit;
  Result := Values[0];
  for I := 1 to High(Values) do
    Result := Result + Separator + Values[I];
end;

class function TStringHelper.Join(const Separator: string; const Values: TStringArray): string;
var
  I: Integer;
begin
  Result := '';
  if Length(Values) = 0 then Exit;
  Result := Values[0];
  for I := 1 to High(Values) do
    Result := Result + Separator + Values[I];
end;

{ TStringsHelper }

function TStringsHelper.GetKeyNames(Index: Integer): string;
begin
  Result := Self.Names[Index];
end;

function TStringsHelper.Contains(const AValue: string): Boolean;
begin
  Result := Self.IndexOf(AValue) <> -1;
end;

{ TCharHelper }

function TCharHelper.IsWhiteSpace: Boolean;
begin
  Result := Self in [' ', #9, #10, #13];
end;

function TCharHelper.IsDigit: Boolean;
begin
  Result := Self in ['0'..'9'];
end;

function TCharHelper.IsLetter: Boolean;
begin
  Result := Self in ['A'..'Z', 'a'..'z'];
end;

function TCharHelper.IsLetterOrDigit: Boolean;
begin
  Result := IsLetter or IsDigit;
end;

function TCharHelper.IsUpper: Boolean;
begin
  Result := Self in ['A'..'Z'];
end;

function TCharHelper.IsLower: Boolean;
begin
  Result := Self in ['a'..'z'];
end;

{ TIntegerHelper }

function TIntegerHelper.ToString: string;
begin
  Result := IntToStr(Self);
end;

{ TInt64Helper }

function TInt64Helper.ToString: string;
begin
  Result := IntToStr(Self);
end;

{$ENDIF}

{$IFDEF FPC}
{ TFile Shim }

class procedure TFile.WriteAllText(const Path: string; const Contents: string);
begin
  WriteAllText(Path, Contents, TEncoding.UTF8); // Default to UTF8
end;

class procedure TFile.WriteAllText(const Path: string; const Contents: string; Encoding: TEncoding);
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create(Contents, Encoding, False);
  try
    Stream.SaveToFile(Path);
  finally
    Stream.Free;
  end;
end;

class function TFile.ReadAllText(const Path: string): string;
begin
  Result := ReadAllText(Path, TEncoding.UTF8);
end;

class function TFile.ReadAllText(const Path: string; Encoding: TEncoding): string;
var
  Stream: TStringStream;
  FS: TFileStream;
begin
  if not FileExists(Path) then
    raise Exception.CreateFmt('File not found: %s', [Path]);
    
  // Use TFileStream to read
  FS := TFileStream.Create(Path, fmOpenRead or fmShareDenyWrite);
  try
    Stream := TStringStream.Create('', Encoding, False);
    try
      Stream.CopyFrom(FS, 0);
      Result := Stream.DataString;
    finally
      Stream.Free;
    end;
  finally
    FS.Free;
  end;
end;

class function TFile.Exists(const Path: string): Boolean;
begin
  Result := FileExists(Path);
end;

class procedure TFile.Delete(const Path: string);
begin
  DeleteFile(Path);
end;

class procedure TFile.Copy(const Source, Dest: string; Overwrite: Boolean);
var
  FSrc, FDest: TFileStream;
begin
  if not FileExists(Source) then
    raise Exception.CreateFmt('File not found: %s', [Source]);

  if FileExists(Dest) and not Overwrite then
    Exit; // Or raise exception? Standard CopyFile fails if exists and failIfExists=True. But TFile.Copy usually overwrites if Overwrite=True.

  FSrc := TFileStream.Create(Source, fmOpenRead or fmShareDenyWrite);
  try
    if FileExists(Dest) then
      FDest := TFileStream.Create(Dest, fmOpenWrite or fmShareDenyWrite)
    else
      FDest := TFileStream.Create(Dest, fmCreate);
      
    try
      FDest.CopyFrom(FSrc, 0); 
    finally
      FDest.Free;
    end;
  finally
    FSrc.Free;
  end;
end;

class procedure TFile.WriteAllBytes(const Path: string; const Bytes: TBytes);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(Path, fmCreate);
  try
    if Length(Bytes) > 0 then
      FS.WriteBuffer(Bytes[0], Length(Bytes));
  finally
    FS.Free;
  end;
end;

{ TDirectory }

class function TDirectory.Exists(const Path: string): Boolean;
begin
  Result := DirectoryExists(Path);
end;

class procedure TDirectory.CreateDirectory(const Path: string);
begin
  ForceDirectories(Path);
end;

{ TPath - nuevos métodos }

class function TPath.GetRandomFileName: string;
begin
  Result := IntToStr(Random(MaxInt)) + '_' + IntToStr(GetTickCount64);
end;

class function TPath.GetDirectoryName(const Path: string): string;
begin
  Result := ExtractFilePath(Path);
end;
{$ENDIF}

// Implementación Delphi de TStringsHelper
{$IFNDEF FPC}
function TStringsHelper.Contains(const AValue: string): Boolean;
begin
  Result := Self.IndexOf(AValue) <> -1;
end;
{$ENDIF}

{$IFDEF FPC}
class procedure TArrayUtils.Sort(var Values: TIntegerDynArray);
var
  LList: TList<Integer>;
begin
  if Length(Values) <= 1 then Exit;
  LList := TList<Integer>.Create;
  try
    LList.AddRange(Values);
    LList.Sort;
    Values := LList.ToArray;
  finally
    LList.Free;
  end;
end;

{$IFDEF FPC}
class procedure TArrayUtils.Sort(var Values: TArray<Integer>);
var
  LList: TList<Integer>;
begin
  if Length(Values) <= 1 then Exit;
  LList := TList<Integer>.Create;
  try
    LList.AddRange(Values);
    LList.Sort;
    Values := LList.ToArray;
  finally
    LList.Free;
  end;
end;
{$ENDIF}
{$ELSE}
class procedure TArrayUtils.Sort(var Values: TIntegerDynArray);
begin
  if Length(Values) <= 1 then Exit;
  TArray.Sort<Integer>(Values);
end;

{$IF CompilerVersion < 33.0}
class procedure TArrayUtils.Sort(var Values: TArray<Integer>);
begin
  if Length(Values) <= 1 then Exit;
  TArray.Sort<Integer>(Values);
end;
{$ENDIF}
{$ENDIF}

{$IFDEF FPC}
{ TStopwatch }

class function TStopwatch.StartNew: TStopwatch;
begin
  Result.Reset;
  Result.Start;
end;

procedure TStopwatch.Start;
begin
  if not FRunning then
  begin
    FStartTime := GetTickCount64;
    FRunning := True;
  end;
end;

procedure TStopwatch.Stop;
begin
  if FRunning then
  begin
    FStopTime := GetTickCount64;
    FRunning := False;
  end;
end;

procedure TStopwatch.Reset;
begin
  FStartTime := 0;
  FStopTime := 0;
  FRunning := False;
end;

function TStopwatch.ElapsedMilliseconds: Int64;
begin
  if FRunning then
    Result := GetTickCount64 - FStartTime
  else
    Result := FStopTime - FStartTime;
end;

function TStopwatch.IsRunning: Boolean;
begin
  Result := FRunning;
end;
{$ENDIF}

{$IFDEF FPC}
{ TStreamReader - Implementación FPC compatible con Delphi }

constructor TStreamReader.Create(AStream: TStream);
begin
  Create(AStream, TEncoding.UTF8);
end;

constructor TStreamReader.Create(AStream: TStream; AEncoding: TEncoding);
begin
  inherited Create;
  FStream := AStream;
  FEncoding := AEncoding;
  FOwnsStream := False;
end;

constructor TStreamReader.Create(const AFileName: string);
begin
  Create(AFileName, TEncoding.UTF8);
end;

constructor TStreamReader.Create(const AFileName: string; AEncoding: TEncoding);
begin
  inherited Create;
  FStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  FEncoding := AEncoding;
  FOwnsStream := True;
end;

destructor TStreamReader.Destroy;
begin
  if FOwnsStream and Assigned(FStream) then
    FStream.Free;
  inherited Destroy;
end;

function TStreamReader.ReadToEnd: string;
var
  Bytes: TBytes;
  Size: Int64;
begin
  Result := '';
  if not Assigned(FStream) then
    Exit;
    
  Size := FStream.Size - FStream.Position;
  if Size <= 0 then 
    Exit;
    
  SetLength(Bytes, Size);
  FStream.Read(Bytes[0], Size);
  
  if Assigned(FEncoding) then
    Result := FEncoding.GetString(Bytes)
  else
    Result := TEncoding.UTF8.GetString(Bytes);
end;

function TStreamReader.ReadLine: string;
var
  B: Byte;
  Builder: TStringBuilder;
begin
  Result := '';
  if not Assigned(FStream) or (FStream.Position >= FStream.Size) then
    Exit;
    
  Builder := TStringBuilder.Create;
  try
    while FStream.Position < FStream.Size do
    begin
      FStream.Read(B, 1);
      if B = 10 then // LF
        Break
      else if B = 13 then // CR
      begin
        // Check for CRLF
        if (FStream.Position < FStream.Size) then
        begin
          FStream.Read(B, 1);
          if B <> 10 then
            FStream.Position := FStream.Position - 1;
        end;
        Break;
      end
      else
        Builder.Append(Char(B));
    end;
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

function TStreamReader.EndOfStream: Boolean;
begin
  Result := not Assigned(FStream) or (FStream.Position >= FStream.Size);
end;

{ TStreamWriter }

constructor TStreamWriter.Create(AStream: TStream);
begin
  Create(AStream, TEncoding.UTF8);
end;

constructor TStreamWriter.Create(AStream: TStream; AEncoding: TEncoding);
begin
  inherited Create;
  FStream := AStream;
  FEncoding := AEncoding;
  FOwnsStream := False;
end;

constructor TStreamWriter.Create(const AFileName: string);
begin
  Create(AFileName, TEncoding.UTF8);
end;

constructor TStreamWriter.Create(const AFileName: string; AEncoding: TEncoding);
begin
  inherited Create;
  FStream := TFileStream.Create(AFileName, fmCreate);
  FEncoding := AEncoding;
  FOwnsStream := True;
end;

destructor TStreamWriter.Destroy;
begin
  if FOwnsStream and Assigned(FStream) then
    FStream.Free;
  inherited;
end;

procedure TStreamWriter.Write(const Value: string);
var
  Bytes: TBytes;
begin
  if not Assigned(FStream) then Exit;
  Bytes := FEncoding.GetBytes(Value);
  if Length(Bytes) > 0 then
    FStream.WriteBuffer(Bytes[0], Length(Bytes));
end;

procedure TStreamWriter.WriteLine(const Value: string);
begin
  Write(Value + sLineBreak);
end;
{$ENDIF}

end.
