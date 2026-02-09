// =============================================================================
// MakerAI Cross-Platform Compatibility Layer - Base64 Helper
// =============================================================================
// 
// Purpose: Unifica encoding/decoding Base64 entre Delphi (varias versiones) y FPC
//
// Compatibility:
//   - Delphi XE7+ (usa System.NetEncoding)
//   - Delphi XE2-XE6 (usa Soap.EncdDecd como fallback)
//   - Free Pascal 3.2+ (usa unit base64)
//
// Usage:
//   uses uBase64Helper;
//   var
//     Encoded: string;
//   begin
//     Encoded := EncodeStringBase64('Hello World');
//     ShowMessage(DecodeStringBase64(Encoded));
//   end;
//
// Developer Notes (Delphi):
//   - En Delphi XE7+ usa System.NetEncoding (API moderna y estándar)
//   - En Delphi XE2-XE6 usa Soap.EncdDecd (RTL legacy pero funcional)
//   - No requiere dependencias externas (Indy, etc.)
//
// =============================================================================

unit uBase64Helper;

{$include ../CompilerDirectives.inc}

interface

uses
  {$IFDEF FPC}
  Classes, SysUtils, base64
  {$ELSE}
  System.Classes, System.SysUtils
  {$IF CompilerVersion >= 28.0}    // Delphi XE7 and later
    , System.NetEncoding
  {$ELSE}                           // Delphi XE2-XE6
    , Soap.EncdDecd
  {$IFEND}
  {$ENDIF}
  ;

function EncodeStringBase64(const Input: string): string;
function DecodeStringBase64(const Input: string): string;

{$IFDEF FPC}
type
  TBase64EncodingShim = class
  public
    function Encode(const Input: string): string;
    function Decode(const Input: string): string;
    function EncodeBytesToString(const Input: Pointer; Size: Integer): string; overload;
    function EncodeBytesToString(const Input: TBytes): string; overload;
    function DecodeStringToBytes(const Input: string): TBytes;
  end;

  TNetEncoding = class
  private
    class var FBase64: TBase64EncodingShim;
  public
    class constructor Create;
    class destructor Destroy;
    class property Base64: TBase64EncodingShim read FBase64;
  end;
{$ENDIF}

implementation

function EncodeStringBase64(const Input: string): string;
{$IFNDEF FPC}
{$IF CompilerVersion < 28.0}
var
  Bytes: TBytes;
{$IFEND}
{$ENDIF}
begin
  {$IFDEF FPC}
  // Free Pascal: usa unit base64
  Result := base64.EncodeStringBase64(Input);
  
  {$ELSE}
  {$IF CompilerVersion >= 28.0}
  // Delphi XE7+: usa System.NetEncoding
  Result := TNetEncoding.Base64.Encode(Input);
  
  {$ELSE}
  // Delphi XE2-XE6: usa Soap.EncdDecd
  Bytes := TEncoding.UTF8.GetBytes(Input);
  Result := Soap.EncdDecd.EncodeString(TEncoding.UTF8.GetString(Bytes));
  
  {$IFEND}
  {$ENDIF}
end;

function DecodeStringBase64(const Input: string): string;
{$IFNDEF FPC}
{$IF CompilerVersion < 28.0}
var
  DecodedStr: AnsiString;
{$IFEND}
{$ENDIF}
begin
  {$IFDEF FPC}
  // Free Pascal: usa unit base64
  Result := base64.DecodeStringBase64(Input);
  
  {$ELSE}
  {$IF CompilerVersion >= 28.0}
  // Delphi XE7+: usa System.NetEncoding
  Result := TNetEncoding.Base64.Decode(Input);
  
  {$ELSE}
  // Delphi XE2-XE6: usa Soap.EncdDecd
  DecodedStr := Soap.EncdDecd.DecodeString(Input);
  Result := string(DecodedStr);
  
  {$IFEND}
  {$ENDIF}
end;

{$IFDEF FPC}
{ TNetEncodingShim }

class constructor TNetEncoding.Create;
begin
  FBase64 := TBase64EncodingShim.Create;
end;

class destructor TNetEncoding.Destroy;
begin
  FBase64.Free;
end;

{ TBase64EncodingShim }

function TBase64EncodingShim.Encode(const Input: string): string;
begin
  Result := base64.EncodeStringBase64(Input);
end;

function TBase64EncodingShim.Decode(const Input: string): string;
begin
  Result := base64.DecodeStringBase64(Input);
end;

function TBase64EncodingShim.EncodeBytesToString(const Input: Pointer; Size: Integer): string;
var
  S: AnsiString;
begin
  SetLength(S, Size);
  Move(Input^, S[1], Size);
  Result := base64.EncodeStringBase64(string(S));
end;

function TBase64EncodingShim.EncodeBytesToString(const Input: TBytes): string;
begin
  if Length(Input) = 0 then
    Result := ''
  else
    Result := EncodeBytesToString(@Input[0], Length(Input));
end;

function TBase64EncodingShim.DecodeStringToBytes(const Input: string): TBytes;
var
  S: string;
begin
  S := base64.DecodeStringBase64(Input);
  Result := TEncoding.ASCII.GetBytes(S); // Base64 decode returns "binary string". Use ASCII/Default encoding to get bytes.
  // Actually, DecodeStringBase64 returns "string" which is AnsiString/binary data.
  // Converting string to TBytes simply:
  SetLength(Result, Length(S));
  if Length(S) > 0 then
    Move(S[1], Result[0], Length(S));
end;
{$ENDIF}

end.
