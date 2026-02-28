// MIT License - Copyright (c) 2024-2026 Gustavo Enriquez
//
// FPC shim de compatibilidad para la unit Delphi "EncdDecd"
// Provee EncodeBase64 y DecodeBase64 con la misma firma que Delphi.
// Implementacion interna via la unit "base64" de FPC (fcl-base).

unit EncdDecd;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

// Codifica bytes crudos a base64 (firma identica a Delphi EncdDecd)
function EncodeBase64(const InputBuffer: Pointer;
    const InputByteCount: PtrInt): string;

// Decodifica base64 a bytes (firma identica a Delphi EncdDecd)
function DecodeBase64(const InputBuffer: string): TBytes;

implementation

uses
  base64, Classes;

function EncodeBase64(const InputBuffer: Pointer;
    const InputByteCount: PtrInt): string;
var
  SrcStream : TMemoryStream;
  MemStream : TMemoryStream;
  B64Stream : TBase64EncodingStream;
begin
  Result    := '';
  SrcStream := TMemoryStream.Create;
  MemStream := TMemoryStream.Create;
  B64Stream := TBase64EncodingStream.Create(MemStream);
  try
    if (InputBuffer <> nil) and (InputByteCount > 0) then
      SrcStream.WriteBuffer(InputBuffer^, InputByteCount);
    SrcStream.Position := 0;
    if SrcStream.Size > 0 then
      B64Stream.CopyFrom(SrcStream, SrcStream.Size);
    B64Stream.Flush;
    MemStream.Position := 0;
    SetLength(Result, MemStream.Size);
    if MemStream.Size > 0 then
      MemStream.ReadBuffer(PChar(Result)^, MemStream.Size);
  finally
    B64Stream.Free;
    MemStream.Free;
    SrcStream.Free;
  end;
end;

function DecodeBase64(const InputBuffer: string): TBytes;
var
  SrcStream  : TStringStream;
  DestStream : TMemoryStream;
  B64Stream  : TBase64DecodingStream;
begin
  Result     := nil;
  SrcStream  := TStringStream.Create(InputBuffer);
  DestStream := TMemoryStream.Create;
  B64Stream  := TBase64DecodingStream.Create(SrcStream, bdmMIME);
  try
    DestStream.CopyFrom(B64Stream, 0);
    SetLength(Result, DestStream.Size);
    if DestStream.Size > 0 then
    begin
      DestStream.Position := 0;
      DestStream.ReadBuffer(Result[0], DestStream.Size);
    end;
  finally
    B64Stream.Free;
    DestStream.Free;
    SrcStream.Free;
  end;
end;

end.
