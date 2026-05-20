unit uBase64Tool;

interface

uses
  System.SysUtils, System.StrUtils, System.Classes, System.NetEncoding,
  System.IOUtils, System.JSON,
  uMakerAi.MCPServer.Core;

type
  // --- Tool 1: base64_encode ---
  TBase64EncodeParams = class
  private
    FText: string;
    FUrlSafe: Boolean;
  public
    [AiMCPSchemaDescription('Text to encode to base64')]
    property Text: string read FText write FText;
    [AiMCPSchemaDescription('Use URL-safe base64 (no +/ chars). Default: false')]
    [AiMCPOptional]
    property UrlSafe: Boolean read FUrlSafe write FUrlSafe;
  end;

  TBase64EncodeTool = class(TAiMCPToolBase<TBase64EncodeParams>)
  protected
    function ExecuteWithParams(const AParams: TBase64EncodeParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

  // --- Tool 2: base64_decode ---
  TBase64DecodeParams = class
  private
    FBase64: string;
  public
    [AiMCPSchemaDescription('Base64 encoded string to decode')]
    property Base64: string read FBase64 write FBase64;
  end;

  TBase64DecodeTool = class(TAiMCPToolBase<TBase64DecodeParams>)
  protected
    function ExecuteWithParams(const AParams: TBase64DecodeParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

  // --- Tool 3: base64_encode_file ---
  TBase64EncodeFileParams = class
  private
    FFilePath: string;
    FUrlSafe: Boolean;
  public
    [AiMCPSchemaDescription('Path to the file to encode')]
    property FilePath: string read FFilePath write FFilePath;
    [AiMCPSchemaDescription('Use URL-safe base64. Default: false')]
    [AiMCPOptional]
    property UrlSafe: Boolean read FUrlSafe write FUrlSafe;
  end;

  TBase64EncodeFileTool = class(TAiMCPToolBase<TBase64EncodeFileParams>)
  protected
    function ExecuteWithParams(const AParams: TBase64EncodeFileParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

  // --- Tool 4: base64_decode_file ---
  TBase64DecodeFileParams = class
  private
    FBase64: string;
    FOutputPath: string;
  public
    [AiMCPSchemaDescription('Base64 encoded content to write')]
    property Base64: string read FBase64 write FBase64;
    [AiMCPSchemaDescription('Path of the file to create/overwrite')]
    property OutputPath: string read FOutputPath write FOutputPath;
  end;

  TBase64DecodeFileTool = class(TAiMCPToolBase<TBase64DecodeFileParams>)
  protected
    function ExecuteWithParams(const AParams: TBase64DecodeFileParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

implementation

// -------------------------------------------------------------------------
// TBase64EncodeTool
// -------------------------------------------------------------------------

constructor TBase64EncodeTool.Create;
begin
  inherited;
  FName        := 'base64_encode';
  FDescription := 'Encode a UTF-8 string to base64. Supports standard or URL-safe encoding.';
end;

function TBase64EncodeTool.ExecuteWithParams(const AParams: TBase64EncodeParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  Encoded: string;
begin
  if AParams.UrlSafe then
    Encoded := TNetEncoding.Base64URL.Encode(AParams.Text)
  else
    Encoded := TNetEncoding.Base64.Encode(AParams.Text);

  Result := TAiMCPResponseBuilder.New
    .AddText('Encoded (' + IntToStr(Length(AParams.Text)) + ' chars -> ' +
             IntToStr(Length(Encoded)) + ' base64 chars):' + sLineBreak + Encoded)
    .Build;
end;

// -------------------------------------------------------------------------
// TBase64DecodeTool
// -------------------------------------------------------------------------

constructor TBase64DecodeTool.Create;
begin
  inherited;
  FName        := 'base64_decode';
  FDescription := 'Decode a base64 string to UTF-8 text.';
end;

function TBase64DecodeTool.ExecuteWithParams(const AParams: TBase64DecodeParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  Decoded: string;
begin
  if Trim(AParams.Base64) = '' then
    raise Exception.Create('Base64 input cannot be empty');

  try
    Decoded := TNetEncoding.Base64.Decode(AParams.Base64);
  except
    on E: Exception do
      raise Exception.Create('Invalid base64 input: ' + E.Message);
  end;

  Result := TAiMCPResponseBuilder.New
    .AddText('Decoded (' + IntToStr(Length(AParams.Base64)) + ' base64 chars -> ' +
             IntToStr(Length(Decoded)) + ' chars):' + sLineBreak + Decoded)
    .Build;
end;

// -------------------------------------------------------------------------
// TBase64EncodeFileTool
// -------------------------------------------------------------------------

constructor TBase64EncodeFileTool.Create;
begin
  inherited;
  FName        := 'base64_encode_file';
  FDescription := 'Read a file and return its contents encoded as base64. ' +
                  'Supports standard or URL-safe encoding.';
end;

function TBase64EncodeFileTool.ExecuteWithParams(const AParams: TBase64EncodeFileParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  Bytes: TBytes;
  Encoded: string;
  FileSizeKB: string;
begin
  if Trim(AParams.FilePath) = '' then
    raise Exception.Create('FilePath cannot be empty');

  if not TFile.Exists(AParams.FilePath) then
    raise Exception.CreateFmt('File not found: "%s"', [AParams.FilePath]);

  Bytes := TFile.ReadAllBytes(AParams.FilePath);

  if AParams.UrlSafe then
    Encoded := TNetEncoding.Base64URL.EncodeBytesToString(Bytes)
  else
    Encoded := TNetEncoding.Base64.EncodeBytesToString(Bytes);

  FileSizeKB := FormatFloat('0.##', Length(Bytes) / 1024);

  Result := TAiMCPResponseBuilder.New
    .AddText('File: ' + AParams.FilePath + sLineBreak +
             'Size: ' + IntToStr(Length(Bytes)) + ' bytes (' + FileSizeKB + ' KB)' + sLineBreak +
             'Encoded length: ' + IntToStr(Length(Encoded)) + ' base64 chars' + sLineBreak +
             'Encoding: ' + IfThen(AParams.UrlSafe, 'URL-safe base64', 'standard base64') +
             sLineBreak + sLineBreak + Encoded)
    .Build;
end;

// -------------------------------------------------------------------------
// TBase64DecodeFileTool
// -------------------------------------------------------------------------

constructor TBase64DecodeFileTool.Create;
begin
  inherited;
  FName        := 'base64_decode_file';
  FDescription := 'Decode a base64 string and write the binary result to a file. ' +
                  'Useful for restoring encoded files or writing binary data.';
end;

function TBase64DecodeFileTool.ExecuteWithParams(const AParams: TBase64DecodeFileParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  Bytes: TBytes;
begin
  if Trim(AParams.Base64) = '' then
    raise Exception.Create('Base64 input cannot be empty');

  if Trim(AParams.OutputPath) = '' then
    raise Exception.Create('OutputPath cannot be empty');

  try
    Bytes := TNetEncoding.Base64.DecodeStringToBytes(AParams.Base64);
  except
    on E: Exception do
      raise Exception.Create('Invalid base64 input: ' + E.Message);
  end;

  TFile.WriteAllBytes(AParams.OutputPath, Bytes);

  Result := TAiMCPResponseBuilder.New
    .AddText('Success: wrote ' + IntToStr(Length(Bytes)) + ' bytes to "' +
             AParams.OutputPath + '"')
    .Build;
end;

end.
