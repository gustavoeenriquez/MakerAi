unit uFetchTool;

interface

uses
  System.SysUtils, System.JSON,
  System.Net.HttpClient,
  uMakerAi.MCPServer.Core;

type
  TFetchParams = class
  private
    FUrl: string;
    FTimeout: Integer;
    FFollowRedirects: Boolean;
  public
    [AiMCPSchemaDescription('URL to fetch (HTTP or HTTPS)')]
    property Url: string read FUrl write FUrl;
    [AiMCPSchemaDescription('Request timeout in seconds. Default: 30')]
    [AiMCPOptional]
    property Timeout: Integer read FTimeout write FTimeout;
    [AiMCPSchemaDescription('Whether to follow redirects automatically. Default: true')]
    [AiMCPOptional]
    property FollowRedirects: Boolean read FFollowRedirects write FFollowRedirects;
  end;

  TFetchTool = class(TAiMCPToolBase<TFetchParams>)
  protected
    function ExecuteWithParams(const AParams: TFetchParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

implementation

uses
  System.Net.URLClient;

constructor TFetchTool.Create;
begin
  inherited;
  FName        := 'fetch';
  FDescription := 'Perform an HTTP GET request and return the response body as text. ' +
                  'Useful for retrieving web pages, APIs, or any URL-accessible content. ' +
                  'Returns the raw response text.';
end;

function TFetchTool.ExecuteWithParams(const AParams: TFetchParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  Client:   THTTPClient;
  Response: IHTTPResponse;
  Timeout:  Integer;
  Body:     string;
begin
  if Trim(AParams.Url) = '' then
    raise Exception.Create('URL cannot be empty');

  Timeout := AParams.Timeout;
  if Timeout <= 0 then Timeout := 30;

  Client := THTTPClient.Create;
  try
    Client.ResponseTimeout    := Timeout * 1000;
    Client.ConnectionTimeout  := Timeout * 1000;
    Client.HandleRedirects    := True; // FollowRedirects default = true (field default)
    if not AParams.FollowRedirects then
      Client.HandleRedirects  := False;

    Client.UserAgent := 'MakerAI-MCP-Fetch/1.0';

    Response := Client.Get(Trim(AParams.Url));
    Body := Response.ContentAsString(TEncoding.UTF8);

    // Prefix with status line for transparency
    Result := TAiMCPResponseBuilder.New
      .AddText(Format('HTTP %d %s'#10'Content-Type: %s'#10'Content-Length: %d'#10#10'%s',
        [Response.StatusCode,
         Response.StatusText,
         Response.HeaderValue['Content-Type'],
         Length(Body),
         Body]))
      .Build;
  finally
    Client.Free;
  end;
end;

end.
