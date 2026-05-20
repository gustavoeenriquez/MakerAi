unit uHttpRequestTool;

interface

uses
  System.SysUtils, System.JSON,
  System.Net.HttpClient,
  uMakerAi.MCPServer.Core;

type
  THttpRequestParams = class
  private
    FMethod: string;
    FUrl: string;
    FHeaders: string;
    FBody: string;
    FTimeout: Integer;
  public
    [AiMCPSchemaDescription('HTTP method: GET, POST, PUT, DELETE, PATCH, HEAD, OPTIONS')]
    property Method: string read FMethod write FMethod;
    [AiMCPSchemaDescription('Target URL (HTTP or HTTPS)')]
    property Url: string read FUrl write FUrl;
    [AiMCPSchemaDescription('Request headers as a JSON object string. Example: "{\"Content-Type\":\"application/json\",\"Authorization\":\"Bearer TOKEN\"}"')]
    [AiMCPOptional]
    property Headers: string read FHeaders write FHeaders;
    [AiMCPSchemaDescription('Request body (for POST, PUT, PATCH). Leave empty for GET/DELETE.')]
    [AiMCPOptional]
    property Body: string read FBody write FBody;
    [AiMCPSchemaDescription('Request timeout in seconds. Default: 30')]
    [AiMCPOptional]
    property Timeout: Integer read FTimeout write FTimeout;
  end;

  THttpRequestTool = class(TAiMCPToolBase<THttpRequestParams>)
  protected
    function ExecuteWithParams(const AParams: THttpRequestParams;
      const AuthContext: TAiAuthContext): TJSONObject; override;
  public
    constructor Create; override;
  end;

implementation

uses
  System.Classes, System.Net.URLClient;

constructor THttpRequestTool.Create;
begin
  inherited;
  FName        := 'http_request';
  FDescription := 'Perform any HTTP request (GET, POST, PUT, DELETE, PATCH) with custom ' +
                  'headers and body. Returns status code, response headers, and body. ' +
                  'Headers should be provided as a JSON object string.';
end;

function THttpRequestTool.ExecuteWithParams(const AParams: THttpRequestParams;
  const AuthContext: TAiAuthContext): TJSONObject;
var
  Client:   THTTPClient;
  Response: IHTTPResponse;
  Method:   string;
  Timeout:  Integer;
  BodyStream: TStringStream;
  HeadersJSON: TJSONObject;
  Pair:        TJSONPair;
  Output:      TStringBuilder;
begin
  Method := UpperCase(Trim(AParams.Method));
  if Method = '' then Method := 'GET';

  Timeout := AParams.Timeout;
  if Timeout <= 0 then Timeout := 30;

  Client := THTTPClient.Create;
  try
    Client.ResponseTimeout   := Timeout * 1000;
    Client.ConnectionTimeout := Timeout * 1000;
    Client.HandleRedirects   := True;
    Client.UserAgent         := 'MakerAI-MCP-HttpRequest/1.0';

    // Parse and apply custom headers
    if Trim(AParams.Headers) <> '' then
    begin
      HeadersJSON := TJSONObject.ParseJSONValue(AParams.Headers) as TJSONObject;
      if Assigned(HeadersJSON) then
      try
        for Pair in HeadersJSON do
          Client.CustomHeaders[Pair.JsonString.Value] := Pair.JsonValue.Value;
      finally
        HeadersJSON.Free;
      end;
    end;

    // Prepare body stream if needed
    BodyStream := nil;
    if (Method <> 'GET') and (Method <> 'DELETE') and (Method <> 'HEAD') and
       (Trim(AParams.Body) <> '') then
      BodyStream := TStringStream.Create(AParams.Body, TEncoding.UTF8);

    try
      if      Method = 'GET'     then Response := Client.Get(AParams.Url)
      else if Method = 'DELETE'  then Response := Client.Delete(AParams.Url)
      else if Method = 'HEAD'    then Response := Client.Head(AParams.Url)
      else if Method = 'POST'    then Response := Client.Post(AParams.Url, BodyStream)
      else if Method = 'PUT'     then Response := Client.Put(AParams.Url, BodyStream)
      else if Method = 'PATCH'   then Response := Client.Patch(AParams.Url, BodyStream)
      else if Method = 'OPTIONS' then Response := Client.Options(AParams.Url)
      else
        raise Exception.CreateFmt('Unsupported HTTP method: "%s"', [Method]);
    finally
      FreeAndNil(BodyStream);
    end;

    // Build response
    Output := TStringBuilder.Create;
    try
      Output.AppendFormat('Status: %d %s', [Response.StatusCode, Response.StatusText]);
      Output.AppendLine;

      // Response headers
      for var H in Response.Headers do
        Output.AppendFormat('%s: %s', [H.Name, H.Value]).AppendLine;

      Output.AppendLine;
      Output.Append(Response.ContentAsString(TEncoding.UTF8));

      Result := TAiMCPResponseBuilder.New.AddText(Output.ToString).Build;
    finally
      Output.Free;
    end;
  finally
    Client.Free;
  end;
end;

end.
