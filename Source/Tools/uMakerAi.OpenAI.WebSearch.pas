// MIT License
//
// Copyright (c) 2026 Gustavo Enriquez
//
// Nombre: Gustavo Enriquez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com
// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - GitHub: https://github.com/gustavoeenriquez/

// -------------------------------------------------------------------------
// TAiOpenAiWebSearchTool: Busqueda web via OpenAI (gpt-4o-mini-search-preview)
//
// Implementa IAiWebSearchTool para usarse como WebSearchTool en TAiChat
// (modos cmWebSearch y cmConversation con cap_WebSearch).
//
// El modelo gpt-4o-mini-search-preview recibe la consulta como un mensaje
// de usuario en el endpoint /v1/chat/completions y devuelve la respuesta
// con citas en el array "annotations" del mensaje de respuesta.
//
// Uso:
//   SearchTool := TAiOpenAiWebSearchTool.Create(Self);
//   SearchTool.ApiKey := '@OPENAI_API_KEY';
//   AiConn.WebSearchTool := SearchTool;
//   AiConn.ChatMode := cmWebSearch;
// -------------------------------------------------------------------------

unit uMakerAi.OpenAI.WebSearch;

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  System.Net.HttpClient, System.Net.HttpClientComponent,
  System.Generics.Collections,
  uMakerAi.Core, uMakerAi.Chat.Tools, uMakerAi.Chat.Messages;

type

{$IF CompilerVersion >= 35}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidOSX64 or pidAndroidArm64)]
{$ELSE}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidOSX64)]
{$ENDIF}
  TAiOpenAiWebSearchTool = class(TAiWebSearchToolBase)
  private
    FApiKey: string;
    FModel:  string;
    FUrl:    string;
    function GetApiKey: string;
  protected
    procedure ExecuteSearch(const AQuery: string; ResMsg, AskMsg: TAiChatMessage); override;
  public
    constructor Create(AOwner: TComponent); override;

    class function Search(const AApiKey, AQuery: string;
                          const AModel: string = 'gpt-4o-mini-search-preview'): TAiChatMessage;
  published
    property ApiKey: string read GetApiKey write FApiKey;
    property Model:  string read FModel    write FModel;
    property Url:    string read FUrl      write FUrl;
  end;

procedure Register;

implementation

const
  GL_OPENAI_URL = 'https://api.openai.com/v1/';

procedure Register;
begin
  RegisterComponents('MakerAI Tools', [TAiOpenAiWebSearchTool]);
end;

{ TAiOpenAiWebSearchTool }

constructor TAiOpenAiWebSearchTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FApiKey := '@OPENAI_API_KEY';
  FModel  := 'gpt-4o-mini-search-preview';
  FUrl    := GL_OPENAI_URL;
end;

function TAiOpenAiWebSearchTool.GetApiKey: string;
begin
  if (csDesigning in ComponentState) then
    Exit(FApiKey);
  if FApiKey.StartsWith('@') then
    Result := GetEnvironmentVariable(Copy(FApiKey, 2, MaxInt))
  else
    Result := FApiKey;
end;

procedure TAiOpenAiWebSearchTool.ExecuteSearch(const AQuery: string; ResMsg, AskMsg: TAiChatMessage);
var
  HTTP         : TNetHTTPClient;
  LReqJson     : TJSONObject;
  LMessages    : TJSONArray;
  LBody        : TStringStream;
  LResponse    : IHTTPResponse;
  LRespJson    : TJSONObject;
  LChoices     : TJSONArray;
  LChoice      : TJSONObject;
  LMessage     : TJSONObject;
  LAnnotations : TJSONArray;
  LAnnotation  : TJSONObject;
  LUrlCit      : TJSONObject;
  LContent     : string;
  LCitation    : TAiMsgCitation;
  LSource      : TAiCitationSource;
  I            : Integer;
begin
  ReportState(acsReasoning, 'Buscando en la web...');

  HTTP     := TNetHTTPClient.Create(nil);
  LReqJson := TJSONObject.Create;
  try
    try
      // Request body
      LMessages := TJSONArray.Create;
      LMessages.Add(
        TJSONObject.Create
          .AddPair('role', 'user')
          .AddPair('content', AQuery));
      LReqJson.AddPair('model', FModel);
      LReqJson.AddPair('messages', LMessages);

      LBody := TStringStream.Create(LReqJson.ToJSON, TEncoding.UTF8);
      try
        HTTP.ContentType := 'application/json';
        HTTP.CustomHeaders['Authorization'] := 'Bearer ' + GetApiKey;
        LResponse := HTTP.Post(FUrl + 'chat/completions', LBody);
      finally
        LBody.Free;
      end;

      if LResponse.StatusCode <> 200 then
      begin
        ReportError('OpenAI WebSearch error ' + LResponse.StatusCode.ToString + ': ' +
                    LResponse.ContentAsString, nil);
        Exit;
      end;

      LRespJson := TJSONObject.ParseJSONValue(LResponse.ContentAsString) as TJSONObject;
      if not Assigned(LRespJson) then
      begin
        ReportError('OpenAI WebSearch: respuesta JSON invalida', nil);
        Exit;
      end;

      try
        LChoices := LRespJson.GetValue<TJSONArray>('choices');
        if (not Assigned(LChoices)) or (LChoices.Count = 0) then
          Exit;

        LChoice  := LChoices.Items[0] as TJSONObject;
        LMessage := LChoice.GetValue<TJSONObject>('message');
        if not Assigned(LMessage) then
          Exit;

        LMessage.TryGetValue<string>('content', LContent);
        ResMsg.Prompt := LContent;

        // Citas: annotations[] con type = "url_citation"
        if LMessage.TryGetValue<TJSONArray>('annotations', LAnnotations) then
        begin
          for I := 0 to LAnnotations.Count - 1 do
          begin
            LAnnotation := LAnnotations.Items[I] as TJSONObject;
            if LAnnotation.GetValue<string>('type') <> 'url_citation' then
              Continue;

            LUrlCit := LAnnotation.GetValue<TJSONObject>('url_citation');
            if not Assigned(LUrlCit) then
              Continue;

            LCitation := TAiMsgCitation.Create;
            LUrlCit.TryGetValue<Integer>('start_index', LCitation.StartIndex);
            LUrlCit.TryGetValue<Integer>('end_index',   LCitation.EndIndex);

            LSource            := TAiCitationSource.Create;
            LSource.SourceType := cstWeb;
            LUrlCit.TryGetValue<string>('url',   LSource.DataSource.Url);
            LUrlCit.TryGetValue<string>('title', LSource.DataSource.Title);

            LCitation.Sources.Add(LSource);
            ResMsg.Citations.Add(LCitation);
          end;
        end;

        ReportDataEnd(ResMsg, 'assistant', LContent);
      finally
        LRespJson.Free;
      end;
    except
      on E: Exception do
        ReportError('OpenAI WebSearch excepcion: ' + E.Message, E);
    end;
  finally
    HTTP.Free;
    LReqJson.Free;
  end;
end;

class function TAiOpenAiWebSearchTool.Search(const AApiKey, AQuery: string;
                                              const AModel: string): TAiChatMessage;
var
  Tool: TAiOpenAiWebSearchTool;
begin
  Tool := TAiOpenAiWebSearchTool.Create(nil);
  try
    Tool.FApiKey := AApiKey;
    Tool.FModel  := AModel;
    Result := TAiChatMessage.Create('', 'assistant');
    Tool.ExecuteSearch(AQuery, Result, nil);
  finally
    Tool.Free;
  end;
end;

end.
