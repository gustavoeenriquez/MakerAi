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
// TAiOpenAiWebSearchTool: Busqueda web via OpenAI (API Responses + tool web_search)
//
// Implementa IAiWebSearchTool para usarse como WebSearchTool en TAiChat
// (modos cmWebSearch y cmConversation con cap_WebSearch): le da busqueda web
// a un modelo que no la tiene (DeepSeek, Ollama, Groq...).
//
// La consulta va como 'input' a /v1/responses con tools=[{type:web_search}]
// (oct 25/2026: los modelos gpt-4o-*-search-preview de chat/completions fueron
// retirados por OpenAI, model_not_found). Modelo por defecto gpt-6-luna. La
// respuesta trae el texto y las citas (url_citation) en output[].content[].
//
// Uso:
//   SearchTool := TAiOpenAiWebSearchTool.Create(Self);
//   SearchTool.ApiKey := '@OPENAI_API_KEY';
//   AiConn.ChatTools.WebSearchTool := SearchTool;
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
  FModel  := 'gpt-6-luna';   // cualquier modelo actual; los *-search-preview fueron retirados
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
  LChoices     : TJSONArray;   // output[]
  LChoice      : TJSONValue;   // un item de output
  LMessage     : TJSONValue;   // un item de content
  LAnnotations : TJSONArray;   // content[]
  LAnnotation  : TJSONObject;
  LUrlCit      : TJSONArray;   // annotations[]
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
      // Request body: API Responses con la tool nativa web_search. Los modelos
      // *-search-preview de chat/completions fueron retirados (model_not_found,
      // verificado oct 25/2026); cualquier modelo actual busca con esta tool.
      LMessages := TJSONArray.Create;
      LMessages.Add(TJSONObject.Create.AddPair('type', 'web_search'));
      LReqJson.AddPair('model', FModel);
      LReqJson.AddPair('input', AQuery);
      LReqJson.AddPair('tools', LMessages);

      LBody := TStringStream.Create(LReqJson.ToJSON, TEncoding.UTF8);
      try
        HTTP.ContentType := 'application/json';
        HTTP.CustomHeaders['Authorization'] := 'Bearer ' + GetApiKey;
        LResponse := HTTP.Post(FUrl + 'responses', LBody);
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
        // Responses: output[] trae web_search_call y uno o mas 'message' cuyo
        // content[] tiene 'output_text' con text + annotations (url_citation plano:
        // url, title, start_index, end_index). Se concatena el texto de todos.
        LContent := '';
        LChoices := nil;
        LRespJson.TryGetValue<TJSONArray>('output', LChoices);
        if (not Assigned(LChoices)) or (LChoices.Count = 0) then
        begin
          ReportError('OpenAI WebSearch: respuesta sin output', nil);
          Exit;
        end;

        for LChoice in LChoices do
        begin
          if LChoice.GetValue<string>('type', '') <> 'message' then
            Continue;
          if not LChoice.TryGetValue<TJSONArray>('content', LAnnotations) then
            Continue;
          for LMessage in LAnnotations do
          begin
            if LMessage.GetValue<string>('type', '') <> 'output_text' then
              Continue;
            LContent := LContent + LMessage.GetValue<string>('text', '');
            // Citas
            if LMessage.TryGetValue<TJSONArray>('annotations', LUrlCit) then
              for I := 0 to LUrlCit.Count - 1 do
              begin
                LAnnotation := LUrlCit.Items[I] as TJSONObject;
                if LAnnotation.GetValue<string>('type', '') <> 'url_citation' then
                  Continue;
                LCitation := TAiMsgCitation.Create;
                LAnnotation.TryGetValue<Integer>('start_index', LCitation.StartIndex);
                LAnnotation.TryGetValue<Integer>('end_index',   LCitation.EndIndex);
                LSource            := TAiCitationSource.Create;
                LSource.SourceType := cstWeb;
                LAnnotation.TryGetValue<string>('url',   LSource.DataSource.Url);
                LAnnotation.TryGetValue<string>('title', LSource.DataSource.Title);
                LCitation.Sources.Add(LSource);
                ResMsg.Citations.Add(LCitation);
              end;
          end;
        end;
        ResMsg.Prompt := LContent;

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
