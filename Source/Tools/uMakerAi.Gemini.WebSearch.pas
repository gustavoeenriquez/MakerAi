unit uMakerAi.Gemini.WebSearch;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Net.HttpClient,
  System.Net.HttpClientComponent, System.Net.URLClient, System.Threading,
  System.Generics.Collections, uMakerAi.Core, uMakerAi.Chat.Tools, uMakerAi.Chat, uMakerAi.Chat.Messages;

type

{$IF CompilerVersion >= 35}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidOSX64 or pidAndroidArm64)]
{$ELSE}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidOSX64)]
{$ENDIF}
  TAiGeminiWebSearchTool = class(TAiWebSearchToolBase)
  private
    FApiKey: string;
    FModel: string;
    FUrl: string;
    FDynamicThreshold: Single;
    function GetApiKey: string;
  protected
    procedure ExecuteSearch(const AQuery: string; ResMsg, AskMsg: TAiChatMessage); override;
    function InternalRunGeminiSearch(const AQuery: string; ResMsg: TAiChatMessage): string;
  public
    constructor Create(AOwner: TComponent); override;

    { Uso est?tico }
    class function Search(const AApiKey, AQuery: string; const AModel: string = 'gemini-2.0-flash'; const ADynamicThreshold: Single = 0.7): TAiChatMessage;
  published
    property ApiKey: string read GetApiKey write FApiKey;
    property Model: string read FModel write FModel;
    property Url: string read FUrl write FUrl;
    property DynamicThreshold: Single read FDynamicThreshold write FDynamicThreshold;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiGeminiWebSearchTool]);
end;

{ TAiGeminiWebSearchTool }

constructor TAiGeminiWebSearchTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FApiKey           := '@GEMINI_API_KEY';
  FUrl              := 'https://generativelanguage.googleapis.com/v1beta/';
  FModel            := 'gemini-2.0-flash';
  FDynamicThreshold := 0;  // 0 = google_search (todos los modelos)
                            // >0 = google_search_retrieval (solo gemini-1.5-*)
end;

function TAiGeminiWebSearchTool.GetApiKey: string;
begin
  if (csDesigning in ComponentState) then
    Exit(FApiKey);
  if FApiKey.StartsWith('@') then
    Result := GetEnvironmentVariable(Copy(FApiKey, 2, MaxInt))
  else
    Result := FApiKey;
end;

procedure TAiGeminiWebSearchTool.ExecuteSearch(const AQuery: string; ResMsg, AskMsg: TAiChatMessage);
begin
  // Siempre síncrono: en async mode ya corremos en un hilo background;
  // en sync mode (SmartDispatch, llamada directa) necesitamos el resultado
  // antes de retornar. TTask.Run aquí rompería ambos flujos.
  InternalRunGeminiSearch(AQuery, ResMsg);
end;

function TAiGeminiWebSearchTool.InternalRunGeminiSearch(const AQuery: string; ResMsg: TAiChatMessage): string;
var
  HTTP          : TNetHTTPClient;
  LUrl          : string;
  LRequestJson  : TJSONObject;
  LTool         : TJSONObject;
  LBody         : TStringStream;
  LResponse     : IHTTPResponse;
  LResponseJson : TJSONObject;
  LCandidates   : TJSONArray;
  LCandidate    : TJSONObject;
  LContent      : TJSONObject;
  LParts        : TJSONArray;
  LUsage        : TJSONObject;
  LGrounding    : TJSONObject;
  LChunks       : TJSONArray;
  LSupports     : TJSONArray;
  LSupport      : TJSONObject;
  LSegment      : TJSONObject;
  LIndices      : TJSONArray;
  LChunk        : TJSONObject;
  LSource       : TJSONObject;
  LCitation     : TAiMsgCitation;
  LCitSource    : TAiCitationSource;
  I, J, LIdx    : Integer;
begin
  Result := '';

  LUrl := Format('%smodels/%s:generateContent?key=%s', [FUrl, FModel, GetApiKey]);

  HTTP         := TNetHTTPClient.Create(nil);
  LRequestJson := TJSONObject.Create;
  try
    // Contents
    LRequestJson.AddPair('contents',
      TJSONArray.Create.Add(
        TJSONObject.Create.AddPair('parts',
          TJSONArray.Create.Add(
            TJSONObject.Create.AddPair('text', AQuery)))));

    // Tool: google_search (gemini-2.0+) o google_search_retrieval (gemini-1.5-*)
    if (FDynamicThreshold > 0) and FModel.Contains('1.5') then
    begin
      var LDynCfg := TJSONObject.Create;
      LDynCfg.AddPair('mode', 'MODE_DYNAMIC');
      LDynCfg.AddPair('dynamic_threshold', TJSONNumber.Create(FDynamicThreshold));
      var LRetrieval := TJSONObject.Create;
      LRetrieval.AddPair('dynamic_retrieval_config', LDynCfg);
      LTool := TJSONObject.Create.AddPair('google_search_retrieval', LRetrieval);
    end
    else
      LTool := TJSONObject.Create.AddPair('google_search', TJSONObject.Create);

    LRequestJson.AddPair('tools', TJSONArray.Create.Add(LTool));

    // POST
    LBody := TStringStream.Create(LRequestJson.ToJSON, TEncoding.UTF8);
    try
      HTTP.ContentType := 'application/json';
      ReportState(acsReasoning, 'Buscando información actualizada en Google...');
      LResponse := HTTP.Post(LUrl, LBody);
    finally
      LBody.Free;
    end;

    if LResponse.StatusCode = 200 then
    begin
      LResponseJson := TJSONObject.ParseJSONValue(LResponse.ContentAsString) as TJSONObject;
      try
        // Tokens
        if LResponseJson.TryGetValue<TJSONObject>('usageMetadata', LUsage) then
        begin
          var LPt, LCt, LTt: Integer;
          LPt := 0; LCt := 0; LTt := 0;
          LUsage.TryGetValue<Integer>('promptTokenCount',      LPt);
          LUsage.TryGetValue<Integer>('candidatesTokenCount',  LCt);
          LUsage.TryGetValue<Integer>('totalTokenCount',       LTt);
          ResMsg.Prompt_tokens     := ResMsg.Prompt_tokens     + LPt;
          ResMsg.Completion_tokens := ResMsg.Completion_tokens + LCt;
          ResMsg.Total_tokens      := ResMsg.Total_tokens      + LTt;
        end;

        // candidates[0] — traversal manual (path notation no funciona en Delphi)
        if LResponseJson.TryGetValue<TJSONArray>('candidates', LCandidates) and
           (LCandidates.Count > 0) then
        begin
          LCandidate := LCandidates.Items[0] as TJSONObject;

          // candidates[0].content.parts[0].text
          if LCandidate.TryGetValue<TJSONObject>('content', LContent) then
            if LContent.TryGetValue<TJSONArray>('parts', LParts) and (LParts.Count > 0) then
              (LParts.Items[0] as TJSONObject).TryGetValue<string>('text', Result);

          ResMsg.Prompt := Result;

          // candidates[0].groundingMetadata
          if LCandidate.TryGetValue<TJSONObject>('groundingMetadata', LGrounding) then
            if LGrounding.TryGetValue<TJSONArray>('groundingChunks',   LChunks) and
               LGrounding.TryGetValue<TJSONArray>('groundingSupports', LSupports) then
            begin
              for I := 0 to LSupports.Count - 1 do
              begin
                LSupport  := LSupports.Items[I] as TJSONObject;
                LCitation := TAiMsgCitation.Create;

                if LSupport.TryGetValue<TJSONObject>('segment', LSegment) then
                begin
                  LSegment.TryGetValue<Integer>('startIndex', LCitation.StartIndex);
                  LSegment.TryGetValue<Integer>('endIndex',   LCitation.EndIndex);
                  LSegment.TryGetValue<string>('text',        LCitation.Text);
                end;

                if LSupport.TryGetValue<TJSONArray>('groundingChunkIndices', LIndices) then
                  for J := 0 to LIndices.Count - 1 do
                  begin
                    LIdx := (LIndices.Items[J] as TJSONNumber).AsInt;
                    if (LIdx >= 0) and (LIdx < LChunks.Count) then
                    begin
                      LChunk := LChunks.Items[LIdx] as TJSONObject;
                      if LChunk.TryGetValue<TJSONObject>('web', LSource) then
                      begin
                        LCitSource            := TAiCitationSource.Create;
                        LCitSource.SourceType := cstWeb;
                        LSource.TryGetValue<string>('title', LCitSource.DataSource.Title);
                        LSource.TryGetValue<string>('uri',   LCitSource.DataSource.Url);
                        LCitation.Sources.Add(LCitSource);
                      end;
                    end;
                  end;

                ResMsg.Citations.Add(LCitation);
              end;
            end;
        end;

        ReportDataEnd(ResMsg, 'assistant', Result);
      finally
        LResponseJson.Free;
      end;
    end
    else
      ReportError('Gemini Search Error ' + LResponse.StatusCode.ToString + ': ' + LResponse.ContentAsString, nil);

  finally
    LRequestJson.Free;
    HTTP.Free;
  end;
end;

{ --- CLASE FUNCI?N EST?TICA --- }

class function TAiGeminiWebSearchTool.Search(const AApiKey, AQuery, AModel: string; const ADynamicThreshold: Single): TAiChatMessage;
var
  LInstance: TAiGeminiWebSearchTool;
begin
  // Creamos el mensaje que retornar? con la respuesta y las citas
  Result := TAiChatMessage.Create('', 'assistant');

  // Creamos una instancia temporal de la herramienta para realizar la labor
  LInstance := TAiGeminiWebSearchTool.Create(nil);
  try
    LInstance.ApiKey := AApiKey;
    LInstance.Model := AModel;
    LInstance.DynamicThreshold := ADynamicThreshold;

    // Ejecuci?n s?ncrona
    LInstance.InternalRunGeminiSearch(AQuery, Result);
  finally
    LInstance.Free;
  end;
end;


end.
