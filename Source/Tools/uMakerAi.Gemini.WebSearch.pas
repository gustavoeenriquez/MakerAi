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
  FApiKey := '@GEMINI_API_KEY';
  FUrl := 'https://generativelanguage.googleapis.com/v1beta/';
  FModel := 'gemini-2.0-flash'; // O gemini-1.5-pro
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
  // Si IsAsync=True ya estamos en el hilo background del chat: ejecutar directo
  // para evitar un TTask anidado que causar?a dangling pointer sobre ResMsg.
  // Si IsAsync=False estamos en el hilo principal: lanzar task para no bloquearlo.
  if IsAsync then
    InternalRunGeminiSearch(AQuery, ResMsg)
  else
    TTask.Run(procedure begin InternalRunGeminiSearch(AQuery, ResMsg); end);
end;

{ --- IMPLEMENTACI?N DE B?SQUEDA --- }

function TAiGeminiWebSearchTool.InternalRunGeminiSearch(const AQuery: string; ResMsg: TAiChatMessage): string;
var
  HTTP: TNetHTTPClient;
  LUrl: string;
  LRequestJson, LContents, LPart, LTool, LUsage, LSearchTool, LDynamicConfig: TJSONObject;
  LBody: TStringStream;
  LResponse: IHTTPResponse;
  LResponseJson, LGrounding, LChunk, LSupport, LSource: TJSONObject;
  LChunks, LSupports, LIndices: TJSONArray;
  LMsg: TAiChatMessage;
  I, J: Integer;
  LCitation: TAiMsgCitation;
  LCitSource: TAiCitationSource;
  LIdx: Integer;
begin
  Result := '';
  LMsg := TAiChatMessage(ResMsg);

  // 1. Construir URL del Endpoint
  LUrl := Format('%smodels/%s:generateContent?key=%s', [FUrl, FModel, GetApiKey]);

  HTTP := TNetHTTPClient.Create(nil);
  LRequestJson := TJSONObject.Create;
  try
    // 2. Preparar el Prompt (Contents)
    LPart := TJSONObject.Create.AddPair('text', AQuery);
    LContents := TJSONObject.Create.AddPair('parts', TJSONArray.Create.Add(LPart));
    LRequestJson.AddPair('contents', TJSONArray.Create.Add(LContents));

    // 3. Configuraci?n de Herramientas (Grounding)
    if FDynamicThreshold > 0 then
    begin
      // MODO DIN?MICO: El modelo decide si buscar en Google o no
      LDynamicConfig := TJSONObject.Create;
      LDynamicConfig.AddPair('mode', 'MODE_DYNAMIC');
      LDynamicConfig.AddPair('dynamic_threshold', TJSONNumber.Create(FDynamicThreshold));

      LSearchTool := TJSONObject.Create;
      LSearchTool.AddPair('dynamic_retrieval_config', LDynamicConfig);

      LTool := TJSONObject.Create.AddPair('google_search_retrieval', LSearchTool);
    end
    else
    begin
      // MODO FORZADO: Siempre realiza b?squeda en Google
      LTool := TJSONObject.Create.AddPair('google_search', TJSONObject.Create);
    end;

    LRequestJson.AddPair('tools', TJSONArray.Create.Add(LTool));

    // 4. Ejecuci?n del POST
    LBody := TStringStream.Create(LRequestJson.ToJSON, TEncoding.UTF8);
    try
      HTTP.ContentType := 'application/json';
      ReportState(acsReasoning, 'Buscando informaci?n actualizada en Google...');
      LResponse := HTTP.Post(LUrl, LBody);
    finally
      LBody.Free;
    end;

    // 5. Procesar Respuesta Exitosa
    if LResponse.StatusCode = 200 then
    begin
      LResponseJson := TJSONObject.ParseJSONValue(LResponse.ContentAsString) as TJSONObject;
      try
        // A. Extraer Texto Principal
        Result := LResponseJson.GetValue<string>('candidates[0].content.parts[0].text', '');
        LMsg.Prompt := Result;

        // B. Actualizar Estad?sticas de Tokens
        if LResponseJson.TryGetValue<TJSONObject>('usageMetadata', LUsage) then
        begin
          var LPt: Integer := 0;
          var LCt: Integer := 0;
          var LTt: Integer := 0;
          LUsage.TryGetValue<Integer>('promptTokenCount', LPt);
          LUsage.TryGetValue<Integer>('candidatesTokenCount', LCt);
          LUsage.TryGetValue<Integer>('totalTokenCount', LTt);
          LMsg.Prompt_tokens := LMsg.Prompt_tokens + LPt;
          LMsg.Completion_tokens := LMsg.Completion_tokens + LCt;
          LMsg.Total_tokens := LMsg.Total_tokens + LTt;
        end;

        // C. Procesar Metadatos de Grounding (Citas e Inline Citations)
        if LResponseJson.TryGetValue<TJSONObject>('candidates[0].groundingMetadata', LGrounding) then
        begin
          if LGrounding.TryGetValue<TJSONArray>('groundingChunks', LChunks) and LGrounding.TryGetValue<TJSONArray>('groundingSupports', LSupports) then
          begin
            for I := 0 to LSupports.Count - 1 do
            begin
              LSupport := LSupports.Items[I] as TJSONObject;
              LCitation := TAiMsgCitation.Create;

              // Segmentos de texto vinculados a fuentes
              LCitation.StartIndex := LSupport.GetValue<TJSONObject>('segment').GetValue<Integer>('startIndex', 0);
              LCitation.EndIndex := LSupport.GetValue<TJSONObject>('segment').GetValue<Integer>('endIndex', 0);
              LCitation.Text := LSupport.GetValue<TJSONObject>('segment').GetValue<string>('text', '');

              // Vincular con las fuentes (web chunks)
              if LSupport.TryGetValue<TJSONArray>('groundingChunkIndices', LIndices) then
              begin
                for J := 0 to LIndices.Count - 1 do
                begin
                  LIdx := (LIndices.Items[J] as TJSONNumber).AsInt;
                  if LIdx < LChunks.Count then
                  begin
                    LChunk := LChunks.Items[LIdx] as TJSONObject;
                    if LChunk.TryGetValue<TJSONObject>('web', LSource) then
                    begin
                      LCitSource := TAiCitationSource.Create;
                      LCitSource.SourceType := cstWeb;
                      LCitSource.DataSource.Title := LSource.GetValue<string>('title', '');
                      LCitSource.DataSource.Url := LSource.GetValue<string>('uri', '');
                      LCitation.Sources.Add(LCitSource);
                    end;
                  end;
                end;
              end;
              LMsg.Citations.Add(LCitation);
            end;
          end;
        end;

        // Notificar el fin de los datos
        ReportDataEnd(ResMsg, 'assistant', Result);
      finally
        LResponseJson.Free;
      end;
    end
    else
    begin
      // Manejo de errores de la API
      ReportError('Gemini Search Grounding Error: ' + LResponse.ContentAsString, nil);
    end;

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
