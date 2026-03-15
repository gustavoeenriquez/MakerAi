// Pendiente [TODO] //Estimar el costo de la generaci?n ya que no retorna el consumo
// https://ai.google.dev/gemini-api/docs/pricing

unit uMakerAi.Gemini.Speech;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Net.HttpClient,
  System.Net.HttpClientComponent, System.Net.URLClient, System.Threading,
  System.StrUtils, uMakerAi.Core, uMakerAi.Chat.Tools, uMakerAi.Utils.PcmToWav, uMakerAi.Chat.Messages;

type
  // [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidOSX64 or {pidiOSArm64 or} pidAndroidArm64)]
  TAiGeminiSpeechTool = class(TAiSpeechToolBase)
  private
    FApiKey: string;
    FModel: string;
    FUrl: string;
    FVoice: string; // Ahora es String para soportar "Nombre=Voz, Nombre2=Voz2"
    FAudioProfile: TStrings;
    FScene: TStrings;
    FDirectorsNotes: TStrings;
    FPrompt_tokens: Integer;
    FCompletion_tokens: Integer;
    FTotal_tokens: Integer;
    FTranscriptionModel: string;
    FTranscriptionPrompt: string;

    procedure SetAudioProfile(const Value: TStrings);
    procedure SetScene(const Value: TStrings);
    procedure SetDirectorsNotes(const Value: TStrings);
    function BuildFullPrompt(const AText: string): string;
    function GetApiKey: string;

    { Tu rutina de parseo adaptada al componente }
    function BuildSpeechConfigJson: TJSONObject;
  protected
    procedure ExecuteTranscription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage); override;
    procedure ExecuteSpeechGeneration(const AText: string; ResMsg, AskMsg: TAiChatMessage); override;

    function InternalRunGeminiTTS(const AText: string; ResMsg: TAiChatMessage): string;
    function InternalRunGeminiTranscription(aMediaFile: TAiMediaFile): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GenerateSpeech(const AApiKey, AText: string; const AVice: string; AAudioProfile, AScene, ADirectorsNotes: TStrings): TAiMediaFile;
  published
    property ApiKey: string read GetApiKey write FApiKey;
    property Model: string read FModel write FModel;
    property Url: string read FUrl write FUrl;

    { Soporta: "Puck" o "Anya=Kore, Liam=Puck" }
    property Voice: string read FVoice write FVoice;

    property AudioProfile: TStrings read FAudioProfile write SetAudioProfile;
    property Scene: TStrings read FScene write SetScene;
    property DirectorsNotes: TStrings read FDirectorsNotes write SetDirectorsNotes;

    property Prompt_tokens: Integer read FPrompt_tokens write FPrompt_tokens;
    property Completion_tokens: Integer read FCompletion_tokens write FCompletion_tokens;
    property Total_tokens: Integer read FTotal_tokens write FTotal_tokens;

    { Speech-to-Text: modelo multimodal que acepta audio (ej: gemini-2.0-flash) }
    property TranscriptionModel: string read FTranscriptionModel write FTranscriptionModel;
    { Instruccion enviada junto al audio para guiar la transcripcion }
    property TranscriptionPrompt: string read FTranscriptionPrompt write FTranscriptionPrompt;
  end;

Procedure Register;

implementation

Uses uMakerAi.Chat;

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiGeminiSpeechTool]);
end;

constructor TAiGeminiSpeechTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FApiKey := '@GEMINI_API_KEY';
  FUrl := 'https://generativelanguage.googleapis.com/v1beta/';
  FModel := 'gemini-2.5-flash-preview-tts';
  FVoice := 'Puck';
  FAudioProfile := TStringList.Create;
  FScene := TStringList.Create;
  FDirectorsNotes := TStringList.Create;
  FTranscriptionModel := 'gemini-2.0-flash';
  FTranscriptionPrompt := 'Transcribe this audio accurately.';
end;

destructor TAiGeminiSpeechTool.Destroy;
begin
  FAudioProfile.Free;
  FScene.Free;
  FDirectorsNotes.Free;
  inherited;
end;

function TAiGeminiSpeechTool.BuildSpeechConfigJson: TJSONObject;
var
  LPrebuilt, LVoiceConfig, LMultiConfig, LSpeakerConfig: TJSONObject;
  LSpeakerList: TJSONArray;
  SL: TStringList;
  I, EqPos: Integer;
  RawVal, SpeakerName, VoiceName: string;
begin
  Result := nil;
  if Trim(FVoice) = '' then
    Exit;

  SL := TStringList.Create;
  try
    SL.CommaText := FVoice;

    for I := SL.Count - 1 downto 0 do
      if Trim(SL[I]) = '' then
        SL.Delete(I);

    if SL.Count > 1 then
    begin
      Result := TJSONObject.Create;
      LMultiConfig := TJSONObject.Create;
      LSpeakerList := TJSONArray.Create;

      for I := 0 to SL.Count - 1 do
      begin
        RawVal := Trim(SL[I]);
        EqPos := Pos('=', RawVal);

        if EqPos > 0 then
        begin
          SpeakerName := Trim(Copy(RawVal, 1, EqPos - 1));
          VoiceName := Trim(Copy(RawVal, EqPos + 1, Length(RawVal)));
        end
        else
        begin
          SpeakerName := 'Speaker' + IntToStr(I + 1);
          VoiceName := RawVal;
        end;

        LSpeakerConfig := TJSONObject.Create;
        LSpeakerConfig.AddPair('speaker', SpeakerName);
        LPrebuilt := TJSONObject.Create.AddPair('voiceName', VoiceName);
        LVoiceConfig := TJSONObject.Create.AddPair('prebuiltVoiceConfig', LPrebuilt);
        LSpeakerConfig.AddPair('voiceConfig', LVoiceConfig);
        LSpeakerList.Add(LSpeakerConfig);
      end;
      LMultiConfig.AddPair('speakerVoiceConfigs', LSpeakerList);
      Result.AddPair('multiSpeakerVoiceConfig', LMultiConfig);
    end
    else if SL.Count = 1 then
    begin
      RawVal := Trim(SL[0]);
      EqPos := Pos('=', RawVal);
      if EqPos > 0 then
        VoiceName := Trim(Copy(RawVal, EqPos + 1, Length(RawVal)))
      else
        VoiceName := RawVal;

      Result := TJSONObject.Create;
      LPrebuilt := TJSONObject.Create.AddPair('voiceName', VoiceName);
      LVoiceConfig := TJSONObject.Create.AddPair('prebuiltVoiceConfig', LPrebuilt);
      Result.AddPair('voiceConfig', LVoiceConfig);
    end;
  finally
    SL.Free;
  end;
end;

{ --- L?GICA DE EJECUCI?N --- }

function TAiGeminiSpeechTool.InternalRunGeminiTTS(const AText: string; ResMsg: TAiChatMessage): string;
var
  HTTP: TNetHTTPClient;
  LUrl: string;
  LRequestJson, LGenConfig, LSpeechConfig, JUsage: TJSONObject;
  LRespModalities: TJSONArray;
  LBody: TStringStream;
  LResponse: IHTTPResponse;
  LResponseJson: TJSONObject;
  LBase64: string;
  LPCMStream, LWAVStream: TMemoryStream;
  LNewFile: TAiMediaFile;
  LMsg: TAiChatMessage;
begin
  Result := '';
  LMsg := TAiChatMessage(ResMsg);
  if not Assigned(LMsg) then
    Exit;
  LUrl := Format('%smodels/%s:generateContent?key=%s', [FUrl, FModel, GetApiKey]);

  HTTP := TNetHTTPClient.Create(nil);
  LRequestJson := TJSONObject.Create;
  LPCMStream := TMemoryStream.Create;
  try
    // 1. Prompt de Director
    LRequestJson.AddPair('contents', TJSONArray.Create.Add(TJSONObject.Create.AddPair('parts', TJSONArray.Create.Add(TJSONObject.Create.AddPair('text', BuildFullPrompt(AText))))));

    // 2. Configuraci?n de Generaci?n
    LGenConfig := TJSONObject.Create;

    LRespModalities := TJSONArray.Create;
    LRespModalities.Add('AUDIO');
    LGenConfig.AddPair('responseModalities', LRespModalities);

    LSpeechConfig := BuildSpeechConfigJson;
    if Assigned(LSpeechConfig) then
      LGenConfig.AddPair('speechConfig', LSpeechConfig);

    LRequestJson.AddPair('generationConfig', LGenConfig);

    // 3. Post
    LBody := TStringStream.Create(LRequestJson.ToJSON, TEncoding.UTF8);
    try
      HTTP.ContentType := 'application/json';
      ReportState(acsWriting, 'Gemini generando voz...');
      LResponse := HTTP.Post(LUrl, LBody, LPCMStream);
    finally
      LBody.Free;
    end;

    // 4. Respuesta y Conversi?n
    if LResponse.StatusCode = 200 then
    begin
      LResponseJson := TJSONObject.ParseJSONValue(LResponse.ContentAsString) as TJSONObject;
      if not Assigned(LResponseJson) then
        Exit;
      try
        // --- CAPTURA DE METADATOS DE USO (TOKENS) ---
        if LResponseJson.TryGetValue<TJSONObject>('usageMetadata', JUsage) then
        begin
          var LPt: Integer := 0;
          var LCt: Integer := 0;
          var LTt: Integer := 0;
          JUsage.TryGetValue<Integer>('promptTokenCount', LPt);
          JUsage.TryGetValue<Integer>('candidatesTokenCount', LCt);
          JUsage.TryGetValue<Integer>('totalTokenCount', LTt);
          LMsg.Prompt_tokens := LMsg.Prompt_tokens + LPt;
          LMsg.Completion_tokens := LMsg.Completion_tokens + LCt;
          LMsg.Total_tokens := LMsg.Total_tokens + LTt;
          FPrompt_tokens := FPrompt_tokens + LPt;
          FCompletion_tokens := FCompletion_tokens + LCt;
          FTotal_tokens := FTotal_tokens + LTt;
        end;

        // --- EXTRACCI?N DEL AUDIO ---
        LBase64 := LResponseJson.GetValue<string>('candidates[0].content.parts[0].inlineData.data', '');
        if not LBase64.IsEmpty then
        begin
          LPCMStream.Clear;
          LNewFile := TAiMediaFile.Create;
          // Cargamos el Base64 (PCM Raw)
          LNewFile.LoadFromBase64('temp.pcm', LBase64);

          // Convertimos a WAV usando tu utilidad PcmToWav
          if ConvertPCMStreamToWAVStream(LNewFile.Content, LWAVStream, 24000, 1, 16) then
          begin
            try
              LNewFile.Clear;
              LNewFile.LoadFromStream('gemini_audio.wav', LWAVStream);
              LMsg.MediaFiles.Add(LNewFile);

              Result := '[Audio Generado]';
              // Sincronizamos el Prompt del mensaje con el resultado
              LMsg.Prompt := Result;

              ReportDataEnd(ResMsg, 'assistant', Result);
            finally
              LWAVStream.Free;
            end;
          end;
        end;
      finally
        LResponseJson.Free;
      end;
    end
    else
    begin
      var
      LRes := LResponse.ContentAsString;
      ReportError('Gemini TTS Error: ' + LRes, nil);
    end;

  finally
    LRequestJson.Free;
    LPCMStream.Free;
    HTTP.Free;
  end;
end;

{ --- M?TODOS DE APOYO --- }

function TAiGeminiSpeechTool.BuildFullPrompt(const AText: string): string;
var
  SB: TStringBuilder;
begin
  SB := TStringBuilder.Create;
  try
    if FAudioProfile.Count > 0 then
      SB.AppendLine('# AUDIO PROFILE:').AppendLine(FAudioProfile.Text);
    if FScene.Count > 0 then
      SB.AppendLine('## THE SCENE:').AppendLine(FScene.Text);
    if FDirectorsNotes.Count > 0 then
      SB.AppendLine('### DIRECTOR''S NOTES:').AppendLine(FDirectorsNotes.Text);
    SB.AppendLine('#### TRANSCRIPT:').AppendLine(AText);
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function TAiGeminiSpeechTool.InternalRunGeminiTranscription(aMediaFile: TAiMediaFile): string;
var
  HTTP: TNetHTTPClient;
  LUrl: string;
  LRequestJson, LPartText, LPartAudio, LInlineData: TJSONObject;
  LContents, LParts: TJSONArray;
  LBody: TStringStream;
  LResponse: IHTTPResponse;
  LResponseJson: TJSONObject;
  LBase64, LMimeType: string;
begin
  Result := '';
  LUrl := Format('%smodels/%s:generateContent?key=%s', [FUrl, FTranscriptionModel, GetApiKey]);

  aMediaFile.Content.Position := 0;
  LBase64 := StreamToBase64(aMediaFile.Content);
  LMimeType := GetMimeTypeFromFileName(ExtractFileExt(aMediaFile.FileName));
  if LMimeType = '' then
    LMimeType := 'audio/wav';

  HTTP := TNetHTTPClient.Create(nil);
  LRequestJson := TJSONObject.Create;
  try
    LPartText := TJSONObject.Create;
    LPartText.AddPair('text', FTranscriptionPrompt);

    LInlineData := TJSONObject.Create;
    LInlineData.AddPair('mimeType', LMimeType);
    LInlineData.AddPair('data', LBase64);
    LPartAudio := TJSONObject.Create;
    LPartAudio.AddPair('inlineData', LInlineData);

    LParts := TJSONArray.Create;
    LParts.Add(LPartText);
    LParts.Add(LPartAudio);

    LContents := TJSONArray.Create;
    LContents.Add(TJSONObject.Create.AddPair('parts', LParts));
    LRequestJson.AddPair('contents', LContents);

    LBody := TStringStream.Create(LRequestJson.ToJSON, TEncoding.UTF8);
    try
      HTTP.ContentType := 'application/json';
      LResponse := HTTP.Post(LUrl, LBody);
    finally
      LBody.Free;
    end;

    if LResponse.StatusCode = 200 then
    begin
      LResponseJson := TJSONObject.ParseJSONValue(LResponse.ContentAsString) as TJSONObject;
      if Assigned(LResponseJson) then
      try
        Result := LResponseJson.GetValue<string>('candidates[0].content.parts[0].text', '');
      finally
        LResponseJson.Free;
      end;
    end
    else
      raise Exception.CreateFmt('Gemini Transcription Error %d: %s',
        [LResponse.StatusCode, LResponse.ContentAsString]);
  finally
    LRequestJson.Free;
    HTTP.Free;
  end;
end;

procedure TAiGeminiSpeechTool.ExecuteTranscription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage);

  procedure DoTranscription;
  var
    LText: string;
  begin
    try
      ReportState(acsReasoning, 'Transcribiendo audio con Gemini...');
      LText := InternalRunGeminiTranscription(aMediaFile);
      aMediaFile.Transcription := LText;
      aMediaFile.Procesado := True;
      ReportDataEnd(ResMsg, 'assistant', LText);
    except
      on E: Exception do
        ReportError('Error en transcripcion Gemini: ' + E.Message, E);
    end;
  end;

begin
  if IsAsync then
    DoTranscription
  else
    TTask.Run(
      procedure
      var
        LText: string;
      begin
        try
          ReportState(acsReasoning, 'Transcribiendo audio con Gemini...');
          LText := InternalRunGeminiTranscription(aMediaFile);
          aMediaFile.Transcription := LText;
          aMediaFile.Procesado := True;
          ReportDataEnd(ResMsg, 'assistant', LText);
        except
          on E: Exception do
            ReportError('Error en transcripcion Gemini: ' + E.Message, E);
        end;
      end);
end;

procedure TAiGeminiSpeechTool.ExecuteSpeechGeneration(const AText: string; ResMsg, AskMsg: TAiChatMessage);
begin
  // Si IsAsync=True ya estamos en un hilo background del chat: llamar directo
  // para evitar un TTask anidado que causaría dangling pointer sobre ResMsg.
  // Si IsAsync=False estamos en el hilo principal: lanzar task para no bloquearlo.
  if IsAsync then
    InternalRunGeminiTTS(AText, ResMsg)
  else
    TTask.Run(
      procedure
      begin
        InternalRunGeminiTTS(AText, ResMsg);
      end);
end;

function TAiGeminiSpeechTool.GetApiKey: string;
begin
  if (csDesigning in ComponentState) then
    Exit(FApiKey);
  if FApiKey.StartsWith('@') then
    Result := GetEnvironmentVariable(Copy(FApiKey, 2, MaxInt))
  else
    Result := FApiKey;
end;

procedure TAiGeminiSpeechTool.SetAudioProfile(const Value: TStrings);
begin
  FAudioProfile.Assign(Value);
end;

procedure TAiGeminiSpeechTool.SetDirectorsNotes(const Value: TStrings);
begin
  FDirectorsNotes.Assign(Value);
end;

procedure TAiGeminiSpeechTool.SetScene(const Value: TStrings);
begin
  FScene.Assign(Value);
end;

class function TAiGeminiSpeechTool.GenerateSpeech(const AApiKey, AText, AVice: string; AAudioProfile, AScene, ADirectorsNotes: TStrings): TAiMediaFile;
  var
    LInstance: TAiGeminiSpeechTool;
    LDummyMsg: TAiChatMessage;
  begin
    Result := nil;

    // 1. Creamos una instancia temporal de la propia clase
    LInstance := TAiGeminiSpeechTool.Create(nil);
    // 2. Creamos el contenedor de mensaje
    LDummyMsg := TAiChatMessage.Create('', 'assistant');

    try
      // 3. Configuramos la instancia
      LInstance.ApiKey := AApiKey;
      LInstance.Voice := AVice;

      // Asignamos los par?metros de direcci?n si se proveen
      if Assigned(AAudioProfile) then
        LInstance.AudioProfile.Assign(AAudioProfile);
      if Assigned(AScene) then
        LInstance.Scene.Assign(AScene);
      if Assigned(ADirectorsNotes) then
        LInstance.DirectorsNotes.Assign(ADirectorsNotes);

      // 4. Ejecutamos la l?gica interna (S?ncronamente)
      // Llamamos a InternalRunGeminiTTS que es donde est? la l?gica de red y tokens
      LInstance.InternalRunGeminiTTS(AText, LDummyMsg);

      // 5. Extraemos el archivo resultante
      if LDummyMsg.MediaFiles.Count > 0 then
      begin
        Result := LDummyMsg.MediaFiles[0];
        // Importante: Extraer para que no se libere al destruir LDummyMsg
        LDummyMsg.MediaFiles.Extract(Result);
      end;
    finally
      // Limpieza de los objetos temporales
      LDummyMsg.Free;
      LInstance.Free;
    end;
  end;

end.
