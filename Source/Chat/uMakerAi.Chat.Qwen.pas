unit uMakerAi.Chat.Qwen;

// MIT License
//
// Copyright (c) <year> <copyright holders>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// Nombre: Gustavo Enriquez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

// Driver Qwen (Alibaba Cloud Model Studio / DashScope) - API OpenAI-compatible
// Docs: https://www.alibabacloud.com/help/en/model-studio/compatibility-of-openai-with-dashscope
// Endpoint internacional (Singapur): https://dashscope-intl.aliyuncs.com/compatible-mode/v1/
// Otras regiones: cambiar URL (EE.UU. https://dashscope-us.aliyuncs.com/compatible-mode/v1/,
// China https://dashscope.aliyuncs.com/compatible-mode/v1/). La API key queda atada a la
// region donde se creo: con otra region el API responde 401.
//
// Particularidades verificadas en runtime (sep 2026):
// - El razonamiento viene ACTIVADO por defecto en los hibridos (qwen3.x, qwen-plus/flash/
//   turbo, qwen3-max, VL, omni). El driver lo controla SIEMPRE con enable_thinking:
//   cap_Reasoning -> true; sin el cap -> false (modo rapido, sin pagar tokens de razonar).
//   enable_thinking=false lo aceptan todos los modelos probados (los que no razonan lo
//   ignoran). El razonamiento vuelve en reasoning_content (lo captura la clase base,
//   con y sin streaming).
// - thinking_budget (tokens de razonamiento) se deriva de ThinkingLevel.
// - Modelos que solo razonan (qwq-*, *-thinking-*): no se les envia enable_thinking.
//   qwq-plus SOLO responde en streaming: sin stream devuelve vacio y sin error.
// - Modelos abiertos (qwen3-32b, qwen3.6-27b, ...) aceptan enable_thinking=true solo en
//   streaming (400 sin stream): en sincrono el driver envia false.
// - tools + stream funcionan (la restriccion de la documentacion no aplica a estos modelos).
// - Vision formato OpenAI (image_url) en qwen3.8-flash/max, qwen3.7-plus, qwen3-vl-*,
//   qwen3.8-omni-flash. Imagenes muy pequenas (2x2) se rechazan con 400.
// - Audio de entrada (omni, asr): el API exige data URI en input_audio.data
//   ('data:audio/wav;base64,...'); el base64 pelado que arma el serializador comun
//   responde 400 "URL does not appear to be valid". InitChatCompletions lo reescribe.
//
// Fase 2 (sep 2026), por gap de capacidades como el resto de drivers:
// - [cap_GenImage] -> API nativa multimodal-generation (sincrona, devuelve URLs):
//   qwen-image-3.0 [default], qwen-image-2.0(-pro), qwen-image-max, z-image-turbo,
//   wan2.7-image(-pro). Tamano en ImageParams.Params.Values['size'] ('1024*1024').
//   Edicion: las imagenes adjuntas al prompt (1 a 3) son la entrada; van como data
//   URI. qwen-image-edit(-plus/-max) [default: plus], y tambien qwen-image-2.0/3.0 y
//   wan2.7-image; z-image-turbo no edita. Sin 'size' se conserva la proporcion.
// - [cap_GenAudio] -> qwen3-tts-flash por la API nativa (devuelve URL de un WAV).
//   Voz en TtsParams.Voice (default Cherry), idioma en TtsParams.Language.
// - [cap_Audio] / cmTranscription -> qwen3-asr-flash por chat/completions con
//   input_audio. En cmTranscription el Prompt viaja como contexto (nombres propios,
//   jerga) y mejora la ortografia de lo transcrito.
// - Embeddings (uMakerAi.Embeddings.Qwen) y rerank (uMakerAi.Qwen.Rerank) van aparte.

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.StrUtils,
  System.RegularExpressions, System.NetEncoding,
  System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent,

{$IF CompilerVersion < 35}
  uJSONHelper,
{$ENDIF}
  uMakerAi.ParamsRegistry, uMakerAi.Chat, uMakerAi.Core, uMakerAI.chat.Messages;

Type

  TAiQwenChat = Class(TAiChat)
  Private
    Function IsThinkingOnly(Const AModel: String): Boolean;
    Function IsOpenWeight(Const AModel: String): Boolean;
    // Modelo para una tarea dedicada: el de la sesion si es de esa familia, si no el default
    Function ModelFor(Const AMarker, ADefault: String): String;
    // POST JSON sincrono (cliente propio: no depende de Asynchronous)
    Function PostJSON(Const AUrl: String; ABody: TJSonObject): TJSonObject;
    Function DownloadMedia(Const AUrl, AFileName: String): TAiMediaFile;
    // Endpoint de la API nativa de DashScope derivado de Url (compatible-mode/v1 -> api/v1)
    Function NativeUrl(Const APath: String): String;
    Procedure FixAudioDataUris(AMessages: TJSonArray);
  Protected
    Function InitChatCompletions: String; Override;
    // Cuerpo del request de imagen (generar o editar); separado para probarlo sin red
    Function BuildImageRequest(AskMsg: TAiChatMessage): TJSonObject;
    function InternalRunNativeImageGeneration(ResMsg, AskMsg: TAiChatMessage): String; Override;
    function InternalRunNativeSpeechGeneration(ResMsg, AskMsg: TAiChatMessage): String; Override;
    function InternalRunNativeTranscription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage): String; Override;
  Public
    Constructor Create(Sender: TComponent); Override;
    class function GetDriverName: string; Override;
    class procedure RegisterDefaultParams(Params: TStrings); Override;
    class function CreateInstance(Sender: TComponent): TAiChat; Override;
  Published
  End;

procedure Register;

implementation

Const
  GlAIUrl = 'https://dashscope-intl.aliyuncs.com/compatible-mode/v1/';
  GlDefaultModel = 'qwen3.8-flash';
  GlDefaultImageModel = 'qwen-image-3.0';
  GlDefaultEditModel = 'qwen-image-edit-plus'; // el mas rapido editando (~9 s)
  GlDefaultTtsModel = 'qwen3-tts-flash';
  GlDefaultAsrModel = 'qwen3-asr-flash';
  GlMediaTimeout = 180000; // una imagen tarda 10-40 s

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiQwenChat]);
end;

{ TAiQwenChat }

class function TAiQwenChat.GetDriverName: string;
Begin
  Result := 'Qwen';
End;

class procedure TAiQwenChat.RegisterDefaultParams(Params: TStrings);
Begin
  Params.Clear;
  Params.Add('ApiKey=@DASHSCOPE_API_KEY');
  Params.Add('Model=' + GlDefaultModel);
  Params.Add('Max_Tokens=8192');
  Params.Add('URL=' + GlAIUrl);
End;

class function TAiQwenChat.CreateInstance(Sender: TComponent): TAiChat;
Begin
  Result := TAiQwenChat.Create(Sender);
End;

constructor TAiQwenChat.Create(Sender: TComponent);
begin
  inherited;
  ApiKey := '@DASHSCOPE_API_KEY';
  Model := GlDefaultModel;
  Url := GlAIUrl;
end;

function TAiQwenChat.IsThinkingOnly(Const AModel: String): Boolean;
begin
  // Siempre razonan: no aceptan apagarlo, asi que no se les manda enable_thinking
  Result := StartsText('qwq', AModel) or ContainsText(AModel, '-thinking');
end;

function TAiQwenChat.IsOpenWeight(Const AModel: String): Boolean;
begin
  // Pesos abiertos: el tamano va en el nombre (qwen3-32b, qwen3.6-27b, qwen3-235b-a22b...)
  Result := TRegEx.IsMatch(AModel, '-\d+(\.\d+)?b(-|$)', [roIgnoreCase]);
end;

function TAiQwenChat.InitChatCompletions: String;
Var
  AJSONObject, jToolChoice, jStreamOpts: TJSonObject;
  JArr: TJSonArray;
  JStop: TJSonArray;
  Lista: TStringList;
  I: Integer;
  LAsincronico, LThinking: Boolean;
  Res, LModel: String;
  LTemperature: Double;
begin

  If User = '' then
    User := 'user';

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);

  If LModel = '' then
    LModel := GlDefaultModel;

  LAsincronico := Self.Asynchronous;
  FClient.Asynchronous := LAsincronico;

  AJSONObject := TJSonObject.Create;
  Lista := TStringList.Create;

  Try

    AJSONObject.AddPair('stream', TJSONBool.Create(LAsincronico));
    if LAsincronico then
    begin
      // Sin esto el stream no trae usage y los contadores de tokens quedan en 0
      jStreamOpts := TJSonObject.Create;
      jStreamOpts.AddPair('include_usage', TJSONBool.Create(True));
      AJSONObject.AddPair('stream_options', jStreamOpts);
    end;

    If Tool_Active and (Trim(GetTools(TToolFormat.tfOpenAi).Text) <> '') then
    Begin

{$IF CompilerVersion < 35}
      JArr := TJSONUtils.ParseAsArray(GetTools(TToolFormat.tfOpenAi).Text);
{$ELSE}
      JArr := TJSonArray(TJSonArray.ParseJSONValue(GetTools(TToolFormat.tfOpenAi).Text));
{$ENDIF}
      If Not Assigned(JArr) then
        Raise Exception.Create('La propiedad Tools estan mal definido, debe ser un JsonArray');
      AJSONObject.AddPair('tools', JArr);

      If (Trim(Tool_choice) <> '') then
      Begin

{$IF CompilerVersion < 35}
        jToolChoice := TJSONUtils.ParseAsObject(Tool_choice);
{$ELSE}
        jToolChoice := TJSonObject(TJSONObject.ParseJSONValue(Tool_choice));
{$ENDIF}
        If Assigned(jToolChoice) then
          AJSONObject.AddPair('tool_choice', jToolChoice);
      End;
    End;

    JArr := GetMessages;
    FixAudioDataUris(JArr);
    AJSONObject.AddPair('messages', JArr);

    AJSONObject.AddPair('model', LModel);

    // Razonamiento: el API lo trae ACTIVADO por defecto en los hibridos, asi que
    // se controla siempre (mismo criterio que GLM / DeepSeek V4): cap_Reasoning
    // -> true; sin el cap -> false (rapido y sin tokens de razonamiento).
    if not IsThinkingOnly(LModel) then
    begin
      LThinking := cap_Reasoning in ModelConfig.ModelCaps;
      // Los modelos abiertos solo razonan en streaming (400 en sincrono)
      if LThinking and IsOpenWeight(LModel) and not LAsincronico then
        LThinking := False;
      AJSONObject.AddPair('enable_thinking', TJSONBool.Create(LThinking));
      if LThinking then
        case ModelConfig.ThinkingLevel of
          tlLow:    AJSONObject.AddPair('thinking_budget', TJSONNumber.Create(1024));
          tlMedium: AJSONObject.AddPair('thinking_budget', TJSONNumber.Create(4096));
          tlHigh:   AJSONObject.AddPair('thinking_budget', TJSONNumber.Create(16384));
          // tlDefault: sin presupuesto, el largo maximo del modelo
        end;
    end;

    // DashScope acepta temperature en [0, 2)
    LTemperature := Trunc(Temperature * 100) / 100;
    if LTemperature >= 2 then
      LTemperature := 1.99;
    if LTemperature < 0 then
      LTemperature := 0;
    AJSONObject.AddPair('temperature', TJSONNumber.Create(LTemperature));

    AJSONObject.AddPair('max_tokens', TJSONNumber.Create(Max_tokens));

    If Top_p <> 0 then
      AJSONObject.AddPair('top_p', TJSONNumber.Create(Top_p));

    Lista.CommaText := Stop;
    If Lista.Count > 0 then
    Begin
      JStop := TJSonArray.Create;
      For I := 0 to Lista.Count - 1 do
        JStop.Add(Lista[I]);
      AJSONObject.AddPair('stop', JStop);
    End;

    Res := UTF8ToString(UTF8Encode(AJSONObject.ToJSON));

    Res := StringReplace(Res, '\/', '/', [rfReplaceAll]);
    Result := StringReplace(Res, '\r\n', '', [rfReplaceAll]);
  Finally
    AJSONObject.Free;
    Lista.Free;
  End;
end;

function TAiQwenChat.ModelFor(Const AMarker, ADefault: String): String;
begin
  Result := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);
  if not ContainsText(Result, AMarker) then
    Result := ADefault;
end;

function TAiQwenChat.NativeUrl(Const APath: String): String;
begin
  Result := Url;
  if ContainsText(Result, 'compatible-mode/v1') then
    Result := StringReplace(Result, 'compatible-mode/v1', 'api/v1', [rfIgnoreCase]);
  if not Result.EndsWith('/') then
    Result := Result + '/';
  Result := Result + APath;
end;

function TAiQwenChat.PostJSON(Const AUrl: String; ABody: TJSonObject): TJSonObject;
var
  Client: TNetHTTPClient;
  Body: TStringStream;
  Res: IHTTPResponse;
  V: TJSONValue;
begin
  Client := TNetHTTPClient.Create(nil);
  Body := TStringStream.Create(ABody.ToJSON, TEncoding.UTF8);
  try
{$IF CompilerVersion >= 34}
    Client.SynchronizeEvents := False;
{$ENDIF}
    Client.ResponseTimeout := GlMediaTimeout;
    Client.ContentType := 'application/json';
    Res := Client.Post(AUrl, Body, nil, [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)]);
    if Res.StatusCode <> 200 then
      raise Exception.CreateFmt('Qwen %d: %s', [Res.StatusCode, Res.ContentAsString(TEncoding.UTF8)]);
    V := TJSonObject.ParseJSONValue(Res.ContentAsString(TEncoding.UTF8));
    if not (V is TJSonObject) then
    begin
      V.Free;
      raise Exception.Create('Qwen: la respuesta no es un objeto JSON');
    end;
    Result := TJSonObject(V);
  finally
    Body.Free;
    Client.Free;
  end;
end;

function TAiQwenChat.DownloadMedia(Const AUrl, AFileName: String): TAiMediaFile;
var
  Client: TNetHTTPClient;
  St: TMemoryStream;
  Res: IHTTPResponse;
begin
  Client := TNetHTTPClient.Create(nil);
  St := TMemoryStream.Create;
  try
{$IF CompilerVersion >= 34}
    Client.SynchronizeEvents := False;
{$ENDIF}
    Client.ResponseTimeout := GlMediaTimeout;
    Res := Client.Get(AUrl, St);
    if Res.StatusCode <> 200 then
      raise Exception.CreateFmt('Qwen: no se pudo descargar %s (%d)', [AFileName, Res.StatusCode]);
    St.Position := 0;
    Result := TAiMediaFile.Create;
    try
      Result.LoadFromStream(AFileName, St);
    except
      Result.Free;
      raise;
    end;
  finally
    St.Free;
    Client.Free;
  end;
end;

procedure TAiQwenChat.FixAudioDataUris(AMessages: TJSonArray);
var
  VMsg, VPart: TJSONValue;
  jContent: TJSonArray;
  jAudio: TJSonObject;
  LData, LFormat: String;
begin
  if not Assigned(AMessages) then
    Exit;
  for VMsg in AMessages do
    if (VMsg is TJSonObject) and TJSonObject(VMsg).TryGetValue<TJSonArray>('content', jContent) then
      for VPart in jContent do
        if (VPart is TJSonObject) and TJSonObject(VPart).TryGetValue<TJSonObject>('input_audio', jAudio) and
          jAudio.TryGetValue<String>('data', LData) and not StartsText('data:', LData) and
          not StartsText('http', LData) then
        begin
          LFormat := jAudio.GetValue<String>('format', 'wav');
          if (LFormat = '') or SameText(LFormat, 'x-wav') then
            LFormat := 'wav';
          jAudio.RemovePair('data').Free;
          jAudio.AddPair('data', 'data:audio/' + LFormat + ';base64,' + LData);
        end;
end;

function TAiQwenChat.BuildImageRequest(AskMsg: TAiChatMessage): TJSonObject;
// Cuerpo de multimodal-generation para generar (solo texto) o editar (1 a 3
// imagenes adjuntas al prompt, que llegan intactas porque cap_Image no esta en el
// gap de un modelo de imagen)
var
  jInput, jMsg, jPart, jParams: TJSonObject;
  jMsgs, jContent: TJSonArray;
  LSize, LModel: String;
  LImages: TAiMediaFilesArray;
  MF: TAiMediaFile;
begin
  if Trim(AskMsg.Prompt) = '' then
    raise Exception.Create('Se requiere un prompt para generar la imagen.');
  LImages := AskMsg.MediaFiles.GetMediaList([Tfc_Image], False);
  if Length(LImages) > 3 then
    raise Exception.CreateFmt('Qwen: la edicion admite de 1 a 3 imagenes (llegaron %d).', [Length(LImages)]);
  if Length(LImages) > 0 then
  begin
    LModel := ModelFor('image', GlDefaultEditModel);
    if StartsText('z-image', LModel) then
      raise Exception.Create('Qwen: ' + LModel + ' solo genera; para editar usa qwen-image-edit-plus, ' +
        'qwen-image-2.0/3.0 o wan2.7-image.');
  end
  else
    LModel := ModelFor('image', GlDefaultImageModel);

  Result := TJSonObject.Create;
  try
    Result.AddPair('model', LModel);

    jContent := TJSonArray.Create;
    for MF in LImages do
    begin
      jPart := TJSonObject.Create;
      if (MF.Content.Size = 0) and (MF.UrlMedia <> '') then
        jPart.AddPair('image', MF.UrlMedia)
      else
        jPart.AddPair('image', 'data:' + MF.MimeType + ';base64,' + MF.Base64);
      jContent.Add(jPart);
    end;
    jPart := TJSonObject.Create;
    jPart.AddPair('text', AskMsg.Prompt);
    jContent.Add(jPart);
    jMsg := TJSonObject.Create;
    jMsg.AddPair('role', 'user');
    jMsg.AddPair('content', jContent);
    jMsgs := TJSonArray.Create;
    jMsgs.Add(jMsg);
    jInput := TJSonObject.Create;
    jInput.AddPair('messages', jMsgs);
    Result.AddPair('input', jInput);

    // Cada familia admite tamanos distintos (qwen-image-plus solo 1328*1328, 1664*928...).
    // Al editar sin tamano explicito el modelo conserva la proporcion del original
    // (1024x576 -> 1376x768); forzar 1024*1024 la deformaria
    LSize := ImageParams.Params.Values['size'];
    if (LSize = '') and (Length(LImages) = 0) then
      LSize := '1024*1024';
    LSize := StringReplace(LSize, 'x', '*', [rfIgnoreCase]);
    jParams := TJSonObject.Create;
    if LSize <> '' then
      jParams.AddPair('size', LSize);
    if N > 1 then
      jParams.AddPair('n', TJSONNumber.Create(N));
    if ImageParams.Params.Values['negative_prompt'] <> '' then
      jParams.AddPair('negative_prompt', ImageParams.Params.Values['negative_prompt']);
    jParams.AddPair('watermark', TJSONBool.Create(SameText(ImageParams.Params.Values['watermark'], 'true')));
    Result.AddPair('parameters', jParams);
  except
    Result.Free;
    raise;
  end;
end;

function TAiQwenChat.InternalRunNativeImageGeneration(ResMsg, AskMsg: TAiChatMessage): String;
// POST api/v1/services/aigc/multimodal-generation/generation (sincrono)
// -> output.choices[].message.content[] con {"image": url}
var
  jBody, jRes, jItem: TJSonObject;
  jChoices, jOut: TJSonArray;
  VChoice, VItem: TJSONValue;
  LUrl: String;
  LCount: Integer;
begin
  Result := '';
  FBusy := True;
  FLastError := '';
  FLastPrompt := AskMsg.Prompt;
  jBody := nil;
  jRes := nil;
  try
    DoStateChange(acsConnecting, 'Generando imagen...');
    jBody := BuildImageRequest(AskMsg);
    jRes := PostJSON(NativeUrl('services/aigc/multimodal-generation/generation'), jBody);

    LCount := 0;
    if jRes.TryGetValue<TJSonArray>('output.choices', jChoices) then
      for VChoice in jChoices do
        if (VChoice is TJSonObject) and TJSonObject(VChoice).TryGetValue<TJSonArray>('message.content', jOut) then
          for VItem in jOut do
            if VItem is TJSonObject then
            begin
              jItem := TJSonObject(VItem);
              if jItem.TryGetValue<String>('image', LUrl) then
              begin
                Inc(LCount);
                ResMsg.MediaFiles.Add(DownloadMedia(LUrl, Format('qwen_image_%d.png', [LCount])));
              end;
            end;
    if LCount = 0 then
      raise Exception.Create('Qwen: la respuesta no trae imagenes: ' + Copy(jRes.ToJSON, 1, 300));

    if ResMsg.Role = '' then
      ResMsg.Role := 'assistant';
    DoStateChange(acsFinished, 'Done');
    if Assigned(FOnReceiveDataEnd) then
      FOnReceiveDataEnd(Self, ResMsg, jRes, 'assistant', ResMsg.Prompt);
  finally
    jBody.Free;
    jRes.Free;
    FBusy := False;
  end;
end;

function TAiQwenChat.InternalRunNativeSpeechGeneration(ResMsg, AskMsg: TAiChatMessage): String;
// POST api/v1/services/aigc/multimodal-generation/generation con input {text, voice,
// language_type} -> output.audio.url (WAV, expira en 24 h)
const
  // TtsParams.Language admite el codigo ISO; el API quiere el nombre en ingles
  Codes: array[0..9] of string = ('es', 'en', 'zh', 'fr', 'de', 'it', 'pt', 'ja', 'ko', 'ru');
  Names: array[0..9] of string = ('Spanish', 'English', 'Chinese', 'French', 'German', 'Italian',
    'Portuguese', 'Japanese', 'Korean', 'Russian');
var
  jBody, jInput, jRes: TJSonObject;
  LVoice, LLang, LUrl, LData: String;
  I: Integer;
  MF: TAiMediaFile;
begin
  Result := '';
  if Trim(AskMsg.Prompt) = '' then
    raise Exception.Create('Se requiere un texto para generar el audio.');
  FBusy := True;
  FLastError := '';
  FLastPrompt := AskMsg.Prompt;
  jBody := TJSonObject.Create;
  jRes := nil;
  try
    DoStateChange(acsConnecting, 'Generando audio...');
    LVoice := TtsParams.Voice;
    if LVoice = '' then
      LVoice := 'Cherry';
    LLang := TtsParams.Language;
    for I := Low(Codes) to High(Codes) do
      if SameText(LLang, Codes[I]) then
        LLang := Names[I];
    if LLang = '' then
      LLang := 'Auto';

    jBody.AddPair('model', ModelFor('-tts', GlDefaultTtsModel));
    jInput := TJSonObject.Create;
    jInput.AddPair('text', AskMsg.Prompt);
    jInput.AddPair('voice', LVoice);
    jInput.AddPair('language_type', LLang);
    jBody.AddPair('input', jInput);

    jRes := PostJSON(NativeUrl('services/aigc/multimodal-generation/generation'), jBody);

    if jRes.TryGetValue<String>('output.audio.url', LUrl) and (LUrl <> '') then
      MF := DownloadMedia(LUrl, 'qwen_tts.wav')
    else if jRes.TryGetValue<String>('output.audio.data', LData) and (LData <> '') then
    begin
      MF := TAiMediaFile.Create;
      MF.LoadFromBase64('qwen_tts.wav', LData);
    end
    else
      raise Exception.Create('Qwen: la respuesta no trae audio: ' + Copy(jRes.ToJSON, 1, 300));
    ResMsg.MediaFiles.Add(MF);

    if ResMsg.Role = '' then
      ResMsg.Role := 'assistant';
    DoStateChange(acsFinished, 'Done');
    if Assigned(FOnReceiveDataEnd) then
      FOnReceiveDataEnd(Self, ResMsg, jRes, 'assistant', '');
  finally
    jBody.Free;
    jRes.Free;
    FBusy := False;
  end;
end;

function TAiQwenChat.InternalRunNativeTranscription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage): String;
// qwen3-asr-flash por chat/completions con input_audio (data URI). Dos usos:
// - cmTranscription: la transcripcion ES la respuesta (ParseJsonTranscript) y el
//   Prompt viaja como contexto para el reconocedor.
// - Puente de Fase 1 (un modelo de texto recibe audio): solo se llena
//   aMediaFile.Transcription; RunNew la inyecta en el prompt y la respuesta final
//   la da el modelo de chat.
var
  jBody, jMsg, jPart, jAudio, jAsr, jRes, jText, jUsage: TJSonObject;
  jMsgs, jContent: TJSonArray;
  LMime, LText: String;
  LIn, LOut, LTotal: Integer;
  LBridge: Boolean;
begin
  Result := '';
  if not Assigned(aMediaFile) or (aMediaFile.Content.Size = 0) then
    raise Exception.Create('Se necesita un archivo de audio con contenido para la transcripcion.');
  LBridge := ChatMode <> cmTranscription;
  jBody := TJSonObject.Create;
  jRes := nil;
  try
    DoStateChange(acsConnecting, 'Transcribiendo audio...');
    jBody.AddPair('model', ModelFor('-asr', GlDefaultAsrModel));
    jBody.AddPair('stream', TJSONBool.Create(False));
    jMsgs := TJSonArray.Create;
    jBody.AddPair('messages', jMsgs);

    if not LBridge and (Trim(AskMsg.Prompt) <> '') then
    begin
      jPart := TJSonObject.Create;
      jPart.AddPair('text', AskMsg.Prompt);
      jContent := TJSonArray.Create;
      jContent.Add(jPart);
      jMsg := TJSonObject.Create;
      jMsg.AddPair('role', 'system');
      jMsg.AddPair('content', jContent);
      jMsgs.Add(jMsg);
    end;

    LMime := aMediaFile.MimeType;
    if (LMime = '') or SameText(LMime, 'audio/x-wav') then
      LMime := 'audio/wav';
    jAudio := TJSonObject.Create;
    jAudio.AddPair('data', 'data:' + LMime + ';base64,' + aMediaFile.Base64);
    jPart := TJSonObject.Create;
    jPart.AddPair('type', 'input_audio');
    jPart.AddPair('input_audio', jAudio);
    jContent := TJSonArray.Create;
    jContent.Add(jPart);
    jMsg := TJSonObject.Create;
    jMsg.AddPair('role', 'user');
    jMsg.AddPair('content', jContent);
    jMsgs.Add(jMsg);

    jAsr := TJSonObject.Create;
    jAsr.AddPair('enable_itn', TJSONBool.Create(False));
    if TranscriptionParams.Language <> '' then
      jAsr.AddPair('language', TranscriptionParams.Language);
    jBody.AddPair('asr_options', jAsr);

    jRes := PostJSON(Url + 'chat/completions', jBody);
    LText := jRes.GetValue<String>('choices[0].message.content', '');
    LIn := jRes.GetValue<Integer>('usage.prompt_tokens', 0);
    LOut := jRes.GetValue<Integer>('usage.completion_tokens', 0);
    LTotal := jRes.GetValue<Integer>('usage.total_tokens', LIn + LOut);

    if LBridge then
    begin
      aMediaFile.Transcription := LText;
      aMediaFile.Procesado := True;
      Prompt_tokens := Prompt_tokens + LIn;
      Completion_tokens := Completion_tokens + LOut;
      Total_tokens := Total_tokens + LTotal;
    end
    else
    begin
      // Forma OpenAI de /audio/transcriptions, para reutilizar ParseJsonTranscript
      jText := TJSonObject.Create;
      try
        jText.AddPair('text', LText);
        jUsage := TJSonObject.Create;
        jUsage.AddPair('input_tokens', TJSONNumber.Create(LIn));
        jUsage.AddPair('output_tokens', TJSONNumber.Create(LOut));
        jUsage.AddPair('total_tokens', TJSONNumber.Create(LTotal));
        jText.AddPair('usage', jUsage);
        ParseJsonTranscript(jText, ResMsg, aMediaFile);
      finally
        jText.Free;
      end;
    end;
    Result := LText;
  finally
    jBody.Free;
    jRes.Free;
  end;
end;

initialization

TAiChatFactory.Instance.RegisterDriver(TAiQwenChat);

end.
