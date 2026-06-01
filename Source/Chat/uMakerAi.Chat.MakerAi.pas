// MIT License
//
// Copyright (c) 2013 Gustavo Enríquez - CimaMaker
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
// Nombre: Gustavo Enríquez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com
// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

// Driver para MakerAI API (https://api.cimamaker.com/v1/).
// El servidor envía SSE real cuando stream=true (default en Asynchronous=True).
// OnInternalReceiveData filtra mk_progress chunks antes de que la base los procese.
//
// PDFs/documentos: ToJSon base deja vacío el caso Tfc_Pdf. Este driver inyecta
// el content part {type:"file", file:{filename, file_data}} en InitChatCompletions.
// Requiere cap_Pdf en ModelCaps y SessionCaps para los modelos que lo soportan.
//
// session_id: campo extra en el root del payload, igual en todos los turnos de
// la conversación. Se renueva automáticamente en cada NewChat.
//
// mk_progress: chunks SSE de progreso (extracting/embedding/querying) que el
// servidor emite mientras procesa PDFs. Tienen delta:{} — se filtran aquí y se
// notifican vía OnProgress sin pasarlos al acumulador de tokens de la base.

unit uMakerAi.Chat.MakerAi;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.StrUtils,
  System.Generics.Collections, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent, REST.Types, REST.Client,

{$IF CompilerVersion < 35}
  uJSONHelper,
{$ENDIF}

  uMakerAi.ParamsRegistry,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Core;

type
  TAiMakerAiChat = class(TAiChat)
  private
    FSessionId:   string;
    FOnProgress:  TProc<string, string, Integer>;
    procedure ResetSessionId;
    procedure HandleStreamDone;
  protected
    Function InitChatCompletions: String; Override;
    Procedure ParseChat(jObj: TJSonObject; ResMsg: TAiChatMessage); Override;
    Function InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String; Override;
    Procedure OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean); Override;
    Procedure OnRequestCompletedEvent(const Sender: TObject; const aResponse: IHTTPResponse); Override;
  public
    constructor Create(Sender: TComponent); override;
    Procedure NewChat; Override;
    class function GetDriverName: string; override;
    class procedure RegisterDefaultParams(Params: TStrings); override;
    class function CreateInstance(Sender: TComponent): TAiChat; override;
    // Notifica el progreso mientras el servidor procesa un PDF:
    //   step  = 'extracting' | 'embedding' | 'querying'
    //   aFile = nombre del archivo
    //   pct   = porcentaje de avance del paso (0-100)
    property OnProgress: TProc<string, string, Integer> read FOnProgress write FOnProgress;
  published
  end;

procedure Register;

implementation

const
  GlMakerAiUrl = 'https://api.cimamaker.com/v1/';

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiMakerAiChat]);
end;

procedure MKLog(const Tag, Msg: string);
begin
  LogDebug('[MakerAI ' + FormatDateTime('hh:nn:ss.zzz', Now) + '][' + Tag + '] ' + Msg);
end;

{ TAiMakerAiChat }

class function TAiMakerAiChat.GetDriverName: string;
begin
  Result := 'MakerAi';
end;

class procedure TAiMakerAiChat.RegisterDefaultParams(Params: TStrings);
begin
  Params.Clear;
  Params.Add('ApiKey=@MAKERAI_API_KEY');
  Params.Add('Model=mk-gpt-oss-20b');
  Params.Add('Max_Tokens=16000');
  Params.Add('Temperature=1');
  Params.Add('URL=' + GlMakerAiUrl);
  Params.Add('Tool_Active=True');
  Params.Add('Asynchronous=True');
  Params.Add('ModelCaps=[cap_Image]');
  Params.Add('SessionCaps=[cap_Image]');
end;

class function TAiMakerAiChat.CreateInstance(Sender: TComponent): TAiChat;
begin
  Result := TAiMakerAiChat.Create(Sender);
end;

procedure TAiMakerAiChat.ResetSessionId;
var
  LGuid: TGUID;
begin
  CreateGUID(LGuid);
  FSessionId := GUIDToString(LGuid);
end;

constructor TAiMakerAiChat.Create(Sender: TComponent);
begin
  inherited;
  Url := GlMakerAiUrl;  // sobreescribe el GlOpenAIUrl que pone el base en Create
  ResetSessionId;
end;

Procedure TAiMakerAiChat.NewChat;
begin
  inherited NewChat;
  ResetSessionId;
end;

Function TAiMakerAiChat.InitChatCompletions: String;
var
  LJson: TJSonObject;
  LTools: TJSonArray;
  LPair: TJSonPair;
  LEffective: string;
  LChoiceVal: TJSonValue;
begin
  Result := inherited InitChatCompletions;

  // Siempre parsear: se inyecta session_id en cada request; también mk_tools,
  // tool_choice y PDF file parts según las caps activas.
  LJson := TJSonObject.ParseJSONValue(Result) as TJSonObject;
  if Assigned(LJson) then
  try
    // Inject mk_tools so the MakerAI server activates Tavily web search.
    // Triggered by SessionCaps including cap_WebSearch; ModelCaps must NOT include
    // cap_WebSearch (to avoid the base class adding web_search_options for OpenAI).
    if cap_WebSearch in SessionCaps then
    begin
      LPair := LJson.RemovePair('mk_tools');
      if Assigned(LPair) then LPair.Free;
      var LMkTools := TJSONObject.Create;
      LMkTools.AddPair('web_search', TJSONBool.Create(True));
      LJson.AddPair('mk_tools', LMkTools);
    end;

    // Normalize tool_choice: base class serializes it as 'tools_choice' (typo) and
    // only handles JSON objects, not plain strings like "auto"/"required"/"none".
    if Tool_Active and
       LJson.TryGetValue<TJSonArray>('tools', LTools) and
       Assigned(LTools) and (LTools.Count > 0) then
    begin
      LPair := LJson.RemovePair('tool_choice');
      if Assigned(LPair) then LPair.Free;
      LPair := LJson.RemovePair('tools_choice');
      if Assigned(LPair) then LPair.Free;

      LEffective := Tool_choice;
      if LEffective = '' then LEffective := 'auto';

      LChoiceVal := TJSonValue.ParseJSONValue(LEffective);
      if Assigned(LChoiceVal) and (LChoiceVal is TJSonObject) then
        LJson.AddPair('tool_choice', LChoiceVal)
      else
      begin
        FreeAndNil(LChoiceVal);
        if (Length(LEffective) >= 2) and
           (LEffective[1] = '"') and (LEffective[Length(LEffective)] = '"') then
          LEffective := Copy(LEffective, 2, Length(LEffective) - 2);
        LJson.AddPair('tool_choice', LEffective);
      end;
    end;

    // Inject PDF file parts as {type:"file"} content parts.
    // ToJSon base deja el caso Tfc_Pdf vacío; lo completamos aquí.
    // FMessages[I] es 1:1 con messages[I] en el JSON (el system prompt vive en FMessages).
    if cap_Pdf in ModelCaps then
    begin
      var LMessages: TJSONArray;
      if LJson.TryGetValue<TJSONArray>('messages', LMessages) then
      begin
        for var LFIdx := 0 to FMessages.Count - 1 do
        begin
          if LFIdx >= LMessages.Count then Break;
          var LMsg      := FMessages[LFIdx];
          var LPdfFiles := LMsg.MediaFiles.GetMediaList([Tfc_Pdf], False);
          if Length(LPdfFiles) = 0 then Continue;

          var LJsonMsg := LMessages.Items[LFIdx] as TJSONObject;

          // content debería ser array (cap_Pdf en ModelCaps hace que ToJSon cree JContent
          // aunque el caso Tfc_Pdf no añada nada al array). Manejar string como fallback.
          var LContent: TJSONArray;
          var LContentVal := LJsonMsg.GetValue('content');
          if LContentVal is TJSONArray then
            LContent := TJSONArray(LContentVal)
          else
          begin
            LContent := TJSONArray.Create;
            var LTextPart := TJSONObject.Create;
            LTextPart.AddPair('type', 'text');
            LTextPart.AddPair('text', LMsg.Prompt);
            LContent.Add(LTextPart);
            LPair := LJsonMsg.RemovePair('content');
            if Assigned(LPair) then LPair.Free;
            LJsonMsg.AddPair('content', LContent);
          end;

          for var LK := 0 to High(LPdfFiles) do
          begin
            var LMedia    := LPdfFiles[LK];
            var LFileName := ExtractFileName(LMedia.Filename);
            if LFileName = '' then LFileName := 'document.pdf';

            var LFileInner := TJSONObject.Create;
            LFileInner.AddPair('filename',  LFileName);
            LFileInner.AddPair('file_data',
              'data:' + LMedia.MimeType + ';base64,' + LMedia.Base64);
            var LFilePart := TJSONObject.Create;
            LFilePart.AddPair('type', 'file');
            LFilePart.AddPair('file', LFileInner);
            LContent.Add(LFilePart);
          end;
        end;
      end;
    end;

    // session_id: identifica la sesión; el servidor recupera el contexto entre turnos.
    // Se renueva en NewChat para separar conversaciones distintas.
    LPair := LJson.RemovePair('session_id');
    if Assigned(LPair) then LPair.Free;
    LJson.AddPair('session_id', FSessionId);

    Result := LJson.ToJSON;
  finally
    LJson.Free;
  end;

  MKLog('REQUEST', Copy(Result, 1, 3000));
end;

Procedure TAiMakerAiChat.ParseChat(jObj: TJSonObject; ResMsg: TAiChatMessage);
var
  LChoices: TJSonArray;
  LFirstChoice: TJSonObject;
  LMessage: TJSonObject;
  LContent: String;
  LToolCalls: TJSonValue;
  LUsage: TJSonObject;
  LCompletionTokens: Integer;
begin
  MKLog('RESPONSE', Copy(jObj.ToJSON, 1, 3000));

  if not jObj.TryGetValue<TJSonArray>('choices', LChoices) or
     not Assigned(LChoices) or (LChoices.Count = 0) then
  begin
    MKLog('PARSE', 'choices ausente/vacío → skip');
    Exit;
  end;

  LFirstChoice := LChoices.Items[0] as TJSonObject;
  if not Assigned(LFirstChoice) or
     not LFirstChoice.TryGetValue<TJSonObject>('message', LMessage) or
     not Assigned(LMessage) then
  begin
    MKLog('PARSE', 'choices[0].message ausente → skip');
    Exit;
  end;

  // Diagnóstico de posible bug del servidor: completion_tokens > 0 pero
  // content vacío y sin tool_calls → el servidor consumió tokens sin devolver resultado.
  LMessage.TryGetValue<String>('content', LContent);
  if LContent.IsEmpty and
     not LMessage.TryGetValue<TJSonValue>('tool_calls', LToolCalls) then
  begin
    LCompletionTokens := 0;
    if jObj.TryGetValue<TJSonObject>('usage', LUsage) and Assigned(LUsage) then
      LUsage.TryGetValue<Integer>('completion_tokens', LCompletionTokens);
    if LCompletionTokens > 0 then
      MKLog('SERVER-WARN',
        'content="" + sin tool_calls + completion_tokens=' + IntToStr(LCompletionTokens) +
        ' — posible bug del servidor: function call generada pero no retornada');
  end;

  inherited ParseChat(jObj, ResMsg);
end;

Function TAiMakerAiChat.InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String;
begin
  MKLog('SEND', 'URL=' + Url + ' Model=' + Model +
    ' Async=' + BoolToStr(Asynchronous, True) +
    ' Tools=' + BoolToStr(Tool_Active, True));
  Result := inherited InternalRunCompletions(ResMsg, AskMsg);
  MKLog('SEND-END', 'sync result len=' + IntToStr(Length(Result)));
end;

procedure TAiMakerAiChat.HandleStreamDone;
// Called when [DONE] is received from the server.
// Builds a fake OpenAI-style response JSON from the accumulated FLastContent and calls
// ParseChat, which fires DoStateChange(acsFinished) + FOnReceiveDataEnd exactly once.
//
// IMPORTANT: TempMsg is added to FMessages BEFORE ParseChat fires FOnReceiveDataEnd.
// ParseChat calls events directly (no TThread.Queue), so the main thread could wake up
// immediately and start Turn 2. Without the pre-add, InitChatCompletions would miss
// the assistant reply (race condition in multi-turn async).
var
  LFakeJson, LFakeChoice, LFakeMsg, LFakeUsage: TJSONObject;
  LFakeChoices: TJSONArray;
  LRole: string;
  TempMsg: TAiChatMessage;
begin
  LRole := FTmpRole;
  if LRole = '' then LRole := 'assistant';

  LFakeJson := TJSONObject.Create;
  try
    LFakeJson.AddPair('id', 'stream-' + IntToStr(TThread.GetTickCount));
    LFakeJson.AddPair('model', Model);

    LFakeUsage := TJSONObject.Create;
    LFakeUsage.AddPair('prompt_tokens',     TJSONNumber.Create(0));
    LFakeUsage.AddPair('completion_tokens', TJSONNumber.Create(0));
    LFakeUsage.AddPair('total_tokens',      TJSONNumber.Create(0));
    LFakeJson.AddPair('usage', LFakeUsage);

    LFakeMsg := TJSONObject.Create;
    LFakeMsg.AddPair('role', LRole);
    if FLastContent <> '' then
      LFakeMsg.AddPair('content', FLastContent);

    LFakeChoice := TJSONObject.Create;
    LFakeChoice.AddPair('message',       LFakeMsg);
    LFakeChoice.AddPair('finish_reason', 'stop');

    LFakeChoices := TJSONArray.Create;
    LFakeChoices.Add(LFakeChoice);
    LFakeJson.AddPair('choices', LFakeChoices);

    TempMsg := TAiChatMessage.Create('', LRole);
    try
      TempMsg.Id := FMessages.Count + 1;
      FMessages.Add(TempMsg);
      var LHistMsg := TempMsg;
      TempMsg := nil;  // FMessages es owner

      ParseChat(LFakeJson, LHistMsg);

      if LHistMsg.Prompt = '' then
        FMessages.Delete(FMessages.Count - 1);
    finally
      if Assigned(TempMsg) then TempMsg.Free;
    end;

    FLastReasoning := '';
    FBusy := False;
  finally
    LFakeJson.Free;
  end;
end;

Procedure TAiMakerAiChat.OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean);
var
  LBuffer, LFullLine, LJsonStr: string;
  LChunk, LProgress: TJSONObject;
  LStep, LFile: string;
  LPct: Integer;
  P: Integer;
  LDoneReceived: Boolean;
begin
  // In sync mode there's nothing to intercept; let the base class exit early.
  if not Asynchronous then
  begin
    inherited;
    Exit;
  end;

  LBuffer := FResponse.DataString;
  FResponse.Clear;
  LDoneReceived := False;

  while Pos(#10, LBuffer) > 0 do
  begin
    P := Pos(#10, LBuffer);
    LFullLine := Copy(LBuffer, 1, P - 1);
    Delete(LBuffer, 1, P);

    LJsonStr := Trim(LFullLine);
    if LJsonStr.StartsWith('data:') then
      LJsonStr := Trim(Copy(LJsonStr, 6, Length(LJsonStr)));

    // [DONE]: intercept here — we fire events ourselves in HandleStreamDone to avoid
    // double-firing (ParseChat inside the base [DONE] handler fires them too) and to
    // fix the race condition (we add to FMessages before ParseChat fires FOnReceiveDataEnd).
    if LJsonStr = '[DONE]' then
    begin
      LDoneReceived := True;
      Continue;
    end;

    // mk_progress: server progress notification during PDF processing — fire OnProgress
    // and skip; do not pass empty delta:{} to the token accumulator.
    if (Length(LJsonStr) >= 2) and (LJsonStr[1] = '{') then
    begin
      LChunk := TJSONObject.ParseJSONValue(LJsonStr) as TJSONObject;
      if Assigned(LChunk) then
      try
        if LChunk.TryGetValue<TJSONObject>('mk_progress', LProgress) then
        begin
          LStep := ''; LFile := ''; LPct := 0;
          LProgress.TryGetValue<string>('step', LStep);
          LProgress.TryGetValue<string>('file', LFile);
          LProgress.TryGetValue<Integer>('pct', LPct);
          MKLog('PROGRESS', 'step=' + LStep + ' file=' + LFile + ' pct=' + IntToStr(LPct));
          if Assigned(FOnProgress) then
          begin
            var CStep := LStep;
            var CFile := LFile;
            var CPct  := LPct;
            var CHandler := FOnProgress;
            TThread.Queue(nil, procedure
            begin
              CHandler(CStep, CFile, CPct);
            end);
          end;
          Continue;
        end;
      finally
        LChunk.Free;
      end;
    end;

    FTmpResponseText := FTmpResponseText + LFullLine + #10;
  end;

  // Partial last line without #10 — keep in buffer (may complete in next chunk).
  if LBuffer <> '' then
    FTmpResponseText := FTmpResponseText + LBuffer;

  // FResponse is empty; inherited reads '' (no change to FTmpResponseText), then
  // processes our pre-filtered lines — fires DoData for each delta token.
  inherited OnInternalReceiveData(Sender, AContentLength, AReadCount, AAbort);

  // Handle [DONE] after inherited so FLastContent is fully updated from this chunk.
  if LDoneReceived and not AAbort then
    HandleStreamDone;
end;

Procedure TAiMakerAiChat.OnRequestCompletedEvent(const Sender: TObject; const aResponse: IHTTPResponse);
begin
  // Delegate HTTP-error handling and stream cleanup to the base class.
  inherited OnRequestCompletedEvent(Sender, aResponse);

  // Async (stream=true): OnInternalReceiveData processed all SSE chunks;
  // the [DONE] sentinel already triggered ParseChat via the base class.
  // Sync (stream=false): InternalRunCompletions called ParseChat directly.
  // Either way there is nothing left to do — just ensure the buffer is clear.
  FTmpResponseText := '';
end;

initialization
  TAiChatFactory.Instance.RegisterDriver(TAiMakerAiChat);

end.
