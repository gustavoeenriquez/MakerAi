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
// El servidor devuelve JSON completo (no SSE) aunque stream=true esté en el request.
// OnRequestCompletedEvent intercepta el buffer sin procesar y llama ParseChat.
//
// PDFs/documentos: ToJSon base deja vacío el caso Tfc_Pdf. Este driver inyecta
// el content part {type:"file", file:{filename, file_data}} en InitChatCompletions.
// Requiere cap_Pdf en ModelCaps y SessionCaps para los modelos que lo soportan.
//
// session_id: campo extra en el root del payload, igual en todos los turnos de
// la conversación. Se renueva automáticamente en cada NewChat.

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
    FSessionId: string;
    procedure ResetSessionId;
  protected
    Function InitChatCompletions: String; Override;
    Procedure ParseChat(jObj: TJSonObject; ResMsg: TAiChatMessage); Override;
    Function InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String; Override;
    Procedure OnRequestCompletedEvent(const Sender: TObject; const aResponse: IHTTPResponse); Override;
  public
    constructor Create(Sender: TComponent); override;
    Procedure NewChat; Override;
    class function GetDriverName: string; override;
    class procedure RegisterDefaultParams(Params: TStrings); override;
    class function CreateInstance(Sender: TComponent): TAiChat; override;
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

Procedure TAiMakerAiChat.OnRequestCompletedEvent(const Sender: TObject; const aResponse: IHTTPResponse);
var
  LJson: String;
  LParsed: TJSonValue;
  jObj: TJSonObject;
  TempMsg: TAiChatMessage;
  LChoices: TJSonArray;
  LFirstChoice, LMessage: TJSonObject;
  LContent: String;
begin
  // La base maneja errores HTTP (non-200) y libera el stream.
  inherited OnRequestCompletedEvent(Sender, aResponse);

  // Si hubo error, la base ya lo gestionó.
  if Assigned(aResponse) and ((aResponse.StatusCode < 200) or (aResponse.StatusCode > 299)) then
    Exit;

  // En modo síncrono el base class lee el body directamente vía Post() bloqueante.
  // Procesar FTmpResponseText aquí causaría double-fire de eventos (DoDataEnd vacío
  // antes de que el segundo Run() de function calling complete).
  if not Asynchronous then
  begin
    FTmpResponseText := '';
    Exit;
  end;

  // Detectar buffer sin procesar: el servidor envió JSON completo sin SSE (\n).
  LJson := Trim(FTmpResponseText);
  if (LJson = '') or (LJson[1] <> '{') then
    Exit;

  MKLog('COMPLETE-JSON', 'Procesando JSON no-SSE, len=' + IntToStr(Length(LJson)));
  FTmpResponseText := '';  // consumido

  LParsed := TJSonObject.ParseJSONValue(LJson);
  if not (LParsed is TJSonObject) then
  begin
    FreeAndNil(LParsed);
    MKLog('COMPLETE-JSON', 'ERROR: JSON inválido');
    Exit;
  end;

  jObj := TJSonObject(LParsed);
  try
    // Extraer texto para disparar OnReceiveData → AppendToken en la UI.
    LContent := '';
    if jObj.TryGetValue<TJSonArray>('choices', LChoices) and
       Assigned(LChoices) and (LChoices.Count > 0) then
    begin
      LFirstChoice := LChoices.Items[0] as TJSonObject;
      if Assigned(LFirstChoice) and LFirstChoice.TryGetValue<TJSonObject>('message', LMessage) then
        LMessage.TryGetValue<String>('content', LContent);
    end;

    TempMsg := TAiChatMessage.Create('', 'assistant');
    try
      // Primero OnReceiveData con el texto completo (AppendToken en la UI).
      if LContent <> '' then
        DoData(TempMsg, 'assistant', LContent, jObj);

      // ParseChat rellena TempMsg y dispara DoStateChange(acsFinished) + FOnReceiveDataEnd.
      ParseChat(jObj, TempMsg);

      // Agregar a historial solo si fue respuesta normal (Prompt relleno por ParseChat).
      if TempMsg.Prompt <> '' then
      begin
        TempMsg.Id := FMessages.Count + 1;
        FMessages.Add(TempMsg);
        TempMsg := nil;
      end;
    finally
      if Assigned(TempMsg) then
        TempMsg.Free;
    end;
  finally
    jObj.Free;
  end;
end;

initialization
  TAiChatFactory.Instance.RegisterDriver(TAiMakerAiChat);

end.
