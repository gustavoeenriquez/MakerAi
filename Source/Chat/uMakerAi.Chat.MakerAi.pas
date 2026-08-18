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
  System.Generics.Collections, System.NetEncoding,
  System.Net.URLClient, System.Net.HttpClient,
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
    FSessionId:          string;
    FOnProgress:         TProc<string, string, Integer>;
    FFileReaderSlug:     string;
    FFileReaderId:       Integer;
    FPendingMediaParts:  TList<TAiMediaFile>;
    procedure ResetSessionId;
    procedure HandleStreamDone;
    procedure ParseAndAccumulateMediaParts(AMediaParts: TJSONArray);
    procedure ExtractMkFileBlocks(ResMsg: TAiChatMessage);
    procedure ExtractImageUrlContent(AContentArr: TJSonArray; ResMsg: TAiChatMessage);
    procedure ClearPendingMediaParts;
  protected
    Function InitChatCompletions: String; Override;
    Procedure ParseChat(jObj: TJSonObject; ResMsg: TAiChatMessage); Override;
    Function InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String; Override;
    Procedure OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean); Override;
    Procedure OnRequestCompletedEvent(const Sender: TObject; const aResponse: IHTTPResponse); Override;
  public
    constructor Create(Sender: TComponent); override;
    destructor Destroy; override;
    Procedure NewChat; Override;
    class function GetDriverName: string; override;
    class procedure RegisterDefaultParams(Params: TStrings); override;
    class function CreateInstance(Sender: TComponent): TAiChat; override;
    // Notifica el progreso mientras el servidor procesa un PDF:
    //   step  = 'extracting' | 'embedding' | 'querying'
    //   aFile = nombre del archivo
    //   pct   = porcentaje de avance del paso (0-100)
    property OnProgress: TProc<string, string, Integer> read FOnProgress write FOnProgress;
    // RAG: slug o ID del knowledge base a conectar vía mk_tools.file_reader.
    // Si FileReaderSlug <> '' se usa el slug; si FileReaderId > 0 se usa el ID.
    property FileReaderSlug: string  read FFileReaderSlug write FFileReaderSlug;
    property FileReaderId:   Integer read FFileReaderId   write FFileReaderId;
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
  FPendingMediaParts := TList<TAiMediaFile>.Create;
  FFileReaderId := 0;
end;

destructor TAiMakerAiChat.Destroy;
begin
  ClearPendingMediaParts;
  FPendingMediaParts.Free;
  inherited;
end;

Procedure TAiMakerAiChat.NewChat;
begin
  inherited NewChat;
  ResetSessionId;
  ClearPendingMediaParts;
end;

procedure TAiMakerAiChat.ClearPendingMediaParts;
var
  I: Integer;
begin
  for I := 0 to FPendingMediaParts.Count - 1 do
    FPendingMediaParts[I].Free;
  FPendingMediaParts.Clear;
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
    // Eliminar campos OpenAI no soportados por el routing MKAI/Claude.
    // frequency_penalty, presence_penalty, n: Claude API los rechaza.
    // top_p: Claude rechaza si top_p y temperature están presentes simultáneamente.
    // user: MKAI no lo usa; response_format:text es redundante (default).
    LPair := LJson.RemovePair('frequency_penalty');
    if Assigned(LPair) then LPair.Free;
    LPair := LJson.RemovePair('presence_penalty');
    if Assigned(LPair) then LPair.Free;
    LPair := LJson.RemovePair('n');
    if Assigned(LPair) then LPair.Free;
    LPair := LJson.RemovePair('user');
    if Assigned(LPair) then LPair.Free;
    LPair := LJson.RemovePair('top_p');
    if Assigned(LPair) then LPair.Free;
    var LRfObj: TJSONObject;
    if LJson.TryGetValue<TJSONObject>('response_format', LRfObj) then
    begin
      var sRfType: string;
      if LRfObj.TryGetValue<string>('type', sRfType) and (sRfType = 'text') then
      begin
        LPair := LJson.RemovePair('response_format');
        if Assigned(LPair) then LPair.Free;
      end;
    end;

    // Construir mk_tools si alguna cap lo requiere o hay file_reader configurado.
    var LNeedsMkTools :=
      (cap_WebSearch      in SessionCaps) or
      (cap_CodeInterpreter in SessionCaps) or
      (cap_ComputerUse    in SessionCaps) or
      (cap_TextEditor     in SessionCaps) or
      (cap_Shell          in SessionCaps) or
      (cap_Memory         in SessionCaps) or
      (FFileReaderSlug <> '') or (FFileReaderId > 0);

    if LNeedsMkTools then
    begin
      LPair := LJson.RemovePair('mk_tools');
      if Assigned(LPair) then LPair.Free;
      // MKAI usa mk_tools para web search — el campo web_search_options (OpenAI nativo)
      // no es reconocido por el servidor y puede interferir con el routing.
      LPair := LJson.RemovePair('web_search_options');
      if Assigned(LPair) then LPair.Free;
      var LMkTools := TJSONObject.Create;
      if cap_WebSearch       in SessionCaps then LMkTools.AddPair('web_search',     TJSONBool.Create(True));
      if cap_CodeInterpreter in SessionCaps then LMkTools.AddPair('code_execution', TJSONBool.Create(True));
      if cap_ComputerUse     in SessionCaps then LMkTools.AddPair('computer_use',   TJSONBool.Create(True));
      if cap_TextEditor      in SessionCaps then LMkTools.AddPair('text_editor',    TJSONBool.Create(True));
      if cap_Shell           in SessionCaps then LMkTools.AddPair('bash',           TJSONBool.Create(True));
      if cap_Memory          in SessionCaps then LMkTools.AddPair('memory',         TJSONBool.Create(True));
      if FFileReaderSlug <> '' then
      begin
        var LFR := TJSONObject.Create;
        LFR.AddPair('encyclopedia_slug', FFileReaderSlug);
        LMkTools.AddPair('file_reader', LFR);
      end
      else if FFileReaderId > 0 then
      begin
        var LFR := TJSONObject.Create;
        LFR.AddPair('encyclopedia_id', TJSONNumber.Create(FFileReaderId));
        LMkTools.AddPair('file_reader', LFR);
      end;
      LJson.AddPair('mk_tools', LMkTools);
    end;

    // reasoning_effort: parámetro top-level para modelos con ThinkingLevel configurado.
    if ModelConfig.ThinkingLevel <> tlDefault then
    begin
      LPair := LJson.RemovePair('reasoning_effort');
      if Assigned(LPair) then LPair.Free;
      var sEffort: string;
      case ModelConfig.ThinkingLevel of
        tlLow:    sEffort := 'low';
        tlHigh:   sEffort := 'high';
      else        sEffort := 'medium'; // tlMedium y cualquier otro
      end;
      LJson.AddPair('reasoning_effort', sEffort);
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

      LChoiceVal := TJSonObject.ParseJSONValue(LEffective); // ParseJSONValue es método de clase de TJSONObject (TJSonValue no lo expone en D10.4)
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
  LContentArr: TJSonArray;
  LToolCalls: TJSonValue;
  LUsage: TJSonObject;
  LCompletionTokens: Integer;
  LSavedHandler: TAiChatOnDataEvent;
  LHasMkFile: Boolean;
  LHasImageArr: Boolean;
begin
  MKLog('RESPONSE', Copy(jObj.ToJSON, 1, 3000));

  LContent     := '';
  LContentArr  := nil;
  LHasMkFile   := False;
  LHasImageArr := False;

  if jObj.TryGetValue<TJSonArray>('choices', LChoices) and
     Assigned(LChoices) and (LChoices.Count > 0) then
  begin
    LFirstChoice := LChoices.Items[0] as TJSonObject;
    if Assigned(LFirstChoice) and
       LFirstChoice.TryGetValue<TJSonObject>('message', LMessage) and
       Assigned(LMessage) then
    begin
      if LMessage.TryGetValue<String>('content', LContent) then
        LHasMkFile := Pos('```mk_file:', LContent) > 0
      else if LMessage.TryGetValue<TJSonArray>('content', LContentArr) and
              Assigned(LContentArr) then
        LHasImageArr := True   // content es array (p.ej. image_url de mk-claude-sonnet)
      else
      begin
        // Diagnóstico: tokens consumidos pero sin content ni tool_calls.
        if not LMessage.TryGetValue<TJSonValue>('tool_calls', LToolCalls) then
        begin
          LCompletionTokens := 0;
          if jObj.TryGetValue<TJSonObject>('usage', LUsage) and Assigned(LUsage) then
            LUsage.TryGetValue<Integer>('completion_tokens', LCompletionTokens);
          if LCompletionTokens > 0 then
            MKLog('SERVER-WARN',
              'content="" + sin tool_calls + completion_tokens=' +
              IntToStr(LCompletionTokens) +
              ' — posible bug del servidor');
        end;
      end;
    end;
  end;

  if LHasMkFile or LHasImageArr then
  begin
    LSavedHandler    := OnReceiveDataEnd;
    OnReceiveDataEnd := nil;
    try
      inherited ParseChat(jObj, ResMsg);
      if LHasMkFile then
      begin
        MKLog('MK_FILE-DETECT', 'extrayendo bloques mk_file del prompt...');
        ExtractMkFileBlocks(ResMsg);
        MKLog('MK_FILE-DONE', IntToStr(ResMsg.MediaFiles.Count) + ' archivo(s) extraído(s)');
      end
      else
      begin
        MKLog('IMAGE_URL', 'extrayendo image_url del content array...');
        ExtractImageUrlContent(LContentArr, ResMsg);
        MKLog('IMAGE_URL-DONE', IntToStr(ResMsg.MediaFiles.Count) + ' archivo(s) extraído(s)');
      end;
    finally
      OnReceiveDataEnd := LSavedHandler;
    end;
    if Assigned(LSavedHandler) then
      LSavedHandler(Self, ResMsg, jObj, ResMsg.Role, ResMsg.Prompt);
  end
  else
  begin
    inherited ParseChat(jObj, ResMsg);
    // ISSUE #99 (regresión): en async la clase base delega el cierre del turno al handler
    // de [DONE] de OnInternalReceiveData. Pero este driver intercepta [DONE] él mismo
    // (-> HandleStreamDone) y se salta ese handler, así que para contenido plano nadie
    // dispara FOnReceiveDataEnd/acsFinished y el cliente cuelga (DEADLINE). Lo disparamos
    // aquí. Guard 'not FPendingToolRun': si la base dejó una continuación tool-calling
    // diferida (ISSUE #100), NO cerramos el turno a mitad del loop agéntico.
    if FClient.Asynchronous and (not FPendingToolRun) and Assigned(FOnReceiveDataEnd) then
    begin
      DoStateChange(acsFinished, 'Done');
      FOnReceiveDataEnd(Self, ResMsg, jObj, ResMsg.Role, ResMsg.Prompt);
    end;
  end;
end;

procedure TAiMakerAiChat.ExtractMkFileBlocks(ResMsg: TAiChatMessage);
const
  COpenTag  = '```mk_file:';
  CCloseTag = '```';
var
  LText, LResult, LHeader, LFilename, LMime, LB64: string;
  LPos, LFindPos, LHeaderEnd, LClosePos, LColonPos: Integer;
  LBytes: TBytes;
  LStream: TMemoryStream;
  LMF: TAiMediaFile;
begin
  LText := ResMsg.Prompt;
  if Pos(COpenTag, LText) = 0 then Exit;

  LResult := '';
  LPos    := 1;
  LFindPos := Pos(COpenTag, LText);
  while LFindPos > 0 do
  begin
    LResult := LResult + Copy(LText, LPos, LFindPos - LPos);

    LHeaderEnd := PosEx(#10, LText, LFindPos);
    if LHeaderEnd <= 0 then
    begin
      LResult := LResult + Copy(LText, LFindPos, MaxInt);
      LPos := Length(LText) + 1;
      Break;
    end;

    LHeader   := TrimRight(Copy(LText, LFindPos + Length(COpenTag),
                                LHeaderEnd - LFindPos - Length(COpenTag)));
    LColonPos := Pos(':', LHeader);
    if LColonPos > 0 then
    begin
      LFilename := Copy(LHeader, 1, LColonPos - 1);
      LMime     := Copy(LHeader, LColonPos + 1, MaxInt);
    end else
    begin
      LFilename := LHeader;
      LMime     := 'application/octet-stream';
    end;

    LClosePos := PosEx(CCloseTag, LText, LHeaderEnd + 1);
    if LClosePos <= 0 then
    begin
      // El servidor omitió la etiqueta de cierre — todo lo que queda desde
      // LHeaderEnd+1 hasta el final es el base64 del archivo.
      LB64      := Trim(Copy(LText, LHeaderEnd + 1, MaxInt));
      LPos      := Length(LText) + 1;  // para salir del while
      LClosePos := Length(LText) + 1;  // indicador de fin de string
    end
    else
      LB64 := Trim(Copy(LText, LHeaderEnd + 1, LClosePos - LHeaderEnd - 1));

    if LB64 <> '' then
    try
      LBytes := TNetEncoding.Base64.DecodeStringToBytes(LB64);
      if Length(LBytes) > 0 then
      begin
        LStream := TMemoryStream.Create;
        try
          LStream.Write(LBytes[0], Length(LBytes));
          LStream.Position := 0;
          LMF := TAiMediaFile.Create;
          LMF.LoadFromStream(LFilename, LStream);
          ResMsg.AddMediaFile(LMF);
          MKLog('MK_FILE', 'extracted ' + LFilename + ' mime=' + LMime +
            ' bytes=' + IntToStr(Length(LBytes)));
        finally
          LStream.Free;
        end;
      end;
    except
      on E: Exception do
        MKLog('MK_FILE-ERR', LFilename + ': ' + E.Message);
    end;

    if LPos > Length(LText) then
      Break;
    LPos     := LClosePos + Length(CCloseTag);
    LFindPos := PosEx(COpenTag, LText, LPos);
  end;
  if LPos <= Length(LText) then
    LResult := LResult + Copy(LText, LPos, MaxInt);

  ResMsg.Prompt := Trim(LResult);
end;

// Extrae archivos y texto de un content array OpenAI-style:
//   [{"type":"image_url","image_url":{"url":"data:mime;base64,XXX"}}, ...]
//   [{"type":"text","text":"..."}]
// mk-claude-sonnet devuelve las imágenes generadas en este formato.
procedure TAiMakerAiChat.ExtractImageUrlContent(AContentArr: TJSonArray;
  ResMsg: TAiChatMessage);
var
  LVal:   TJSonValue;
  LItem:  TJSonObject;
  LImgObj: TJSonObject;
  sType, sUrl, sMime, sB64, sExt, sFileName, sText: string;
  LColonPos, LB64Start: Integer;
  LBytes: TBytes;
  LStream: TMemoryStream;
  LMF: TAiMediaFile;
begin
  if not Assigned(AContentArr) then Exit;
  for LVal in AContentArr do
  begin
    if not (LVal is TJSonObject) then Continue;
    LItem := TJSonObject(LVal);
    if not LItem.TryGetValue<String>('type', sType) then Continue;

    if SameText(sType, 'text') then
    begin
      if LItem.TryGetValue<String>('text', sText) and (sText <> '') then
        ResMsg.Prompt := ResMsg.Prompt + sText;
    end
    else if SameText(sType, 'image_url') then
    begin
      if not LItem.TryGetValue<TJSonObject>('image_url', LImgObj) or
         not Assigned(LImgObj) then Continue;
      if not LImgObj.TryGetValue<String>('url', sUrl) then Continue;
      // sUrl = "data:image/png;base64,iVBOR..."
      if not sUrl.StartsWith('data:') then Continue;
      LColonPos := Pos(';base64,', sUrl);
      if LColonPos <= 0 then Continue;
      sMime     := Copy(sUrl, 6, LColonPos - 6);
      LB64Start := LColonPos + 8;
      sB64      := Copy(sUrl, LB64Start, MaxInt);
      // Derivar extensión desde el mime-type (image/png → png, image/jpeg → jpg)
      sExt := LowerCase(Copy(sMime, Pos('/', sMime) + 1, MaxInt));
      if sExt = 'jpeg' then sExt := 'jpg';
      if sExt = '' then sExt := 'bin';
      sFileName := 'image.' + sExt;
      try
        LBytes := TNetEncoding.Base64.DecodeStringToBytes(sB64);
        if Length(LBytes) > 0 then
        begin
          LStream := TMemoryStream.Create;
          try
            LStream.Write(LBytes[0], Length(LBytes));
            LStream.Position := 0;
            LMF := TAiMediaFile.Create;
            LMF.LoadFromStream(sFileName, LStream);
            ResMsg.AddMediaFile(LMF);
            MKLog('IMAGE_URL', 'extracted ' + sFileName + ' mime=' + sMime +
              ' bytes=' + IntToStr(Length(LBytes)));
          finally
            LStream.Free;
          end;
        end;
      except
        on E: Exception do
          MKLog('IMAGE_URL-ERR', sFileName + ': ' + E.Message);
      end;
    end;
  end;
  ResMsg.Prompt := Trim(ResMsg.Prompt);
end;

Function TAiMakerAiChat.InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String;
begin
  MKLog('SEND', 'URL=' + Url + ' Model=' + Model +
    ' Async=' + BoolToStr(Asynchronous, True) +
    ' Tools=' + BoolToStr(Tool_Active, True));
  Result := inherited InternalRunCompletions(ResMsg, AskMsg);
  MKLog('SEND-END', 'sync result len=' + IntToStr(Length(Result)));
end;

procedure TAiMakerAiChat.ParseAndAccumulateMediaParts(AMediaParts: TJSONArray);
var
  LVal:    TJSONValue;
  LPart:   TJSONObject;
  LInner:  TJSONObject;
  sType, sFilename, sMime, sData, sUrl: string;
  LBytes:  TBytes;
  LStream: TMemoryStream;
  LMF:     TAiMediaFile;
  IComma:  Integer;
begin
  for LVal in AMediaParts do
  begin
    LPart := LVal as TJSONObject;
    if not Assigned(LPart) then Continue;
    sType := ''; sFilename := ''; sMime := ''; sData := '';
    LPart.TryGetValue<string>('type', sType);

    if (sType = 'audio') or (sType = 'video') or (sType = 'file') then
    begin
      if not LPart.TryGetValue<TJSONObject>(sType, LInner) then Continue;
      LInner.TryGetValue<string>('filename',  sFilename);
      LInner.TryGetValue<string>('mime_type', sMime);
      LInner.TryGetValue<string>('data',      sData);
      // Variante solo-URL (lazy): si no trae base64 pero sí una URL http(s)
      if (sData = '') then
      begin
        sUrl := '';
        LInner.TryGetValue<string>('url', sUrl);
        if sUrl.StartsWith('http', True) then
        begin
          if sFilename = '' then
            sFilename := sType + '-' + IntToStr(FPendingMediaParts.Count + 1) +
                         GetFileExtensionFromMimeType(sMime);
          LMF := TAiMediaFile.Create;
          LMF.filename := sFilename;
          LMF.UrlMedia := sUrl;          // LAZY: descarga al 1er acceso a .Content
          FPendingMediaParts.Add(LMF);
          MKLog('MEDIA_PART', 'lazy url ' + sType + ' ' + sFilename + ' <- ' + sUrl);
          Continue;
        end;
      end;
    end
    else if sType = 'image_url' then
    begin
      if not LPart.TryGetValue<TJSONObject>('image_url', LInner) then Continue;
      LInner.TryGetValue<string>('url', sUrl);

      // Rama URL LAZY (Pieza S del server: media solo-URL -> http(s), no data:).
      // SetUrlMedia es puro lazy (Core:1169); baja al 1er acceso a .Content.
      if sUrl.StartsWith('http', True) then
      begin
        sMime := '';
        LInner.TryGetValue<string>('mime_type', sMime);   // opcional
        LMF := TAiMediaFile.Create;
        LMF.filename := 'ci_image-' + IntToStr(FPendingMediaParts.Count + 1) +
                        GetFileExtensionFromMimeType(sMime);
        LMF.UrlMedia := sUrl;
        FPendingMediaParts.Add(LMF);
        MKLog('MEDIA_PART', 'lazy url ' + LMF.filename + ' <- ' + sUrl);
        Continue;   // acumulada; saltar el decode base64 compartido de abajo
      end;

      // data:mime/type;base64,<data>  (rama existente)
      IComma := Pos(',', sUrl);
      if IComma <= 0 then Continue;
      sData := Copy(sUrl, IComma + 1, MaxInt);
      var IMime1 := Pos(':', sUrl);
      var IMime2 := Pos(';', sUrl);
      if (IMime1 > 0) and (IMime2 > IMime1) then
        sMime := Copy(sUrl, IMime1 + 1, IMime2 - IMime1 - 1);
      sFilename := 'ci_image' + GetFileExtensionFromMimeType(sMime);
    end
    else
      Continue;

    if sData = '' then Continue;
    LBytes := TNetEncoding.Base64.DecodeStringToBytes(sData);
    if Length(LBytes) = 0 then Continue;

    LStream := TMemoryStream.Create;
    try
      LStream.Write(LBytes[0], Length(LBytes));
      LStream.Position := 0;
      LMF := TAiMediaFile.Create;
      LMF.LoadFromStream(sFilename, LStream);
      FPendingMediaParts.Add(LMF);
      MKLog('MEDIA_PART', 'type=' + sType + ' file=' + sFilename +
        ' size=' + IntToStr(Length(LBytes)));
    finally
      LStream.Free;
    end;
  end;
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
  LToolCallsStr: string;
  LCombinedTools: TJSONArray;
  LSortedKeys: TList<Integer>;
begin
  LRole := FTmpRole;
  if LRole = '' then LRole := 'assistant';

  // El servidor stremea los tool_calls como delta SSE (NO fuerza un JSON completo cuando
  // hay tools). La clase base los acumuló en FTmpToolCallBuffer durante el stream. Como
  // este driver intercepta [DONE] y se salta el handler base que normalmente los reconstruye,
  // lo hacemos aquí para que ParseChat vea las tools y las ejecute; si no, async+tools queda
  // con respuesta vacía y, tras el cierre, sin continuación (la tool nunca se llama).
  LToolCallsStr := '';
  if FTmpToolCallBuffer.Count > 0 then
  begin
    LCombinedTools := TJSONArray.Create;
    try
      LSortedKeys := TList<Integer>.Create;
      try
        for var LKey in FTmpToolCallBuffer.Keys do
          LSortedKeys.Add(LKey);
        LSortedKeys.Sort;
        for var LKey in LSortedKeys do
          LCombinedTools.Add(FTmpToolCallBuffer[LKey].Clone as TJSONObject);
      finally
        LSortedKeys.Free;
      end;
      LToolCallsStr := LCombinedTools.ToJSON;
    finally
      LCombinedTools.Free;
      FTmpToolCallBuffer.Clear;
    end;
  end;

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
    if LToolCallsStr <> '' then
      LFakeMsg.AddPair('tool_calls', TJSONArray(TJSONObject.ParseJSONValue(LToolCallsStr)));

    LFakeChoice := TJSONObject.Create;
    LFakeChoice.AddPair('message', LFakeMsg);
    if LToolCallsStr <> '' then
      LFakeChoice.AddPair('finish_reason', 'tool_calls')
    else
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

      // Transferir archivos acumulados de media_parts (code_execution, etc.)
      // antes de ParseChat para que MediaFiles.Count sea correcto en OnDataEnd.
      while FPendingMediaParts.Count > 0 do
      begin
        var LMF := FPendingMediaParts[0];
        FPendingMediaParts.Delete(0);  // remueve sin liberar
        LHistMsg.AddMediaFile(LMF);    // mensaje toma ownership
      end;

      ParseChat(LFakeJson, LHistMsg);

      // Si LHistMsg quedó vacío, removerlo POR REFERENCIA: en la rama de tools, ParseChat
      // agregó el mensaje assistant-con-tool_calls y el tool-result DESPUÉS, así que el
      // último de la lista ya no es LHistMsg (un Delete(Count-1) borraría el tool-result).
      if (LHistMsg.Prompt = '') and (LHistMsg.MediaFiles.Count = 0) and (LHistMsg.Tool_calls = '') then
        FMessages.Remove(LHistMsg);
    finally
      if Assigned(TempMsg) then TempMsg.Free;
    end;

    FLastReasoning := '';
    // No liberar FBusy si quedó una continuación tool-calling diferida (#100): el turno sigue.
    if not FPendingToolRun then
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

  // ISSUE #124: si el chunk termina en un caracter UTF-8 incompleto, DataString
  // lanza EEncodingError; se sale sin hacer Clear y el proximo chunk lo completa.
  try
    LBuffer := FResponse.DataString;
  except
    on EEncodingError do
      Exit;
  end;
  FResponse.Clear;
  LDoneReceived := False;
  if LBuffer <> '' then
    MKLog('RAW-SSE', Copy(LBuffer, 1, 2000));

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

        // media_parts: archivos generados por code_execution u otros tools del servidor
        var LChoicesArr: TJSONArray;
        var LChoiceDelta: TJSONObject;
        var LMediaParts: TJSONArray;
        if LChunk.TryGetValue<TJSONArray>('choices', LChoicesArr) and
           (LChoicesArr.Count > 0) then
        begin
          var LChoice0 := LChoicesArr.Items[0] as TJSONObject;
          if Assigned(LChoice0) and
             LChoice0.TryGetValue<TJSONObject>('delta', LChoiceDelta) and
             LChoiceDelta.TryGetValue<TJSONArray>('media_parts', LMediaParts) then
            ParseAndAccumulateMediaParts(LMediaParts);
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
var
  LBody: string;
  LJObj: TJSONObject;
  LMsg: TAiChatMessage;
begin
  LBody := Trim(FTmpResponseText);
  FTmpResponseText := '';

  // Con tools en el request el servidor fuerza una respuesta JSON completa
  // (chat.completion) aunque se haya pedido stream=true. En ese caso el buffer
  // quedo con el JSON integro (sin lineas 'data:' ni [DONE]): procesarlo por
  // ParseChat, que ademas resuelve los tool_calls y dispara OnReceiveDataEnd.
  //
  // ISSUE #100: se procesa ANTES de inherited a proposito. Si ParseChat resuelve
  // tool_calls deja FPendingToolRun=True (continuacion diferida); el inherited base
  // (que libera el stream y lanza el siguiente round si FPendingToolRun) debe correr
  // DESPUES para que la continuacion del loop agentico no se pierda.
  if Asynchronous and Assigned(aResponse) and
     (aResponse.StatusCode >= 200) and (aResponse.StatusCode <= 299) and
     LBody.StartsWith('{') then
  begin
    LJObj := TJSONObject.ParseJSONValue(LBody) as TJSONObject;
    if Assigned(LJObj) then
    try
      if Assigned(LJObj.GetValue('choices')) then
      begin
        MKLog('NOSTREAM-JSON', Copy(LBody, 1, 500));
        LMsg := TAiChatMessage.Create('', 'assistant');
        LMsg.Id := FMessages.Count + 1;
        FMessages.Add(LMsg);
        ParseChat(LJObj, LMsg);
      end;
    finally
      LJObj.Free;
    end;
  end;

  // Delegate HTTP-error handling, stream cleanup y la continuacion tool-calling
  // diferida (FPendingToolRun) a la clase base. Corre al final (ver nota arriba).
  inherited OnRequestCompletedEvent(Sender, aResponse);
end;

initialization
  TAiChatFactory.Instance.RegisterDriver(TAiMakerAiChat);

end.
