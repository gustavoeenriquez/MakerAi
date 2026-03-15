unit uMakerAi.Chat.DeepSeek;

// IT License
//
// Copyright (c) <year> <copyright holders>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// o use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// HE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
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

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Threading,
  System.Variants, System.Net.Mime, System.IOUtils, System.Generics.Collections,
  System.NetEncoding,
  System.JSON, System.StrUtils, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent,
  REST.JSON, REST.Types, REST.Client,

{$IF CompilerVersion < 35}
  uJSONHelper,
{$ENDIF}
  uMakerAi.ParamsRegistry, uMakerAi.Chat, uMakerAi.Embeddings, uMakerAi.Core, uMakerAI.chat.Messages;

Type

  TAiDeepSeekChat = Class(TAiChat)
  Private
    FTmpReasoning: String; // Acumula reasoning_content durante streaming SSE (deepseek-reasoner)
  Protected
    Function InitChatCompletions: String; Override;
    Function InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String; Override;
    Function GetMessages: TJSonArray; Override;
    procedure OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean); Override;
  Public
    Constructor Create(Sender: TComponent); Override;
    Destructor Destroy; Override;
    class function GetDriverName: string; Override;
    class procedure RegisterDefaultParams(Params: TStrings); Override;
    class function CreateInstance(Sender: TComponent): TAiChat; Override;
  Published
  End;

procedure Register;

implementation

Const
  GlAIUrl = 'https://api.deepseek.com/v1/';

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiDeepSeekChat]);
end;

{ TAiDeepSeekChat }

class function TAiDeepSeekChat.GetDriverName: string;
Begin
  Result := 'DeepSeek';
End;

class procedure TAiDeepSeekChat.RegisterDefaultParams(Params: TStrings);
Begin
  Params.Clear;
  Params.Add('ApiKey=@DEEPSEEK_API_KEY');
  Params.Add('Model=deepseek-chat');
  Params.Add('MaxTokens=4096');
  Params.Add('URL=https://api.deepseek.com/v1/');
End;

class function TAiDeepSeekChat.CreateInstance(Sender: TComponent): TAiChat;
Begin
  Result := TAiDeepSeekChat.Create(Sender);
End;

constructor TAiDeepSeekChat.Create(Sender: TComponent);
begin
  inherited;
  ApiKey := '@DEEPSEEK_API_KEY';

  Model := 'deepseek-chat';
  Url := GlAIUrl;
end;

destructor TAiDeepSeekChat.Destroy;
begin

  inherited;
end;

function TAiDeepSeekChat.InitChatCompletions: String;
Var
  AJSONObject, jToolChoice: TJSonObject;
  JArr: TJSonArray;
  JStop: TJSonArray;
  Lista: TStringList;
  I: Integer;
  LAsincronico: Boolean;
  Res, LModel: String;
begin

  If User = '' then
    User := 'user';

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);

  If LModel = '' then
    LModel := 'deepseek-chat';

  LAsincronico := Self.Asynchronous;
  FClient.Asynchronous := LAsincronico;

  AJSONObject := TJSonObject.Create;
  Lista := TStringList.Create;

  Try

    AJSONObject.AddPair('stream', TJSONBool.Create(LAsincronico));

    If Tool_Active and (Trim(GetTools(TToolFormat.tfOpenAi).Text) <> '') then
    Begin

{$IF CompilerVersion < 35}
      JArr := TJSONUtils.ParseAsArray(GetTools(TToolFormat.tfOpenAi).Text);
{$ELSE}
      JArr := TJSonArray(TJSonArray.ParseJSONValue(GetTools(TToolFormat.tfOpenAi).Text));
{$ENDIF}
      If Not Assigned(JArr) then
        Raise Exception.Create('La propiedad Tools están mal definido, debe ser un JsonArray');
      AJSONObject.AddPair('tools', JArr);

      If (Trim(Tool_choice) <> '') then
      Begin

{$IF CompilerVersion < 35}
        jToolChoice := TJSONUtils.ParseAsObject(Tool_choice);
{$ELSE}
        jToolChoice := TJSonObject(TJSONObject.ParseJSONValue(Tool_choice));
{$ENDIF}
        If Assigned(jToolChoice) then
          AJSONObject.AddPair('tools_choice', jToolChoice);
      End;
    End;

    AJSONObject.AddPair('messages', GetMessages);

    AJSONObject.AddPair('model', LModel);

    AJSONObject.AddPair('temperature', TJSONNumber.Create(Trunc(Temperature * 100) / 100));
    AJSONObject.AddPair('max_tokens', TJSONNumber.Create(Max_tokens));

    If Top_p <> 0 then
      AJSONObject.AddPair('top_p', TJSONNumber.Create(Top_p));

    AJSONObject.AddPair('frequency_penalty', TJSONNumber.Create(Trunc(Frequency_penalty * 100) / 100));
    AJSONObject.AddPair('presence_penalty', TJSONNumber.Create(Trunc(Presence_penalty * 100) / 100));
    AJSONObject.AddPair('user', User);
    AJSONObject.AddPair('n', TJSONNumber.Create(N));

    Lista.CommaText := Stop;
    If Lista.Count > 0 then
    Begin
      JStop := TJSonArray.Create;
      For I := 0 to Lista.Count - 1 do
        JStop.Add(Lista[I]);
      AJSONObject.AddPair('stop', JStop);
    End;

    If Logprobs = True then
    Begin
      If Logit_bias <> '' then
        AJSONObject.AddPair('logit_bias', TJSONNumber.Create(Logit_bias));

      AJSONObject.AddPair('logprobs', TJSONBool.Create(Logprobs));

      If Top_logprobs <> '' then
        AJSONObject.AddPair('top_logprobs', TJSONNumber.Create(Top_logprobs));
    End;

    If Seed > 0 then
      AJSONObject.AddPair('seed', TJSONNumber.Create(Seed));

    Res := UTF8ToString(UTF8Encode(AJSONObject.ToJSON));

    Res := StringReplace(Res, '\/', '/', [rfReplaceAll]);
    Result := StringReplace(Res, '\r\n', '', [rfReplaceAll]);
  Finally
    AJSONObject.Free;
    Lista.Free;
  End;
end;

function TAiDeepSeekChat.InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String;
begin
  FTmpReasoning := ''; // Resetear antes de cada nueva petición
  Result := inherited InternalRunCompletions(ResMsg, AskMsg);
end;

function TAiDeepSeekChat.GetMessages: TJSonArray;
var
  I: Integer;
  JObj: TJSonObject;
  Msg: TAiChatMessage;
begin
  Result := inherited GetMessages;
  // Para deepseek-reasoner: el mensaje del asistente con tool_calls DEBE incluir
  // reasoning_content en el historial. TAiChatMessages.ToJSon no lo serializa por defecto.
  if Self.Messages.Count <> Result.Count then Exit;
  for I := 0 to Self.Messages.Count - 1 do
  begin
    Msg := Self.Messages.Items[I];
    if (Msg.ReasoningContent <> '') and (Result.Items[I] is TJSonObject) then
    begin
      JObj := TJSonObject(Result.Items[I]);
      if JObj.GetValue('reasoning_content') = nil then
        JObj.AddPair('reasoning_content', Msg.ReasoningContent);
    end;
  end;
end;

procedure TAiDeepSeekChat.OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean);
Var
  // Variables compartidas entre el cuerpo principal y ProcessLine (closure)
  jObj, Delta: TJSonObject;
  JToolCalls: TJSonArray;
  sJson, Value, Role1: String;
  P: Integer;
  jFunc: TJSonValue;
  ToolIndex: Integer;
  BufferObj, BufferFunc: TJSonObject;
  CombinedTools: TJSonArray;
  sToolCallsStr: String;
  SortedKeys: TList<Integer>;
  FakeResponseObj, FakeChoice, FakeMsg: TJSonObject;
  FakeChoicesArr: TJSonArray;
  FakeUsage: TJSonObject;
  TempMsg: TAiChatMessage;
  jArrChoices: TJSonArray;

  // Copia exacta del ProcessLine del base class (TAiChat.OnInternalReceiveData).
  // Maneja SSE delta y [DONE] con reconstrucción de tool_calls.
  Procedure ProcessLine(ALine: String);
  Begin
    if ALine = '' then Exit;

    if StartsStr('data:', ALine) then
      ALine := Trim(Copy(ALine, 6, Length(ALine)));
    if ALine = '' then Exit;

    // -------------------------------------------------------------------
    // CASO 1: [DONE] — fin de stream SSE
    // -------------------------------------------------------------------
    if ALine = '[DONE]' then
    Begin
      sToolCallsStr := '';
      if FTmpToolCallBuffer.Count > 0 then
      begin
        CombinedTools := TJSonArray.Create;
        try
          SortedKeys := TList<Integer>.Create;
          try
            Var aKey: Integer;
            for aKey in FTmpToolCallBuffer.Keys do
              SortedKeys.Add(aKey);
            SortedKeys.Sort;
            for aKey in SortedKeys do
              CombinedTools.Add(FTmpToolCallBuffer[aKey].Clone as TJSonObject);
          finally
            SortedKeys.Free;
          end;
          sToolCallsStr := CombinedTools.ToJSon;
        finally
          CombinedTools.Free;
          FTmpToolCallBuffer.Clear;
        end;
      end;

      FakeResponseObj := TJSonObject.Create;
      try
        FakeResponseObj.AddPair('id', 'stream-' + IntToStr(TThread.GetTickCount));
        FakeResponseObj.AddPair('model', Model);
        FakeUsage := TJSonObject.Create;
        FakeUsage.AddPair('prompt_tokens', TJSONNumber.Create(0));
        FakeUsage.AddPair('completion_tokens', TJSONNumber.Create(0));
        FakeUsage.AddPair('total_tokens', TJSONNumber.Create(0));
        FakeResponseObj.AddPair('usage', FakeUsage);
        FakeChoicesArr := TJSonArray.Create;
        FakeChoice := TJSonObject.Create;
        FakeMsg := TJSonObject.Create;
        FakeMsg.AddPair('role', FTmpRole);
        if FLastContent <> '' then
          FakeMsg.AddPair('content', FLastContent);
        if FTmpReasoning <> '' then
          FakeMsg.AddPair('reasoning_content', FTmpReasoning); // Requerido por deepseek-reasoner en tool_calls
        if sToolCallsStr <> '' then
          FakeMsg.AddPair('tool_calls', TJSonArray(TJSonObject.ParseJSONValue(sToolCallsStr)));
        FakeChoice.AddPair('message', FakeMsg);
        FakeChoice.AddPair('finish_reason', 'stop');
        FakeChoicesArr.Add(FakeChoice);
        FakeResponseObj.AddPair('choices', FakeChoicesArr);
        TempMsg := TAiChatMessage.Create('', FTmpRole);
        try
          // Si hay tool_calls, forzamos Asynchronous=True antes de ParseChat.
          // Esto evita que cambios externos (ParamsChanged, etc.) hayan
          // puesto FAsynchronous=False y el segundo round salga con stream=false.
          if sToolCallsStr <> '' then
            Self.Asynchronous := True;
          ParseChat(FakeResponseObj, TempMsg);
          if sToolCallsStr = '' then
          begin
            // ParseChat (rama else) ya disparó FOnReceiveDataEnd y DoStateChange(acsFinished).
            // Solo agregamos TempMsg al historial si no está ya (RunNew async no lo agrega).
            if FMessages.IndexOf(TempMsg) = -1 then
            begin
              TempMsg.Id := FMessages.Count + 1;
              FMessages.Add(TempMsg);
            end;
            TempMsg := nil; // propiedad de FMessages
          end
          else
          begin
            // Tool calls: ParseChat ejecutó las herramientas y Self.Run inició
            // el segundo round en modo async (stream=true). El segundo round
            // emitirá sus propios eventos (SSE → [DONE] → FOnReceiveDataEnd).
            // No ponemos FBusy=False aquí: el segundo round sigue en vuelo
            // y lo marcará como False cuando termine (BLOQUE DEEPSEEK / [DONE]).
            DoStateChange(acsToolCalling, 'Ejecutando tools, segundo round en proceso...');
          end;
        finally
          if Assigned(TempMsg) then TempMsg.Free;
        end;
      finally
        FakeResponseObj.Free;
      end;
      if sToolCallsStr = '' then
        FBusy := False; // texto: todo terminó
      // tool_calls: FBusy sigue True — lo cierra el segundo round
      FTmpReasoning := ''; // Limpiar para el próximo round o próxima petición
      Exit;
    End;

    // -------------------------------------------------------------------
    // CASO 2: JSON estándar SSE con delta
    // -------------------------------------------------------------------
    var LParsedLine := TJSonObject.ParseJSONValue(ALine);
    if not (LParsedLine is TJSonObject) then
    begin
      LParsedLine.Free;
      Exit;
    end;
    jObj := TJSonObject(LParsedLine);
    Try
      jArrChoices := jObj.GetValue<TJSonArray>('choices');
      if (jArrChoices <> nil) and (jArrChoices.Count > 0) then
      begin
        Delta := jArrChoices.Items[0].GetValue<TJSonObject>('delta');
        if Assigned(Delta) then
        begin
          if Delta.TryGetValue<String>('role', Role1) then
            FTmpRole := Role1;

          Value := '';
          if Delta.TryGetValue<String>('reasoning', Value) or
             Delta.TryGetValue<String>('reasoning_content', Value) then
          begin
            FTmpReasoning := FTmpReasoning + Value; // Acumular para incluir en historial
            if (Value <> '') and Assigned(OnReceiveThinking) then
            begin
              Value := StringReplace(Value, #$A, sLineBreak, [rfReplaceAll]);
              OnReceiveThinking(Self, Nil, jObj, FTmpRole, Value);
            end;
          end;

          var JVal: TJSonValue;
          JVal := Delta.GetValue('content');
          if Assigned(JVal) and (JVal is TJSONString) then
          begin
            Value := JVal.Value;
            FLastContent := FLastContent + Value;
            if Assigned(FOnReceiveDataEvent) and (Value <> '') then
            begin
              Value := StringReplace(Value, #$A, sLineBreak, [rfReplaceAll]);
              FOnReceiveDataEvent(Self, Nil, jObj, FTmpRole, Value);
            end;
          end
          else if Assigned(JVal) and (JVal is TJSonArray) then
            ParseDeltaContentArray(TJSonArray(JVal), jObj);

          if Delta.TryGetValue<TJSonArray>('tool_calls', JToolCalls) then
          begin
            Var JToolCall: TJSonValue;
            for JToolCall in JToolCalls do
            begin
              if JToolCall is TJSonObject then
              begin
                var JTC := TJSonObject(JToolCall);
                if JTC.TryGetValue<Integer>('index', ToolIndex) then
                begin
                  if not FTmpToolCallBuffer.TryGetValue(ToolIndex, BufferObj) then
                  begin
                    BufferObj := TJSonObject.Create;
                    FTmpToolCallBuffer.Add(ToolIndex, BufferObj);
                  end;
                  if JTC.TryGetValue<String>('id', Value) then
                    if BufferObj.GetValue('id') = nil then
                      BufferObj.AddPair('id', Value);
                  if JTC.TryGetValue<String>('type', Value) then
                    if BufferObj.GetValue('type') = nil then
                      BufferObj.AddPair('type', Value);
                  if JTC.TryGetValue<TJSonObject>('function', TJSonObject(jFunc)) then
                  begin
                    if not BufferObj.TryGetValue<TJSonObject>('function', BufferFunc) then
                    begin
                      BufferFunc := TJSonObject.Create;
                      BufferObj.AddPair('function', BufferFunc);
                    end;
                    if jFunc.TryGetValue<String>('name', Value) then
                    begin
                      var OldName: string := '';
                      if BufferFunc.TryGetValue<String>('name', OldName) then
                        BufferFunc.RemovePair('name');
                      BufferFunc.AddPair('name', OldName + Value);
                    end;
                    if jFunc.TryGetValue<String>('arguments', Value) then
                    begin
                      var OldArgs: string := '';
                      if BufferFunc.TryGetValue<String>('arguments', OldArgs) then
                        BufferFunc.RemovePair('arguments');
                      BufferFunc.AddPair('arguments', OldArgs + Value);
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    Finally
      jObj.Free;
    End;
  End; // ProcessLine

begin
  LogDebug('--OnInternalReceiveData DeepSeek--');
  if Length(FResponse.DataString) > 500 then
    LogDebug(Copy(FResponse.DataString, 1, 500) + '...[truncado]')
  else
    LogDebug(FResponse.DataString);

  If FClient.Asynchronous = False then Exit;

  AAbort := FAbort;
  If FAbort = True then
  Begin
    FBusy := False;
    FTmpToolCallBuffer.Clear;
    FTmpReasoning := '';
    If Assigned(FOnReceiveDataEnd) then
      FOnReceiveDataEnd(Self, Nil, Nil, 'system', 'abort');
    Exit;
  End;

  Try
    FTmpResponseText := FTmpResponseText + FResponse.DataString;
    FResponse.Clear;

    // -------------------------------------------------------------------
    // BLOQUE DEEPSEEK: JSON completo con choices[0].message
    // Cuando DeepSeek usa stream=false con FClient.Asynchronous=True, la
    // respuesta llega como un objeto JSON completo (no líneas SSE con delta).
    // Lo detectamos al inicio para procesarlo directamente.
    // -------------------------------------------------------------------
    sJson := Trim(FTmpResponseText);
    if StartsStr('{', sJson) then
    begin
      var LParsed := TJSonObject.ParseJSONValue(sJson);
      if LParsed is TJSonObject then
      begin
        var LJObj := TJSonObject(LParsed);
        try
          var LChoices: TJSonArray;
          var LChoice: TJSonObject;
          var LMessage: TJSonObject;
          LChoices := nil; LChoice := nil; LMessage := nil;
          if LJObj.TryGetValue<TJSonArray>('choices', LChoices) and
             Assigned(LChoices) and (LChoices.Count > 0) and
             (LChoices.Items[0] is TJSonObject) then
          begin
            LChoice := TJSonObject(LChoices.Items[0]);
            if LChoice.TryGetValue<TJSonObject>('message', LMessage) then
            begin
              var LFinishReason: string := '';
              LChoice.TryGetValue<String>('finish_reason', LFinishReason);
              if not LMessage.TryGetValue<String>('role', FTmpRole) then
                FTmpRole := 'assistant';
              FTmpResponseText := '';
              FBusy := False;
              TempMsg := TAiChatMessage.Create('', FTmpRole);
              try
                ParseChat(LJObj, TempMsg);
                if LFinishReason <> 'tool_calls' then
                begin
                  // Respuesta de texto: agregar al historial
                  if FMessages.IndexOf(TempMsg) = -1 then
                  begin
                    TempMsg.Id := FMessages.Count + 1;
                    FMessages.Add(TempMsg);
                  end;
                  DoStateChange(acsFinished, 'Done');
                  if Assigned(FOnReceiveDataEnd) then
                    FOnReceiveDataEnd(Self, TempMsg, Nil, FTmpRole, FLastContent);
                  TempMsg := nil; // propiedad de FMessages
                end
                else
                begin
                  // Tool calls: ParseChat ejecutó herramientas y Self.Run inició
                  // el segundo round async. No disparar evento aquí.
                  DoStateChange(acsToolCalling, 'Segundo round en proceso...');
                end;
              finally
                if Assigned(TempMsg) then TempMsg.Free;
              end;
              Exit; // JSON DeepSeek procesado — no continuar con flujo SSE
            end;
          end;
        finally
          LJObj.Free;
        end;
      end
      else
        LParsed.Free;
    end;

    // -------------------------------------------------------------------
    // FLUJO SSE ESTÁNDAR (stream=true, delta por líneas)
    // -------------------------------------------------------------------

    // 1. Bucle principal: procesa líneas completas terminadas en #10
    while Pos(#10, FTmpResponseText) > 0 do
    begin
      P := Pos(#10, FTmpResponseText);
      sJson := Trim(Copy(FTmpResponseText, 1, P - 1));
      Delete(FTmpResponseText, 1, P);
      ProcessLine(sJson);
    end;

    // 2. Borde de seguridad: [DONE] sin salto de línea final
    sJson := Trim(FTmpResponseText);
    if (sJson = '[DONE]') or (sJson = 'data: [DONE]') then
    begin
      ProcessLine(sJson);
      FTmpResponseText := '';
    end;

  Except
    // Manejo de errores silencioso para stream (igual que base class)
  End;
end;

initialization

TAiChatFactory.Instance.RegisterDriver(TAiDeepSeekChat);

end.
