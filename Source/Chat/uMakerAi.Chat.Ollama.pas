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

unit uMakerAi.Chat.Ollama;

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
  uMakerAi.ParamsRegistry, uMakerAi.Chat, uMakerAi.Core, uMakerAi.Embeddings, uMakerAi.Utils.CodeExtractor, uMakerAi.Embeddings.Core;

type

  TAiOllamaChat = Class(TAiChat)
  Private
    Fkeep_alive: String;
    procedure Setkeep_alive(const Value: String);
  Protected
    Procedure OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean); Override;
    Function InitChatCompletions: String; Override;
    Function InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String; Override;
    Procedure ParseChat(JObj: TJSonObject; ResMsg: TAiChatMessage); Override;
    function ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions; Override;
  Public
    Class Function GetModels(aApiKey: String; aUrl: String = ''): TStringList; Override;
    Constructor Create(Sender: TComponent); Override;
    Destructor Destroy; Override;
    Function GetMessages: TJSonArray; Override;

    // ----- FUNCIONES DE GESTIÓN DE MODELOS  -----------
    procedure PullModel(const aModelName: string);
    procedure CreateModel(const aNewModelName, aModelfileContent: string);
    function ShowModelInfo(const aModelName: string): TJSonObject;
    procedure CopyModel(const aSourceModel, aDestinationModel: string);
    procedure DeleteModel(const aModelName: string);

    class function GetDriverName: string; Override;
    class procedure RegisterDefaultParams(Params: TStrings); Override;
    class function CreateInstance(Sender: TComponent): TAiChat; Override;

  Published
    property keep_alive: String read Fkeep_alive write Setkeep_alive;
  End;

  TAiOllamaEmbeddings = class(TAiEmbeddings)
  Public
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;
    Function CreateEmbedding(aInput, aUser: String; aDimensions: Integer = -1; aModel: String = ''; aEncodingFormat: String = 'float'): TAiEmbeddingData; Override;
    Procedure ParseEmbedding(JObj: TJSonObject); Override;
  end;

procedure Register;

implementation

Const
  GlAIUrl = 'http://localhost:11434/';

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiOllamaChat, TAiOllamaEmbeddings]);
end;

class function TAiOllamaChat.GetDriverName: string;
Begin
  Result := 'Ollama';
End;

class procedure TAiOllamaChat.RegisterDefaultParams(Params: TStrings);
Begin
  Params.Clear;
  Params.Add('ApiKey=@OLLAMA_API_KEY');
  Params.Add('Model=llama3');
  Params.Add('MaxTokens=4096');
  Params.Add('URL=http://localhost:11434/');
End;

procedure TAiOllamaChat.Setkeep_alive(const Value: String);
begin
  Fkeep_alive := Value;
end;

class function TAiOllamaChat.CreateInstance(Sender: TComponent): TAiChat;
Begin
  Result := TAiOllamaChat.Create(Sender);
End;

{ TAiOllamaChat }

constructor TAiOllamaChat.Create(Sender: TComponent);
begin
  inherited;
  ApiKey := '@OLLAMA_API_KEY';
  Model := 'gpt-oss:20b';
  Url := GlAIUrl;
  keep_alive := '1m';
end;

destructor TAiOllamaChat.Destroy;
begin

  inherited;
end;

function TAiOllamaChat.ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions;
var
  LResponseObj, LMessageObj, LToolCallObj, LFunctionObj, LArgumentsObj: TJSonObject;
  LToolCallsArray: TJSonArray;
  LToolCall: TAiToolsFunction;
  LItem: TJSONValue;
  LPair: TJSONPair;
begin
  Result := TAiToolsFunctions.Create;

  if (jChoices = nil) or (jChoices.Count = 0) then
    Exit;

  if not(jChoices.Items[0] is TJSonObject) then
    Exit;

  LResponseObj := jChoices.Items[0] as TJSonObject;

  if not LResponseObj.TryGetValue<TJSonObject>('message', LMessageObj) then
    Exit;

  if not LMessageObj.TryGetValue<TJSonArray>('tool_calls', LToolCallsArray) then
    Exit;

  for LItem in LToolCallsArray do
  begin
    if not(LItem is TJSonObject) then
      Continue;

    LToolCallObj := LItem as TJSonObject;

    if LToolCallObj.TryGetValue<TJSonObject>('function', LFunctionObj) then
    begin
      LToolCall := TAiToolsFunction.Create;
      try
        // Ollama ahora sí incluye un 'id', pero lo generamos como fallback por si acaso.
        LToolCall.Id := LToolCallObj.GetValue<string>('id', 'call_' + TGuid.NewGuid.ToString);
        LToolCall.Name := LFunctionObj.GetValue<string>('name', '');
        LToolCall.Tipo := 'function';

        if LFunctionObj.TryGetValue<TJSonObject>('arguments', LArgumentsObj) then
        begin
          LToolCall.Arguments := LArgumentsObj.Format;
          for LPair in LArgumentsObj do
          begin
            LToolCall.Params.Values[LPair.JsonString.Value] := LPair.JsonValue.Value;
          end;
        end
        else
        begin
          LToolCall.Arguments := '{}';
        end;

        Result.Add(LToolCall.Id, LToolCall);
      except
        LToolCall.Free;
        raise;
      end;
    end;
  end;
end;

function TAiOllamaChat.GetMessages: TJSonArray;
Var
  I, J: Integer;
  Msg: TAiChatMessage;
  JObj: TJSonObject;
  jImages: TJSonArray;

  Base64: String;
  MediaArr: TAiMediaFilesArray;
  Mime: String;
begin
  Result := TJSonArray.Create;

  For I := 0 to Messages.Count - 1 do
  Begin
    Msg := Messages.Items[I];
    JObj := TJSonObject.Create;

    If Msg.TollCallId <> '' then
      JObj.AddPair('tool_call_id', Msg.TollCallId);

    If Msg.FunctionName <> '' then
      JObj.AddPair('name', Msg.FunctionName);

    JObj.AddPair('role', Msg.Role);
    JObj.AddPair('content', Msg.Prompt);

    MediaArr := Msg.MediaFiles.GetMediaList([Tfc_Image], False);

    If (Length(MediaArr) > 0) then
    Begin

      jImages := TJSonArray.Create;
      JObj.AddPair('images', jImages);

      For J := 0 to Msg.MediaFiles.Count - 1 do // Open Ai permite subir el Base64 o el Url, siempre se sube el Base64, por estandar
      Begin
        Base64 := Msg.MediaFiles[J].Base64;
        Mime := Msg.MediaFiles[J].MimeType;

        jImages.Add(Msg.MediaFiles[J].Base64);
      End;
    End;

    If Msg.Tool_calls <> '' then

{$IF CompilerVersion < 35}
      JObj.AddPair('tool_calls', TJSONUtils.ParseAsArray(Msg.Tool_calls));
{$ELSE}
      JObj.AddPair('tool_calls', TJSonArray(TJSonArray.ParseJSONValue(Msg.Tool_calls)));
{$ENDIF}
    Result.Add(JObj);
  End;
end;

class function TAiOllamaChat.GetModels(aApiKey, aUrl: String): TStringList;
Var
  Client: TNetHTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  Response: TStringStream;
  sUrl, EndPointUrl: String;
  jRes: TJSonObject;
  JArr: TJSonArray;
  JVal: TJSONValue;
  sModel: string;
  CustomModels: TArray<string>;
  I: Integer;
begin
  Result := TStringList.Create;

  If aUrl <> '' then
    EndPointUrl := aUrl
  Else
    EndPointUrl := GlAIUrl;

  Client := TNetHTTPClient.Create(Nil);
{$IF CompilerVersion >= 35}
  Client.SynchronizeEvents := False;
{$ENDIF}
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := EndPointUrl + 'api/tags';

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + aApiKey)];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      jRes := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      If jRes.TryGetValue<TJSonArray>('models', JArr) then
      Begin
        For JVal in JArr do
        Begin
          sModel := JVal.GetValue<String>('name');
          If sModel <> '' then
            Result.Add(sModel);
        End;
      End;

      // Agregar modelos personalizados
      CustomModels := TAiChatFactory.Instance.GetCustomModels(Self.GetDriverName);

      for I := Low(CustomModels) to High(CustomModels) do
      begin
{$IF CompilerVersion <= 35.0}
        if Result.IndexOf(CustomModels[I]) = -1 then
          Result.Add(CustomModels[I]);
{$ELSE}
        if not Result.Contains(CustomModels[I]) then
          Result.Add(CustomModels[I]);
{$ENDIF}
      end;

    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;
  Finally
    Client.Free;
    Response.Free;
  End;
end;

function TAiOllamaChat.InitChatCompletions: String;
Var
  AJSONObject, jOptions: TJSonObject;
  JArr, JStop: TJSonArray;
  Lista: TStringList;
  I: Integer;
  LModel: String;
begin
  // 1. Configuración básica y Modelo
  If User = '' then
    User := 'user';

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);
  If LModel = '' then
    LModel := 'gpt-oss:20b'; // Fallback seguro

  // Configuramos el cliente HTTP según la propiedad del componente
  FClient.Asynchronous := Self.Asynchronous;

  // Aumentamos timeout por defecto ya que los modelos locales pueden tardar en cargar
  if FClient.ResponseTimeout < 60000 then
    FClient.ResponseTimeout := 1000 * 60 * 5; // 5 minutos por defecto

  AJSONObject := TJSonObject.Create;
  jOptions := TJSonObject.Create; // Objeto para parámetros avanzados
  Lista := TStringList.Create;

  Try
    // --- PARÁMETROS RAÍZ ---
    AJSONObject.AddPair('model', LModel);
    AJSONObject.AddPair('messages', GetMessages); // Usa GetMessages (revisaremos este después)

    // Respetamos la configuración asíncrona (True/False)
    AJSONObject.AddPair('stream', TJSONBool.Create(Self.Asynchronous));

    if Fkeep_alive <> '' then
      AJSONObject.AddPair('keep_alive', Fkeep_alive);

    // 1. Structured Outputs (JSON Schema)
    if FResponse_format = tiaChatRfJsonSchema then
    begin
      if JsonSchema.Text <> '' then
      begin
        try
          // Ollama espera el esquema DIRECTAMENTE en el parámetro "format".
          // No requiere wrappers como "json_schema" o "schema".
          Var sShema := StringReplace(JsonSchema.Text,'\n',' ',[rfReplaceAll]);
          var
          JSchema := TJSonObject.ParseJSONValue(sShema);

          if Assigned(JSchema) then
          begin
            if JSchema is TJSonObject then
              AJSONObject.AddPair('format', JSchema as TJSonObject)
            else
              JSchema.Free; // Si no es un objeto válido, limpiar
          end;
        except
          // Manejo silencioso de errores de parseo, se enviará sin formato o ignorado
        end;
      end;
    end
    // 2. JSON Mode (Simple)
    else if (FResponse_format = tiaChatRfJson) then
    begin
      AJSONObject.AddPair('format', 'json');
    end;

    // --- MANEJO DE TOOLS ---
    If Tool_Active and (Trim(GetTools(TToolFormat.tfOpenAi).Text) <> '') then
    Begin
{$IF CompilerVersion < 35}
      JArr := TJSONUtils.ParseAsArray(GetTools(TToolFormat.tfOpenAi).Text);
{$ELSE}
      JArr := TJSonArray(TJSonArray.ParseJSONValue(GetTools(TToolFormat.tfOpenAi).Text));
{$ENDIF}
      If Assigned(JArr) then
        AJSONObject.AddPair('tools', JArr);
    End;

    // --- PARÁMETROS "OPTIONS" (Diferencia clave con OpenAI) ---
    // Ollama requiere encapsular estos parámetros dentro de 'options'

    if Temperature > 0 then
      jOptions.AddPair('temperature', TJSONNumber.Create(Temperature));

    // OJO: Ollama usa 'num_predict' en lugar de 'max_tokens'
    if Max_tokens > 0 then
      jOptions.AddPair('num_predict', TJSONNumber.Create(Max_tokens));

    If Top_p <> 0 then
      jOptions.AddPair('top_p', TJSONNumber.Create(Top_p));

    If Frequency_penalty <> 0 then
      jOptions.AddPair('frequency_penalty', TJSONNumber.Create(Frequency_penalty));

    If Presence_penalty <> 0 then
      jOptions.AddPair('presence_penalty', TJSONNumber.Create(Presence_penalty));

    If Seed > 0 then
      jOptions.AddPair('seed', TJSONNumber.Create(Seed));

    // Manejo de Stop Sequences
    Lista.CommaText := Stop;
    If Lista.Count > 0 then
    Begin
      JStop := TJSonArray.Create;
      For I := 0 to Lista.Count - 1 do
        JStop.Add(Lista[I]);
      jOptions.AddPair('stop', JStop);
    End;

    // Agregamos el objeto options al JSON principal
    if jOptions.Count > 0 then
      AJSONObject.AddPair('options', jOptions)
    else
      jOptions.Free; // Si no se añadió al padre, hay que liberarlo

    // Generación del String final
    Result := AJSONObject.ToJSON;

  Finally
    AJSONObject.Free;
    // jOptions se libera automáticamente si fue añadido a AJSONObject
    Lista.Free;
  End;
end;

function TAiOllamaChat.InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String;
Var
  ABody: String;
  sUrl: String;
  Res: IHTTPResponse;
  St: TStringStream;
  FHeaders: TNetHeaders;
  JObj: TJSonObject;
begin

  FBusy := True; // Marca como ocupado al sistema
  FAbort := False; // Inicia como no en abort
  FLastError := '';
  FLastContent := '';
  FLastPrompt := '';

  DoStateChange(acsConnecting, 'Sending request...');

  St := TStringStream.Create('', TEncoding.UTF8);
  sUrl := Url + 'api/chat';

  Try
    FHeaders := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    FClient.ContentType := 'application/json';

    ABody := InitChatCompletions;

    LogDebug('-Peticion-');
    LogDebug(ABody);

    St.WriteString(ABody);
    St.Position := 0;
    FResponse.Clear;
    FResponse.Position := 0;

    Res := FClient.Post(sUrl, St, FResponse, FHeaders);

    FLastContent := '';

    If FClient.Asynchronous = False then
    Begin
      if Res.StatusCode = 200 then
      Begin
        Var
        S := Res.ContentAsString;

        LogDebug('-Resultado-');
        LogDebug(S);

        JObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
        Try
          FBusy := False;
          ParseChat(JObj, ResMsg);
          Result := FLastContent;

        Finally
          // FreeAndNil(JObj);  //se comenta porque marca error porque ya ha sido eliminado previamente en alguna parte
        End;
      End
      else
      begin
        Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
      end;
    End;
  Finally
    If FClient.Asynchronous = False then
      St.Free;
    // Esto no funciona en multiarea, así que se libera cuando no lo es.
  End;
end;

function IsCompleteJson(const S: string): Boolean;
var
  I, Balance: Integer;
  InString, Escape: Boolean;
begin
  Balance := 0;
  InString := False;
  Escape := False;

  for I := 1 to Length(S) do
  begin
    if Escape then
    begin
      Escape := False;
      Continue;
    end;

    case S[I] of
      '\':
        if InString then
          Escape := True;

      '"':
        InString := not InString;

      '{':
        if not InString then
          Inc(Balance);

      '}':
        if not InString then
          Dec(Balance);
    end;
  end;

  Result := (Balance = 0) and (not InString);
end;

{ procedure TAiOllamaChat.OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean);
  var
  JObj, Delta: TJSonObject;
  sJson: String;
  Done: Boolean;
  Msg: TAiChatMessage;
  aPrompt_tokens, aCompletion_tokens, aTotal_tokens: Integer;
  Role, Respuesta, Thinking: String;
  LModel: String;
  begin
  //  if not FClient.Asynchronous then
  //    Exit;

  AAbort := FAbort;

  if FAbort then
  begin
  FBusy := False;
  if Assigned(FOnReceiveDataEnd) then
  FOnReceiveDataEnd(Self, Nil, Nil, 'system', 'abort');
  Exit;
  end;

  try
  // Recuperar chunk recibido
  sJson := FResponse.DataString;

  FResponse.Clear;

  // Acumular
  FTmpResponseText := FTmpResponseText + sJson;

  // Solo procesar si el JSON está completo
  if (sJson <> '') and IsCompleteJson(FTmpResponseText) then
  begin
  JObj := TJSonObject(TJSonObject.ParseJSONValue(FTmpResponseText));
  try
  if Assigned(JObj) then
  begin
  if JObj.TryGetValue<Boolean>('done', Done) then
  begin
  if Done then
  begin
  // ✅ Cuando termina de recibir
  LModel := JObj.GetValue('model').Value;
  aPrompt_tokens := JObj.GetValue<Integer>('prompt_eval_count');
  aCompletion_tokens := JObj.GetValue<Integer>('eval_count');
  aTotal_tokens := aPrompt_tokens + aCompletion_tokens;

  // Actualiza contadores
  Prompt_tokens := Prompt_tokens + aPrompt_tokens;
  Completion_tokens := Completion_tokens + aCompletion_tokens;
  Total_tokens := Total_tokens + aTotal_tokens;

  if JObj.TryGetValue<TJSonObject>('message', Delta) then
  begin
  Respuesta := Delta.GetValue<String>('content');
  Role := Delta.GetValue<String>('role');
  FLastContent := FLastContent + Respuesta;

  Msg := TAiChatMessage.Create(FLastContent, Role);
  Msg.Prompt := FLastContent;
  Msg.Prompt_tokens := aPrompt_tokens;
  Msg.Completion_tokens := aCompletion_tokens;
  Msg.Total_tokens := aTotal_tokens;
  Msg.Id := FMessages.Count + 1;
  FMessages.Add(Msg);
  FBusy := False;

  if Assigned(FOnReceiveDataEnd) then
  FOnReceiveDataEnd(Self, Msg, Nil, Role, FLastContent);
  end;
  end
  else
  begin
  // ✅ Todavía no termina el mensaje
  if JObj.TryGetValue<TJSonObject>('message', Delta) then
  begin
  Respuesta := Delta.GetValue<String>('content');

  Delta.TryGetValue<String>('thinking', Thinking);

  Role := Delta.GetValue<String>('role');
  FLastContent := FLastContent + Respuesta;

  Respuesta := StringReplace(Respuesta, #$A, sLineBreak, [rfReplaceAll]);
  Thinking := StringReplace(Thinking, #$A, sLineBreak, [rfReplaceAll]);

  If (Respuesta <> '') and Assigned(FOnReceiveDataEvent) then
  FOnReceiveDataEvent(Self, Nil, JObj, Role, Respuesta);

  If (Thinking <> '') and Assigned(OnReceiveThinking) then
  OnReceiveThinking(Self, Nil, JObj, Role, Thinking);


  end;
  end;
  end;
  end;
  finally
  JObj.Free;
  FTmpResponseText := ''; // 🔹 Limpiamos buffer ya procesado
  end;
  end;
  except
  on E: Exception do
  LastError := 'El json "' + FTmpResponseText + '" no es válido. ' + E.Message;
  end;
  end;
}

procedure TAiOllamaChat.OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean);
var
  LJsonObject, LMessageObj: TJSonObject;
  LChunkStr, LJsonLine, LRole, LContentPart, LThinkingPart: string;
  LDone: Boolean;
  LFinalMsg: TAiChatMessage;
  LStreamFinished: Boolean;
begin

  if Self.Asynchronous = False then
    Exit;

  AAbort := FAbort;
  if FAbort then
  begin
    FBusy := False;
    if Assigned(FOnReceiveDataEnd) then
      FOnReceiveDataEnd(Self, nil, nil, 'system', 'abort');
    Exit;
  end;

  try
    // 1. Acumular el nuevo chunk de datos recibido del stream
    LChunkStr := FResponse.DataString;
    FResponse.Clear;
    FTmpResponseText := FTmpResponseText + LChunkStr;
    LStreamFinished := False;

    LogDebug('-Response Stream-');
    LogDebug(LChunkStr);

    // --- Bucle principal para procesar líneas completas (delimitadas por #10) ---
    while Pos(#10, FTmpResponseText) > 0 do
    begin
      LJsonLine := Copy(FTmpResponseText, 1, Pos(#10, FTmpResponseText) - 1);
      Delete(FTmpResponseText, 1, Pos(#10, FTmpResponseText));

      if LJsonLine.Trim.IsEmpty then
        Continue;

      try
        LJsonObject := TJSonObject.ParseJSONValue(LJsonLine.Trim) as TJSonObject;
        if not Assigned(LJsonObject) then
          Continue;
        try
          LDone := LJsonObject.GetValue<Boolean>('done', False);
          if not LDone then // --- Es un fragmento INTERMEDIO ---
          begin
            if LJsonObject.TryGetValue<TJSonObject>('message', LMessageObj) then
            begin
              LRole := LMessageObj.GetValue<string>('role', 'assistant');

              if LMessageObj.TryGetValue<string>('content', LContentPart) and (LContentPart <> '') then
              begin
                // --- CORRECCIÓN: Normalizar saltos de línea (\n -> \r\n en Windows) ---
                LContentPart := StringReplace(LContentPart, #$A, sLineBreak, [rfReplaceAll]);

                FLastContent := FLastContent + LContentPart;
                if Assigned(FOnReceiveDataEvent) then
                  FOnReceiveDataEvent(Self, nil, LJsonObject, LRole, LContentPart);
              end;

              if LMessageObj.TryGetValue<string>('thinking', LThinkingPart) and (LThinkingPart <> '') then
              begin
                // --- CORRECCIÓN: Normalizar saltos de línea en Thinking también ---
                LThinkingPart := StringReplace(LThinkingPart, #$A, sLineBreak, [rfReplaceAll]);

                if Assigned(OnReceiveThinking) then
                  OnReceiveThinking(Self, nil, LJsonObject, LRole, LThinkingPart);
              end;

            end;
          end
          else // --- Es un fragmento FINAL dentro del bucle ---
          begin
            LStreamFinished := True;
            FBusy := False;
            LFinalMsg := TAiChatMessage.Create('', 'assistant');
            try
              ParseChat(LJsonObject, LFinalMsg);
            except
              LFinalMsg.Free;
              raise;
            end;
            FLastContent := '';
            FTmpResponseText := ''; // Limpiamos todo el buffer
            Break; // Salimos del bucle, ya hemos terminado
          end;
        finally
          // LJsonObject.Free;  //OJO Revisar porque marca error al intentar liberar
        end;
      except
        // Ignorar JSON incompleto, el bucle continuará
      end;
    end; // Fin del while

    // --- MANEJO DEL FRAGMENTO FINAL: Si el bucle terminó y aún queda texto, ---
    // --- podría ser el último JSON que no terminó en #10. ---
    if (not LStreamFinished) and (FTmpResponseText.Trim <> '') then
    begin
      try
        // Intentamos parsear lo que queda en el buffer. Si falla, es un fragmento incompleto y salta al except.
        LJsonObject := TJSonObject.ParseJSONValue(FTmpResponseText.Trim) as TJSonObject;
        if not Assigned(LJsonObject) then
          Exit; // No debería pasar si el parseo fue exitoso
        try
          LDone := LJsonObject.GetValue<Boolean>('done', False);
          if LDone then // ¡Éxito! Es el JSON final.
          begin
            LStreamFinished := True;
            FBusy := False;
            LFinalMsg := TAiChatMessage.Create('', 'assistant');
            try
              ParseChat(LJsonObject, LFinalMsg);
            except
              LFinalMsg.Free;
              raise;
            end;
            FLastContent := '';
            FTmpResponseText := ''; // Limpiamos el buffer porque ya lo procesamos por completo.
          end;
          // Si no es "done: true", simplemente dejamos el texto en el buffer y esperamos más datos.
        finally
          LJsonObject.Free;
        end;
      except
        // No es un JSON completo. No hacemos nada, esperamos el siguiente chunk de datos.
      end;
    end;
  except
    on E: Exception do
      DoError('Error en OnInternalReceiveData: ' + E.Message, E);
  end;
end;

{ procedure TAiOllamaChat.ParseChat(JObj: TJSonObject; ResMsg: TAiChatMessage);
  Var
  choices, JToolCalls: TJSonArray;
  jMessage: TJSonObject;
  aPrompt_tokens, aCompletion_tokens, aTotal_tokens: Integer;
  Role, Respuesta: String;
  ToolMsg, AskMsg: TAiChatMessage;
  // Msg: TAiChatMessage;
  LFunciones: TAiToolsFunctions;
  ToolCall: TAiToolsFunction;

  TaskList: array of ITask;
  I, NumTasks: Integer;
  Clave, sToolCalls: String;
  LModel: String;

  Code: TMarkdownCodeExtractor;
  CodeFile: TCodeFile;
  CodeFiles: TCodeFileList;
  MF: TAiMediaFile;
  St: TStringStream;

  begin

  If Not Assigned(JObj) then
  Exit;


  aPrompt_tokens := 0;
  aCompletion_tokens := 0;
  // aTotal_tokens := 0;

  AskMsg := GetLastMessage; // Obtiene el mensaje de la solicitud

  LModel := JObj.GetValue('model').Value;

  JObj.TryGetValue<Integer>('prompt_eval_count', aPrompt_tokens);
  JObj.GetValue<Integer>('eval_count', aCompletion_tokens);
  aTotal_tokens := aPrompt_tokens + aCompletion_tokens;

  If JObj.TryGetValue<TJSonObject>('message', jMessage) then
  Begin
  Respuesta := jMessage.GetValue<String>('content').Trim + sLineBreak;
  Role := jMessage.GetValue<String>('role');

  If jMessage.TryGetValue<TJSonArray>('tool_calls', JToolCalls) then
  sToolCalls := JToolCalls.Format;
  End;

  DoProcessResponse(GetLastMessage, ResMsg, Respuesta);

  Self.FLastContent := Respuesta;
  Prompt_tokens := Prompt_tokens + aPrompt_tokens;
  Completion_tokens := Completion_tokens + aCompletion_tokens;
  Total_tokens := Total_tokens + aTotal_tokens;

  ResMsg.Prompt := Trim(ResMsg.Prompt + sLineBreak + Respuesta);
  ResMsg.Tool_calls := sToolCalls;
  ResMsg.Prompt_tokens := ResMsg.Prompt_tokens + aPrompt_tokens;
  ResMsg.Completion_tokens := ResMsg.Completion_tokens + aCompletion_tokens;
  ResMsg.Total_tokens := ResMsg.Total_tokens + aTotal_tokens;
  ResMsg.Model := LModel;

  choices := TJSonArray.Create;
  choices.Add(JObj);
  LFunciones := ExtractToolCallFromJson(choices);
  choices.Remove(0);
  choices.Free;

  Try
  If LFunciones.Count > 0 then
  Begin

  NumTasks := LFunciones.Count;
  SetLength(TaskList, NumTasks);
  // Ajusta el tamaño del array para el número de tareas

  I := 0;
  For Clave in LFunciones.Keys do
  Begin
  ToolCall := LFunciones[Clave];
  ToolCall.ResMsg := ResMsg;
  ToolCall.AskMsg := AskMsg;

  TaskList[I] := TTask.Create(
  procedure
  begin
  DoCallFunction(ToolCall);
  end);
  TaskList[I].Start;
  Inc(I);

  End;
  TTask.WaitForAll(TaskList);

  For Clave in LFunciones.Keys do
  Begin
  ToolCall := LFunciones[Clave];
  ToolMsg := TAiChatMessage.Create(ToolCall.Response, 'tool', ToolCall.Id, ToolCall.Name);
  ToolMsg.Id := FMessages.Count + 1;
  FMessages.Add(ToolMsg);
  End;

  Self.Run(Nil, ResMsg);

  End
  Else
  Begin

  If (tfc_ExtracttextFile in NativeOutputFiles) then
  Begin
  Code := TMarkdownCodeExtractor.Create;
  Try

  CodeFiles := Code.ExtractCodeFiles(Respuesta);
  For CodeFile in CodeFiles do
  Begin
  St := TStringStream.Create(CodeFile.Code);
  Try
  St.Position := 0;

  MF := TAiMediaFile.Create;
  MF.LoadFromStream('file.' + CodeFile.FileType, St);
  ResMsg.MediaFiles.Add(MF);
  Finally
  St.Free;
  End;

  End;
  Finally
  Code.Free;
  End;
  End;

  DoProcessResponse(AskMsg, ResMsg, Respuesta);

  ResMsg.Prompt := Respuesta;

  FBusy := False;
  If Assigned(FOnReceiveDataEnd) then
  FOnReceiveDataEnd(Self, ResMsg, JObj, Role, Respuesta);
  End;
  Finally
  LFunciones.Free;
  End;
  end;
}

procedure TAiOllamaChat.ParseChat(JObj: TJSonObject; ResMsg: TAiChatMessage);
var
  LMessageObj: TJSonObject;
  LToolCallsArray: TJSonArray;
  LAskMsg: TAiChatMessage;
  LRole, LContent, LModel, LReasoning: string;
  LPromptTokens, LEvalTokens: Integer;

  // Tools
  LChoicesSimulado: TJSonArray;
  LFunciones: TAiToolsFunctions;
  LToolCall: TAiToolsFunction;
  LToolMsg, LHistoryToolMsg: TAiChatMessage;

  // Paralelismo
  TaskList: array of ITask;
  I, NumTasks: Integer;
  Clave: String;

  // Archivos
  Code: TMarkdownCodeExtractor;
  CodeFiles: TCodeFileList;
  CodeFile: TCodeFile;
  MF: TAiMediaFile;
  St: TStringStream;
begin
  if not Assigned(JObj) then
    Exit;

  // 1. Extraer Datos
  LModel := JObj.GetValue<string>('model', '');
  LPromptTokens := JObj.GetValue<Integer>('prompt_eval_count', 0);
  LEvalTokens := JObj.GetValue<Integer>('eval_count', 0);

  Self.Prompt_tokens := Self.Prompt_tokens + LPromptTokens;
  Self.Completion_tokens := Self.Completion_tokens + LEvalTokens;
  Self.Total_tokens := Self.Total_tokens + LPromptTokens + LEvalTokens;

  // 2. Validar Message
  if not JObj.TryGetValue<TJSonObject>('message', LMessageObj) then
  begin
    if JObj.GetValue<Boolean>('done', False) then
    begin
      DoStateChange(acsFinished, 'Done');
      if Assigned(FOnReceiveDataEnd) then
        FOnReceiveDataEnd(Self, ResMsg, JObj, 'assistant', FLastContent);
    end;
    Exit;
  end;

  LRole := LMessageObj.GetValue<string>('role', 'assistant');
  LReasoning := LMessageObj.GetValue<string>('thinking', '');
  LContent := LMessageObj.GetValue<string>('content', '');

  if FLastContent <> '' then
    ResMsg.Content := FLastContent
  else
  Begin
    ResMsg.Content := LContent;
    FLastContent := LContent; // <-- Importante esta asignació se requiere en el resto del proceso
  End;

  ResMsg.Prompt := ResMsg.Content;
  ResMsg.Role := LRole;
  ResMsg.Model := LModel;
  ResMsg.ReasoningContent := LReasoning;
  ResMsg.Prompt_tokens := LPromptTokens;
  ResMsg.Completion_tokens := LEvalTokens;
  ResMsg.Total_tokens := LPromptTokens + LEvalTokens;

  LAskMsg := GetLastMessage;

  // 3. Lógica de Tools
  if LMessageObj.TryGetValue<TJSonArray>('tool_calls', LToolCallsArray) and (LToolCallsArray.Count > 0) then
  begin
    // CASO A: Tool Calls

    // Creamos mensaje histórico para la petición de tool (Assistant)
    LHistoryToolMsg := TAiChatMessage.Create(LContent, 'assistant');
    LHistoryToolMsg.Tool_calls := LToolCallsArray.ToJSON;
    LHistoryToolMsg.Id := FMessages.Count + 1;
    FMessages.Add(LHistoryToolMsg);

    DoStateChange(acsToolExecuting, 'Executing tools...');

    LChoicesSimulado := TJSonArray.Create;
    LFunciones := nil;
    try
      LChoicesSimulado.Add(JObj.Clone as TJSonObject);
      LFunciones := ExtractToolCallFromJson(LChoicesSimulado);

      if (LFunciones <> nil) and (LFunciones.Count > 0) then
      begin
        // Paralelismo con TTask
        NumTasks := LFunciones.Count;
        SetLength(TaskList, NumTasks);
        I := 0;

        for Clave in LFunciones.Keys do
        begin
          LToolCall := LFunciones[Clave];
          LToolCall.ResMsg := ResMsg;
          LToolCall.AskMsg := LAskMsg;

          TaskList[I] := TTask.Create(
            procedure
            var
              CapturaTool: TAiToolsFunction;
            begin
              CapturaTool := LToolCall;
              try
                DoCallFunction(CapturaTool);
              except
                on E: Exception do
                  TThread.Queue(nil,
                    procedure
                    begin
                      DoError('Tool Error', E);
                    end);
              end;
            end);
          TaskList[I].Start;
          Inc(I);
        end;

        TTask.WaitForAll(TaskList);

        // Agregar resultados (Tool Role)
        for LToolCall in LFunciones.Values do
        begin
          LToolMsg := TAiChatMessage.Create(LToolCall.Response, 'tool', LToolCall.Id, LToolCall.Name);
          LToolMsg.Id := FMessages.Count + 1;
          FMessages.Add(LToolMsg);
        end;

        // Limpiar ResMsg y re-ejecutar
        ResMsg.Content := '';
        ResMsg.Tool_calls := '';
        Self.Run(nil, ResMsg);
      end;
    finally
      LChoicesSimulado.Free;
      if Assigned(LFunciones) then
        LFunciones.Free;
    end;
  end
  else
  begin
    // CASO B: Texto Normal

    if (tfc_ExtracttextFile in NativeOutputFiles) and (ResMsg.Content <> '') then
    begin
      Code := TMarkdownCodeExtractor.Create;
      try
        CodeFiles := Code.ExtractCodeFiles(ResMsg.Content);
        for CodeFile in CodeFiles do
        begin
          St := TStringStream.Create(CodeFile.Code);
          try
            St.Position := 0;
            MF := TAiMediaFile.Create;
            MF.LoadFromStream('file.' + CodeFile.FileType, St);
            ResMsg.MediaFiles.Add(MF);
          finally
            St.Free;
          end;
        end;
      finally
        Code.Free;
      end;
    end;

    DoProcessResponse(LAskMsg, ResMsg, FLastContent);

    // --- CORRECCIÓN CRÍTICA: Evitar Doble Add ---
    // Solo añadimos si NO está en la lista.
    // En modo Síncrono, TAiChat.Run lo intentará añadir después, así que esto protege.
    // En modo Asíncrono, TAiChat.Run salió, así que esto lo añade.
    if Self.Asynchronous and (FMessages.IndexOf(ResMsg) = -1) then
    begin
      ResMsg.Id := FMessages.Count + 1;
      FMessages.Add(ResMsg);
    end;

    DoStateChange(acsFinished, 'Done');
    if Assigned(FOnReceiveDataEnd) then
      FOnReceiveDataEnd(Self, ResMsg, JObj, LRole, FLastContent);

    FBusy := False;
  end;
end;


// ----- FUNCIONES DE GESTIÓN DE MODELOS  -----------

procedure TAiOllamaChat.CopyModel(const aSourceModel, aDestinationModel: string);
var
  LJsonObject: TJSonObject;
  LBodyStream: TStringStream;
  LResponse: IHTTPResponse;
  LUrl: string;
begin
  LUrl := TPath.Combine(Self.Url, 'api/copy');
  LJsonObject := TJSonObject.Create;
  LBodyStream := TStringStream.Create('', TEncoding.UTF8);
  try
    LJsonObject.AddPair('source', aSourceModel);
    LJsonObject.AddPair('destination', aDestinationModel);
    LBodyStream.WriteString(LJsonObject.ToJSON);
    LBodyStream.Position := 0;

    FClient.ContentType := 'application/json';
    LResponse := FClient.Post(LUrl, LBodyStream);

    if LResponse.StatusCode <> 200 then
      raise Exception.CreateFmt('Error al copiar el modelo: %d - %s', [LResponse.StatusCode, LResponse.ContentAsString]);

  finally
    LJsonObject.Free;
    LBodyStream.Free;
  end;
end;

procedure TAiOllamaChat.CreateModel(const aNewModelName, aModelfileContent: string);
var
  LJsonObject: TJSonObject;
  LBodyStream: TStringStream;
  LResponse: IHTTPResponse;
  LResponseStream: TStringStream;
  LUrl: string;
  LJsonLines: TArray<string>;
  LLine: string;
  LStatusObj: TJSonObject;
  LStatus: string;
  LCompleted, LTotal: Int64;
begin
  LUrl := TPath.Combine(Self.Url, 'api/create');
  LJsonObject := TJSonObject.Create;
  LBodyStream := TStringStream.Create('', TEncoding.UTF8);
  LResponseStream := TStringStream.Create('', TEncoding.UTF8);
  try
    LJsonObject.AddPair('name', aNewModelName);
    LJsonObject.AddPair('modelfile', aModelfileContent);
    LJsonObject.AddPair('stream', TJSONBool.Create(True)); // Siempre en stream para progreso
    LBodyStream.WriteString(LJsonObject.ToJSON);
    LBodyStream.Position := 0;

    FClient.ContentType := 'application/json';
    LResponse := FClient.Post(LUrl, LBodyStream, LResponseStream);

    if LResponse.StatusCode <> 200 then
      raise Exception.CreateFmt('Error al crear el modelo: %d - %s', [LResponse.StatusCode, LResponse.ContentAsString]);

    // Procesar la respuesta en stream (linea por linea)
    LResponseStream.Position := 0;
    LJsonLines := LResponseStream.DataString.Split([#10], TStringSplitOptions.ExcludeEmpty);

    for LLine in LJsonLines do
    begin
      if Assigned(OnProgressEvent) then
      begin
        LStatusObj := TJSonObject.ParseJSONValue(LLine) as TJSonObject;
        if Assigned(LStatusObj) then
          try
            LStatus := LStatusObj.GetValue<string>('status');
            LCompleted := 0;
            LTotal := 0;
            LStatusObj.TryGetValue<Int64>('completed', LCompleted);
            LStatusObj.TryGetValue<Int64>('total', LTotal);
            OnProgressEvent(Self, LStatus, LCompleted, LTotal);
          finally
            LStatusObj.Free;
          end;
      end;
    end;

  finally
    LJsonObject.Free;
    LBodyStream.Free;
    LResponseStream.Free;
  end;
end;

procedure TAiOllamaChat.DeleteModel(const aModelName: string);
var
  LJsonObject: TJSonObject;
  LBodyStream: TStringStream;
  LResponse: IHTTPResponse;
  LUrl: string;
begin
  LUrl := TPath.Combine(Self.Url, 'api/delete');
  LJsonObject := TJSonObject.Create;
  LBodyStream := TStringStream.Create('', TEncoding.UTF8);
  try
    LJsonObject.AddPair('name', aModelName);
    LBodyStream.WriteString(LJsonObject.ToJSON);
    LBodyStream.Position := 0;

    // TNetHTTPClient.Delete necesita un TStream como body
    LResponse := FClient.Delete(LUrl, LBodyStream);

    if LResponse.StatusCode <> 200 then
      raise Exception.CreateFmt('Error al eliminar el modelo: %d - %s', [LResponse.StatusCode, LResponse.ContentAsString]);

  finally
    LJsonObject.Free;
    LBodyStream.Free;
  end;
end;

procedure TAiOllamaChat.PullModel(const aModelName: string);
var
  LJsonObject: TJSonObject;
  LBodyStream: TStringStream;
  LResponse: IHTTPResponse;
  LResponseStream: TStringStream;
  LUrl: string;
  LJsonLines: TArray<string>;
  LLine: string;
  LStatusObj: TJSonObject;
  LStatus: string;
  LCompleted, LTotal: Int64;
begin
  LUrl := TPath.Combine(Self.Url, 'api/pull');
  LJsonObject := TJSonObject.Create;
  LBodyStream := TStringStream.Create('', TEncoding.UTF8);
  LResponseStream := TStringStream.Create('', TEncoding.UTF8);
  try
    LJsonObject.AddPair('name', aModelName);
    LJsonObject.AddPair('stream', TJSONBool.Create(True)); // Siempre en stream para progreso
    LBodyStream.WriteString(LJsonObject.ToJSON);
    LBodyStream.Position := 0;

    FClient.ContentType := 'application/json';
    // Hacemos la llamada síncrona, pero Ollama devuelve el stream completo de una vez
    LResponse := FClient.Post(LUrl, LBodyStream, LResponseStream);

    if LResponse.StatusCode <> 200 then
      raise Exception.CreateFmt('Error al descargar el modelo: %d - %s', [LResponse.StatusCode, LResponse.ContentAsString]);

    // Procesar la respuesta en stream (linea por linea)
    LResponseStream.Position := 0;
    LJsonLines := LResponseStream.DataString.Split([#10], TStringSplitOptions.ExcludeEmpty);

    for LLine in LJsonLines do
    begin
      // Si el evento de progreso está asignado, lo disparamos
      if Assigned(OnProgressEvent) then
      begin
        LStatusObj := TJSonObject.ParseJSONValue(LLine) as TJSonObject;
        if Assigned(LStatusObj) then
          try
            LStatus := LStatusObj.GetValue<string>('status');
            LCompleted := 0;
            LTotal := 0;
            // TryGetValue es más seguro si los campos no siempre están presentes
            LStatusObj.TryGetValue<Int64>('completed', LCompleted);
            LStatusObj.TryGetValue<Int64>('total', LTotal);
            OnProgressEvent(Self, LStatus, LCompleted, LTotal);
          finally
            LStatusObj.Free;
          end;
      end;
    end;

  finally
    LJsonObject.Free;
    LBodyStream.Free;
    LResponseStream.Free;
  end;
end;

function TAiOllamaChat.ShowModelInfo(const aModelName: string): TJSonObject;
var
  LJsonObject: TJSonObject;
  LBodyStream: TStringStream;
  LResponse: IHTTPResponse;
  LUrl: string;
begin
  Result := nil;
  LUrl := TPath.Combine(Self.Url, 'api/show');
  LJsonObject := TJSonObject.Create;
  LBodyStream := TStringStream.Create('', TEncoding.UTF8);
  try
    LJsonObject.AddPair('name', aModelName);
    LBodyStream.WriteString(LJsonObject.ToJSON);
    LBodyStream.Position := 0;

    FClient.ContentType := 'application/json';
    LResponse := FClient.Post(LUrl, LBodyStream);

    if LResponse.StatusCode = 200 then
    begin
      // El llamador es responsable de liberar el TJSONObject devuelto
      Result := TJSonObject.ParseJSONValue(LResponse.ContentAsString) as TJSonObject;
    end
    else
    begin
      raise Exception.CreateFmt('Error al obtener información del modelo: %d - %s', [LResponse.StatusCode, LResponse.ContentAsString]);
    end;

  finally
    LJsonObject.Free;
    LBodyStream.Free;
  end;
end;

{ TAiOlamalEmbeddings }

constructor TAiOllamaEmbeddings.Create(aOwner: TComponent);
begin
  inherited;
  ApiKey := '@OLLAMA_API_KEY';
  Url := GlAIUrl;
  FDimensions := 1024;
  FModel := 'snowflake-arctic-embed';

end;

{ TOllEmbeddings }

{ modelos disponibles en Ollama a mayo 2024 Library https://ollama.com/library
  Model := 'mxbai-embed-large'; //Vector[1024]
  Model := 'nomic-embed-text'; // Vector[768]
  Model := 'all-minilm';      //Vector[384]
  Model := 'snowflake-arctic-embed'; //Vector[1024]    //Esta es la mejor versión a mayo/2024

  Url para llamado http://IPOLLAMASERVER:11434/
}

function TAiOllamaEmbeddings.CreateEmbedding(aInput, aUser: String; aDimensions: Integer; aModel, aEncodingFormat: String): TAiEmbeddingData;
Var
  Client: TNetHTTPClient;
  Headers: TNetHeaders;
  JObj: TJSonObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
begin

  If aModel = '' then
    aModel := FModel;

  If aDimensions <= 0 then
    aDimensions := FDimensions;

  Client := TNetHTTPClient.Create(Nil);
{$IF CompilerVersion >= 35}
  Client.SynchronizeEvents := False;
{$ENDIF}
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := Url + 'api/embeddings';
  JObj := TJSonObject.Create;

  Try
    JObj.AddPair('prompt', aInput);
    JObj.AddPair('model', aModel);
    JObj.AddPair('user', aUser);
    JObj.AddPair('dimensions', aDimensions);
    JObj.AddPair('encoding_format', aEncodingFormat);

    St.WriteString(JObj.ToJSON);
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      JObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      ParseEmbedding(JObj);
      Result := Self.Data;
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
    St.Free;
    Response.Free;
    JObj.Free;
  End;
end;

destructor TAiOllamaEmbeddings.Destroy;
begin

  inherited;
end;

procedure TAiOllamaEmbeddings.ParseEmbedding(JObj: TJSonObject);
Var
  JArr: TJSonArray;
  J: Integer;
  Valor: Double;
begin
  // A diferencia de openAi el embedding es uno solo y no un arreglo

  JArr := JObj.GetValue<TJSonArray>('embedding');
  J := JArr.Count;
  SetLength(FData, J);

  // FillChar(FData, Length(FData) * SizeOf(Double), 0);

  For J := 0 to JArr.Count - 1 do
  Begin
    Valor := JArr.Items[J].GetValue<Double>;
    FData[J] := Valor;
  End;

  // FData := Emb;
end;

Initialization

TAiChatFactory.Instance.RegisterDriver(TAiOllamaChat);

end.
