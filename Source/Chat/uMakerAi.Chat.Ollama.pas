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
// Nombre: Gustavo Enríquez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.Chat.Ollama;

{$INCLUDE ../CompilerDirectives.inc}

interface

uses
  // FPC: Unidades estándar de FPC sin prefijo System
  {$IFDEF FPC}
  Classes, SysUtils, StrUtils, Generics.Collections, Types, Variants, SyncObjs, Math,
  {$ELSE}
  // Delphi: Unidades con namespace System, incluye Net/HTTP/JSON/REST nativos
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Threading,
  System.Variants, System.Net.Mime, System.IOUtils, System.Generics.Collections,
  System.NetEncoding,
  System.JSON, System.StrUtils, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent,
  REST.JSON, REST.Types, REST.Client,
  {$ENDIF}
  uMakerAi.ParamsRegistry, uMakerAi.Chat, uMakerAi.Core, uMakerAi.Embeddings, uMakerAi.Utils.CodeExtractor, uMakerAi.Embeddings.Core, uMakerAi.Chat.Messages,
  uJsonHelper, uHttpHelper, uSysUtilsHelper, uBase64Helper, uThreadingHelper, uRttiHelper;

type

  TAiOllamaChat = Class(TAiChat)
  Private
    Fkeep_alive: String;
    FTmpToolCallsStr: string;
    procedure Setkeep_alive(const Value: String);
  Protected
    Procedure OnInternalReceiveData(const Sender: TObject; {$IFDEF FPC}const{$ENDIF} AContentLength, AReadCount: Int64; var AAbort: Boolean); Override;
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

  if not LResponseObj.TryGetValue('message', LMessageObj) then
    Exit;

  if not LMessageObj.TryGetValue('tool_calls', LToolCallsArray) then
    Exit;

  for LItem in LToolCallsArray do
  begin
    if not(LItem is TJSonObject) then
      Continue;

    LToolCallObj := LItem as TJSonObject;

    if LToolCallObj.TryGetValue('function', LFunctionObj) then
    begin
      LToolCall := TAiToolsFunction.Create;
      try
        // Ollama ahora sí incluye un 'id', pero lo generamos como fallback por si acaso.
        LToolCall.Id := LToolCallObj.GetValueAsString('id', 'call_' + TGuid.NewGuid.ToString);
        LToolCall.Name := LFunctionObj.GetValueAsString('name');
        LToolCall.Tipo := 'function';

        if LFunctionObj.TryGetValue('arguments', LArgumentsObj) then
        begin
          LToolCall.Arguments := LArgumentsObj.Format;
          for LPair in LArgumentsObj do
          begin
            LToolCall.Params.Values[GetJSONStringValue(LPair.JsonString)] := GetJSONStringValue(LPair.JsonValue);
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

      JObj.AddPair('tool_calls', TJSONObject.ParseJSONValue(Msg.Tool_calls) as TJSONArray);
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
  Client.ConfigureForAsync;
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := EndPointUrl + 'api/tags';

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + aApiKey)];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      jRes := TJSonObject(TJSONObject.ParseJSONValue(Res.ContentAsString));
      If jRes.TryGetValue('models', JArr) then
      Begin
        For JVal in JArr do
        Begin
          sModel := TJSONObject(JVal).GetValueAsString('name');
          If sModel <> '' then
            Result.Add(sModel);
        End;
      End;

      // Agregar modelos personalizados
      CustomModels := TAiChatFactory.Instance.GetCustomModels(Self.GetDriverName);

      for I := Low(CustomModels) to High(CustomModels) do
      begin
        // TStringsHelper.Contains definido en uSysUtilsHelper para FPC
        // Delphi moderno lo tiene nativo
        if not Result.Contains(CustomModels[I]) then
          Result.Add(CustomModels[I]);
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
  sShema: String;
  JSchema: TJSONValue;

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
    AJSONObject.AddPair('stream', CreateJSONBool(Self.Asynchronous));

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
          sShema := StringReplace(JsonSchema.Text,'\n',' ',[rfReplaceAll]);
          JSchema := TJSONObject.ParseJSONValue(sShema);

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
      // Reusing JSchema as temp TJSONValue
      JArr := TJSONObject.ParseAsArray(GetTools(TToolFormat.tfOpenAi).Text);
      If Assigned(JArr) then
        AJSONObject.AddPair('tools', JArr);
    End;

    // --- PARÁMETROS "OPTIONS" (Diferencia clave con OpenAI) ---
    // Ollama requiere encapsular estos parámetros dentro de 'options'

    if Temperature > 0 then
      jOptions.AddPair('temperature', CreateJSONNumber(Temperature));

    // OJO: Ollama usa 'num_predict' en lugar de 'max_tokens'
    if Max_tokens > 0 then
      jOptions.AddPair('num_predict', CreateJSONNumber(Max_tokens));

    If Top_p <> 0 then
      jOptions.AddPair('top_p', CreateJSONNumber(Top_p));

    If Frequency_penalty <> 0 then
      jOptions.AddPair('frequency_penalty', CreateJSONNumber(Frequency_penalty));

    If Presence_penalty <> 0 then
      jOptions.AddPair('presence_penalty', CreateJSONNumber(Presence_penalty));

    If Seed > 0 then
      jOptions.AddPair('seed', CreateJSONNumber(Seed));

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
    Result := AJSONObject.ToJSONString;

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
  S: String;
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
        S := Res.ContentAsString;

        LogDebug('-Resultado-');
        LogDebug(S);

        JObj := TJSonObject(TJSONObject.ParseJSONValue(Res.ContentAsString));
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

procedure TAiOllamaChat.OnInternalReceiveData(const Sender: TObject; {$IFDEF FPC}const{$ENDIF} AContentLength, AReadCount: Int64; var AAbort: Boolean);
var
  LJsonObject, LMessageObj: TJSonObject;
  LToolCallsArray: TJSonArray;
  LChunkStr, LJsonLine, LRole, LContentPart, LThinkingPart: string;
  LDone: Boolean;
  LFinalMsg: TAiChatMessage;
  LStreamFinished: Boolean;

  procedure ProcessFinalJsonObject(AJson: TJSonObject);
  var
    LMsgObj: TJSonObject;
  begin
    LStreamFinished := True;
    FBusy := False;

    // Si capturamos tool_calls en fragmentos anteriores pero el JSON final no los tiene,
    // los re-inyectamos para que ParseChat los procese.
    if not FTmpToolCallsStr.IsEmpty then
    begin
      if not AJson.TryGetValue('message', LMsgObj) then
      begin
        LMsgObj := TJSonObject.Create;
        AJson.AddPair('message', LMsgObj);
      end;

      if LMsgObj.GetValue('tool_calls') = nil then
      begin
        LMsgObj.AddPair('tool_calls', TJSonArray(TJSONObject.ParseJSONValue(FTmpToolCallsStr)));
      end;
    end;

    LFinalMsg := TAiChatMessage.Create('', 'assistant');
    try
      ParseChat(AJson, LFinalMsg);
    except
      on E: Exception do
      begin
        LFinalMsg.Free;
        raise;
      end;
    end;

    // Limpieza de buffers para la siguiente petición
    FLastContent := '';
    FTmpResponseText := '';
    FTmpToolCallsStr := '';
  end;

begin
  if not Self.Asynchronous then
    Exit;

  AAbort := FAbort;
  if FAbort then
  begin
    FBusy := False;
    FTmpToolCallsStr := '';
    if Assigned(FOnReceiveDataEnd) then
      FOnReceiveDataEnd(Self, nil, nil, 'system', 'abort');
    Exit;
  end;

  try
    // 1. Acumular el nuevo chunk de datos
    LChunkStr := FResponse.DataString;
    FResponse.Clear;
    FTmpResponseText := FTmpResponseText + LChunkStr;
    LStreamFinished := False;

    // --- Bucle principal para procesar líneas (Ollama envía un JSON por línea) ---
    while Pos(#10, FTmpResponseText) > 0 do
    begin
      LJsonLine := Copy(FTmpResponseText, 1, Pos(#10, FTmpResponseText) - 1);
      Delete(FTmpResponseText, 1, Pos(#10, FTmpResponseText));

      if LJsonLine.Trim.IsEmpty then
        Continue;

      LJsonObject := TJSONObject.ParseJSONValue(LJsonLine.Trim) as TJSonObject;
      if not Assigned(LJsonObject) then
        Continue;

      try
        LDone := LJsonObject.GetValueAsBoolean('done');

        if not LDone then // --- FRAGMENTO INTERMEDIO ---
        begin
          if LJsonObject.TryGetValue('message', LMessageObj) then
          begin
            LRole := LMessageObj.GetValueAsString('role', 'assistant');

            // 1. CAPTURAR TOOL CALLS (Fundamental para Ollama)
            if LMessageObj.TryGetValue('tool_calls', LToolCallsArray) then
            begin
              // Guardamos el string de las herramientas. Ollama suele enviarlas
              // completas en un solo chunk, pero lo sobreescribimos por seguridad.
              FTmpToolCallsStr := LToolCallsArray.Format;
              DoStateChange(acsToolCalling, 'Tool call detected in stream...');
            end;

            // 2. PROCESAR CONTENIDO
            if LMessageObj.TryGetValue('content', LContentPart) and (LContentPart <> '') then
            begin
              LContentPart := StringReplace(LContentPart, #$A, sLineBreak, [rfReplaceAll]);
              FLastContent := FLastContent + LContentPart;
              if Assigned(FOnReceiveDataEvent) then
                FOnReceiveDataEvent(Self, nil, LJsonObject, LRole, LContentPart);
            end;

            // 3. PROCESAR RAZONAMIENTO (Thinking)
            if LMessageObj.TryGetValue('thinking', LThinkingPart) and (LThinkingPart <> '') then
            begin
              LThinkingPart := StringReplace(LThinkingPart, #$A, sLineBreak, [rfReplaceAll]);
              if Assigned(OnReceiveThinking) then
                OnReceiveThinking(Self, nil, LJsonObject, LRole, LThinkingPart);
            end;
          end;
        end
        else // --- FRAGMENTO FINAL ---
        begin
          ProcessFinalJsonObject(LJsonObject);
          Break;
        end;
      finally
        LJsonObject.Free;
      end;
    end;

    // --- MANEJO DE FRAGMENTO FINAL (sin salto de línea) ---
    if (not LStreamFinished) and (FTmpResponseText.Trim <> '') then
    begin
      LJsonObject := TJSONObject.ParseJSONValue(FTmpResponseText.Trim) as TJSonObject;
      if Assigned(LJsonObject) then
      try
        if LJsonObject.GetValueAsBoolean('done') then
          ProcessFinalJsonObject(LJsonObject);
      finally
        LJsonObject.Free;
      end;
    end;

  except
    on E: Exception do
    begin
      FBusy := False;
      DoError('Error en OnInternalReceiveData (Ollama Stream): ' + E.Message);
    end;
  end;
end;


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

  // 1. EXTRAER METADATOS Y ESTADÍSTICAS
  LModel := JObj.GetValueAsString('model');
  // Ollama usa nombres específicos para los tokens
  LPromptTokens := JObj.GetValueAsInteger('prompt_eval_count');
  LEvalTokens := JObj.GetValueAsInteger('eval_count');

  // Actualizar contadores globales del componente
  Self.Prompt_tokens := Self.Prompt_tokens + LPromptTokens;
  Self.Completion_tokens := Self.Completion_tokens + LEvalTokens;
  Self.Total_tokens := Self.Total_tokens + LPromptTokens + LEvalTokens;

  // 2. VALIDAR LA EXISTENCIA DE "MESSAGE"
  if not JObj.TryGetValue('message', LMessageObj) then
  begin
    // Si no hay mensaje pero el JSON indica que terminó, disparamos el evento de fin
    if JObj.GetValueAsBoolean('done') then
    begin
      DoStateChange(acsFinished, 'Done');
      if Assigned(FOnReceiveDataEnd) then
        FOnReceiveDataEnd(Self, ResMsg, JObj, 'assistant', FLastContent);
    end;
    Exit;
  end;

  // 3. EXTRAER CONTENIDO DEL MENSAJE
  LRole := LMessageObj.GetValueAsString('role', 'assistant');
  LReasoning := LMessageObj.GetValueAsString('thinking', '');
  LContent := LMessageObj.GetValueAsString('content', '');

  // Sincronizar el contenido acumulado durante el streaming (FLastContent) con el ResMsg
  if (FLastContent <> '') then
    ResMsg.Content := FLastContent
  else
  begin
    ResMsg.Content := LContent;
    FLastContent := LContent;
  end;

  // Configurar propiedades del mensaje de respuesta
  ResMsg.Prompt := ResMsg.Content;
  ResMsg.Role := LRole;
  ResMsg.Model := LModel;
  ResMsg.ReasoningContent := LReasoning;
  ResMsg.Prompt_tokens := LPromptTokens;
  ResMsg.Completion_tokens := LEvalTokens;
  ResMsg.Total_tokens := LPromptTokens + LEvalTokens;

  LAskMsg := GetLastMessage;

  // 4. LÓGICA DE LLAMADO A FUNCIONES (TOOLS)
  // Verificamos si Ollama nos ha devuelto tool_calls
  if LMessageObj.TryGetValue('tool_calls', LToolCallsArray) and (LToolCallsArray.Count > 0) then
  begin
    // --- CASO A: El modelo solicita ejecutar herramientas ---

    // A.1 Guardamos el mensaje del asistente (la petición de tool) en el historial
    LHistoryToolMsg := TAiChatMessage.Create(ResMsg.Content, LRole);
    LHistoryToolMsg.Tool_calls := LToolCallsArray.ToJSON;
    LHistoryToolMsg.Id := FMessages.Count + 1;
    FMessages.Add(LHistoryToolMsg);

    DoStateChange(acsToolExecuting, 'Executing local tools...');

    // A.2 Extraer las definiciones de las funciones
    LChoicesSimulado := TJSonArray.Create;
    LFunciones := nil;
    try
      // Simulamos una estructura compatible con ExtractToolCallFromJson
      LChoicesSimulado.Add(JObj.Clone as TJSonObject);
      LFunciones := ExtractToolCallFromJson(LChoicesSimulado);

      if (LFunciones <> nil) and (LFunciones.Count > 0) then
      begin
        // A.3 Ejecución en paralelo de las funciones encontradas
        NumTasks := LFunciones.Count;
        SetLength(TaskList, NumTasks);
        I := 0;

        for Clave in LFunciones.Keys do
        begin
          LToolCall := LFunciones[Clave];
          LToolCall.ResMsg := ResMsg;
          LToolCall.AskMsg := LAskMsg;

          TaskList[I] := TTask.Create(
            TProc(procedure
            var
              CapturaTool: TAiToolsFunction;
              LErrorMsg: string;
            begin
              CapturaTool := LToolCall;
              try
                DoCallFunction(CapturaTool);
              except
                on E: Exception do
                begin
                  // [DIFERENCIA FPC/DELPHI - Procedimientos anónimos anidados]
                  // FPC 3.2/3.3 NO soportan procedimientos anónimos anidados.
                  // Delphi: TThread.Queue ejecuta DoError en hilo principal (thread-safe para UI).
                  // FPC: Ejecuta DoError directamente en hilo del TTask.
                  // DoError de TAiChat solo guarda FLastError y dispara OnError,
                  // lo cual es seguro si el handler no modifica UI directamente.
                  // Unified Thread-Safety for FPC/Delphi
                  LErrorMsg := 'Function Execution Error: ' + CapturaTool.Name + ' - ' + E.Message;
                  // FPC Syntax Fix: Calling DoError directly to avoid anonymous method issues in Queue
                  DoError(LErrorMsg);
                end;
              end;
            end));
          TaskList[I].Start;
          Inc(I);
        end;

        // Esperar a que todas las funciones terminen (bloqueo controlado)
        TTask.WaitForAll(TaskList);

        // A.4 Añadir los resultados de las funciones (role: tool) al historial
        for LToolCall in LFunciones.Values do
        begin
          LToolMsg := TAiChatMessage.Create(LToolCall.Response, 'tool', LToolCall.Id, LToolCall.Name);
          LToolMsg.Id := FMessages.Count + 1;
          FMessages.Add(LToolMsg);
        end;

        // A.5 Re-ejecutar el Run para que el modelo analice los resultados de las herramientas
        // Limpiamos el ResMsg para recibir la respuesta final
        ResMsg.Content := '';
        ResMsg.Tool_calls := '';
        FLastContent := '';
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
    // --- CASO B: Respuesta de texto normal o final de cadena ---

    // B.1 Extracción automática de bloques de código si se solicita
    if (tfc_ExtracttextFile in NativeOutputFiles) and (ResMsg.Content <> '') then
    begin
      Code := TMarkdownCodeExtractor.Create;
      try
        CodeFiles := Code.ExtractCodeFiles(ResMsg.Content);
        for CodeFile in CodeFiles do
        begin
          St := TStringStream.Create(CodeFile.Code, TEncoding.UTF8);
          try
            St.Position := 0;
            MF := TAiMediaFile.Create;
            // Cargamos el código extraído como un archivo adjunto al mensaje
            MF.LoadFromStream('file.' + CodeFile.FileType, St as TMemoryStream);
            ResMsg.MediaFiles.Add(MF);
          finally
            St.Free;
          end;
        end;
      finally
        Code.Free;
      end;
    end;

    // B.2 Notificar el procesamiento de la respuesta (Hooks externos)
    DoProcessResponse(LAskMsg, ResMsg, FLastContent);

    // B.3 Gestión del historial (Evitar Doble Add en Asíncrono)
    // En síncrono, TAiChat.Run añade el mensaje al finalizar.
    // En asíncrono, como el Run ya salió, debemos añadirlo aquí.
    if Self.Asynchronous and (FMessages.IndexOf(ResMsg) = -1) then
    begin
      ResMsg.Id := FMessages.Count + 1;
      FMessages.Add(ResMsg);
    end;

    // B.4 Finalización y notificación a la UI
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
    LJsonObject.AddPair('stream', CreateJSONBool(True)); // Siempre en stream para progreso
    LBodyStream.WriteString(LJsonObject.ToJSON);
    LBodyStream.Position := 0;

    FClient.ContentType := 'application/json';
    LResponse := FClient.Post(LUrl, LBodyStream, LResponseStream);

    if LResponse.StatusCode <> 200 then
      raise Exception.CreateFmt('Error al crear el modelo: %d - %s', [LResponse.StatusCode, LResponse.ContentAsString]);

    // Procesar la respuesta en stream (linea por linea)
    LResponseStream.Position := 0;
    LJsonLines := CompatSplit(LResponseStream.DataString, [#10], TStringSplitOptions.ExcludeEmpty);

    for LLine in LJsonLines do
    begin
      if Assigned(OnProgressEvent) then
      begin
        LStatusObj := TJSONObject.ParseJSONValue(LLine) as TJSonObject;
        if Assigned(LStatusObj) then
          try
            LStatus := LStatusObj.GetValueAsString('status');
            LCompleted := 0;
            LTotal := 0;
            LStatusObj.TryGetValue('completed', LCompleted);
            LStatusObj.TryGetValue('total', LTotal);
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
    LJsonObject.AddPair('stream', CreateJSONBool(True)); // Siempre en stream para progreso
    LBodyStream.WriteString(LJsonObject.ToJSON);
    LBodyStream.Position := 0;

    FClient.ContentType := 'application/json';
    // Hacemos la llamada síncrona, pero Ollama devuelve el stream completo de una vez
    LResponse := FClient.Post(LUrl, LBodyStream, LResponseStream);

    if LResponse.StatusCode <> 200 then
      raise Exception.CreateFmt('Error al descargar el modelo: %d - %s', [LResponse.StatusCode, LResponse.ContentAsString]);

    // Procesar la respuesta en stream (linea por linea)
    LResponseStream.Position := 0;
    LJsonLines := CompatSplit(LResponseStream.DataString, [#10], TStringSplitOptions.ExcludeEmpty);

    for LLine in LJsonLines do
    begin
      // Si el evento de progreso está asignado, lo disparamos
      if Assigned(OnProgressEvent) then
      begin
        LStatusObj := TJSONObject.ParseJSONValue(LLine) as TJSonObject;
        if Assigned(LStatusObj) then
          try
            LStatus := LStatusObj.GetValueAsString('status');
            LCompleted := 0;
            LTotal := 0;
            // TryGetValue es más seguro si los campos no siempre están presentes
            LStatusObj.TryGetValue('completed', LCompleted);
            LStatusObj.TryGetValue('total', LTotal);
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
  //Result := nil;
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
      Result := TJSONObject.ParseJSONValue(LResponse.ContentAsString) as TJSonObject;
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
  Client.ConfigureForAsync;
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
      JObj := TJSonObject(TJSONObject.ParseJSONValue(Res.ContentAsString));
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

  JArr := JObj.GetValueAsArray('embedding');
  J := JArr.Count;
  SetLength(FData, J);

  // FillChar(FData, Length(FData) * SizeOf(Double), 0);

  For J := 0 to JArr.Count - 1 do
  Begin
    Valor := (JArr.Items[J] as TJSONNumber).AsDouble;
    FData[J] := Valor;
  End;

  // FData := Emb;
end;

Initialization

TAiChatFactory.Instance.RegisterDriver(TAiOllamaChat);

end.
