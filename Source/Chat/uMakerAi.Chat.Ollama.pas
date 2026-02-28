// MIT License
//
// Copyright (c) 2024-2026 Gustavo Enriquez
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
// - Email: gustavoeenriquez@gmail.com
// - GitHub: https://github.com/gustavoeenriquez/
//
// --------- FPC PORT --------------------
// Diferencias respecto al driver Delphi:
//   - URL: Url + 'api/chat'  (no /v1/chat/completions)
//   - Params en objeto 'options': temperature, num_predict (no max_tokens), etc.
//   - GetMessages: campo 'images' con base64 puro, sin prefijo data:
//   - ParseChat: prompt_eval_count/eval_count (no usage.prompt/completion_tokens)
//   - ProcessSSELine: JSON puro por linea (sin prefijo 'data: ')
//   - TFPHTTPClient creado por peticion (no FClient compartido)
//   - TTask.Create/WaitForAll -> tool calls secuenciales (Fase 2)
//   - TPath.Combine -> concatenacion manual de strings

unit uMakerAi.Chat.Ollama;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fpjson, jsonparser,
  fphttpclient,
  uMakerAi.Chat,
  uMakerAi.Core,
  uMakerAi.Chat.Messages,
  uMakerAi.Utils.CodeExtractor,
  UMakerAi.ParamsRegistry;

const
  GlOllamaUrl = 'http://localhost:11434/';

type

  TAiOllamaChat = class(TAiChat)
  private
    FKeepAlive      : string;
    FTmpToolCallsStr: string;
    procedure SetKeepAlive(const Value: string);
    procedure ProcessFinalOllamaJson(AJson: TJSONObject);
  protected
    procedure ProcessSSELine(const ALine: string); override;
    function  InitChatCompletions: string; override;
    function  InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): string; override;
    procedure ParseChat(jObj: TJSONObject; ResMsg: TAiChatMessage); override;
    function  ExtractToolCallFromJson(jChoices: TJSONArray): TAiToolsFunctions; override;
  public
    constructor Create(Sender: TComponent); override;
    destructor  Destroy; override;
    function    GetMessages: TJSONArray; override;
    class function  GetModels(aApiKey, aUrl: string): TStringList; override;
    class function  GetDriverName: string; override;
    class procedure RegisterDefaultParams(Params: TStrings); override;
    class function  CreateInstance(Sender: TComponent): TAiChat; override;
    // Gestion de modelos locales Ollama
    procedure PullModel(const aModelName: string);
    procedure CreateModel(const aNewModelName, aModelfileContent: string);
    function  ShowModelInfo(const aModelName: string): TJSONObject;
    procedure CopyModel(const aSourceModel, aDestinationModel: string);
    procedure DeleteModel(const aModelName: string);
  published
    property KeepAlive: string read FKeepAlive write SetKeepAlive;
  end;

implementation

// ===========================================================================
//  TAiOllamaChat — constructor / destructor / factory
// ===========================================================================

constructor TAiOllamaChat.Create(Sender: TComponent);
begin
  inherited;
  ApiKey     := '@OLLAMA_API_KEY';
  Model      := 'llama3';
  Url        := GlOllamaUrl;
  FKeepAlive := '1m';
  FTmpToolCallsStr := '';
end;

destructor TAiOllamaChat.Destroy;
begin
  inherited;
end;

class function TAiOllamaChat.GetDriverName: string;
begin
  Result := 'Ollama';
end;

class procedure TAiOllamaChat.RegisterDefaultParams(Params: TStrings);
begin
  Params.Clear;
  Params.Add('ApiKey=@OLLAMA_API_KEY');
  Params.Add('Model=llama3');
  Params.Add('MaxTokens=4096');
  Params.Add('URL=' + GlOllamaUrl);
end;

class function TAiOllamaChat.CreateInstance(Sender: TComponent): TAiChat;
begin
  Result := TAiOllamaChat.Create(Sender);
end;

procedure TAiOllamaChat.SetKeepAlive(const Value: string);
begin
  FKeepAlive := Value;
end;

// ===========================================================================
//  GetMessages — serializa historial al formato nativo Ollama
//  Diferencias vs OpenAI:
//    - campo 'images' con array de base64 puro (sin prefijo data:image/...)
//    - campos 'tool_call_id' y 'name' para mensajes de herramientas
//    - sin system message extra (ya esta en FMessages si se configuro)
// ===========================================================================

function TAiOllamaChat.GetMessages: TJSONArray;
var
  I, J   : Integer;
  Msg    : TAiChatMessage;
  JObj   : TJSONObject;
  jImages: TJSONArray;
begin
  Result := TJSONArray.Create;

  for I := 0 to Messages.Count - 1 do
  begin
    Msg  := Messages.Items[I];
    JObj := TJSONObject.Create;

    if Msg.ToolCallId <> '' then
      JObj.Add('tool_call_id', Msg.ToolCallId);

    if Msg.FunctionName <> '' then
      JObj.Add('name', Msg.FunctionName);

    JObj.Add('role',    Msg.Role);
    JObj.Add('content', Msg.Prompt);

    // Imagenes — Ollama acepta base64 puro (sin prefijo data:image/...)
    if Msg.MediaFiles.Count > 0 then
    begin
      jImages := TJSONArray.Create;
      JObj.Add('images', jImages);
      for J := 0 to Msg.MediaFiles.Count - 1 do
        jImages.Add(Msg.MediaFiles[J].Base64);
    end;

    // Tool calls previas del historial
    if Msg.Tool_calls <> '' then
      JObj.Add('tool_calls', TJSONArray(GetJSON(Msg.Tool_calls)));

    Result.Add(JObj);
  end;
end;

// ===========================================================================
//  InitChatCompletions — /api/chat con parametros en objeto 'options'
//  Diferencias vs OpenAI:
//    - 'num_predict' en lugar de 'max_tokens'
//    - params de sampling encapsulados en 'options'
//    - 'format' en lugar de 'response_format'
//    - 'keep_alive' para controlar tiempo de vida del modelo en GPU
// ===========================================================================

function TAiOllamaChat.InitChatCompletions: string;
var
  AJSONObject, jOptions: TJSONObject;
  JArr, JStop         : TJSONArray;
  Lista               : TStringList;
  I                   : Integer;
  LModel, SSchema     : string;
  JSchema             : TJSONData;
begin
  if User = '' then
    User := 'user';

  LModel := Model;
  if LModel = '' then
    LModel := 'llama3';

  AJSONObject := TJSONObject.Create;
  jOptions    := TJSONObject.Create;
  Lista       := TStringList.Create;
  try
    AJSONObject.Add('model',    LModel);
    AJSONObject.Add('messages', GetMessages);
    AJSONObject.Add('stream',   Asynchronous);  // True=streaming, False=respuesta unica

    if FKeepAlive <> '' then
      AJSONObject.Add('keep_alive', FKeepAlive);

    // Formato de salida estructurado
    if Response_format = tiaChatRfJsonSchema then
    begin
      if JsonSchema.Text <> '' then
        try
          SSchema := StringReplace(JsonSchema.Text, '\n', ' ', [rfReplaceAll]);
          JSchema := GetJSON(SSchema);
          if Assigned(JSchema) then
          begin
            if JSchema is TJSONObject then
              AJSONObject.Add('format', TJSONObject(JSchema))
            else
              JSchema.Free;
          end;
        except
          // ignorar error de parseo del schema — continuar sin formato
        end;
    end
    else if Response_format = tiaChatRfJson then
      AJSONObject.Add('format', 'json');

    // Tools (mismo formato OpenAI)
    if Tool_Active and (Trim(GetToolsStr(TToolFormat.tfOpenAi)) <> '') then
    begin
      JArr := TJSONArray(GetJSON(GetToolsStr(TToolFormat.tfOpenAi)));
      if Assigned(JArr) then
        AJSONObject.Add('tools', JArr);
    end;

    // Opciones de sampling — encapsuladas en 'options' (diferencia clave vs OpenAI)
    if Temperature > 0 then
      jOptions.Add('temperature', Temperature);

    if Max_tokens > 0 then
      jOptions.Add('num_predict', Max_tokens);  // Ollama: num_predict, no max_tokens

    if Top_p <> 0 then
      jOptions.Add('top_p', Top_p);

    if Frequency_penalty <> 0 then
      jOptions.Add('frequency_penalty', Frequency_penalty);

    if Presence_penalty <> 0 then
      jOptions.Add('presence_penalty', Presence_penalty);

    if Seed > 0 then
      jOptions.Add('seed', Seed);

    Lista.CommaText := Stop;
    if Lista.Count > 0 then
    begin
      JStop := TJSONArray.Create;
      for I := 0 to Lista.Count - 1 do
        JStop.Add(Lista[I]);
      jOptions.Add('stop', JStop);
    end;

    if jOptions.Count > 0 then
      AJSONObject.Add('options', jOptions)
    else
      jOptions.Free;  // no se anadio al padre — liberar manualmente

    Result := AJSONObject.AsJSON;
  finally
    AJSONObject.Free;
    Lista.Free;
  end;
end;

// ===========================================================================
//  InternalRunCompletions — POST a /api/chat (no /v1/chat/completions)
// ===========================================================================

function TAiOllamaChat.InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): string;
var
  ABody     : string;
  sUrl      : string;
  BodyStream: TStringStream;
  RespStream: TStringStream;
  Client    : TFPHTTPClient;
  JObj      : TJSONObject;
begin
  FBusy    := True;
  FAbort   := False;
  FLastError   := '';
  FLastContent := '';
  FLastPrompt  := '';
  FTmpToolCallsStr := '';

  DoStateChange(acsConnecting, 'Sending request...');

  sUrl  := Url + 'api/chat';
  ABody := InitChatCompletions;

  LogDebug('-Peticion Ollama-');
  LogDebug(ABody);

  if Asynchronous then
  begin
    // Modo async: TAiHttpThread llama OnSSELine -> ProcessSSELine por cada linea
    StartHttpThread(sUrl, ABody,
      ['Authorization', 'Bearer ' + ApiKey,
       'Content-Type', 'application/json']);
    Result := '';
    Exit;
  end;

  // Modo sincrono
  BodyStream := TStringStream.Create(ABody);
  RespStream := TStringStream.Create('');
  Client     := TFPHTTPClient.Create(nil);
  try
    Client.AddHeader('Authorization', 'Bearer ' + ApiKey);
    Client.AddHeader('Content-Type', 'application/json');
    Client.RequestBody := BodyStream;
    Client.HTTPMethod('POST', sUrl, RespStream, [200]);

    if Client.ResponseStatusCode = 200 then
    begin
      LogDebug('-Resultado Ollama-');
      LogDebug(RespStream.DataString);

      JObj := TJSONObject(GetJSON(RespStream.DataString));
      try
        FBusy := False;
        ParseChat(JObj, ResMsg);
        Result := FLastContent;
      finally
        FreeAndNil(JObj);
      end;
    end
    else
      raise Exception.CreateFmt('Error Received: %d, %s',
        [Client.ResponseStatusCode, RespStream.DataString]);
  finally
    Client.Free;
    BodyStream.Free;
    RespStream.Free;
  end;
end;

// ===========================================================================
//  ProcessSSELine — cada linea de Ollama es un JSON completo (sin 'data: ')
//  A diferencia de OpenAI SSE, Ollama usa NDJSON:
//    {"model":"llama3","message":{"role":"assistant","content":"Hola"},"done":false}
//    {"model":"llama3","done":true,"prompt_eval_count":10,"eval_count":5,...}
// ===========================================================================

procedure TAiOllamaChat.ProcessSSELine(const ALine: string);
var
  LJsonObject  : TJSONObject;
  LMessageObj  : TJSONObject;
  LToolCallsArr: TJSONArray;
  LRole        : string;
  LContent     : string;
  LThinking    : string;
  LDone        : Boolean;
begin
  if Trim(ALine) = '' then
    Exit;

  if FAbort then
  begin
    FBusy := False;
    FTmpToolCallsStr := '';
    DoDataEnd(nil, 'system', 'abort', nil);
    Exit;
  end;

  try
    LJsonObject := TJSONObject(GetJSON(Trim(ALine)));
  except
    Exit;  // linea JSON invalida — ignorar
  end;

  if not Assigned(LJsonObject) then
    Exit;

  try
    LDone := JGetBool(LJsonObject, 'done', False);

    if not LDone then
    begin
      // Fragmento intermedio — extraer contenido del campo 'message'
      LMessageObj := JGetObj(LJsonObject, 'message');
      if Assigned(LMessageObj) then
      begin
        LRole := JGetStr(LMessageObj, 'role', 'assistant');

        // Tool calls en streaming — guardar para inyectar en el JSON final
        LToolCallsArr := JGetArr(LMessageObj, 'tool_calls');
        if Assigned(LToolCallsArr) then
        begin
          FTmpToolCallsStr := LToolCallsArr.AsJSON;
          DoStateChange(acsToolCalling, 'Tool call detected in stream...');
        end;

        // Contenido de texto
        LContent := JGetStr(LMessageObj, 'content', '');
        if LContent <> '' then
        begin
          LContent     := StringReplace(LContent, #10, LineEnding, [rfReplaceAll]);
          FLastContent := FLastContent + LContent;
          DoData(nil, LRole, LContent, LJsonObject);
        end;

        // Razonamiento (thinking — modelos como DeepSeek-R1 via Ollama)
        LThinking := JGetStr(LMessageObj, 'thinking', '');
        if LThinking <> '' then
        begin
          LThinking := StringReplace(LThinking, #10, LineEnding, [rfReplaceAll]);
          DoThinking(nil, LRole, LThinking, LJsonObject);
        end;
      end;
    end
    else
    begin
      // Fragmento final (done=true) — contiene tokens de uso y fin de stream
      ProcessFinalOllamaJson(LJsonObject);
    end;
  finally
    LJsonObject.Free;
  end;
end;

// ---------------------------------------------------------------------------
//  ProcessFinalOllamaJson — procesa el chunk final Ollama (done=true)
// ---------------------------------------------------------------------------

procedure TAiOllamaChat.ProcessFinalOllamaJson(AJson: TJSONObject);
var
  LMsgObj  : TJSONObject;
  LToolsArr: TJSONArray;
  LFinalMsg: TAiChatMessage;
begin
  FBusy := False;

  // Si capturamos tool_calls en chunks previos pero no estan en el JSON final,
  // inyectarlos para que ParseChat los procese correctamente
  if FTmpToolCallsStr <> '' then
  begin
    LMsgObj := JGetObj(AJson, 'message');
    if not Assigned(LMsgObj) then
    begin
      LMsgObj := TJSONObject.Create;
      AJson.Add('message', LMsgObj);
    end;

    if not Assigned(JGetArr(LMsgObj, 'tool_calls')) then
    begin
      LToolsArr := TJSONArray(GetJSON(FTmpToolCallsStr));
      if Assigned(LToolsArr) then
        LMsgObj.Add('tool_calls', LToolsArr);
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

  // Limpiar buffers para la siguiente peticion
  FTmpResponseText := '';
  FTmpToolCallsStr := '';
end;

// ===========================================================================
//  ParseChat — parsea respuesta Ollama completa (sync o final de stream async)
//  Diferencias vs OpenAI:
//    - tokens en 'prompt_eval_count' y 'eval_count' (no usage.*)
//    - 'message.thinking' para razonamiento interno
//    - tool_calls en 'message.tool_calls' directamente (no choices[].message)
// ===========================================================================

procedure TAiOllamaChat.ParseChat(jObj: TJSONObject; ResMsg: TAiChatMessage);
var
  LMessageObj     : TJSONObject;
  LToolCallsArray : TJSONArray;
  LAskMsg         : TAiChatMessage;
  LRole, LContent, LModel, LReasoning: string;
  LPromptTokens, LEvalTokens: Integer;
  LChoicesSimulado: TJSONArray;
  LFunciones      : TAiToolsFunctions;
  LToolCall       : TAiToolsFunction;
  LToolMsg        : TAiChatMessage;
  LHistoryToolMsg : TAiChatMessage;
  Clave           : string;
  Code            : TMarkdownCodeExtractor;
  CodeFiles       : TCodeFileList;
  CodeFile        : TCodeFile;
  MF              : TAiMediaFile;
  Mem             : TMemoryStream;
begin
  if not Assigned(jObj) then
    Exit;

  // 1. Estadisticas — Ollama usa nombres distintos a OpenAI
  LModel        := JGetStr(jObj, 'model', '');
  LPromptTokens := JGetInt(jObj, 'prompt_eval_count', 0);
  LEvalTokens   := JGetInt(jObj, 'eval_count', 0);

  Self.Prompt_tokens     := Self.Prompt_tokens     + LPromptTokens;
  Self.Completion_tokens := Self.Completion_tokens + LEvalTokens;
  Self.Total_tokens      := Self.Total_tokens      + LPromptTokens + LEvalTokens;

  // 2. Validar existencia de 'message'
  LMessageObj := JGetObj(jObj, 'message');
  if not Assigned(LMessageObj) then
  begin
    // Sin mensaje pero con done=true — disparar evento de fin
    if JGetBool(jObj, 'done', False) then
    begin
      DoStateChange(acsFinished, 'Done');
      DoDataEnd(ResMsg, 'assistant', FLastContent, jObj);
    end;
    Exit;
  end;

  // 3. Extraer contenido del mensaje
  LRole      := JGetStr(LMessageObj, 'role', 'assistant');
  LReasoning := JGetStr(LMessageObj, 'thinking', '');
  LContent   := JGetStr(LMessageObj, 'content', '');

  // Sincronizar contenido acumulado en streaming con ResMsg
  if FLastContent <> '' then
    ResMsg.Content := FLastContent
  else
  begin
    ResMsg.Content := LContent;
    FLastContent   := LContent;
  end;

  ResMsg.Prompt            := ResMsg.Content;
  ResMsg.Role              := LRole;
  ResMsg.Model             := LModel;
  ResMsg.ReasoningContent  := LReasoning;
  ResMsg.Prompt_tokens     := LPromptTokens;
  ResMsg.Completion_tokens := LEvalTokens;
  ResMsg.Total_tokens      := LPromptTokens + LEvalTokens;

  LAskMsg := GetLastMessage;

  // 4. Tool calls?
  LToolCallsArray := JGetArr(LMessageObj, 'tool_calls');
  if Assigned(LToolCallsArray) and (LToolCallsArray.Count > 0) then
  begin
    // A. El modelo solicita ejecutar herramientas

    // A.1 Guardar el mensaje del asistente en historial (con tool_calls serializado)
    LHistoryToolMsg := TAiChatMessage.Create(ResMsg.Content, LRole);
    LHistoryToolMsg.Tool_calls := LToolCallsArray.AsJSON;
    LHistoryToolMsg.Id := FMessages.Count + 1;
    FMessages.Add(LHistoryToolMsg);

    DoStateChange(acsToolExecuting, 'Executing local tools...');

    // A.2 Extraer definiciones de funciones usando ExtractToolCallFromJson
    LChoicesSimulado := TJSONArray.Create;
    LFunciones := nil;
    try
      // Simular estructura choices[] compatible con ExtractToolCallFromJson
      LChoicesSimulado.Add(TJSONObject(GetJSON(jObj.AsJSON)));
      LFunciones := ExtractToolCallFromJson(LChoicesSimulado);

      if Assigned(LFunciones) and (LFunciones.Count > 0) then
      begin
        // A.3 Ejecucion secuencial (Fase 2 — se puede paralelizar con TToolCallThread)
        for Clave in LFunciones.Keys do
        begin
          LToolCall := LFunciones[Clave];
          LToolCall.ResMsg := ResMsg;
          LToolCall.AskMsg := LAskMsg;
          try
            DoCallFunction(LToolCall);
          except
            on E: Exception do
              OnRequestExceptionEvent(E);
          end;
        end;

        // A.4 Agregar resultados (role: tool) al historial
        for Clave in LFunciones.Keys do
        begin
          LToolCall := LFunciones[Clave];
          LToolMsg := TAiChatMessage.Create(LToolCall.Response, 'tool',
              LToolCall.Id, LToolCall.Name);
          LToolMsg.Id := FMessages.Count + 1;
          FMessages.Add(LToolMsg);
        end;

        // A.5 Re-ejecutar para que el modelo analice resultados de herramientas
        ResMsg.Content   := '';
        ResMsg.Tool_calls := '';
        FLastContent     := '';
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
    // B. Respuesta de texto normal

    // B.1 Extraccion de bloques de codigo si se solicita
    if (tfc_ExtractTextFile in NativeOutputFiles) and (ResMsg.Content <> '') then
    begin
      Code := TMarkdownCodeExtractor.Create;
      try
        CodeFiles := Code.ExtractCodeFiles(ResMsg.Content);
        for CodeFile in CodeFiles do
        begin
          Mem := TMemoryStream.Create;
          try
            if Length(CodeFile.Code) > 0 then
              Mem.Write(CodeFile.Code[1], Length(CodeFile.Code));
            Mem.Position := 0;
            MF := TAiMediaFile.Create;
            MF.LoadFromStream('file.' + CodeFile.FileType, Mem);
            ResMsg.MediaFiles.Add(MF);
          finally
            Mem.Free;
          end;
        end;
      finally
        Code.Free;
      end;
    end;

    // B.2 Hook de procesamiento externo
    DoProcessResponse(LAskMsg, ResMsg, FLastContent);

    // B.3 En async: agregar al historial (el Run ya retorno, ParseChat debe hacerlo)
    if Asynchronous and (FMessages.IndexOf(ResMsg) = -1) then
    begin
      ResMsg.Id := FMessages.Count + 1;
      FMessages.Add(ResMsg);
    end;

    // B.4 Fin y notificacion
    DoStateChange(acsFinished, 'Done');
    DoDataEnd(ResMsg, LRole, FLastContent, jObj);
    FBusy := False;
  end;
end;

// ===========================================================================
//  ExtractToolCallFromJson — tool_calls en message.tool_calls (formato Ollama)
//  Estructura: choices[0].message.tool_calls[].{id, function:{name, arguments}}
//  Diferencia vs OpenAI: arguments puede ser TJSONObject (no string serializado)
// ===========================================================================

function TAiOllamaChat.ExtractToolCallFromJson(jChoices: TJSONArray): TAiToolsFunctions;
var
  LResponseObj, LMessageObj, LToolCallObj, LFunctionObj, LArgumentsObj: TJSONObject;
  LToolCallsArray: TJSONArray;
  LToolCall      : TAiToolsFunction;
  I, J           : Integer;
  LId            : string;
begin
  Result := TAiToolsFunctions.Create;

  if (jChoices = nil) or (jChoices.Count = 0) then
    Exit;

  if not (jChoices.Items[0] is TJSONObject) then
    Exit;

  LResponseObj := TJSONObject(jChoices.Items[0]);

  LMessageObj := JGetObj(LResponseObj, 'message');
  if not Assigned(LMessageObj) then
    Exit;

  LToolCallsArray := JGetArr(LMessageObj, 'tool_calls');
  if not Assigned(LToolCallsArray) then
    Exit;

  for I := 0 to LToolCallsArray.Count - 1 do
  begin
    if not (LToolCallsArray.Items[I] is TJSONObject) then
      Continue;

    LToolCallObj := TJSONObject(LToolCallsArray.Items[I]);

    LFunctionObj := JGetObj(LToolCallObj, 'function');
    if not Assigned(LFunctionObj) then
      Continue;

    LToolCall := TAiToolsFunction.Create;
    try
      LId := JGetStr(LToolCallObj, 'id', '');
      if LId = '' then
        LId := 'call_' + IntToStr(I);

      LToolCall.Id   := LId;
      LToolCall.Name := JGetStr(LFunctionObj, 'name', '');
      LToolCall.Tipo := 'function';

      // arguments puede ser TJSONObject (Ollama) o string JSON (OpenAI)
      LArgumentsObj := JGetObj(LFunctionObj, 'arguments');
      if Assigned(LArgumentsObj) then
      begin
        LToolCall.Arguments := LArgumentsObj.AsJSON;
        for J := 0 to LArgumentsObj.Count - 1 do
          LToolCall.Params.Values[LArgumentsObj.Names[J]] :=
              LArgumentsObj.Items[J].AsString;
      end
      else
        LToolCall.Arguments := '{}';

      Result.Add(LToolCall.Id, LToolCall);
    except
      LToolCall.Free;
      raise;
    end;
  end;
end;

// ===========================================================================
//  GetModels — usa /api/tags (no /v1/models)
// ===========================================================================

class function TAiOllamaChat.GetModels(aApiKey, aUrl: string): TStringList;
var
  Client      : TFPHTTPClient;
  RespStream  : TStringStream;
  sUrl        : string;
  jRes        : TJSONObject;
  JArr        : TJSONArray;
  I           : Integer;
  sModel      : string;
  CustomModels: TAiStringArray;
begin
  Result := TStringList.Create;

  if aUrl <> '' then
    sUrl := aUrl
  else
    sUrl := GlOllamaUrl;

  // Asegurar slash final
  if (Length(sUrl) > 0) and (sUrl[Length(sUrl)] <> '/') then
    sUrl := sUrl + '/';

  sUrl := sUrl + 'api/tags';

  Client     := TFPHTTPClient.Create(nil);
  RespStream := TStringStream.Create('');
  try
    Client.AddHeader('Authorization', 'Bearer ' + aApiKey);
    Client.HTTPMethod('GET', sUrl, RespStream, [200]);

    if Client.ResponseStatusCode = 200 then
    begin
      jRes := TJSONObject(GetJSON(RespStream.DataString));
      if Assigned(jRes) then
      try
        JArr := JGetArr(jRes, 'models');
        if Assigned(JArr) then
          for I := 0 to JArr.Count - 1 do
            if JArr.Items[I] is TJSONObject then
            begin
              sModel := JGetStr(TJSONObject(JArr.Items[I]), 'name', '');
              if sModel <> '' then
                Result.Add(sModel);
            end;

        // Agregar modelos personalizados del registro
        CustomModels := TAiChatFactory.Instance.GetCustomModels(Self.GetDriverName);
        for I := Low(CustomModels) to High(CustomModels) do
          if Result.IndexOf(CustomModels[I]) = -1 then
            Result.Add(CustomModels[I]);
      finally
        jRes.Free;
      end;
    end
    else
      raise Exception.CreateFmt('Error Received: %d, %s',
        [Client.ResponseStatusCode, RespStream.DataString]);
  finally
    Client.Free;
    RespStream.Free;
  end;
end;

// ===========================================================================
//  Gestion de modelos locales Ollama
// ===========================================================================

// Descarga un modelo del registro Ollama (streaming de progreso)
procedure TAiOllamaChat.PullModel(const aModelName: string);
var
  Client     : TFPHTTPClient;
  BodyStream : TStringStream;
  RespStream : TStringStream;
  LJObj      : TJSONObject;
  LBodyStr   : string;
  LRespStr   : string;
  P          : Integer;
  LLine      : string;
  LStatusObj : TJSONObject;
  LStatus    : string;
  LCompleted : Int64;
  LTotal     : Int64;
begin
  LJObj := TJSONObject.Create;
  try
    LJObj.Add('name',   aModelName);
    LJObj.Add('stream', True);
    LBodyStr := LJObj.AsJSON;
  finally
    LJObj.Free;
  end;

  Client     := TFPHTTPClient.Create(nil);
  BodyStream := TStringStream.Create(LBodyStr);
  RespStream := TStringStream.Create('');
  try
    Client.AddHeader('Content-Type', 'application/json');
    Client.AddHeader('Authorization', 'Bearer ' + ApiKey);
    Client.RequestBody := BodyStream;
    Client.HTTPMethod('POST', Url + 'api/pull', RespStream, [200]);

    if Client.ResponseStatusCode <> 200 then
      raise Exception.CreateFmt('Error al descargar modelo: %d - %s',
        [Client.ResponseStatusCode, RespStream.DataString]);

    // Procesar NDJSON de progreso linea por linea
    LRespStr := RespStream.DataString;
    while Length(LRespStr) > 0 do
    begin
      P := Pos(#10, LRespStr);
      if P > 0 then
      begin
        LLine    := Trim(Copy(LRespStr, 1, P - 1));
        Delete(LRespStr, 1, P);
      end
      else
      begin
        LLine    := Trim(LRespStr);
        LRespStr := '';
      end;

      if LLine = '' then
        Continue;

      try
        LStatusObj := TJSONObject(GetJSON(LLine));
      except
        Continue;
      end;

      if Assigned(LStatusObj) then
      try
        LStatus    := JGetStr(LStatusObj, 'status', '');
        LCompleted := JGetInt64(LStatusObj, 'completed', 0);
        LTotal     := JGetInt64(LStatusObj, 'total', 0);
        DoProgress(LStatus, LCompleted, LTotal);
      finally
        LStatusObj.Free;
      end;
    end;
  finally
    Client.Free;
    BodyStream.Free;
    RespStream.Free;
  end;
end;

// Crea un modelo personalizado desde un Modelfile
procedure TAiOllamaChat.CreateModel(const aNewModelName, aModelfileContent: string);
var
  Client     : TFPHTTPClient;
  BodyStream : TStringStream;
  RespStream : TStringStream;
  LJObj      : TJSONObject;
  LBodyStr   : string;
  LRespStr   : string;
  P          : Integer;
  LLine      : string;
  LStatusObj : TJSONObject;
  LStatus    : string;
  LCompleted : Int64;
  LTotal     : Int64;
begin
  LJObj := TJSONObject.Create;
  try
    LJObj.Add('name',      aNewModelName);
    LJObj.Add('modelfile', aModelfileContent);
    LJObj.Add('stream',    True);
    LBodyStr := LJObj.AsJSON;
  finally
    LJObj.Free;
  end;

  Client     := TFPHTTPClient.Create(nil);
  BodyStream := TStringStream.Create(LBodyStr);
  RespStream := TStringStream.Create('');
  try
    Client.AddHeader('Content-Type', 'application/json');
    Client.AddHeader('Authorization', 'Bearer ' + ApiKey);
    Client.RequestBody := BodyStream;
    Client.HTTPMethod('POST', Url + 'api/create', RespStream, [200]);

    if Client.ResponseStatusCode <> 200 then
      raise Exception.CreateFmt('Error al crear modelo: %d - %s',
        [Client.ResponseStatusCode, RespStream.DataString]);

    LRespStr := RespStream.DataString;
    while Length(LRespStr) > 0 do
    begin
      P := Pos(#10, LRespStr);
      if P > 0 then
      begin
        LLine    := Trim(Copy(LRespStr, 1, P - 1));
        Delete(LRespStr, 1, P);
      end
      else
      begin
        LLine    := Trim(LRespStr);
        LRespStr := '';
      end;

      if LLine = '' then
        Continue;

      try
        LStatusObj := TJSONObject(GetJSON(LLine));
      except
        Continue;
      end;

      if Assigned(LStatusObj) then
      try
        LStatus    := JGetStr(LStatusObj, 'status', '');
        LCompleted := JGetInt64(LStatusObj, 'completed', 0);
        LTotal     := JGetInt64(LStatusObj, 'total', 0);
        DoProgress(LStatus, LCompleted, LTotal);
      finally
        LStatusObj.Free;
      end;
    end;
  finally
    Client.Free;
    BodyStream.Free;
    RespStream.Free;
  end;
end;

// Obtiene informacion detallada de un modelo. El llamador debe liberar el resultado.
function TAiOllamaChat.ShowModelInfo(const aModelName: string): TJSONObject;
var
  Client     : TFPHTTPClient;
  BodyStream : TStringStream;
  RespStream : TStringStream;
  LJObj      : TJSONObject;
  LBodyStr   : string;
begin
  LJObj := TJSONObject.Create;
  try
    LJObj.Add('name', aModelName);
    LBodyStr := LJObj.AsJSON;
  finally
    LJObj.Free;
  end;

  Client     := TFPHTTPClient.Create(nil);
  BodyStream := TStringStream.Create(LBodyStr);
  RespStream := TStringStream.Create('');
  try
    Client.AddHeader('Content-Type', 'application/json');
    Client.AddHeader('Authorization', 'Bearer ' + ApiKey);
    Client.RequestBody := BodyStream;
    Client.HTTPMethod('POST', Url + 'api/show', RespStream, [200]);

    if Client.ResponseStatusCode = 200 then
      Result := TJSONObject(GetJSON(RespStream.DataString))
    else
      raise Exception.CreateFmt('Error al obtener info del modelo: %d - %s',
        [Client.ResponseStatusCode, RespStream.DataString]);
  finally
    Client.Free;
    BodyStream.Free;
    RespStream.Free;
  end;
end;

// Copia un modelo con otro nombre
procedure TAiOllamaChat.CopyModel(const aSourceModel, aDestinationModel: string);
var
  Client     : TFPHTTPClient;
  BodyStream : TStringStream;
  RespStream : TStringStream;
  LJObj      : TJSONObject;
  LBodyStr   : string;
begin
  LJObj := TJSONObject.Create;
  try
    LJObj.Add('source',      aSourceModel);
    LJObj.Add('destination', aDestinationModel);
    LBodyStr := LJObj.AsJSON;
  finally
    LJObj.Free;
  end;

  Client     := TFPHTTPClient.Create(nil);
  BodyStream := TStringStream.Create(LBodyStr);
  RespStream := TStringStream.Create('');
  try
    Client.AddHeader('Content-Type', 'application/json');
    Client.AddHeader('Authorization', 'Bearer ' + ApiKey);
    Client.RequestBody := BodyStream;
    Client.HTTPMethod('POST', Url + 'api/copy', RespStream, [200]);

    if Client.ResponseStatusCode <> 200 then
      raise Exception.CreateFmt('Error al copiar modelo: %d - %s',
        [Client.ResponseStatusCode, RespStream.DataString]);
  finally
    Client.Free;
    BodyStream.Free;
    RespStream.Free;
  end;
end;

// Elimina un modelo local
procedure TAiOllamaChat.DeleteModel(const aModelName: string);
var
  Client     : TFPHTTPClient;
  BodyStream : TStringStream;
  RespStream : TStringStream;
  LJObj      : TJSONObject;
  LBodyStr   : string;
begin
  LJObj := TJSONObject.Create;
  try
    LJObj.Add('name', aModelName);
    LBodyStr := LJObj.AsJSON;
  finally
    LJObj.Free;
  end;

  Client     := TFPHTTPClient.Create(nil);
  BodyStream := TStringStream.Create(LBodyStr);
  RespStream := TStringStream.Create('');
  try
    Client.AddHeader('Content-Type', 'application/json');
    Client.AddHeader('Authorization', 'Bearer ' + ApiKey);
    Client.RequestBody := BodyStream;
    // Ollama DELETE usa metodo HTTP DELETE con body JSON
    Client.HTTPMethod('DELETE', Url + 'api/delete', RespStream, [200]);
  finally
    Client.Free;
    BodyStream.Free;
    RespStream.Free;
  end;
end;

initialization
  TAiChatFactory.Instance.RegisterDriver(TAiOllamaChat);

end.
