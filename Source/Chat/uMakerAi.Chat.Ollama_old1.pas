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
  System.Threading, System.Variants, System.Net.Mime, System.IOUtils,
  System.Generics.Collections, System.NetEncoding, System.JSON,
  System.StrUtils, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent, REST.JSON, REST.Types, REST.Client,
{$IF CompilerVersion < 35}
  uJSONHelper,
{$ENDIF}
  uMakerAi.ParamsRegistry, uMakerAi.Chat, uMakerAi.Core,
  uMakerAi.Embeddings, uMakerAi.Utils.CodeExtractor, uMakerAi.Embeddings.Core;

type

  TAiOllamaChat = Class(TAiChat)
  Private
    Fkeep_alive: String;
    procedure Setkeep_alive(const Value: String);
  Protected
    Procedure OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean); Override;
    Function InitChatCompletions: String; Override;
    Procedure ParseChat(JObj: TJSonObject; ResMsg: TAiChatMessage); Override;
    function ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions; Override;
    Function InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String; Override;
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
    // --------------------------------------------------

    class function GetDriverName: string; Override;
    class procedure RegisterDefaultParams(Params: TStrings); Override;
    class function CreateInstance(Sender: TComponent): TAiChat; Override;

  Published
    // Controla cuánto tiempo se mantiene el modelo en memoria (ej: "5m", "1h", "-1" para infinito)
    property keep_alive: String read Fkeep_alive write Setkeep_alive;
  End;

  TAiOllamaEmbeddings = class(TAiEmbeddings)
  Public
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;
    // Ollama usa /api/embed ahora
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

{ TAiOllamaChat }

class function TAiOllamaChat.GetDriverName: string;
Begin
  Result := 'Ollama';
End;

class procedure TAiOllamaChat.RegisterDefaultParams(Params: TStrings);
Begin
  Params.Clear;
  Params.Add('ApiKey=ollama'); // Ollama local no suele requerir key, pero la arquitectura lo pide
  Params.Add('Model=llama3.2');
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

constructor TAiOllamaChat.Create(Sender: TComponent);
begin
  inherited;
  ApiKey := 'ollama';
  Model := 'llama3.2';
  Url := GlAIUrl;
  keep_alive := '5m'; // Default de Ollama
end;

destructor TAiOllamaChat.Destroy;
begin
  inherited;
end;

function TAiOllamaChat.ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions;
var
  LResponseObj, LMessageObj, LToolCallObj, LFunctionObj: TJSonObject;
  LToolCallsArray: TJSonArray;
  LToolCall: TAiToolsFunction;
  LItem: TJSONValue;
  LPair: TJSONPair;
  LArgsObj: TJSonObject;
  LArgsStr: string;
begin
  Result := TAiToolsFunctions.Create;

  if (jChoices = nil) or (jChoices.Count = 0) then Exit;
  if not(jChoices.Items[0] is TJSonObject) then Exit;

  LResponseObj := jChoices.Items[0] as TJSonObject;

  // Ollama structure: root -> message -> tool_calls
  if not LResponseObj.TryGetValue<TJSonObject>('message', LMessageObj) then Exit;
  if not LMessageObj.TryGetValue<TJSonArray>('tool_calls', LToolCallsArray) then Exit;

  for LItem in LToolCallsArray do
  begin
    if not(LItem is TJSonObject) then Continue;
    LToolCallObj := LItem as TJSonObject;

    if LToolCallObj.TryGetValue<TJSonObject>('function', LFunctionObj) then
    begin
      LToolCall := TAiToolsFunction.Create;
      try
        // Recuperar ID y Nombre
        LToolCall.Id := LToolCallObj.GetValue<string>('id', 'call_' + TGuid.NewGuid.ToString);
        LToolCall.Name := LFunctionObj.GetValue<string>('name', '');
        LToolCall.Tipo := 'function';

        LArgsObj := nil;

        // --- SOLUCIÓN HÍBRIDA ---
        // CASO 1: Ollama envía un Objeto JSON real
        if LFunctionObj.TryGetValue<TJSonObject>('arguments', LArgsObj) then
        begin
          LToolCall.Arguments := LArgsObj.ToJSON;
          for LPair in LArgsObj do
            LToolCall.Params.Values[LPair.JsonString.Value] := LPair.JsonValue.Value;
        end
        // CASO 2: Envía un String (OpenAI Standard)
        else if LFunctionObj.TryGetValue<string>('arguments', LArgsStr) then
        begin
          LToolCall.Arguments := LArgsStr;
          if (LArgsStr <> '') and (LArgsStr <> '{}') then
          begin
             var ParsedJSON := TJSonObject.ParseJSONValue(LArgsStr);
             try
               if Assigned(ParsedJSON) and (ParsedJSON is TJSonObject) then
               begin
                 for LPair in TJSonObject(ParsedJSON) do
                   LToolCall.Params.Values[LPair.JsonString.Value] := LPair.JsonValue.Value;
               end;
             finally
               ParsedJSON.Free;
             end;
          end;
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
begin
  Result := TJSonArray.Create;

  For I := 0 to Messages.Count - 1 do
  Begin
    Msg := Messages.Items[I];
    JObj := TJSonObject.Create;

    JObj.AddPair('role', Msg.Role);
    JObj.AddPair('content', Msg.Prompt);

    // Manejo de imágenes (Multimodal)
    // Ollama espera "images": ["base64string", ...] dentro del objeto message
    if Msg.MediaFiles.Count > 0 then
    begin
      jImages := TJSonArray.Create;
      for J := 0 to Msg.MediaFiles.Count - 1 do
      begin
        if Msg.MediaFiles[J].FileCategory = Tfc_Image then
          jImages.Add(Msg.MediaFiles[J].Base64);
      end;

      if jImages.Count > 0 then
        JObj.AddPair('images', jImages)
      else
        jImages.Free;
    end;

    // Manejo de Tool Calls previos (Assistant context)
    if Msg.Tool_calls <> '' then
    begin
{$IF CompilerVersion < 35}
      JObj.AddPair('tool_calls', TJSONUtils.ParseAsArray(Msg.Tool_calls));
{$ELSE}
      JObj.AddPair('tool_calls', TJSonArray(TJSonArray.ParseJSONValue(Msg.Tool_calls)));
{$ENDIF}
    end;

    // Manejo de Tool Responses (Tool role context)
    // Nota: Ollama /api/chat maneja el rol "tool" para las respuestas.
    if (Msg.Role = 'tool') and (Msg.FunctionName <> '') then
    begin
      // A veces Ollama requiere 'name' en el mensaje de rol tool, aunque no es estricto en todos los modelos.
      JObj.AddPair('name', Msg.FunctionName);
    end;

    Result.Add(JObj);
  End;
end;

class function TAiOllamaChat.GetModels(aApiKey, aUrl: String): TStringList;
Var
  Client: TNetHTTPClient;
  Res: IHTTPResponse;
  sUrl, EndPointUrl: String;
  jRes: TJSonObject;
  JArr: TJSonArray;
  JVal: TJSONValue;
  sModel: string;
  CustomModels: TArray<string>;
  I: Integer;
begin
  Result := TStringList.Create;

  if aUrl <> '' then
    EndPointUrl := aUrl
  else
    EndPointUrl := GlAIUrl;

  // Asegurar que termine en /
  if not EndPointUrl.EndsWith('/') then
    EndPointUrl := EndPointUrl + '/';

  Client := TNetHTTPClient.Create(Nil);
  try
    sUrl := EndPointUrl + 'api/tags';

    // GET /api/tags no requiere body
    Res := Client.Get(sUrl);

    if Res.StatusCode = 200 then
    Begin
      jRes := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      try
        if Assigned(jRes) and jRes.TryGetValue<TJSonArray>('models', JArr) then
        Begin
          For JVal in JArr do
          Begin
            sModel := JVal.GetValue<String>('name');
            if sModel <> '' then
              Result.Add(sModel);
          End;
        End;
      finally
        jRes.Free;
      end;

      // Agregar modelos personalizados definidos en el Factory
      CustomModels := TAiChatFactory.Instance.GetCustomModels('Ollama');
      for I := Low(CustomModels) to High(CustomModels) do
      begin
        if Result.IndexOf(CustomModels[I]) = -1 then
          Result.Add(CustomModels[I]);
      end;
    End
    else
    begin
      // No lanzamos excepción crítica, solo devolvemos lista vacía o log
      // Raise Exception.CreateFmt('Error Getting Models: %d', [Res.StatusCode]);
    end;
  Finally
    Client.Free;
  End;
end;

function TAiOllamaChat.InitChatCompletions: String;
Var
  AJSONObject, JOptions: TJSonObject;
  JTools, JStop: TJSonArray;
  Lista: TStringList;
  I: Integer;
  LAsincronico: Boolean;
  LModel: String;
begin
  // Documentación: https://github.com/ollama/ollama/blob/main/docs/api.md#generate-a-chat-completion

  if User = '' then
    User := 'user';

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);
  if LModel = '' then
    LModel := 'llama3.2';

  LAsincronico := Self.Asynchronous;
  AJSONObject := TJSonObject.Create;
  JOptions := TJSonObject.Create;
  Lista := TStringList.Create;

  Try
    // Parámetros raíz
    AJSONObject.AddPair('model', LModel);
    AJSONObject.AddPair('messages', GetMessages);
    AJSONObject.AddPair('stream', LAsincronico);

    if keep_alive <> '' then
      AJSONObject.AddPair('keep_alive', keep_alive);

    // Formato JSON
    if FResponse_format = tiaChatRfJson then
      AJSONObject.AddPair('format', 'json');

    // Herramientas (Tools)
    if Tool_Active and (Trim(GetTools(TToolFormat.tfOpenAi).Text) <> '') then
    Begin
      // Ollama soporta el formato de OpenAI para tools
      JTools := TJSonArray(TJSonArray.ParseJSONValue(GetTools(TToolFormat.tfOpenAi).Text));
      if Assigned(JTools) then
        AJSONObject.AddPair('tools', JTools);
    End;

    // --- OPCIONES (OPTIONS) ---
    // En la API nativa de Ollama, temperature, top_p, etc van DENTRO de "options"

    if Temperature > 0 then
      JOptions.AddPair('temperature', TJSONNumber.Create(Temperature));

    if Top_p > 0 then
      JOptions.AddPair('top_p', TJSONNumber.Create(Top_p));

    if Max_tokens > 0 then
      JOptions.AddPair('num_predict', TJSONNumber.Create(Max_tokens)); // Ollama usa num_predict

    if Frequency_penalty <> 0 then
      JOptions.AddPair('frequency_penalty', TJSONNumber.Create(Frequency_penalty));

    if Presence_penalty <> 0 then
      JOptions.AddPair('presence_penalty', TJSONNumber.Create(Presence_penalty));

    if Seed > 0 then
      JOptions.AddPair('seed', TJSONNumber.Create(Seed));

    // Stop sequences
    Lista.CommaText := Stop;
    if Lista.Count > 0 then
    begin
      JStop := TJSonArray.Create;
      for I := 0 to Lista.Count - 1 do
        JStop.Add(Lista[I]);
      JOptions.AddPair('stop', JStop);
    end;

    // Añadir objeto options al root
    if JOptions.Count > 0 then
      AJSONObject.AddPair('options', JOptions)
    else
      JOptions.Free; // Si está vacío lo liberamos, si se añadió, AJSONObject es dueño.

    Result := AJSONObject.ToJSON;

  Finally
    // Si JOptions fue añadido a AJSONObject, no liberarlo aquí.
    // Si no fue añadido, lo liberamos arriba.
    // AJSONObject libera sus hijos.
    AJSONObject.Free;
    Lista.Free;
  End;
end;

function TAiOllamaChat.InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String;
Var
  ABody: String;
  sUrl: String;
  Res: IHTTPResponse;
  //St: TStringStream;
  FHeaders: TNetHeaders;
  JObj: TJSonObject;
  SavedOnReceiveData: TReceiveDataEvent;
  St : TStringStream;
begin

  FBusy := True; // Marca como ocupado al sistema
  FAbort := False; // Inicia como no en abort
  FLastError := '';
  FLastContent := '';
  FLastPrompt := '';

  DoStateChange(acsConnecting, 'Sending request...');

  //Variable global para mantener el stream activo en llamados asincronos
  //se elimina al terminar el llamado en
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

    If FClient.Asynchronous = False then
    Begin
      SavedOnReceiveData := FClient.OnReceiveData;
      FClient.OnReceiveData := Nil;
      Try
        Res := FClient.Post(sUrl, St, FResponse, FHeaders);
      Finally
        FClient.OnReceiveData := SavedOnReceiveData;
      End;
    End
    Else
    Begin
      Res := FClient.Post(sUrl, St, FResponse, FHeaders);
    End;

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
          FreeAndNil(JObj);
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

procedure TAiOllamaChat.OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean);
var
  LChunkStr, LJsonLine, LRole, LContentPart, LThinkingPart: string;
  LJsonObject, LMessageObj: TJSonObject;
  LDone: Boolean;
  LFinalMsg: TAiChatMessage;
  P: Integer;

  // Variables para Tools
  LToolCallsArray: TJSonArray;
  // LToolCallVal: TJSONValue;
  LToolIndex: Integer;
  LBufferObj, LBufferFunc: TJSonObject;
  LStrValue: string;
  LFuncObj: TJSonObject;

  procedure ProcessOllamaLine(ALine: string);
  begin
    if ALine.Trim.IsEmpty then
      Exit;

    try
      LJsonObject := TJSonObject.ParseJSONValue(ALine) as TJSonObject;
      if not Assigned(LJsonObject) then
        Exit;

      try
        LDone := LJsonObject.GetValue<Boolean>('done', False);

        if not LDone then
        begin
          // --- PROCESAR FRAGMENTO ---
          if LJsonObject.TryGetValue<TJSonObject>('message', LMessageObj) then
          begin
            // 1. Rol y Contenido
            if LMessageObj.TryGetValue<string>('role', LRole) then
              FTmpRole := LRole; // Guardamos rol temporalmente

            if LMessageObj.TryGetValue<string>('content', LContentPart) then
            begin
              LContentPart := StringReplace(LContentPart, #10, sLineBreak, [rfReplaceAll]);
              FLastContent := FLastContent + LContentPart;
              if Assigned(FOnReceiveDataEvent) then
                FOnReceiveDataEvent(Self, nil, LJsonObject, FTmpRole, LContentPart);
            end;

            // 2. Pensamiento (Thinking)
            if LMessageObj.TryGetValue<string>('thinking', LThinkingPart) or LMessageObj.TryGetValue<string>('reasoning', LThinkingPart) then
            begin
              if (LThinkingPart <> '') then
              begin
                LThinkingPart := StringReplace(LThinkingPart, #10, sLineBreak, [rfReplaceAll]);
                if Assigned(OnReceiveThinking) then
                  OnReceiveThinking(Self, nil, LJsonObject, FTmpRole, LThinkingPart);
              end;
            end;

            // 3. ACUMULACIÓN DE TOOL CALLS (Streaming)
            if LMessageObj.TryGetValue<TJSonArray>('tool_calls', LToolCallsArray) then
            begin
              // Nota: Ollama suele mandar el tool call completo, pero preparamos lógica de buffer
              // por si acaso fragmenta argumentos en el futuro o con ciertos modelos.
              for var LToolCallVal in LToolCallsArray do
              begin
                if LToolCallVal is TJSonObject then
                begin
                  var
                  LCurrentTool := TJSonObject(LToolCallVal);

                  // Ollama no siempre manda 'index' en stream, asumimos 0 si no existe
                  if not LCurrentTool.TryGetValue<Integer>('index', LToolIndex) then
                    LToolIndex := 0;

                  if not FTmpToolCallBuffer.TryGetValue(LToolIndex, LBufferObj) then
                  begin
                    LBufferObj := TJSonObject.Create;
                    FTmpToolCallBuffer.Add(LToolIndex, LBufferObj);
                  end;

                  // Acumular ID y Tipo
                  if LCurrentTool.TryGetValue<string>('id', LStrValue) then
                    if LBufferObj.GetValue('id') = nil then
                      LBufferObj.AddPair('id', LStrValue);

                  if LCurrentTool.TryGetValue<string>('type', LStrValue) then
                    if LBufferObj.GetValue('type') = nil then
                      LBufferObj.AddPair('type', LStrValue);

                  // Acumular Función (Nombre y Argumentos)
                  if LCurrentTool.TryGetValue<TJSonObject>('function', LFuncObj) then
                  begin
                    if not LBufferObj.TryGetValue<TJSonObject>('function', LBufferFunc) then
                    begin
                      LBufferFunc := TJSonObject.Create;
                      LBufferObj.AddPair('function', LBufferFunc);
                    end;

                    if LFuncObj.TryGetValue<string>('name', LStrValue) then
                    begin
                      var
                        OldName: string := '';
                      if LBufferFunc.TryGetValue<string>('name', OldName) then
                        LBufferFunc.RemovePair('name');
                      LBufferFunc.AddPair('name', OldName + LStrValue);
                    end;

                    if LFuncObj.TryGetValue<string>('arguments', LStrValue) then
                    begin
                      // Nota: En Ollama 'arguments' ya es un objeto JSON, no un string.
                      // Si viene como objeto, lo serializamos para acumular texto si fuera necesario,
                      // o reemplazamos el objeto completo si es un update completo.
                      // Para simplificar y dado que Ollama manda JSON Objects:
                      if LBufferFunc.GetValue('arguments') <> nil then
                        LBufferFunc.RemovePair('arguments');

                      // Clonamos el objeto de argumentos actual
                      LBufferFunc.AddPair('arguments', TJSonObject(TJSonObject.ParseJSONValue(LStrValue).Clone));
                      // OJO: Si Ollama manda argumentos parciales como string, la lógica cambia.
                      // Actualmente Ollama manda argumentos completos.
                    end;
                  end;
                end;
              end;
            end;
          end;
        end
        else
        begin
          // --- FINAL DEL STREAM (DONE=TRUE) ---
          FBusy := False;
          LFinalMsg := TAiChatMessage.Create('', 'assistant');
          try
            // Si acumulamos tools en el buffer, hay que inyectarlos en el objeto final
            // para que ParseChat los vea.
            if FTmpToolCallBuffer.Count > 0 then
            begin
              var
              LCombinedTools := TJSonArray.Create;
              var
              LMessageFinal := TJSonObject.Create;

              // Volcar buffer al array
              for var Key in FTmpToolCallBuffer.Keys do
              begin
                LCombinedTools.Add(FTmpToolCallBuffer[Key].Clone as TJSonObject);
                FTmpToolCallBuffer[Key].Free;
              end;
              FTmpToolCallBuffer.Clear;

              // Crear estructura simulada para ParseChat: { message: { tool_calls: [...] } }
              if LJsonObject.GetValue('message') = nil then
              begin
                LMessageFinal.AddPair('role', 'assistant');
                LMessageFinal.AddPair('tool_calls', LCombinedTools);
                LJsonObject.AddPair('message', LMessageFinal);
              end
              else
              begin
                // Si ya existe message (stats), le agregamos los tools
                TJSonObject(LJsonObject.GetValue('message')).AddPair('tool_calls', LCombinedTools);
              end;
            end;

            ParseChat(LJsonObject, LFinalMsg);
          except
            LFinalMsg.Free;
            raise;
          end;

          FTmpResponseText := '';
        end;
      finally
        LJsonObject.Free;
      end;
    except
      // Ignorar errores de parseo en líneas corruptas
    end;
  end;

begin
  if not FClient.Asynchronous then
    Exit;

  AAbort := FAbort;
  if FAbort then
  begin
    FBusy := False;
    FTmpToolCallBuffer.Clear; // Limpiar buffer en abort
    if Assigned(FOnReceiveDataEnd) then
      FOnReceiveDataEnd(Self, nil, nil, 'system', 'abort');
    Exit;
  end;

  try
    LChunkStr := FResponse.DataString;
    FResponse.Clear;
    FTmpResponseText := FTmpResponseText + LChunkStr;

    // Procesar líneas completas
    while Pos(#10, FTmpResponseText) > 0 do
    begin
      P := Pos(#10, FTmpResponseText);
      LJsonLine := Copy(FTmpResponseText, 1, P - 1);
      Delete(FTmpResponseText, 1, P);
      ProcessOllamaLine(LJsonLine);
    end;
  except
    on E: Exception do
      DoError('Error en OnInternalReceiveData: ' + E.Message, E);
  end;
end;

procedure TAiOllamaChat.ParseChat(JObj: TJSonObject; ResMsg: TAiChatMessage);
var
  LMessageObj: TJSonObject;
  LToolCallsArray: TJSonArray;
  AskMsg, ToolMsg: TAiChatMessage;
  LRole, LContent, LReasoning, sToolCallsJSON: string;
  LPromptTokens, LEvalTokens: Integer;

  // Variables para simulación y ejecución
  LChoicesSimulado: TJSonArray;
  LFunciones: TAiToolsFunctions;
  LToolCall: TAiToolsFunction;

  // Variables para extracción de archivos
  Code: TMarkdownCodeExtractor;
  CodeFiles: TCodeFileList;
  CodeFile: TCodeFile;
  MF: TAiMediaFile;
  St: TStringStream;
begin
  if not Assigned(JObj) then Exit;

  // 1. EXTRAER METADATOS
  LPromptTokens := JObj.GetValue<Integer>('prompt_eval_count', 0);
  LEvalTokens := JObj.GetValue<Integer>('eval_count', 0);

  Self.Prompt_tokens := Self.Prompt_tokens + LPromptTokens;
  Self.Completion_tokens := Self.Completion_tokens + LEvalTokens;
  Self.Total_tokens := Self.Total_tokens + LPromptTokens + LEvalTokens;

  AskMsg := GetLastMessage;

  // 2. OBTENER MENSAJE
  if not JObj.TryGetValue<TJSonObject>('message', LMessageObj) then
  begin
     if FLastContent <> '' then
     begin
       LRole := 'assistant';
       LContent := FLastContent;
     end
     else
     begin
       // Si no hay mensaje ni contenido previo, salimos
       Exit;
     end;
  end
  else
  begin
    LRole := LMessageObj.GetValue<string>('role', 'assistant');
    LContent := LMessageObj.GetValue<string>('content', '');

    if LMessageObj.TryGetValue<string>('thinking', LReasoning) then
      ResMsg.Reasoning := LReasoning;
  end;

  if (LContent = '') and (FLastContent <> '') then
    LContent := FLastContent;

  // 3. DETECTAR TOOL CALLS
  sToolCallsJSON := '';
  if Assigned(LMessageObj) and LMessageObj.TryGetValue<TJSonArray>('tool_calls', LToolCallsArray) then
  begin
    if LToolCallsArray.Count > 0 then
      sToolCallsJSON := LToolCallsArray.ToJSON; // Usar ToJSON para evitar saltos de línea extras
  end;

  ResMsg.Role := LRole;
  ResMsg.Model := JObj.GetValue<string>('model', '');
  ResMsg.Content := LContent;
  ResMsg.Prompt := LContent;
  ResMsg.Prompt_tokens := LPromptTokens;
  ResMsg.Completion_tokens := LEvalTokens;
  ResMsg.Total_tokens := LPromptTokens + LEvalTokens;
  ResMsg.Tool_calls := sToolCallsJSON;

  // ===========================================================================
  // FLUJO A: HAY LLAMADAS A HERRAMIENTAS
  // ===========================================================================
  if sToolCallsJSON <> '' then
  begin
    // 1. Añadimos el mensaje que PIDE la herramienta
    if FMessages.IndexOf(ResMsg) = -1 then
    begin
       ResMsg.Id := FMessages.Count + 1;
       FMessages.Add(ResMsg);
    end;

    // 2. Extraer y Ejecutar
    LChoicesSimulado := TJSonArray.Create;
    LFunciones := nil;
    try
      LChoicesSimulado.Add(JObj.Clone as TJSonObject);
      LFunciones := ExtractToolCallFromJson(LChoicesSimulado);

      if Assigned(LFunciones) and (LFunciones.Count > 0) then
      begin
        for var Key in LFunciones.Keys do
        begin
          LToolCall := LFunciones[Key];
          LToolCall.ResMsg := ResMsg;
          LToolCall.AskMsg := AskMsg;
          try
            DoCallFunction(LToolCall);
          except
            on E: Exception do
              DoError('Error en tool: ' + LToolCall.Name, E);
          end;
        end;

        // 3. Añadir Respuestas (Role: Tool)
        for var Key in LFunciones.Keys do
        begin
          LToolCall := LFunciones[Key];
          ToolMsg := TAiChatMessage.Create(LToolCall.Response, 'tool', LToolCall.Id, LToolCall.Name);
          ToolMsg.Id := FMessages.Count + 1;
          FMessages.Add(ToolMsg);
        end;

        // 4. RECURSIVIDAD: Obtener respuesta final
        // Creamos un nuevo mensaje para la respuesta final
        var FinalResponseMsg := TAiChatMessage.Create('', 'assistant');
        try
           Self.Run(nil, FinalResponseMsg);
        except
           FinalResponseMsg.Free;
           raise;
        end;
      end;
    finally
      LChoicesSimulado.Free;
      if Assigned(LFunciones) then LFunciones.Free;
    end;
  end
  // ===========================================================================
  // FLUJO B: RESPUESTA FINAL (TEXTO)
  // ===========================================================================
  else
  begin
    FBusy := False;

    Self.FLastContent := LContent;

    // Extracción de Archivos
    if (tfc_ExtracttextFile in NativeOutputFiles) and (LContent <> '') then
    begin
      Code := TMarkdownCodeExtractor.Create;
      try
        CodeFiles := Code.ExtractCodeFiles(LContent);
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

    // =======================================================================
    // CORRECCIÓN CRÍTICA:
    // La clase padre TAiChat.Run NO añade el mensaje si AskMsg.Role = 'tool'.
    // Como venimos de una recursión de tools, debemos forzar la adición aquí.
    // =======================================================================
    if FMessages.IndexOf(ResMsg) = -1 then
    begin
      ResMsg.Id := FMessages.Count + 1;
      FMessages.Add(ResMsg);
    end;

    // Notificaciones finales
    DoProcessResponse(AskMsg, ResMsg, LContent);
    DoStateChange(acsFinished, 'Done');

    if Assigned(FOnReceiveDataEnd) then
      FOnReceiveDataEnd(Self, ResMsg, JObj, LRole, LContent);
  end;
end;


// ======================= GESTIÓN DE MODELOS =======================

procedure TAiOllamaChat.CopyModel(const aSourceModel, aDestinationModel: string);
var
  LJsonObject: TJSonObject;
  LBodyStream: TStringStream;
  LUrl: string;
begin
  LUrl := Url + 'api/copy';
  LJsonObject := TJSonObject.Create;
  LBodyStream := TStringStream.Create('', TEncoding.UTF8);
  try
    LJsonObject.AddPair('source', aSourceModel);
    LJsonObject.AddPair('destination', aDestinationModel);
    LBodyStream.WriteString(LJsonObject.ToJSON);
    LBodyStream.Position := 0;

    FClient.Post(LUrl, LBodyStream);
  finally
    LJsonObject.Free;
    LBodyStream.Free;
  end;
end;

procedure TAiOllamaChat.CreateModel(const aNewModelName, aModelfileContent: string);
var
  LJsonObject: TJSonObject;
  LBodyStream, LResponseStream: TStringStream;
  LUrl: string;
  LJsonLines: TArray<string>;
  LLine: string;
  LStatusObj: TJSonObject;
begin
  LUrl := Url + 'api/create';
  LJsonObject := TJSonObject.Create;
  LBodyStream := TStringStream.Create('', TEncoding.UTF8);
  LResponseStream := TStringStream.Create('', TEncoding.UTF8);
  try
    LJsonObject.AddPair('name', aNewModelName);
    LJsonObject.AddPair('modelfile', aModelfileContent);
    LJsonObject.AddPair('stream', TJSONBool.Create(True));
    LBodyStream.WriteString(LJsonObject.ToJSON);
    LBodyStream.Position := 0;

    FClient.Post(LUrl, LBodyStream, LResponseStream);

    // Procesar status
    LJsonLines := LResponseStream.DataString.Split([#10], TStringSplitOptions.ExcludeEmpty);
    for LLine in LJsonLines do
    begin
      if Assigned(OnProgressEvent) then
      begin
        LStatusObj := TJSonObject.ParseJSONValue(LLine) as TJSonObject;
        if Assigned(LStatusObj) then
          try
            // Aquí se podría extraer 'status', 'total', 'completed'
            OnProgressEvent(Self, LStatusObj.GetValue<string>('status', ''), 0, 0);
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
  LUrl: string;
begin
  LUrl := Url + 'api/delete';
  LJsonObject := TJSonObject.Create;
  LBodyStream := TStringStream.Create('', TEncoding.UTF8);
  try
    LJsonObject.AddPair('name', aModelName);
    LBodyStream.WriteString(LJsonObject.ToJSON);
    LBodyStream.Position := 0;

    // TNetHTTPClient.Delete con body requiere versión reciente o usar Execute('DELETE'...)
    // Asumimos que tu versión soporta Delete con Stream o usamos Execute
    FClient.Execute('DELETE', LUrl, LBodyStream);
  finally
    LJsonObject.Free;
    LBodyStream.Free;
  end;
end;

procedure TAiOllamaChat.PullModel(const aModelName: string);
var
  LJsonObject: TJSonObject;
  LBodyStream, LResponseStream: TStringStream;
  LUrl: string;
  LJsonLines: TArray<string>;
  LLine: string;
  LStatusObj: TJSonObject;
  LCompleted, LTotal: Int64;
begin
  LUrl := Url + 'api/pull';
  LJsonObject := TJSonObject.Create;
  LBodyStream := TStringStream.Create('', TEncoding.UTF8);
  LResponseStream := TStringStream.Create('', TEncoding.UTF8);
  try
    LJsonObject.AddPair('name', aModelName);
    LJsonObject.AddPair('stream', TJSONBool.Create(True));
    LBodyStream.WriteString(LJsonObject.ToJSON);
    LBodyStream.Position := 0;

    FClient.Post(LUrl, LBodyStream, LResponseStream);

    LJsonLines := LResponseStream.DataString.Split([#10], TStringSplitOptions.ExcludeEmpty);
    for LLine in LJsonLines do
    begin
      if Assigned(OnProgressEvent) then
      begin
        LStatusObj := TJSonObject.ParseJSONValue(LLine) as TJSonObject;
        if Assigned(LStatusObj) then
          try
            LCompleted := LStatusObj.GetValue<Int64>('completed', 0);
            LTotal := LStatusObj.GetValue<Int64>('total', 0);
            OnProgressEvent(Self, LStatusObj.GetValue<string>('status', ''), LCompleted, LTotal);
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
  LUrl := Url + 'api/show';
  LJsonObject := TJSonObject.Create;
  LBodyStream := TStringStream.Create('', TEncoding.UTF8);
  try
    LJsonObject.AddPair('name', aModelName);
    LBodyStream.WriteString(LJsonObject.ToJSON);
    LBodyStream.Position := 0;

    LResponse := FClient.Post(LUrl, LBodyStream);
    if LResponse.StatusCode = 200 then
      Result := TJSonObject.ParseJSONValue(LResponse.ContentAsString) as TJSonObject
    else
      Result := nil;
  finally
    LJsonObject.Free;
    LBodyStream.Free;
  end;
end;

{ TAiOllamaEmbeddings }

constructor TAiOllamaEmbeddings.Create(aOwner: TComponent);
begin
  inherited;
  ApiKey := 'ollama';
  Url := GlAIUrl;
  FDimensions := 0; // Default del modelo
  FModel := 'all-minilm'; // Modelo ligero popular para embeddings en Ollama
end;

destructor TAiOllamaEmbeddings.Destroy;
begin
  inherited;
end;

function TAiOllamaEmbeddings.CreateEmbedding(aInput, aUser: String; aDimensions: Integer; aModel, aEncodingFormat: String): TAiEmbeddingData;
Var
  Client: TNetHTTPClient;
  JObj: TJSonObject;
  Res: IHTTPResponse;
  Response, St: TStringStream;
  sUrl: String;
begin
  if aModel = '' then
    aModel := FModel;

  // Endpoint actualizado según documentación
  sUrl := Url + 'api/embed';

  Client := TNetHTTPClient.Create(Nil);
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  JObj := TJSonObject.Create;

  Try
    JObj.AddPair('model', aModel);

    // API /api/embed espera "input" que puede ser string o array de strings
    JObj.AddPair('input', aInput);

    // Parámetros opcionales si se soportan en options
    // JObj.AddPair('options', ...);

    St.WriteString(JObj.ToJSON);
    St.Position := 0;

    Client.ContentType := 'application/json';
    Res := Client.Post(sUrl, St, Response);

    if Res.StatusCode = 200 then
    Begin
      JObj.Free; // Liberar el de la petición
      JObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      ParseEmbedding(JObj);
      Result := Self.Data;
    End
    else
    begin
      Raise Exception.CreateFmt('Ollama Embed Error: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
    St.Free;
    Response.Free;
    JObj.Free;
  End;
end;

procedure TAiOllamaEmbeddings.ParseEmbedding(JObj: TJSonObject);
Var
  JEmbeddingsArr, JVector: TJSonArray;
  J: Integer;
begin
  // Respuesta de /api/embed: { "embeddings": [ [0.1, 0.2, ...], [ ... ] ] }
  // Asumimos un solo input, tomamos el primer vector.

  if JObj.TryGetValue<TJSonArray>('embeddings', JEmbeddingsArr) and (JEmbeddingsArr.Count > 0) then
  begin
    JVector := JEmbeddingsArr.Items[0] as TJSonArray;

    SetLength(FData, JVector.Count);
    for J := 0 to JVector.Count - 1 do
      FData[J] := JVector.Items[J].GetValue<Double>;
  end;
end;

Initialization

TAiChatFactory.Instance.RegisterDriver(TAiOllamaChat);

end.
