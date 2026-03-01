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
// Driver para OpenAI Responses API (https://api.openai.com/v1/responses)
//
// Usa la API "Responses" de OpenAI (GPT-5+):
//   - Endpoint: /v1/responses  (no /v1/chat/completions)
//   - Campo "input" (no "messages"), "instructions" (no "system")
//   - SSE: eventos con campo "type" (response.created, output_text.delta, etc.)
//   - Herramientas: function_call, shell_call, apply_patch_call,
//                  web_search, code_interpreter, image_generation
//   - Memoria de servidor via "previous_response_id" (evita reenviar historial)
//
// Diferencias respecto al Delphi original:
//   - TNetHTTPClient/TNetHeaders → TFPHTTPClient + array of string
//   - Asynchronous = True        → TAiHttpThread (thread manual)
//   - OnInternalReceiveData      → ProcessSSELine override (por linea)
//   - TTask.WaitForAll/TTask.Run → secuencial (Fase 1 pattern)
//   - TMultipartFormData         → BuildMultipart manual con TMemoryStream
//   - Inline var (Delphi 10.4+) → declaracion al inicio del metodo
//   - string.IsEmpty/StartsWith  → comparaciones manuales
//   - ContainsText               → Pos(LowerCase) > 0
//   - FOnReceiveDataEvent/End    → DoData / DoDataEnd (protegidos)
//   - FOnAddMessage              → OnAddMessage (property)
//   - Sora video generation      → stub minimal (Fase 2)
//   - TStreamReader              → TStringStream.DataString

unit uMakerAi.Chat.OpenAi;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Generics.Collections, SyncObjs,
  fpjson, jsonparser,
  fphttpclient, opensslsockets,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.Tools,
  uMakerAi.Tools.Functions,
  uMakerAi.Utils.CodeExtractor;

type
  // Evento para manejar manualmente la herramienta apply_patch.
  // OperationType: 'create_file', 'update_file', 'delete_file'.
  // aStatus debe ser 'completed' o 'failed'.
  TAiApplyPatchEvent = procedure(Sender: TObject;
      const OperationType, Path, Diff, CallId: string;
      var aStatus, aOutput: string) of object;

  // Evento para comandos de shell.
  TAiShellCommandEvent = procedure(Sender: TObject;
      const Command, CallId: string;
      var StdOut, StdErr: string;
      var ExitCode: Integer;
      var Handled: Boolean) of object;

  TAiReasoningSummary = (rsmDefault, rsmAuto, rsmConcise, rsmDetailed);

  // ---------------------------------------------------------------------------
  //  TAiOpenChat — driver OpenAI Responses API
  // ---------------------------------------------------------------------------
  TAiOpenChat = class(TAiChat)
  private
    // Estado Responses API
    FResponseId      : string;
    FResponseStatus  : string; // completed, failed, in_progress, incomplete
    FRecursionNeeded : Boolean;

    // Propiedades especificas
    FStore             : Boolean;
    FTruncation        : string;
    FParallel_ToolCalls: Boolean;
    FVerbosity         : string;
    FReasoningSummary  : TAiReasoningSummary;
    FAllowAutoShell    : Boolean;

    // Eventos
    FOnApplyPatch    : TAiApplyPatchEvent;
    FOnShellCommand  : TAiShellCommandEvent;

    // Setters
    procedure SetStore(const Value: Boolean);
    procedure SetTruncation(const Value: string);
    procedure SetParallel_ToolCalls(const Value: Boolean);
    procedure SetVerbosity(const Value: string);

    // Helpers de construccion del request
    procedure AddMessageToInput(Msg: TAiChatMessage; TargetArray: TJSONArray);
    function  GetToolsJson: TJSONArray;

    // Multipart manual para upload de archivos
    function  BuildMultipart(const ABoundary, APurpose: string;
        aMediaFile: TAiMediaFile): TMemoryStream;

    // Helper HTTP GET con Bearer (para DownloadFile, DeleteFile, GetModels)
    function  DoHttpGet(const sUrl: string;
        aResponseStream: TStream = nil): string;
    function  DoHttpDelete(const sUrl: string): string;

    procedure UpdateResponseStatus(const aStatus: string);
    procedure ProcessSSEOutputItemDone(JItem: TJSONObject);
    procedure ProcessSSEFunctionCallDone(JItem: TJSONObject);
    procedure ParseSyncShellCall(JItem: TJSONObject);
    procedure ParseSyncApplyPatchCall(JItem: TJSONObject);
    procedure ExecuteStandardToolCalls(JFunctionCalls: TJSONArray;
        ResMsg: TAiChatMessage);

  protected
    procedure ProcessSSELine(const ALine: string); override;
    function  InitChatCompletions: string; override;
    procedure ParseChat(jObj: TJSONObject; ResMsg: TAiChatMessage); override;
    function  InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): string; override;
    function  InternalRunSpeechGeneration(ResMsg, AskMsg: TAiChatMessage): string; override;
    function  InternalRunTranscription(aMediaFile: TAiMediaFile;
        ResMsg, AskMsg: TAiChatMessage): string; override;
    function  InternalRunImageVideoGeneration(ResMsg, AskMsg: TAiChatMessage): string; override;
    procedure DoCallFunction(ToolCall: TAiToolsFunction); override;

  public
    constructor Create(Sender: TComponent); override;
    destructor  Destroy; override;

    class function  GetDriverName: string; override;
    class procedure RegisterDefaultParams(Params: TStrings); override;
    class function  CreateInstance(Sender: TComponent): TAiChat; override;
    class function  GetModels(aApiKey: string; aUrl: string = ''): TStringList; override;

    procedure NewChat; override;

    // File API
    function  UploadFileToCache(aMediaFile: TAiMediaFile;
        aTTL_Seconds: Integer = 3600): string; override;
    function  DeleteFile(aMediaFile: TAiMediaFile): string; override;
    function  DownloadFile(aMediaFile: TAiMediaFile): string; override;
    function  DeleteAllUploadedFiles: Integer;

    property ResponseId    : string read FResponseId;
    property ResponseStatus: string read FResponseStatus;

  published
    property Store             : Boolean           read FStore              write SetStore              default True;
    property Truncation        : string            read FTruncation         write SetTruncation;
    property Parallel_ToolCalls: Boolean           read FParallel_ToolCalls write SetParallel_ToolCalls default True;
    property Verbosity         : string            read FVerbosity          write SetVerbosity;
    property ReasoningSummary  : TAiReasoningSummary
        read FReasoningSummary write FReasoningSummary default rsmDefault;
    property AllowAutoShell    : Boolean           read FAllowAutoShell     write FAllowAutoShell       default False;
    property OnApplyPatch      : TAiApplyPatchEvent
        read FOnApplyPatch write FOnApplyPatch;
    property OnShellCommand    : TAiShellCommandEvent
        read FOnShellCommand write FOnShellCommand;
  end;

procedure Register;

implementation

uses
  UMakerAi.ParamsRegistry;

const
  GlOpenAIUrl = 'https://api.openai.com/v1/';

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiOpenChat]);
end;

// ===========================================================================
//  Helpers internos (no metodos de clase)
// ===========================================================================

// Comprueba si Sub esta contenido en S (case-insensitive)
function ContainsStr(const S, Sub: string): Boolean;
begin
  Result := Pos(LowerCase(Sub), LowerCase(S)) > 0;
end;

// ===========================================================================
//  Constructor / Destructor
// ===========================================================================

constructor TAiOpenChat.Create(Sender: TComponent);
begin
  inherited Create(Sender);
  FStore              := True;
  FTruncation         := 'disabled';
  FParallel_ToolCalls := True;
  FVerbosity          := '';
  FResponseId         := '';
  FResponseStatus     := '';
  FRecursionNeeded    := False;
  FAllowAutoShell     := False;
  FReasoningSummary   := rsmDefault;

  if ApiKey = '' then
    ApiKey := '@OPENAI_API_KEY';
  if Url = '' then
    Url := GlOpenAIUrl;
  if Model = '' then
    Model := 'gpt-4o';

  // Capacidades soportadas
  ChatMediaSupports := [Tcm_Text, Tcm_Image, Tcm_WebSearch,
                        Tcm_CodeInterpreter, tcm_pdf];
  NativeInputFiles  := [Tfc_Text, Tfc_Image, Tfc_pdf];
  NativeOutputFiles := [Tfc_Text, Tfc_Image];
end;

destructor TAiOpenChat.Destroy;
begin
  inherited;
end;

// ===========================================================================
//  Factory
// ===========================================================================

class function TAiOpenChat.GetDriverName: string;
begin
  Result := 'OpenAi';
end;

class procedure TAiOpenChat.RegisterDefaultParams(Params: TStrings);
begin
  Params.Clear;
  Params.Add('ApiKey=@OPENAI_API_KEY');
  Params.Add('Model=gpt-4o');
  Params.Add('Url=' + GlOpenAIUrl);
end;

class function TAiOpenChat.CreateInstance(Sender: TComponent): TAiChat;
begin
  Result := TAiOpenChat.Create(Sender);
end;

// ===========================================================================
//  Setters
// ===========================================================================

procedure TAiOpenChat.SetStore(const Value: Boolean);
begin FStore := Value; end;

procedure TAiOpenChat.SetTruncation(const Value: string);
begin FTruncation := Value; end;

procedure TAiOpenChat.SetParallel_ToolCalls(const Value: Boolean);
begin FParallel_ToolCalls := Value; end;

procedure TAiOpenChat.SetVerbosity(const Value: string);
begin FVerbosity := Value; end;

procedure TAiOpenChat.UpdateResponseStatus(const aStatus: string);
begin
  FResponseStatus := aStatus;
end;

// ===========================================================================
//  NewChat
// ===========================================================================

procedure TAiOpenChat.NewChat;
begin
  FResponseId := '';
  inherited;
end;

// ===========================================================================
//  Helper HTTP — GET y DELETE con Bearer
// ===========================================================================

function TAiOpenChat.DoHttpGet(const sUrl: string;
    aResponseStream: TStream): string;
var
  Client   : TFPHTTPClient;
  RespStream: TStringStream;
begin
  Result := '';
  Client := TFPHTTPClient.Create(nil);
  try
    Client.AddHeader('Authorization', 'Bearer ' + ApiKey);
    Client.IOTimeout := ResponseTimeOut;
    if Assigned(aResponseStream) then
    begin
      Client.HTTPMethod('GET', sUrl, aResponseStream, [200]);
    end
    else
    begin
      RespStream := TStringStream.Create('');
      try
        Client.HTTPMethod('GET', sUrl, RespStream, [200]);
        Result := RespStream.DataString;
      finally
        RespStream.Free;
      end;
    end;
  finally
    Client.Free;
  end;
end;

function TAiOpenChat.DoHttpDelete(const sUrl: string): string;
var
  Client    : TFPHTTPClient;
  RespStream: TStringStream;
begin
  Result := '';
  Client := TFPHTTPClient.Create(nil);
  RespStream := TStringStream.Create('');
  try
    Client.AddHeader('Authorization', 'Bearer ' + ApiKey);
    Client.IOTimeout := ResponseTimeOut;
    Client.HTTPMethod('DELETE', sUrl, RespStream, [200]);
    Result := RespStream.DataString;
  finally
    RespStream.Free;
    Client.Free;
  end;
end;

// ===========================================================================
//  GetModels — lista modelos via GET /v1/models
// ===========================================================================

class function TAiOpenChat.GetModels(aApiKey: string; aUrl: string): TStringList;
var
  Client    : TFPHTTPClient;
  RespStream: TStringStream;
  sUrl      : string;
  jObj      : TJSONObject;
  jArr      : TJSONArray;
  jItem     : TJSONObject;
  I         : Integer;
begin
  Result := TStringList.Create;
  if aUrl = '' then
    aUrl := GlOpenAIUrl;
  if Copy(aUrl, Length(aUrl), 1) <> '/' then
    aUrl := aUrl + '/';
  sUrl := aUrl + 'models';

  Client    := TFPHTTPClient.Create(nil);
  RespStream := TStringStream.Create('');
  try
    Client.AddHeader('Authorization', 'Bearer ' + aApiKey);
    Client.IOTimeout := 30000;
    try
      Client.HTTPMethod('GET', sUrl, RespStream, [200]);
      jObj := TJSONObject(GetJSON(RespStream.DataString));
      if Assigned(jObj) then
      try
        jArr := JGetArr(jObj, 'data');
        if Assigned(jArr) then
          for I := 0 to jArr.Count - 1 do
          begin
            jItem := TJSONObject(jArr.Items[I]);
            Result.Add(JGetStr(jItem, 'id', ''));
          end;
      finally
        jObj.Free;
      end;
    except
      // Silencioso — devuelve lista vacia
    end;
  finally
    RespStream.Free;
    Client.Free;
  end;
end;

// ===========================================================================
//  BuildMultipart — construye multipart/form-data manualmente
// ===========================================================================

function TAiOpenChat.BuildMultipart(const ABoundary, APurpose: string;
    aMediaFile: TAiMediaFile): TMemoryStream;
var
  S         : string;
  FileName  : string;
  MimeType  : string;
begin
  Result := TMemoryStream.Create;
  FileName := aMediaFile.Filename;
  if FileName = '' then FileName := 'upload.bin';
  MimeType := aMediaFile.MimeType;
  if MimeType = '' then MimeType := 'application/octet-stream';

  // Campo: purpose
  S := '--' + ABoundary + #13#10 +
       'Content-Disposition: form-data; name="purpose"' + #13#10 +
       #13#10 +
       APurpose + #13#10;
  Result.WriteBuffer(PChar(S)^, Length(S));

  // Campo: file
  S := '--' + ABoundary + #13#10 +
       'Content-Disposition: form-data; name="file"; filename="' + FileName + '"' + #13#10 +
       'Content-Type: ' + MimeType + #13#10 +
       #13#10;
  Result.WriteBuffer(PChar(S)^, Length(S));

  // Contenido del archivo
  aMediaFile.Content.Position := 0;
  Result.CopyFrom(aMediaFile.Content, aMediaFile.Content.Size);

  // Cierre
  S := #13#10 + '--' + ABoundary + '--' + #13#10;
  Result.WriteBuffer(PChar(S)^, Length(S));

  Result.Position := 0;
end;

// ===========================================================================
//  UploadFileToCache — POST multipart a /files
// ===========================================================================

function TAiOpenChat.UploadFileToCache(aMediaFile: TAiMediaFile;
    aTTL_Seconds: Integer): string;
var
  Client    : TFPHTTPClient;
  RespStream: TStringStream;
  MPStream  : TMemoryStream;
  sUrl      : string;
  Boundary  : string;
  jObj      : TJSONObject;
  LFileName : string;
  LStatus   : string;
begin
  Result := '';
  if not Assigned(aMediaFile) then
    raise Exception.Create('El objeto MediaFile no esta asignado.');

  if aMediaFile.IdFile <> '' then
  begin
    Result := aMediaFile.IdFile;
    Exit;
  end;

  if aMediaFile.Content.Size = 0 then
    raise Exception.Create('El archivo esta vacio.');

  if Copy(Url, Length(Url), 1) = '/' then
    sUrl := Url + 'files'
  else
    sUrl := Url + '/files';

  Boundary  := 'OAIBoundary' + IntToStr(Random(999999));
  MPStream  := BuildMultipart(Boundary, 'user_data', aMediaFile);
  Client    := TFPHTTPClient.Create(nil);
  RespStream := TStringStream.Create('');
  try
    Client.AddHeader('Authorization', 'Bearer ' + ApiKey);
    Client.AddHeader('Content-Type', 'multipart/form-data; boundary=' + Boundary);
    Client.IOTimeout := ResponseTimeOut;
    Client.RequestBody := MPStream;
    try
      Client.HTTPMethod('POST', sUrl, RespStream, [200]);
      jObj := TJSONObject(GetJSON(RespStream.DataString));
      if Assigned(jObj) then
      try
        Result := JGetStr(jObj, 'id', '');
        if Result <> '' then
        begin
          aMediaFile.IdFile := Result;
          LFileName := JGetStr(jObj, 'filename', '');
          if LFileName <> '' then
            aMediaFile.CloudName := LFileName;
          LStatus := JGetStr(jObj, 'status', '');
          if LStatus <> '' then
            aMediaFile.CloudState := LStatus;
        end;
      finally
        jObj.Free;
      end;
    except
      on E: Exception do
        raise Exception.CreateFmt('Error Uploading File: %s', [E.Message]);
    end;
  finally
    RespStream.Free;
    MPStream.Free;
    Client.Free;
  end;
end;

// ===========================================================================
//  DeleteFile — DELETE /files/{id}
// ===========================================================================

function TAiOpenChat.DeleteFile(aMediaFile: TAiMediaFile): string;
var
  sUrl  : string;
  Resp  : string;
  jObj  : TJSONObject;
begin
  Result := '';
  if (aMediaFile = nil) or (aMediaFile.IdFile = '') then Exit;

  if Copy(Url, Length(Url), 1) = '/' then
    sUrl := Url + 'files/' + aMediaFile.IdFile
  else
    sUrl := Url + '/files/' + aMediaFile.IdFile;

  try
    Resp := DoHttpDelete(sUrl);
    jObj := TJSONObject(GetJSON(Resp));
    if Assigned(jObj) then
    try
      if JGetBool(jObj, 'deleted', False) then
      begin
        Result               := 'deleted';
        aMediaFile.IdFile    := '';
        aMediaFile.CloudState := '';
        aMediaFile.CloudName := '';
      end
      else
        Result := 'failed';
    finally
      jObj.Free;
    end;
  except
    on E: Exception do
      raise Exception.CreateFmt('Error Deleting File: %s', [E.Message]);
  end;
end;

// ===========================================================================
//  DownloadFile — GET /containers/{container_id}/files/{file_id}/content
// ===========================================================================

function TAiOpenChat.DownloadFile(aMediaFile: TAiMediaFile): string;
var
  Client    : TFPHTTPClient;
  LStream   : TMemoryStream;
  sUrl      : string;
begin
  Result := '';
  if (aMediaFile.IdFile = '') or (aMediaFile.CloudState = '') then Exit;

  // CloudState contiene el container_id
  if Copy(Url, Length(Url), 1) = '/' then
    sUrl := Url + Format('containers/%s/files/%s/content',
        [aMediaFile.CloudState, aMediaFile.IdFile])
  else
    sUrl := Url + Format('/containers/%s/files/%s/content',
        [aMediaFile.CloudState, aMediaFile.IdFile]);

  LStream := TMemoryStream.Create;
  Client  := TFPHTTPClient.Create(nil);
  try
    Client.AddHeader('Authorization', 'Bearer ' + ApiKey);
    Client.IOTimeout := ResponseTimeOut;
    try
      Client.HTTPMethod('GET', sUrl, LStream, [200]);
      if LStream.Size > 0 then
      begin
        LStream.Position := 0;
        aMediaFile.LoadFromStream(aMediaFile.FileName, LStream);
        Result := aMediaFile.FileName;
      end;
    except
      on E: Exception do
        LogDebug('DownloadFile error: ' + E.Message);
    end;
  finally
    LStream.Free;
    Client.Free;
  end;
end;

// ===========================================================================
//  DeleteAllUploadedFiles
// ===========================================================================

function TAiOpenChat.DeleteAllUploadedFiles: Integer;
var
  I, J  : Integer;
  Msg   : TAiChatMessage;
  Media : TAiMediaFile;
begin
  Result := 0;
  for I := 0 to FMessages.Count - 1 do
  begin
    Msg := FMessages[I];
    for J := 0 to Msg.MediaFiles.Count - 1 do
    begin
      Media := Msg.MediaFiles[J];
      if Media.IdFile <> '' then
      try
        if DeleteFile(Media) = 'deleted' then
          Inc(Result);
      except
        // Silencioso
      end;
    end;
  end;
end;

// ===========================================================================
//  GetToolsJson — arma el array de tools para el request
// ===========================================================================

function TAiOpenChat.GetToolsJson: TJSONArray;
var
  ToolsStr : string;
  JParsed  : TJSONData;
  JItem    : TJSONObject;
  I        : Integer;
  IntVal   : Integer;
  BoolVal  : Boolean;
  JWebOpts : TJSONObject;
  Key, Val : string;
  JCodeTool, JContainer, JFileIdsArr: TJSONObject;
begin
  Result := nil;

  // 1. Funciones externas
  if Tool_Active and Assigned(AiFunctions) then
  begin
    ToolsStr := AiFunctions.GetTools(TToolFormat.tfOpenAIResponses);
    if ToolsStr <> '' then
    begin
      JParsed := GetJSON(ToolsStr);
      if Assigned(JParsed) and (JParsed is TJSONArray) then
        Result := TJSONArray(JParsed)
      else
      begin
        FreeAndNil(JParsed);
        Result := TJSONArray.Create;
      end;
    end;
  end;

  if not Assigned(Result) then
    Result := TJSONArray.Create;

  // 2. Shell
  if tcm_Shell in ChatMediaSupports then
  begin
    JItem := TJSONObject.Create;
    JItem.Add('type', 'shell');
    Result.Add(JItem);
  end;

  // 3. Image generation
  if (Tcm_Image in ChatMediaSupports) and (Tfc_Image in NativeOutputFiles) then
  begin
    JItem := TJSONObject.Create;
    JItem.Add('type', 'image_generation');
    if ImageParams.Count > 0 then
    begin
      if ImageParams.Values['size'] <> '' then
        JItem.Add('size', ImageParams.Values['size']);
      if ImageParams.Values['quality'] <> '' then
        JItem.Add('quality', ImageParams.Values['quality']);
    end;
    Result.Add(JItem);
  end;

  // 4. Code Interpreter
  if Tcm_CodeInterpreter in ChatMediaSupports then
  begin
    JCodeTool := TJSONObject.Create;
    JCodeTool.Add('type', 'code_interpreter');
    JContainer := TJSONObject.Create;
    JContainer.Add('type', 'auto');
    JCodeTool.Add('container', JContainer);
    Result.Add(JCodeTool);
  end;

  // 5. Text editor (apply_patch)
  if tcm_TextEditor in ChatMediaSupports then
  begin
    JItem := TJSONObject.Create;
    JItem.Add('type', 'apply_patch');
    Result.Add(JItem);
  end;

  // 6. Web search
  if Tcm_WebSearch in ChatMediaSupports then
  begin
    JItem := TJSONObject.Create;
    JItem.Add('type', 'web_search');
    if WebSearchParams.Count > 0 then
    begin
      JWebOpts := TJSONObject.Create;
      for I := 0 to WebSearchParams.Count - 1 do
      begin
        Key := WebSearchParams.Names[I];
        Val := WebSearchParams.ValueFromIndex[I];
        if Key <> '' then
        begin
          if TryStrToInt(Val, IntVal) then
            JWebOpts.Add(Key, TJSONIntegerNumber.Create(IntVal))
          else if TryStrToBool(Val, BoolVal) then
            JWebOpts.Add(Key, TJSONBoolean.Create(BoolVal))
          else
            JWebOpts.Add(Key, Val);
        end;
      end;
      if JWebOpts.Count > 0 then
        JItem.Add('web_search', JWebOpts)
      else
        JWebOpts.Free;
    end;
    Result.Add(JItem);
  end;

  if Result.Count = 0 then
    FreeAndNil(Result);
end;

// ===========================================================================
//  AddMessageToInput — serializa un TAiChatMessage al formato Responses API
// ===========================================================================

procedure TAiOpenChat.AddMessageToInput(Msg: TAiChatMessage;
    TargetArray: TJSONArray);
var
  JUserObj     : TJSONObject;
  JContentArr  : TJSONArray;
  JTextObj     : TJSONObject;
  JImageObj    : TJSONObject;
  JDocObj      : TJSONObject;
  JToolOutObj  : TJSONObject;
  JPreBuilt    : TJSONObject;
  MediaFile    : TAiMediaFile;
  MediaArr     : TAiMediaFilesArray;
  MediaIdx     : Integer;
  PromptText   : string;
  IsSpecialAsst: Boolean;
begin
  // Detectar mensaje de asistente con JSON de shell/patch
  IsSpecialAsst := (Msg.Role = 'assistant') and
      (Copy(Trim(Msg.Prompt), 1, 1) = '{') and
      (ContainsStr(Msg.Prompt, '"type":"shell_call"') or
       ContainsStr(Msg.Prompt, '"type":"apply_patch_call"'));

  // Mensajes tool o assistant especiales: insertar JSON pre-construido
  if (Msg.Role = 'tool') or IsSpecialAsst then
  begin
    if (Copy(Trim(Msg.Prompt), 1, 1) = '{') and
       (ContainsStr(Msg.Prompt, '"type":"apply_patch_call_output"') or
        ContainsStr(Msg.Prompt, '"type":"shell_call_output"') or
        ContainsStr(Msg.Prompt, '"type":"shell_call"') or
        ContainsStr(Msg.Prompt, '"type":"apply_patch_call"')) then
    begin
      JPreBuilt := TJSONObject(GetJSON(Msg.Prompt));
      if Assigned(JPreBuilt) then
      begin
        TargetArray.Add(JPreBuilt);
        Exit;
      end;
    end;

    if Msg.Role = 'tool' then
    begin
      JToolOutObj := TJSONObject.Create;
      JToolOutObj.Add('type', 'function_call_output');
      if Msg.ToolCallId <> '' then
        JToolOutObj.Add('call_id', Msg.ToolCallId)
      else
        JToolOutObj.Add('call_id', 'call_unknown_' + IntToStr(Msg.id));
      JToolOutObj.Add('output', Msg.Prompt);
      TargetArray.Add(JToolOutObj);
      Exit;
    end;
  end;

  JUserObj := TJSONObject.Create;

  if Msg.Role = 'system' then
    JUserObj.Add('role', 'developer')
  else
    JUserObj.Add('role', Msg.Role);

  // Assistant: solo texto plano
  if Msg.Role = 'assistant' then
  begin
    JUserObj.Add('content', Msg.Prompt);
    TargetArray.Add(JUserObj);
    Exit;
  end;

  MediaArr   := Msg.MediaFiles.ToMediaFileArray;
  PromptText := Msg.Prompt;

  if Length(MediaArr) = 0 then
  begin
    JUserObj.Add('content', PromptText);
  end
  else
  begin
    JContentArr := TJSONArray.Create;

    for MediaIdx := 0 to Length(MediaArr) - 1 do
    begin
      MediaFile := MediaArr[MediaIdx];
      // Auto-upload si no tiene ID
      if (MediaFile.IdFile = '') and (MediaFile.Content.Size > 0) then
      try
        UploadFileToCache(MediaFile);
      except
        on E: Exception do
          LogDebug('Auto-upload error: ' + E.Message);
      end;

      case MediaFile.FileCategory of
        Tfc_Image:
        begin
          if Msg.Role = 'user' then
          begin
            JImageObj := TJSONObject.Create;
            JImageObj.Add('type', 'input_image');
            if (MediaFile.UrlMedia <> '') and
               (Copy(LowerCase(MediaFile.UrlMedia), 1, 5) <> 'data:') then
              JImageObj.Add('image_url', MediaFile.UrlMedia)
            else
              JImageObj.Add('image_url',
                'data:' + MediaFile.MimeType + ';base64,' + MediaFile.Base64);
            if MediaFile.Detail <> '' then
              JImageObj.Add('detail', MediaFile.Detail);
            JContentArr.Add(JImageObj);
          end;
        end;

        Tfc_pdf:
        begin
          JDocObj := TJSONObject.Create;
          JDocObj.Add('type', 'input_file');
          if MediaFile.IdFile <> '' then
            JDocObj.Add('file_id', MediaFile.IdFile)
          else if (MediaFile.UrlMedia <> '') and
              (Copy(LowerCase(MediaFile.UrlMedia), 1, 5) <> 'data:') then
            JDocObj.Add('file_url', MediaFile.UrlMedia)
          else
          begin
            if MediaFile.FileName <> '' then
              JDocObj.Add('filename', MediaFile.FileName)
            else
              JDocObj.Add('filename', 'document.pdf');
            JDocObj.Add('file_data',
              'data:' + MediaFile.MimeType + ';base64,' + MediaFile.Base64);
          end;
          JContentArr.Add(JDocObj);
        end;
      end;
    end;

    if PromptText <> '' then
    begin
      JTextObj := TJSONObject.Create;
      JTextObj.Add('type', 'input_text');
      JTextObj.Add('text', PromptText);
      JContentArr.Add(JTextObj);
    end;

    JUserObj.Add('content', JContentArr);
  end;

  TargetArray.Add(JUserObj);
end;

// ===========================================================================
//  InitChatCompletions — construye el JSON del request Responses API
// ===========================================================================

function TAiOpenChat.InitChatCompletions: string;
var
  JResult      : TJSONObject;
  JInputArray  : TJSONArray;
  JReasoning   : TJSONObject;
  JTextConfig  : TJSONObject;
  JFormatConfig: TJSONObject;
  JToolsArray  : TJSONArray;
  JToolChoice  : TJSONData;
  JSystemMsg   : string;
  LModel       : string;
  StartIndex   : Integer;
  LastMsg      : TAiChatMessage;
  IsToolLoop   : Boolean;
  I            : Integer;
  ItemMsg      : TAiChatMessage;
  sSchema      : string;
  JInnerSchema : TJSONObject;
  JProps       : TJSONObject;
  JReq         : TJSONArray;
  PropName     : string;
  Found        : Boolean;
  PropIdx, ReqIdx: Integer;
begin
  JResult := TJSONObject.Create;
  JFormatConfig := nil;
  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);

  try
    JResult.Add('model', LModel);

    if Asynchronous then
      JResult.Add('stream', TJSONBoolean.Create(True));

    // 1. Instructions (system prompt)
    JSystemMsg := '';
    if (FMessages.Count > 0) and (FMessages[0].Role = 'system') then
      JSystemMsg := FMessages[0].Prompt
    else if SystemPrompt.Text <> '' then
      JSystemMsg := SystemPrompt.Text;

    if Memory.Count > 0 then
      JSystemMsg := JSystemMsg + LineEnding + 'Memory context: ' + Memory.Text;

    if JSystemMsg <> '' then
      JResult.Add('instructions', JSystemMsg);

    // -----------------------------------------------------------------------
    // 2. INPUT — gestion de estado con previous_response_id
    // -----------------------------------------------------------------------
    JInputArray := TJSONArray.Create;
    StartIndex  := 0;

    if (FResponseId <> '') and (FMessages.Count > 0) then
    begin
      JResult.Add('previous_response_id', FResponseId);
      LastMsg    := FMessages[FMessages.Count - 1];
      IsToolLoop := (LastMsg.Role = 'tool') or
          ((LastMsg.Role = 'assistant') and
           ContainsStr(LastMsg.Prompt, '_output"'));

      if IsToolLoop then
      begin
        // En loop de herramientas: enviar solo los resultados posteriores
        for I := FMessages.Count - 1 downto 0 do
        begin
          if (FMessages[I].Role = 'user') or
             ((FMessages[I].Role = 'assistant') and
              (not ContainsStr(FMessages[I].Prompt, '_output"'))) then
          begin
            StartIndex := I + 1;
            Break;
          end;
        end;
      end
      else
      begin
        // Turno normal: enviar solo desde despues del ultimo assistant
        for I := FMessages.Count - 1 downto 0 do
        begin
          if (FMessages[I].Role = 'assistant') and
             (not ContainsStr(FMessages[I].Prompt, '_output"')) then
          begin
            StartIndex := I + 1;
            Break;
          end;
        end;
      end;
    end;

    for I := StartIndex to FMessages.Count - 1 do
    begin
      ItemMsg := FMessages[I];
      if ItemMsg.Role <> 'system' then
        AddMessageToInput(ItemMsg, JInputArray);
    end;

    if JInputArray.Count > 0 then
      JResult.Add('input', JInputArray)
    else
      JInputArray.Free;

    // 3. Parametros de configuracion
    JResult.Add('store', TJSONBoolean.Create(FStore));
    if FTruncation <> 'disabled' then
      JResult.Add('truncation', FTruncation);

    // Reasoning
    if (ThinkingLevel <> tlDefault) or (FReasoningSummary <> rsmDefault) then
    begin
      JReasoning := TJSONObject.Create;
      case ThinkingLevel of
        tlLow   : JReasoning.Add('effort', 'low');
        tlMedium: JReasoning.Add('effort', 'medium');
        tlHigh  : JReasoning.Add('effort', 'high');
      end;
      case FReasoningSummary of
        rsmAuto    : JReasoning.Add('summary', 'auto');
        rsmConcise : JReasoning.Add('summary', 'concise');
        rsmDetailed: JReasoning.Add('summary', 'detailed');
      end;
      JResult.Add('reasoning', JReasoning);
    end;

    // Text config (verbosity + format)
    JTextConfig := TJSONObject.Create;
    if FVerbosity <> '' then
      JTextConfig.Add('verbosity', FVerbosity);

    // Structured outputs (JSON Schema)
    if Response_format = tiaChatRfJsonSchema then
    begin
      JFormatConfig := TJSONObject.Create;
      JFormatConfig.Add('type', 'json_schema');
      if JsonSchema.Text <> '' then
      begin
        sSchema := StringReplace(JsonSchema.Text, '\n', ' ', [rfReplaceAll]);
        JInnerSchema := TJSONObject(GetJSON(sSchema));
        if Assigned(JInnerSchema) then
        begin
          // Garantizar additionalProperties y required completo
          if JGetStr(JInnerSchema, 'type', '') = 'object' then
          begin
            if JInnerSchema.IndexOfName('additionalProperties') < 0 then
              JInnerSchema.Add('additionalProperties', TJSONBoolean.Create(False));
            JProps := JGetObj(JInnerSchema, 'properties');
            if Assigned(JProps) then
            begin
              JReq := JGetArr(JInnerSchema, 'required');
              if not Assigned(JReq) then
              begin
                JReq := TJSONArray.Create;
                JInnerSchema.Add('required', JReq);
              end;
              for PropIdx := 0 to JProps.Count - 1 do
              begin
                PropName := JProps.Names[PropIdx];
                Found := False;
                for ReqIdx := 0 to JReq.Count - 1 do
                  if JReq.Items[ReqIdx].AsString = PropName then
                  begin
                    Found := True;
                    Break;
                  end;
                if not Found then
                  JReq.Add(PropName);
              end;
            end;
          end;
          JFormatConfig.Add('name', 'structured_response');
          JFormatConfig.Add('strict', TJSONBoolean.Create(True));
          JFormatConfig.Add('schema', JInnerSchema);
        end;
      end;
    end
    else if Response_format = tiaChatRfJson then
    begin
      JFormatConfig := TJSONObject.Create;
      JFormatConfig.Add('type', 'json_object');
    end;

    if Assigned(JFormatConfig) then
      JTextConfig.Add('format', JFormatConfig);

    if JTextConfig.Count > 0 then
      JResult.Add('text', JTextConfig)
    else
      JTextConfig.Free;

    // 4. Tools
    JToolsArray := GetToolsJson;
    if Assigned(JToolsArray) then
    begin
      if JToolsArray.Count > 0 then
      begin
        JResult.Add('tools', JToolsArray);
        if FParallel_ToolCalls then
          JResult.Add('parallel_tool_calls', TJSONBoolean.Create(True));
        if Tool_choice <> '' then
        begin
          if (Tool_choice = 'auto') or (Tool_choice = 'required') or
             (Tool_choice = 'none') then
            JResult.Add('tool_choice', Tool_choice)
          else
          begin
            JToolChoice := GetJSON(Tool_choice);
            if Assigned(JToolChoice) then
              JResult.Add('tool_choice', JToolChoice);
          end;
        end;
      end
      else
        JToolsArray.Free;
    end;

    Result := JResult.AsJSON;
  finally
    JResult.Free;
  end;
end;

// ===========================================================================
//  DoCallFunction — despacha tool calls
// ===========================================================================

procedure TAiOpenChat.DoCallFunction(ToolCall: TAiToolsFunction);
begin
  if not Assigned(AiFunctions) then
  begin
    if Assigned(OnCallToolFunction) then
      OnCallToolFunction(Self, ToolCall);
    Exit;
  end;

  if not AiFunctions.DoCallFunction(ToolCall) then
    if Assigned(OnCallToolFunction) then
      OnCallToolFunction(Self, ToolCall);
end;

// ===========================================================================
//  ParseChat — procesa respuesta sincrona (output array)
// ===========================================================================

procedure TAiOpenChat.ExecuteStandardToolCalls(JFunctionCalls: TJSONArray;
    ResMsg: TAiChatMessage);
var
  I         : Integer;
  JTC       : TJSONObject;
  ToolCall  : TAiToolsFunction;
  NewMsg    : TAiChatMessage;
  LArgObj   : TJSONObject;
  ArgIdx    : Integer;
begin
  for I := 0 to JFunctionCalls.Count - 1 do
  begin
    JTC      := TJSONObject(JFunctionCalls.Items[I]);
    ToolCall := TAiToolsFunction.Create;
    try
      ToolCall.Id        := JGetStr(JTC, 'call_id', JGetStr(JTC, 'id', ''));
      ToolCall.Name      := JGetStr(JTC, 'name', '');
      ToolCall.Arguments := JGetStr(JTC, 'arguments', '');
      ToolCall.ResMsg    := ResMsg;
      ToolCall.AskMsg    := GetLastMessage;

      // Poblar Params
      if ToolCall.Arguments <> '' then
      begin
        LArgObj := TJSONObject(GetJSON(ToolCall.Arguments));
        if Assigned(LArgObj) then
        try
          for ArgIdx := 0 to LArgObj.Count - 1 do
            ToolCall.Params.Values[LArgObj.Names[ArgIdx]] :=
                LArgObj.Items[ArgIdx].AsString;
        finally
          LArgObj.Free;
        end;
      end;

      DoCallFunction(ToolCall);

      NewMsg := TAiChatMessage.Create(ToolCall.Response, 'tool');
      NewMsg.ToolCallId          := ToolCall.Id;
      NewMsg.FunctionName        := ToolCall.Name;
      NewMsg.PreviousResponseId  := FResponseId;
      NewMsg.id                  := FMessages.Count + 1;
      FMessages.Add(NewMsg);
    finally
      ToolCall.Free;
    end;
  end;
end;

procedure TAiOpenChat.ParseSyncShellCall(JItem: TJSONObject);
var
  SCallId    : string;
  JAction    : TJSONObject;
  HistMsg    : TAiChatMessage;
  NewMsg     : TAiChatMessage;
  OutputJson : TJSONObject;
  JOutputArr : TJSONArray;
  JCmdRes    : TJSONObject;
  JOutcome   : TJSONObject;
  OutStd, OutErr: string;
  ExitCode   : Integer;
  Handled    : Boolean;
  JCommands  : TJSONArray;
  MaxLen     : Integer;
  C          : Integer;
  CmdStr     : string;
begin
  if not JTryGetStr(JItem, 'call_id', SCallId) then Exit;

  // Guardar la llamada en el historial
  HistMsg := TAiChatMessage.Create(JItem.AsJSON, 'assistant');
  HistMsg.ToolCallId         := SCallId;
  HistMsg.PreviousResponseId := FResponseId;
  HistMsg.id                 := FMessages.Count + 1;
  FMessages.Add(HistMsg);

  if not JTryGetObj(JItem, 'action', JAction) then Exit;

  JCommands := JGetArr(JAction, 'commands');
  MaxLen    := JGetInt(JAction, 'max_output_length', 0);

  JOutputArr := TJSONArray.Create;
  try
    if Assigned(JCommands) then
      for C := 0 to JCommands.Count - 1 do
      begin
        CmdStr   := JCommands.Items[C].AsString;
        OutStd   := '';
        OutErr   := '';
        ExitCode := 0;
        Handled  := False;

        if Assigned(FOnShellCommand) then
          FOnShellCommand(Self, CmdStr, SCallId, OutStd, OutErr, ExitCode, Handled);

        if not Handled then
        begin
          OutErr   := 'Shell execution denied (AllowAutoShell=False).';
          ExitCode := 126;
        end;

        if (MaxLen > 0) and (Length(OutStd) > MaxLen) then
          OutStd := Copy(OutStd, 1, MaxLen) + '... [truncated]';

        JCmdRes := TJSONObject.Create;
        JCmdRes.Add('stdout', OutStd);
        JCmdRes.Add('stderr', OutErr);
        JOutcome := TJSONObject.Create;
        JOutcome.Add('type', 'exit');
        JOutcome.Add('exit_code', ExitCode);
        JCmdRes.Add('outcome', JOutcome);
        JOutputArr.Add(JCmdRes);
      end;

    OutputJson := TJSONObject.Create;
    try
      OutputJson.Add('type', 'shell_call_output');
      OutputJson.Add('call_id', SCallId);
      if MaxLen > 0 then
        OutputJson.Add('max_output_length', MaxLen);
      OutputJson.Add('output', JOutputArr);
      JOutputArr := nil; // ownership transferred

      NewMsg := TAiChatMessage.Create(OutputJson.AsJSON, 'tool');
      NewMsg.ToolCallId         := SCallId;
      NewMsg.PreviousResponseId := FResponseId;
      NewMsg.id                 := FMessages.Count + 1;
      FMessages.Add(NewMsg);
    finally
      OutputJson.Free;
    end;
  finally
    if Assigned(JOutputArr) then
      JOutputArr.Free;
  end;
end;

procedure TAiOpenChat.ParseSyncApplyPatchCall(JItem: TJSONObject);
var
  SCallId     : string;
  JOp         : TJSONObject;
  OpType, OpPath, OpDiff: string;
  PatchStatus, PatchOutput: string;
  OutputJson  : TJSONObject;
  NewMsg      : TAiChatMessage;
begin
  if not JTryGetStr(JItem, 'call_id', SCallId) then Exit;

  OpType  := '';
  OpPath  := '';
  OpDiff  := '';
  if JTryGetObj(JItem, 'operation', JOp) then
  begin
    JTryGetStr(JOp, 'type', OpType);
    JTryGetStr(JOp, 'path', OpPath);
    JTryGetStr(JOp, 'diff', OpDiff);
  end;

  PatchStatus := 'failed';
  PatchOutput := '';

  if Assigned(FOnApplyPatch) then
  try
    FOnApplyPatch(Self, OpType, OpPath, OpDiff, SCallId, PatchStatus, PatchOutput);
  except
    on E: Exception do
    begin
      PatchStatus := 'failed';
      PatchOutput := 'Exception in OnApplyPatch: ' + E.Message;
    end;
  end;

  OutputJson := TJSONObject.Create;
  try
    OutputJson.Add('type', 'apply_patch_call_output');
    OutputJson.Add('call_id', SCallId);
    OutputJson.Add('status', PatchStatus);
    if PatchOutput <> '' then
      OutputJson.Add('output', PatchOutput);

    NewMsg := TAiChatMessage.Create(OutputJson.AsJSON, 'tool');
    NewMsg.ToolCallId         := SCallId;
    NewMsg.PreviousResponseId := FResponseId;
    NewMsg.id                 := FMessages.Count + 1;
    FMessages.Add(NewMsg);
  finally
    OutputJson.Free;
  end;
end;

procedure TAiOpenChat.ParseChat(jObj: TJSONObject; ResMsg: TAiChatMessage);
var
  JOutput       : TJSONArray;
  JItem         : TJSONObject;
  JContent      : TJSONArray;
  JAnnotations  : TJSONArray;
  JContentItem  : TJSONObject;
  JAnno         : TJSONObject;
  JUsage        : TJSONObject;
  JInputDetails : TJSONObject;
  JUsageDetails : TJSONObject;
  JIncomplete   : TJSONObject;
  JVal          : TJSONData;
  JFunctionCalls: TJSONArray;
  GeneratedFile : TAiMediaFile;
  WebItem       : TAiWebSearchItem;
  SType, SVal   : string;
  SId, SCallId  : string;
  HasShellOrPatch: Boolean;
  I, ContentIdx, AnnIdx : Integer;
  Code          : TMarkdownCodeExtractor;
  CodeFiles     : TCodeFileList;
  CodeFile      : TCodeFile;
  St            : TStringStream;
  MF            : TAiMediaFile;
begin
  // 1. Metadata inicial
  if JTryGetStr(jObj, 'id', SVal) then
    FResponseId := SVal;
  if JTryGetStr(jObj, 'status', SVal) then
    UpdateResponseStatus(SVal);
  if JTryGetStr(jObj, 'model', SVal) then
    ResMsg.Model := SVal;

  // Finish reason
  JVal := jObj.Find('incomplete_details');
  if Assigned(JVal) and (JVal is TJSONObject) then
  begin
    JIncomplete := TJSONObject(JVal);
    if JTryGetStr(JIncomplete, 'reason', SVal) then
      ResMsg.FinishReason := SVal;
  end
  else if FResponseStatus = 'completed' then
    ResMsg.FinishReason := 'stop';

  // Tokens
  if JTryGetObj(jObj, 'usage', JUsage) then
  begin
    ResMsg.Prompt_tokens     := JGetInt64(JUsage, 'input_tokens',  0);
    ResMsg.Completion_tokens := JGetInt64(JUsage, 'output_tokens', 0);
    ResMsg.Total_tokens      := JGetInt64(JUsage, 'total_tokens',  0);
    if JTryGetObj(JUsage, 'input_tokens_details', JInputDetails) then
      ResMsg.cached_tokens   := JGetInt64(JInputDetails, 'cached_tokens', 0);
    if JTryGetObj(JUsage, 'output_tokens_details', JUsageDetails) then
      ResMsg.Thinking_tokens := JGetInt64(JUsageDetails, 'reasoning_tokens', 0);
  end;

  // 2. Iterar el array 'output'
  FLastContent   := '';
  JFunctionCalls := TJSONArray.Create;
  HasShellOrPatch := False;

  try
    if JTryGetArr(jObj, 'output', JOutput) then
    begin
      for I := 0 to JOutput.Count - 1 do
      begin
        JItem := TJSONObject(JOutput.Items[I]);
        if not JTryGetStr(JItem, 'type', SType) then Continue;

        // A) Mensaje de texto del asistente
        if SType = 'message' then
        begin
          if JTryGetArr(JItem, 'content', JContent) then
          begin
            for ContentIdx := 0 to JContent.Count - 1 do
            begin
              JContentItem := TJSONObject(JContent.Items[ContentIdx]);
              if not JTryGetStr(JContentItem, 'type', SVal) then Continue;

              if SVal = 'output_text' then
              begin
                if JTryGetStr(JContentItem, 'text', SVal) then
                  FLastContent := FLastContent + SVal;

                // Anotaciones (citas web y archivos de code interpreter)
                if JTryGetArr(JContentItem, 'annotations', JAnnotations) then
                  for AnnIdx := 0 to JAnnotations.Count - 1 do
                  begin
                    JAnno := TJSONObject(JAnnotations.Items[AnnIdx]);
                    if not JTryGetStr(JAnno, 'type', SVal) then Continue;

                    if SVal = 'container_file_citation' then
                    begin
                      GeneratedFile := TAiMediaFile.Create;
                      if JTryGetStr(JAnno, 'file_id',  SId)   then GeneratedFile.IdFile   := SId;
                      if JTryGetStr(JAnno, 'filename', SVal)  then GeneratedFile.Filename := SVal;
                      JTryGetStr(JAnno, 'container_id', SCallId);
                      GeneratedFile.CloudState := SCallId;
                      try
                        DownloadFile(GeneratedFile);
                      except
                        on E: Exception do
                          LogDebug('DownloadFile error: ' + E.Message);
                      end;
                      ResMsg.MediaFiles.Add(GeneratedFile);
                    end
                    else if SVal = 'url_citation' then
                    begin
                      if ResMsg.WebSearchResponse = nil then
                        ResMsg.WebSearchResponse := TAiWebSearch.Create;
                      ResMsg.WebSearchResponse.&type := 'web_search_results';
                      WebItem := TAiWebSearchItem.Create;
                      WebItem.&type := 'url_citation';
                      JTryGetInt(JAnno, 'start_index', WebItem.start_index);
                      JTryGetInt(JAnno, 'end_index',   WebItem.end_index);
                      JTryGetStr(JAnno, 'url',   WebItem.Url);
                      JTryGetStr(JAnno, 'title', WebItem.Title);
                      ResMsg.WebSearchResponse.annotations.Add(WebItem);
                    end;
                  end;
              end
              else if SVal = 'refusal' then
              begin
                if JTryGetStr(JContentItem, 'refusal', SVal) then
                  FLastContent := FLastContent + ' [REFUSAL: ' + SVal + ']';
                ResMsg.IsRefusal := True;
              end;
            end;
          end;
        end

        // B) Imagen generada
        else if SType = 'image_generation_call' then
        begin
          SId := '';
          SVal := '';
          if not JTryGetStr(JItem, 'result', SVal) then
            JTryGetStr(JItem, 'image', SVal);
          if SVal <> '' then
          begin
            JTryGetStr(JItem, 'id', SId);
            if SId = '' then
              SId := 'gen_' + IntToStr(GetTickCount64);
            GeneratedFile := TAiMediaFile.Create;
            SCallId := '';
            JTryGetStr(JItem, 'revised_prompt', SCallId);
            GeneratedFile.LoadFromBase64(SId + '.png', SVal);
            GeneratedFile.Transcription := SCallId;
            GeneratedFile.IdFile        := SId;
            ResMsg.MediaFiles.Add(GeneratedFile);
            if ResMsg.Prompt = '' then
              ResMsg.Prompt := '[Image Generated] ' + Copy(SCallId, 1, 50);
          end;
        end

        // C) Function call
        else if SType = 'function_call' then
        begin
          JFunctionCalls.Add(TJSONObject(JItem.Clone));
        end

        // D) Shell call
        else if SType = 'shell_call' then
        begin
          ParseSyncShellCall(JItem);
          HasShellOrPatch := True;
        end

        // E) Apply patch call
        else if SType = 'apply_patch_call' then
        begin
          ParseSyncApplyPatchCall(JItem);
          HasShellOrPatch := True;
        end;
      end;
    end;

    // 3. Texto final + acumulacion de tokens al nivel de clase
    ResMsg.Prompt  := FLastContent;
    ResMsg.Content := FLastContent;
    ResMsg.PreviousResponseId := FResponseId;
    Prompt_tokens     := Prompt_tokens     + ResMsg.Prompt_tokens;
    Completion_tokens := Completion_tokens + ResMsg.Completion_tokens;
    Total_tokens      := Total_tokens      + ResMsg.Total_tokens;

    // 4. Function calls estandar
    if JFunctionCalls.Count > 0 then
    begin
      ExecuteStandardToolCalls(JFunctionCalls, ResMsg);
      Self.Run(nil, ResMsg);
    end
    else if HasShellOrPatch and (FLastContent = '') then
    begin
      // Recursion para shell/patch
      Self.Run(nil, ResMsg);
    end
    else
    begin
      // Flujo normal finalizado
      if tfc_ExtracttextFile in NativeOutputFiles then
      begin
        Code := TMarkdownCodeExtractor.Create;
        try
          CodeFiles := Code.ExtractCodeFiles(FLastContent);
          for CodeFile in CodeFiles do
          begin
            St := TStringStream.Create(CodeFile.Code);
            try
              St.Position := 0;
              MF := TAiMediaFile.Create;
              MF.LoadFromStream('file.' + CodeFile.FileType,
                  TMemoryStream(St)); // stub — TStringStream hereda
              ResMsg.MediaFiles.Add(MF);
            finally
              St.Free;
            end;
          end;
        finally
          Code.Free;
        end;
      end;

      DoProcessResponse(GetLastMessage, ResMsg, FLastContent);
      DoDataEnd(ResMsg, 'assistant', FLastContent, nil);
    end;

  finally
    JFunctionCalls.Free;
  end;
end;

// ===========================================================================
//  InternalRunCompletions — HTTP POST a /responses
// ===========================================================================

function TAiOpenChat.InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): string;
var
  ABody     : string;
  sUrl      : string;
  BodyStream: TStringStream;
  RespStream: TStringStream;
  Client    : TFPHTTPClient;
  jObj      : TJSONObject;
  HdrArr    : array of string;
begin
  Result := '';
  FBusy      := True;
  FAbort     := False;
  FLastError := '';
  FResponseStatus := '';

  // Asegurar que el mensaje del usuario esta en el historial
  if FMessages.IndexOf(AskMsg) < 0 then
  begin
    AskMsg.id := FMessages.Count + 1;
    FMessages.Add(AskMsg);
    if Assigned(OnAddMessage) then
      OnAddMessage(Self, AskMsg, nil, AskMsg.Role, AskMsg.Prompt);
  end;

  if Copy(Url, Length(Url), 1) = '/' then
    sUrl := Url + 'responses'
  else
    sUrl := Url + '/responses';

  ABody      := InitChatCompletions;
  BodyStream := TStringStream.Create(ABody);
  try
    DoStateChange(acsConnecting, 'Sending request...');
    LogDebug('--Request-Body--');
    LogDebug(ABody);

    if Asynchronous then
    begin
      SetLength(HdrArr, 4);
      HdrArr[0] := 'Authorization';
      HdrArr[1] := 'Bearer ' + ApiKey;
      HdrArr[2] := 'Content-Type';
      HdrArr[3] := 'application/json';
      StartHttpThread(sUrl, ABody, HdrArr);
    end
    else
    begin
      RespStream := TStringStream.Create('');
      Client     := TFPHTTPClient.Create(nil);
      try
        Client.AddHeader('Authorization', 'Bearer ' + ApiKey);
        Client.AddHeader('Content-Type', 'application/json');
        Client.IOTimeout   := ResponseTimeOut;
        Client.RequestBody := BodyStream;
        try
          Client.HTTPMethod('POST', sUrl, RespStream, [200]);
          LogDebug('--Response Sincrono--');
          LogDebug(RespStream.DataString);
          jObj := TJSONObject(GetJSON(RespStream.DataString));
          try
            FBusy := False;
            ParseChat(jObj, ResMsg);
            Result := FLastContent;
          finally
            FreeAndNil(jObj);
          end;
        except
          on E: Exception do
          begin
            FBusy      := False;
            FLastError := E.Message;
            DoError(E.Message, E);
          end;
        end;
      finally
        Client.Free;
        RespStream.Free;
      end;
    end;
  finally
    if not Asynchronous then
      BodyStream.Free
    else
      BodyStream.Free; // en async StartHttpThread ya tiene el body como string
  end;
end;

// ===========================================================================
//  ProcessSSELine — maneja el streaming de la Responses API
// ===========================================================================

procedure TAiOpenChat.ProcessSSEOutputItemDone(JItem: TJSONObject);
var
  ItemType   : string;
  ImgBase64  : string;
  ImgId      : string;
  ImgPrompt  : string;
  GenImg     : TAiMediaFile;
  ImgMsg     : TAiChatMessage;
  ToolCall   : TAiToolsFunction;
  NewMsg     : TAiChatMessage;
  BufferTool : TJSONObject;
  ToolName   : string;
  LArgObj    : TJSONObject;
  ArgIdx     : Integer;
  OutputIndex: Integer;
  CallId     : string;
  JAction    : TJSONObject;
  JContentArr: TJSONArray;
  JCPart     : TJSONObject;
  JAnnotations: TJSONArray;
  JAnno      : TJSONObject;
  AnnoType   : string;
  WebItem    : TAiWebSearchItem;
  CurMsg     : TAiChatMessage;
  GenFile    : TAiMediaFile;
  FuncName   : string;
  ItemId     : string;
  C          : Integer;
begin
  if not JTryGetStr(JItem, 'type', ItemType) then Exit;
  OutputIndex := -1; // Se asigna en los bucles for..in abajo

  // 1. Function call
  if ItemType = 'function_call' then
  begin
    // Recuperar del buffer por call_id
    CallId := JGetStr(JItem, 'call_id', JGetStr(JItem, 'id', ''));
    // Buscar en buffer por call_id
    BufferTool := nil;
    for OutputIndex in FTmpToolCallBuffer.Keys do
      if JGetStr(FTmpToolCallBuffer[OutputIndex], 'call_id', '') = CallId then
      begin
        BufferTool := FTmpToolCallBuffer[OutputIndex];
        Break;
      end;

    if not Assigned(BufferTool) then
    begin
      // Fallback: usar datos del item directamente
      ToolCall          := TAiToolsFunction.Create;
      ToolCall.Id       := JGetStr(JItem, 'call_id', JGetStr(JItem, 'id', ''));
      ToolCall.Name     := JGetStr(JItem, 'name', '');
      ToolCall.Arguments := JGetStr(JItem, 'arguments', '');
    end
    else
    begin
      ToolCall          := TAiToolsFunction.Create;
      ToolCall.Id       := JGetStr(BufferTool, 'call_id', '');
      ToolCall.Name     := JGetStr(BufferTool, 'name', '');
      ToolCall.Arguments := JGetStr(BufferTool, 'arguments', '');
      FTmpToolCallBuffer.Remove(OutputIndex);
    end;

    try
      ToolName := ToolCall.Name;
      DoStateChange(acsToolExecuting, 'Executing: ' + ToolName);

      if ToolCall.Arguments <> '' then
      begin
        LArgObj := TJSONObject(GetJSON(ToolCall.Arguments));
        if Assigned(LArgObj) then
        try
          for ArgIdx := 0 to LArgObj.Count - 1 do
            ToolCall.Params.Values[LArgObj.Names[ArgIdx]] :=
                LArgObj.Items[ArgIdx].AsString;
        finally
          LArgObj.Free;
        end;
      end;

      DoCallFunction(ToolCall);

      NewMsg := TAiChatMessage.Create(ToolCall.Response, 'tool');
      NewMsg.ToolCallId         := ToolCall.Id;
      NewMsg.FunctionName       := ToolCall.Name;
      NewMsg.PreviousResponseId := FResponseId;
      NewMsg.id                 := FMessages.Count + 1;
      FMessages.Add(NewMsg);
      FRecursionNeeded := True;
    finally
      ToolCall.Free;
    end;
  end

  // 2. Image generation
  else if ItemType = 'image_generation_call' then
  begin
    ImgBase64 := '';
    if not JTryGetStr(JItem, 'result', ImgBase64) then
      JTryGetStr(JItem, 'image', ImgBase64);
    if ImgBase64 <> '' then
    begin
      ImgId := '';
      JTryGetStr(JItem, 'id', ImgId);
      if ImgId = '' then
        ImgId := 'gen_' + IntToStr(GetTickCount64);
      ImgPrompt := '';
      JTryGetStr(JItem, 'revised_prompt', ImgPrompt);

      GenImg := TAiMediaFile.Create;
      GenImg.LoadFromBase64(ImgId + '.png', ImgBase64);
      GenImg.Transcription := ImgPrompt;
      GenImg.IdFile        := ImgId;

      ImgMsg := TAiChatMessage.Create('', 'assistant');
      ImgMsg.Prompt := '[Imagen Generada] ' + ImgPrompt;
      ImgMsg.MediaFiles.Add(GenImg);
      ImgMsg.id := FMessages.Count + 1;
      ImgMsg.PreviousResponseId := FResponseId;
      FMessages.Add(ImgMsg);

      DoDataEnd(ImgMsg, 'assistant', ImgMsg.Prompt, nil);
    end;
  end

  // 3. Message (citas web/archivos al final)
  else if ItemType = 'message' then
  begin
    CurMsg := GetLastMessage;
    if Assigned(CurMsg) and JTryGetArr(JItem, 'content', JContentArr) then
    begin
      for C := 0 to JContentArr.Count - 1 do
      begin
        JCPart := TJSONObject(JContentArr.Items[C]);
        if JTryGetArr(JCPart, 'annotations', JAnnotations) then
          for OutputIndex := 0 to JAnnotations.Count - 1 do
          begin
            JAnno    := TJSONObject(JAnnotations.Items[OutputIndex]);
            AnnoType := JGetStr(JAnno, 'type', '');

            if AnnoType = 'url_citation' then
            begin
              if CurMsg.WebSearchResponse = nil then
                CurMsg.WebSearchResponse := TAiWebSearch.Create;
              CurMsg.WebSearchResponse.&type := 'web_search_results';
              WebItem := TAiWebSearchItem.Create;
              WebItem.&type := 'url_citation';
              JTryGetStr(JAnno, 'url',   WebItem.Url);
              JTryGetStr(JAnno, 'title', WebItem.Title);
              CurMsg.WebSearchResponse.annotations.Add(WebItem);
            end
            else if AnnoType = 'container_file_citation' then
            begin
              GenFile := TAiMediaFile.Create;
              if JTryGetStr(JAnno, 'file_id',  ItemId) then GenFile.IdFile   := ItemId;
              if JTryGetStr(JAnno, 'filename', ItemId) then GenFile.Filename := ItemId;
              if JTryGetStr(JAnno, 'container_id', ItemId) then
                GenFile.CloudState := ItemId;
              try
                DownloadFile(GenFile);
              except end;
              CurMsg.MediaFiles.Add(GenFile);
            end;
          end;
      end;
    end;
  end;
end;

procedure TAiOpenChat.ProcessSSEFunctionCallDone(JItem: TJSONObject);
begin
  // Re-entrar a ProcessSSEOutputItemDone con el mismo item
  ProcessSSEOutputItemDone(JItem);
end;

procedure TAiOpenChat.ProcessSSELine(const ALine: string);
var
  Line       : string;
  DataStr    : string;
  EventType  : string;
  JsonEvent  : TJSONObject;
  JResp      : TJSONObject;
  JUsage     : TJSONObject;
  JInputDet  : TJSONObject;
  JUsageDet  : TJSONObject;
  JItem      : TJSONObject;
  BufferTool : TJSONObject;
  ItemType   : string;
  ItemId     : string;
  CallId     : string;
  FuncName   : string;
  DeltaVal   : string;
  OutputIndex: Integer;
  FinalMsg   : TAiChatMessage;
  NewStreamMsg: TAiChatMessage;
  OldArgs    : string;
  Code       : TMarkdownCodeExtractor;
  CodeFiles  : TCodeFileList;
  CodeFile   : TCodeFile;
  St         : TStringStream;
  MF         : TAiMediaFile;
begin
  if FAbort then
  begin
    FBusy := False;
    FTmpToolCallBuffer.Clear;
    DoDataEnd(nil, 'system', 'abort', nil);
    Exit;
  end;

  Line := ALine;
  if Line = '' then Exit;

  // Ignorar lineas que no sean data:
  if Copy(Line, 1, 5) <> 'data:' then Exit;

  DataStr := Trim(Copy(Line, 6, MaxInt));
  if DataStr = '' then Exit;
  if DataStr = '[DONE]' then Exit; // Responses API termina con response.completed

  JsonEvent := TJSONObject(GetJSON(DataStr));
  if not Assigned(JsonEvent) then Exit;
  try
    if not JTryGetStr(JsonEvent, 'type', EventType) then Exit;

    // -----------------------------------------------------------------
    // A) response.created — inicio de respuesta
    // -----------------------------------------------------------------
    if EventType = 'response.created' then
    begin
      if JTryGetObj(JsonEvent, 'response', JResp) then
        JTryGetStr(JResp, 'id', FResponseId);

      // Crear nuevo mensaje del asistente si el ultimo no lo es
      FinalMsg := GetLastMessage;
      if (FinalMsg = nil) or (FinalMsg.Role <> 'assistant') then
      begin
        NewStreamMsg := TAiChatMessage.Create('', 'assistant');
        NewStreamMsg.id := FMessages.Count + 1;
        NewStreamMsg.PreviousResponseId := FResponseId;
        FMessages.Add(NewStreamMsg);
      end;

      FRecursionNeeded := False;
      DoStateChange(acsCreated, 'Response ID: ' + FResponseId);
    end

    // -----------------------------------------------------------------
    // B) response.output_text.delta — fragmento de texto
    // -----------------------------------------------------------------
    else if EventType = 'response.output_text.delta' then
    begin
      if JTryGetStr(JsonEvent, 'delta', DeltaVal) then
      begin
        FLastContent := FLastContent + DeltaVal;
        DoData(nil, 'assistant', DeltaVal, nil);
      end;
    end

    // -----------------------------------------------------------------
    // C) response.output_item.added — nuevo item (function_call, reasoning)
    // -----------------------------------------------------------------
    else if EventType = 'response.output_item.added' then
    begin
      OutputIndex := JGetInt(JsonEvent, 'output_index', -1);
      if JTryGetObj(JsonEvent, 'item', JItem) and (OutputIndex >= 0) then
      begin
        JTryGetStr(JItem, 'type', ItemType);
        if ItemType = 'function_call' then
        begin
          DoStateChange(acsToolCalling, 'Detected tool call...');
          BufferTool := TJSONObject.Create;
          JTryGetStr(JItem, 'id',      ItemId);   if ItemId   <> '' then BufferTool.Add('id',      ItemId);
          JTryGetStr(JItem, 'call_id', CallId);   if CallId   <> '' then BufferTool.Add('call_id', CallId);
          JTryGetStr(JItem, 'name',    FuncName); if FuncName <> '' then BufferTool.Add('name',    FuncName);
          BufferTool.Add('arguments', '');
          FTmpToolCallBuffer.AddOrSetValue(OutputIndex, BufferTool);
        end
        else if ItemType = 'reasoning' then
          DoStateChange(acsReasoning, 'Thinking...');
      end;
    end

    // -----------------------------------------------------------------
    // D) response.function_call_arguments.delta — acumulacion de argumentos
    // -----------------------------------------------------------------
    else if EventType = 'response.function_call_arguments.delta' then
    begin
      OutputIndex := JGetInt(JsonEvent, 'output_index', -1);
      if JTryGetStr(JsonEvent, 'delta', DeltaVal) and (OutputIndex >= 0) then
      begin
        if FTmpToolCallBuffer.TryGetValue(OutputIndex, BufferTool) then
        begin
          OldArgs := JGetStr(BufferTool, 'arguments', '');
          BufferTool.Delete(BufferTool.IndexOfName('arguments'));
          BufferTool.Add('arguments', OldArgs + DeltaVal);
        end;
      end;
    end

    // -----------------------------------------------------------------
    // E) response.output_item.done — item completado (ejecutar tool, etc.)
    // -----------------------------------------------------------------
    else if EventType = 'response.output_item.done' then
    begin
      if JTryGetObj(JsonEvent, 'item', JItem) then
        ProcessSSEOutputItemDone(JItem);
    end

    // -----------------------------------------------------------------
    // F) response.completed — fin de respuesta
    // -----------------------------------------------------------------
    else if EventType = 'response.completed' then
    begin
      if FRecursionNeeded then
      begin
        // Enviar resultados de tools y continuar
        DoStateChange(acsConnecting, 'Sending tool results...');
        Self.Run(nil, nil);
      end
      else
      begin
        FBusy    := False;
        FinalMsg := GetLastMessage;

        // Tokens
        if JTryGetObj(JsonEvent, 'response', JResp) then
        begin
          if JTryGetObj(JResp, 'usage', JUsage) then
          begin
            if Assigned(FinalMsg) then
            begin
              FinalMsg.Total_tokens      := JGetInt64(JUsage, 'total_tokens',  0);
              FinalMsg.Prompt_tokens     := JGetInt64(JUsage, 'input_tokens',  0);
              FinalMsg.Completion_tokens := JGetInt64(JUsage, 'output_tokens', 0);
              if JTryGetObj(JUsage, 'input_tokens_details', JInputDet) then
                FinalMsg.cached_tokens   := JGetInt64(JInputDet, 'cached_tokens', 0);
              if JTryGetObj(JUsage, 'output_tokens_details', JUsageDet) then
                FinalMsg.Thinking_tokens := JGetInt64(JUsageDet, 'reasoning_tokens', 0);
            end;
          end;
          if JTryGetStr(JResp, 'model', DeltaVal) and Assigned(FinalMsg) then
            FinalMsg.Model := DeltaVal;
        end;

        // Extraccion de codigo
        if tfc_ExtracttextFile in NativeOutputFiles then
        begin
          Code := TMarkdownCodeExtractor.Create;
          try
            CodeFiles := Code.ExtractCodeFiles(FLastContent);
            for CodeFile in CodeFiles do
            begin
              St := TStringStream.Create(CodeFile.Code);
              try
                St.Position := 0;
                MF := TAiMediaFile.Create;
                MF.LoadFromStream('file.' + CodeFile.FileType,
                    TMemoryStream(St));
                if Assigned(FinalMsg) then
                  FinalMsg.MediaFiles.Add(MF);
              finally
                St.Free;
              end;
            end;
          finally
            Code.Free;
          end;
        end;

        // Acumular tokens al nivel de clase (leidos por Chat.Prompt_tokens etc.)
        if Assigned(FinalMsg) then
        begin
          Prompt_tokens     := Prompt_tokens     + FinalMsg.Prompt_tokens;
          Completion_tokens := Completion_tokens + FinalMsg.Completion_tokens;
          Total_tokens      := Total_tokens      + FinalMsg.Total_tokens;
        end;

        DoStateChange(acsFinished, 'Completed');
        DoDataEnd(FinalMsg, 'assistant', FLastContent, nil);
      end;
    end

    // -----------------------------------------------------------------
    // G) error
    // -----------------------------------------------------------------
    else if EventType = 'error' then
    begin
      FBusy := False;
      DoError('Responses API error: ' + DataStr, nil);
    end;

  finally
    JsonEvent.Free;
  end;
end;

// ===========================================================================
//  InternalRunSpeechGeneration — POST /audio/speech
// ===========================================================================

function TAiOpenChat.InternalRunSpeechGeneration(ResMsg, AskMsg: TAiChatMessage): string;
var
  sUrl         : string;
  LModel       : string;
  JBody        : TJSONObject;
  BodyStream   : TStringStream;
  RespStream   : TMemoryStream;
  Client       : TFPHTTPClient;
  NewAudioFile : TAiMediaFile;
  LFormat      : string;
begin
  Result := '';
  FBusy      := True;
  FLastError := '';
  FLastContent := '';
  FLastPrompt  := AskMsg.Prompt;

  if FMessages.IndexOf(AskMsg) < 0 then
  begin
    AskMsg.id := FMessages.Count + 1;
    FMessages.Add(AskMsg);
    if Assigned(OnAddMessage) then
      OnAddMessage(Self, AskMsg, nil, AskMsg.Role, AskMsg.Prompt);
  end;

  if Copy(Url, Length(Url), 1) = '/' then
    sUrl := Url + 'audio/speech'
  else
    sUrl := Url + '/audio/speech';

  LModel  := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);
  LFormat := voice_format;
  if LFormat = '' then LFormat := 'mp3';

  JBody := TJSONObject.Create;
  try
    JBody.Add('model', LModel);
    JBody.Add('input', AskMsg.Prompt);
    JBody.Add('voice', Voice);
    JBody.Add('response_format', LFormat);

    BodyStream := TStringStream.Create(JBody.AsJSON);
    RespStream := TMemoryStream.Create;
    Client     := TFPHTTPClient.Create(nil);
    try
      Client.AddHeader('Authorization', 'Bearer ' + ApiKey);
      Client.AddHeader('Content-Type', 'application/json');
      Client.IOTimeout   := ResponseTimeOut;
      Client.RequestBody := BodyStream;
      try
        Client.HTTPMethod('POST', sUrl, RespStream, [200]);
        NewAudioFile := TAiMediaFile.Create;
        RespStream.Position := 0;
        NewAudioFile.LoadFromStream('generated_audio.' + LFormat, RespStream);
        ResMsg.MediaFiles.Add(NewAudioFile);
        DoStateChange(acsFinished, 'Done');
        DoDataEnd(ResMsg, 'model', '', nil);
      except
        on E: Exception do
        begin
          FLastError := E.Message;
          DoError(FLastError, E);
        end;
      end;
    finally
      Client.Free;
      RespStream.Free;
      BodyStream.Free;
    end;
  finally
    JBody.Free;
  end;

  FBusy := False;
end;

// ===========================================================================
//  InternalRunTranscription — POST multipart a /audio/transcriptions
// ===========================================================================

function TAiOpenChat.InternalRunTranscription(aMediaFile: TAiMediaFile;
    ResMsg, AskMsg: TAiChatMessage): string;
var
  sUrl        : string;
  LModel      : string;
  Boundary    : string;
  MPStream    : TMemoryStream;
  RespStream  : TStringStream;
  Client      : TFPHTTPClient;
  jObj        : TJSONObject;
  STranscript : string;
  S           : string;
  NameBlock   : string;
  Granularity : string;
  Grans       : TStringList;
  I           : Integer;
  GranBound   : string;
begin
  Result := '';
  if not Assigned(aMediaFile) or (aMediaFile.Content.Size = 0) then
    raise Exception.Create('Se necesita un archivo de audio con contenido.');

  if Copy(Url, Length(Url), 1) = '/' then
    sUrl := Url + 'audio/transcriptions'
  else
    sUrl := Url + '/audio/transcriptions';

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);

  Boundary   := 'TxBoundary' + IntToStr(Random(999999));
  MPStream   := TMemoryStream.Create;
  RespStream := TStringStream.Create('');
  Client     := TFPHTTPClient.Create(nil);
  Grans      := TStringList.Create;
  try
    // --- Construir multipart manualmente ---
    // Campo: file
    NameBlock := aMediaFile.Filename;
    if NameBlock = '' then NameBlock := 'audio.mp3';
    S := '--' + Boundary + #13#10 +
         'Content-Disposition: form-data; name="file"; filename="' + NameBlock + '"' + #13#10 +
         'Content-Type: ' + aMediaFile.MimeType + #13#10 + #13#10;
    MPStream.WriteBuffer(PChar(S)^, Length(S));
    aMediaFile.Content.Position := 0;
    MPStream.CopyFrom(aMediaFile.Content, aMediaFile.Content.Size);
    S := #13#10;
    MPStream.WriteBuffer(PChar(S)^, Length(S));

    // Campo: model
    S := '--' + Boundary + #13#10 +
         'Content-Disposition: form-data; name="model"' + #13#10 + #13#10 +
         LModel + #13#10;
    MPStream.WriteBuffer(PChar(S)^, Length(S));

    // Campo: response_format
    if Transcription_ResponseFormat <> '' then
      S := Transcription_ResponseFormat
    else
      S := 'json';
    S := '--' + Boundary + #13#10 +
         'Content-Disposition: form-data; name="response_format"' + #13#10 + #13#10 +
         S + #13#10;
    MPStream.WriteBuffer(PChar(S)^, Length(S));

    // Campo: prompt (opcional)
    if AskMsg.Prompt <> '' then
    begin
      S := '--' + Boundary + #13#10 +
           'Content-Disposition: form-data; name="prompt"' + #13#10 + #13#10 +
           AskMsg.Prompt + #13#10;
      MPStream.WriteBuffer(PChar(S)^, Length(S));
    end;

    // Campo: language (opcional)
    if Language <> '' then
    begin
      S := '--' + Boundary + #13#10 +
           'Content-Disposition: form-data; name="language"' + #13#10 + #13#10 +
           Language + #13#10;
      MPStream.WriteBuffer(PChar(S)^, Length(S));
    end;

    // Granularidades (timestamp_granularities[])
    if Transcription_TimestampGranularities <> '' then
    begin
      Grans.CommaText := Transcription_TimestampGranularities;
      GranBound := Boundary;
      for I := 0 to Grans.Count - 1 do
      begin
        Granularity := Trim(Grans[I]);
        S := '--' + GranBound + #13#10 +
             'Content-Disposition: form-data; name="timestamp_granularities[]"' + #13#10 + #13#10 +
             Granularity + #13#10;
        MPStream.WriteBuffer(PChar(S)^, Length(S));
      end;
    end;

    // Cierre multipart
    S := '--' + Boundary + '--' + #13#10;
    MPStream.WriteBuffer(PChar(S)^, Length(S));
    MPStream.Position := 0;

    // --- Enviar ---
    Client.AddHeader('Authorization', 'Bearer ' + ApiKey);
    Client.AddHeader('Content-Type', 'multipart/form-data; boundary=' + Boundary);
    Client.IOTimeout   := ResponseTimeOut;
    Client.RequestBody := MPStream;
    try
      Client.HTTPMethod('POST', sUrl, RespStream, [200]);
      jObj := TJSONObject(GetJSON(RespStream.DataString));
      if Assigned(jObj) then
      try
        STranscript := JGetStr(jObj, 'text', RespStream.DataString);
        ResMsg.Prompt  := STranscript;
        ResMsg.Content := STranscript;
      finally
        jObj.Free;
      end
      else
      begin
        ResMsg.Prompt  := RespStream.DataString;
        ResMsg.Content := ResMsg.Prompt;
      end;
      Result := ResMsg.Prompt;
    except
      on E: Exception do
        raise Exception.CreateFmt('Error en transcripcion: %s', [E.Message]);
    end;
  finally
    Grans.Free;
    Client.Free;
    RespStream.Free;
    MPStream.Free;
  end;
end;

// ===========================================================================
//  InternalRunImageVideoGeneration — Sora (stub Fase 2)
// ===========================================================================

function TAiOpenChat.InternalRunImageVideoGeneration(ResMsg, AskMsg: TAiChatMessage): string;
begin
  Result := '';
  // Sora video generation: pendiente de implementar en Fase 2.
  // Requiere polling loop y TThread manual.
  DoError('InternalRunImageVideoGeneration (Sora): no implementado en Fase 1 FPC', nil);
end;

// ===========================================================================
//  initialization
// ===========================================================================

initialization
  TAiChatFactory.Instance.RegisterDriver(TAiOpenChat);

end.
