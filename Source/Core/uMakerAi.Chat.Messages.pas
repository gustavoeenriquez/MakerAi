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
// Adaptaciones respecto a la version Delphi:
//   - Rest.JSON (TJson.ObjectToJsonObject/JsonToObject) → serialización manual
//   - System.JSON → fpjson + jsonparser
//   - System.NetEncoding → EncdDecd
//   - System.SyncObjs → SyncObjs (TCriticalSection igual en FPC)
//   - TEncoding.UTF8 en TStringStream → eliminado (FPC usa string nativo)
//   - {$IF CompilerVersion} → eliminado
//   - TJSONObject.ParseJSONValue → GetJSON()
//   - JObj.GetValue<T>/TryGetValue<T> → JObj.Get()/IndexOfName
//   - JObj.AddPair → JObj.Add
//   - JArr.Format / JObj.Format → FormatJSON
//   - TObjectList<T> / TList<T> → specialize TObjectList<T> / TList<T>
//   - TDictionary<K,V> → specialize TDictionary<K,V>
//   - ValueNotify usa 'constref' en lugar de 'const'

unit uMakerAi.Chat.Messages;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Generics.Collections,
  SyncObjs,       // TCriticalSection
  EncdDecd,       // EncodeBase64
  fpjson,         // TJSONObject, TJSONArray, TJSONString, ...
  jsonparser,     // GetJSON()
  uMakerAi.Core;

type

  TAiMsgCitation  = class;
  TAiMsgCitations = class;
  TAiChatMessage  = class;

  // Tipos de fuente de citacion
  TAiCitationSourceType = (cstUnknown, cstDocument, cstWeb, cstFile, cstDatabase);

  // ---------------------------------------------------------------------------
  //  TAiToolsFunction — representa una llamada a herramienta (tool call)
  // ---------------------------------------------------------------------------
  TAiToolsFunction = class(TObject)
  public
    id         : string;
    Tipo       : string;
    name       : string;
    Description: string;
    Arguments  : string;        // Parámetros en forma de JSON string
    Params     : TStringList;   // Parámetros en forma name=value
    &Function  : string;        // Nombre de la función
    Response   : string;        // Respuesta que devuelve la función al LLM
    Body       : TJSONObject;   // Body JSON completo (para debug o info adicional)
    Metadata   : TAiMetadata;   // Metadatos adicionales
    AskMsg     : TAiChatMessage; // Mensaje que representa la pregunta
    ResMsg     : TAiChatMessage; // Mensaje que representa la respuesta

    constructor Create;
    destructor  Destroy; override;
    procedure ParseFunction(JObj: TJSONObject);
    procedure Assign(aSource: TAiToolsFunction);
  end;

  // ---------------------------------------------------------------------------
  //  TAiToolsFunctions — diccionario de tool calls indexado por nombre
  // ---------------------------------------------------------------------------
  TAiToolsFunctions = class(specialize TDictionary<string, TAiToolsFunction>)
  protected
    // FPC usa 'constref' en lugar de 'const' para el primer parámetro
    procedure ValueNotify(constref Value: TAiToolsFunction;
                          Action: TCollectionNotification); override;
  public
    function  ToOutputJson: TJSONArray;
    function  ToFunctionsJson: TJSONArray;
    procedure AddFunction(aBody: string); overload;
    procedure AddFunction(aBody: TJSONObject); overload;
  end;

  // ---------------------------------------------------------------------------
  //  TAiChatMessage — un mensaje individual del historial de chat
  // ---------------------------------------------------------------------------
  TAiChatMessage = class(TObject)
  private
    FPreviousResponseId : string;
    FWebSearchResponse  : TAiWebSearch;
    FReasoningContent   : string;
    FIsToolCallResponse : Boolean;
    FModel              : string;
    FCitations          : TAiMsgCitations;
    FStopReason         : string;
    FIsRefusal          : Boolean;
    FThinkingSignature  : string;
    FCacheControl       : Boolean;
    FThinking_tokens    : Integer;
    FFinishReason       : string;
    FCached_tokens      : Integer;
    FLock               : TCriticalSection;

    procedure SetContent(const Value: string);
    procedure SetRole(const Value: string);
    procedure SetPrompt(const Value: string);
    procedure SetFunctionName(const Value: string);
    procedure SetToolCallId(const Value: string);
    procedure SetTool_calls(const Value: string);
    procedure SetFId(const Value: Integer);
    procedure SetCompletion_tokens(const Value: Integer);
    procedure SetPrompt_tokens(const Value: Integer);
    procedure SetTotal_tokens(const Value: Integer);
    procedure SetFPreviousResponseId(const Value: string);
    procedure SetWebSearchResponse(const Value: TAiWebSearch);
    procedure SetReasoningContent(const Value: string);
    procedure SetIsToolCallResponse(const Value: Boolean);
    procedure SetModel(const Value: string);
    procedure SetCitations(const Value: TAiMsgCitations);
    procedure SetIsRefusal(const Value: Boolean);
    procedure SetStopReason(const Value: string);
    procedure SetThinking_tokens(const Value: Integer);
    procedure SetFinishReason(const Value: string);
    procedure SetCached_tokens(const Value: Integer);
  protected
    FRole             : string;
    FContent          : string;
    FPrompt           : string;
    FCompletion_tokens: Integer;
    FTotal_tokens     : Integer;
    FPrompt_tokens    : Integer;
    FId               : Integer;
    FToolCallId       : string;
    FFunctionName     : string;
    FTool_calls       : string;
    FMediaFiles       : TAiMediaFiles;
  public
    constructor Create(aPrompt, aRole: string;
                       aToolCallId: string = '';
                       aFunctionName: string = '');
    destructor Destroy; override;

    procedure AddMediaFile(aMediaFile: TAiMediaFile);
    procedure LoadMediaFromFile(aFileName: string);
    procedure LoadMediaFromStream(aFileName: string; Stream: TMemoryStream);
    procedure LoadMediaFromBase64(aFileName: string; aBase64: string);

    function HasUnprocessedItems: Boolean;
    function GetMediaTranscription: string;
    function StreamToBase64(Stream: TMemoryStream): string;
    function ToJson: TJSONArray;

    property id                : Integer         read FId               write SetFId;
    property Role              : string          read FRole             write SetRole;
    property Content           : string          read FContent          write SetContent;
    property Prompt            : string          read FPrompt           write SetPrompt;
    property Prompt_tokens     : Integer         read FPrompt_tokens    write SetPrompt_tokens;
    property Completion_tokens : Integer         read FCompletion_tokens write SetCompletion_tokens;
    property Total_tokens      : Integer         read FTotal_tokens     write SetTotal_tokens;
    property Thinking_tokens   : Integer         read FThinking_tokens  write SetThinking_tokens;
    property Cached_tokens     : Integer         read FCached_tokens    write SetCached_tokens;
    property Model             : string          read FModel            write SetModel;
    property ToolCallId        : string          read FToolCallId       write SetToolCallId;
    property FunctionName      : string          read FFunctionName     write SetFunctionName;
    property Tool_calls        : string          read FTool_calls       write SetTool_calls;
    property MediaFiles        : TAiMediaFiles   read FMediaFiles;
    property WebSearchResponse : TAiWebSearch    read FWebSearchResponse write SetWebSearchResponse;
    property PreviousResponseId: string          read FPreviousResponseId write SetFPreviousResponseId;
    property ReasoningContent  : string          read FReasoningContent write SetReasoningContent;
    property IsToolCallResponse: Boolean         read FIsToolCallResponse write SetIsToolCallResponse;
    property Citations         : TAiMsgCitations read FCitations        write SetCitations;
    property StopReason        : string          read FStopReason       write SetStopReason;
    property IsRefusal         : Boolean         read FIsRefusal        write SetIsRefusal;
    property ThinkingSignature : string          read FThinkingSignature write FThinkingSignature;
    property CacheControl      : Boolean         read FCacheControl     write FCacheControl;
    property FinishReason      : string          read FFinishReason     write SetFinishReason;
  end;

  // ---------------------------------------------------------------------------
  //  TAiChatMessages — lista de mensajes del historial
  //  Nota: TList<T> no es dueña de los objetos (igual que en Delphi original)
  // ---------------------------------------------------------------------------
  TAiChatMessages = class(specialize TList<TAiChatMessage>)
  private
    FNativeInputFiles: TAiFileCategories;
    function  GetAsText: string;
    procedure SetAsText(const Value: string);
    procedure SetNativeInputFiles(const Value: TAiFileCategories);
  public
    function  ToJson: TJSONArray;
    function  ExportChatHistory: TJSONObject;
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(FileName: string);
    property AsText          : string           read GetAsText         write SetAsText;
    property NativeInputFiles: TAiFileCategories read FNativeInputFiles write SetNativeInputFiles;
  end;

  // ---------------------------------------------------------------------------
  //  TAiSourceData — fuente de datos base para citaciones
  // ---------------------------------------------------------------------------
  TAiSourceData = class
  public
    id      : string;
    Title   : string;
    Content : string;
    Url     : string;
    Metadata: TAiMetadata;

    constructor Create;
    destructor  Destroy; override;
    procedure Assign(Source: TAiSourceData);
  end;

  // ---------------------------------------------------------------------------
  //  TAiCitationSource — fuente específica vinculada a una cita
  // ---------------------------------------------------------------------------
  TAiCitationSource = class
  public
    SourceType: TAiCitationSourceType;
    DataSource: TAiSourceData;

    constructor Create;
    destructor  Destroy; override;
    procedure Assign(Source: TAiCitationSource);
  end;

  // ---------------------------------------------------------------------------
  //  TAiMsgCitation — una cita dentro del texto de respuesta del modelo
  // ---------------------------------------------------------------------------
  TAiMsgCitation = class
  public
    StartIndex: Integer;
    EndIndex  : Integer;
    Text      : string;
    Sources   : specialize TObjectList<TAiCitationSource>;

    constructor Create;
    destructor  Destroy; override;
    procedure Assign(Source: TAiMsgCitation);
  end;

  // ---------------------------------------------------------------------------
  //  TAiMsgCitations — coleccion de citas de un mensaje
  // ---------------------------------------------------------------------------
  TAiMsgCitations = class(specialize TObjectList<TAiMsgCitation>)
  public
    procedure Assign(Source: TAiMsgCitations);
  end;

implementation

// ===========================================================================
//  Helpers locales para JSON (fpjson no tiene TryGetValue<T> como Delphi)
// ===========================================================================

// Retorna un string de un campo JSON, o aDefault si no existe
function JGetStr(JObj: TJSONObject; const Key, aDefault: string): string;
var
  Idx: Integer;
begin
  Idx := JObj.IndexOfName(Key);
  if Idx >= 0 then
    Result := JObj.Items[Idx].AsString
  else
    Result := aDefault;
end;

// Retorna un Boolean de un campo JSON, o aDefault si no existe
function JGetBool(JObj: TJSONObject; const Key: string; aDefault: Boolean): Boolean;
var
  Idx: Integer;
begin
  Idx := JObj.IndexOfName(Key);
  if Idx >= 0 then
    Result := JObj.Items[Idx].AsBoolean
  else
    Result := aDefault;
end;

// Retorna un Integer de un campo JSON, o aDefault si no existe
function JGetInt(JObj: TJSONObject; const Key: string; aDefault: Integer): Integer;
var
  Idx: Integer;
begin
  Idx := JObj.IndexOfName(Key);
  if Idx >= 0 then
    Result := JObj.Items[Idx].AsInteger
  else
    Result := aDefault;
end;

// Serialización manual de TAiChatMessage a TJSONObject (reemplaza TJson.ObjectToJsonObject)
function MessageToJsonObject(Msg: TAiChatMessage): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('role',               Msg.FRole);
  Result.Add('prompt',             Msg.FPrompt);
  Result.Add('content',            Msg.FContent);
  Result.Add('tool_call_id',       Msg.FToolCallId);
  Result.Add('function_name',      Msg.FFunctionName);
  Result.Add('tool_calls',         Msg.FTool_calls);
  Result.Add('reasoning_content',  Msg.FReasoningContent);
  Result.Add('model',              Msg.FModel);
  Result.Add('prompt_tokens',      Msg.FPrompt_tokens);
  Result.Add('completion_tokens',  Msg.FCompletion_tokens);
  Result.Add('total_tokens',       Msg.FTotal_tokens);
  Result.Add('thinking_tokens',    Msg.FThinking_tokens);
  Result.Add('cached_tokens',      Msg.FCached_tokens);
  Result.Add('finish_reason',      Msg.FFinishReason);
  Result.Add('stop_reason',        Msg.FStopReason);
  Result.Add('is_tool_call_resp',  Msg.FIsToolCallResponse);
  Result.Add('is_refusal',         Msg.FIsRefusal);
  Result.Add('cache_control',      Msg.FCacheControl);
  Result.Add('prev_response_id',   Msg.FPreviousResponseId);
end;

// Deserialización manual de TJSONObject a TAiChatMessage (reemplaza TJson.JsonToObject)
function MessageFromJsonObject(JItem: TJSONObject): TAiChatMessage;
begin
  Result := TAiChatMessage.Create(
    JGetStr(JItem, 'prompt', ''),
    JGetStr(JItem, 'role', ''),
    JGetStr(JItem, 'tool_call_id', ''),
    JGetStr(JItem, 'function_name', '')
  );
  Result.FContent            := JGetStr(JItem,  'content',           '');
  Result.FTool_calls         := JGetStr(JItem,  'tool_calls',        '');
  Result.FReasoningContent   := JGetStr(JItem,  'reasoning_content', '');
  Result.FModel              := JGetStr(JItem,  'model',             '');
  Result.FPrompt_tokens      := JGetInt(JItem,  'prompt_tokens',     0);
  Result.FCompletion_tokens  := JGetInt(JItem,  'completion_tokens', 0);
  Result.FTotal_tokens       := JGetInt(JItem,  'total_tokens',      0);
  Result.FThinking_tokens    := JGetInt(JItem,  'thinking_tokens',   0);
  Result.FCached_tokens      := JGetInt(JItem,  'cached_tokens',     0);
  Result.FFinishReason       := JGetStr(JItem,  'finish_reason',     '');
  Result.FStopReason         := JGetStr(JItem,  'stop_reason',       '');
  Result.FIsToolCallResponse := JGetBool(JItem, 'is_tool_call_resp', False);
  Result.FIsRefusal          := JGetBool(JItem, 'is_refusal',        False);
  Result.FCacheControl       := JGetBool(JItem, 'cache_control',     False);
  Result.FPreviousResponseId := JGetStr(JItem,  'prev_response_id',  '');
end;

// ===========================================================================
//  TAiChatMessage
// ===========================================================================

constructor TAiChatMessage.Create(aPrompt, aRole: string;
                                  aToolCallId: string;
                                  aFunctionName: string);
begin
  inherited Create;
  FLock               := TCriticalSection.Create;
  FRole               := aRole;
  FPrompt             := aPrompt;
  FFunctionName       := aFunctionName;
  FToolCallId         := aToolCallId;
  FMediaFiles         := TAiMediaFiles.Create;
  FWebSearchResponse  := TAiWebSearch.Create;
  FPreviousResponseId := '';
  FCitations          := TAiMsgCitations.Create(True); // dueña de los objetos
  FCacheControl       := False;
end;

destructor TAiChatMessage.Destroy;
begin
  FMediaFiles.Free;
  FCitations.Free;
  FWebSearchResponse.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TAiChatMessage.AddMediaFile(aMediaFile: TAiMediaFile);
begin
  FLock.Enter;
  try
    FMediaFiles.Add(aMediaFile);
  finally
    FLock.Leave;
  end;
end;

function TAiChatMessage.GetMediaTranscription: string;
var
  MF: TAiMediaFile;
begin
  FLock.Enter;
  try
    Result := '';
    for MF in MediaFiles do
      if MF.Procesado then
        Result := Trim(Result + LineEnding + MF.Transcription);
  finally
    FLock.Leave;
  end;
end;

function TAiChatMessage.HasUnprocessedItems: Boolean;
var
  MF: TAiMediaFile;
begin
  FLock.Enter;
  try
    Result := False;
    for MF in MediaFiles do
    begin
      if not MF.Procesado then
      begin
        Result := True;
        Break;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TAiChatMessage.LoadMediaFromBase64(aFileName: string; aBase64: string);
var
  Media: TAiMediaFile;
begin
  FLock.Enter;
  try
    if Length(Trim(aBase64)) < 100 then
      raise Exception.Create('El Base64 está vacío, no se cargará');
    if aFileName = '' then
      aFileName := 'imagen.jpg';
    Media := TAiMediaFile.Create;
    Media.LoadFromBase64(aFileName, aBase64);
    AddMediaFile(Media);
  finally
    FLock.Leave;
  end;
end;

procedure TAiChatMessage.LoadMediaFromFile(aFileName: string);
var
  Media: TAiMediaFile;
begin
  FLock.Enter;
  try
    if not FileExists(aFileName) then
      raise Exception.Create('El archivo "' + aFileName + '" no se encuentra');
    Media := TAiMediaFile.Create;
    Media.LoadFromFile(aFileName);
    AddMediaFile(Media);
  finally
    FLock.Leave;
  end;
end;

procedure TAiChatMessage.LoadMediaFromStream(aFileName: string; Stream: TMemoryStream);
var
  Media: TAiMediaFile;
begin
  FLock.Enter;
  try
    if Stream.Size <= 100 then
      raise Exception.Create('El stream está vacío');
    if aFileName = '' then
      aFileName := 'imagen.jpg';
    Media := TAiMediaFile.Create;
    Media.LoadFromStream(aFileName, Stream);
    Self.AddMediaFile(Media);
  finally
    FLock.Leave;
  end;
end;

function TAiChatMessage.StreamToBase64(Stream: TMemoryStream): string;
begin
  FLock.Enter;
  try
    Stream.Position := 0;
    Result := EncodeBase64(Stream.Memory, Stream.Size);
    Result := StringReplace(Result, LineEnding, '', [rfReplaceAll]);
    Result := StringReplace(Result, #10, '', [rfReplaceAll]);
  finally
    FLock.Leave;
  end;
end;

function TAiChatMessage.ToJson: TJSONArray;
// Solo toma el mensaje actual con la primera imagen que encuentra.
// Se usa en modelos que solo aceptan una imagen por petición.
var
  Msg        : TAiChatMessage;
  JObj, JMsg : TJSONObject;
  JContent   : TJSONArray;
  Base64, Mime: string;
  MediaArr   : TAiMediaFilesArray;
  JToolCalls : TJSONData;
begin
  FLock.Enter;
  try
    Result := TJSONArray.Create;
    Msg    := Self;
    JObj   := TJSONObject.Create;
    JObj.Add('role', Msg.FRole);

    if Msg.FToolCallId <> '' then
      JObj.Add('tool_call_id', Msg.FToolCallId);

    if Msg.FFunctionName <> '' then
      JObj.Add('name', Msg.FFunctionName);

    MediaArr := Msg.MediaFiles.GetMediaList([Tfc_Image], False);

    if Length(MediaArr) > 0 then
    begin
      JContent := TJSONArray.Create;
      JMsg     := TJSONObject.Create;
      JMsg.Add('type', 'text');
      JMsg.Add('text', Msg.FPrompt);
      JContent.Add(JMsg);

      if Msg.MediaFiles.Count > 0 then
      begin
        Base64 := Msg.MediaFiles[0].Base64;
        Mime   := Msg.MediaFiles[0].MimeType;

        // fpjson: parsear el JSON del payload de imagen
        JToolCalls := GetJSON(
          '{"type":"image_url","image_url":{"url":"data:' + Mime +
          ';base64,' + Base64 + '"}}');
        JContent.Add(JToolCalls);
      end;

      JObj.Add('content', JContent);
    end
    else
      JObj.Add('content', Msg.FPrompt);

    if Msg.FTool_calls <> '' then
    begin
      // fpjson: parsear el JSON array de tool_calls
      JToolCalls := GetJSON(Msg.FTool_calls);
      if JToolCalls is TJSONArray then
        JObj.Add('tool_calls', TJSONArray(JToolCalls))
      else
        JToolCalls.Free;
    end;

    Result.Add(JObj);
  finally
    FLock.Leave;
  end;
end;

// Setters — todos protegidos por FLock

procedure TAiChatMessage.SetContent(const Value: string);
begin FLock.Enter; try FContent := Value; finally FLock.Leave; end; end;

procedure TAiChatMessage.SetRole(const Value: string);
begin FLock.Enter; try FRole := Value; finally FLock.Leave; end; end;

procedure TAiChatMessage.SetPrompt(const Value: string);
begin FLock.Enter; try FPrompt := Value; finally FLock.Leave; end; end;

procedure TAiChatMessage.SetFunctionName(const Value: string);
begin FLock.Enter; try FFunctionName := Value; finally FLock.Leave; end; end;

procedure TAiChatMessage.SetToolCallId(const Value: string);
begin FLock.Enter; try FToolCallId := Value; finally FLock.Leave; end; end;

procedure TAiChatMessage.SetTool_calls(const Value: string);
begin FLock.Enter; try FTool_calls := Value; finally FLock.Leave; end; end;

procedure TAiChatMessage.SetFId(const Value: Integer);
begin FLock.Enter; try FId := Value; finally FLock.Leave; end; end;

procedure TAiChatMessage.SetCompletion_tokens(const Value: Integer);
begin FLock.Enter; try FCompletion_tokens := Value; finally FLock.Leave; end; end;

procedure TAiChatMessage.SetPrompt_tokens(const Value: Integer);
begin FLock.Enter; try FPrompt_tokens := Value; finally FLock.Leave; end; end;

procedure TAiChatMessage.SetTotal_tokens(const Value: Integer);
begin FLock.Enter; try FTotal_tokens := Value; finally FLock.Leave; end; end;

procedure TAiChatMessage.SetThinking_tokens(const Value: Integer);
begin FLock.Enter; try FThinking_tokens := Value; finally FLock.Leave; end; end;

procedure TAiChatMessage.SetCached_tokens(const Value: Integer);
begin FLock.Enter; try FCached_tokens := Value; finally FLock.Leave; end; end;

procedure TAiChatMessage.SetModel(const Value: string);
begin FLock.Enter; try FModel := Value; finally FLock.Leave; end; end;

procedure TAiChatMessage.SetFinishReason(const Value: string);
begin FLock.Enter; try FFinishReason := Value; finally FLock.Leave; end; end;

procedure TAiChatMessage.SetStopReason(const Value: string);
begin FLock.Enter; try FStopReason := Value; finally FLock.Leave; end; end;

procedure TAiChatMessage.SetReasoningContent(const Value: string);
begin FLock.Enter; try FReasoningContent := Value; finally FLock.Leave; end; end;

procedure TAiChatMessage.SetIsToolCallResponse(const Value: Boolean);
begin FLock.Enter; try FIsToolCallResponse := Value; finally FLock.Leave; end; end;

procedure TAiChatMessage.SetIsRefusal(const Value: Boolean);
begin FLock.Enter; try FIsRefusal := Value; finally FLock.Leave; end; end;

procedure TAiChatMessage.SetFPreviousResponseId(const Value: string);
begin FLock.Enter; try FPreviousResponseId := Value; finally FLock.Leave; end; end;

procedure TAiChatMessage.SetWebSearchResponse(const Value: TAiWebSearch);
begin
  FLock.Enter;
  try
    if FWebSearchResponse <> Value then
    begin
      FWebSearchResponse.Free;
      FWebSearchResponse := Value;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TAiChatMessage.SetCitations(const Value: TAiMsgCitations);
var
  SourceCitation: TAiMsgCitation;
  NewCitation   : TAiMsgCitation;
begin
  FLock.Enter;
  try
    FCitations.Clear;
    if not Assigned(Value) or (Value.Count = 0) then
      Exit;
    for SourceCitation in Value do
    begin
      NewCitation := TAiMsgCitation.Create;
      try
        NewCitation.Assign(SourceCitation);
        FCitations.Add(NewCitation);
      except
        NewCitation.Free;
        raise;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

// ===========================================================================
//  TAiChatMessages
// ===========================================================================

function TAiChatMessages.ExportChatHistory: TJSONObject;
var
  JArr : TJSONArray;
  JItem: TJSONObject;
  I    : Integer;
  Item : TAiChatMessage;
begin
  Result := TJSONObject.Create;
  JArr   := TJSONArray.Create;

  Result.Add('model', 'MakerAiChat');
  Result.Add('type',  'Messages');
  Result.Add('ver',   '1.0');

  for I := 0 to Count - 1 do
  begin
    Item  := Items[I];
    JItem := TJSONObject.Create;
    JItem.Add('role', Item.Role);
    if Trim(Item.Content) <> '' then
      JItem.Add('request', Item.Content)
    else
      JItem.Add('prompt', Item.Prompt);
    JArr.Add(JItem);
  end;

  Result.Add('data', JArr);
end;

function TAiChatMessages.GetAsText: string;
var
  St: TStringStream;
begin
  St := TStringStream.Create('');
  try
    SaveToStream(St);
    Result := St.DataString;
  finally
    St.Free;
  end;
end;

procedure TAiChatMessages.LoadFromFile(FileName: string);
var
  St: TStringStream;
begin
  St := TStringStream.Create('');
  try
    if FileExists(FileName) then
    begin
      St.LoadFromFile(FileName);
      St.Position := 0;
      LoadFromStream(St);
    end;
  finally
    St.Free;
  end;
end;

procedure TAiChatMessages.LoadFromStream(Stream: TStream);
var
  JVal : TJSONData;
  JObj : TJSONObject;
  JArr : TJSONArray;
  sJson, Model: string;
  St   : TStringStream;
  Item : TAiChatMessage;
  I    : Integer;
begin
  St := TStringStream.Create('');
  try
    St.CopyFrom(Stream, 0);
    sJson := St.DataString;
  finally
    St.Free;
  end;

  JVal := GetJSON(sJson);
  if not Assigned(JVal) then
    Exit;
  try
    if not (JVal is TJSONObject) then
      Exit;
    JObj  := TJSONObject(JVal);
    Model := JGetStr(JObj, 'model', '');

    if Model = 'MakerAiChat' then  // compatibilidad con formato guardado por SaveToStream
    begin
      if JObj.IndexOfName('data') < 0 then
        Exit;
      JArr := TJSONArray(JObj.Find('data'));
      if not Assigned(JArr) then
        Exit;
      if JArr.Count > 0 then
        Self.Clear;
      for I := 0 to JArr.Count - 1 do
      begin
        if JArr.Items[I] is TJSONObject then
        begin
          Item := MessageFromJsonObject(TJSONObject(JArr.Items[I]));
          Self.Add(Item);
        end;
      end;
    end;
  finally
    JVal.Free;
  end;
end;

procedure TAiChatMessages.SaveToFile(FileName: string);
var
  St: TStringStream;
begin
  St := TStringStream.Create('');
  try
    Self.SaveToStream(St);
    St.Position := 0;
    St.SaveToFile(FileName);
  finally
    St.Free;
  end;
end;

procedure TAiChatMessages.SaveToStream(Stream: TStream);
var
  JObj : TJSONObject;
  JArr : TJSONArray;
  JItem: TJSONObject;
  St   : TStringStream;
  I    : Integer;
  Item : TAiChatMessage;
  sJson: string;
begin
  JObj := TJSONObject.Create;
  JArr := TJSONArray.Create;
  try
    JObj.Add('model', 'MakerAiChat');
    JObj.Add('type',  'Messages');
    JObj.Add('ver',   '1.0');

    for I := 0 to Count - 1 do
    begin
      Item  := Items[I];
      JItem := MessageToJsonObject(Item);
      JArr.Add(JItem);
    end;

    JObj.Add('data', JArr);
    sJson := JObj.FormatJSON;
  finally
    JObj.Free;
  end;

  St := TStringStream.Create(sJson);
  try
    Stream.CopyFrom(St, 0);
  finally
    St.Free;
  end;
end;

procedure TAiChatMessages.SetAsText(const Value: string);
var
  St: TStringStream;
begin
  if Trim(Value) = '' then
    Exit;
  St := TStringStream.Create(Value);
  try
    St.Position := 0;
    LoadFromStream(St);
  finally
    St.Free;
  end;
end;

procedure TAiChatMessages.SetNativeInputFiles(const Value: TAiFileCategories);
begin
  FNativeInputFiles := Value;
end;

function TAiChatMessages.ToJson: TJSONArray;
var
  I, J       : Integer;
  Msg        : TAiChatMessage;
  JObj, JMsg : TJSONObject;
  JObjImg, jImgUrl, jAudio: TJSONObject;
  JContent   : TJSONArray;
  Base64, Mime: string;
  MediaArr   : TAiMediaFilesArray;
  MediaFile  : TAiMediaFile;
  S          : string;
  JToolCalls : TJSONData;
begin
  Result := TJSONArray.Create;

  for I := 0 to Count - 1 do
  begin
    Msg  := Items[I];
    JObj := TJSONObject.Create;
    JObj.Add('role', Msg.FRole);

    if Msg.FToolCallId <> '' then
      JObj.Add('tool_call_id', Msg.FToolCallId);

    if Msg.FFunctionName <> '' then
      JObj.Add('name', Msg.FFunctionName);

    // Selecciona los archivos de media según el filtro configurado
    MediaArr := Msg.MediaFiles.GetMediaList(FNativeInputFiles, False);

    if Length(MediaArr) > 0 then
    begin
      JContent := TJSONArray.Create;
      JMsg     := TJSONObject.Create;
      JMsg.Add('type', 'text');
      JMsg.Add('text', Msg.FPrompt);
      JContent.Add(JMsg);

      for J := 0 to Length(MediaArr) - 1 do
      begin
        MediaFile := MediaArr[J];

        case MediaFile.FileCategory of

          Tfc_Image:
            begin
              Base64 := MediaFile.Base64;
              Mime   := MediaFile.MimeType;
              S      := 'data:' + Mime + ';base64,' + Base64;

              jImgUrl := TJSONObject.Create;
              jImgUrl.Add('url', S);
              if MediaFile.Detail <> '' then
                jImgUrl.Add('detail', MediaFile.Detail);

              JObjImg := TJSONObject.Create;
              JObjImg.Add('type', 'image_url');
              JObjImg.Add('image_url', jImgUrl);
              JContent.Add(JObjImg);
            end;

          Tfc_Audio:
            begin
              if MediaFile.IdAudio <> '' then
              begin
                // Audio generado por el modelo (referencia por ID)
                jAudio := TJSONObject.Create;
                jAudio.Add('id', MediaFile.IdAudio);
                JObj.Add('audio', jAudio);
              end
              else
              begin
                // Audio del usuario (inline base64)
                jAudio := TJSONObject.Create;
                jAudio.Add('data',   MediaFile.Base64);
                jAudio.Add('format', StringReplace(
                               MediaFile.MimeType, 'audio/', '', [rfReplaceAll]));

                JContent := TJSONArray.Create;
                JMsg     := TJSONObject.Create;
                JMsg.Add('type',        'input_audio');
                JMsg.Add('input_audio', jAudio);
                JContent.Add(JMsg);
                JObj.Add('content', JContent);
              end;
            end;

          // Tipos no implementados aún en la versión base:
          Tfc_Video, Tfc_Pdf, Tfc_Document, Tfc_Text,
          Tfc_CalcSheet, Tfc_Presentation, Tfc_CompressFile,
          Tfc_Web, Tfc_GraphicDesign, Tfc_Unknown: ; // sin operación

        else
          JObj.Add('content', Msg.FPrompt);
        end;
      end;

      // TODO: si hay imágenes + audio del usuario en el mismo mensaje, el
      // JContent de imágenes se pierde porque Tfc_Audio crea un nuevo JContent.
      // Requiere rediseñar la lógica de media mixta.
      JObj.Add('content', JContent);
    end
    else
      JObj.Add('content', Msg.FPrompt);

    // tool_calls: parsear el JSON string y embeber el array
    if Msg.FTool_calls <> '' then
    begin
      JToolCalls := GetJSON(Msg.FTool_calls);
      if JToolCalls is TJSONArray then
        JObj.Add('tool_calls', TJSONArray(JToolCalls))
      else
        JToolCalls.Free;
    end;

    // reasoning_content: requerido por DeepSeek, Kimi, etc. cuando thinking está activo
    if Msg.FReasoningContent <> '' then
      JObj.Add('reasoning_content', Msg.FReasoningContent);

    Result.Add(JObj);
  end;
end;

// ===========================================================================
//  TAiSourceData
// ===========================================================================

constructor TAiSourceData.Create;
begin
  inherited Create;
  Metadata := TAiMetadata.Create;
end;

destructor TAiSourceData.Destroy;
begin
  Metadata.Free;
  inherited Destroy;
end;

procedure TAiSourceData.Assign(Source: TAiSourceData);
var
  Key: string;
begin
  if not Assigned(Source) or (Source = Self) then
    Exit;
  Self.id      := Source.id;
  Self.Title   := Source.Title;
  Self.Content := Source.Content;
  Self.Url     := Source.Url;
  Self.Metadata.Clear;
  for Key in Source.Metadata.Keys do
    Self.Metadata.Add(Key, Source.Metadata[Key]);
end;

// ===========================================================================
//  TAiCitationSource
// ===========================================================================

constructor TAiCitationSource.Create;
begin
  inherited Create;
  SourceType := cstUnknown;
  DataSource := TAiSourceData.Create;
end;

destructor TAiCitationSource.Destroy;
begin
  DataSource.Free;
  inherited Destroy;
end;

procedure TAiCitationSource.Assign(Source: TAiCitationSource);
begin
  if not Assigned(Source) or (Source = Self) then
    Exit;
  Self.SourceType := Source.SourceType;
  Self.DataSource.Assign(Source.DataSource);
end;

// ===========================================================================
//  TAiMsgCitation
// ===========================================================================

constructor TAiMsgCitation.Create;
begin
  inherited Create;
  StartIndex := 0;
  EndIndex   := 0;
  Text       := '';
  Sources    := specialize TObjectList<TAiCitationSource>.Create(True);
end;

destructor TAiMsgCitation.Destroy;
begin
  Sources.Free;
  inherited Destroy;
end;

procedure TAiMsgCitation.Assign(Source: TAiMsgCitation);
var
  SourceItem: TAiCitationSource;
  NewItem   : TAiCitationSource;
begin
  if not Assigned(Source) or (Source = Self) then
    Exit;
  Self.StartIndex := Source.StartIndex;
  Self.EndIndex   := Source.EndIndex;
  Self.Text       := Source.Text;
  Self.Sources.Clear;
  for SourceItem in Source.Sources do
  begin
    NewItem := TAiCitationSource.Create;
    try
      NewItem.Assign(SourceItem);
      Self.Sources.Add(NewItem);
    except
      NewItem.Free;
      raise;
    end;
  end;
end;

// ===========================================================================
//  TAiMsgCitations
// ===========================================================================

procedure TAiMsgCitations.Assign(Source: TAiMsgCitations);
var
  SourceCitation: TAiMsgCitation;
  NewCitation   : TAiMsgCitation;
begin
  if Source = Self then
    Exit;
  Self.Clear;
  if not Assigned(Source) then
    Exit;
  for SourceCitation in Source do
  begin
    NewCitation := TAiMsgCitation.Create;
    try
      NewCitation.Assign(SourceCitation);
      Self.Add(NewCitation);
    except
      NewCitation.Free;
      raise;
    end;
  end;
end;

// ===========================================================================
//  TAiToolsFunction
// ===========================================================================

constructor TAiToolsFunction.Create;
begin
  inherited Create;
  Metadata := TAiMetadata.Create;
  Params   := TStringList.Create;
end;

destructor TAiToolsFunction.Destroy;
begin
  Metadata.Free;
  Params.Free;
  inherited Destroy;
end;

procedure TAiToolsFunction.Assign(aSource: TAiToolsFunction);
begin
  Self.id          := aSource.id;
  Self.Tipo        := aSource.Tipo;
  Self.name        := aSource.name;
  Self.Description := aSource.Description;
  Self.Arguments   := aSource.Arguments;
  Self.&Function   := aSource.&Function;
  Self.Response    := aSource.Response;
  Self.Body        := aSource.Body;
  Metadata.JsonText:= aSource.Metadata.JsonText;
end;

procedure TAiToolsFunction.ParseFunction(JObj: TJSONObject);
var
  JFunc: TJSONObject;
begin
  // fpjson: Find() devuelve TJSONData; Cast a TJSONObject
  JFunc := TJSONObject(JObj.Find('function'));
  if not Assigned(JFunc) then
    Exit;

  Name             := JFunc.Get('name', '');
  Self.Description := JFunc.Get('description', '');
  &Function        := JFunc.FormatJSON;
  Body             := JObj;  // referencia al objeto completo
end;

// ===========================================================================
//  TAiToolsFunctions
// ===========================================================================

procedure TAiToolsFunctions.ValueNotify(constref Value: TAiToolsFunction;
                                        Action: TCollectionNotification);
begin
  case Action of
    cnRemoved:
      Value.Free;
  end;
  inherited ValueNotify(Value, Action);
end;

procedure TAiToolsFunctions.AddFunction(aBody: TJSONObject);
var
  Func, Func1: TAiToolsFunction;
begin
  Func := TAiToolsFunction.Create;
  Func.ParseFunction(aBody);

  if not TryGetValue(Func.name, Func1) then
    Add(Func.name, Func)
  else
  begin
    Func1.Assign(Func);
    Func.Free;
  end;
end;

procedure TAiToolsFunctions.AddFunction(aBody: string);
var
  JData: TJSONData;
begin
  JData := GetJSON(aBody);
  try
    if JData is TJSONObject then
      AddFunction(TJSONObject(JData));
  finally
    // No liberar si fue pasado a AddFunction (AddFunction toma ownership del JObj vía Body)
    // Pero aquí Body = referencia, así que sí hay que liberar
    JData.Free;
  end;
end;

function TAiToolsFunctions.ToFunctionsJson: TJSONArray;
var
  Clave: string;
  TObj : TJSONObject;
  Func : TAiToolsFunction;
begin
  Result := TJSONArray.Create;
  for Clave in Keys do
  begin
    Func := Items[Clave];
    // fpjson: Clone() devuelve TJSONData; cast a TJSONObject
    TObj := TJSONObject(Func.Body.Clone);
    Result.Add(TObj);
  end;
end;

function TAiToolsFunctions.ToOutputJson: TJSONArray;
var
  Clave: string;
  TObj : TJSONObject;
begin
  Result := TJSONArray.Create;
  for Clave in Keys do
  begin
    TObj := TJSONObject.Create;
    TObj.Add('tool_call_id', Items[Clave].id);
    TObj.Add('output',       Items[Clave].Response);
    Result.Add(TObj);
  end;
end;

end.
