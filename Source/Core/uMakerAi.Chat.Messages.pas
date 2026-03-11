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
// Nombre: Gustavo Enr?quez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com
// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/

unit uMakerAi.Chat.Messages;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.JSON,
  Rest.JSON, REST.Json.Types, System.Net.Mime, System.NetEncoding, System.TypInfo, System.Types, System.SyncObjs,
{$IF CompilerVersion < 35}
  uJSONHelper,
{$ENDIF}
  uMakerAi.Core; // Asumiendo que TAiMediaFiles, TAiWebSearch y TAiMetadata est?n aqu?

Type

  TAiMsgCitation = class;
  TAiMsgCitations = class;
  TAiChatMessage = Class;

  // Las citaciones relacionan el detalle de la respuesta con el texto origianl
  // puede funcionar como detalles de b?squedas web,  dentro de un pdf, en RAG, etc.
  // se utiliza especialmente como propiedad en TAiMessage.

  TAiCitationSourceType = (cstUnknown, cstDocument, cstWeb, cstFile, cstDatabase);

  // Clase que maneja las funciones de los tools
  TAiToolsFunction = class(TObject)
    id: string;
    Tipo: string;
    name: string;
    Description: String; // Descripci?n de la funci?n
    Arguments: string; // Si tiene par?metros en forma de json se utiliza este
    Params: TStringList; // Si tiene par?metros en forma de name=value se utiliza este si arguments = ''
    &Function: string; // Nombre de la funci?n
    Response: String; // String que responde la funci?n al LLM
    Body: TJSONObject; // El body en json que retorna la funci?n, se utiliza para depuraci?n o para obtener informaci?n adicional
    Metadata: TAiMetadata; // Metadatos adicionales que se pueden enviar a la funci?n
    AskMsg: TAiChatMessage; // TAiChatMessage que representa la pregunta
    ResMsg: TAiChatMessage; // TAiChatMessage que representa la respuesta

    Constructor Create;
    Destructor Destroy; Override;
    Procedure ParseFunction(JObj: TJSONObject); // Esta funci?n se reemplazar? por estas dos seg?n la necesidad

    Procedure Assign(aSource: TAiToolsFunction);
  end;

  TAiToolsFunctions = Class(TDictionary<String, TAiToolsFunction>)
  Private
  Protected
    procedure ValueNotify(const Value: TAiToolsFunction; Action: TCollectionNotification); override;
  Public
    Function ToOutputJSon: TJSonArray;
    Function ToFunctionsJSon: TJSonArray;
    Procedure AddFunction(aBody: String); Overload;
    Procedure AddFunction(aBody: TJSONObject); Overload;
  End;

  TAiChatMessage = Class(TObject)
  Private
    FPreviousResponseId: String;
    [JSONMarshalled(False)]
    FWebSearchResponse: TAiWebSearch;
    FReasoningContent: String;
    FIsToolCallResponse: Boolean;
    FModel: String;
    [JSONMarshalled(False)]
    FCitations: TAiMsgCitations;
    FStopReason: String;
    FIsRefusal: Boolean;
    FThinkingSignature: String;
    FCacheControl: Boolean;
    FThinking_tokens: Integer;
    FFinishReason: String;
    FCached_tokens: Integer;
    [JSONMarshalled(False)]
    FLock: TCriticalSection;
    procedure SetContent(const Value: String);
    procedure SetRole(const Value: String);
    procedure SetPrompt(const Value: String);
    procedure SetFunctionName(const Value: String);
    procedure SetToolCallId(const Value: String);
    procedure SetTool_calls(const Value: String);
    procedure SetFId(const Value: Integer);
    procedure SetCompletion_tokens(const Value: Integer);
    procedure SetPrompt_tokens(const Value: Integer);
    procedure SetTotal_tokens(const Value: Integer);
    procedure SetFPreviousResponseId(const Value: String);
    procedure SetWebSearchResponse(const Value: TAiWebSearch);
    procedure SetReasoningContent(const Value: String);
    procedure SetIsToolCallResponse(const Value: Boolean);
    procedure SetModel(const Value: String);
    procedure SetCitations(const Value: TAiMsgCitations);
    procedure SetIsRefusal(const Value: Boolean);
    procedure SetStopReason(const Value: String);
    procedure SetThinking_tokens(const Value: Integer);
    procedure SetFinishReason(const Value: String);
    procedure SetCached_tokens(const Value: Integer);
  Protected
    FRole: String;
    FContent: String;
    FPrompt: String;
    FCompletion_tokens: Integer;
    FTotal_tokens: Integer;
    FPrompt_tokens: Integer;
    FId: Integer;
    FToolCallId: String;
    FFunctionName: String;
    FTool_calls: String;
    [JSONMarshalled(False)]
    FMediaFiles: TAiMediaFiles;
  Public
    Constructor Create; overload;
    Constructor Create(aPrompt, aRole: String; aToolCallId: String = ''; aFunctionName: String = ''); overload;
    Destructor Destroy; Override;

    Procedure AddMediaFile(aMediaFile: TAiMediaFile);
    Procedure LoadMediaFromFile(aFileName: String);
    Procedure LoadMediaFromStream(aFileName: String; Stream: TMemoryStream);
    Procedure LoadMediaFromBase64(aFileName: String; aBase64: String);

    Function HasUnprocessedItems: Boolean;
    Function GetMediaTranscription: String; // Retorna las transcripciones de todos los archivos de medios

    Function StreamToBase64(Stream: TMemoryStream): String;
    Function ToJSon: TJSonArray; // Convierte el Objeto en un json para enviar al api

    Property id: Integer Read FId Write SetFId;
    Property Role: String read FRole write SetRole;
    Property Content: String read FContent write SetContent;
    Property Prompt: String read FPrompt write SetPrompt;
    Property Prompt_tokens: Integer read FPrompt_tokens Write SetPrompt_tokens;
    Property Completion_tokens: Integer read FCompletion_tokens Write SetCompletion_tokens;
    Property Total_tokens: Integer read FTotal_tokens Write SetTotal_tokens;
    Property Thinking_tokens: Integer read FThinking_tokens write SetThinking_tokens;
    Property Cached_tokens: Integer read FCached_tokens write SetCached_tokens;

    Property Model: String read FModel write SetModel;
    Property ToolCallId: String read FToolCallId write SetToolCallId;
    Property FunctionName: String read FFunctionName write SetFunctionName;
    Property Tool_calls: String read FTool_calls write SetTool_calls;
    Property MediaFiles: TAiMediaFiles Read FMediaFiles;
    Property WebSearchResponse: TAiWebSearch read FWebSearchResponse write SetWebSearchResponse;
    Property PreviousResponseId: String read FPreviousResponseId Write SetFPreviousResponseId; // Nueva propiedad
    Property ReasoningContent: String read FReasoningContent write SetReasoningContent;
    Property IsToolCallResponse: Boolean read FIsToolCallResponse write SetIsToolCallResponse;
    property Citations: TAiMsgCitations read FCitations write SetCitations;
    Property StopReason: String read FStopReason write SetStopReason;
    Property IsRefusal: Boolean read FIsRefusal write SetIsRefusal;
    Property ThinkingSignature: String read FThinkingSignature write FThinkingSignature; // Por ahora es solo para Claude
    Property CacheControl: Boolean read FCacheControl write FCacheControl; // Por ahora se usa solo en Claude para indicar mensajes que se mantengan en Cache
    Property FinishReason: String read FFinishReason write SetFinishReason;
  End;

  TAiChatMessages = Class(TList<TAiChatMessage>) // futura actualizaci?n cambiar tlist por TObjectList
  Private
    FNativeInputFiles: TAiFileCategories;
    function GetAsText: String;
    procedure SetAsText(const Value: String);
    procedure SetNativeInputFiles(const Value: TAiFileCategories);
  Protected
  Public
    Function ToJSon: TJSonArray;
    Function ExportChatHistory: TJSONObject;
    Function ToJsonChatArchive: TJSONObject;
    Procedure SaveToStream(Stream: TStream);
    Procedure SaveToFile(FileName: String);
    Procedure LoadFromStream(Stream: TStream);
    Procedure LoadFromFile(FileName: String);
    Property AsText: String Read GetAsText Write SetAsText;
    Property NativeInputFiles: TAiFileCategories read FNativeInputFiles write SetNativeInputFiles;
  End;

  // Clase base para cualquier tipo de fuente de datos.
  TAiSourceData = class
  public
    id: string; // ID interno (ej: 'doc-0', 'file-123')
    Title: string; // T?tulo de la p?gina web, del documento, etc.
    Content: string; // Snippet, contenido del archivo, etc.
    Url: string; // URL si es una fuente web.
    Metadata: TAiMetadata; // Para cualquier otro dato (autor, fecha, etc.)

    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TAiSourceData);
  end;

  // Representa una fuente espec?fica vinculada a una cita.
  TAiCitationSource = class
  public
    SourceType: TAiCitationSourceType;
    DataSource: TAiSourceData;

    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TAiCitationSource);
  end;

  // Representa una cita dentro del texto de respuesta del modelo.
  TAiMsgCitation = class
  public
    StartIndex: Integer; // Posici?n inicial del texto citado en la respuesta.
    EndIndex: Integer; // Posici?n final.
    Text: String; // El fragmento de texto exacto que fue citado.
    Sources: TObjectList<TAiCitationSource>; // Lista de fuentes para este fragmento.

    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TAiMsgCitation);
  end;

  // Una colecci?n de citas para un mensaje.
  TAiMsgCitations = class(TObjectList<TAiMsgCitation>)
  public
    procedure Assign(Source: TAiMsgCitations);
  end;

implementation

{$IF CompilerVersion < 35}
uses
  uJSONHelper;
{$ENDIF}

{ TAiChatMessage }

procedure TAiChatMessage.AddMediaFile(aMediaFile: TAiMediaFile);
begin
  FLock.Enter;
  Try
    FMediaFiles.Add(aMediaFile);
  Finally
    FLock.Leave;
  End;
end;

constructor TAiChatMessage.Create;
begin
  Create('', '');
end;

constructor TAiChatMessage.Create(aPrompt, aRole: String; aToolCallId: String = ''; aFunctionName: String = '');
begin
  Inherited Create;
  FLock := TCriticalSection.Create;
  Self.FRole := aRole;
  Self.FPrompt := aPrompt;
  Self.FFunctionName := aFunctionName;
  Self.FToolCallId := aToolCallId;
  FMediaFiles := TAiMediaFiles.Create;
  FWebSearchResponse := TAiWebSearch.Create;
  FPreviousResponseId := ''; // Inicializar la nueva propiedad
  FCitations := TAiMsgCitations.Create(True);
  FCacheControl := False;
end;

destructor TAiChatMessage.Destroy;
begin
  FMediaFiles.Free;
  FCitations.Free;
  FWebSearchResponse.Free;
  FLock.Free;
  inherited;
end;

function TAiChatMessage.GetMediaTranscription: String;
Var
  MF: TAiMediaFile;
begin
  FLock.Enter;
  Try
    Result := '';

    for MF in MediaFiles do
    begin
      If MF.Procesado = True then
        Result := Trim(Result + sLineBreak + MF.Transcription);
    end;
  Finally
    FLock.Leave;
  End;
end;

function TAiChatMessage.HasUnprocessedItems: Boolean;
Var
  MF: TAiMediaFile;
begin
  FLock.Enter;
  Try
    Result := False;

    for MF in MediaFiles do
    begin
      If MF.Procesado = False then
      Begin
        Result := True;
        Break;
      End;
    end;
  Finally
    FLock.Leave;
  End;
end;

procedure TAiChatMessage.LoadMediaFromBase64(aFileName, aBase64: String);
Var
  Media: TAiMediaFile;
begin
  FLock.Enter;
  Try
    If Length(Trim(aBase64)) < 100 then
      Raise Exception.Create('El Base64 est? vac?o, no se cargar?');

    If aFileName = '' then // Ver como se asigna un nombre a partir del contenido del stream
      aFileName := 'imagen.jpg';

    Media := TAiMediaFile.Create;
    Media.LoadFromBase64(aFileName, aBase64);

    AddMediaFile(Media);
  Finally
    FLock.Leave;
  End;
end;

procedure TAiChatMessage.LoadMediaFromFile(aFileName: String);
Var
  Media: TAiMediaFile;
begin
  FLock.Enter;
  Try
    If Not FileExists(aFileName) then
      Raise Exception.Create('El archivo "' + aFileName + '" no se encuentra');

    Media := TAiMediaFile.Create;
    Media.LoadFromFile(aFileName);
    AddMediaFile(Media);
  Finally
    FLock.Leave;
  End;
end;

procedure TAiChatMessage.LoadMediaFromStream(aFileName: String; Stream: TMemoryStream);
Var
  Media: TAiMediaFile;
begin
  FLock.Enter;
  Try
    If Stream.Size <= 100 then
      Raise Exception.Create('El stream est? vac?o');

    If aFileName = '' then // Ver como se asigna un nombre a partir del contenido del stream
      aFileName := 'imagen.jpg';

    Media := TAiMediaFile.Create;
    Media.LoadFromStream(aFileName, Stream);
    Self.AddMediaFile(Media);
  Finally
    FLock.Leave;
  End;
end;

procedure TAiChatMessage.SetCached_tokens(const Value: Integer);
begin
  FLock.Enter;
  Try
    FCached_tokens := Value;
  Finally
    FLock.Leave;

  End;
end;

procedure TAiChatMessage.SetCitations(const Value: TAiMsgCitations);
var
  SourceCitation: TAiMsgCitation;
  NewCitation: TAiMsgCitation;
begin
  FLock.Enter;
  Try

    // Siempre limpiar la lista de destino primero.
    // Como la lista es due?a de sus objetos, esto los destruir? correctamente.
    FCitations.Clear;

    // Si la lista de origen es nula o est? vac?a, ya hemos terminado.
    if not Assigned(Value) or (Value.Count = 0) then
      Exit;

    // Iterar sobre la lista de origen y crear clones de cada objeto.
    for SourceCitation in Value do
    begin
      // 1. Crear una nueva instancia del objeto de cita.
      NewCitation := TAiMsgCitation.Create;
      try
        // 2. Usar el m?todo Assign que acabamos de crear para clonar el contenido.
        NewCitation.Assign(SourceCitation);
        // 3. A?adir el nuevo objeto clonado a nuestra lista.
        FCitations.Add(NewCitation);
      except
        // En caso de error durante la asignaci?n, asegurarse de liberar la memoria.
        NewCitation.Free;
        raise;
      end;
    end;
  Finally
    FLock.Leave;
  End;
end;

procedure TAiChatMessage.SetCompletion_tokens(const Value: Integer);
begin
  FLock.Enter;
  Try
    FCompletion_tokens := Value;
  Finally
    FLock.Leave;
  End;
end;

procedure TAiChatMessage.SetContent(const Value: String);
begin
  FLock.Enter;
  Try
    FContent := Value;
  Finally
    FLock.Leave;
  End;
end;

procedure TAiChatMessage.SetFId(const Value: Integer);
begin
  FLock.Enter;
  Try
    FId := Value;
  Finally
    FLock.Leave;
  End;
end;

procedure TAiChatMessage.SetFinishReason(const Value: String);
begin
  FLock.Enter;
  Try
    FFinishReason := Value;
  Finally
    FLock.Leave;
  End;
end;

procedure TAiChatMessage.SetFPreviousResponseId(const Value: String);
begin
  FLock.Enter;
  Try
    FPreviousResponseId := Value;
  Finally
    FLock.Leave;

  End;
end;

procedure TAiChatMessage.SetFunctionName(const Value: String);
begin
  FLock.Enter;
  Try
    FFunctionName := Value;
  Finally
    FLock.Leave;
  End;
end;

procedure TAiChatMessage.SetIsRefusal(const Value: Boolean);
begin
  FLock.Enter;
  Try
    FIsRefusal := Value;
  Finally
    FLock.Leave;
  End;
end;

procedure TAiChatMessage.SetIsToolCallResponse(const Value: Boolean);
begin
  FLock.Enter;
  Try
    FIsToolCallResponse := Value;
  Finally
    FLock.Leave;
  End;
end;

procedure TAiChatMessage.SetModel(const Value: String);
begin
  FLock.Enter;
  Try
    FModel := Value;
  Finally
    FLock.Leave;
  End;
end;

procedure TAiChatMessage.SetPrompt(const Value: String);
begin
  FLock.Enter;
  Try
    FPrompt := Value;
  Finally
    FLock.Leave;
  End;
end;

procedure TAiChatMessage.SetPrompt_tokens(const Value: Integer);
begin
  FLock.Enter;
  Try
    FPrompt_tokens := Value;
  Finally
    FLock.Leave;
  End;
end;

procedure TAiChatMessage.SetReasoningContent(const Value: String);
begin
  FLock.Enter;
  Try
    FReasoningContent := Value;
  Finally
    FLock.Leave;
  End;
end;

procedure TAiChatMessage.SetRole(const Value: String);
begin
  FLock.Enter;
  Try
    FRole := Value;
  Finally
    FLock.Leave;
  End;
end;

procedure TAiChatMessage.SetStopReason(const Value: String);
begin
  FLock.Enter;
  Try
    FStopReason := Value;
  Finally
    FLock.Leave;
  End;
end;

procedure TAiChatMessage.SetThinking_tokens(const Value: Integer);
begin
  FLock.Enter;
  Try
    FThinking_tokens := Value;
  Finally
    FLock.Leave;
  End;
end;

procedure TAiChatMessage.SetToolCallId(const Value: String);
begin
  FLock.Enter;
  Try
    FToolCallId := Value;
  Finally
    FLock.Leave;
  End;
end;

procedure TAiChatMessage.SetTool_calls(const Value: String);
begin
  FLock.Enter;
  Try
    FTool_calls := Value;
  Finally
    FLock.Leave;
  End;
end;

procedure TAiChatMessage.SetTotal_tokens(const Value: Integer);
begin
  FLock.Enter;
  Try
    FTotal_tokens := Value;
  Finally
    FLock.Leave;
  End;
end;

procedure TAiChatMessage.SetWebSearchResponse(const Value: TAiWebSearch);
begin
  FLock.Enter;
  Try
    if FWebSearchResponse <> Value then
    begin
      FWebSearchResponse.Free;
      FWebSearchResponse := Value;
    end;
  Finally
    FLock.Leave;
  End;
end;

function TAiChatMessage.StreamToBase64(Stream: TMemoryStream): String;
begin
  FLock.Enter;
  Try
    Stream.Position := 0;
    Result := TNetEncoding.Base64.EncodeBytesToString(Stream.Memory, Stream.Size);
  Finally
    FLock.Leave;
  End;
end;

function TAiChatMessage.ToJSon: TJSonArray;
Var
  Msg: TAiChatMessage;
  JObj, JMsg: TJSONObject;
  JContent: TJSonArray;
  ImagePayload: TStringStream;
  Base64, Mime: String;
  MediaArr: TAiMediaFilesArray;
begin
  // Esta funci?n solo toma el mensaje actual y una sola im?gen, la primera que encuentra en la lista
  // Esto se hace especialmente para modelos que solo aceptan una im?gen por petici?n y no un chat completo

  FLock.Enter;
  Try

    Result := TJSonArray.Create;

    Msg := Self;
    JObj := TJSONObject.Create;
    JObj.AddPair('role', Msg.FRole);

    If Msg.FToolCallId <> '' then
      JObj.AddPair('tool_call_id', Msg.FToolCallId);

    If (Msg.FFunctionName <> '') then // and (Msg.FRole <> 'tool') then
      JObj.AddPair('name', Msg.FFunctionName);

    // de todos los archivos de medios selecciona las im?genes que es lo que podemos manejar por ahora
    // y las im?genes que no han sigo preprocesadas, por si el modelo no maneja imagenes, previamente
    // se deben haber procesado en en el momendo de adicionar el mensaje al chat
    MediaArr := Msg.MediaFiles.GetMediaList([Tfc_Image], False);

    If (Length(MediaArr) > 0) then
    Begin

      JContent := TJSonArray.Create;
      JMsg := TJSONObject.Create;
      JMsg.AddPair('type', 'text');
      JMsg.AddPair('text', Msg.FPrompt);
      JContent.Add(JMsg);

      If Msg.MediaFiles.Count > 0 then // Solo toma la primera imagen
      Begin
        Base64 := Msg.MediaFiles[0].Base64;
        Mime := Msg.MediaFiles[0].MimeType;

        ImagePayload := TStringStream.Create('{"type": "image_url", "image_url": {"url": "data:' + Mime + ';base64,' + Base64 + '"}}', TEncoding.UTF8);
        try
          JContent.Add(TJSONObject.ParseJSONValue(ImagePayload.DataString) as TJSONObject);
        finally
          ImagePayload.Free;
        end;

      End;
      JObj.AddPair('content', JContent);
    End
    Else
    Begin

      JObj.AddPair('content', Msg.FPrompt);
    End;

    If Msg.FTool_calls <> '' then

{$IF CompilerVersion < 35}
      JObj.AddPair('tool_calls', TJSONUtils.ParseAsArray(Msg.FTool_calls));
{$ELSE}
      JObj.AddPair('tool_calls', TJSonArray(TJSonArray.ParseJSONValue(Msg.FTool_calls)));
{$ENDIF}
    Result.Add(JObj);
  Finally
    FLock.Leave;
  End;
end;




{ TAiChatMessages }

function TAiChatMessages.ExportChatHistory: TJSONObject;
Var
  JObj, JItem: TJSONObject;
  JArr: TJSonArray;
  I: Integer;
  Item: TAiChatMessage;
begin
  JObj := TJSONObject.Create;
  JArr := TJSonArray.Create;

  Try
    JObj.AddPair('model', 'MakerAiChat');
    JObj.AddPair('type', 'Messages');
    JObj.AddPair('ver', '1.0');

    For I := 0 to Self.Count - 1 do
    Begin
      Item := Self.Items[I];
      JItem := TJSONObject.Create;

      JItem.AddPair('role', Item.Role);
      If Trim(Item.Content) <> '' then
        JItem.AddPair('request', Item.Content)
      Else
        JItem.AddPair('prompt', Item.Prompt);
      JArr.Add(JItem);
    End;
    JObj.AddPair('data', JArr);
    Result := JObj;

  Finally
    // jObj.Free;
  End;
end;



function TAiChatMessages.GetAsText: String;
Var
  St: TStringStream;
begin
  St := TStringStream.Create('', TEncoding.UTF8);
  Try
    SaveToStream(St);
    Result := St.DataString;
  Finally
    St.Free;
  End;
end;

procedure TAiChatMessages.LoadFromFile(FileName: String);
Var
  St: TStringStream;
begin
  St := TStringStream.Create('', TEncoding.UTF8);
  Try
    If FileExists(FileName) then
    Begin
      St.LoadFromFile(FileName);
      St.Position := 0;
      LoadFromStream(St);
    End;
  Finally
    St.Free;
  End;
end;

procedure TAiChatMessages.LoadFromStream(Stream: TStream);
Var
  JObj, JItem: TJSONObject;
  JArr: TJSonArray;
  sJson, Model, S: String;
  St: TStringStream;
  Item: TAiChatMessage;
  I: Integer;
begin
  St := TStringStream.Create('', TEncoding.UTF8);
  Try
    St.LoadFromStream(Stream);
    sJson := St.DataString;

    JObj := TJSONObject(TJSONObject.ParseJSONValue(sJson));

    If Assigned(JObj) and (JObj.TryGetValue<String>('model', Model)) then
    Begin
      If (Model = 'AiOpenChat') or (Model = 'MakerAiChat') then
      Begin
        JArr := TJSonArray(JObj.GetValue<TJSonArray>('data'));

        S := JArr.Format;
        If JArr.Count > 0 then
          Self.Clear;

        For I := 0 to JArr.Count - 1 do
        Begin

{$IF CompilerVersion < 35}
          JItem := JArr.GetItemAsObject(I);
{$ELSE}
          JItem := TJSONObject(JArr[I]);
{$ENDIF}
          Item := TJSon.JsonToObject<TAiChatMessage>(JItem);
          Self.Add(Item);
        End;
      End;
    End;
  Finally
    St.Free;
    FreeAndNil(JObj);
  End;
end;

procedure TAiChatMessages.SaveToFile(FileName: String);
Var
  St: TStringStream;
begin
  St := TStringStream.Create('', TEncoding.UTF8);
  Try
    Self.SaveToStream(St);
    St.Position := 0;
    St.SaveToFile(FileName);
  Finally
    St.Free;
  End;
end;

function TAiChatMessages.ToJsonChatArchive: TJSONObject;
Var
  JArr: TJSonArray;
  JItem: TJSONObject;
  I: Integer;
  Item: TAiChatMessage;
begin
  Result := TJSONObject.Create;
  JArr := TJSonArray.Create;

  Result.AddPair('model', 'MakerAiChat');
  Result.AddPair('type', 'Messages');
  Result.AddPair('ver', '1.0');

  For I := 0 to Self.Count - 1 do
  Begin
    Item := Self.Items[I];
    JItem := TJSon.ObjectToJsonObject(Item);
    JArr.Add(JItem);
  End;

  Result.AddPair('data', JArr);
end;

procedure TAiChatMessages.SaveToStream(Stream: TStream);
Var
  JObj: TJSONObject;
  St: TStringStream;
begin
  St := TStringStream.Create('', TEncoding.UTF8);
  JObj := ToJsonChatArchive;
  Try
    St.WriteString(JObj.Format);
    St.SaveToStream(Stream);
  Finally
    St.Free;
    JObj.Free;
  End;
end;

procedure TAiChatMessages.SetAsText(const Value: String);
Var
  St: TStringStream;
begin
  St := TStringStream.Create('', TEncoding.UTF8);
  Try
    If Trim(Value) <> '' then
    Begin
      St.WriteString(Value);
      St.Position := 0;
      LoadFromStream(St);
    End;
  Finally
    St.Free;
  End;
end;

procedure TAiChatMessages.SetNativeInputFiles(const Value: TAiFileCategories);
begin
  FNativeInputFiles := Value;
end;

function TAiChatMessages.ToJSon: TJSonArray;
Var
  I, J: Integer;
  Msg: TAiChatMessage;
  JObj, JMsg: TJSONObject;
  JObjImg, jImgUrl, jAudio: TJSONObject;
  JContent: TJSonArray;
  Base64, Mime: String;
  MediaArr: TAiMediaFilesArray;
  S: String;
  MediaFile: TAiMediaFile;
begin
  Result := TJSonArray.Create;

  For I := 0 to Count - 1 do
  Begin
    Msg := Self.Items[I];
    JObj := TJSONObject.Create;
    JObj.AddPair('role', Msg.FRole);

    If Msg.FToolCallId <> '' then
      JObj.AddPair('tool_call_id', Msg.FToolCallId);

    If Msg.FFunctionName <> '' then
      JObj.AddPair('name', Msg.FFunctionName);

    // de todos los archivos de medios selecciona las im?genes que es lo que podemos manejar por ahora
    // y las im?genes que no han sigo preprocesadas, por si el modelo no maneja imagenes, previamente
    // se deben haber procesado en en el momento de adicionar el mensaje al chat
    MediaArr := Msg.MediaFiles.GetMediaList(FNativeInputFiles, False);

    If (Length(MediaArr) > 0) then
    Begin

      JContent := TJSonArray.Create;
      JMsg := TJSONObject.Create;
      JMsg.AddPair('type', 'text');
      JMsg.AddPair('text', Msg.FPrompt);
      JContent.Add(JMsg);

      For J := 0 to Length(MediaArr) - 1 do // Open Ai permite subir el Base64 o el Url, siempre se sube el Base64, por estandar
      Begin
        MediaFile := MediaArr[J];

        Case MediaFile.FileCategory of
          TAiFileCategory.Tfc_Image:
            Begin
              Base64 := MediaFile.Base64;
              Mime := MediaFile.MimeType;

              // Esta es otra forma de hacer lo mismo con json directamente
              S := 'data:' + Mime + ';base64,' + Base64;

              jImgUrl := TJSONObject.Create;
              jImgUrl.AddPair('url', S);

              If Msg.MediaFiles[J].Detail <> '' then // Si define high or low.
                jImgUrl.AddPair('detail', MediaFile.Detail);

              JObjImg := TJSONObject.Create;
              JObjImg.AddPair('type', 'image_url');
              JObjImg.AddPair('image_url', jImgUrl);

              JContent.Add(JObjImg);

            End;
          TAiFileCategory.Tfc_Audio:
            Begin
              If MediaFile.IdAudio <> '' then // Si es una respuesta del modelo va esto
              Begin
                jAudio := TJSONObject.Create;
                jAudio.AddPair('id', MediaFile.IdAudio);
                JObj.AddPair('audio', jAudio);
              End
              Else // Si es un audio del usuario va esto
              Begin
                jAudio := TJSONObject.Create;
                jAudio.AddPair('data', MediaFile.Base64);
                jAudio.AddPair('format', StringReplace(MediaFile.MimeType, 'audio/', '', [rfReplaceAll]));

                JContent := TJSonArray.Create;
                JMsg := TJSONObject.Create;
                JMsg.AddPair('type', 'input_audio');
                JMsg.AddPair('input_audio', jAudio);
                JContent.Add(JMsg);

                JObj.AddPair('content', JContent);
              End;
            End;

          TAiFileCategory.Tfc_Video:
            Begin
            end;
          TAiFileCategory.Tfc_Pdf: // El completions no maneja pdf todav?a hay que usar el Responses
            Begin
            end;
          TAiFileCategory.Tfc_Document:
            Begin
            end;
          TAiFileCategory.Tfc_Text:
            Begin
            end;
          TAiFileCategory.Tfc_CalcSheet:
            Begin
            end;
          TAiFileCategory.Tfc_Presentation:
            Begin
            end;
          TAiFileCategory.Tfc_CompressFile:
            Begin
            end;
          TAiFileCategory.Tfc_Web:
            Begin
            end;
          TAiFileCategory.Tfc_GraphicDesign:
            Begin
            end;
          TAiFileCategory.Tfc_Unknown:
            Begin
            end;

        Else
          Begin
            JObj.AddPair('content', Msg.FPrompt);
          End;
        End;
      End;

      // TODO: Si hay im?genes + audio de usuario en el mismo mensaje, el JContent de im?genes
      // se pierde porque el caso audio crea un nuevo JContent. Requiere redise?ar la l?gica de media mixta.
      JObj.AddPair('content', JContent);

    End
    Else // Si no tiene archivos de medios simplemente env?a el prompt
    Begin
      JObj.AddPair('content', Msg.FPrompt);
    End;

    If Msg.FTool_calls <> '' then

{$IF CompilerVersion < 35}
      JObj.AddPair('tool_calls', TJSONUtils.ParseAsArray(Msg.FTool_calls));
{$ELSE}
      JObj.AddPair('tool_calls', TJSonArray(TJSonArray.ParseJSONValue(Msg.FTool_calls)));
{$ENDIF}

    // reasoning_content requerido por APIs como DeepSeek y Kimi cuando thinking está habilitado.
    // Solo se serializa cuando está presente (no afecta a providers que no lo usan).
    if Msg.FReasoningContent <> '' then
      JObj.AddPair('reasoning_content', Msg.FReasoningContent);

    Result.Add(JObj);
  end;
end;

procedure TAiSourceData.Assign(Source: TAiSourceData);
var
  Pair: TPair<string, string>;
begin
  if not Assigned(Source) or (Source = Self) then
    Exit;

  Self.id := Source.id;
  Self.Title := Source.Title;
  Self.Content := Source.Content;
  Self.Url := Source.Url;

  // Limpiar el diccionario de metadatos de destino.
  Self.Metadata.Clear;

  // Iterar sobre cada par clave-valor en el diccionario de origen
  // y a?adirlo al diccionario de destino. Esta es la forma correcta.
  for Pair in Source.Metadata do
  begin
    Self.Metadata.Add(Pair.Key, Pair.Value);
  end;
end;

{ ------------------------------------------------------------------------------ }
{ TAiSourceData }
{ ------------------------------------------------------------------------------ }

constructor TAiSourceData.Create;
begin
  inherited Create;
  // Es crucial instanciar el objeto Metadata para evitar errores de acceso a memoria.
  Metadata := TAiMetadata.Create;
end;

destructor TAiSourceData.Destroy;
begin
  // Liberamos el objeto que creamos en el constructor.
  Metadata.Free;
  inherited Destroy;
end;

procedure TAiCitationSource.Assign(Source: TAiCitationSource);
begin
  if not Assigned(Source) or (Source = Self) then
    Exit;
  Self.SourceType := Source.SourceType;
  Self.DataSource.Assign(Source.DataSource);
end;
{ ------------------------------------------------------------------------------ }
{ TAiCitationSource }
{ ------------------------------------------------------------------------------ }

constructor TAiCitationSource.Create;
begin
  inherited Create;
  // Inicializamos el tipo de fuente a un valor por defecto seguro.
  SourceType := cstUnknown;
  // Instanciamos el contenedor de datos para que est? listo para ser usado.
  DataSource := TAiSourceData.Create;
end;

destructor TAiCitationSource.Destroy;
begin
  // Liberamos el objeto de datos que le pertenece.
  DataSource.Free;
  inherited Destroy;
end;

procedure TAiMsgCitation.Assign(Source: TAiMsgCitation);
var
  SourceItem: TAiCitationSource;
  NewItem: TAiCitationSource;
begin
  if not Assigned(Source) or (Source = Self) then
    Exit;

  Self.StartIndex := Source.StartIndex;
  Self.EndIndex := Source.EndIndex;
  Self.Text := Source.Text;

  // Realizar una copia profunda de la lista de fuentes
  Self.Sources.Clear;
  for SourceItem in Source.Sources do
  begin
    NewItem := TAiCitationSource.Create;
    NewItem.Assign(SourceItem);
    Self.Sources.Add(NewItem);
  end;
end;

{ ------------------------------------------------------------------------------ }
{ TAiMsgCitation }
{ ------------------------------------------------------------------------------ }

constructor TAiMsgCitation.Create;
begin
  inherited Create;
  // Inicializamos los valores primitivos.
  StartIndex := 0;
  EndIndex := 0;
  Text := '';
  // Creamos la lista de fuentes y le indicamos que es due?a de los objetos
  // que contiene (el par?metro 'True'). Esto simplifica la gesti?n de memoria,
  // ya que al liberar la lista, se liberar?n autom?ticamente todas las fuentes.
  Sources := TObjectList<TAiCitationSource>.Create(True);
end;

destructor TAiMsgCitation.Destroy;
begin
  // Liberamos la lista de fuentes. Gracias a AOwnsObjects=True,
  // todos los TAiCitationSource en la lista se destruir?n autom?ticamente.
  Sources.Free;
  inherited Destroy;
end;

{ TAiMsgCitations }

procedure TAiMsgCitations.Assign(Source: TAiMsgCitations);
var
  SourceCitation: TAiMsgCitation;
  NewCitation: TAiMsgCitation;
begin
  // 1. Evitar la auto-asignaci?n
  if Source = Self then
    Exit;

  // 2. Limpiar la lista de destino. Como TObjectList es el propietario,
  // esto liberar? cualquier objeto TAiMsgCitation que ya exista.
  Self.Clear;

  // 3. Si la fuente es nula, no hay nada m?s que hacer.
  if not Assigned(Source) then
    Exit;

  // 4. Recorrer la lista de origen (Source)
  for SourceCitation in Source do
  begin
    // 5. Crear una nueva instancia del objeto de destino
    NewCitation := TAiMsgCitation.Create;
    try
      // 6. Usar el m?todo Assign del objeto individual para copiar los datos (copia profunda)
      NewCitation.Assign(SourceCitation);
      // 7. A?adir el objeto reci?n clonado a nuestra lista (Self)
      Self.Add(NewCitation);
    except
      // En caso de un error, asegurarse de liberar el objeto que acabamos de crear
      NewCitation.Free;
      raise;
    end;
  end;
end;

{ TAiToolFunction }

procedure TAiToolsFunction.Assign(aSource: TAiToolsFunction);
begin
  Self.id := aSource.id;
  Self.Tipo := aSource.Tipo;
  Self.name := aSource.name;
  Self.Description := aSource.Description;
  Self.Arguments := aSource.Arguments;
  Self.&Function := aSource.&Function;
  Self.Response := aSource.Response;
  Self.Body := aSource.Body;
  Metadata.JsonText := aSource.Metadata.JsonText;
end;

constructor TAiToolsFunction.Create;
begin
  inherited;
  Metadata := TAiMetadata.Create;
  Params := TStringList.Create;

end;

destructor TAiToolsFunction.Destroy;
begin
  Metadata.Free;
  Params.Free;
  inherited;
end;

procedure TAiToolsFunction.ParseFunction(JObj: TJSONObject);
Var
  JFunc: TJSONObject;
begin
  JFunc := JObj.GetValue<TJSONObject>('function');

  Name := JFunc.GetValue<String>('name');
  Self.Description := JFunc.GetValue<String>('description');
  &Function := JFunc.Format;
  Body := JObj; // La funcion original completa
end;

{ TAitools_outputs }

procedure TAiToolsFunctions.AddFunction(aBody: TJSONObject);
Var
  Func, Func1: TAiToolsFunction;
begin
  Func := TAiToolsFunction.Create;
  Func.ParseFunction(aBody);

  If Self.TryGetValue(Func.name, Func1) = False then
    Self.Add(Func.name, Func)
  Else
  Begin
    Func1.Assign(Func);
    Func.Free;
  End;
end;

procedure TAiToolsFunctions.ValueNotify(const Value: TAiToolsFunction; Action: TCollectionNotification);
begin
  case Action of
    cnDeleting, cnRemoved:
      Value.Free;
  end;
  inherited;
end;

procedure TAiToolsFunctions.AddFunction(aBody: String);
Var
  Func: TJSONObject;
begin
  Func := TJSONObject(TJSONObject.ParseJSONValue(aBody));
  AddFunction(Func);
end;

function TAiToolsFunctions.ToFunctionsJSon: TJSonArray;
Var
  Clave: String;
  TObj: TJSONObject;
  Func: TAiToolsFunction;
begin
  Result := TJSonArray.Create;

  For Clave in Self.Keys do
  Begin
    Func := Self.Items[Clave];
    // Result.Add(TJSonObject(TJSonObject.ParseJSONValue(Self.Items[Clave].&Function)));
    TObj := TJSONObject(Func.Body.Clone);
    // TObj.AddPair('type', 'function');
    // TObj.AddPair('function', TJsonObject(Func.Body.Clone));
    Result.Add(TObj);
  End;
end;

function TAiToolsFunctions.ToOutputJSon: TJSonArray;
Var
  Clave: String;
  TObj: TJSONObject;
begin
  Result := TJSonArray.Create;

  For Clave in Self.Keys do // La clave es el nombre de la funci?n
  Begin
    TObj := TJSONObject.Create;
    TObj.AddPair('tool_call_id', Self.Items[Clave].id);
    TObj.AddPair('output', Self.Items[Clave].Response);
    Result.Add(TObj);
  End;
end;

end.
