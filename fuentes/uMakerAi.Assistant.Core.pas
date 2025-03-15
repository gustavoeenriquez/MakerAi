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
// - Telegram: +57 3128441700
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.Assistant.Core;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Threading,
  System.Variants, System.Net.Mime, System.IOUtils, System.Generics.Collections,
  System.NetEncoding,
  System.JSON, System.StrUtils, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent,
  REST.JSON, REST.Types, REST.Client, uMakerAi.Core;

Const
  GlOpenAIUrl = 'https://api.openai.com/v1/';

type

  TAiMessage = Class;

  { TAiFile }
  TAiFile = Class(TObject)
  Private
    Ffilename: String;
    Fcreated_at: String;
    Fid: String;
    Fpurpose: String;
    Fstatus: String;
    FTypeObject: String;
    FFileType: String;
    FContent: TMemoryStream;
    Fbytes: Integer;
    FApiKey: String;
    FUrl: String;
    procedure Setfilename(const Value: String);
    procedure Setid(const Value: String);
    procedure SetApiKey(const Value: String);
    procedure SetUrl(const Value: String);
  Protected
    function GetContent: TMemoryStream; Virtual;
  Public
    Constructor Create(aApiKey: String; aUrl: String = '');
    Destructor Destroy; Override;
    Property TypeObject: String read FTypeObject;
    Property id: String read Fid write Setid;
    Property purpose: String read Fpurpose;
    Property filename: String read Ffilename write Setfilename;
    Property bytes: Integer read Fbytes;
    Property created_at: String read Fcreated_at;
    Property status: String read Fstatus;
    Property Content: TMemoryStream read GetContent;
    Property FileType: String read FFileType;
    Property ApiKey: String read FApiKey write SetApiKey;
    Property Url: String read FUrl write SetUrl;
  End;

  TAiFileArray = Class(TDictionary<String, TAiFile>)
  Private
  Protected
  Public
    function GetFileById(AiFileId: String; Out AiFile: TAiFile): Boolean;
    Function GetFileByName(aFileName: String; Out AiFile: TAiFile): Boolean;
  End;

  TAiFiles = Class(TObject)
  Private
    FApiKey: String;
    FFileList: TAiFileArray;
    FUrl: String;
    procedure SetApiKey(const Value: String);
    function GetFileList: TAiFileArray;
    procedure SetUrl(const Value: String);
  Protected
  Public
    Constructor Create(aApiKey: String; aUrl: String = '');
    Destructor Destroy; Override;
    Function UploadFile(filename: String; IsForAssistant: Boolean): TAiFile;
    Function ListFiles: TAiFileArray;
    Function DeleteFile(aFileId: String): Boolean;
    Function GetFile(aFileId: String): TAiFile;
    Function GetFileByName(aFileName: String): TAiFile;

    Property ApiKey: String read FApiKey write SetApiKey;
    Property FileList: TAiFileArray Read GetFileList;
    Property Url: String read FUrl write SetUrl;
  End;

  { TAiVectorStoreFile }

  TAiVectorStoreFile = Class(TObject)
  Public
    id: String;
    fObject: String;
    usage_bytes: Integer;
    Created_ad: Integer;
    Vectore_store_id: String;
    status: String;
    Last_Error: String;
  End;

  TAiVectorStoreFileArray = Class(TDictionary<String, TAiVectorStoreFile>)
  Private
  Protected
  Public
    function GetFileById(AiFileId: String; Out AiStoreFile: TAiVectorStoreFile): Boolean;
    function GetFileByVectorStoreId(AiVectorStoreFileId: String; Out AiStoreFile: TAiVectorStoreFile): Boolean;
    // Function GetFileByName(aFileName: String; Out AiStoreFile: TAiVectorStoreFile): Boolean;
  End;

  TAiVectorStoreFiles = Class(TObject)
  Private
    FApiKey: String;
    FVectorStoreId: String;
    FFileList: TAiVectorStoreFileArray;
    FUrl: String;
    procedure SetApiKey(const Value: String);
    procedure SetVectorStoreId(const Value: String);
    function GetFileList: TAiVectorStoreFileArray;
    procedure SetUrl(const Value: String);
  Protected
  Public
    Constructor Create(aApiKey, aVectorStoreId: String; aUrl: String = '');
    Destructor Destroy; Override;
    Function AtachFile(aFileId: String): TAiVectorStoreFile;
    Function ListFiles(Limit: Integer = 20; OrderDesc: Boolean = False; After: String = ''; Before: String = ''): TAiVectorStoreFileArray;
    Function GetFile(aFileId: String): TAiVectorStoreFile;
    Function DetachFile(aFileId: String): Boolean;

    Property VectorStoreId: String read FVectorStoreId write SetVectorStoreId;
    Property ApiKey: String read FApiKey write SetApiKey;
    Property FileList: TAiVectorStoreFileArray Read GetFileList;
    Property Url: String read FUrl write SetUrl;
  End;

  { TAiVectorStore }

  TAiVectorStoreFileCounts = class
  Public
    In_Progress: Integer;
    Completed: Integer;
    Cancelled: Integer;
    Failed: Integer;
    Total: Integer;
  end;

  TAiVectorStore = Class(TObject)
  Private
    FUrl: String;
    function GetFiles: TAiVectorStoreFiles;
    procedure SetUrl(const Value: String);
  Protected
    FFiles: TAiVectorStoreFiles;
    FApiKey: String;
  Public
    id: String;
    fObject: String;
    created_at: Integer;
    usage_bytes: Integer;
    last_active_at: Integer;
    name: String;
    status: String;
    file_counts: TAiVectorStoreFileCounts;
    { in_progress: Integer;
      Completed: Integer;
      Cancelled: Integer;
      Failed: Integer;
      Total: Integer;
    }
    Metadata: TAiMetadata;
    last_used_at: Integer;
    Constructor Create(aApiKey: String; aUrl: String = '');
    Destructor Destroy; Override;

    Property Files: TAiVectorStoreFiles Read GetFiles;
    Property ApiKey: String Read FApiKey Write FApiKey;
    Property VectorStoreId: String Read id Write id;
    Property Url: String read FUrl write SetUrl;
  End;

  TAiVectorStoreArray = Class(TDictionary<String, TAiVectorStore>)
  Private
  Protected
  Public
    function GetVectorStoreById(AiVectorStoreId: String; out AiVectorStore: TAiVectorStore): Boolean;
    Function GetVectorStoreByName(AiVectorStoreName: String; Out AiVectorStore: TAiVectorStore): Boolean;
  End;

  TAiVectorStores = Class(TObject)
  Private
    FUrl: String;
    function GetVectorStoreList: TAiVectorStoreArray;
    procedure SetUrl(const Value: String);
  Protected
    FApiKey: String;
    FVectorStoreList: TAiVectorStoreArray;
    Class Function ParseVectorStore(JObj: TJSonObject; aApiKey: String; aUrl: String = ''): TAiVectorStore;
  Public
    Constructor Create(aApiKey: String; aUrl: String = '');
    Destructor Destroy; Override;

    Class Function CreateNewStore(aApiKey, aVectorName: String; aUrl: String = ''): TAiVectorStore;
    Function List(Limit: Integer = 20; OrderDesc: Boolean = False; After: String = ''; Before: String = ''): TAiVectorStoreArray;
    Function Retrive(aVectorStoreId: String): TAiVectorStore;
    Function Modify(aVectorStoreId, aVectorName: String): TAiVectorStore;
    Function Delete(aVectorStoreId: String): Boolean;
    Property VectorStoreList: TAiVectorStoreArray Read GetVectorStoreList;
    Property Url: String read FUrl write SetUrl;
  End;

  { TAiVectorStoreFileBatch }
  TAiVectorStoreFileBatch = Class(TObject)
  Private
  Protected
  Public
    id: String;
    fObject: String;
    created_at: Integer;
    vector_store_id: String;
    status: String;
    file_counts: TAiVectorStoreFileCounts;
    { in_progress: Integer;
      Completed: Integer;
      Cancelled: Integer;
      Failed: Integer;
      Total: Integer;
    }
  End;

  TAiVectorStoreFileBatchArray = Class(TDictionary<String, TAiVectorStoreFileBatch>)
  Private
  Protected
  Public
    function GetVectorStoreFileBatchById(AiVectorStoreId: String; out AiVectorStoreFileBatch: TAiVectorStoreFileBatch): Boolean;
  End;

  TAiVectorStoresFileBatch = Class(TObject)
  Private
  Protected
  Public
    Class Function CreateNewVectorStoreBatch(aApiKey, aVectorStoreId, FileIdsList: String): TAiVectorStoreFileBatch;
    Class Function Retrive(aApiKey, aVectorStoreId, aBatchId: String): TAiVectorStoreFileBatch;
    Class Function Cancel(aApiKey, aVectorStoreId, aBatchId: String): TAiVectorStoreFileBatch;
    Class Function List(aApiKey, aVectorStoreId, aBatchId: String): TAiVectorStoreFileBatchArray;
  End;

{$REGION 'Open Assitant Classes'}

  TOnAssistantResponse = Procedure(Sender: TObject; Response: TAiMessage; Content: String) of object;
  TOnStatusNotifyEvent = Procedure(Sender: TObject; aStatus: String) of object;
  TOnBeforeResponse = Procedure(Sender: TObject; Funcion, Response: String) of object;

  TAiMessageAnotations = Record
    Tipo: String;
    Text: String;
    filename: String;
    FileType: String;
    FileId: String;
  End;

  TAiMessageAnotationsArray = Array of TAiMessageAnotations;

  TAiMessageContent = Record
    Tipo: String;
    Text: String;
    Anotations: TAiMessageAnotationsArray;
  End;

  TAiMessageContentArray = Array of TAiMessageContent;

  TAiMessage = Class(TObject)
  private
    FRole: String;
    Ffile_ids: TStringList;
    FMetadata: TAiMetadata;
    FMessageId: String;
    FAssisistantId: String;
    FRunId: String;
    FFiles: TAiFileArray;
    FThReadId: String;
    FApiKey: String;
    FContent: TAiMessageContentArray;
    FUrl: String;
    FFinishReason: String;
    procedure Setrole(const Value: String);
    procedure Setfile_ids(const Value: TStringList);
    procedure SetMetadata(const Value: TAiMetadata);
    procedure SetMessageId(const Value: String);
    procedure SetAssisistantId(const Value: String);
    procedure SetRunId(const Value: String);
    procedure SetThReadId(const Value: String);
    procedure SetApiKey(const Value: String);
    procedure SetContent(const Value: TAiMessageContentArray);
    procedure SetUrl(const Value: String);
    procedure SetFinishReason(const Value: String);
  Public
    Constructor Create(aApiKey: String; aUrl: String = '');
    Destructor Destroy; Override;
    Function ToJSon(ShowAll: Boolean = False): TJSonObject;
    Function ToString: String;

    Property MessageId: String read FMessageId write SetMessageId;
    Property role: String read FRole write Setrole;
    Property Content: TAiMessageContentArray read FContent write SetContent;
    Property file_ids: TStringList read Ffile_ids write Setfile_ids;
    Property Files: TAiFileArray read FFiles;
    Property Metadata: TAiMetadata read FMetadata write SetMetadata;
    Property AssistantId: String read FAssisistantId write SetAssisistantId;
    Property FinishReason: String read FFinishReason write SetFinishReason;
    Property ThReadId: String read FThReadId write SetThReadId;
    Property RunId: String read FRunId write SetRunId;
    Property ApiKey: String read FApiKey write SetApiKey;
    Property Url: String read FUrl write SetUrl;
  End;

  TAiMessages = Class(TList<TAiMessage>)
  Private
  Protected
  Public
    // Function AddMessage(aRole, aContent, aFiles_ids: String): TAiMessage;
    Function ToJSon(ShowAll: Boolean = False): TJSonArray;
    destructor Destroy; override;
  End;

  TAiAssistant = Class(TObject)
  private
    FName: String;
    FModel: String;
    FInstructions: String;
    FMetadata: TAiMetadata;
    FAssistantId: String;
    FJSonObject: TJSonObject;
    FFilesIds: TStringList;
    FVectorStoreIds: TStringList;
    FCode_Interpreter: Boolean;
    FFileSearch: Boolean;
    FFunciones: TAiToolsFunctions;
    FTxtJson: String;
    FApiKey: String;
    FOnBeforeResponse: TOnBeforeResponse;
    FFiles: TAiFiles;
    FVectorStores: TAiVectorStores;
    Ftop_p: Double;
    FTemperature: Double;
    FUrl: String;
    procedure SetInstructions(const Value: String);
    procedure SetMetadata(const Value: TAiMetadata);
    procedure SetModel(const Value: String);
    procedure SetName(const Value: String);
    procedure SetAssistantId(const Value: String);
    procedure SetJSonObject(const Value: TJSonObject);
    procedure SetFilesIds(const Value: TStringList);
    procedure SetCode_Interpreter(const Value: Boolean);
    procedure SetFileSearch(const Value: Boolean);
    procedure SetFunciones(const Value: TAiToolsFunctions);
    procedure SetApiKey(const Value: String);
    procedure SetOnBeforeResponse(const Value: TOnBeforeResponse);
    procedure SetTemperature(const Value: Double);
    procedure Settop_p(const Value: Double);
    procedure SetUrl(const Value: String);

  Protected
    Procedure ParseAssistantJson(Obj: TJSonObject);
  Public
    Constructor Create(aApiKey: String; aUrl: String = '');
    Destructor Destroy; Override;

    Procedure DoBeforeResponse(Sender: TObject; Funcion, Value: String);

    Class Function GetList(aApiKey: String; aUrl: String = ''; Limit: Integer = 20; Order: String = 'desc'): TJSonObject;
    Class Function GetAssistantIdByName(aApiKey: String; AssistantName: String; aUrl: String = ''): String;

    Function LoadAssistant(aAssistantId: String): Boolean;
    Function CreateNewAssistant(aModelo, aNombre, aInstrucciones: String; aFunciones: TAiToolsFunctions; aFileSearch: Boolean = False; aVectorStoreIds: String = ''; aCodeInterpreter: Boolean = False; aCodeFilesIds: String = '';
      aTemperature: Double = 1; atop_p: Double = 1): Boolean;

    Class Function GetModels(aApiKey: String; aUrl: String = ''): TJSonObject; Overload; virtual;

    Function ApplyUpdates: TJSonObject;
    Function Remove: TJSonObject;

    { TODO : Falta implementar los archivos del code_interperter }
    Function CodeFileAttach(FileId: String): Boolean;
    Function CodeFileUploadAndAttach(FileId: String): Boolean;
    Function CodeFileDetach(FileId: String): Boolean;
    Function CodeFileList: TJSonObject;
    Function CodeFileListFilesArray: TAiFileArray;

    Function VectorStoreFileAttach(VectorStoreId, FileId: String): Boolean;
    Function VectorStoreFileUploadAndAttach(VectorStoreId, filename: String): TAiFile;
    Function VectorStoreFileDetach(VectorStoreId, FileId: String): Boolean;
    Function VectorStoreFileList(VectorStoreId: String): TAiVectorStoreFileArray;

    Property Model: String read FModel write SetModel;
    Property Name: String read FName write SetName;
    Property Instructions: String read FInstructions write SetInstructions;
    Property FilesIds: TStringList read FFilesIds write SetFilesIds;
    Property Metadata: TAiMetadata read FMetadata write SetMetadata;
    Property AssistantId: String read FAssistantId write SetAssistantId;
    Property JSonObject: TJSonObject read FJSonObject write SetJSonObject;
    Property Code_Interpreter: Boolean read FCode_Interpreter write SetCode_Interpreter;
    Property FileSearch: Boolean read FFileSearch write SetFileSearch;
    Property Funciones: TAiToolsFunctions read FFunciones write SetFunciones;
    Property TxtJson: String Read FTxtJson;
    Property ApiKey: String read FApiKey write SetApiKey;
    Property OnBeforeResponse: TOnBeforeResponse read FOnBeforeResponse write SetOnBeforeResponse;
    Property Files: TAiFiles read FFiles;
    Property Temperature: Double read FTemperature write SetTemperature;
    Property top_p: Double read Ftop_p write Settop_p;
    Property VectorStores: TAiVectorStores Read FVectorStores;
    Property Url: String read FUrl write SetUrl;
  End;

  TAiThRead = Class(TObject)
  Private
    FMessages: TAiMessages;
    FMetadata: TAiMetadata;
    FThReadId: String;
    FJSonObject: TJSonObject;
    FAiAssistant: TAiAssistant;
    FUrl: String;
    procedure SetMessages(const Value: TAiMessages);
    procedure SetMetadata(const Value: TAiMetadata);
    procedure SetThReadId(const Value: String);
    procedure SetJSonObject(const Value: TJSonObject);
    procedure SetAiAssistant(const Value: TAiAssistant);
    procedure SetUrl(const Value: String);
  Protected
    Procedure ParseThReadJson(JObj: TJSonObject);
    Function ParseMessage(JObj: TJSonObject): TAiMessage;
  Public
    Constructor Create(aAiAssistant: TAiAssistant);
    // Constructor Create(ThReadId: String); Overload;
    // Constructor Create(aMensajes: TAiMessages; aMetadata: TAiMetadata); Overload;
    Destructor Destroy; Override;

    Function LoadThRead(ThReadId: String): Boolean;
    Function CreateNewThRead(aMensajes: TAiMessages; aMetadata: TAiMetadata; aVectorStoreIds: String = ''; aCodeFilesIds: String = ''): Boolean;

    Function ApplyUpdates: Boolean;
    Function Remove: Boolean;
    Function AddMessage(aMessage: String; aRole: String = ''; aFiles_ids: String = ''): TAiMessage;
    Function ListMessages(Limit: Integer = 20; Order: String = 'desc'): TAiMessages;
    Function GetMessage(aMessageId: String): TAiMessage;
    Function GetMessageFile(aThReadId, aMessageId, aFileId: String): TJSonObject;
    Function ListMessageFiles(aMsgId: String): TJSonObject;
    Function MessageApplyUpdate(aMessage: TAiMessage): TJSonObject;

    Property Messages: TAiMessages read FMessages write SetMessages;
    Property Metadata: TAiMetadata read FMetadata write SetMetadata;
    Property ThReadId: String read FThReadId write SetThReadId;
    Property JSonObject: TJSonObject read FJSonObject write SetJSonObject;
    Property AiAssistant: TAiAssistant read FAiAssistant write SetAiAssistant;
    Property Url: String read FUrl write SetUrl;
  End;

  TAiRun = Class(TObject)
  Private
    FThRead: TAiThRead;
    FAssistant: TAiAssistant;
    FRunId: String;
    Fstatus: String;
    FLastError: String;
    FMetadata: TAiMetadata;
    FJObjectRun: TJSonObject;
    FToolsCalls: TAiToolsFunctions;
    FOnCallToolFunction: TOnCallToolFunction;
    FBusy: Boolean;
    FUrl: String;
    procedure SetMetadata(const Value: TAiMetadata);
    procedure SetJObjectRun(const Value: TJSonObject);
    procedure SetToolsCalls(const Value: TAiToolsFunctions);
    procedure SetOnCallToolFunction(const Value: TOnCallToolFunction);
    procedure SetUrl(const Value: String);
  Protected
    Procedure ParseRun(JObj: TJSonObject);
    Function ExtractToolCallsFromJson(JSonObject: TJSonObject): Boolean;
    Procedure InternalCallToolFunction(AiToolCall: TAiToolsFunction);
  Public
    Constructor Create(aAssistant: TAiAssistant; aThRead: TAiThRead; aAditionalInstructions: String = ''; aMetadata: TAiMetadata = Nil); Overload;
    Destructor Destroy; Override;

    Function Run: Boolean;

    Function ListRuns: TJSonObject;
    Function ListRunsSteps: TJSonObject;
    Function Retrieve: TJSonObject;
    Function RetrieveStep(StepId: String): TJSonObject;

    Function ApplyUpdates: TJSonObject;

    Function SubmitTool(AitoolsOutputs: TAiToolsFunctions): TJSonObject;
    Function Cancel: TJSonObject;

    Property Assistant: TAiAssistant read FAssistant;
    Property ThRead: TAiThRead read FThRead;
    Property RunId: String read FRunId;
    Property status: String Read Fstatus;
    Property LastError: String Read FLastError;
    Property Metadata: TAiMetadata read FMetadata write SetMetadata;
    Property JObjectRun: TJSonObject read FJObjectRun write SetJObjectRun;
    Property ToolsCalls: TAiToolsFunctions read FToolsCalls write SetToolsCalls;
    Property OnCallToolFunction: TOnCallToolFunction read FOnCallToolFunction write SetOnCallToolFunction;
    Property Busy: Boolean read FBusy;
    Property Url: String read FUrl write SetUrl;

  End;

{$ENDREGION}

procedure Register;

implementation

{$IFDEF LINUX}

uses uLinuxUtils;
{$ENDIF}
{$IFDEF MSWINDOWS}

uses ShellAPI, WinApi.Windows;
{$ENDIF}
{$REGION 'Utilidades varias' }

procedure Register;
begin
  // RegisterComponents('MakerAI');
end;

{$ENDREGION}
{$REGION 'Open Assitant Classes'}
{ TAiMessage }

constructor TAiMessage.Create(aApiKey: String; aUrl: String = '');
begin
  Inherited Create;
  Ffile_ids := TStringList.Create;
  FMetadata := TAiMetadata.Create;
  FFiles := TAiFileArray.Create;
  Url := aUrl;
end;

destructor TAiMessage.Destroy;
begin
  Ffile_ids.Free;
  FMetadata.Free;
  FFiles.Free;

  FContent := nil;

  inherited Destroy;
end;

procedure TAiMessage.SetApiKey(const Value: String);
begin
  FApiKey := Value;
end;

procedure TAiMessage.SetAssisistantId(const Value: String);
begin
  FAssisistantId := Value;
end;

procedure TAiMessage.SetContent(const Value: TAiMessageContentArray);
begin
  FContent := Value;
end;

procedure TAiMessage.Setfile_ids(const Value: TStringList);
begin
  Ffile_ids := Value;
end;

procedure TAiMessage.SetFinishReason(const Value: String);
begin
  FFinishReason := Value;
end;

procedure TAiMessage.SetMessageId(const Value: String);
begin
  FMessageId := Value;
end;

procedure TAiMessage.SetMetadata(const Value: TAiMetadata);
begin
  FMetadata := Value;
end;

procedure TAiMessage.Setrole(const Value: String);
begin
  FRole := Value;
end;

procedure TAiMessage.SetRunId(const Value: String);
begin
  FRunId := Value;
end;

procedure TAiMessage.SetThReadId(const Value: String);
begin
  FThReadId := Value;
end;

procedure TAiMessage.SetUrl(const Value: String);
begin
  If Value <> '' then
    FUrl := Value
  Else
    FUrl := GlOpenAIUrl;
end;

function TAiMessage.ToJSon(ShowAll: Boolean = False): TJSonObject;
Var
  JFiles: TJSonArray;
  i: Integer;
  Con: String;
begin
  Result := TJSonObject.Create;

  If FRole <> '' then
    Result.AddPair('role', FRole);

  If Length(FContent) > 0 then
  Begin
    For i := 0 to Length(FContent) - 1 do
      Con := Con + FContent[i].Text + sLineBreak;

    Result.AddPair('content', Trim(Con));
  End;

  If ShowAll then
  Begin
    If FMessageId <> '' then
      Result.AddPair('MessageId', UTF8Encode(FMessageId));

    If FAssisistantId <> '' then
      Result.AddPair('AsissistantId', UTF8Encode(FAssisistantId));

    If RunId <> '' then
      Result.AddPair('RunId', UTF8Encode(FRunId));
  End;

  If Ffile_ids.Count > 0 then
  Begin
    JFiles := TJSonArray.Create;

    For i := 0 to file_ids.Count - 1 do
    Begin
      If file_ids[i].Trim <> '' then
        JFiles.Add(file_ids[i].Trim);
    End;

    Result.AddPair('file_ids', JFiles);
  End;

  Result.AddPair('metadata', FMetadata.ToJSon);

end;

function TAiMessage.ToString: String;
Var
  Con: String;
  i: Integer;
begin
  Result := '';

  If Length(FContent) > 0 then
  Begin
    For i := 0 to Length(FContent) - 1 do
      Con := Con + FContent[i].Text + sLineBreak;
  End;
  Result := Con;
end;

destructor TAiMessages.Destroy;
begin
  for var item in Self do
    Item.Free;
  inherited;
end;

{ TAiMessages }

{
  function TAiMessages.AddMessage(aRole, aContent, aFiles_ids: String): TAiMessage;
  Var
  AiMessage: TAiMessage;
  begin
  AiMessage := TAiMessage.Create(aRole, aContent, aFiles_ids);
  Self.Add(AiMessage);
  end;
}

function TAiMessages.ToJSon(ShowAll: Boolean = False): TJSonArray;
Var
  AiMessage: TAiMessage;
begin
  Result := TJSonArray.Create;
  For AiMessage in Self do
    Result.Add(AiMessage.ToJSon(ShowAll));
end;

{ TAssistant }

function TAiAssistant.CodeFileAttach(FileId: String): Boolean;
begin

end;

function TAiAssistant.CodeFileDetach(FileId: String): Boolean;
begin

end;

function TAiAssistant.CodeFileList: TJSonObject;
begin

end;

function TAiAssistant.CodeFileListFilesArray: TAiFileArray;
begin

end;

function TAiAssistant.CodeFileUploadAndAttach(FileId: String): Boolean;
begin

end;

constructor TAiAssistant.Create(aApiKey: String; aUrl: String = '');
begin
  Inherited Create;
  FApiKey := aApiKey;
  FMetadata := TAiMetadata.Create;
  FFilesIds := TStringList.Create;
  FFunciones := TAiToolsFunctions.Create;
  FVectorStoreIds := TStringList.Create;
  FFiles := TAiFiles.Create(ApiKey, '');
  FVectorStores := TAiVectorStores.Create(ApiKey, ''); // la url se asigna en la siguiente linea
  Url := aUrl;
  FTemperature := 1; // de 0 a 2,  a mayor valor más creativo, valores cercanos a cero mas deterministico
  Ftop_p := 1; // de 0 a 1 límite de probabilidad,  1 = 100% acepta todas las probabilidades de los tokens,  cercano a cero como 0.1 limita solo al 10% más alto de probabilidad
end;

destructor TAiAssistant.Destroy;
begin
  FOnBeforeResponse := Nil;
  FMetadata.Free;
  FFilesIds.Free;
  FFunciones.Free;
  FVectorStoreIds.Free;
  FFiles.Free;
  FVectorStores.Free;
  if Assigned(FJSonObject) then
    FJSonObject.Free;
  Inherited Destroy;
end;

Function TAiAssistant.CreateNewAssistant(aModelo, aNombre, aInstrucciones: String; aFunciones: TAiToolsFunctions; aFileSearch: Boolean = False; aVectorStoreIds: String = ''; aCodeInterpreter: Boolean = False; aCodeFilesIds: String = '';
  aTemperature: Double = 1; atop_p: Double = 1): Boolean;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj: TJSonObject;
  JTools, JFiles, JVectors: TJSonArray;
  JToolResources, JFileSearch, JCodeInterpreter: TJSonObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
  i: Integer;
  ListFilesIds: TStringList;
begin
  If FApiKey = '' then
    Raise Exception.Create('Inicialice el ApiKey antes de continuar');

  FMetadata.Clear;
  FFilesIds.Clear;
  FFunciones.Clear;
  FName := '';
  FModel := '';
  FInstructions := '';
  FAssistantId := '';
  FreeAndNil(FJSonObject);
  FCode_Interpreter := False;
  FFileSearch := False;

  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  ListFilesIds := TStringList.Create;
  JObj := TJSonObject.Create;

  JToolResources := TJSonObject.Create;
  JFileSearch := TJSonObject.Create;
  JCodeInterpreter := TJSonObject.Create;
  JToolResources.AddPair('file_search', JFileSearch);
  JToolResources.AddPair('code_interpreter', JCodeInterpreter);

  sUrl := FUrl + 'assistants';

  Try
    JObj.AddPair('model', aModelo);
    JObj.AddPair('name', aNombre);
    JObj.AddPair('instructions', aInstrucciones);

    JTools := aFunciones.ToFunctionsJSon;

    If aCodeInterpreter = True then
      JTools.Add(TJSonObject.Create.AddPair('type', 'code_interpreter'));

    If aFileSearch = True then
      JTools.Add(TJSonObject.Create.AddPair('type', 'file_search'));

    JObj.AddPair('tools', JTools);

    JFiles := TJSonArray.Create;
    JVectors := TJSonArray.Create;

    // Adiciona los VectorsIds de los archivos de filesearch

    ListFilesIds.CommaText := aVectorStoreIds;

    For i := 0 to ListFilesIds.Count - 1 do
      JVectors.Add(ListFilesIds[i]);

    JFileSearch.AddPair('vector_store_ids', JVectors);

    // Adiciona los FilesIds de los archivos de code_interpreter
    ListFilesIds.CommaText := aCodeFilesIds;

    For i := 0 to ListFilesIds.Count - 1 do
      JFiles.Add(ListFilesIds[i]);

    JCodeInterpreter.AddPair('file_ids', JFiles);

    JObj.AddPair('tool_resources', JToolResources);

    // JObj.AddPair('metadata', aMetadata.ToJSon);

    JObj.AddPair('temperature', aTemperature.ToString);
    JObj.AddPair('top_p', atop_p.ToString);

    St.WriteString(JObj.Format);
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    FTxtJson := JObj.Format;

    {
      var
      Lista := TStringList.Create;
      Begin
      Lista.Text := FTxtJson;
      Lista.SaveToFile('c:\temp\createassistant.json.txt');
      Lista.Free;
      Exit;
      End;
    }

    Res := Client.Post(sUrl, St, Response, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        DoBeforeResponse(Self, 'TAiAssistant.CreateNewAssistant', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      FJSonObject := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      ParseAssistantJson(FJSonObject);
      Result := True;
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
    St.Free;
    Response.Free;
    ListFilesIds.Free;
    JObj.Free;
  End;
end;

procedure TAiAssistant.DoBeforeResponse(Sender: TObject; Funcion, Value: String);
begin
  If Assigned(FOnBeforeResponse) then
    FOnBeforeResponse(Sender, Funcion, Value);
end;

class function TAiAssistant.GetAssistantIdByName(aApiKey: String; AssistantName: String; aUrl: String = ''): String;
Var
  JArr: TJSonArray;
  JVal: TJSonValue;
  JList, JObj: TJSonObject;
  sId, sName: String;
begin
  Result := '';

  JList := GetList(aApiKey, aUrl, 100);
  try
    JArr := JList.GetValue<TJSonArray>('data');

    For JVal in JArr do
    Begin
      JObj := TJSonObject(JVal);
      sId := JObj.GetValue<String>('id');
      sName := JObj.GetValue<String>('name');

      If SameText(AssistantName, sName) then
      Begin
        Result := sId;
        Break;
      End;
    End;
  finally
    JList.Free;
  end;
end;

Class function TAiAssistant.GetList(aApiKey: String; aUrl: String = ''; Limit: Integer = 20; Order: String = 'desc'): TJSonObject;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  Response: TStringStream;
  sUrl, EndPointUrl: String;
begin

  If aUrl <> '' then
    EndPointUrl := aUrl
  Else
    EndPointUrl := GlOpenAIUrl;

  If Limit > 100 then
    Limit := 100;

  Client := THTTPClient.Create;
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := EndPointUrl + 'assistants?limit=' + Limit.ToString + '&order=' + Order;

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + aApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      Result := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
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

Class function TAiAssistant.GetModels(aApiKey: String; aUrl: String = ''): TJSonObject;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  Response: TStringStream;
  sUrl, EndPointUrl: String;
begin

  If aUrl <> '' then
    EndPointUrl := aUrl
  Else
    EndPointUrl := GlOpenAIUrl;

  Client := THTTPClient.Create;
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := EndPointUrl + 'models';

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + aApiKey)];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      Result := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
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

function TAiAssistant.LoadAssistant(aAssistantId: String): Boolean;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  Response: TStringStream;
  sUrl: String;
begin

  FMetadata.Clear;
  FFilesIds.Clear;
  FFunciones.Clear;
  FName := '';
  FModel := '';
  FInstructions := '';
  FAssistantId := '';
  FreeAndNil(FJSonObject);
  FCode_Interpreter := False;
  FFileSearch := False;

  FFiles.FApiKey := Self.FApiKey;
  FVectorStores.FApiKey := Self.FApiKey;

  Client := THTTPClient.Create;
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := FUrl + 'assistants/' + aAssistantId;

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Response, Headers);

    // Response.SaveToFile('c:\temp\loadasistant.json.txt');

    TThread.Synchronize(nil,
      procedure
      begin
        DoBeforeResponse(Self, 'TAiAssistant.LoadAssistant', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      FJSonObject := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      ParseAssistantJson(FJSonObject);
      Result := True;
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

function TAiAssistant.ApplyUpdates: TJSonObject;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj: TJSonObject;
  JTools, JFiles: TJSonArray;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
  i: Integer;
  JToolResources: TJSonObject;
  JFileSearch: TJSonObject;
  JCodeInterpreter: TJSonObject;
  JVectors: TJSonArray;

begin

  If AssistantId = '' then
    Raise Exception.Create('Primero debe crear un asistente');

  If FModel.Trim = '' then
    Raise Exception.Create('El nombre del modelo es obligatorio');

  If Name.Trim = '' then
    Raise Exception.Create('El nombre del Asistente es obligatorio');

  If FInstructions.Trim = '' then
    Raise Exception.Create('Eres un asistente muy servicial');

  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := FUrl + 'assistants/' + Self.AssistantId;

  JToolResources := TJSonObject.Create;
  JFileSearch := TJSonObject.Create;
  JCodeInterpreter := TJSonObject.Create;
  JToolResources.AddPair('file_search', JFileSearch);
  JToolResources.AddPair('code_interpreter', JCodeInterpreter);

  Try
    JObj := TJSonObject.Create;
    JObj.AddPair('model', FModel);
    JObj.AddPair('name', FName);
    JObj.AddPair('instructions', FInstructions);

    JTools := FFunciones.ToFunctionsJSon;

    If FCode_Interpreter = True then
      JTools.Add(TJSonObject.Create.AddPair('type', 'code_interpreter'));

    If FFileSearch = True then
      JTools.Add(TJSonObject.Create.AddPair('type', 'file_search'));

    JObj.AddPair('tools', JTools);

    { JFiles := TJSonArray.Create;

      For i := 0 to FFilesIds.Count - 1 do
      JFiles.Add(FFilesIds[i].Trim);

      JObj.AddPair('file_ids', JFiles);

      JObj.AddPair('metadata', FMetadata.ToJSon);
    }

    JFiles := TJSonArray.Create;
    JVectors := TJSonArray.Create;

    // Adiciona los VectorsIds de los archivos de filesearch

    // ListFilesIds.CommaText := aVectorStoreIds;

    For i := 0 to FVectorStoreIds.Count - 1 do
      JVectors.Add(FVectorStoreIds[i]);

    JFileSearch.AddPair('vector_store_ids', JVectors);

    // Adiciona los FilesIds de los archivos de code_interpreter

    For i := 0 to FFilesIds.Count - 1 do
      JFiles.Add(FFilesIds[i]);

    JCodeInterpreter.AddPair('file_ids', JFiles);

    JObj.AddPair('tool_resources', JToolResources);

    JObj.AddPair('temperature', FTemperature);
    JObj.AddPair('top_p', Ftop_p);

    St.WriteString(JObj.Format);
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    // St.SaveToFile('c:\temp\request.json.txt');

    Res := Client.Post(sUrl, St, Response, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        DoBeforeResponse(Self, 'TAiAssistant.ApplyUpdates', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      if Assigned(FJSONObject) then FreeAndNil(FJSONObject);
      FJSonObject := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      Result := TJSonObject(FJSonObject.Clone);
      ParseAssistantJson(FJSonObject);
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;
  Finally
    Client.Free;
    St.Free;
    Response.Free;
  End;
end;

procedure TAiAssistant.ParseAssistantJson(Obj: TJSonObject);
Var
  Tools, FileIds: TJSonArray;
  JVal: TJSonValue;
  JObj, JFunc, jMetadata: TJSonObject;
  JToolResources, JFileSearch, JCodeInterpreter: TJSonObject;
  Pair: TJSONPair;
  Tipo, FunName, S: String;
  FNumTemp, FNumTop: Double;
begin

  FCode_Interpreter := False;
  FFileSearch := False;
  FFunciones.Clear;
  FMetadata.Clear;
  FFilesIds.Clear;
  FVectorStoreIds.Clear;

  FAssistantId := Obj.GetValue<String>('id');
  FName := Obj.GetValue<String>('name');
  FModel := Obj.GetValue<String>('model');
  FInstructions := Obj.GetValue<String>('instructions');

  If Obj.TryGetValue<Double>('temperature', FNumTemp) then
    Self.Temperature := FNumTemp;

  If Obj.TryGetValue<Double>('top_p', FNumTop) then
    Self.top_p := FNumTop;

  Tools := Obj.GetValue<TJSonArray>('tools');

  Self.FFunciones.Clear; // Limpia la lista de funciones para actualizar

  For JVal in Tools do
  Begin
    JObj := TJSonObject(JVal);

    S := JObj.Format;
    Tipo := JObj.GetValue<String>('type');

    If Tipo = 'code_interpreter' then
      FCode_Interpreter := True;

    If Tipo = 'file_search' then
      FFileSearch := True;

    If Tipo = 'function' then
    Begin
      JFunc := JObj.GetValue<TJSonObject>('function');
      FunName := JFunc.GetValue<string>('name');
      FFunciones.AddFunction(JObj);
    End;
  End;

  If Obj.TryGetValue<TJSonObject>('tool_resources', JToolResources) then
  Begin
    If JToolResources.TryGetValue<TJSonObject>('file_search', JFileSearch) then
    Begin
      FileIds := JFileSearch.GetValue<TJSonArray>('vector_store_ids');

      For JVal in FileIds do
      Begin
        FVectorStoreIds.Add(JVal.Value);
      End;
    End;

    If JToolResources.TryGetValue<TJSonObject>('code_interpreter', JCodeInterpreter) then
    Begin
      FileIds := JCodeInterpreter.GetValue<TJSonArray>('file_ids');

      For JVal in FileIds do
        FFilesIds.Add(JVal.Value);
    End;
  End;

  jMetadata := Obj.GetValue<TJSonObject>('metadata');

  For Pair in jMetadata do
    FMetadata.Add(Pair.JsonString.Value, Pair.JsonValue.Value);
end;

function TAiAssistant.Remove: TJSonObject;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  Response: TStringStream;
  sUrl: String;
  id: String;
begin

  id := TAiAssistant.GetAssistantIdByName(ApiKey, Name);

  If id = '' then
    Raise Exception.Create('El asistente "' + Name + '" no existe, no se puede eliminar');

  Client := THTTPClient.Create;
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := FUrl + 'assistants/' + id;

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Delete(sUrl, Response, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        DoBeforeResponse(Self, 'TAiAssistant.Remove', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      Result := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
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

procedure TAiAssistant.SetApiKey(const Value: String);
begin
  FApiKey := Value;
end;

procedure TAiAssistant.SetAssistantId(const Value: String);
begin
  FAssistantId := Value;
end;

procedure TAiAssistant.SetCode_Interpreter(const Value: Boolean);
begin
  FCode_Interpreter := Value;
end;

procedure TAiAssistant.SetFilesIds(const Value: TStringList);
begin
  FFilesIds := Value;
end;

procedure TAiAssistant.SetFunciones(const Value: TAiToolsFunctions);
begin
  FFunciones := Value;
end;

procedure TAiAssistant.SetInstructions(const Value: String);
begin
  FInstructions := Value;
end;

procedure TAiAssistant.SetJSonObject(const Value: TJSonObject);
begin
  FJSonObject := Value;
end;

procedure TAiAssistant.SetMetadata(const Value: TAiMetadata);
begin
  FMetadata := Value;
end;

procedure TAiAssistant.SetModel(const Value: String);
begin
  FModel := Value;
end;

procedure TAiAssistant.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TAiAssistant.SetOnBeforeResponse(const Value: TOnBeforeResponse);
begin
  FOnBeforeResponse := Value;
end;

procedure TAiAssistant.SetFileSearch(const Value: Boolean);
begin
  FFileSearch := Value;
end;

procedure TAiAssistant.SetTemperature(const Value: Double);
Var
  F: Double;
begin
  F := Value;
  If F < 0 then
    F := 0
  else If F > 2 then
    F := 2;

  FTemperature := F;
end;

procedure TAiAssistant.Settop_p(const Value: Double);
Var
  F: Double;
begin
  F := Value;
  If F <= 0 then
    F := 0.1
  else If F > 1 then
    F := 1;

  Ftop_p := F;
end;

procedure TAiAssistant.SetUrl(const Value: String);
begin
  If Value <> '' then
    FUrl := Value
  else
    FUrl := GlOpenAIUrl;

  If Assigned(FFiles) then
    FFiles.Url := FUrl;

  If Assigned(FVectorStores) then
    FVectorStores.Url := FUrl;
end;

function TAiAssistant.VectorStoreFileAttach(VectorStoreId, FileId: String): Boolean;
Var
  Lista: TAiVectorStoreArray;
  Store: TAiVectorStore;
begin
  If VectorStoreId <> '' then
  Begin
    Store := VectorStores.Retrive(VectorStoreId);
  End
  Else
  Begin
    Lista := VectorStores.List; // Este hace parte del objeto, no se elimina
    If Lista.Count <= 0 then
      Raise Exception.Create('El Assistente no tiene asignado un VectorStore');
    Store := Lista.ToArray[0].Value;
  End;

  Try
    Store.Files.AtachFile(FileId);
  Finally
    Store.Free;
  End;
end;

function TAiAssistant.VectorStoreFileUploadAndAttach(VectorStoreId, filename: String): TAiFile;
begin
  Result := FFiles.UploadFile(filename, True);

  If Assigned(Result) then
    Self.VectorStoreFileAttach(VectorStoreId, Result.id);
end;

function TAiAssistant.VectorStoreFileDetach(VectorStoreId, FileId: String): Boolean;
Var
  Lista: TAiVectorStoreArray;
  Store: TAiVectorStore;
begin
  If VectorStoreId <> '' then
  Begin
    Store := VectorStores.Retrive(VectorStoreId);
  End
  Else
  Begin
    Lista := VectorStores.List; // Este hace parte del objeto, no se elimina
    If Lista.Count <= 0 then
      Raise Exception.Create('El Assistente no tiene asignado un VectorStore');
    Store := Lista.ToArray[0].Value;
  End;

  Try
    Store.Files.DetachFile(FileId);
  Finally
    Store.Free;
  End;
end;

function TAiAssistant.VectorStoreFileList(VectorStoreId: String): TAiVectorStoreFileArray;
Var
  Lista: TAiVectorStoreArray;
  Store: TAiVectorStore;
begin
  If VectorStoreId <> '' then
  Begin
    Store := VectorStores.Retrive(VectorStoreId);
  End
  Else
  Begin
    Lista := VectorStores.List; // Este hace parte del objeto, no se elimina
    If Lista.Count <= 0 then
      Raise Exception.Create('El Assistente no tiene asignado un VectorStore');
    Store := Lista.ToArray[0].Value;
  End;

  Try
    Store.Files.ListFiles;
  Finally
    Store.Free;
  End;
end;

{ TAiThRead }

constructor TAiThRead.Create(aAiAssistant: TAiAssistant);
begin
  Inherited Create;
  FAiAssistant := aAiAssistant;
  FMessages := TAiMessages.Create;
  FMetadata := TAiMetadata.Create;
  Url := aAiAssistant.Url;
end;

function TAiThRead.AddMessage(aMessage: String; aRole: String = ''; aFiles_ids: String = ''): TAiMessage;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj: TJSonObject;
//  JFiles: TJSonArray;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
  i: Integer;
  ListFilesIds: TStringList;
  AiMessage: TAiMessage;
  JRes: TJSonObject;
  JAttachments, JTools: TJSonArray;
  JAttachment: TJSonObject;
begin

  If ThReadId = '' then
    Raise Exception.Create('Debe crear primero un thread');

  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  ListFilesIds := TStringList.Create;

  If aRole = '' then
    aRole := 'user';

  sUrl := FUrl + 'threads/' + ThReadId + '/messages';
  JObj := TJSonObject.Create;

  Try
    JObj.AddPair('role', aRole);
    JObj.AddPair('content', aMessage);

    If (aFiles_ids <> '') then // Si hay archivos anexos
    Begin
      JAttachments := TJSonArray.Create;
      JObj.AddPair('attachments', JAttachments);

      ListFilesIds.CommaText := aFiles_ids;
//      JFiles := TJSonArray.Create;

      For i := 0 to ListFilesIds.Count - 1 do
      Begin
        JAttachment := TJSonObject.Create;
        JAttachment.AddPair('file_id', ListFilesIds[i]);

        JTools := TJSonArray.Create;
        JAttachment.AddPair('tools', JTools);
        JTools.Add(TJSonObject.Create.AddPair('type', 'file_search'));
        JTools.Add(TJSonObject.Create.AddPair('type', 'code_interpreter'));

        JAttachments.Add(JAttachment);
        // JFiles.Add(ListFilesIds[i]);
      End;
    End;

    St.WriteString(UTF8Encode(JObj.Format));
    St.Position := 0;

    // St.SaveToFile('c:\temp\aimessage.json.txt');
    // St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAiAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAiAssistant) then
          FAiAssistant.DoBeforeResponse(Self, 'TAiThRead.AddMessage', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      JRes := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      try
        AiMessage := ParseMessage(JRes);
        AiMessage.Url := FUrl;
        FMessages.Insert(0, AiMessage);
        Result := AiMessage;
      finally
        JRes.Free;
      end;
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
    ListFilesIds.Free;
  End;
end;

function TAiThRead.ApplyUpdates: Boolean;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj: TJSonObject;
  jMetadata: TJSonObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
  Clave: String;
begin

  If Self.FThReadId = '' then
    Raise Exception.Create('Debe crear primero el hilo');

  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := FUrl + 'threads/' + ThReadId;
  JObj := TJSonObject.Create;

  Try
    If Assigned(FMetadata) and (FMetadata.Count > 0) then
    Begin
      jMetadata := TJSonObject.Create;

      For Clave in Metadata.Keys do
        jMetadata.AddPair(Clave, Metadata.Items[Clave]);

      JObj.AddPair('metadata', jMetadata);
    End;

    St.WriteString(JObj.Format);
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAiAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAiAssistant) then
          FAiAssistant.DoBeforeResponse(Self, 'TAiThRead.ApplyUpdates', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      if Assigned(FJSONObject) then FreeAndNil(FJSONObject);
      FJSonObject := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      ParseThReadJson(FJSonObject);
      Result := True;
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

function TAiThRead.CreateNewThRead(aMensajes: TAiMessages; aMetadata: TAiMetadata; aVectorStoreIds: String = ''; aCodeFilesIds: String = ''): Boolean;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj: TJSonObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;

  JCodeFiles, JVectors: TJSonArray;
  JToolResources, JFileSearch, JCodeInterpreter: TJSonObject;
  ListFilesIds: TStringList;
  i: Integer;

begin
  FMessages.Clear;
  FMetadata.Clear;

  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := FUrl + 'threads';
  JObj := TJSonObject.Create;

  JToolResources := TJSonObject.Create;
  ListFilesIds := TStringList.Create;

  Try
    If Assigned(aMensajes) and (aMensajes.Count > 0) then
      JObj.AddPair('messages', aMensajes.ToJSon);

    If Assigned(aMetadata) and (aMetadata.Count > 0) then
      JObj.AddPair('metadata', aMetadata.ToJSon);

    // Adiciona los VectorsIds de los archivos de filesearch

    ListFilesIds.CommaText := aVectorStoreIds;
    If ListFilesIds.Count > 0 then
    Begin
      JFileSearch := TJSonObject.Create;
      JVectors := TJSonArray.Create;

      JToolResources.AddPair('file_search', JFileSearch);

      For i := 0 to ListFilesIds.Count - 1 do
        JVectors.Add(ListFilesIds[i]);

      JFileSearch.AddPair('vector_store_ids', JVectors);
    End;

    // Adiciona los FilesIds de los archivos de code_interpreter
    ListFilesIds.CommaText := aCodeFilesIds;
    If ListFilesIds.Count > 0 then
    Begin
      JCodeInterpreter := TJSonObject.Create;
      JCodeFiles := TJSonArray.Create;

      JToolResources.AddPair('code_interpreter', JCodeInterpreter);

      For i := 0 to ListFilesIds.Count - 1 do
        JCodeFiles.Add(ListFilesIds[i]);

      JCodeInterpreter.AddPair('file_ids', JCodeFiles);
    End;

    JObj.AddPair('tool_resources', JToolResources);

    // --------------------------

    St.WriteString(UTF8Encode(JObj.Format));
    St.Position := 0;

    // St.SaveToFile('c:\temp\crearthread.json.txt');
    // St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAiAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAiAssistant) then
          FAiAssistant.DoBeforeResponse(Self, 'TAiThRead.CreateNewThRead', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      if Assigned(FJSONObject) then FreeAndNil(FJSONObject);
      FJSonObject := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      ParseThReadJson(FJSonObject);
      Result := True;
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
    ListFilesIds.Free;
  End;
end;

destructor TAiThRead.Destroy;
begin
  FMessages.Free;
  FMetadata.Free;
  if Assigned(FJSonObject) then
    FJSonObject.Free;
  inherited;
end;

function TAiThRead.GetMessage(aMessageId: String): TAiMessage;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  sUrl: String;
  JRes: TJSonObject;
begin
  Client := THTTPClient.Create;
  sUrl := FUrl + 'threads/' + ThReadId + '/messages/' + aMessageId;

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAiAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Nil, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAiAssistant) then
          FAiAssistant.DoBeforeResponse(Self, 'TAiThRead.GetMessage', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      JRes := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      try
        Result := ParseMessage(JRes);
        Result.Url := FUrl;
      finally
        JRes.Free;
      end;
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;
  Finally
    Client.Free;
  End;
end;

function TAiThRead.GetMessageFile(aThReadId, aMessageId, aFileId: String): TJSonObject;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  sUrl: String;
  Response: TMemoryStream;
begin

  If aMessageId = '' then
    Raise Exception.Create('Debe ser un MessageId válido');

  If aFileId = '' then
    Raise Exception.Create('Debe ser un FileId válido');

  Client := THTTPClient.Create;
  Response := TMemoryStream.Create;
  sUrl := FUrl + 'threads/' + ThReadId + '/messages/' + aMessageId + '/files/' + aFileId;

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAiAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Response, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAiAssistant) then
          FAiAssistant.DoBeforeResponse(Self, 'TAiThRead.GetMessageFile', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      Result := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
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

function TAiThRead.ListMessageFiles(aMsgId: String): TJSonObject;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  JRes: TJSonObject;
  sUrl: String;
begin
  If ThReadId = '' then
    Raise Exception.Create('debe crear primero un thread');

  Client := THTTPClient.Create;
  sUrl := FUrl + 'threads/' + Self.ThReadId + '/messages/' + aMsgId + '/files';

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAiAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Nil, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAiAssistant) then
          FAiAssistant.DoBeforeResponse(Self, 'TAiThRead.ListMessages', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      JRes := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      Result := JRes;
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;
  Finally
    Client.Free;
  End;
end;

function TAiThRead.ListMessages(Limit: Integer; Order: String): TAiMessages;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj, JRes: TJSonObject;
  JVal: TJSonValue;
  Res: IHTTPResponse;
  sUrl: String;
  Data: TJSonArray;
  AiMessages: TAiMessages;
  AiMessage: TAiMessage;
begin
  If ThReadId = '' then
    Raise Exception.Create('debe crear primero un thread');

  Client := THTTPClient.Create;
  sUrl := FUrl + 'threads/' + ThReadId + '/messages?limit=' + Limit.ToString + '&order=' + Order;

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAiAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Nil, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAiAssistant) then
          FAiAssistant.DoBeforeResponse(Self, 'TAiThRead.ListMessages', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      JRes := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));

      AiMessages := TAiMessages.Create;
      try
        Data := JRes.GetValue<TJSonArray>('data');
        For JVal in Data do
        Begin
          JObj := TJSonObject(JVal);
          AiMessage := ParseMessage(JObj);
          AiMessage.Url := FUrl;
          // AiMessages.Insert(0, AiMessage);
          AiMessages.Add(AiMessage);
        End;
      finally
        JRes.Free;
      end;

      Result := AiMessages;
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;
  Finally
    Client.Free;
  End;
end;

function TAiThRead.LoadThRead(ThReadId: String): Boolean;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  sUrl: String;
begin
  FMessages := TAiMessages.Create;
  FMetadata := TAiMetadata.Create;

  Client := THTTPClient.Create;
  sUrl := FUrl + 'threads/' + ThReadId;

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAiAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Nil, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAiAssistant) then
          FAiAssistant.DoBeforeResponse(Self, 'TAiThRead.LoadThRead', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      if Assigned(FJSONObject) then FreeAndNil(FJSONObject);
      FJSonObject := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      ParseThReadJson(FJSonObject);
      Result := True;
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
  End;
end;

function TAiThRead.MessageApplyUpdate(aMessage: TAiMessage): TJSonObject;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj: TJSonObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
begin
  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := FUrl + 'threads/' + ThReadId + '/messages/' + aMessage.MessageId;
  JObj := TJSonObject.Create;

  Try

    If Assigned(aMessage.Metadata) and (aMessage.Metadata.Count > 0) then
      JObj.AddPair('metadata', aMessage.Metadata.ToJSon);

    St.WriteString(UTF8Encode(JObj.Format));
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAiAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAiAssistant) then
          FAiAssistant.DoBeforeResponse(Self, 'TAiThRead.MessageApplyUpdate', Res.ContentAsString);
      end);

    Result := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));

  Finally
    Client.Free;
    St.Free;
    Response.Free;
    JObj.Free;
  End;
end;

procedure TAiThRead.ParseThReadJson(JObj: TJSonObject);
Var
  sMeta: String;
begin
  Self.FThReadId := JObj.GetValue<String>('id');
  sMeta := JObj.GetValue<TJSonObject>('metadata').Format;
  FMetadata.JsonText := sMeta;
end;

function TAiThRead.ParseMessage(JObj: TJSonObject): TAiMessage;
Var
  JObj1, jText, JObjAnn: TJSonObject;
  JContent: TJSonArray;
  JAnnotations: TJSonArray;
  JVal, JVal1: TJSonValue;
  AiFile: TAiFile;
  AiFiles: TAiFiles;
  MessageId, ThReadId, FileId, Value: String;
  filename: String;
  i, J: Integer;
begin
  Result := TAiMessage.Create(FAiAssistant.ApiKey);
  AiFiles := TAiFiles.Create(FAiAssistant.ApiKey);

  Try
    MessageId := JObj.GetValue<String>('id');
    ThReadId := JObj.GetValue<String>('thread_id');

    Result.FMessageId := MessageId;
    Result.ThReadId := ThReadId;
    Result.role := JObj.GetValue<String>('role');
    Result.FAssisistantId := JObj.GetValue<String>('assistant_id');
    Result.RunId := JObj.GetValue<String>('run_id');

    { TODO : Version 2. Falta implementar aquí el filesearch y code_interpreter fileids }
    {
      var JFiles: TJSonArray;
      JFiles := JObj.GetValue<TJSonArray>('file_ids');
      For JVal in JFiles do
      Begin
      FileId := JVal.Value;

      Result.Ffile_ids.Add(FileId);

      If Result.Files.GetFileById(FileId, AiFile) = False then
      Begin
      AiFile := TAiFile.Create(FAiAssistant.ApiKey);
      AiFile.id := FileId;
      // GetMessageFile(ThReadId, MessageId, JVal.Value);
      // Aquí debe recuperar la información del archivo
      Result.Files.Add(FileId, AiFile);
      End;
      End;
    }

    Metadata.JsonText := JObj.GetValue<TJSonObject>('metadata').Format;

    JContent := JObj.GetValue<TJSonArray>('content');

    SetLength(Result.FContent, JContent.Count);

    i := 0;
    For JVal in JContent do
    Begin
      JObj1 := TJSonObject(JVal);

      If JObj1.GetValue<String>('type') = 'text' then
      Begin
        jText := JObj1.GetValue<TJSonObject>('text');
        Value := jText.GetValue<String>('value');

        Result.FContent[i].Tipo := 'text';
        Result.FContent[i].Text := Value;

        JAnnotations := jText.GetValue<TJSonArray>('annotations');
        SetLength(Result.FContent[i].Anotations, JAnnotations.Count);
        J := 0;

        For JVal1 in JAnnotations do
        Begin
          JObjAnn := TJSonObject(JVal1);
          If JObjAnn.GetValue<String>('type') = 'file_path' then
          Begin
            filename := JObjAnn.GetValue<String>('text');

{$IFDEF MSWINDOWS}
            filename := StringReplace(filename, '/', '\', [rfReplaceAll]);
{$ENDIF}
            filename := ExtractFileName(filename);

            FileId := JObjAnn.GetValue<TJSonObject>('file_path').GetValue<String>('file_id');

            Result.FContent[i].Anotations[J].Tipo := 'file_path';
            Result.FContent[i].Anotations[J].Text := JObjAnn.GetValue<String>('text');
            Result.FContent[i].Anotations[J].filename := filename;
            Result.FContent[i].Anotations[J].FileType := ExtractFileExt(filename);
            Result.FContent[i].Anotations[J].FileId := FileId;

            If Result.Files.GetFileById(FileId, AiFile) = False then
            Begin
              AiFile := TAiFile.Create(FAiAssistant.ApiKey);
              AiFile.id := FileId;
              Result.Files.Add(FileId, AiFile);
            End;

            AiFile.Fid := FileId;
            AiFile.FFileType := Result.FContent[i].Anotations[J].FileType;
            AiFile.Ffilename := filename;
          End;
          Inc(J);
        End;
        Inc(i);
      End;

      If JObj1.GetValue<String>('type') = 'image_file' then
      Begin
        FileId := JObj1.GetValue<TJSonObject>('image_file').GetValue<String>('file_id');

        If Result.Files.GetFileById(FileId, AiFile) = False then
        Begin
          AiFile := TAiFile.Create(FAiAssistant.ApiKey);
          AiFile.id := FileId;
          Result.Files.Add(FileId, AiFile);
        End;

        AiFile.Fid := FileId;
        AiFile.FFileType := '.png';
        AiFile.Ffilename := TPath.GetTempFileName + '.png';

        // GetMessageFile(ThReadId, MessageId, FileId);
        // AiFile := AiFiles.GetFile(FileId);
        // AiFile.Content.Position := 0;
        // AiFile.Content.SaveToFile('c:\temp\prueba.png');
      End;
    End;
  Finally
    AiFiles.Free;
  End;
end;

function TAiThRead.Remove: Boolean;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  sUrl: String;
begin
  If ThReadId = '' then
    Exit;

  Client := THTTPClient.Create;
  sUrl := FUrl + 'threads/' + ThReadId;

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAiAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Delete(sUrl, Nil, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAiAssistant) then
          FAiAssistant.DoBeforeResponse(Self, 'TAiThRead.Remove', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      // Result := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
      Result := True;
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;
  Finally
    Client.Free;
  End;
end;

procedure TAiThRead.SetAiAssistant(const Value: TAiAssistant);
begin
  FAiAssistant := Value;
end;

procedure TAiThRead.SetJSonObject(const Value: TJSonObject);
begin
  FJSonObject := Value;
end;

procedure TAiThRead.SetMessages(const Value: TAiMessages);
begin
  FMessages := Value;
end;

procedure TAiThRead.SetMetadata(const Value: TAiMetadata);
begin
  FMetadata := Value;
end;

procedure TAiThRead.SetThReadId(const Value: String);
begin
  FThReadId := Value;
end;

procedure TAiThRead.SetUrl(const Value: String);
begin
  If Value <> '' then
    FUrl := Value
  Else
    FUrl := GlOpenAIUrl;
end;

{ TAiRun }

function TAiRun.ApplyUpdates: TJSonObject;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj: TJSonObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
  JRes: TJSonObject;
begin
  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := FUrl + 'threads/' + ThRead.ThReadId + '/runs/' + RunId;
  JObj := TJSonObject.Create;

  Try

    If Assigned(Metadata) and (Metadata.Count > 0) then
      JObj.AddPair('metadata', Metadata.ToJSon);

    St.WriteString(UTF8Encode(JObj.Format));
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAssistant) then
          FAssistant.DoBeforeResponse(Self, 'TAiRun.ApplyUpdates', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      JRes := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      try
        ParseRun(JRes);
      finally
        JRes.Free;
      end;
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

function TAiRun.Cancel: TJSonObject;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
  JRes: TJSonObject;
begin
  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := FUrl + 'threads/' + ThRead.ThReadId + '/runs/' + RunId + '/cancel';

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAssistant) then
          FAssistant.DoBeforeResponse(Self, 'TAiRun.Cancel', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      JRes := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      try
        ParseRun(JRes);
      finally
        JRes.Free;
      end;
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
    St.Free;
    Response.Free;
  End;
end;

constructor TAiRun.Create(aAssistant: TAiAssistant; aThRead: TAiThRead; aAditionalInstructions: String = ''; aMetadata: TAiMetadata = Nil);
begin
  Inherited Create;

  If Not Assigned(aAssistant) then
    Raise Exception.Create('Se requiere el AssistantId');

  If Not Assigned(aThRead) then
    Raise Exception.Create('Se requiere el ThRead');

  FAssistant := aAssistant;
  ToolsCalls := TAiToolsFunctions.Create;

  FMetadata := TAiMetadata.Create;

  If Assigned(aMetadata) then
    FMetadata.AsText := aMetadata.AsText;

  If Assigned(aThRead) then
    FThRead := aThRead
  Else
  Begin
    FThRead := TAiThRead.Create(FAssistant);
    FThRead.CreateNewThRead(Nil, Nil);
  End;

  Url := aAssistant.Url;

end;

destructor TAiRun.Destroy;
begin
  if Assigned(FJObjectRun) then  FJObjectRun.Free;
  FMetadata.Free;
  FToolsCalls.Free;
  FAssistant := Nil;
  FThRead := Nil;
  inherited;
end;

Function TAiRun.ExtractToolCallsFromJson(JSonObject: TJSonObject): Boolean;
var
  ToolCallsArray: TJSonArray;
  ToolCallObject: TJSonObject;
  ToolCall: TAiToolsFunction;
  JSubmitTools: TJSonObject;
  JVal: TJSonValue;
  Tipo: String;
begin
  FToolsCalls.Clear;
  try
    Tipo := JSonObject.GetValue<String>('type');
    If Tipo = 'submit_tool_outputs' then
    Begin
      If JSonObject.TryGetValue<TJSonObject>('submit_tool_outputs', JSubmitTools) then
      Begin
        ToolCallsArray := JSubmitTools.GetValue<TJSonArray>('tool_calls');
        for JVal in ToolCallsArray do
        begin
          ToolCallObject := TJSonObject(JVal);
          ToolCall := TAiToolsFunction.Create;
          ToolCall.id := ToolCallObject.GetValue('id').Value;
          ToolCall.Tipo := ToolCallObject.GetValue('type').Value;
          ToolCall.name := ToolCallObject.GetValue<TJSonObject>('function').GetValue('name').Value;
          ToolCall.Arguments := ToolCallObject.GetValue<TJSonObject>('function').GetValue('arguments').Value;
          ToolCall.&Function := ToolCallObject.GetValue<TJSonObject>('function').Value;
          FToolsCalls.Add(ToolCall.id, ToolCall);
        end;
      End;
    End;
  finally
    // cant free child object
    // JSonObject.Free;
  end;
  Result := FToolsCalls.Count > 0;
end;

procedure TAiRun.InternalCallToolFunction(AiToolCall: TAiToolsFunction);
begin
  // AiToolCall.Response := 'no tengo ninguna respuesta para esta función "' + AiToolCall.Name;
  // AiToolCall.Response := 'La temperatura es de 26 grados centígrados '
end;

function TAiRun.ListRuns: TJSonObject;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  sUrl: String;
begin
  Client := THTTPClient.Create;
  sUrl := FUrl + 'threads/' + ThRead.ThReadId + '/runs';

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Nil, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAssistant) then
          FAssistant.DoBeforeResponse(Self, 'TAiRun.ListRuns', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      Result := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
  End;
end;

function TAiRun.ListRunsSteps: TJSonObject;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  sUrl: String;
begin
  Client := THTTPClient.Create;
  sUrl := FUrl + 'threads/' + ThRead.ThReadId + '/runs/' + Self.RunId + '/steps';

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Nil, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAssistant) then
          FAssistant.DoBeforeResponse(Self, 'TAiRun.ListRunsSteps', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      Result := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
  End;
end;

procedure TAiRun.ParseRun(JObj: TJSonObject);
var
  TaskList: array of ITask;
  i, NumTasks: Integer;

  JTool: TJSonObject;
  ToolCall: TAiToolsFunction;
  Clave: String;
begin
  FRunId := JObj.GetValue<String>('id');
  FLastError := JObj.GetValue<String>('last_error');
  Fstatus := JObj.GetValue<String>('status');

  // queued succeeded cancelled running, failed uploaded, processed requires_action

  FBusy := (Fstatus <> 'completed') and (Fstatus <> 'succeeded') and (Fstatus <> 'cancelled') and (Fstatus <> 'failed');

  JTool := Nil;
  Try
    JObj.TryGetValue<TJSonObject>('required_action', JTool);
  Except
  End;

  If Assigned(JTool) then
  Begin
    FToolsCalls.Clear;
    If ExtractToolCallsFromJson(JTool) = True then
    Begin
      NumTasks := FToolsCalls.Count;
      SetLength(TaskList, NumTasks);
      i := 0;
      For Clave in FToolsCalls.Keys do
      Begin
        ToolCall := FToolsCalls[Clave];
        TaskList[i] := TTask.Create(
          procedure
          begin
            If Assigned(FOnCallToolFunction) then
              FOnCallToolFunction(Self, ToolCall)
            Else
              InternalCallToolFunction(ToolCall);
          end);
        TaskList[i].Start;
        Inc(i);
      End;
      TTask.WaitForAll(TaskList);
      SubmitTool(FToolsCalls);
    End;
  end;
end;

function TAiRun.Retrieve: TJSonObject;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  sUrl: String;
begin
  Client := THTTPClient.Create;
  sUrl := FUrl + 'threads/' + FThRead.ThReadId + '/runs/' + RunId;

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Nil, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAssistant) then
          FAssistant.DoBeforeResponse(Self, 'TAiRun.Retrieve', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      if Assigned(FJObjectRun) then FreeAndNil(FJObjectRun);
      FJObjectRun := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      ParseRun(FJObjectRun);
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
  End;
end;

function TAiRun.RetrieveStep(StepId: String): TJSonObject;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  sUrl: String;
begin
  Client := THTTPClient.Create;
  sUrl := FUrl + 'threads/' + ThRead.ThReadId + '/runs/' + RunId + '/steps/' + StepId;

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Nil, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAssistant) then
          FAssistant.DoBeforeResponse(Self, 'TAiRun.RetrieveStep', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      Result := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
  End;
end;

function TAiRun.Run: Boolean;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JRes, JObj: TJSonObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
begin
  If Not Assigned(FAssistant) then
    Raise Exception.Create('Se requiere el AssistantId');

  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := FUrl + 'threads/' + FThRead.ThReadId + '/runs';
  JObj := TJSonObject.Create;

  Try

    JObj.AddPair('assistant_id', FAssistant.AssistantId);

    // If FAditionalInstructions <> '' then
    // JObj.AddPair('additional_instructions', FAditionalInstructions);

    If Assigned(FMetadata) and (FMetadata.Count > 0) then
      JObj.AddPair('metadata', FMetadata.ToJSon);

    St.WriteString(UTF8Encode(JObj.Format));
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAssistant) then
          FAssistant.DoBeforeResponse(Self, 'TAiRun.Run', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      JRes := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      try
        ParseRun(JRes);
      finally
        JRes.Free;
      end;
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

procedure TAiRun.SetOnCallToolFunction(const Value: TOnCallToolFunction);
begin
  FOnCallToolFunction := Value;
end;

procedure TAiRun.SetJObjectRun(const Value: TJSonObject);
begin
  FJObjectRun := Value;
end;

procedure TAiRun.SetMetadata(const Value: TAiMetadata);
begin
  FMetadata := Value;
end;

procedure TAiRun.SetToolsCalls(const Value: TAiToolsFunctions);
begin
  FToolsCalls := Value;
end;

procedure TAiRun.SetUrl(const Value: String);
begin
  If Value <> '' then
    FUrl := Value
  Else
    FUrl := GlOpenAIUrl;
end;

function TAiRun.SubmitTool(AitoolsOutputs: TAiToolsFunctions): TJSonObject;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj: TJSonObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
  JRes: TJSonObject;
begin
  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := FUrl + 'threads/' + ThRead.ThReadId + '/runs/' + RunId + '/submit_tool_outputs';

  JObj := TJSonObject.Create;
  Try
    If Assigned(AitoolsOutputs) and (AitoolsOutputs.Count > 0) then
      JObj.AddPair('tool_outputs', AitoolsOutputs.ToOutputJSon);

    St.WriteString(UTF8Encode(JObj.Format));
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAssistant) then
          FAssistant.DoBeforeResponse(Self, 'SubmitTool', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      JRes := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      try
        ParseRun(JRes);
      finally
        JRes.Free;
      end;
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

{$ENDREGION}
{ TAiFiles }

Constructor TAiFiles.Create(aApiKey: String; aUrl: String = '');
begin
  inherited Create;
  FFileList := TAiFileArray.Create;
  FApiKey := aApiKey;
  Url := aUrl;
end;

Destructor TAiFiles.Destroy;
begin
  FFileList.Free;
  inherited;
end;

function TAiFiles.DeleteFile(aFileId: String): Boolean;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  Response: TStringStream;
  sUrl: String;
begin
  Client := THTTPClient.Create;
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := FUrl + 'files/' + aFileId;

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Delete(sUrl, Response, Headers);
    if Res.StatusCode = 200 then
    Begin
      // TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
      Result := True;
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

function TAiFiles.GetFile(aFileId: String): TAiFile;
begin
  If FFileList.GetFileById(aFileId, Result) = False then
  Begin
    Result := TAiFile.Create(FApiKey);
    Result.id := aFileId;
  End;

  Result.GetContent;

end;

function TAiFiles.GetFileByName(aFileName: String): TAiFile;
Var
  AiFile: TAiFile;
begin
  ListFiles; // Actualiza los archivos
  Result := Nil;
  If FFileList.GetFileByName(aFileName, AiFile) then
    Result := Self.GetFile(AiFile.id);
end;

function TAiFiles.GetFileList: TAiFileArray;
begin
  ListFiles;
  Result := FFileList;
end;

function TAiFiles.ListFiles: TAiFileArray;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj: TJSonObject;
  JArr: TJSonArray;
  JVal: TJSonValue;

  Res: IHTTPResponse;
  sUrl: String;
  AiFile: TAiFile;
begin
  FFileList.Clear;

  Client := THTTPClient.Create;
  sUrl := FUrl + 'files';

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Nil, Headers);

    if Res.StatusCode = 200 then
    Begin
      JObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      try
        JArr := JObj.GetValue<TJSonArray>('data');
        For JVal in JArr do
        Begin
          AiFile := TJson.JsonToObject<TAiFile>(TJSonObject(JVal));
          FFileList.Add(AiFile.id, AiFile);
        End;
        Result := FFileList;
      finally
        JObj.Free;
      end;
    End
    else
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);

  Finally
    Client.Free;
  End;
end;

procedure TAiFiles.SetApiKey(const Value: String);
begin
  FApiKey := Value;
end;

procedure TAiFiles.SetUrl(const Value: String);
begin
  If Value <> '' then
    FUrl := Value
  Else
    FUrl := GlOpenAIUrl;
end;

function TAiFiles.UploadFile(filename: String; IsForAssistant: Boolean): TAiFile;
Var
  Body: TMultipartFormData;
  Client: THTTPClient;
  sUrl: String;
  Res: IHTTPResponse;
  Stream: TFileStream;
  JObj: TJSonObject;
  AiFile: TAiFile;
begin
  AiFile := GetFileByName(filename);
  // Evita que se cargue 2 veces un archivo con el mismo nombre
  If Assigned(AiFile) then
    Raise Exception.Create('El archivo "' + filename + '" ya está cargado en el sistema');

  sUrl := FUrl + 'files';

  Client := THTTPClient.Create;
  Try
    Body := TMultipartFormData.Create;
    Try
      Stream := TFileStream.Create(filename, fmOpenRead);
      Try
        Body.AddStream('file', Stream, ExtractFileName(filename));
        // Asumir que 'mydata.jsonl' es application/json

        If IsForAssistant then
          Body.AddField('purpose', 'assistants')
        Else
          Body.AddField('purpose', 'fine-tune');

        // Añadir el header de autorización
        Client.CustomHeaders['Authorization'] := 'Bearer ' + ApiKey;

        // La llamada Post debería cuidar del Content-Type automáticamente
        Res := Client.Post(sUrl, Body, nil);

        if Res.StatusCode = 200 then
        Begin
          JObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
          try
            Result := TJson.JsonToObject<TAiFile>(JObj);
          finally
            JObj.Free;
          end;
        End
        else
          Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
      Finally
        Stream.Free;
      End;
    Finally
      Body.Free;
    End;
  Finally
    Client.Free;
  End;
end;

{ TAiFilesArray }

function TAiFileArray.GetFileById(AiFileId: String; Out AiFile: TAiFile): Boolean;
Var
  Clave: String;
begin
  AiFile := Nil;
  For Clave in Self.Keys do
  Begin
    If SameText(Self.Items[Clave].id, AiFileId) = True then
    Begin
      Result := True;
      AiFile := Self.Items[Clave];
      Break;
    End;
  End;
end;

function TAiFileArray.GetFileByName(aFileName: String; Out AiFile: TAiFile): Boolean;
begin
   AiFile := Nil;
   Result := Self.TryGetValue(aFileName, AiFile);
end;

{ TAiFile }

constructor TAiFile.Create(aApiKey: String; aUrl: String = '');
begin
  inherited Create;
  FApiKey := aApiKey;
  FContent := TMemoryStream.Create;
  Url := aUrl;
end;

destructor TAiFile.Destroy;
begin
  FContent.Free;
  inherited;
end;

function TAiFile.GetContent: TMemoryStream;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Response: TMemoryStream;
  Res: IHTTPResponse;
  sUrl: String;
begin

  FContent.Clear;
  FContent.Position := 0;

  Client := THTTPClient.Create;
  sUrl := FUrl + 'files/' + Fid + '/content';
  Response := TMemoryStream.Create;

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      Response.Position := 0;
      FContent.LoadFromStream(Response);
      FContent.Position := 0;
      Result := FContent;
    End
    else
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);

  Finally
    Client.Free;
    Response.Free;
  End;
end;

procedure TAiFile.SetApiKey(const Value: String);
begin
  FApiKey := Value;
end;

procedure TAiFile.Setfilename(const Value: String);
begin
  Ffilename := Value;
end;

procedure TAiFile.Setid(const Value: String);
begin
  Fid := Value;
end;

procedure TAiFile.SetUrl(const Value: String);
begin
  If Value <> '' then
    FUrl := Value
  Else
    FUrl := GlOpenAIUrl;
end;

{
  Var Requ: IHTTPRequest;
  Client.CredentialsStorage.AddCredential(TCredentialsStorage.TCredential.Create(TAuthTargetType.Server, '', '', 'gustavo', 'C1maMaker*'));
  Requ := Client.GetRequest('PROPFIND', TUri.Create(sUrl));
  Res := Client.Execute(Requ);
}

{ TAiVectorStoreFiles }

constructor TAiVectorStoreFiles.Create(aApiKey, aVectorStoreId: String; aUrl: String = '');
begin
  Inherited Create;
  FApiKey := aApiKey;
  FVectorStoreId := aVectorStoreId;
  FFileList := TAiVectorStoreFileArray.Create;
  Url := aUrl;
end;

destructor TAiVectorStoreFiles.Destroy;
begin
  FFileList.Free;
  inherited;
end;

function TAiVectorStoreFiles.AtachFile(aFileId: String): TAiVectorStoreFile;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj, JRes: TJSonObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
begin

  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := FUrl + 'vector_stores/' + FVectorStoreId + '/files';

  JObj := TJSonObject.Create;

  Try
    JObj.AddPair('file_id', aFileId);
    St.WriteString(JObj.Format);
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      JRes := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      Try
        Result := TJson.JsonToObject<TAiVectorStoreFile>(JRes);
      Finally
        JRes.Free;
      End;
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

function TAiVectorStoreFiles.ListFiles(Limit: Integer = 20; OrderDesc: Boolean = False; After: String = ''; Before: String = ''): TAiVectorStoreFileArray;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj: TJSonObject;
  JArr: TJSonArray;
  JVal: TJSonValue;

  Res: IHTTPResponse;
  sUrl: String;
  StoreFile: TAiVectorStoreFile;
begin
  FFileList.Clear;

  Client := THTTPClient.Create;
  sUrl := FUrl + 'vector_stores/' + FVectorStoreId + '/files';

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Nil, Headers);

    if Res.StatusCode = 200 then
    Begin
      JObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      try
        JArr := JObj.GetValue<TJSonArray>('data');
        For JVal in JArr do
        Begin
          StoreFile := TJson.JsonToObject<TAiVectorStoreFile>(TJSonObject(JVal));
          FFileList.Add(StoreFile.id, StoreFile);
        End;
        Result := FFileList;
      finally
        JObj.Free;
      end;
    End
    else
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);

  Finally
    Client.Free;
  End;
end;

function TAiVectorStoreFiles.GetFileList: TAiVectorStoreFileArray;
begin
  ListFiles;
  Result := FFileList;
end;

function TAiVectorStoreFiles.GetFile(aFileId: String): TAiVectorStoreFile;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Response: TMemoryStream;
  Res: IHTTPResponse;
  sUrl: String;
  JObj: TJSonObject;
begin

  Client := THTTPClient.Create;
  sUrl := FUrl + 'vector_stores/' + FVectorStoreId + '/files/' + aFileId;
  Response := TMemoryStream.Create;

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      JObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      Try
        Result := TJson.JsonToObject<TAiVectorStoreFile>(JObj);
      Finally
        JObj.Free;
      End;
    End
    else
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
  Finally
    Client.Free;
    Response.Free;
  End;
end;

function TAiVectorStoreFiles.DetachFile(aFileId: String): Boolean;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  Response: TStringStream;
  sUrl: String;
begin
  Client := THTTPClient.Create;
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := FUrl + 'vector_stores/' + FVectorStoreId + '/files/' + aFileId;

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Delete(sUrl, Response, Headers);
    if Res.StatusCode = 200 then
    Begin
      // TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
      Result := True;
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

procedure TAiVectorStoreFiles.SetApiKey(const Value: String);
begin
  FApiKey := Value;
end;

procedure TAiVectorStoreFiles.SetUrl(const Value: String);
begin
  If Value <> '' then
    FUrl := Value
  Else
    FUrl := GlOpenAIUrl;
end;

procedure TAiVectorStoreFiles.SetVectorStoreId(const Value: String);
begin
  FVectorStoreId := Value;
end;

{ TAiVectorFilesArray }

function TAiVectorStoreFileArray.GetFileById(AiFileId: String; out AiStoreFile: TAiVectorStoreFile): Boolean;
begin
  Result := Self.TryGetValue(AiFileId, AiStoreFile);
end;

function TAiVectorStoreFileArray.GetFileByVectorStoreId(AiVectorStoreFileId: String; out AiStoreFile: TAiVectorStoreFile): Boolean;
Var
  Clave: String;
begin
  For Clave in Self.Keys do
  Begin
    If SameText(Self.Items[Clave].id, AiVectorStoreFileId) = True then
    Begin
      Result := True;
      AiStoreFile := Self.Items[Clave];
      Break;
    End;
  End;
end;

{ TAiVectorStore }

constructor TAiVectorStore.Create(aApiKey: String; aUrl: String = '');
begin
  Inherited Create;
  FApiKey := aApiKey;
  file_counts := TAiVectorStoreFileCounts.Create;
  FFiles := TAiVectorStoreFiles.Create(aApiKey, '', ''); // la Url se asigna en la siguiente lines
  Url := aUrl;

  // Metadata := TAiMetadata.Create;
end;

destructor TAiVectorStore.Destroy;
begin
  // Metadata.Free;
  file_counts.Free;
  FFiles.Free;
  inherited;
end;

function TAiVectorStore.GetFiles: TAiVectorStoreFiles;
begin
  FFiles.VectorStoreId := Self.id;
  Result := FFiles;
end;

procedure TAiVectorStore.SetUrl(const Value: String);
begin
  If Value <> '' then
    FUrl := Value
  Else
    FUrl := GlOpenAIUrl;

  If Assigned(FFiles) then
    FFiles.Url := FUrl;

end;

{ TAiVectorStores }

constructor TAiVectorStores.Create(aApiKey: String; aUrl: String = '');
begin
  Inherited Create;
  FApiKey := aApiKey;
  FVectorStoreList := TAiVectorStoreArray.Create;
  Url := aUrl;
end;

destructor TAiVectorStores.Destroy;
begin
  FVectorStoreList.Free;
  inherited;
end;

function TAiVectorStores.GetVectorStoreList: TAiVectorStoreArray;
begin
  Result := Self.List(100);
end;

Class function TAiVectorStores.CreateNewStore(aApiKey, aVectorName: String; aUrl: String = ''): TAiVectorStore;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj, JRes: TJSonObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl, EndPointUrl: String;
begin

  if aUrl <> '' then
    EndPointUrl := aUrl
  Else
    EndPointUrl := GlOpenAIUrl;

  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := EndPointUrl + 'vector_stores';

  JObj := TJSonObject.Create;

  Try
    JObj.AddPair('name', aVectorName);
    St.WriteString(JObj.Format);
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + aApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      JRes := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      If Assigned(JRes) then
      Begin
        Try
          Result := ParseVectorStore(JRes, aApiKey, EndPointUrl);
          Result.Url := EndPointUrl;
        Finally
          JRes.Free;
        End;
      End;
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

function TAiVectorStores.List(Limit: Integer = 20; OrderDesc: Boolean = False; After: String = ''; Before: String = ''): TAiVectorStoreArray;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj: TJSonObject;
  JArr: TJSonArray;
  JVal: TJSonValue;

  Res: IHTTPResponse;
  sUrl: String;
  StoreVector: TAiVectorStore;
begin
  FVectorStoreList.Clear;

  Client := THTTPClient.Create;
  sUrl := FUrl + 'vector_stores?limit=' + Limit.ToString;
  { TODO : Falta implementar los parámetros adicionales }

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Nil, Headers);

    if Res.StatusCode = 200 then
    Begin
      JObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      try
        JArr := JObj.GetValue<TJSonArray>('data');
        For JVal in JArr do
        Begin
          StoreVector := ParseVectorStore(TJSonObject(JVal), FApiKey, FUrl);
          FVectorStoreList.Add(StoreVector.id, StoreVector);
        End;
        Result := FVectorStoreList;
      finally
        JObj.Free;
      end;
    End
    else
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);

  Finally
    Client.Free;
  End;
end;

function TAiVectorStores.Retrive(aVectorStoreId: String): TAiVectorStore;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Response: TMemoryStream;
  Res: IHTTPResponse;
  sUrl: String;
  JObj: TJSonObject;
begin

  Client := THTTPClient.Create;
  sUrl := FUrl + 'vector_stores/' + aVectorStoreId;
  Response := TMemoryStream.Create;

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      JObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      Try
        Result := ParseVectorStore(JObj, FApiKey, FUrl);
        Result.Url := FUrl;
      Finally
        JObj.Free;
      End;
    End
    else
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
  Finally
    Client.Free;
    Response.Free;
  End;
end;

procedure TAiVectorStores.SetUrl(const Value: String);
begin
  If Value <> '' then
    FUrl := Value
  Else
    FUrl := GlOpenAIUrl;
end;

function TAiVectorStores.Modify(aVectorStoreId, aVectorName: String): TAiVectorStore;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj, JRes: TJSonObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
begin

  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := FUrl + 'vector_stores/' + aVectorStoreId;

  JObj := TJSonObject.Create;

  Try
    JObj.AddPair('name', aVectorName);
    St.WriteString(JObj.Format);
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      JRes := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      If Assigned(JRes) then
      Begin
        Try
          Result := ParseVectorStore(JRes, FApiKey, FUrl);
          Result.Url := FUrl;
        Finally
          JRes.Free;
        End;
      End;
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

Class function TAiVectorStores.ParseVectorStore(JObj: TJSonObject; aApiKey: String; aUrl: String = ''): TAiVectorStore;
Var
  TmpSv: TAiVectorStore;
  EndPointUrl: String;
begin
  If aUrl <> '' then
    EndPointUrl := aUrl
  Else
    EndPointUrl := GlOpenAIUrl;

  TmpSv := TJson.JsonToObject<TAiVectorStore>(TJSonObject(JObj));
  Try
    Result := TAiVectorStore.Create(aApiKey, EndPointUrl);
    Result.id := TmpSv.id;
    Result.fObject := TmpSv.fObject;
    Result.created_at := TmpSv.created_at;
    Result.usage_bytes := TmpSv.usage_bytes;
    Result.last_active_at := TmpSv.last_active_at;
    Result.name := TmpSv.name;
    Result.status := TmpSv.status;
    Result.file_counts.In_Progress := TmpSv.file_counts.In_Progress;
    Result.file_counts.Completed := TmpSv.file_counts.Completed;
    Result.file_counts.Cancelled := TmpSv.file_counts.Cancelled;
    Result.file_counts.Failed := TmpSv.file_counts.Failed;
    Result.file_counts.Total := TmpSv.file_counts.Total;
    Result.last_used_at := TmpSv.last_used_at;
  Finally
    TmpSv.Free;
  End;
end;

function TAiVectorStores.Delete(aVectorStoreId: String): Boolean;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  Response: TStringStream;
  sUrl: String;
begin
  Client := THTTPClient.Create;
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := FUrl + 'vector_stores/' + aVectorStoreId;

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Delete(sUrl, Response, Headers);
    if Res.StatusCode = 200 then
    Begin
      // TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
      Result := True;
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

{ TAiVectorStoreArray }

function TAiVectorStoreArray.GetVectorStoreById(AiVectorStoreId: String; out AiVectorStore: TAiVectorStore): Boolean;
begin
  Result := Self.TryGetValue(AiVectorStoreId, AiVectorStore);
end;

function TAiVectorStoreArray.GetVectorStoreByName(AiVectorStoreName: String; out AiVectorStore: TAiVectorStore): Boolean;
Var
  Clave: String;
begin
  For Clave in Self.Keys do
  Begin
    If SameText(Self.Items[Clave].name, AiVectorStoreName) = True then
    Begin
      Result := True;
      AiVectorStore := Self.Items[Clave];
      Break;
    End;
  End;
end;

{ TAiVectorStoresFileBatch }

Class function TAiVectorStoresFileBatch.CreateNewVectorStoreBatch(aApiKey, aVectorStoreId, FileIdsList: String): TAiVectorStoreFileBatch;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj, JRes: TJSonObject;
  JArr: TJSonArray;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
  Lista: TStringList;
  i: Integer;
begin

  Client := THTTPClient.Create;
  Lista := TStringList.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := 'https://api.openai.com/v1/vector_stores/' + aVectorStoreId + '/file_batches';

  JObj := TJSonObject.Create;

  Try
    JArr := TJSonArray.Create;

    Lista.Delimiter := ',';
    Lista.DelimitedText := FileIdsList;
    For i := 0 to Lista.Count - 1 do
      JArr.Add(Lista[i].Trim);

    JObj.AddPair('file_ids', JArr);
    St.WriteString(JObj.Format);
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + aApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      JRes := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      If Assigned(JRes) then
      Begin
        Try
          Result := TJson.JsonToObject<TAiVectorStoreFileBatch>(JRes);
        Finally
          JRes.Free;
        End;
      End;
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
    Lista.Free;
  End;
end;

Class function TAiVectorStoresFileBatch.Retrive(aApiKey, aVectorStoreId, aBatchId: String): TAiVectorStoreFileBatch;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Response: TMemoryStream;
  Res: IHTTPResponse;
  sUrl: String;
  JObj: TJSonObject;
begin

  Client := THTTPClient.Create;
  sUrl := 'https://api.openai.com/v1/vector_stores/' + aVectorStoreId + '/file_batches/' + aBatchId;

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + aApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      JObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      try
        Result := TJson.JsonToObject<TAiVectorStoreFileBatch>(JObj);
      finally
        JObj.Free;
      end;
    End
    else
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
  Finally
    Client.Free;
    Response.Free;
  End;
end;

Class function TAiVectorStoresFileBatch.Cancel(aApiKey, aVectorStoreId, aBatchId: String): TAiVectorStoreFileBatch;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Response: TMemoryStream;
  Res: IHTTPResponse;
  sUrl: String;
  JObj: TJSonObject;
begin

  Client := THTTPClient.Create;
  sUrl := 'https://api.openai.com/v1/vector_stores/' + aVectorStoreId + '/file_batches/' + aBatchId + '/cancel';

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + aApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      JObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      try
        Result := TJson.JsonToObject<TAiVectorStoreFileBatch>(JObj);
      finally
        JObj.Free;
      end;
    End
    else
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
  Finally
    Client.Free;
    Response.Free;
  End;
end;

Class function TAiVectorStoresFileBatch.List(aApiKey, aVectorStoreId, aBatchId: String): TAiVectorStoreFileBatchArray;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj: TJSonObject;
  JArr: TJSonArray;
  JVal: TJSonValue;

  Res: IHTTPResponse;
  sUrl: String;
  StoreVector: TAiVectorStoreFileBatch;
  FileBatchList: TAiVectorStoreFileBatchArray;
begin
  FileBatchList := TAiVectorStoreFileBatchArray.Create;

  FileBatchList.Clear;

  Client := THTTPClient.Create;
  sUrl := 'https://api.openai.com/v1/vector_stores/' + aVectorStoreId + '/file_batches/' + aBatchId + '/files';
  { TODO : Falta implementar los parámetros adicionales }

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + aApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Nil, Headers);

    if Res.StatusCode = 200 then
    Begin
      JObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      try
        JArr := JObj.GetValue<TJSonArray>('data');
        For JVal in JArr do
        Begin
          StoreVector := TJson.JsonToObject<TAiVectorStoreFileBatch>(TJSonObject(JVal));
          FileBatchList.Add(StoreVector.id, StoreVector);
        End;
        Result := FileBatchList;
      finally
        JObj.Free;
      end;
    End
    else
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);

  Finally
    Client.Free;
  End;
end;

{ TAiVectorStoreFileBatchArray }

function TAiVectorStoreFileBatchArray.GetVectorStoreFileBatchById(AiVectorStoreId: String; out AiVectorStoreFileBatch: TAiVectorStoreFileBatch): Boolean;
begin
  AiVectorStoreFileBatch := Nil;
  Result := Self.TryGetValue(AiVectorStoreId, AiVectorStoreFileBatch);
end;

end.
