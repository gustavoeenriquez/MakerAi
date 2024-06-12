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

unit uOpenAi;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Threading,
  System.Variants, System.Net.Mime, System.IOUtils, System.Generics.Collections,
  System.NetEncoding,
  System.JSON, System.StrUtils, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent,
  REST.JSON, REST.Types, REST.Client;

Const
  GlOpenAIUrl = 'https://api.openai.com/v1/';

type
  TAiImageSize = (TiaSize256, TiaSize512, TiaSize1024, TiaSize1024_1792, TiaSize1792_1024);
  TAiImageResponseFormat = (tiaRUrl, tiaRB64);
  TAiImageAStyleFormat = (tiaStyleVivid, tiaStyleNatural);
  TAiFileCategory = (Tfc_Image, Tfc_Audio, Tfc_Video, Tfc_Document, Tfc_Text, Tfc_CalcSheet, Tfc_Presentation, Tfc_CompressFile, Tfc_Web, Tfc_Aplication, Tfc_DiskImage, Tfc_GraphicDesign, Tfc_Unknow);

  TAiMetadata = Class;
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

  TAiToolsFunction = class(TObject)
    id: string;
    Tipo: string;
    name: string;
    Description: String;
    Arguments: string;
    Params: TStringList;
    &Function: string;
    Response: String;
    Body: TJSonObject;
    Metadata: TAiMetadata;

    Constructor Create;
    Destructor Destroy; Override;
    Procedure ParseFunction(JObj: TJSonObject);
    Procedure Assign(aSource: TAiToolsFunction);
  end;

  TAiToolsFunctions = Class(TDictionary<String, TAiToolsFunction>)
  Private
  Protected
  Public
    Function ToOutputJSon: TJSonArray;
    Function ToFunctionsJSon: TJSonArray;
    Procedure AddFunction(aBody: String); Overload;
    Procedure AddFunction(aBody: TJSonObject); Overload;
  End;

  TOnCallToolFunction = Procedure(Sender: TObject; AiToolCall: TAiToolsFunction) of object;
  TOnAssistantResponse = Procedure(Sender: TObject; Response: TAiMessage; Content: String) of object;
  TOnStatusNotifyEvent = Procedure(Sender: TObject; aStatus: String) of object;
  TOnBeforeResponse = Procedure(Sender: TObject; Funcion, Response: String) of object;

  TAiMetadata = Class(TDictionary<String, String>)
  Private
    function GetAsText: String;
    procedure SetAsText(const Value: String);
    function GetJSonText: String;
    procedure SetJsonText(const Value: String);
  Protected
  Public
    Function ToJSon: TJSonObject;
    Property AsText: String Read GetAsText Write SetAsText;
    Property JsonText: String Read GetJSonText Write SetJsonText;
  End;

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

  { TODO : Falta implementar el Streaming mode }
  TAiAudio = Class(TComponent)
  Private
    FApiKey: String;
    FUrl: String;
    FModel: String;
    FVoice: String;
    FFormat: String;
    FLanguaje: String;
    FTemperature: Single;
    FSpeed: Single;
    FResponseFormat: String;
    FQuality: String;
    procedure SetApiKey(const Value: String);
    procedure SetUrl(const Value: String);
    procedure SetModel(const Value: String);
    procedure SetVoice(const Value: String);
    procedure SetFormat(const Value: String);
    procedure SetLanguaje(const Value: String);
    procedure SetSpeed(const Value: Single);
    procedure SetTemperature(const Value: Single);
    procedure SetResponseFormat(const Value: String);
    procedure SetQuality(const Value: String);
  Protected
    Function IsValidExtension(FileExtension: String): Boolean;
    Function ConvertFileFormat(Origen: TMemoryStream; filename: String; out Destino: TMemoryStream; Out DestinoFileName: String): Boolean;
  Public
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;
    Function Speech(aText: String; aVoice: String = ''): TMemoryStream;
    Function Transcription(aStream: TMemoryStream; aFileName, aPrompt: String): String;
    Function Translation(aStream: TMemoryStream; aFileName, aPrompt: String): String;
  Published
    Property ApiKey: String read FApiKey write SetApiKey;
    Property Url: String read FUrl write SetUrl;
    Property Model: String read FModel write SetModel;
    Property Voice: String read FVoice write SetVoice;
    Property Format: String read FFormat write SetFormat;
    Property Languaje: String read FLanguaje write SetLanguaje;
    Property Speed: Single read FSpeed write SetSpeed;
    Property Temperature: Single read FTemperature write SetTemperature;
    Property ResponseFormat: String read FResponseFormat write SetResponseFormat;
    Property Quality: String read FQuality write SetQuality;
  End;

  TAiEmbeddingData = TArray<Double>;

  TAiEmbeddings = Class(TComponent)
  Private

    procedure SetApiKey(const Value: String);
    procedure SetModel(const Value: String);
    procedure SetData(const Value: TAiEmbeddingData);
    procedure SetUrl(const Value: String);
    procedure SetDimensions(const Value: Integer);
  Protected
    FApiKey: String;
    FModel: String;
    Ftotal_tokens: Integer;
    Fprompt_tokens: Integer;
    FData: TAiEmbeddingData;
    FUrl: String;
    FDimensions: Integer;
  Public
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;
    Function CreateEmbedding(aInput, aUser: String; aDimensions: Integer = -1; aModel: String = ''; aEncodingFormat: String = 'float'): TAiEmbeddingData; Virtual;
    Procedure ParseEmbedding(JObj: TJSonObject); Virtual;
    Function ToJsonArray: TJSonArray; Overload;
    class Function ToJsonArray(Val: TAiEmbeddingData): TJSonArray; Overload;

    class function Magnitude(const V: TAiEmbeddingData): Double;
    class function DotProduct(const A, B: TAiEmbeddingData): Double;
    class function CosineSimilarity(const A, B: TAiEmbeddingData): Double;

    Property Data: TAiEmbeddingData read FData write SetData;

  Published
    Property ApiKey: String read FApiKey write SetApiKey;
    Property Model: String read FModel write SetModel;
    Property prompt_tokens: Integer read Fprompt_tokens;
    Property total_tokens: Integer read Ftotal_tokens;
    Property Url: String read FUrl write SetUrl;
    Property Dimensions: Integer read FDimensions write SetDimensions;
  End;

  TAiImagesFile = Class(TObject)
  Private
    FImage: TMemoryStream;
    FBase64: String;
    Frevised_prompt: String;
    FUrlFile: String;
    function GetImage: TMemoryStream;
  Protected
    function Base64ToStream(Base64: String): TMemoryStream;
    Function LoadImage(UrlFile: String): TMemoryStream;
    Procedure ParseImage(JObj: TJSonObject);
  Public
    Constructor Create;
    Destructor Destroy; Override;

  Published
    Property Revised_Prompt: String read Frevised_prompt;
    Property Base64: String read FBase64;
    Property UrlFile: String read FUrlFile;
    Property Image: TMemoryStream Read GetImage;
  End;

  TAiImagesFiles = Array of TAiImagesFile;

  TAiImages = Class(TComponent)
  Private
    FApiKey: String;
    Frevised_prompt: String;
    FImages: TAiImagesFiles;
    FPrompt: String;
    FUrl: String;
    FResponseFormat: TAiImageResponseFormat;
    FHdQuality: Boolean;
    FStyleFormat: TAiImageAStyleFormat;
    FUseDalle3: Boolean;
    FUser: String;
    procedure SetApiKey(const Value: String);
    procedure Setrevised_prompt(const Value: String);
    procedure SetImages(const Value: TAiImagesFiles);
    procedure SetUrl(const Value: String);
    procedure SetResponseFormat(const Value: TAiImageResponseFormat);
    procedure SetHdQuality(const Value: Boolean);
    procedure SetStyleFormat(const Value: TAiImageAStyleFormat);
    procedure SetUseDalle3(const Value: Boolean);
    procedure SetUser(const Value: String);
  Protected
    Procedure ParseGenerate(JObj: TJSonObject);
    Procedure ParseVariations(JObj: TJSonObject);
  Public
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;
    Function Generate(aPrompt: String; aSize: TAiImageSize; N: Integer): TAiImagesFile;
    Function Edit(aImage, aMask: TMemoryStream; aPrompt: String; aSize: TAiImageSize; N: Integer): TAiImagesFile;
    Function Variation(aImage: TMemoryStream; aSize: TAiImageSize; N: Integer): TAiImagesFile;

  Published
    Property Url: String read FUrl write SetUrl;
    Property ApiKey: String read FApiKey write SetApiKey;
    Property Revised_Prompt: String read Frevised_prompt write Setrevised_prompt;
    Property Images: TAiImagesFiles read FImages write SetImages;
    Property Prompt: String Read FPrompt;
    Property ResponseFormat: TAiImageResponseFormat read FResponseFormat write SetResponseFormat;
    Property HdQuality: Boolean read FHdQuality write SetHdQuality;
    Property StyleFormat: TAiImageAStyleFormat read FStyleFormat write SetStyleFormat;
    Property UseDalle3: Boolean read FUseDalle3 write SetUseDalle3;
    Property User: String read FUser write SetUser;

  End;

  TAiVision = Class(TComponent)
  Private
    FApiKey: String;
    Fcompletion_tokens: Integer;
    FModel: String;
    Ftotal_tokens: Integer;
    FFinish_reason: String;
    FRole: String;
    Fid: String;
    FContent: String;
    FPrompt_Tokenes: Integer;
    FUrl: String;
    procedure SetApiKey(const Value: String);
    procedure SetUrl(const Value: String);
    procedure SetModel(const Value: String);
  Protected
    Function ParseVision(Response: TJSonObject): String;
  Public
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;
    Function GenerateByUrl(aPrompt, aUrl: String; aMax_tokens: Integer = 4000; aDetail: Boolean = False): String;
    function GenerateByBase64(aPrompt, aBase64: String; aMax_tokens: Integer = 4000; aDeetail: Boolean = False): String;
    function GenerateByStream(aPrompt: String; aStream: TMemoryStream; aMax_tokens: Integer = 4000; aDetail: Boolean = False): String;

  Published
    Property ApiKey: String read FApiKey write SetApiKey;
    Property id: String read Fid;
    Property Model: String read FModel write SetModel;
    Property Finish_reason: String read FFinish_reason;
    Property role: String read FRole;
    Property Content: String read FContent;
    Property Prompt_Tokenes: Integer read FPrompt_Tokenes;
    Property completion_tokens: Integer read Fcompletion_tokens;
    Property total_tokens: Integer read Ftotal_tokens;
    Property Url: String read FUrl write SetUrl;
  End;

  TAiMediaFile = Class
  Private
    Ffilename: String;
    FUrlMedia: String;
    FFileType: String;
    FContent: TMemoryStream;
    FFullFileName: String;
    FTranscription: String;
    FProcesado: Boolean;
    function GetBase64: String;
    function GetContent: TMemoryStream;
    procedure SetBase64(const Value: String);
    procedure Setfilename(const Value: String);
    procedure SetUrlMedia(const Value: String);
    function GetBytes: Integer;
    procedure SetFullFileName(const Value: String);
    function GetMimeType: String;
    function GetFileCategory: TAiFileCategory;
    procedure SetTranscription(const Value: String);
    procedure SetProcesado(const Value: Boolean);
  Protected
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure LoadFromfile(aFileName: String);
    Procedure LoadFromUrl(aUrl: String);
    Procedure LoadFromBase64(aFileName, aBase64: String);
    Procedure LoadFromStream(aFileName: String; Stream: TMemoryStream);
    Procedure SaveToFile(aFileName: String);
    Procedure Clear;
    Property filename: String read Ffilename write Setfilename;
    Property bytes: Integer read GetBytes;
    Property Content: TMemoryStream read GetContent;
    Property FileCategory: TAiFileCategory read GetFileCategory;
    Property UrlMedia: String read FUrlMedia write SetUrlMedia;
    Property Base64: String read GetBase64 write SetBase64;
    Property FullFileName: String read FFullFileName write SetFullFileName;
    Property MimeType: String read GetMimeType;
    // Transcription- Si el archivo adjunto se procesa por separado aquí se guarda lo que retorna el modelo correspondiente
    Property Transcription: String read FTranscription write SetTranscription;
    Property Procesado: Boolean read FProcesado write SetProcesado; // Si ya se utilizó, para que no guarde nuevamente la información de este archivo
  End;

  TAiMediaFilesArray = Array of TAiMediaFile;

  TAiMediaFiles = Class(TObjectList<TAiMediaFile>)
  Private
  Protected
  Public
    // Si el modelo nomaneja este tipo de media failes, se pueden preprocesar en el evento del chat
    // y el texto del proceso se adiciona al prompt, y aquí ya no se tendrían en cuenta
    Function GetMediaList(aFilter: TAiFileCategory; aProcesado: Boolean = False): TAiMediaFilesArray;
    // = (Tfc_Image, Tfc_Audio, Tfc_Video, Tfc_Document, Tfc_Text, Tfc_CalcSheet, Tfc_Presentation, Tfc_CompressFile, Tfc_Web, Tfc_Aplication, Tfc_DiskImage, Tfc_GraphicDesign, Tfc_Unknow); :
  End;

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
  RegisterComponents('MakerAI', [TAiEmbeddings, TAiVision, TAiAudio, TAiImages]);
end;

function GetParametrosURL(Parametros: TStringList): string;
var
  i: Integer;
begin
  Result := '';
  if Assigned(Parametros) and (Parametros.Count > 0) then
  begin
    Result := '?';
    for i := 0 to Parametros.Count - 1 do
    begin
      Result := Result + Parametros.Names[i] + '=' + Parametros.ValueFromIndex[i];
      if i < Parametros.Count - 1 then
        Result := Result + '&';
    end;
  end;
end;

function StreamToBase64(Stream: TMemoryStream): String;
begin
  Stream.Position := 0;
  Result := TNetEncoding.Base64.EncodeBytesToString(Stream.Memory, Stream.Size);
end;

function GetMimeTypeFromFileName(FileExtension: string): string;
begin
  FileExtension := LowerCase(Trim(StringReplace(FileExtension, '.', '', [rfReplaceAll])));

  if SameText(FileExtension, 'mp3') then
    Result := 'audio/mpeg'
  else if SameText(FileExtension, 'mp4') then
    Result := 'video/mp4'
  else if SameText(FileExtension, 'mpeg') then
    Result := 'video/mpeg'
  else if SameText(FileExtension, 'mpga') then
    Result := 'audio/mpeg'
  else if SameText(FileExtension, 'm4a') then
    Result := 'audio/mp4'
  else if SameText(FileExtension, 'ogg') then
    Result := 'audio/ogg'
  else if SameText(FileExtension, 'wav') then
    Result := 'audio/wav'
  else if SameText(FileExtension, 'webm') then
    Result := 'video/webm'
  else if SameText(FileExtension, 'txt') then
    Result := 'text/plain'
  else if SameText(FileExtension, 'html') then
    Result := 'text/html'
  else if SameText(FileExtension, 'htm') then
    Result := 'text/html'
  else if SameText(FileExtension, 'css') then
    Result := 'text/css'
  else if SameText(FileExtension, 'csv') then
    Result := 'text/csv'
  else if SameText(FileExtension, 'xml') then
    Result := 'application/xml'
  else if SameText(FileExtension, 'json') then
    Result := 'application/json'
  else if SameText(FileExtension, 'pdf') then
    Result := 'application/pdf'
  else if SameText(FileExtension, 'zip') then
    Result := 'application/zip'
  else if SameText(FileExtension, 'gzip') then
    Result := 'application/gzip'
  else if SameText(FileExtension, 'tar') then
    Result := 'application/x-tar'
  else if SameText(FileExtension, 'rar') then
    Result := 'application/vnd.rar'
  else if SameText(FileExtension, 'exe') then
    Result := 'application/vnd.microsoft.portable-executable'
  else if SameText(FileExtension, 'gif') then
    Result := 'image/gif'
  else if SameText(FileExtension, 'jpeg') then
    Result := 'image/jpeg'
  else if SameText(FileExtension, 'jpg') then
    Result := 'image/jpeg'
  else if SameText(FileExtension, 'png') then
    Result := 'image/png'
  else if SameText(FileExtension, 'bmp') then
    Result := 'image/bmp'
  else if SameText(FileExtension, 'svg') then
    Result := 'image/svg+xml'
  else if SameText(FileExtension, 'ico') then
    Result := 'image/vnd.microsoft.icon'
  else if SameText(FileExtension, 'tiff') then
    Result := 'image/tiff'
  else if SameText(FileExtension, 'tif') then
    Result := 'image/tiff'
  else if SameText(FileExtension, 'avi') then
    Result := 'video/x-msvideo'
  else if SameText(FileExtension, 'mov') then
    Result := 'video/quicktime'
  else if SameText(FileExtension, 'wmv') then
    Result := 'video/x-ms-wmv'
  else if SameText(FileExtension, 'flv') then
    Result := 'video/x-flv'
  else if SameText(FileExtension, '3gp') then
    Result := 'video/3gpp'
  else if SameText(FileExtension, 'mkv') then
    Result := 'video/x-matroska'
  else
    Result := 'application/octet-stream'; // Tipo de contenido predeterminado para otras extensiones
end;

{ 'jpg', 'jpeg', 'png', 'gif', 'bmp', 'tiff', 'svg', 'webp' Result := 'Imagen'
  'mp3', 'wav', 'flac', 'aac', 'ogg', 'wma', 'm4a'   Result := 'Audio'
  'avi', 'mp4', 'mkv', 'mov', 'wmv', 'flv', 'webm'   Result := 'Video'
  'doc', 'docx', 'pdf', 'odt', 'rtf', 'tex'     Result := 'Documento'
  'txt', 'md', 'rtf'     Result := 'Texto'
  'xls', 'xlsx', 'ods', 'csv'  Result := 'Hoja de Cálculo'
  'ppt', 'pptx', 'odp'   Result := 'Presentación'
  'zip', 'rar', 'tar', 'gz', 'bz2', '7z', 'xz'   Result := 'Archivo comprimido'
  'html', 'htm', 'xml', 'json', 'css', 'js'   Result := 'Web'
  'exe', 'msi', 'bat', 'sh', 'bin', 'cmd'   Result := 'Aplicación'
  'iso', 'img'   Result := 'Imagen de Disco'
  'psd', 'ai'    Result := 'Diseño Gráfico'
  Result := 'Desconocido';
}

function GetContentCategory(FileExtension: string): TAiFileCategory;
begin
  FileExtension := LowerCase(Trim(StringReplace(ExtractFileName(FileExtension), '.', '', [rfReplaceAll])));

  if (FileExtension = 'jpg') or (FileExtension = 'jpeg') or (FileExtension = 'png') or (FileExtension = 'gif') or (FileExtension = 'bmp') or (FileExtension = 'tiff') or (FileExtension = 'svg') or (FileExtension = 'webp') then
    Result := Tfc_Image
  else if (FileExtension = 'mp3') or (FileExtension = 'wav') or (FileExtension = 'flac') or (FileExtension = 'aac') or (FileExtension = 'ogg') or (FileExtension = 'wma') or (FileExtension = 'm4a') then
    Result := Tfc_Audio
  else if (FileExtension = 'avi') or (FileExtension = 'mp4') or (FileExtension = 'mkv') or (FileExtension = 'mov') or (FileExtension = 'wmv') or (FileExtension = 'flv') or (FileExtension = 'webm') then
    Result := Tfc_Video
  else if (FileExtension = 'doc') or (FileExtension = 'docx') or (FileExtension = 'pdf') or (FileExtension = 'odt') or (FileExtension = 'rtf') or (FileExtension = 'tex') then
    Result := Tfc_Document
  else if (FileExtension = 'txt') or (FileExtension = 'md') or (FileExtension = 'rtf') then
    Result := Tfc_Text
  else if (FileExtension = 'xls') or (FileExtension = 'xlsx') or (FileExtension = 'ods') or (FileExtension = 'csv') then
    Result := Tfc_CalcSheet
  else if (FileExtension = 'ppt') or (FileExtension = 'pptx') or (FileExtension = 'odp') then
    Result := Tfc_Presentation
  else if (FileExtension = 'zip') or (FileExtension = 'rar') or (FileExtension = 'tar') or (FileExtension = 'gz') or (FileExtension = 'bz2') or (FileExtension = '7z') or (FileExtension = 'xz') then
    Result := Tfc_CompressFile
  else if (FileExtension = 'html') or (FileExtension = 'htm') or (FileExtension = 'xml') or (FileExtension = 'json') or (FileExtension = 'css') or (FileExtension = 'js') then
    Result := Tfc_Web
  else if (FileExtension = 'exe') or (FileExtension = 'msi') or (FileExtension = 'bat') or (FileExtension = 'sh') or (FileExtension = 'bin') or (FileExtension = 'cmd') then
    Result := Tfc_Aplication
  else if (FileExtension = 'iso') or (FileExtension = 'img') then
    Result := Tfc_DiskImage
  else if (FileExtension = 'psd') or (FileExtension = 'ai') then
    Result := Tfc_GraphicDesign
  else
    Result := Tfc_Unknow;
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

{ TAiMetadata }

function TAiMetadata.GetAsText: String;
Var
  Lista: TStringList;
  Clave: String;
begin

  Lista := TStringList.Create;
  Try
    For Clave in Self.Keys do
      Lista.Values[Clave] := Self.Items[Clave];

    Result := Lista.Text;

  Finally
    Lista.Free;
  End;
end;

function TAiMetadata.GetJSonText: String;
Var
  JObj: TJSonObject;
  Clave: String;
begin
  JObj := TJSonObject.Create;

  Try
    For Clave in Self.Keys do
      JObj.AddPair(Clave, Self.Items[Clave]);

    Result := JObj.Format;
  Finally
    JObj.Free;
  End;
end;

procedure TAiMetadata.SetAsText(const Value: String);
Var
  Lista: TStringList;
  Clave, Valor: String;
  i: Integer;
begin

  Lista := TStringList.Create;

  Try
    Lista.Text := Value;
    Self.Clear;
    For i := 0 to Lista.Count - 1 do
    Begin
      Clave := Lista.Names[i];
      Valor := Lista.Values[Clave];
      Self.Add(Clave, Valor);
    End;
  Finally
    Lista.Free;
  End;

end;

procedure TAiMetadata.SetJsonText(const Value: String);
Var
  JObj: TJSonObject;
  Pair: TJSONPair;
begin
  JObj := TJSonObject(TJSonObject.ParseJSONValue(Value));

  Self.Clear;
  For Pair in JObj do
    Self.Add(Pair.JsonString.Value, Pair.JsonValue.Value)
end;

function TAiMetadata.ToJSon: TJSonObject;
Var
  Clave: String;
begin
  Result := TJSonObject.Create;

  For Clave in Self.Keys do
    Result.AddPair(Clave, Self.Items[Clave]);
end;

{ TAitools_outputs }

procedure TAiToolsFunctions.AddFunction(aBody: TJSonObject);
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

procedure TAiToolsFunctions.AddFunction(aBody: String);
Var
  Func: TJSonObject;
begin
  Func := TJSonObject(TJSonObject.ParseJSONValue(aBody));
  AddFunction(Func);

end;

function TAiToolsFunctions.ToFunctionsJSon: TJSonArray;
Var
  Clave: String;
  TObj: TJSonObject;
  Func: TAiToolsFunction;
begin
  Result := TJSonArray.Create;

  For Clave in Self.Keys do
  Begin
    Func := Self.Items[Clave];
    // Result.Add(TJSonObject(TJSonObject.ParseJSONValue(Self.Items[Clave].&Function)));
    TObj := TJSonObject(Func.Body.Clone);
    // TObj.AddPair('type', 'function');
    // TObj.AddPair('function', TJsonObject(Func.Body.Clone));
    Result.Add(TObj);
  End;
end;

function TAiToolsFunctions.ToOutputJSon: TJSonArray;
Var
  Clave: String;
  TObj: TJSonObject;
begin
  Result := TJSonArray.Create;

  For Clave in Self.Keys do // La clave es el nombre de la función
  Begin
    TObj := TJSonObject.Create;
    TObj.AddPair('tool_call_id', Self.Items[Clave].id);
    TObj.AddPair('output', Self.Items[Clave].Response);
    Result.Add(TObj);
  End;
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
      // FJSonObject := TJsonObject.Create.AddPair('error', Res.ContentAsString);
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
  JObj: TJSonObject;
  sId, sName: String;
begin
  Result := '';

  JObj := GetList(aApiKey, aUrl, 100);
  JArr := JObj.GetValue<TJSonArray>('data');

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

  FAssistantId := FJSonObject.GetValue<String>('id');
  FName := FJSonObject.GetValue<String>('name');
  FModel := FJSonObject.GetValue<String>('model');
  FInstructions := FJSonObject.GetValue<String>('instructions');
  Tools := FJSonObject.GetValue<TJSonArray>('tools');

  If FJSonObject.TryGetValue<Double>('temperature', FNumTemp) then
    Self.Temperature := FNumTemp;

  If FJSonObject.TryGetValue<Double>('top_p', FNumTop) then
    Self.top_p := FNumTop;

  Tools := FJSonObject.GetValue<TJSonArray>('tools');

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

  If FJSonObject.TryGetValue<TJSonObject>('tool_resources', JToolResources) then
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

  jMetadata := FJSonObject.GetValue<TJSonObject>('metadata');

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
  JFiles: TJSonArray;
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
      JFiles := TJSonArray.Create;

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
      AiMessage := ParseMessage(JRes);
      AiMessage.Url := FUrl;
      FMessages.Insert(0, AiMessage);
      Result := AiMessage;
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
  JFileSearch := TJSonObject.Create;
  JCodeInterpreter := TJSonObject.Create;
  ListFilesIds := TStringList.Create;

  Try
    If Assigned(aMensajes) and (aMensajes.Count > 0) then
      JObj.AddPair('messages', aMensajes.ToJSon);

    If Assigned(aMetadata) and (aMetadata.Count > 0) then
      JObj.AddPair('metadata', aMetadata.ToJSon);

    JCodeFiles := TJSonArray.Create;
    JVectors := TJSonArray.Create;

    // Adiciona los VectorsIds de los archivos de filesearch

    ListFilesIds.CommaText := aVectorStoreIds;
    If ListFilesIds.Count > 0 then
    Begin
      JToolResources.AddPair('file_search', JFileSearch);

      For i := 0 to ListFilesIds.Count - 1 do
        JVectors.Add(ListFilesIds[i]);

      JFileSearch.AddPair('vector_store_ids', JVectors);
    End;

    // Adiciona los FilesIds de los archivos de code_interpreter
    ListFilesIds.CommaText := aCodeFilesIds;
    If ListFilesIds.Count > 0 then
    Begin
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
      Result := ParseMessage(JRes);
      Result.Url := FUrl;
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

      Data := JRes.GetValue<TJSonArray>('data');
      For JVal in Data do
      Begin
        JObj := TJSonObject(JVal);
        AiMessage := ParseMessage(JObj);
        AiMessage.Url := FUrl;
        // AiMessages.Insert(0, AiMessage);
        AiMessages.Add(AiMessage);
      End;

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
  FMetadata.SetJsonText(sMeta);
end;

function TAiThRead.ParseMessage(JObj: TJSonObject): TAiMessage;
Var
  JObj1, jText, JObjAnn: TJSonObject;
  JContent: TJSonArray;
  JFiles, JAnnotations: TJSonArray;
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
      ParseRun(JRes);
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
      ParseRun(JRes);
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
    JSonObject.Free;
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
  sRequiredAction: String;
  ToolCall: TAiToolsFunction;
  Clave: String;

begin

  Self.FRunId := JObj.GetValue<String>('id');
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
    // JTool := JObj.GetValue<TJsonObject>('required_action');

    FToolsCalls.Clear;

    If ExtractToolCallsFromJson(JTool) = True then
    Begin

      NumTasks := FToolsCalls.Count;
      SetLength(TaskList, NumTasks);
      // Ajusta el tamaño del array para el número de tareas

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
      SubmitTool(Self.FToolsCalls);
    End;
  end;
end;

function TAiRun.Retrieve: TJSonObject;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  sUrl: String;
  JRes: TJSonObject;
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
      JRes := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      FJObjectRun := JRes;
      ParseRun(JRes);
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
      ParseRun(JRes);
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
      ParseRun(JRes);
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

procedure TAiToolsFunction.ParseFunction(JObj: TJSonObject);
Var
  JFunc: TJSonObject;
  FunName: String;
begin
  JFunc := JObj.GetValue<TJSonObject>('function');
  FunName := JFunc.GetValue<string>('name');

  Begin
    Name := JFunc.GetValue<String>('name');
    Self.Description := JFunc.GetValue<String>('description');
    &Function := JFunc.Format;
    Body := JObj; // La funcion original completa
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
      JArr := JObj.GetValue<TJSonArray>('data');

      For JVal in JArr do
      Begin
        AiFile := TJson.JsonToObject<TAiFile>(TJSonObject(JVal));
        FFileList.Add(AiFile.id, AiFile);
      End;
      Result := FFileList;
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
          Result := TJson.JsonToObject<TAiFile>(JObj);
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
  Result := Self.TryGetValue(aFileName, AiFile);
end;

{ TAiAudio }

procedure RunCommand(const Command: string);
begin

{$IFDEF LINUX}
  TLinuxUtils.RunCommandLine(Command);
{$ENDIF}
{$IFDEF MSWINDOWS}
  ShellExecute(0, nil, 'cmd.exe', PChar('/C ' + Command), nil, SW_HIDE);
{$ENDIF}
end;

function TAiAudio.ConvertFileFormat(Origen: TMemoryStream; filename: String; out Destino: TMemoryStream; out DestinoFileName: String): Boolean;
Var
  FOrigen, FDestino: String;
  CommandLine: String;
begin
  filename := LowerCase(filename);
  FDestino := ChangeFileExt(filename, '.mp3');

  FOrigen := System.IOUtils.TPath.Combine(System.IOUtils.TPath.GetTempPath, filename);
  FDestino := System.IOUtils.TPath.Combine(System.IOUtils.TPath.GetTempPath, FDestino);

  Origen.Position := 0;
  Origen.SaveToFile(FOrigen);

  CommandLine := 'ffmpeg -i ' + FOrigen + ' ' + FDestino;

  RunCommand(CommandLine);

  Destino := TMemoryStream.Create;
  Destino.LoadFromfile(FDestino);
  Destino.Position := 0;
  DestinoFileName := ExtractFileName(FDestino);

  TFile.Delete(FOrigen);
  TFile.Delete(FDestino);
end;

constructor TAiAudio.Create(aOwner: TComponent);
begin
  Inherited;
  FUrl := GlOpenAIUrl;
  FModel := 'whisper-1';
  FVoice := 'nova'; // alloy, echo, fable, onyx, nova, shimmer
  FFormat := 'mp3'; // "mp3", opus", "aac", "flac", and "pcm"
  FLanguaje := 'es';
  FSpeed := 1;
  FTemperature := 0;
  FResponseFormat := 'text'; // json, text, srt, verbose_json, or vtt
  FQuality := 'tts-1-hd'; // tts-1, tts-1-hd
end;

destructor TAiAudio.Destroy;
begin

  inherited;
end;

function TAiAudio.IsValidExtension(FileExtension: String): Boolean;
begin
  FileExtension := LowerCase(Trim(StringReplace(FileExtension, '.', '', [rfReplaceAll])));
  Result := (FileExtension = 'mp3') or (FileExtension = 'mp4') or (FileExtension = 'mpeg') or (FileExtension = 'mpga') or (FileExtension = 'm4a') or (FileExtension = 'ogg') or (FileExtension = 'wav') or (FileExtension = 'webm');
end;

procedure TAiAudio.SetApiKey(const Value: String);
begin
  FApiKey := Value;
end;

procedure TAiAudio.SetFormat(const Value: String);
begin
  FFormat := Value;
end;

procedure TAiAudio.SetLanguaje(const Value: String);
begin
  FLanguaje := Value;
end;

procedure TAiAudio.SetModel(const Value: String);
begin
  FModel := Value;
end;

procedure TAiAudio.SetQuality(const Value: String);
begin
  FQuality := Value;
end;

procedure TAiAudio.SetResponseFormat(const Value: String);
begin
  FResponseFormat := Value;
end;

procedure TAiAudio.SetSpeed(const Value: Single);
begin
  FSpeed := Trunc(Value * 10) / 10;
end;

procedure TAiAudio.SetTemperature(const Value: Single);
begin
  FTemperature := Trunc(Value * 10) / 10;
end;

procedure TAiAudio.SetUrl(const Value: String);
begin
  If Value <> '' then
    FUrl := Value
  Else
    FUrl := GlOpenAIUrl;
end;

procedure TAiAudio.SetVoice(const Value: String);
begin
  FVoice := Value;
end;

function TAiAudio.Speech(aText: String; aVoice: String = ''): TMemoryStream;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj: TJSonObject;
  Res: IHTTPResponse;
  Response: TMemoryStream;
  St: TStringStream;
  sUrl: String;
begin
  If aVoice = '' then
    aVoice := FVoice;

  /// Output Format Outputs
  /// The default response format is "mp3", but other formats like "opus", "aac", "flac", and "pcm" are available.
  /// Opus: For internet streaming and communication, low latency.
  /// AAC: For digital audio compression, preferred by YouTube, Android, iOS.
  /// FLAC: For lossless audio compression, favored by audio enthusiasts for archiving.
  /// WAV: Uncompressed WAV audio, suitable for low-latency applications to avoid decoding overhead.
  /// PCM: Similar to WAV but containing the raw samples in 24kHz (16-bit signed, low-endian), without the header.

  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TMemoryStream.Create;
  sUrl := FUrl + 'audio/speech';
  JObj := TJSonObject.Create;

  FSpeed := Trunc(FSpeed * 10) / 10;

  Try
    JObj.AddPair('model', FModel);

    JObj.AddPair('input', aText);
    JObj.AddPair('voice', aVoice); // alloy, echo, fable, onyx, nova, and shimmer
    JObj.AddPair('response_format', FFormat);
    JObj.AddPair('speed', FSpeed);

    St.WriteString(UTF8Encode(JObj.Format));
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      Response.Position := 0;
      Result := Response;
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
    St.Free;
    // Response.Free;  //No se libera, se pasa al usuario
    JObj.Free;
  End;
end;

function TAiAudio.Transcription(aStream: TMemoryStream; aFileName, aPrompt: String): String;
Var

  Body: TMultipartFormData;
  Client: THTTPClient;
  Headers: TNetHeaders;
  sUrl, FileNameDestino: String;
  Destino: TMemoryStream;
  Res: IHTTPResponse;
begin

  // Los formatos disponibles son: json, text, srt, verbose_json, or vtt

  If IsValidExtension(ExtractFileExt(aFileName)) = False then
  Begin
    ConvertFileFormat(aStream, aFileName, Destino, FileNameDestino);
    aStream.Clear;
    aStream.LoadFromStream(Destino);
    aStream.Position := 0;
    aFileName := FileNameDestino;
    Destino.Free;
  End;

  sUrl := FUrl + 'audio/transcriptions';

  Client := THTTPClient.Create;
  Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
  Client.ContentType := 'application/json';
  Body := TMultipartFormData.Create;

  Try
    aStream.Position := 0;

    Body.AddStream('file', aStream, aFileName, GetMimeTypeFromFileName(ExtractFileExt(aFileName)));
    Body.AddField('model', FModel);
    Body.AddField('prompt', aPrompt);
    Body.AddField('response_format', FResponseFormat);
    // json, text, srt, verbose_json, or vtt
    Body.AddField('temperature', FormatFloat('0.0', FTemperature));
    Body.AddField('language', FLanguaje);

    Client.Accept := 'application/text';
    Client.ContentType := 'multipart/form-data';

    Res := Client.Post(sUrl, Body, Nil, Headers);

    if Res.StatusCode = 200 then
    Begin
      Result := Res.ContentAsString
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Body.Free;
    Client.Free;
  End;
end;

function TAiAudio.Translation(aStream: TMemoryStream; aFileName, aPrompt: String): String;
Var
  Body: TMultipartFormData;
  Client: THTTPClient;
  Headers: TNetHeaders;
  sUrl, FileNameDestino: String;
  Destino: TMemoryStream;
  Res: IHTTPResponse;
begin
  If IsValidExtension(ExtractFileExt(aFileName)) = False then
  Begin
    ConvertFileFormat(aStream, aFileName, Destino, FileNameDestino);
    aStream.Clear;
    aStream.LoadFromStream(Destino);
    aStream.Position := 0;
    aFileName := FileNameDestino;
    Destino.Free;
  End;

  sUrl := FUrl + 'audio/translations';

  Client := THTTPClient.Create;
  Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
  Client.ContentType := 'application/json';
  Body := TMultipartFormData.Create;

  Try
    aStream.Position := 0;

    Body.AddStream('file', aStream, aFileName, GetMimeTypeFromFileName(ExtractFileExt(aFileName)));
    Body.AddField('model', FModel);
    Body.AddField('prompt', aPrompt);
    Body.AddField('response_format', FResponseFormat);
    // json, text, srt, verbose_json, or vtt
    Body.AddField('temperature', FormatFloat('0.0', Temperature));

    Client.Accept := 'application/text';
    Client.ContentType := 'multipart/form-data';

    Res := Client.Post(sUrl, Body, Nil, Headers);

    if Res.StatusCode = 200 then
    Begin
      Result := Res.ContentAsString
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;
  Finally
    Body.Free;
    Client.Free;
  End;
end;

{ TEmbeddings }

class function TAiEmbeddings.CosineSimilarity(const A, B: TAiEmbeddingData): Double;
var
  MagA, MagB: Double;
begin
  MagA := Magnitude(A);
  MagB := Magnitude(B);
  if (MagA = 0) or (MagB = 0) then
    Result := 0 // Para evitar división por cero
  else
    Result := DotProduct(A, B) / (MagA * MagB);
end;

constructor TAiEmbeddings.Create(aOwner: TComponent);
begin
  Inherited;
  Url := GlOpenAIUrl;
  FDimensions := 1536;
  FModel := 'text-embedding-3-small';
end;

destructor TAiEmbeddings.Destroy;
begin

  inherited;
end;

class function TAiEmbeddings.DotProduct(const A, B: TAiEmbeddingData): Double;
var
  i: Integer;
begin
  Result := 0.0;
  for i := Low(A) to High(A) do
    Result := Result + A[i] * B[i];
end;

class function TAiEmbeddings.Magnitude(const V: TAiEmbeddingData): Double;
var
  Sum: Double;
  i: Integer;
begin
  Sum := 0.0;
  for i := Low(V) to High(V) do
    Sum := Sum + V[i] * V[i];
  Result := Sqrt(Sum);
end;

procedure TAiEmbeddings.ParseEmbedding(JObj: TJSonObject);
Var
  JArr, jData: TJSonArray;
  Emb: TAiEmbeddingData;
  JVal: TJSonValue;
  i, J: Integer;
  Usage: TJSonObject;

begin
  JObj.TryGetValue<String>('model', FModel);

  If JObj.TryGetValue<TJSonObject>('usage', Usage) then
  Begin
    Usage.TryGetValue<Integer>('prompt_tokens', Fprompt_tokens);
    Usage.TryGetValue<Integer>('total_tokens', Ftotal_tokens);
  End;

  jData := JObj.GetValue<TJSonArray>('data');

  SetLength(FData, jData.Count);

  i := 0;
  For JVal in jData do
  Begin
    // El embedding de OpenAi Retorna un array, pero solo se toma el primero de la fila
    JArr := TJSonObject(JVal).GetValue<TJSonArray>('embedding');
    J := JArr.Count;
    SetLength(Emb, J);
    // FillChar(Emb, Length(Emb) * SizeOf(Double), 0);

    For J := 0 to JArr.Count - 1 do
      Emb[J] := JArr.Items[J].GetValue<Double>;

    FData := Emb;
    Inc(i);
    Break; // Si el embedding de OpenAI retorna varios solo tomamos el primero, usualmente solo hay uno
  End;
end;

procedure TAiEmbeddings.SetApiKey(const Value: String);
begin
  FApiKey := Value;
end;

procedure TAiEmbeddings.SetData(const Value: TAiEmbeddingData);
begin
  FData := Value;
end;

procedure TAiEmbeddings.SetDimensions(const Value: Integer);
begin
  FDimensions := Value;
end;

procedure TAiEmbeddings.SetModel(const Value: String);
begin
  FModel := Value;
end;

procedure TAiEmbeddings.SetUrl(const Value: String);
begin
  If Value <> '' then
    FUrl := Value
  Else
    FUrl := GlOpenAIUrl;
end;

class function TAiEmbeddings.ToJsonArray(Val: TAiEmbeddingData): TJSonArray;
Var
  i: Integer;
begin
  Result := TJSonArray.Create;

  For i := 0 to Length(Val) - 1 do
    Result.Add(Val[i]);
end;

function TAiEmbeddings.ToJsonArray: TJSonArray;
Var
  J: Integer;
  JEmb: TJSonArray;
begin
  Try
    JEmb := TJSonArray.Create;
    For J := 0 to Length(FData) - 1 do
      JEmb.Add(FData[J]);

    Result := JEmb;
  Finally

  End;
end;

function TAiEmbeddings.CreateEmbedding(aInput, aUser: String; aDimensions: Integer; aModel, aEncodingFormat: String): TAiEmbeddingData;
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
  sUrl := FUrl + 'embeddings';
  JObj := TJSonObject.Create;

  If aModel = '' then
    aModel := FModel;

  if aDimensions <= 0 then
    aDimensions := FDimensions;

  Try
    JObj.AddPair('input', aInput); // Este se adiciona por compatibilidad con ollama
    JObj.AddPair('prompt', aInput);
    JObj.AddPair('model', aModel);
    JObj.AddPair('user', aUser);
    JObj.AddPair('dimensions', aDimensions);
    JObj.AddPair('encoding_format', aEncodingFormat);

    St.WriteString(UTF8Encode(JObj.Format));
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);
    Response.Position := 0;

    // Response.SaveToFile('c:\temp\response.txt');

    if Res.StatusCode = 200 then
    Begin
      JObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      ParseEmbedding(JObj);
      Result := Self.FData;

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

{ TAiImages }

constructor TAiImages.Create(aOwner: TComponent);
begin
  Inherited;
  SetLength(FImages, 0);
  FUrl := GlOpenAIUrl;
  FHdQuality := False;
  FResponseFormat := tiaRUrl;
  FStyleFormat := tiaStyleVivid;
  FUseDalle3 := True;
  FUser := 'user';
end;

destructor TAiImages.Destroy;
Var
  i: Integer;
begin
  For i := 0 to Length(FImages) - 1 do
    FImages[i].Free;

  SetLength(FImages, 0);
  inherited;
end;

function TAiImages.Edit(aImage, aMask: TMemoryStream; aPrompt: String; aSize: TAiImageSize; N: Integer): TAiImagesFile;
Var
  Body: TMultipartFormData;
  Client: THTTPClient;
  sUrl: String;
  Res: IHTTPResponse;
  JObj: TJSonObject;
begin
  sUrl := FUrl + 'images/edits';

  aImage.Position := 0;

  If Assigned(aMask) then
    aMask.Position := 0;

  Client := THTTPClient.Create;
  Try
    Body := TMultipartFormData.Create;
    Try
      Try
        Body.AddStream('image', aImage, ExtractFileName('origen.png'));

        If Assigned(aMask) then
          Body.AddStream('mask', aImage, True, ExtractFileName('mask.png'));

        Body.AddField('prompt', aPrompt);
        Body.AddField('user', FUser);
        Body.AddField('model', 'dall-e-2'); // Solo acepta esta versión
        Body.AddField('n', N.ToString);

        Case aSize of
          TiaSize256:
            Body.AddField('size', '256x256');
          TiaSize512:
            Body.AddField('size', '512x512');
          TiaSize1024:
            Body.AddField('size', '1024x1024');
          TiaSize1024_1792:
            Body.AddField('size', '1024x1024'); // '1024x1792');
          TiaSize1792_1024:
            Body.AddField('size', '1024x1024'); // '1792x1024');
        End;

        If ResponseFormat = TAiImageResponseFormat.tiaRUrl then
          Body.AddField('response_format', 'url')
        else
          Body.AddField('response_format', 'b64_json');

        // Añadir el header de autorización
        Client.CustomHeaders['Authorization'] := 'Bearer ' + ApiKey;

        // La llamada Post debería cuidar del Content-Type automáticamente
        Res := Client.Post(sUrl, Body, nil);

        if Res.StatusCode = 200 then
        Begin
          JObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
          ParseGenerate(JObj);
          Result := FImages[0];
        End
        else
          Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
      Finally
      End;
    Finally
      Body.Free;
    End;
  Finally
    Client.Free;
  End;
end;

function TAiImages.Generate(aPrompt: String; aSize: TAiImageSize; N: Integer): TAiImagesFile;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj: TJSonObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
begin
  // prompt           string Required
  // model            string Optional dall-e-2 dall-e-3
  // n                integer or null Optional Defaults to 1 The number of images to generate. Must be between 1 and 10. For dall-e-3, only n=1 is supported.
  // quality          string Optional Defaults to standard The quality of the image that will be generated. hd creates images with finer details and greater consistency across the image. This param is only supported for dall-e-3.
  // response_format  string or null Optional Defaults to url The format in which the generated images are returned. Must be one of url or b64_json.
  // size             string or null Optional Defaults to 1024x1024 The size of the generated images. Must be one of 256x256, 512x512, or 1024x1024 for dall-e-2. Must be one of 1024x1024, 1792x1024, or 1024x1792 for dall-e-3 models.
  // style            string or null Optional Defaults to vivid The style of the generated images. Must be one of vivid or natural. Vivid causes the model to lean towards generating hyper-real and dramatic images. Natural causes the model to produce more natural, less hyper-real looking images. This param is only supported for dall-e-3.
  // user             string Optional A unique identifier representing your end-user, which can help OpenAI to monitor and detect abuse. Learn more.

  FPrompt := aPrompt;

  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := FUrl + 'images/generations';

  JObj := TJSonObject.Create;

  Try
    JObj.AddPair('prompt', aPrompt);

    If FUseDalle3 then
    Begin
      JObj.AddPair('model', 'dall-e-3');
      N := 1;

      Case aSize of
        TiaSize256:
          aSize := TiaSize1024; // pequeña
        TiaSize512:
          aSize := TiaSize1024_1792; // Mediana
        TiaSize1024:
          aSize := TiaSize1792_1024; // Grande
      End;

    End
    else
    Begin
      JObj.AddPair('model', 'dall-e-2');
      Case aSize of
        TiaSize1024:
          aSize := TiaSize256;
        TiaSize1024_1792:
          aSize := TiaSize512;
        TiaSize1792_1024:
          aSize := TiaSize1024;
      End;
    End;

    JObj.AddPair('n', N);

    If HdQuality then
      JObj.AddPair('quality', 'hd');

    Case aSize of
      TiaSize256:
        JObj.AddPair('size', '256x256');
      TiaSize512:
        JObj.AddPair('size', '512x512');
      TiaSize1024:
        JObj.AddPair('size', '1024x1024');
      TiaSize1024_1792:
        JObj.AddPair('size', '1024x1792');
      TiaSize1792_1024:
        JObj.AddPair('size', '1792x1024');
    End;

    If ResponseFormat = TAiImageResponseFormat.tiaRUrl then
      JObj.AddPair('response_format', 'url')
    else
      JObj.AddPair('response_format', 'b64_json');

    If FStyleFormat = TAiImageAStyleFormat.tiaStyleVivid then
      JObj.AddPair('style', 'vivid')
    Else
      JObj.AddPair('style', 'natural');

    JObj.AddPair('user', FUser);

    St.WriteString(UTF8Encode(JObj.Format));
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      JObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      ParseGenerate(JObj);
      Result := FImages[0];
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

procedure TAiImages.ParseGenerate(JObj: TJSonObject);
Var
  Data: TJSonArray;
  JObj1: TJSonObject;
  i: Integer;
  Image: TAiImagesFile;
begin
  Data := JObj.GetValue<TJSonArray>('data');

  SetLength(FImages, Data.Count);

  For i := 0 to Data.Count - 1 do
  Begin
    JObj1 := TJSonObject(Data.Items[i]);
    Image := TAiImagesFile.Create;
    FImages[i] := Image;
    Image.ParseImage(JObj1);
  End;
end;

procedure TAiImages.ParseVariations(JObj: TJSonObject);
Var
  Data: TJSonArray;
  JObj1: TJSonObject;
  i: Integer;
  Image: TAiImagesFile;
begin
  Data := JObj.GetValue<TJSonArray>('data');

  SetLength(FImages, Data.Count);

  For i := 0 to Data.Count - 1 do
  Begin
    JObj1 := TJSonObject(Data.Items[i]);
    Image := TAiImagesFile.Create;
    FImages[i] := Image;
    Image.ParseImage(JObj1);
  End;
end;

procedure TAiImages.SetApiKey(const Value: String);
begin
  FApiKey := Value;
end;

procedure TAiImages.SetHdQuality(const Value: Boolean);
begin
  FHdQuality := Value;
end;

procedure TAiImages.SetImages(const Value: TAiImagesFiles);
begin
  FImages := Value;
end;

procedure TAiImages.SetResponseFormat(const Value: TAiImageResponseFormat);
begin
  FResponseFormat := Value;
end;

procedure TAiImages.Setrevised_prompt(const Value: String);
begin
  Frevised_prompt := Value;
end;

procedure TAiImages.SetStyleFormat(const Value: TAiImageAStyleFormat);
begin
  FStyleFormat := Value;
end;

procedure TAiImages.SetUrl(const Value: String);
begin
  If Value <> '' then
    FUrl := Value
  Else
    FUrl := GlOpenAIUrl;
end;

procedure TAiImages.SetUseDalle3(const Value: Boolean);
begin
  FUseDalle3 := Value;
end;

procedure TAiImages.SetUser(const Value: String);
begin
  FUser := Value;
end;

function TAiImages.Variation(aImage: TMemoryStream; aSize: TAiImageSize; N: Integer): TAiImagesFile;
Var
  Body: TMultipartFormData;
  Client: THTTPClient;
  sUrl: String;
  Res: IHTTPResponse;
  JObj: TJSonObject;
begin
  sUrl := FUrl + 'images/variations';

  aImage.Position := 0;

  Client := THTTPClient.Create;
  Try
    Body := TMultipartFormData.Create;
    Try
      Try
        Body.AddStream('image', aImage, ExtractFileName('origen.png'));

        Body.AddField('user', FUser);
        Body.AddField('model', 'dall-e-2'); // Solo acepta esta versión
        Body.AddField('n', N.ToString);

        Case aSize of
          TiaSize256:
            Body.AddField('size', '256x256');
          TiaSize512:
            Body.AddField('size', '512x512');
          TiaSize1024:
            Body.AddField('size', '1024x1024');
          TiaSize1024_1792:
            Body.AddField('size', '1024x1024'); // '1024x1792');
          TiaSize1792_1024:
            Body.AddField('size', '1024x1024'); // '1792x1024');
        End;

        If ResponseFormat = TAiImageResponseFormat.tiaRUrl then
          Body.AddField('response_format', 'url')
        else
          Body.AddField('response_format', 'b64_json');

        // Añadir el header de autorización
        Client.CustomHeaders['Authorization'] := 'Bearer ' + ApiKey;

        // La llamada Post debería cuidar del Content-Type automáticamente
        Res := Client.Post(sUrl, Body, nil);

        if Res.StatusCode = 200 then
        Begin

          JObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
          ParseGenerate(JObj);
          Result := FImages[0];

        End
        else
          Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
      Finally
      End;
    Finally
      Body.Free;
    End;
  Finally
    Client.Free;
  End;
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

{ TAiVision }

constructor TAiVision.Create(aOwner: TComponent);
begin
  Inherited;
  Url := GlOpenAIUrl;
  FModel := 'gpt-4o';
end;

destructor TAiVision.Destroy;
begin

  inherited;
end;

function TAiVision.GenerateByBase64(aPrompt, aBase64: String; aMax_tokens: Integer = 4000; aDeetail: Boolean = False): String;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj, JMsg, Obj1: TJSonObject;
  JMessages, JContent: TJSonArray;
  Res: IHTTPResponse;
  Response: TStringStream;
  ImagePayload: TStringStream;
  St: TStringStream;
  sUrl: String;
begin
  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := Url + 'chat/completions';
  JObj := TJSonObject.Create;

  JMessages := TJSonArray.Create;
  JContent := TJSonArray.Create;

  Try
    JObj.AddPair('model', FModel);
    JObj.AddPair('max_tokens', 4000);

    JMsg := TJSonObject.Create;
    JMsg.AddPair('role', FRole);

    Obj1 := TJSonObject.Create;
    Obj1.AddPair('type', 'text');
    Obj1.AddPair('text', aPrompt);
    JContent.Add(Obj1);

    ImagePayload := TStringStream.Create('{"type": "image_url", "image_url": {"url": "data:image/jpeg;base64,' + aBase64 + '"}}', TEncoding.UTF8);
    try
      JContent.Add(TJSonObject.ParseJSONValue(ImagePayload.DataString) as TJSonObject);
    finally
      ImagePayload.Free;
    end;

    JMsg.AddPair('content', JContent);
    JMessages.Add(JMsg);
    JObj.AddPair('messages', JMessages);

    St.WriteString(UTF8Encode(JObj.Format));
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      JObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      ParseVision(JObj);
      Result := Self.Content;
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

function TAiVision.GenerateByStream(aPrompt: String; aStream: TMemoryStream; aMax_tokens: Integer; aDetail: Boolean): String;
Var
  Base64: String;
begin
  Base64 := StreamToBase64(aStream);
  Result := GenerateByBase64(aPrompt, Base64, aMax_tokens, aDetail);
end;

function TAiVision.GenerateByUrl(aPrompt, aUrl: String; aMax_tokens: Integer; aDetail: Boolean): String;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj, JMsg, Obj1: TJSonObject;
  JMessages, JContent: TJSonArray;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
begin

  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := Url + 'chat/completions';
  JObj := TJSonObject.Create;

  JMessages := TJSonArray.Create;

  JContent := TJSonArray.Create;

  Try
    JObj.AddPair('model', FModel);
    JObj.AddPair('max_tokens', aMax_tokens);

    JMsg := TJSonObject.Create;
    JMsg.AddPair('role', FRole);

    Obj1 := TJSonObject.Create;
    Obj1.AddPair('type', 'text');
    Obj1.AddPair('text', aPrompt);
    JContent.Add(Obj1);

    Obj1 := TJSonObject.Create;
    Obj1.AddPair('type', 'image_url');
    Obj1.AddPair('image_url', TJSonObject.Create.AddPair('url', aUrl));
    JContent.Add(Obj1);

    JMsg.AddPair('content', JContent);

    JMessages.Add(JMsg);

    JObj.AddPair('messages', JMessages);

    St.WriteString(JObj.Format);
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      JObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
      Result := ParseVision(JObj);
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

Function TAiVision.ParseVision(Response: TJSonObject): String;
Var
  id, Model, role: String;
  // Prompt_Tokenes, completion_tokens, total_tokens, index: Integer;
  JArr: TJSonArray;
  i: Integer;

begin

  id := Response.GetValue<String>('id');
  Model := Response.GetValue<String>('model');
  FPrompt_Tokenes := Response.GetValue<TJSonObject>('usage').GetValue<Integer>('prompt_tokens');
  Fcompletion_tokens := Response.GetValue<TJSonObject>('usage').GetValue<Integer>('completion_tokens');
  Ftotal_tokens := Response.GetValue<TJSonObject>('usage').GetValue<Integer>('total_tokens');
  JArr := Response.GetValue<TJSonArray>('choices');
  FContent := '';

  For i := 0 to JArr.Count - 1 do
  Begin
    role := JArr.Items[i].GetValue<TJSonObject>('message').GetValue<String>('role');
    FContent := FContent + JArr.Items[i].GetValue<TJSonObject>('message').GetValue<String>('content') + sLineBreak;
  End;
  FContent := Trim(FContent);
  Result := FContent;

end;

procedure TAiVision.SetApiKey(const Value: String);
begin
  FApiKey := Value;
end;

procedure TAiVision.SetModel(const Value: String);
begin
  FModel := Value;
end;

procedure TAiVision.SetUrl(const Value: String);
begin
  If Value <> '' then
    FUrl := Value
  Else
    FUrl := GlOpenAIUrl;
end;

{ TAiImagesFiles }

Function TAiImagesFile.Base64ToStream(Base64: String): TMemoryStream;
begin
  Result := TBytesStream.Create(TNetEncoding.Base64.DecodeStringToBytes(Base64));
end;

constructor TAiImagesFile.Create;
begin
  Inherited Create;
  FImage := TMemoryStream.Create;
end;

destructor TAiImagesFile.Destroy;
begin
  FImage.Free;
  inherited;
end;

function TAiImagesFile.GetImage: TMemoryStream;
begin
  If (FImage.Size <= 0) then
  Begin
    If UrlFile <> '' then
      LoadImage(UrlFile);
  End;

  Result := FImage;
end;

Function TAiImagesFile.LoadImage(UrlFile: String): TMemoryStream;
Var
  NetHttp: TNetHTTPClient;
  Resp: IHTTPResponse;
Begin
  Result := Nil;
  FImage.Clear;
  FImage.Position := 0;
  NetHttp := TNetHTTPClient.Create(Nil);
  try
    Resp := NetHttp.Get(UrlFile, FImage);
    FImage.Position := 0;
    if Resp.StatusCode = 200 then // file was found
    Begin
      Result := FImage;
    End;
  finally
    NetHttp.Free;
  end;
End;

procedure TAiImagesFile.ParseImage(JObj: TJSonObject);
begin
  If JObj.TryGetValue<String>('url', FUrlFile) then
  Begin

  End;

  If JObj.TryGetValue<String>('b64_json', FBase64) then
  Begin
    FImage.Free;
    FImage := Base64ToStream(FBase64);
    FImage.Position := 0;
  End;

  JObj.TryGetValue<String>('revised_prompt', Frevised_prompt);

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
      JArr := JObj.GetValue<TJSonArray>('data');

      For JVal in JArr do
      Begin
        StoreFile := TJson.JsonToObject<TAiVectorStoreFile>(TJSonObject(JVal));
        FFileList.Add(StoreFile.id, StoreFile);
      End;
      Result := FFileList;
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
  TmpSv, StoreVector: TAiVectorStore;
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
      JArr := JObj.GetValue<TJSonArray>('data');

      For JVal in JArr do
      Begin
        StoreVector := ParseVectorStore(TJSonObject(JVal), FApiKey, FUrl);
        FVectorStoreList.Add(StoreVector.id, StoreVector);
      End;
      Result := FVectorStoreList;
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
      Result := TJson.JsonToObject<TAiVectorStoreFileBatch>(JObj);
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
      Result := TJson.JsonToObject<TAiVectorStoreFileBatch>(JObj);
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
      JArr := JObj.GetValue<TJSonArray>('data');

      For JVal in JArr do
      Begin
        StoreVector := TJson.JsonToObject<TAiVectorStoreFileBatch>(TJSonObject(JVal));
        FileBatchList.Add(StoreVector.id, StoreVector);
      End;
      Result := FileBatchList;
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
  Result := Self.TryGetValue(AiVectorStoreId, AiVectorStoreFileBatch);
end;

{ TAiMediaFiles }

procedure TAiMediaFile.Clear;
begin
  FContent.Clear;
end;

constructor TAiMediaFile.Create;
begin
  Inherited;
  FContent := TMemoryStream.Create;
  FProcesado := False;
end;

destructor TAiMediaFile.Destroy;
begin
  FContent.Free;
  inherited;
end;

function TAiMediaFile.GetBase64: String;
begin
  FContent.Position := 0;
  Result := TNetEncoding.Base64.EncodeBytesToString(FContent.Memory, FContent.Size);
end;

function TAiMediaFile.GetBytes: Integer;
begin
  Result := FContent.Size;
end;

function TAiMediaFile.GetContent: TMemoryStream;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Response: TMemoryStream;
  Res: IHTTPResponse;
  sUrl: String;
begin

  If FContent.Size > 5000 then // Si ya está cargado el archivo solo lo retorna
  Begin
    Result := FContent;
    Exit;
  End;

  // Si tiene asignada una url la carga de la url y la deja en memoria

  FContent.Clear;
  FContent.Position := 0;

  If FUrlMedia <> '' then
  Begin

    Client := THTTPClient.Create;
    sUrl := FUrlMedia;
    Response := TMemoryStream.Create;

    Try

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
  End;
end;

function TAiMediaFile.GetFileCategory: TAiFileCategory;
begin
  Result := GetContentCategory(ExtractFileExt(LowerCase(Ffilename)));
end;

function TAiMediaFile.GetMimeType: String;
begin
  Result := GetMimeTypeFromFileName(LowerCase(ExtractFileExt(Ffilename)));
end;

procedure TAiMediaFile.LoadFromBase64(aFileName, aBase64: String);
Var
  St: TMemoryStream;
begin
  St := TBytesStream.Create(TNetEncoding.Base64.DecodeStringToBytes(Base64));
  Try
    If Assigned(St) then
    Begin
      FContent.Clear;
      FContent.LoadFromStream(St);
      FFullFileName := aFileName;
      Ffilename := ExtractFileName(aFileName);
      FFileType := ExtractFileExt(filename);
    End;
  Finally
    St.Free;
  End;
end;

procedure TAiMediaFile.LoadFromfile(aFileName: String);
begin
  If TFile.Exists(aFileName) then
  Begin
    FContent.Clear;
    FContent.LoadFromfile(aFileName);
    FFullFileName := aFileName;
    Ffilename := ExtractFileName(aFileName);
    FFileType := LowerCase(ExtractFileExt(Ffilename));
  End;
end;

procedure TAiMediaFile.LoadFromStream(aFileName: String; Stream: TMemoryStream);
begin
  If Assigned(Stream) then
  Begin
    FContent.Clear;
    FContent.LoadFromStream(Stream);
    FFullFileName := aFileName;
    Ffilename := ExtractFileName(aFileName);
    FFileType := LowerCase(ExtractFileExt(Ffilename));
  End;
end;

procedure TAiMediaFile.LoadFromUrl(aUrl: String);
begin
  FUrlMedia := aUrl;
  FContent.Clear;
  GetContent;
end;

procedure TAiMediaFile.SaveToFile(aFileName: String);
begin
  FContent.SaveToFile(aFileName);
end;

procedure TAiMediaFile.SetBase64(const Value: String);
begin
  LoadFromBase64('', Value);
end;

procedure TAiMediaFile.Setfilename(const Value: String);
begin
  Ffilename := Value;
end;

procedure TAiMediaFile.SetFullFileName(const Value: String);
begin
  FFullFileName := Value;
end;

procedure TAiMediaFile.SetProcesado(const Value: Boolean);
begin
  FProcesado := Value;
end;

procedure TAiMediaFile.SetTranscription(const Value: String);
begin
  FTranscription := Value;
end;

procedure TAiMediaFile.SetUrlMedia(const Value: String);
begin
  FUrlMedia := Value;
end;

{ TAiMediaFiles }

function TAiMediaFiles.GetMediaList(aFilter: TAiFileCategory; aProcesado: Boolean = False): TAiMediaFilesArray;
Var
  i: Integer;
  Item: TAiMediaFile;
  Len: Integer;
begin
  For i := 0 to Self.Count - 1 do
  Begin
    Item := Self.Items[i];
    If (Item.FileCategory = aFilter) and (Item.Procesado = aProcesado) then
    Begin
      Len := Length(Result);
      SetLength(Result, Len + 1);
      Result[Length(Result) - 1] := Item;
    End;
  End;
end;

end.
