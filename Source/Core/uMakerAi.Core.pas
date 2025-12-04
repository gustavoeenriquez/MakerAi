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
//
// --------- CAMBIOS --------------------
// 04/11/2024 - adiciona el manejo de TAiMediaFile.detail para identificar la calidad de analisis de una imagen
// 04/11/2024 - Se corrige error de asignación en TAiMediaFile.LoadFromBase64
// 15/10/2025 - Code Cleanup

unit uMakerAi.Core;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Threading, System.Variants, System.Net.Mime, System.IOUtils,
  System.Generics.Collections, System.NetEncoding, System.JSON,
  System.StrUtils, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent, REST.JSON, REST.Types, REST.Client,
  System.Rtti, uMakerAi.Tools.TextEditor;

Type

  TAiToolsFunction = Class;

  // TAiImageSize = (TiaSize256, TiaSize512, TiaSize1024, TiaSize1024_1792, TiaSize1792_1024);
  // TAiImageResponseFormat = (tiaRUrl, tiaRB64);
  // TAiImageAStyleFormat = (tiaStyleVivid, tiaStyleNatural);

  TAiFileCategory = (Tfc_Text, Tfc_Image, Tfc_Audio, Tfc_Video, Tfc_pdf, Tfc_Document, //
    Tfc_WebSearch, Tfc_CalcSheet, Tfc_Presentation, Tfc_CompressFile, Tfc_Web, //
    Tfc_GraphicDesign, tfc_ExtracttextFile, Tfc_Any, Tfc_Unknow); //

  TAiFileCategories = set of TAiFileCategory;

  TAiChatMediaSupport = (Tcm_Text, Tcm_Image, Tcm_Audio, Tcm_Video, tcm_pdf, Tcm_Document, //
    tcm_WebSearch, Tcm_CalcSheet, Tcm_Presentation, Tcm_CompressFile, Tcm_Web, Tcm_GraphicDesign, tcm_code_interpreter, tcm_Memory, tcm_TextEditor, tcm_ComputerUse, tcm_Shell, tcm_Any, Tcm_Unknow); //

  TAiChatMediaSupports = set of TAiChatMediaSupport;

  // Tipo de evento para manejar errores
  TAiErrorEvent = procedure(Sender: TObject; const ErrorMsg: string; Exception: Exception; const AResponse: IHTTPResponse) of object;

  TAiThinkingLevel = (tlDefault, tlLow, tlMedium, tlHigh); // Default es medium en la mayoría de los casos
  TAiMediaResolution = (mrDefault, mrLow, mrMedium, mrHigh);

  // Se utiliza especialmente en OpenAi en la transcripción

  TAiTranscriptionResponseFormat = (trfText, trfJson, trfSrt, trfVtt, trfVerboseJson);

  // Enum para la granularidad de los timestamps
  TAiTimestampGranularity = (tsgNone, tsgWord, tsgSegment);
  TAiTimestampGranularities = set of TAiTimestampGranularity;

  // Tipos de datos necesarios para MCP y ToolFunctions
  TMCPLogEvent = procedure(Sender: TObject; const Msg: string) of object;

  // Evento para notificar cambios de estado del servidor (iniciado, detenido, etc.)
  TMCPStatusEvent = procedure(Sender: TObject; const StatusMsg: string) of object;

  TToolFormat = (tfUnknown, tfOpenAI, tfOpenAIResponses, tfClaude, tfGemini, tfMCP);
  TToolTransportType = (tpStdIo, tpHttp, tpSSE, tpMakerAi);


  TAiChatState = (
    acsIdle,           // Inactivo
    acsConnecting,     // Conectando / Enviando Request
    acsCreated,        // Servidor aceptó (Recibido ID)
    acsReasoning,      // Pensando / Razonando (Chain of Thought)
    acsWriting,        // Escribiendo respuesta visible
    acsToolCalling,    // El modelo pide usar una herramienta
    acsToolExecuting,  // Ejecutando la herramienta (Local o Remota)
    acsFinished,       // Completado con éxito
    acsAborted,        // Abortado por el usuario
    acsError           // Error
  );

  // Definición del evento
  TAiStateChangeEvent = procedure(Sender: TObject; State: TAiChatState; const Description: string) of object;


  TAiMediaFiles = Class;

  // Clase utilizada para el manejo de archivos de medios como audio, imágenes, pdf, text, etc.

  TAiMediaFile = Class
  Private
    Ffilename: String;
    FUrlMedia: String;
    FFileType: String;
    FContent: TMemoryStream;
    FFullFileName: String;
    FTranscription: String;
    FProcesado: Boolean;
    FDetail: String;
    FIdAudio: String;
    FCloudState: String;
    FCloudName: String;
    FCacheName: String;
    FIdFile: String;
    FMediaFiles: TAiMediaFiles;
    FCacheControl: Boolean;
    FEnableCitations: Boolean;
    FContext: string;
    FTitle: string;
    function GetBase64: String;
    procedure SetBase64(const Value: String);
    procedure Setfilename(const Value: String);
    procedure SetUrlMedia(const Value: String);
    function GetBytes: Integer;
    procedure SetFullFileName(const Value: String);
    function GetMimeType: String;
    function GetFileCategory: TAiFileCategory;
    procedure SetTranscription(const Value: String);
    procedure SetProcesado(const Value: Boolean);
    procedure SetDetail(const Value: String);
    procedure SetIdAudio(const Value: String);
    procedure SetCacheName(const Value: String);
    procedure SetIdFile(const Value: String);
    procedure SetMediaFiles(const Value: TAiMediaFiles);
  Protected
    Procedure DownloadFileFromUrl(Url: String); Virtual;
    function GetContent: TMemoryStream; Virtual;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure LoadFromfile(aFileName: String); Virtual;
    Procedure LoadFromUrl(aUrl: String); Virtual;
    Procedure LoadFromBase64(aFileName, aBase64: String); Virtual;
    Procedure LoadFromStream(aFileName: String; Stream: TMemoryStream); Virtual;
    Procedure SaveToFile(aFileName: String); Virtual;
    Function ToString: String; override;
    Procedure Clear; Virtual;

    function ToJsonObject: TJSONObject; // Exporta el objeto completo a un json
    procedure LoadFromJsonObject(AObject: TJSONObject);

    procedure Assign(Source: TAiMediaFile);

    Property filename: String read Ffilename write Setfilename;
    Property bytes: Integer read GetBytes;
    Property Content: TMemoryStream read GetContent;
    Property FileCategory: TAiFileCategory read GetFileCategory;
    // Uri de donde se encuentra el archivo para ser subido al modelo
    Property UrlMedia: String read FUrlMedia write SetUrlMedia;

    Property CloudState: String read FCloudState write FCloudState;
    // Nombre del archivo con que fue guardado dentro del modelo disponible para la API
    Property CloudName: String read FCloudName write FCloudName;
    // Nombre del archivo guardado como caché dentro de la api, es posible preguntar entre varias iteracciones del chat
    Property CacheName: String read FCacheName write SetCacheName;

    // El Id con el que se identifica el archivo en el servidor
    Property IdFile: String read FIdFile write SetIdFile;
    // Guarda la URI de archivo generado por la API para almacenar el audio que ya generó el modelo
    Property IdAudio: String read FIdAudio write SetIdAudio;
    Property Base64: String read GetBase64 write SetBase64;
    Property FullFileName: String read FFullFileName write SetFullFileName;
    Property MimeType: String read GetMimeType;
    // Propiedad que se pasa con el archivo de media, en la imagen con OpenAi  indica si se analiza en detalle o "high" o en baja resolución "low"
    // En la transcripción va el otro formato si lo hay,  ej.  el json que genera el formato VTS
    Property Detail: String read FDetail write SetDetail;
    // Transcription- Si el archivo adjunto se procesa por separado aquí se guarda lo que retorna el modelo correspondiente
    Property Transcription: String read FTranscription write SetTranscription;
    Property Procesado: Boolean read FProcesado write SetProcesado;
    Property MediaFiles: TAiMediaFiles read FMediaFiles write SetMediaFiles;
    Property CacheControl: Boolean read FCacheControl write FCacheControl;

    Property Title: string read FTitle write FTitle; // Titulo del documento
    Property Context: string read FContext write FContext; // Información adicional del documento es solo contexto
    Property EnableCitations: Boolean read FEnableCitations write FEnableCitations; // Si este documento se incluye para ser citado por la IA
  End;

  // Conjunto de archivos para su manejo en el chat
  TAiMediaFilesArray = Array of TAiMediaFile;

  TAiMediaFiles = Class(TObjectList<TAiMediaFile>)
  Private
  Protected
  Public
    // Si el modelo no maneja este tipo de media files, se pueden preprocesar en el evento del chat
    // y el texto del proceso se adiciona al prompt, y aquí ya no se tendrían en cuenta
    Function GetMediaList(aFilters: TAiFileCategories; aProcesado: Boolean = False): TAiMediaFilesArray;
    Function ToMediaFileArray: TAiMediaFilesArray; // Retrona una lista con clones de los objetos
  End;

  // Clase de manejo de los metadatos que se pasan al api del chat de los llm
  TAiMetadata = Class(TDictionary<String, String>)
  Private
    function GetAsText: String;
    procedure SetAsText(const Value: String);
    function GetJSonText: String;
    procedure SetJsonText(const Value: String);
  Protected
  Public
    Function ToJSon: TJSONObject;
    Property AsText: String Read GetAsText Write SetAsText;
    Property JsonText: String Read GetJSonText Write SetJsonText;
  End;

  // Clase que maneja las funciones de los tools
  TAiToolsFunction = class(TObject)
    id: string;
    Tipo: string;
    name: string;
    Description: String; // Descripción de la función
    Arguments: string; // Si tiene parámetros en forma de json se utiliza este
    Params: TStringList; // Si tiene parámetros en forma de name=value se utiliza este si arguments = ''
    &Function: string; // Nombre de la función
    Response: String; // String que responde la función al LLM
    Body: TJSONObject; // El body en json que retorna la función, se utiliza para depuración o para obtener información adicional
    Metadata: TAiMetadata; // Metadatos adicionales que se pueden enviar a la función
    AskMsg: TObject; // TAiChatMessage que representa la pregunta
    ResMsg: TObject; // TAiChatMessage que representa la respuesta

    Constructor Create;
    Destructor Destroy; Override;
    Procedure ParseFunction(JObj: TJSONObject); // Esta función se reemplazará por estas dos según la necesidad

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

  TAiWebSearchItem = Class
    &type: String;
    start_index: Integer;
    end_index: Integer;
    Url: String;
    Title: String;
  End;

  TAiWebSearchArray = Class(TObjectList<TAiWebSearchItem>);

  TAiWebSearch = Class
    &type: String;
    text: String;
    annotations: TAiWebSearchArray;
    Constructor Create;
  End;


  // Partiendo de la extensión del archivo obtiene la categoria TAiFileCategori
function GetContentCategory(FileExtension: string): TAiFileCategory;

// Obtiene el mime de un archivo basado en la extensión .mp3 o mp3
function GetMimeTypeFromFileName(FileExtension: string): string;
function GetFileExtensionFromMimeType(MimeType: string): string;

// Convierte un stream en Base64
function StreamToBase64(Stream: TMemoryStream): String;

// convierte una lista de valores Key1=Value1  en una lista de parametros de query de una URL
function GetParametrosURL(Parametros: TStringList): string;

implementation

{$IFDEF LINUX}

uses uMakerAi.Utils.System;
{$ENDIF}
{$IFDEF MSWINDOWS}

uses Winapi.ShellAPI, Winapi.Windows;
{$ENDIF}
{$REGION 'Utilidades varias' }
{$I uMakerAi.Version.inc}

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

  // Audio formats
  if SameText(FileExtension, 'mp3') then
    Result := 'audio/mpeg'
  else if SameText(FileExtension, 'mp4') and (Pos('audio', FileExtension) > 0) then // Para distinguir de video
    Result := 'audio/mp4'
  else if SameText(FileExtension, 'mpga') then
    Result := 'audio/mpeg'
  else if SameText(FileExtension, 'm4a') then
    Result := 'audio/mp4'
  else if SameText(FileExtension, 'ogg') then
    Result := 'audio/ogg'
  else if SameText(FileExtension, 'wav') then
    Result := 'audio/wav'
  else if SameText(FileExtension, 'flac') then
    Result := 'audio/flac'
  else if SameText(FileExtension, 'aac') then
    Result := 'audio/aac'
  else if SameText(FileExtension, 'wma') then
    Result := 'audio/x-ms-wma'
  else if SameText(FileExtension, 'opus') then
    Result := 'audio/opus'

    // Video formats
  else if SameText(FileExtension, 'mp4') then
    Result := 'video/mp4'
  else if SameText(FileExtension, 'mpeg') then
    Result := 'video/mpeg'
  else if SameText(FileExtension, 'mpg') then
    Result := 'video/mpeg'
  else if SameText(FileExtension, 'webm') then
    Result := 'video/webm'
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
  else if SameText(FileExtension, 'm4v') then
    Result := 'video/x-m4v'

    // Image formats (existentes + nuevos)
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
  else if SameText(FileExtension, 'webp') then
    Result := 'image/webp'
  else if SameText(FileExtension, 'avif') then
    Result := 'image/avif'
  else if SameText(FileExtension, 'heic') then
    Result := 'image/heic'
  else if SameText(FileExtension, 'heif') then
    Result := 'image/heif'

    // Text formats (existentes + nuevos)
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
  else if SameText(FileExtension, 'js') then
    Result := 'text/javascript'
  else if SameText(FileExtension, 'md') then
    Result := 'text/markdown'
  else if SameText(FileExtension, 'rtf') then
    Result := 'application/rtf'

    // Application formats
  else if SameText(FileExtension, 'xml') then
    Result := 'application/xml'
  else if SameText(FileExtension, 'json') then
    Result := 'application/json'
  else if SameText(FileExtension, 'pdf') then
    Result := 'application/pdf'

    // Microsoft Office formats
  else if SameText(FileExtension, 'doc') then
    Result := 'application/msword'
  else if SameText(FileExtension, 'docx') then
    Result := 'application/vnd.openxmlformats-officedocument.wordprocessingml.document'
  else if SameText(FileExtension, 'xls') then
    Result := 'application/vnd.ms-excel'
  else if SameText(FileExtension, 'xlsx') then
    Result := 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
  else if SameText(FileExtension, 'ppt') then
    Result := 'application/vnd.ms-powerpoint'
  else if SameText(FileExtension, 'pptx') then
    Result := 'application/vnd.openxmlformats-officedocument.presentationml.presentation'

    // OpenDocument formats
  else if SameText(FileExtension, 'odt') then
    Result := 'application/vnd.oasis.opendocument.text'
  else if SameText(FileExtension, 'ods') then
    Result := 'application/vnd.oasis.opendocument.spreadsheet'
  else if SameText(FileExtension, 'odp') then
    Result := 'application/vnd.oasis.opendocument.presentation'

    // Archive formats (existentes + nuevos)
  else if SameText(FileExtension, 'zip') then
    Result := 'application/zip'
  else if SameText(FileExtension, 'gzip') then
    Result := 'application/gzip'
  else if SameText(FileExtension, 'gz') then
    Result := 'application/gzip'
  else if SameText(FileExtension, 'tar') then
    Result := 'application/x-tar'
  else if SameText(FileExtension, 'rar') then
    Result := 'application/vnd.rar'
  else if SameText(FileExtension, '7z') then
    Result := 'application/x-7z-compressed'
  else if SameText(FileExtension, 'bz2') then
    Result := 'application/x-bzip2'

    // Executable formats
  else if SameText(FileExtension, 'exe') then
    Result := 'application/vnd.microsoft.portable-executable'
  else if SameText(FileExtension, 'msi') then
    Result := 'application/x-msi'
  else if SameText(FileExtension, 'dll') then
    Result := 'application/x-msdownload'
  else if SameText(FileExtension, 'deb') then
    Result := 'application/vnd.debian.binary-package'
  else if SameText(FileExtension, 'dmg') then
    Result := 'application/x-apple-diskimage'
  else if SameText(FileExtension, 'pkg') then
    Result := 'application/vnd.apple.installer+xml'

    // Font formats
  else if SameText(FileExtension, 'ttf') then
    Result := 'font/ttf'
  else if SameText(FileExtension, 'otf') then
    Result := 'font/otf'
  else if SameText(FileExtension, 'woff') then
    Result := 'font/woff'
  else if SameText(FileExtension, 'woff2') then
    Result := 'font/woff2'
  else if SameText(FileExtension, 'eot') then
    Result := 'application/vnd.ms-fontobject'

    // 3D and CAD formats
  else if SameText(FileExtension, 'glb') then
    Result := 'model/gltf-binary'
  else if SameText(FileExtension, 'gltf') then
    Result := 'model/gltf+json'
  else if SameText(FileExtension, 'stl') then
    Result := 'model/stl'
  else if SameText(FileExtension, 'obj') then
    Result := 'model/obj'

    // Database formats
  else if SameText(FileExtension, 'sqlite') then
    Result := 'application/vnd.sqlite3'
  else if SameText(FileExtension, 'db') then
    Result := 'application/x-sqlite3'
  else if SameText(FileExtension, 'mdb') then
    Result := 'application/vnd.ms-access'

    // eBook formats
  else if SameText(FileExtension, 'epub') then
    Result := 'application/epub+zip'
  else if SameText(FileExtension, 'mobi') then
    Result := 'application/x-mobipocket-ebook'
  else if SameText(FileExtension, 'azw') then
    Result := 'application/vnd.amazon.ebook'

    // Certificate formats
  else if SameText(FileExtension, 'p12') then
    Result := 'application/x-pkcs12'
  else if SameText(FileExtension, 'crt') then
    Result := 'application/x-x509-ca-cert'
  else if SameText(FileExtension, 'cer') then
    Result := 'application/x-x509-ca-cert'
  else if SameText(FileExtension, 'pem') then
    Result := 'application/x-pem-file'

    // Programming and development files
  else if SameText(FileExtension, 'py') then
    Result := 'text/x-python'
  else if SameText(FileExtension, 'java') then
    Result := 'text/x-java-source'
  else if SameText(FileExtension, 'c') then
    Result := 'text/x-c'
  else if SameText(FileExtension, 'cpp') then
    Result := 'text/x-c++'
  else if SameText(FileExtension, 'cs') then
    Result := 'text/x-csharp'
  else if SameText(FileExtension, 'php') then
    Result := 'application/x-httpd-php'
  else if SameText(FileExtension, 'rb') then
    Result := 'text/x-ruby'
  else if SameText(FileExtension, 'go') then
    Result := 'text/x-go'
  else if SameText(FileExtension, 'rs') then
    Result := 'text/x-rust'
  else if SameText(FileExtension, 'sh') then
    Result := 'application/x-sh'
  else if SameText(FileExtension, 'bat') then
    Result := 'application/x-msdos-program'

    // Specialized formats
  else if SameText(FileExtension, 'psd') then
    Result := 'application/vnd.adobe.photoshop'
  else if SameText(FileExtension, 'ai') then
    Result := 'application/postscript'
  else if SameText(FileExtension, 'eps') then
    Result := 'application/postscript'
  else if SameText(FileExtension, 'ps') then
    Result := 'application/postscript'

    // Default case
  else
    Result := 'application/octet-stream';
end;

function GetFileExtensionFromMimeType(MimeType: string): string;
begin
  MimeType := LowerCase(Trim(MimeType));

  // Audio formats (ya existentes)
  if SameText(MimeType, 'audio/mpeg') then
    Result := 'mp3'
  else if SameText(MimeType, 'audio/mp4') then
    Result := 'm4a'
  else if SameText(MimeType, 'audio/ogg') then
    Result := 'ogg'
  else if SameText(MimeType, 'audio/wav') then
    Result := 'wav'
  else if SameText(MimeType, 'audio/flac') then
    Result := 'flac'
  else if SameText(MimeType, 'audio/aac') then
    Result := 'aac'
  else if SameText(MimeType, 'audio/wma') then
    Result := 'wma'

    // Video formats (ya existentes + nuevos)
  else if SameText(MimeType, 'video/mp4') then
    Result := 'mp4'
  else if SameText(MimeType, 'video/mpeg') then
    Result := 'mpeg'
  else if SameText(MimeType, 'video/webm') then
    Result := 'webm'
  else if SameText(MimeType, 'video/x-msvideo') then
    Result := 'avi'
  else if SameText(MimeType, 'video/quicktime') then
    Result := 'mov'
  else if SameText(MimeType, 'video/x-ms-wmv') then
    Result := 'wmv'
  else if SameText(MimeType, 'video/x-flv') then
    Result := 'flv'
  else if SameText(MimeType, 'video/3gpp') then
    Result := '3gp'
  else if SameText(MimeType, 'video/x-matroska') then
    Result := 'mkv'

    // Image formats (ya existentes + nuevos)
  else if SameText(MimeType, 'image/gif') then
    Result := 'gif'
  else if SameText(MimeType, 'image/jpeg') then
    Result := 'jpg'
  else if SameText(MimeType, 'image/png') then
    Result := 'png'
  else if SameText(MimeType, 'image/bmp') then
    Result := 'bmp'
  else if SameText(MimeType, 'image/svg+xml') then
    Result := 'svg'
  else if SameText(MimeType, 'image/vnd.microsoft.icon') then
    Result := 'ico'
  else if SameText(MimeType, 'image/tiff') then
    Result := 'tiff'
  else if SameText(MimeType, 'image/webp') then
    Result := 'webp'
  else if SameText(MimeType, 'image/avif') then
    Result := 'avif'
  else if SameText(MimeType, 'image/heic') then
    Result := 'heic'
  else if SameText(MimeType, 'image/heif') then
    Result := 'heif'

    // Document formats binarios
  else if SameText(MimeType, 'application/pdf') then
    Result := 'pdf'
  else if SameText(MimeType, 'application/msword') then
    Result := 'doc'
  else if SameText(MimeType, 'application/vnd.openxmlformats-officedocument.wordprocessingml.document') then
    Result := 'docx'
  else if SameText(MimeType, 'application/vnd.ms-excel') then
    Result := 'xls'
  else if SameText(MimeType, 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet') then
    Result := 'xlsx'
  else if SameText(MimeType, 'application/vnd.ms-powerpoint') then
    Result := 'ppt'
  else if SameText(MimeType, 'application/vnd.openxmlformats-officedocument.presentationml.presentation') then
    Result := 'pptx'
  else if SameText(MimeType, 'application/vnd.oasis.opendocument.text') then
    Result := 'odt'
  else if SameText(MimeType, 'application/vnd.oasis.opendocument.spreadsheet') then
    Result := 'ods'
  else if SameText(MimeType, 'application/vnd.oasis.opendocument.presentation') then
    Result := 'odp'
  else if SameText(MimeType, 'application/rtf') then
    Result := 'rtf'

    // Formatos de archivo comprimido
  else if SameText(MimeType, 'application/zip') then
    Result := 'zip'
  else if SameText(MimeType, 'application/gzip') then
    Result := 'gz'
  else if SameText(MimeType, 'application/x-tar') then
    Result := 'tar'
  else if SameText(MimeType, 'application/vnd.rar') then
    Result := 'rar'
  else if SameText(MimeType, 'application/x-7z-compressed') then
    Result := '7z'
  else if SameText(MimeType, 'application/x-bzip2') then
    Result := 'bz2'

    // Ejecutables y bibliotecas
  else if SameText(MimeType, 'application/vnd.microsoft.portable-executable') then
    Result := 'exe'
  else if SameText(MimeType, 'application/x-msdownload') then
    Result := 'dll'
  else if SameText(MimeType, 'application/x-msi') then
    Result := 'msi'
  else if SameText(MimeType, 'application/vnd.apple.installer+xml') then
    Result := 'pkg'
  else if SameText(MimeType, 'application/vnd.debian.binary-package') then
    Result := 'deb'

    // Formatos 3D y CAD
  else if SameText(MimeType, 'model/gltf-binary') then
    Result := 'glb'
  else if SameText(MimeType, 'model/gltf+json') then
    Result := 'gltf'
  else if SameText(MimeType, 'model/stl') then
    Result := 'stl'
  else if SameText(MimeType, 'model/obj') then
    Result := 'obj'
  else if SameText(MimeType, 'application/sla') then
    Result := 'stl'

    // Bases de datos
  else if SameText(MimeType, 'application/vnd.sqlite3') then
    Result := 'sqlite'
  else if SameText(MimeType, 'application/x-sqlite3') then
    Result := 'db'
  else if SameText(MimeType, 'application/vnd.ms-access') then
    Result := 'mdb'

    // Fuentes
  else if SameText(MimeType, 'font/ttf') then
    Result := 'ttf'
  else if SameText(MimeType, 'font/otf') then
    Result := 'otf'
  else if SameText(MimeType, 'font/woff') then
    Result := 'woff'
  else if SameText(MimeType, 'font/woff2') then
    Result := 'woff2'
  else if SameText(MimeType, 'application/vnd.ms-fontobject') then
    Result := 'eot'

    // Formatos de datos científicos/técnicos
  else if SameText(MimeType, 'application/x-hdf') then
    Result := 'hdf'
  else if SameText(MimeType, 'application/x-netcdf') then
    Result := 'nc'
  else if SameText(MimeType, 'application/fits') then
    Result := 'fits'

    // Formatos de ebook
  else if SameText(MimeType, 'application/epub+zip') then
    Result := 'epub'
  else if SameText(MimeType, 'application/x-mobipocket-ebook') then
    Result := 'mobi'
  else if SameText(MimeType, 'application/vnd.amazon.ebook') then
    Result := 'azw'

    // Certificados y claves
  else if SameText(MimeType, 'application/x-pkcs12') then
    Result := 'p12'
  else if SameText(MimeType, 'application/x-x509-ca-cert') then
    Result := 'crt'
  else if SameText(MimeType, 'application/pkcs8') then
    Result := 'p8'

    // Formatos específicos de aplicaciones
  else if SameText(MimeType, 'application/vnd.adobe.photoshop') then
    Result := 'psd'
  else if SameText(MimeType, 'application/postscript') then
    Result := 'ps'
  else if SameText(MimeType, 'application/vnd.sketchup.skp') then
    Result := 'skp'

    // Binario genérico
  else if SameText(MimeType, 'application/octet-stream') then
    Result := 'bin'

    // Default case
  else
    Result := 'bin';

  Result := '.' + Result; // adiciona el punto para mantener el estandar.
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

{
  function GetContentCategory(FileExtension: string): TAiFileCategory;
  begin
  FileExtension := LowerCase(Trim(StringReplace(ExtractFileName(FileExtension), '.', '', [rfReplaceAll])));

  if (FileExtension = 'jpg') or (FileExtension = 'jpeg') or (FileExtension = 'png') or (FileExtension = 'gif') or (FileExtension = 'bmp') or
  (FileExtension = 'tiff') or (FileExtension = 'svg') or (FileExtension = 'webp') then
  Result := Tfc_Image
  else if (FileExtension = 'mp3') or (FileExtension = 'wav') or (FileExtension = 'flac') or (FileExtension = 'aac') or
  (FileExtension = 'ogg') or (FileExtension = 'wma') or (FileExtension = 'm4a') then
  Result := Tfc_Audio
  else if (FileExtension = 'avi') or (FileExtension = 'mp4') or (FileExtension = 'mkv') or (FileExtension = 'mov') or
  (FileExtension = 'wmv') or (FileExtension = 'flv') or (FileExtension = 'webm') then
  Result := Tfc_Video
  else if (FileExtension = 'doc') or (FileExtension = 'docx') or (FileExtension = 'odt') or (FileExtension = 'rtf') or
  (FileExtension = 'tex') then
  Result := Tfc_Document
  else if (FileExtension = 'pdf') then
  Result := Tfc_pdf
  else if (FileExtension = 'txt') or (FileExtension = 'md') or (FileExtension = 'rtf') then
  Result := Tfc_Text
  else if (FileExtension = 'xls') or (FileExtension = 'xlsx') or (FileExtension = 'ods') or (FileExtension = 'csv') then
  Result := Tfc_CalcSheet
  else if (FileExtension = 'ppt') or (FileExtension = 'pptx') or (FileExtension = 'odp') then
  Result := Tfc_Presentation
  else if (FileExtension = 'zip') or (FileExtension = 'rar') or (FileExtension = 'tar') or (FileExtension = 'gz') or (FileExtension = 'bz2')
  or (FileExtension = '7z') or (FileExtension = 'xz') then
  Result := Tfc_CompressFile
  else if (FileExtension = 'html') or (FileExtension = 'htm') or (FileExtension = 'xml') or (FileExtension = 'json') or
  (FileExtension = 'css') or (FileExtension = 'js') then
  Result := Tfc_Web
  else if (FileExtension = 'psd') or (FileExtension = 'ai') then
  Result := Tfc_GraphicDesign
  else
  Result := Tfc_Unknow;
  end;
}

function GetContentCategory(FileExtension: string): TAiFileCategory;
begin
  FileExtension := LowerCase(Trim(StringReplace(FileExtension, '.', '', [rfReplaceAll])));

  // Image formats
  if (FileExtension = 'jpg') or (FileExtension = 'jpeg') or (FileExtension = 'png') or (FileExtension = 'gif') or (FileExtension = 'bmp') or (FileExtension = 'tiff') or (FileExtension = 'tif') or (FileExtension = 'svg') or
    (FileExtension = 'webp') or (FileExtension = 'avif') or (FileExtension = 'heic') or (FileExtension = 'heif') or (FileExtension = 'ico') then
    Result := Tfc_Image

    // Audio formats
  else if (FileExtension = 'mp3') or (FileExtension = 'wav') or (FileExtension = 'flac') or (FileExtension = 'aac') or (FileExtension = 'ogg') or (FileExtension = 'wma') or (FileExtension = 'm4a') or (FileExtension = 'opus') or
    (FileExtension = 'mpga') then
    Result := Tfc_Audio

    // Video formats
  else if (FileExtension = 'avi') or (FileExtension = 'mp4') or (FileExtension = 'mkv') or (FileExtension = 'mov') or (FileExtension = 'wmv') or (FileExtension = 'flv') or (FileExtension = 'webm') or (FileExtension = 'mpeg') or
    (FileExtension = 'mpg') or (FileExtension = '3gp') or (FileExtension = 'm4v') then
    Result := Tfc_Video

    // PDF (categoría separada)
  else if (FileExtension = 'pdf') then
    Result := Tfc_pdf

    // Document formats (Word processors)
  else if (FileExtension = 'doc') or (FileExtension = 'docx') or (FileExtension = 'odt') or (FileExtension = 'rtf') or (FileExtension = 'tex') then
    Result := Tfc_Document

    // Spreadsheets
  else if (FileExtension = 'xls') or (FileExtension = 'xlsx') or (FileExtension = 'ods') or (FileExtension = 'csv') then
    Result := Tfc_CalcSheet

    // Presentations
  else if (FileExtension = 'ppt') or (FileExtension = 'pptx') or (FileExtension = 'odp') then
    Result := Tfc_Presentation

    // Plain text files
  else if (FileExtension = 'txt') or (FileExtension = 'md') or (FileExtension = 'log') or (FileExtension = 'readme') then
    Result := Tfc_Text

    // Programming/Code files
    {
  else if (FileExtension = 'py') or (FileExtension = 'java') or (FileExtension = 'c') or (FileExtension = 'cpp') or (FileExtension = 'cs') or (FileExtension = 'php') or (FileExtension = 'rb') or (FileExtension = 'go') or
    (FileExtension = 'rs') or (FileExtension = 'swift') or (FileExtension = 'kt') or (FileExtension = 'scala') or (FileExtension = 'r') or (FileExtension = 'sql') or (FileExtension = 'sh') or (FileExtension = 'bat') or
    (FileExtension = 'ps1') or (FileExtension = 'dockerfile') then
    Result := tfc_code_interpreter
    }

    // Web files
  else if (FileExtension = 'html') or (FileExtension = 'htm') or (FileExtension = 'xml') or (FileExtension = 'json') or (FileExtension = 'css') or (FileExtension = 'js') or (FileExtension = 'jsx') or (FileExtension = 'ts') or
    (FileExtension = 'tsx') or (FileExtension = 'vue') or (FileExtension = 'angular') or (FileExtension = 'php') or (FileExtension = 'asp') or (FileExtension = 'aspx') or (FileExtension = 'jsp') then
    Result := Tfc_Web

    // Compressed files
  else if (FileExtension = 'zip') or (FileExtension = 'rar') or (FileExtension = 'tar') or (FileExtension = 'gz') or (FileExtension = 'bz2') or (FileExtension = '7z') or (FileExtension = 'xz') or (FileExtension = 'gzip') then
    Result := Tfc_CompressFile

    // Graphic Design files
  else if (FileExtension = 'psd') or (FileExtension = 'ai') or (FileExtension = 'eps') or (FileExtension = 'indd') or (FileExtension = 'sketch') or (FileExtension = 'fig') or (FileExtension = 'xd') then
    Result := Tfc_GraphicDesign

    // Text-based files that are not plain text
  else if (FileExtension = 'yaml') or (FileExtension = 'yml') or (FileExtension = 'toml') or (FileExtension = 'ini') or (FileExtension = 'cfg') or (FileExtension = 'conf') then
    Result := tfc_ExtracttextFile

    // Default case
  else
    Result := Tfc_Unknow;
end;

{ TAiMediaFiles }

procedure TAiMediaFile.Assign(Source: TAiMediaFile);
begin
  // 1. Protección contra auto-asignación y fuentes nulas.
  if (Source = nil) or (Source = Self) then
    Exit;

  // 2. Copiar todas las propiedades "planas" (campos de valor).
  // Usamos los campos privados (F...) para evitar disparar lógica
  // innecesaria que podría estar en los setters.
  Self.Ffilename := Source.Ffilename;
  Self.FUrlMedia := Source.FUrlMedia;
  Self.FFileType := Source.FFileType; // No tiene setter, así que copiamos el campo.
  Self.FFullFileName := Source.FFullFileName;
  Self.FTranscription := Source.FTranscription;
  Self.FProcesado := Source.FProcesado;
  Self.FDetail := Source.FDetail;
  Self.FIdAudio := Source.FIdAudio;
  Self.FCloudState := Source.FCloudState;
  Self.FCloudName := Source.FCloudName;
  Self.FCacheName := Source.FCacheName;
  Self.FIdFile := Source.FIdFile;

  // 3. Copia profunda (Deep Copy) del contenido del TMemoryStream.
  // Este es el paso más crítico para evitar que ambos objetos compartan
  // el mismo stream de memoria.
  if Assigned(Source.Content) and (Source.Content.Size > 0) then
  begin
    // Si nuestro propio stream no existe, lo creamos.
    if not Assigned(Self.FContent) then
      Self.FContent := TMemoryStream.Create;

    // Preparamos los streams para la copia.
    Self.FContent.Clear;
    Source.Content.Position := 0; // Aseguramos que leemos el origen desde el principio.

    // Copiamos el contenido.
    Self.FContent.CopyFrom(Source.Content, 0);

    // Buena práctica: Dejar ambos streams en su posición inicial.
    Self.FContent.Position := 0;
    Source.Content.Position := 0;
  end
  else
  begin
    // Si el stream de origen está vacío o no existe, nos aseguramos
    // de que nuestro propio stream también esté vacío.
    if Assigned(Self.FContent) then
      Self.FContent.Clear;
  end;

  // 4. Propiedades que NO se copian.
  // Self.FMediaFiles: Esta es una referencia al contenedor padre.
  // El nuevo objeto clonado será añadido a una nueva lista,
  // y esa lista le asignará su propia referencia. No la tocamos aquí.

end;

procedure TAiMediaFile.Clear;
begin
  FContent.Clear;
  Ffilename := '';
  FUrlMedia := '';
  FFileType := '';
  FFullFileName := '';
  FTranscription := '';
  FProcesado := False;
  FDetail := '';
  FIdAudio := '';
  // FCloudUri := ''
end;

constructor TAiMediaFile.Create;
begin
  Inherited;
  FContent := TMemoryStream.Create;
  FMediaFiles := TAiMediaFiles.Create;
  FProcesado := False;
  FDetail := ''; // por defecto utiliza vacío para no enviar nada y hacerlo compatible con otros modelos, detallado = "high" or "low"
end;

destructor TAiMediaFile.Destroy;
begin
  FContent.Free;
  FMediaFiles.Clear;
  FMediaFiles.Free;
  inherited;
end;

procedure TAiMediaFile.DownloadFileFromUrl(Url: String);
Var
  Client: TNetHTTPClient;
  Headers: TNetHeaders;
  Response: TMemoryStream;
  Res: IHTTPResponse;
begin

  If Url <> '' then
  Begin

    Client := TNetHTTPClient.Create(Nil);
{$IF CompilerVersion >= 34} // Delphi 10.3 Rio y posteriores
    Client.SynchronizeEvents := False;
{$IFEND}
    Response := TMemoryStream.Create;

    Try

      Res := Client.Get(Url, Response, Headers);

      if Res.StatusCode = 200 then
      Begin

        FContent.Clear; // Limpia el contenido actual antes de adicionar el nuevo
        FContent.Position := 0;

        Response.Position := 0;
        FContent.LoadFromStream(Response);
        FContent.Position := 0;
      End
      else
        Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);

    Finally
      Client.Free;
      Response.Free;
    End;
  End;
end;

function TAiMediaFile.GetBase64: String;
begin
  FContent.Position := 0;
  Result := TNetEncoding.Base64.EncodeBytesToString(FContent.Memory, FContent.Size);
  Result := StringReplace(Result, sLineBreak, '', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '', [rfReplaceAll]); // #10 es LF (\n)
end;

function TAiMediaFile.GetBytes: Integer;
begin
  Result := FContent.Size;
end;

function TAiMediaFile.GetContent: TMemoryStream;
begin
  Result := FContent;
  If FContent.Size > 5000 then // Si ya está cargado el archivo solo lo retorna
    Exit;
  // Si tiene asignada una url la carga de la url y la deja en memoria

  If FUrlMedia <> '' then
  Begin
    DownloadFileFromUrl(FUrlMedia);
    Result := FContent;
  End;
end;

function TAiMediaFile.GetFileCategory: TAiFileCategory;
begin
  If Trim(Ffilename) = '' then
    Result := Tfc_Unknow
  Else
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
  St := TBytesStream.Create(TNetEncoding.Base64.DecodeStringToBytes(aBase64));
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

procedure TAiMediaFile.LoadFromJsonObject(AObject: TJSONObject);
var
  LBase64: string;
  LFilename: string;
begin
  // Limpiamos el estado actual antes de cargar
  Clear;

  AObject.TryGetValue<string>('filename', Self.Ffilename);
  AObject.TryGetValue<string>('urlMedia', Self.FUrlMedia);
  AObject.TryGetValue<string>('fullFileName', Self.FFullFileName);
  AObject.TryGetValue<string>('transcription', Self.FTranscription);
  AObject.TryGetValue<Boolean>('procesado', Self.FProcesado);
  AObject.TryGetValue<string>('detail', Self.FDetail);
  AObject.TryGetValue<string>('idAudio', Self.FIdAudio);
  AObject.TryGetValue<string>('cloudState', Self.FCloudState);
  AObject.TryGetValue<string>('cloudName', Self.FCloudName);
  AObject.TryGetValue<string>('cacheName', Self.FCacheName);
  AObject.TryGetValue<string>('idFile', Self.FIdFile);

  // Cargamos el contenido usando el método existente
  if AObject.TryGetValue<string>('base64', LBase64) and (LBase64 <> '') then
  begin
    AObject.TryGetValue<string>('filename', LFilename);
    Self.LoadFromBase64(LFilename, LBase64);
  end;
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

  FFullFileName := aUrl;
  Ffilename := ExtractFileName(aUrl);
  FFileType := ExtractFileExt(filename);

end;

procedure TAiMediaFile.SaveToFile(aFileName: String);
begin
  FContent.SaveToFile(aFileName);
end;

procedure TAiMediaFile.SetBase64(const Value: String);
begin
  LoadFromBase64('', Value);
end;

procedure TAiMediaFile.SetCacheName(const Value: String);
begin
  FCacheName := Value;
end;

procedure TAiMediaFile.SetDetail(const Value: String);
begin
  FDetail := Value;
end;

procedure TAiMediaFile.Setfilename(const Value: String);
begin
  Ffilename := Value;
end;

procedure TAiMediaFile.SetFullFileName(const Value: String);
begin
  FFullFileName := Value;
end;

procedure TAiMediaFile.SetIdAudio(const Value: String);
begin
  FIdAudio := Value;
end;

procedure TAiMediaFile.SetIdFile(const Value: String);
begin
  FIdFile := Value;
end;

procedure TAiMediaFile.SetMediaFiles(const Value: TAiMediaFiles);
begin
  FMediaFiles := Value;
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

function TAiMediaFile.ToJsonObject: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('filename', Self.filename);
  Result.AddPair('urlMedia', Self.UrlMedia);
  Result.AddPair('fullFileName', Self.FullFileName);
  Result.AddPair('transcription', Self.Transcription);
  Result.AddPair('procesado', TJSONBool.Create(Self.Procesado));
  Result.AddPair('detail', Self.Detail);
  Result.AddPair('idAudio', Self.IdAudio);
  Result.AddPair('cloudState', Self.CloudState);
  Result.AddPair('cloudName', Self.CloudName);
  Result.AddPair('cacheName', Self.CacheName);
  Result.AddPair('idFile', Self.IdFile);
  Result.AddPair('base64', Self.Base64);
end;

function TAiMediaFile.ToString: String;
Var
  St: TStringStream;
begin
  St := TStringStream.Create;
  Try
    St.LoadFromStream(Self.Content);
    Result := St.DataString;
  Finally
    St.Free;
  End;
end;

{ TAiMediaFiles }

{
function TAiMediaFiles.GetMediaList(aFilters: TAiFileCategories; aProcesado: Boolean = False): TAiMediaFilesArray;
var
  i: Integer;
  Item: TAiMediaFile;
  Len: Integer;
begin
  SetLength(Result, 0); // Inicializamos el resultado para evitar basura
  for i := 0 to Self.Count - 1 do
  begin
    Item := Self.Items[i];
    if (Item.FileCategory in aFilters) and (Item.Procesado = aProcesado) then
    begin
      Len := Length(Result);
      SetLength(Result, Len + 1);
      Result[Len] := Item;
    end;
  end;
end;
}


function TAiMediaFiles.GetMediaList(aFilters: TAiFileCategories; aProcesado: Boolean = False): TAiMediaFilesArray;
var
  i: Integer;
  Item: TAiMediaFile;
  Len: Integer;
  IncludeAll: Boolean;
begin
  SetLength(Result, 0);
  IncludeAll := Tfc_Any in aFilters; // Verificar si debe incluir todos los tipos

  for i := 0 to Self.Count - 1 do
  begin
    Item := Self.Items[i];
    // Si IncludeAll es True, solo verifica aProcesado; si no, aplica ambos filtros
    if (IncludeAll or (Item.FileCategory in aFilters)) and (Item.Procesado = aProcesado) then
    begin
      Len := Length(Result);
      SetLength(Result, Len + 1);
      Result[Len] := Item;
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
  FunName: String;
begin
  JFunc := JObj.GetValue<TJSONObject>('function');
  FunName := JFunc.GetValue<string>('name');

  Begin
    Name := JFunc.GetValue<String>('name');
    Self.Description := JFunc.GetValue<String>('description');
    &Function := JFunc.Format;
    Body := JObj; // La funcion original completa
  End;
end;

function TAiMediaFiles.ToMediaFileArray: TAiMediaFilesArray;
var
  i: Integer;
  Item, NewItem: TAiMediaFile;
  Len: Integer;
begin
  SetLength(Result, 0); // Inicializamos el resultado para evitar basura
  for i := 0 to Self.Count - 1 do
  begin
    Item := Self.Items[i];
    NewItem := TAiMediaFile.Create;
    NewItem.Assign(Item);

    Len := Length(Result);
    SetLength(Result, Len + 1);
    Result[Len] := NewItem;
  end;
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

    Result := Lista.text;

  Finally
    Lista.Free;
  End;
end;

function TAiMetadata.GetJSonText: String;
Var
  JObj: TJSONObject;
  Clave: String;
begin
  JObj := TJSONObject.Create;

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
    Lista.text := Value;
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
  JObj: TJSONObject;
  Pair: TJSONPair;
begin
  Self.Clear;
  JObj := TJSONObject(TJSONObject.ParseJSONValue(Value));
  try
    For Pair in JObj do
      Self.Add(Pair.JsonString.Value, Pair.JsonValue.Value)
  finally
    JObj.Free;
  end;
end;

function TAiMetadata.ToJSon: TJSONObject;
Var
  Clave: String;
begin
  Result := TJSONObject.Create;
  For Clave in Self.Keys do
    Result.AddPair(Clave, Self.Items[Clave]);
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

  For Clave in Self.Keys do // La clave es el nombre de la función
  Begin
    TObj := TJSONObject.Create;
    TObj.AddPair('tool_call_id', Self.Items[Clave].id);
    TObj.AddPair('output', Self.Items[Clave].Response);
    Result.Add(TObj);
  End;
end;

{ TAiWebSearch }

constructor TAiWebSearch.Create;
begin
  &type := '';
  text := '';
  annotations := TAiWebSearchArray.Create;
end;

end.
