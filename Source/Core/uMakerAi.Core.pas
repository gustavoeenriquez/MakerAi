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
// Adaptaciones principales respecto a la version Delphi:
//   - System.JSON       → fpjson + jsonparser
//   - System.NetEncoding → EncdDecd (Base64) + funcion URL encode propia
//   - System.Net.HttpClient → fphttpclient + opensslsockets
//   - IHTTPResponse     → interfaz propia TAiHttpResponse
//   - Directivas {$IF CompilerVersion} eliminadas, reemplazadas por {$IF FPC_FULLVERSION}

unit uMakerAi.Core;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  SysUtils, Classes, Generics.Collections,
  StrUtils,
  EncdDecd,       // Base64: EncodeBase64 / DecodeBase64
  fpjson,         // TJSONObject, TJSONArray, TJSONString, TJSONBoolean, etc.
  jsonparser,     // GetJSON()
  fphttpclient,   // TFPHTTPClient
  URIParser;      // TUri, para encoding de URLs

// ---------------------------------------------------------------------------
// Interfaz minima para representar una respuesta HTTP (sustituye IHTTPResponse)
// ---------------------------------------------------------------------------
type
  IAiHttpResponse = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    function GetStatusCode: Integer;
    function GetContentAsString: string;
    property StatusCode: Integer read GetStatusCode;
    property ContentAsString: string read GetContentAsString;
  end;

// ---------------------------------------------------------------------------
//  A. Capa de Archivos (Fisica) - tipos de datos binarios/fisicos
// ---------------------------------------------------------------------------
type
  TAiFileCategory = (
    Tfc_Text, Tfc_Image, Tfc_Audio, Tfc_Video, Tfc_Pdf,
    Tfc_Document, Tfc_CalcSheet, Tfc_Presentation, Tfc_CompressFile,
    Tfc_Web, Tfc_GraphicDesign, Tfc_ExtractTextFile,
    Tfc_Report,    // Reporte generado (PDF, HTML, XLSX) por herramienta externa
    Tfc_Any, Tfc_Unknown
  );

  TAiFileCategories = set of TAiFileCategory;

// ---------------------------------------------------------------------------
//  B. Capa de Habilidades (Logica) - capacidades intelectuales o herramientas
// ---------------------------------------------------------------------------
  TAiChatMediaSupport = (
    Tcm_Text, Tcm_Image, Tcm_Audio, Tcm_Video, Tcm_Pdf,
    Tcm_WebSearch, Tcm_CodeInterpreter, Tcm_Memory,
    Tcm_TextEditor, Tcm_ComputerUse, Tcm_Shell,
    Tcm_Reasoning,         // Capacidad de CoT (Chain of Thought)
    Tcm_ReportGeneration,  // Generacion de reportes
    Tcm_Any, Tcm_Unknown
  );

  TAiChatMediaSupports = set of TAiChatMediaSupport;

// ---------------------------------------------------------------------------
//  C. Capa Unificada de Capacidades (sistema v3.3)
// ---------------------------------------------------------------------------
  TAiCapability = (
    // Entrada / Comprension (cubierto por completions nativo)
    cap_Image,            // modelo entiende imagenes entrantes
    cap_Audio,            // modelo entiende/transcribe audio entrante
    cap_Video,            // modelo entiende video entrante
    cap_Pdf,              // modelo entiende PDFs entrantes
    cap_WebSearch,        // modelo puede buscar en la web
    cap_Reasoning,        // modelo tiene razonamiento extendido (CoT)
    cap_CodeInterpreter,  // modelo puede ejecutar codigo
    cap_Memory,           // modelo tiene memoria persistente
    cap_TextEditor,       // modelo puede editar archivos
    cap_ComputerUse,      // modelo puede controlar el ordenador
    cap_Shell,            // modelo puede ejecutar comandos shell
    // Salida / Generacion (gap -> activa ChatTool o endpoint dedicado)
    cap_GenImage,         // producir imagen como output
    cap_GenAudio,         // producir audio como output (TTS)
    cap_GenVideo,         // producir video como output
    cap_GenReport,        // producir reporte (PDF, HTML, XLSX)
    cap_ExtractCode       // post-procesar: extraer bloques de codigo de la respuesta
  );

  TAiCapabilities = set of TAiCapability;

// ---------------------------------------------------------------------------
//  Tipos de evento y enumeraciones generales
// ---------------------------------------------------------------------------

  // Evento de error — usa IAiHttpResponse en lugar de IHTTPResponse de Delphi
  TAiErrorEvent = procedure(Sender: TObject; const ErrorMsg: string;
                            AException: Exception;
                            const AResponse: IAiHttpResponse) of object;

  TAiThinkingLevel = (tlDefault, tlLow, tlMedium, tlHigh);
  TAiMediaResolution = (mrDefault, mrLow, mrMedium, mrHigh);

  // Formatos de respuesta para transcripcion (Whisper)
  TAiTranscriptionResponseFormat = (trfText, trfJson, trfSrt, trfVtt, trfVerboseJson);

  // Granularidad de timestamps
  TAiTimestampGranularity = (tsgNone, tsgWord, tsgSegment);
  TAiTimestampGranularities = set of TAiTimestampGranularity;

  // Eventos MCP y herramientas
  TMCPLogEvent    = procedure(Sender: TObject; const Msg: string) of object;
  TMCPStatusEvent = procedure(Sender: TObject; const StatusMsg: string) of object;

  TToolFormat        = (tfUnknown, tfOpenAI, tfOpenAIResponses, tfClaude, tfGemini, tfMCP);
  TToolTransportType = (tpStdIo, tpHttp, tpSSE, tpMakerAi);

  // Maquina de estados del chat
  TAiChatState = (
    acsIdle,          // Inactivo
    acsConnecting,    // Conectando / Enviando Request
    acsCreated,       // Servidor acepto (Recibido ID)
    acsReasoning,     // Pensando / Razonando (Chain of Thought)
    acsWriting,       // Escribiendo respuesta visible
    acsToolCalling,   // El modelo pide usar una herramienta
    acsToolExecuting, // Ejecutando la herramienta (Local o Remota)
    acsFinished,      // Completado con exito
    acsAborted,       // Abortado por el usuario
    acsLoading,
    acsProcessing,
    acsError          // Error
  );

  TAiStateChangeEvent = procedure(Sender: TObject; State: TAiChatState;
                                  const Description: string) of object;

// ---------------------------------------------------------------------------
//  Forward declarations
// ---------------------------------------------------------------------------
  TAiMediaFiles = class;

// ---------------------------------------------------------------------------
//  TAiMediaFile — manejo de archivos de medios (audio, imagen, pdf, texto, etc.)
// ---------------------------------------------------------------------------
  TAiMediaFile = class
  private
    Ffilename       : string;
    FUrlMedia       : string;
    FFileType       : string;
    FContent        : TMemoryStream;
    FFullFileName   : string;
    FTranscription  : string;
    FProcesado      : Boolean;
    FDetail         : string;
    FIdAudio        : string;
    FCloudState     : string;
    FCloudName      : string;
    FCacheName      : string;
    FIdFile         : string;
    FMediaFiles     : TAiMediaFiles;
    FCacheControl   : Boolean;
    FEnableCitations: Boolean;
    FContext        : string;
    FTitle          : string;
    FContentLoaded  : Boolean;

    function  GetBase64: string;
    procedure SetBase64(const Value: string);
    procedure SetFilename(const Value: string);
    procedure SetUrlMedia(const Value: string);
    function  GetBytes: Integer;
    procedure SetFullFileName(const Value: string);
    function  GetMimeType: string;
    function  GetFileCategory: TAiFileCategory;
    procedure SetTranscription(const Value: string);
    procedure SetProcesado(const Value: Boolean);
    procedure SetDetail(const Value: string);
    procedure SetIdAudio(const Value: string);
    procedure SetCacheName(const Value: string);
    procedure SetIdFile(const Value: string);
    procedure SetMediaFiles(const Value: TAiMediaFiles);
  protected
    procedure DownloadFileFromUrl(Url: string); virtual;
    function  GetContent: TMemoryStream; virtual;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure LoadFromFile(aFileName: string); virtual;
    procedure LoadFromUrl(aUrl: string); virtual;
    procedure LoadFromBase64(aFileName, aBase64: string); virtual;
    procedure LoadFromStream(aFileName: string; Stream: TMemoryStream); virtual;
    procedure SaveToFile(aFileName: string); virtual;
    function  ToString: string; override;
    procedure Clear; virtual;

    function  ToJsonObject: TJSONObject;
    procedure LoadFromJsonObject(AObject: TJSONObject);
    procedure Assign(Source: TAiMediaFile);

    property Filename        : string        read Ffilename      write SetFilename;
    property Bytes           : Integer       read GetBytes;
    property Content         : TMemoryStream read GetContent;
    property FileCategory    : TAiFileCategory read GetFileCategory;
    property UrlMedia        : string        read FUrlMedia      write SetUrlMedia;
    property CloudState      : string        read FCloudState    write FCloudState;
    property CloudName       : string        read FCloudName     write FCloudName;
    property CacheName       : string        read FCacheName     write SetCacheName;
    property IdFile          : string        read FIdFile        write SetIdFile;
    property IdAudio         : string        read FIdAudio       write SetIdAudio;
    property Base64          : string        read GetBase64      write SetBase64;
    property FullFileName    : string        read FFullFileName  write SetFullFileName;
    property MimeType        : string        read GetMimeType;
    property Detail          : string        read FDetail        write SetDetail;
    property Transcription   : string        read FTranscription write SetTranscription;
    property Procesado       : Boolean       read FProcesado     write SetProcesado;
    property MediaFiles      : TAiMediaFiles read FMediaFiles    write SetMediaFiles;
    property CacheControl    : Boolean       read FCacheControl  write FCacheControl;
    property Title           : string        read FTitle         write FTitle;
    property Context         : string        read FContext       write FContext;
    property EnableCitations : Boolean       read FEnableCitations write FEnableCitations;
  end;

// ---------------------------------------------------------------------------
//  TAiMediaFiles — coleccion de TAiMediaFile
// ---------------------------------------------------------------------------
  TAiMediaFilesArray = array of TAiMediaFile;

  TAiMediaFiles = class(specialize TObjectList<TAiMediaFile>)
  public
    function GetMediaList(aFilters: TAiFileCategories;
                          aProcesado: Boolean = False): TAiMediaFilesArray;
    function ToMediaFileArray: TAiMediaFilesArray;
  end;

// ---------------------------------------------------------------------------
//  TAiMetadata — diccionario String→String de metadatos
// ---------------------------------------------------------------------------
  TAiMetadata = class(specialize TDictionary<string, string>)
  private
    function  GetAsText: string;
    procedure SetAsText(const Value: string);
    function  GetJsonText: string;
    procedure SetJsonText(const Value: string);
  public
    function ToJson: TJSONObject;
    property AsText  : string read GetAsText   write SetAsText;
    property JsonText: string read GetJsonText write SetJsonText;
  end;

// ---------------------------------------------------------------------------
//  TAiWebSearch — resultado de busqueda web (anotaciones)
// ---------------------------------------------------------------------------
  TAiWebSearchItem = class
  public
    &type       : string;
    start_index : Integer;
    end_index   : Integer;
    Url         : string;
    Title       : string;
  end;

  TAiWebSearchArray = class(specialize TObjectList<TAiWebSearchItem>);

  TAiWebSearch = class
  public
    &type      : string;
    text       : string;
    annotations: TAiWebSearchArray;
    constructor Create;
    destructor  Destroy; override;
  end;

// ---------------------------------------------------------------------------
//  Funciones utilitarias globales
// ---------------------------------------------------------------------------

// Devuelve la categoria TAiFileCategory segun la extension del archivo
function GetContentCategory(FileExtension: string): TAiFileCategory;

// Devuelve el MIME type segun la extension (.mp3 o mp3)
function GetMimeTypeFromFileName(FileExtension: string): string;
function GetFileExtensionFromMimeType(MimeType: string): string;

// Convierte un TMemoryStream a string Base64
function StreamToBase64(Stream: TMemoryStream): string;

// Convierte lista Key=Value en query string de URL (?k=v&k2=v2)
function GetParametrosURL(Parametros: TStringList): string;

// Codifica un string para uso en URL (reemplaza caracteres especiales)
function UrlEncode(const S: string): string;

implementation

{$I uMakerAi.Version.inc}

// ===========================================================================
//  Utilidades globales
// ===========================================================================

function UrlEncode(const S: string): string;
// Implementacion propia — en FPC no hay TNetEncoding.URL
// Codifica todos los caracteres que no sean letras, digitos o -_.~
const
  SafeChars = ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.', '~'];
var
  i: Integer;
  C: Char;
begin
  Result := '';
  for i := 1 to Length(S) do
  begin
    C := S[i];
    if C in SafeChars then
      Result := Result + C
    else
      Result := Result + '%' + IntToHex(Ord(C), 2);
  end;
end;

function GetParametrosURL(Parametros: TStringList): string;
var
  i: Integer;
begin
  Result := '';
  if not Assigned(Parametros) or (Parametros.Count = 0) then
    Exit;
  Result := '?';
  for i := 0 to Parametros.Count - 1 do
  begin
    Result := Result + UrlEncode(Parametros.Names[i]) + '=' +
              UrlEncode(Parametros.ValueFromIndex[i]);
    if i < Parametros.Count - 1 then
      Result := Result + '&';
  end;
end;

function StreamToBase64(Stream: TMemoryStream): string;
// FPC: EncdDecd.EncodeBase64 — recibe puntero y tamanio
begin
  Stream.Position := 0;
  Result := EncodeBase64(Stream.Memory, Stream.Size);
  // Eliminar saltos de linea que puede introducir EncodeBase64
  Result := StringReplace(Result, LineEnding, '', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '', [rfReplaceAll]);
end;

function GetMimeTypeFromFileName(FileExtension: string): string;
begin
  FileExtension := LowerCase(Trim(StringReplace(FileExtension, '.', '', [rfReplaceAll])));

  // Audio
  if      SameText(FileExtension, 'mp3')  then Result := 'audio/mpeg'
  else if SameText(FileExtension, 'mpga') then Result := 'audio/mpeg'
  else if SameText(FileExtension, 'm4a')  then Result := 'audio/mp4'
  else if SameText(FileExtension, 'ogg')  then Result := 'audio/ogg'
  else if SameText(FileExtension, 'wav')  then Result := 'audio/wav'
  else if SameText(FileExtension, 'flac') then Result := 'audio/flac'
  else if SameText(FileExtension, 'aac')  then Result := 'audio/aac'
  else if SameText(FileExtension, 'wma')  then Result := 'audio/x-ms-wma'
  else if SameText(FileExtension, 'opus') then Result := 'audio/opus'
  // Video
  else if SameText(FileExtension, 'mp4')  then Result := 'video/mp4'
  else if SameText(FileExtension, 'mpeg') then Result := 'video/mpeg'
  else if SameText(FileExtension, 'mpg')  then Result := 'video/mpeg'
  else if SameText(FileExtension, 'webm') then Result := 'video/webm'
  else if SameText(FileExtension, 'avi')  then Result := 'video/x-msvideo'
  else if SameText(FileExtension, 'mov')  then Result := 'video/quicktime'
  else if SameText(FileExtension, 'wmv')  then Result := 'video/x-ms-wmv'
  else if SameText(FileExtension, 'flv')  then Result := 'video/x-flv'
  else if SameText(FileExtension, '3gp')  then Result := 'video/3gpp'
  else if SameText(FileExtension, 'mkv')  then Result := 'video/x-matroska'
  else if SameText(FileExtension, 'm4v')  then Result := 'video/x-m4v'
  // Imagen
  else if SameText(FileExtension, 'gif')  then Result := 'image/gif'
  else if SameText(FileExtension, 'jpeg') then Result := 'image/jpeg'
  else if SameText(FileExtension, 'jpg')  then Result := 'image/jpeg'
  else if SameText(FileExtension, 'png')  then Result := 'image/png'
  else if SameText(FileExtension, 'bmp')  then Result := 'image/bmp'
  else if SameText(FileExtension, 'svg')  then Result := 'image/svg+xml'
  else if SameText(FileExtension, 'ico')  then Result := 'image/vnd.microsoft.icon'
  else if SameText(FileExtension, 'tiff') then Result := 'image/tiff'
  else if SameText(FileExtension, 'tif')  then Result := 'image/tiff'
  else if SameText(FileExtension, 'webp') then Result := 'image/webp'
  else if SameText(FileExtension, 'avif') then Result := 'image/avif'
  else if SameText(FileExtension, 'heic') then Result := 'image/heic'
  else if SameText(FileExtension, 'heif') then Result := 'image/heif'
  // Texto
  else if SameText(FileExtension, 'txt')  then Result := 'text/plain'
  else if SameText(FileExtension, 'html') then Result := 'text/html'
  else if SameText(FileExtension, 'htm')  then Result := 'text/html'
  else if SameText(FileExtension, 'css')  then Result := 'text/css'
  else if SameText(FileExtension, 'csv')  then Result := 'text/csv'
  else if SameText(FileExtension, 'js')   then Result := 'text/javascript'
  else if SameText(FileExtension, 'md')   then Result := 'text/markdown'
  else if SameText(FileExtension, 'rtf')  then Result := 'application/rtf'
  // Application
  else if SameText(FileExtension, 'xml')  then Result := 'application/xml'
  else if SameText(FileExtension, 'json') then Result := 'application/json'
  else if SameText(FileExtension, 'pdf')  then Result := 'application/pdf'
  // Office
  else if SameText(FileExtension, 'doc')  then Result := 'application/msword'
  else if SameText(FileExtension, 'docx') then Result := 'application/vnd.openxmlformats-officedocument.wordprocessingml.document'
  else if SameText(FileExtension, 'xls')  then Result := 'application/vnd.ms-excel'
  else if SameText(FileExtension, 'xlsx') then Result := 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
  else if SameText(FileExtension, 'ppt')  then Result := 'application/vnd.ms-powerpoint'
  else if SameText(FileExtension, 'pptx') then Result := 'application/vnd.openxmlformats-officedocument.presentationml.presentation'
  // OpenDocument
  else if SameText(FileExtension, 'odt')  then Result := 'application/vnd.oasis.opendocument.text'
  else if SameText(FileExtension, 'ods')  then Result := 'application/vnd.oasis.opendocument.spreadsheet'
  else if SameText(FileExtension, 'odp')  then Result := 'application/vnd.oasis.opendocument.presentation'
  // Comprimidos
  else if SameText(FileExtension, 'zip')  then Result := 'application/zip'
  else if SameText(FileExtension, 'gz')   then Result := 'application/gzip'
  else if SameText(FileExtension, 'tar')  then Result := 'application/x-tar'
  else if SameText(FileExtension, 'rar')  then Result := 'application/vnd.rar'
  else if SameText(FileExtension, '7z')   then Result := 'application/x-7z-compressed'
  else if SameText(FileExtension, 'bz2')  then Result := 'application/x-bzip2'
  // Programacion
  else if SameText(FileExtension, 'py')   then Result := 'text/x-python'
  else if SameText(FileExtension, 'java') then Result := 'text/x-java-source'
  else if SameText(FileExtension, 'c')    then Result := 'text/x-c'
  else if SameText(FileExtension, 'cpp')  then Result := 'text/x-c++'
  else if SameText(FileExtension, 'cs')   then Result := 'text/x-csharp'
  else if SameText(FileExtension, 'rb')   then Result := 'text/x-ruby'
  else if SameText(FileExtension, 'go')   then Result := 'text/x-go'
  else if SameText(FileExtension, 'rs')   then Result := 'text/x-rust'
  else if SameText(FileExtension, 'sh')   then Result := 'application/x-sh'
  else if SameText(FileExtension, 'bat')  then Result := 'application/x-msdos-program'
  // Disenio grafico
  else if SameText(FileExtension, 'psd')  then Result := 'application/vnd.adobe.photoshop'
  else if SameText(FileExtension, 'ai')   then Result := 'application/postscript'
  else if SameText(FileExtension, 'eps')  then Result := 'application/postscript'
  else if SameText(FileExtension, 'ps')   then Result := 'application/postscript'
  // Fuentes
  else if SameText(FileExtension, 'ttf')  then Result := 'font/ttf'
  else if SameText(FileExtension, 'otf')  then Result := 'font/otf'
  else if SameText(FileExtension, 'woff') then Result := 'font/woff'
  else if SameText(FileExtension, 'woff2')then Result := 'font/woff2'
  // Bases de datos
  else if SameText(FileExtension, 'sqlite')then Result := 'application/vnd.sqlite3'
  else if SameText(FileExtension, 'db')   then Result := 'application/x-sqlite3'
  // eBooks
  else if SameText(FileExtension, 'epub') then Result := 'application/epub+zip'
  else if SameText(FileExtension, 'mobi') then Result := 'application/x-mobipocket-ebook'
  else
    Result := 'application/octet-stream';
end;

function GetFileExtensionFromMimeType(MimeType: string): string;
begin
  MimeType := LowerCase(Trim(MimeType));

  if      SameText(MimeType, 'audio/mpeg')        then Result := 'mp3'
  else if SameText(MimeType, 'audio/mp4')         then Result := 'm4a'
  else if SameText(MimeType, 'audio/ogg')         then Result := 'ogg'
  else if SameText(MimeType, 'audio/wav')         then Result := 'wav'
  else if SameText(MimeType, 'audio/flac')        then Result := 'flac'
  else if SameText(MimeType, 'audio/aac')         then Result := 'aac'
  else if SameText(MimeType, 'video/mp4')         then Result := 'mp4'
  else if SameText(MimeType, 'video/mpeg')        then Result := 'mpeg'
  else if SameText(MimeType, 'video/webm')        then Result := 'webm'
  else if SameText(MimeType, 'video/x-msvideo')   then Result := 'avi'
  else if SameText(MimeType, 'video/quicktime')   then Result := 'mov'
  else if SameText(MimeType, 'video/x-ms-wmv')    then Result := 'wmv'
  else if SameText(MimeType, 'video/3gpp')        then Result := '3gp'
  else if SameText(MimeType, 'video/x-matroska')  then Result := 'mkv'
  else if SameText(MimeType, 'image/gif')         then Result := 'gif'
  else if SameText(MimeType, 'image/jpeg')        then Result := 'jpg'
  else if SameText(MimeType, 'image/png')         then Result := 'png'
  else if SameText(MimeType, 'image/bmp')         then Result := 'bmp'
  else if SameText(MimeType, 'image/svg+xml')     then Result := 'svg'
  else if SameText(MimeType, 'image/tiff')        then Result := 'tiff'
  else if SameText(MimeType, 'image/webp')        then Result := 'webp'
  else if SameText(MimeType, 'image/avif')        then Result := 'avif'
  else if SameText(MimeType, 'image/heic')        then Result := 'heic'
  else if SameText(MimeType, 'image/heif')        then Result := 'heif'
  else if SameText(MimeType, 'application/pdf')   then Result := 'pdf'
  else if SameText(MimeType, 'application/msword')then Result := 'doc'
  else if SameText(MimeType, 'application/vnd.openxmlformats-officedocument.wordprocessingml.document') then Result := 'docx'
  else if SameText(MimeType, 'application/vnd.ms-excel') then Result := 'xls'
  else if SameText(MimeType, 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet') then Result := 'xlsx'
  else if SameText(MimeType, 'application/vnd.ms-powerpoint') then Result := 'ppt'
  else if SameText(MimeType, 'application/vnd.openxmlformats-officedocument.presentationml.presentation') then Result := 'pptx'
  else if SameText(MimeType, 'application/vnd.oasis.opendocument.text') then Result := 'odt'
  else if SameText(MimeType, 'application/vnd.oasis.opendocument.spreadsheet') then Result := 'ods'
  else if SameText(MimeType, 'application/rtf')   then Result := 'rtf'
  else if SameText(MimeType, 'application/zip')   then Result := 'zip'
  else if SameText(MimeType, 'application/gzip')  then Result := 'gz'
  else if SameText(MimeType, 'application/x-tar') then Result := 'tar'
  else if SameText(MimeType, 'application/vnd.rar')then Result := 'rar'
  else if SameText(MimeType, 'application/x-7z-compressed') then Result := '7z'
  else if SameText(MimeType, 'application/json')  then Result := 'json'
  else if SameText(MimeType, 'application/xml')   then Result := 'xml'
  else if SameText(MimeType, 'text/plain')        then Result := 'txt'
  else if SameText(MimeType, 'text/csv')          then Result := 'csv'
  else if SameText(MimeType, 'text/html')         then Result := 'html'
  else if SameText(MimeType, 'text/css')          then Result := 'css'
  else if SameText(MimeType, 'text/markdown')     then Result := 'md'
  else if SameText(MimeType, 'text/javascript')   then Result := 'js'
  else if SameText(MimeType, 'text/x-python')     then Result := 'py'
  else if SameText(MimeType, 'font/ttf')          then Result := 'ttf'
  else if SameText(MimeType, 'font/otf')          then Result := 'otf'
  else if SameText(MimeType, 'font/woff')         then Result := 'woff'
  else if SameText(MimeType, 'font/woff2')        then Result := 'woff2'
  else if SameText(MimeType, 'application/vnd.sqlite3') then Result := 'sqlite'
  else if SameText(MimeType, 'application/epub+zip') then Result := 'epub'
  else if SameText(MimeType, 'application/octet-stream') then Result := 'bin'
  else
    Result := 'bin';

  Result := '.' + Result;
end;

function GetContentCategory(FileExtension: string): TAiFileCategory;
begin
  FileExtension := LowerCase(Trim(StringReplace(FileExtension, '.', '', [rfReplaceAll])));

  // Imagenes
  if MatchStr(FileExtension, ['jpg','jpeg','png','gif','bmp','tiff','tif','svg','webp','avif','heic','heif','ico']) then
    Result := Tfc_Image
  // Audio
  else if MatchStr(FileExtension, ['mp3','wav','flac','aac','ogg','wma','m4a','opus','mpga']) then
    Result := Tfc_Audio
  // Video
  else if MatchStr(FileExtension, ['avi','mp4','mkv','mov','wmv','flv','webm','mpeg','mpg','3gp','m4v']) then
    Result := Tfc_Video
  // PDF
  else if FileExtension = 'pdf' then
    Result := Tfc_Pdf
  // Documentos
  else if MatchStr(FileExtension, ['doc','docx','odt','rtf','tex']) then
    Result := Tfc_Document
  // Hojas de calculo
  else if MatchStr(FileExtension, ['xls','xlsx','ods','csv']) then
    Result := Tfc_CalcSheet
  // Presentaciones
  else if MatchStr(FileExtension, ['ppt','pptx','odp']) then
    Result := Tfc_Presentation
  // Texto plano
  else if MatchStr(FileExtension, ['txt','md','log','readme']) then
    Result := Tfc_Text
  // Web
  else if MatchStr(FileExtension, ['html','htm','xml','json','css','js','jsx','ts','tsx','vue','php']) then
    Result := Tfc_Web
  // Comprimidos
  else if MatchStr(FileExtension, ['zip','rar','tar','gz','bz2','7z','xz','gzip']) then
    Result := Tfc_CompressFile
  // Disenio grafico
  else if MatchStr(FileExtension, ['psd','ai','eps','indd','sketch','fig','xd']) then
    Result := Tfc_GraphicDesign
  // Archivos de configuracion / extraibles
  else if MatchStr(FileExtension, ['yaml','yml','toml','ini','cfg','conf']) then
    Result := Tfc_ExtractTextFile
  else
    Result := Tfc_Unknown;
end;

// ===========================================================================
//  TAiMediaFile
// ===========================================================================

constructor TAiMediaFile.Create;
begin
  inherited Create;
  FContent      := TMemoryStream.Create;
  FMediaFiles   := TAiMediaFiles.Create;
  FProcesado    := False;
  FContentLoaded:= False;
  FDetail       := '';
end;

destructor TAiMediaFile.Destroy;
begin
  FContent.Free;
  FMediaFiles.Clear;
  FMediaFiles.Free;
  inherited Destroy;
end;

procedure TAiMediaFile.Clear;
begin
  FContent.Clear;
  FContentLoaded  := False;
  Ffilename       := '';
  FUrlMedia       := '';
  FFileType       := '';
  FFullFileName   := '';
  FTranscription  := '';
  FProcesado      := False;
  FDetail         := '';
  FIdAudio        := '';
  FCloudState     := '';
  FCloudName      := '';
  FCacheName      := '';
  FIdFile         := '';
  FCacheControl   := False;
  FEnableCitations:= False;
  FContext        := '';
  FTitle          := '';
end;

procedure TAiMediaFile.Assign(Source: TAiMediaFile);
begin
  if (Source = nil) or (Source = Self) then
    Exit;

  Self.Ffilename        := Source.Ffilename;
  Self.FUrlMedia        := Source.FUrlMedia;
  Self.FFileType        := Source.FFileType;
  Self.FFullFileName    := Source.FFullFileName;
  Self.FTranscription   := Source.FTranscription;
  Self.FProcesado       := Source.FProcesado;
  Self.FDetail          := Source.FDetail;
  Self.FIdAudio         := Source.FIdAudio;
  Self.FCloudState      := Source.FCloudState;
  Self.FCloudName       := Source.FCloudName;
  Self.FCacheName       := Source.FCacheName;
  Self.FIdFile          := Source.FIdFile;
  Self.FCacheControl    := Source.FCacheControl;
  Self.FEnableCitations := Source.FEnableCitations;
  Self.FContext         := Source.FContext;
  Self.FTitle           := Source.FTitle;

  // Copia profunda del stream
  if Assigned(Source.Content) and (Source.Content.Size > 0) then
  begin
    if not Assigned(Self.FContent) then
      Self.FContent := TMemoryStream.Create;
    Self.FContent.Clear;
    Source.Content.Position := 0;
    Self.FContent.CopyFrom(Source.Content, 0);
    Self.FContent.Position := 0;
    Source.Content.Position := 0;
    Self.FContentLoaded := True;
  end
  else
  begin
    if Assigned(Self.FContent) then
      Self.FContent.Clear;
  end;
  // FMediaFiles no se copia (referencia al contenedor padre)
end;

procedure TAiMediaFile.DownloadFileFromUrl(Url: string);
var
  Client  : TFPHTTPClient;
  Response: TMemoryStream;
begin
  if Url = '' then
    Exit;

  Client   := TFPHTTPClient.Create(nil);
  Response := TMemoryStream.Create;
  try
    // TODO: manejar redirects y HTTPS (requiere opensslsockets)
    Client.Get(Url, Response);

    FContent.Clear;
    Response.Position := 0;
    FContent.CopyFrom(Response, 0);
    FContent.Position  := 0;
    FContentLoaded     := True;
  finally
    Client.Free;
    Response.Free;
  end;
end;

function TAiMediaFile.GetContent: TMemoryStream;
begin
  Result := FContent;
  if FContentLoaded then
    Exit;
  if FUrlMedia <> '' then
  begin
    DownloadFileFromUrl(FUrlMedia);
    FContentLoaded := True;
    Result := FContent;
  end;
end;

function TAiMediaFile.GetBase64: string;
begin
  FContent.Position := 0;
  Result := EncodeBase64(FContent.Memory, FContent.Size);
  Result := StringReplace(Result, LineEnding, '', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '', [rfReplaceAll]);
end;

procedure TAiMediaFile.SetBase64(const Value: string);
begin
  LoadFromBase64('', Value);
end;

function TAiMediaFile.GetBytes: Integer;
begin
  Result := FContent.Size;
end;

function TAiMediaFile.GetFileCategory: TAiFileCategory;
begin
  if Trim(Ffilename) = '' then
    Result := Tfc_Unknown
  else
    Result := GetContentCategory(ExtractFileExt(LowerCase(Ffilename)));
end;

function TAiMediaFile.GetMimeType: string;
begin
  Result := GetMimeTypeFromFileName(LowerCase(ExtractFileExt(Ffilename)));
end;

procedure TAiMediaFile.LoadFromFile(aFileName: string);
begin
  if FileExists(aFileName) then
  begin
    FContent.Clear;
    FContent.LoadFromFile(aFileName);
    FContentLoaded := True;
    FFullFileName  := aFileName;
    Ffilename      := ExtractFileName(aFileName);
    FFileType      := LowerCase(ExtractFileExt(Ffilename));
  end;
end;

procedure TAiMediaFile.LoadFromUrl(aUrl: string);
var
  LUrl: string;
  LPos: Integer;
begin
  FUrlMedia := aUrl;
  FContent.Clear;
  GetContent;

  LUrl := aUrl;
  LPos := Pos('?', LUrl);
  if LPos > 0 then
    LUrl := Copy(LUrl, 1, LPos - 1);
  LPos := Pos('#', LUrl);
  if LPos > 0 then
    LUrl := Copy(LUrl, 1, LPos - 1);

  FFullFileName := aUrl;
  Ffilename     := ExtractFileName(LUrl);
  FFileType     := ExtractFileExt(Ffilename);
end;

procedure TAiMediaFile.LoadFromBase64(aFileName, aBase64: string);
var
  Decoded: TBytes;
  St     : TBytesStream;
begin
  // FPC: DecodeBase64 devuelve un string con bytes crudos
  // Usamos TBytesStream para wrapearlo correctamente
  Decoded := DecodeBase64(aBase64);
  St := TBytesStream.Create(Decoded);
  try
    FContent.Clear;
    St.Position := 0;
    FContent.CopyFrom(St, 0);
    FContentLoaded := True;
    FFullFileName  := aFileName;
    Ffilename      := ExtractFileName(aFileName);
    FFileType      := ExtractFileExt(Ffilename);
  finally
    St.Free;
  end;
end;

procedure TAiMediaFile.LoadFromStream(aFileName: string; Stream: TMemoryStream);
begin
  if Assigned(Stream) then
  begin
    FContent.Clear;
    Stream.Position := 0;
    FContent.CopyFrom(Stream, 0);
    FContentLoaded := True;
    FFullFileName  := aFileName;
    Ffilename      := ExtractFileName(aFileName);
    FFileType      := LowerCase(ExtractFileExt(Ffilename));
  end;
end;

procedure TAiMediaFile.SaveToFile(aFileName: string);
begin
  FContent.SaveToFile(aFileName);
end;

function TAiMediaFile.ToString: string;
var
  St: TStringStream;
begin
  St := TStringStream.Create('');
  try
    FContent.Position := 0;
    St.CopyFrom(FContent, 0);
    Result := St.DataString;
  finally
    St.Free;
  end;
end;

function TAiMediaFile.ToJsonObject: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('filename',      Ffilename);
  Result.Add('urlMedia',      FUrlMedia);
  Result.Add('fullFileName',  FFullFileName);
  Result.Add('transcription', FTranscription);
  Result.Add('procesado',     FProcesado);
  Result.Add('detail',        FDetail);
  Result.Add('idAudio',       FIdAudio);
  Result.Add('cloudState',    FCloudState);
  Result.Add('cloudName',     FCloudName);
  Result.Add('cacheName',     FCacheName);
  Result.Add('idFile',        FIdFile);
  Result.Add('base64',        GetBase64);
end;

procedure TAiMediaFile.LoadFromJsonObject(AObject: TJSONObject);
var
  LBase64  : string;
  LFilename: string;
begin
  Clear;

  // fpjson: usar Get() con valor por defecto o TryGet cuando disponible
  if AObject.IndexOfName('filename')     >= 0 then Ffilename      := AObject.Get('filename',     '');
  if AObject.IndexOfName('urlMedia')     >= 0 then FUrlMedia      := AObject.Get('urlMedia',     '');
  if AObject.IndexOfName('fullFileName') >= 0 then FFullFileName  := AObject.Get('fullFileName', '');
  if AObject.IndexOfName('transcription')>= 0 then FTranscription := AObject.Get('transcription','');
  if AObject.IndexOfName('procesado')    >= 0 then FProcesado     := AObject.Get('procesado',    False);
  if AObject.IndexOfName('detail')       >= 0 then FDetail        := AObject.Get('detail',       '');
  if AObject.IndexOfName('idAudio')      >= 0 then FIdAudio       := AObject.Get('idAudio',      '');
  if AObject.IndexOfName('cloudState')   >= 0 then FCloudState    := AObject.Get('cloudState',   '');
  if AObject.IndexOfName('cloudName')    >= 0 then FCloudName     := AObject.Get('cloudName',    '');
  if AObject.IndexOfName('cacheName')    >= 0 then FCacheName     := AObject.Get('cacheName',    '');
  if AObject.IndexOfName('idFile')       >= 0 then FIdFile        := AObject.Get('idFile',       '');

  LBase64 := AObject.Get('base64', '');
  if LBase64 <> '' then
  begin
    LFilename := AObject.Get('filename', '');
    LoadFromBase64(LFilename, LBase64);
  end;
end;

// Setters triviales
procedure TAiMediaFile.SetFilename(const Value: string);      begin Ffilename      := Value; end;
procedure TAiMediaFile.SetUrlMedia(const Value: string);      begin FUrlMedia      := Value; end;
procedure TAiMediaFile.SetFullFileName(const Value: string);  begin FFullFileName  := Value; end;
procedure TAiMediaFile.SetTranscription(const Value: string); begin FTranscription := Value; end;
procedure TAiMediaFile.SetProcesado(const Value: Boolean);    begin FProcesado     := Value; end;
procedure TAiMediaFile.SetDetail(const Value: string);        begin FDetail        := Value; end;
procedure TAiMediaFile.SetIdAudio(const Value: string);       begin FIdAudio       := Value; end;
procedure TAiMediaFile.SetCacheName(const Value: string);     begin FCacheName     := Value; end;
procedure TAiMediaFile.SetIdFile(const Value: string);        begin FIdFile        := Value; end;
procedure TAiMediaFile.SetMediaFiles(const Value: TAiMediaFiles); begin FMediaFiles := Value; end;

// ===========================================================================
//  TAiMediaFiles
// ===========================================================================

function TAiMediaFiles.GetMediaList(aFilters: TAiFileCategories;
                                    aProcesado: Boolean): TAiMediaFilesArray;
var
  i         : Integer;
  Item      : TAiMediaFile;
  Len       : Integer;
  IncludeAll: Boolean;
begin
  SetLength(Result, 0);
  IncludeAll := Tfc_Any in aFilters;

  for i := 0 to Count - 1 do
  begin
    Item := Items[i];
    if (IncludeAll or (Item.FileCategory in aFilters)) and
       (Item.Procesado = aProcesado) then
    begin
      Len := Length(Result);
      SetLength(Result, Len + 1);
      Result[Len] := Item;
    end;
  end;
end;

function TAiMediaFiles.ToMediaFileArray: TAiMediaFilesArray;
var
  i      : Integer;
  Item   : TAiMediaFile;
  NewItem: TAiMediaFile;
  Len    : Integer;
begin
  SetLength(Result, 0);
  for i := 0 to Count - 1 do
  begin
    Item    := Items[i];
    NewItem := TAiMediaFile.Create;
    NewItem.Assign(Item);
    Len := Length(Result);
    SetLength(Result, Len + 1);
    Result[Len] := NewItem;
  end;
end;

// ===========================================================================
//  TAiMetadata
// ===========================================================================

function TAiMetadata.GetAsText: string;
var
  Lista: TStringList;
  Clave: string;
begin
  Lista := TStringList.Create;
  try
    for Clave in Keys do
      Lista.Values[Clave] := Items[Clave];
    Result := Lista.Text;
  finally
    Lista.Free;
  end;
end;

procedure TAiMetadata.SetAsText(const Value: string);
var
  Lista: TStringList;
  i    : Integer;
  Clave: string;
begin
  Lista := TStringList.Create;
  try
    Lista.Text := Value;
    Clear;
    for i := 0 to Lista.Count - 1 do
    begin
      Clave := Lista.Names[i];
      Add(Clave, Lista.Values[Clave]);
    end;
  finally
    Lista.Free;
  end;
end;

function TAiMetadata.GetJsonText: string;
var
  JObj : TJSONObject;
  Clave: string;
begin
  JObj := TJSONObject.Create;
  try
    for Clave in Keys do
      JObj.Add(Clave, Items[Clave]);
    Result := JObj.FormatJSON;
  finally
    JObj.Free;
  end;
end;

procedure TAiMetadata.SetJsonText(const Value: string);
var
  JVal: TJSONData;
  JObj: TJSONObject;
  i   : Integer;
begin
  Clear;
  JVal := GetJSON(Value);
  if not Assigned(JVal) then
    Exit;
  try
    if not (JVal is TJSONObject) then
      Exit;
    JObj := TJSONObject(JVal);
    for i := 0 to JObj.Count - 1 do
      Add(JObj.Names[i], JObj.Items[i].AsString);
  finally
    JVal.Free;
  end;
end;

function TAiMetadata.ToJson: TJSONObject;
var
  Clave: string;
begin
  Result := TJSONObject.Create;
  for Clave in Keys do
    Result.Add(Clave, Items[Clave]);
end;

// ===========================================================================
//  TAiWebSearch
// ===========================================================================

constructor TAiWebSearch.Create;
begin
  inherited Create;
  &type       := '';
  text        := '';
  annotations := TAiWebSearchArray.Create;
end;

destructor TAiWebSearch.Destroy;
begin
  annotations.Free;
  inherited Destroy;
end;

end.
