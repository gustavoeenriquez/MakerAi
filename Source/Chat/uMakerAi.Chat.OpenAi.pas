unit uMakerAi.Chat.OpenAi;

// MIT License
//
// Copyright (c) 2013 Gustavo Enríquez - CimaMaker
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
// - Telegram: +57 3128441700
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

// La documentación de esta librería se encuentra en
// https://platform.openai.com/docs/guides/migrate-to-responses

// no se ha implemtnado FileSearch, Retrieval, Mcp Remote ni DeepResearch directamente de la plataforma de openai
// https://platform.openai.com/docs/guides/tools-file-search

{$INCLUDE ../CompilerDirectives.inc}

interface

uses
  // FPC: Unidades estándar de FPC sin prefijo System
  {$IFDEF FPC}
  Classes, SysUtils, StrUtils, Generics.Collections,
  {$ELSE}
  // Delphi: Unidades con namespace System, incluye Net/HTTP/JSON/REST nativos
  System.SysUtils, System.Classes, System.Threading, System.Generics.Collections,
  System.NetEncoding, System.Net.URLClient, System.Net.HttpClient, System.StrUtils,
  System.Net.Mime, System.Net.HttpClientComponent, System.JSON, Rest.JSON,
  {$ENDIF}
  uJsonHelper, uHttpHelper, uSysUtilsHelper, uBase64Helper, uThreadingHelper,
  uMakerAi.ParamsRegistry, uMakerAi.Chat, uMakerAi.Core,
  uMakerAi.Embeddings, uMakerAi.Embeddings.Core,
  uMakerAi.Tools.Functions, uMakerAi.Utils.CodeExtractor, uMakerAi.Utils.System, uMakerAi.Chat.Messages;

type

  // Evento para manejar manualmente la herramienta apply_patch.
  // Parámetros:
  // - OperationType: 'create_file', 'update_file', 'delete_file'.
  // - Path: Ruta del archivo objetivo.
  // - Diff: El contenido del parche (V4A diff).
  // - CallId: ID único de la llamada.
  // - aStatus: Variable de retorno. Debe ser 'completed' o 'failed'.
  // - aOutput: Variable de retorno. Mensaje de éxito o descripción del error.
  TAiApplyPatchEvent = procedure(Sender: TObject; const OperationType, Path, Diff: string; const CallId: string; var aStatus, aOutput: string) of object;

  // Evento para manejar comandos de shell.
  // Si Handled es True, el componente asume que el usuario llenó StdOut/StdErr/ExitCode.
  // Si Handled es False, el componente intentará ejecutarlo automáticamente (si está configurado).
  TAiShellCommandEvent = procedure(Sender: TObject; const Command: string; const CallId: string; var StdOut, StdErr: string; var ExitCode: Integer; var Handled: Boolean) of object;

  TAiReasoningSummary = (rsmDefault, rsmAuto, rsmConcise, rsmDetailed);

  TAiOpenChat = class(TAiChat)
  private
    FStore: Boolean;
    FTruncation: String;
    FParallel_ToolCalls: Boolean;
    FVerbosity: String;

    // Estado interno del endpoint Responses
    FResponseId: String;
    FResponseStatus: String; // completed, failed, in_progress, incomplete
    FLastApiResponse: TJSonObject;
    FOnApplyPatch: TAiApplyPatchEvent;
    FOnShellCommand: TAiShellCommandEvent;
    FAllowAutoShell: Boolean;
    FReasoningSummary: TAiReasoningSummary;
    FRecursionNeeded: Boolean;

    procedure SetStore(const Value: Boolean);
    procedure SetTruncation(const Value: String);
    procedure SetParallel_ToolCalls(const Value: Boolean);
    procedure SetVerbosity(const Value: String);

  protected
    // Sobrescritura de métodos del Core para adaptar al nuevo API
    Procedure OnInternalReceiveData(const Sender: TObject; {$IFDEF FPC}const{$ENDIF} AContentLength, AReadCount: Int64; var AAbort: Boolean); Override;
    Procedure ParseChat(jObj: TJSonObject; ResMsg: TAiChatMessage); Override;
    Function InitChatCompletions: String; Override;
    Function GetTools(Funcion: TAiFunctions): TJSonArray; Reintroduce;

    // Métodos internos
    procedure UpdateResponseStatus(aStatus: String);
    procedure DoCallFunction(ToolCall: TAiToolsFunction); Override;

    // Métodos heredados que se mantienen igual o se adaptan ligeramente
    Function InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String; Override;
    function InternalRunSpeechGeneration(ResMsg, AskMsg: TAiChatMessage): String; Override;
    function InternalRunTranscription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage): String; Override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Factory
    class function GetDriverName: string; Override;
    class procedure RegisterDefaultParams(Params: TStrings); Override;
    class function CreateInstance(Sender: TComponent): TAiChat; Override;

    Function UploadFileToCache(aMediaFile: TAiMediaFile; aTTL_Seconds: Integer = 3600): String; Override;
    Function DeleteFile(aMediaFile: TAiMediaFile): String; Override;
    function DownLoadFile(aMediaFile: TAiMediaFile): String; Override;
    function DeleteAllUploadedFiles: Integer; // Borra todos los archivos subidos durante la conversación de lo contrario seguirán en el repositorio de OpenAi
    Procedure NewChat; Override;
    function InternalRunImageVideoGeneration(ResMsg, AskMsg: TAiChatMessage): String; Override;
    procedure AsyncImageGenWorker(const AUrl, AKey, AModel, APrompt: string; AVidParams: TStrings; AResMsg, AAskMsg: TAiChatMessage);

  published
    // Propiedades específicas mapeadas
    property Store: Boolean read FStore write SetStore default True;
    property Truncation: String read FTruncation write SetTruncation;
    property Parallel_ToolCalls: Boolean read FParallel_ToolCalls write SetParallel_ToolCalls default True;

    // Nuevas propiedades de GPT-5 / Responses API
    property Verbosity: String read FVerbosity write SetVerbosity; // low, medium, high
    property OnApplyPatch: TAiApplyPatchEvent read FOnApplyPatch write FOnApplyPatch;
    property OnShellCommand: TAiShellCommandEvent read FOnShellCommand write FOnShellCommand;
    property AllowAutoShell: Boolean read FAllowAutoShell write FAllowAutoShell default False;
    property ReasoningSummary: TAiReasoningSummary read FReasoningSummary write FReasoningSummary default rsmDefault;
  end;

  TAiOpenAiEmbeddings = class(TAiEmbeddings)
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
    Function CreateEmbedding(aInput, aUser: String; aDimensions: Integer = -1; aModel: String = ''; aEncodingFormat: String = 'float'): TAiEmbeddingData; Override;
    Procedure ParseEmbedding(jObj: TJSonObject); Override;
  end;

procedure Register;

implementation

uses
  {$IFDEF FPC}
  Generics.Defaults;  // FPC: Sin namespace System
  {$ELSE}
  System.Generics.Defaults;
  {$ENDIF}

Const
  GlOpenAIUrl = 'https://api.openai.com/v1/';

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiOpenChat, TAiOpenAiEmbeddings]);
end;

{ TAiOpenChat }

constructor TAiOpenChat.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Configuración por defecto
  FStore := True;
  FTruncation := 'disabled';
  FParallel_ToolCalls := True;
  FVerbosity := '';
  FResponseId := '';
  FResponseStatus := '';
  FLastApiResponse := Nil;

  // URL por defecto
  if Url = '' then
    Url := GlOpenAIUrl;
  if Model = '' then
    Model := 'gpt-5.1';

  // Configurar capacidades soportadas por este driver
  ChatMediaSupports := [Tcm_Text, Tcm_Image, Tcm_WebSearch, Tcm_CodeInterpreter, tcm_pdf];
  // Audio NO se incluye aquí para evitar que TAiChat intente procesarlo en el flujo principal
  // NativeInputFiles indica qué archivos se pueden enviar "crudos" en el input
  NativeInputFiles := [Tfc_Text, Tfc_Image, Tfc_pdf];
  // NativeOutputFiles indica qué puede generar el modelo
  NativeOutputFiles := [Tfc_Text, Tfc_Image];
  FReasoningSummary := rsmDefault;
end;

function TAiOpenChat.DeleteAllUploadedFiles: Integer;
var
  I, K: Integer;
  Msg: TAiChatMessage;
  Media: TAiMediaFile;
begin
  Result := 0;

  // Recorremos todos los mensajes del historial
  for I := 0 to FMessages.Count - 1 do
  begin
    Msg := FMessages[I];

    // Recorremos los archivos adjuntos de cada mensaje
    for K := 0 to Msg.MediaFiles.Count - 1 do
    begin
      Media := Msg.MediaFiles[K];

      // Solo intentamos borrar si tiene un ID de nube asignado
      if Media.IdFile <> '' then
      begin
        try
          // Llamamos a nuestra función DeleteFile
          // Nota: DeleteFile ya se encarga de limpiar la propiedad IdFile del objeto si tiene éxito
          if DeleteFile(Media) = 'deleted' then
            Inc(Result);
        except
          // Capturamos excepciones silenciosamente para no detener el proceso
          // Si un archivo falla, intentamos con el siguiente.
        end;
      end;
    end;
  end;
end;

function TAiOpenChat.DeleteFile(aMediaFile: TAiMediaFile): String;
var
  LUrl: string;
  LResponse: IHTTPResponse;
  LHeaders: TNetHeaders;
  LJson: TJSonObject;

begin
  Result := '';
  if (aMediaFile = nil) or (aMediaFile.IdFile = '') then
    Exit;

  // Endpoint: DELETE https://api.openai.com/v1/files/{file_id}
  LUrl := Url;
  if not LUrl.EndsWith('/') then
    LUrl := LUrl + '/';
  LUrl := LUrl + 'files/' + aMediaFile.IdFile;

  LHeaders := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];

  LJson := nil;
  try
    // Unified Implementation via uHttpHelper and uJsonHelper
    LResponse := FClient.Delete(LUrl, nil, LHeaders);
    if LResponse.StatusCode = 200 then
    begin
      LJson := TJSonObject.ParseJSONValue(LResponse.ContentAsString) as TJSonObject;
      try
        // GetValueAsBoolean is a universal helper method in uJsonHelper
        if Assigned(LJson) and LJson.GetValueAsBoolean('deleted') then
          Result := 'deleted'
        else
          Result := 'failed';
      finally
        // LJson freed in outer finally
      end;
    end
    else
      raise Exception.CreateFmt('Error Deleting File (%d): %s', [LResponse.StatusCode, LResponse.ContentAsString]);

    if Result = 'deleted' then
    begin
        // Limpiamos los datos de nube del objeto
        aMediaFile.IdFile := '';
        aMediaFile.CloudState := '';
        aMediaFile.CacheName := '';
    end;

  finally
    if Assigned(LJson) then
      LJson.Free;
  end;
end;

destructor TAiOpenChat.Destroy;
begin
  if Assigned(FLastApiResponse) then
    FLastApiResponse.Free;
  inherited;
end;

class function TAiOpenChat.GetDriverName: string;
begin
  Result := 'OpenAi';
end;

class procedure TAiOpenChat.RegisterDefaultParams(Params: TStrings);
begin
  Params.Clear;
  Params.Add('ApiKey=@OPENAI_API_KEY');
  Params.Add('Model=gpt-5.1');
  Params.Add('Url=https://api.openai.com/v1/');
end;

class function TAiOpenChat.CreateInstance(Sender: TComponent): TAiChat;
begin
  Result := TAiOpenChat.Create(Sender);
end;

{ #region Setters }
procedure TAiOpenChat.SetStore(const Value: Boolean);
begin
  FStore := Value;
end;

procedure TAiOpenChat.SetTruncation(const Value: String);
begin
  FTruncation := Value;
end;

procedure TAiOpenChat.SetParallel_ToolCalls(const Value: Boolean);
begin
  FParallel_ToolCalls := Value;
end;

procedure TAiOpenChat.SetVerbosity(const Value: String);
begin
  FVerbosity := Value;
end;
{ #endregion }

procedure TAiOpenChat.UpdateResponseStatus(aStatus: String);
begin
  FResponseStatus := aStatus;
end;

function TAiOpenChat.UploadFileToCache(aMediaFile: TAiMediaFile; aTTL_Seconds: Integer): String;
var
  LBody: TMultipartFormData;
  LResponse: IHTTPResponse;
  LHeaders: TNetHeaders;
  LUrl: string;
  LJson: TJSonObject;
  LFileName: string;
  LStatus: string;
  OldAsync: Boolean;

const
  APurpose = 'user_data';

begin
  Result := '';

  // 1. Validaciones previas
  if not Assigned(aMediaFile) then
    raise Exception.Create('El objeto MediaFile no está asignado.');

  // Si ya tiene ID, asumimos que ya está en la nube y no lo subimos de nuevo
  if aMediaFile.IdFile <> '' then
    Exit(aMediaFile.IdFile);

  if (aMediaFile.Content.Size = 0) and (aMediaFile.Base64 = '') then
    raise Exception.Create('El archivo está vacío (sin Content ni Base64).');

  // 2. Preparar URL (asegurar endpoint /files)
  // Nota: Url suele ser 'https://api.openai.com/v1/'
  if Url.EndsWith('/') then
    LUrl := Url + 'files'
  else
    LUrl := Url + '/files';

  // 3. Preparar el contenido si viene solo en Base64
  if (aMediaFile.Content.Size = 0) and (aMediaFile.Base64 <> '') then
  begin
    // Asumimos que LoadFromBase64 llena el stream Content o decodificamos aquí.
    // Si tu clase TAiMediaFile no lo hace automático, aquí deberías decodificar.
    // Por seguridad, usaremos el método nativo si existe, o asumiremos que Content está listo.
  end;

  aMediaFile.Content.Position := 0;
  LFileName := aMediaFile.FileName;
  if LFileName = '' then
    LFileName := 'upload.pdf'; // Nombre fallback obligatorio

  LBody := TMultipartFormData.Create;
  try
    // 4. Parámetros para GPT-5 / Responses
    LBody.AddField('purpose', APurpose); // Por defecto 'user_data'

    // stream, nombre, content-type (opcional, automático por extensión usualmente)
    // Usar helper para compatibilidad entre versiones Delphi/FPC
    uHttpHelper.AddStreamToMultipart(LBody, 'file', aMediaFile.Content, False, LFileName, '');

    // 5. Ejecutar Request usando FClient (Hereda Proxies/Timeouts)
    LHeaders := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];

    // Importante: No establecer ContentType, el cliente lo pone en multipart/form-data automáticamente

    // Los llamados a esta función es sincrónico por ahora
    OldAsync := FClient.Asynchronous;
    FClient.Asynchronous := False;
    Try
      LResponse := FClient.Post(LUrl, LBody, nil, LHeaders);
    Finally
      FClient.Asynchronous := OldAsync;
    End;

    // 6. Procesar Respuesta
    if LResponse.StatusCode = 200 then
    begin
      LJson := TJSONObject.ParseJSONValue(LResponse.ContentAsString) as TJSonObject;
      try
        if Assigned(LJson) then
        begin
          // Obtener ID
          if LJson.TryGetValue('id', Result) then
          begin
            // 7. Actualizar el objeto MediaFile (como en la función que encontraste)
            aMediaFile.IdFile := Result;

            // Guardamos metadatos útiles si existen
            if LJson.TryGetValue('filename', LFileName) then
              aMediaFile.CloudName := LFileName;

            // Guardamos el estado (ej: "processed", "uploaded")
            if LJson.TryGetValue('status', LStatus) then
              aMediaFile.CloudState := LStatus;
          end;
        end;
      finally
        LJson.Free;
      end;
    end
    else
    begin
      // Manejo de errores detallado
      raise Exception.CreateFmt('Error Uploading File (%d): %s', [LResponse.StatusCode, LResponse.ContentAsString]);
    end;

  finally
    LBody.Free;
    // No liberamos AMediaFile.Content aquí, el dueño es el objeto AMediaFile
  end;
end;

// -----------------------------------------------------------------------------
// TOOLS HELPERS
// -----------------------------------------------------------------------------

function TAiOpenChat.GetTools(Funcion: TAiFunctions): TJSonArray;
var
  ToolsString: string;

begin
  if Assigned(Funcion) then
  begin
    // CAMBIO IMPORTANTE:
    // Ahora solicitamos directamente el formato tfOpenAI.
    // La unidad ToolFunctions se encarga de generar la estructura plana y el strict: true.
    ToolsString := Funcion.GetTools(TToolFormat.tfOpenAIResponses);

    if ToolsString = '' then
      Exit(TJSonArray.Create);

    Result := TJSONObject.ParseAsArray(ToolsString);
    if Result = nil then
      Result := TJSonArray.Create;

  end
  else
    Result := TJSonArray.Create;
end;

// -----------------------------------------------------------------------------
// CONSTRUCCIÓN DEL REQUEST (Mapping TAiChat -> Responses API JSON)
// -----------------------------------------------------------------------------
function TAiOpenChat.InitChatCompletions: String;
var
  JResult, JReasoning, JTextConfig, JFormatConfig: TJSonObject;
  JInputArray, JToolsArray: TJSonArray;
  JSystemMsg: String;
  ItemMsg: TAiChatMessage;
  I: Integer;
  LModel: String;
  DataFilesList: TStringList;
  StartIndex: Integer; // Variable nueva para control de historial
  LastMsg: TAiChatMessage;
  IsToolLoop: Boolean;
  // Vars moved from inline
  sShema, PropName, Key, Val, FID: string;
  JInnerSchema, JProps, JShellTool, JImgTool, JCodeTool, JContainer, JPatchTool, JWebTool, JWebOptions: TJSONObject;
  JReq, JFileIdsArr: TJSonArray;
  I1, K, IntVal: Integer;
  Found, BoolVal: Boolean;

  // Helper local (SIN CAMBIOS con respecto a la última corrección)
  procedure AddMessageToInput(Msg: TAiChatMessage; TargetArray: TJSonArray);
  var
    JUserObj, JTextObj, JImageObj, JDocObj, JToolOutObj: TJSonObject;
    JContentArr: TJSonArray;
    MediaFile: TAiMediaFile;
    MediaArr: TAiMediaFilesArray;
    K: Integer;
    JPreBuilt: TJSonObject;
    PromptText: String;
    ExtraFileRefs: String;
    IsSpecialAssistant: Boolean;
  begin
    IsSpecialAssistant := (Msg.Role = 'assistant') and Msg.Prompt.Trim.StartsWith('{') and (ContainsText(Msg.Prompt, '"type":"shell_call"') or ContainsText(Msg.Prompt, '"type":"apply_patch_call"'));

    if (Msg.Role = 'tool') or IsSpecialAssistant then
    begin
      if Msg.Prompt.Trim.StartsWith('{') and (ContainsText(Msg.Prompt, '"type":"apply_patch_call_output"') or ContainsText(Msg.Prompt, '"type":"shell_call_output"') or ContainsText(Msg.Prompt, '"type":"shell_call"') or
        ContainsText(Msg.Prompt, '"type":"apply_patch_call"')) then
      begin
        JPreBuilt := TJSONObject.ParseJSONValue(Msg.Prompt) as TJSonObject;
        if Assigned(JPreBuilt) then
        begin
          TargetArray.Add(JPreBuilt);
          Exit;
        end;
      end;

      if (Msg.Role = 'tool') then
      begin
        JToolOutObj := TJSonObject.Create;
        JToolOutObj.AddPair('type', 'function_call_output');
        if Msg.TollCallId <> '' then
          JToolOutObj.AddPair('call_id', Msg.TollCallId)
        else
          JToolOutObj.AddPair('call_id', 'call_unknown_' + IntToStr(Msg.Id));
        JToolOutObj.AddPair('output', Msg.Prompt);
        TargetArray.Add(JToolOutObj);
        Exit;
      end;
    end;

    JUserObj := TJSonObject.Create;

    if Msg.Role = 'system' then
      JUserObj.AddPair('role', 'developer')
    else
      JUserObj.AddPair('role', Msg.Role);

    if Msg.Role = 'assistant' then
    begin
      JUserObj.AddPair('content', Msg.Prompt);
      TargetArray.Add(JUserObj);
      Exit;
    end;

    MediaArr := Msg.MediaFiles.ToMediaFileArray;
    PromptText := Msg.Prompt;
    ExtraFileRefs := '';

    if (Length(MediaArr) = 0) then
    begin
      JUserObj.AddPair('content', Msg.Prompt);
    end
    else
    begin
      JContentArr := TJSonArray.Create;

      for K := 0 to Length(MediaArr) - 1 do
      begin
        MediaFile := MediaArr[K];
        if (MediaFile.IdFile = '') and (MediaFile.Content.Size > 0) then
        begin
          try
            UploadFileToCache(MediaFile);
          except
          end;
        end;

        case MediaFile.FileCategory of
          Tfc_Image:
            begin
              if Msg.Role = 'user' then
              begin
                JImageObj := TJSonObject.Create;
                JImageObj.AddPair('type', 'input_image');
                if (MediaFile.UrlMedia <> '') and (not MediaFile.UrlMedia.StartsWith('data:', True)) then
                  JImageObj.AddPair('image_url', MediaFile.UrlMedia)
                else
                  JImageObj.AddPair('image_url', 'data:' + MediaFile.MimeType + ';base64,' + MediaFile.Base64);
                if MediaFile.Detail <> '' then
                  JImageObj.AddPair('detail', MediaFile.Detail);
                JContentArr.Add(JImageObj);
              end;
            end;
          Tfc_pdf:
            begin
              JDocObj := TJSonObject.Create;
              JDocObj.AddPair('type', 'input_file');
              if MediaFile.IdFile <> '' then
                JDocObj.AddPair('file_id', MediaFile.IdFile)
              else if (MediaFile.UrlMedia <> '') and (not MediaFile.UrlMedia.StartsWith('data:', True)) then
                JDocObj.AddPair('file_url', MediaFile.UrlMedia)
              else
              begin
                if MediaFile.FileName <> '' then
                  JDocObj.AddPair('filename', MediaFile.FileName)
                else
                  JDocObj.AddPair('filename', 'document.pdf');
                JDocObj.AddPair('file_data', 'data:' + MediaFile.MimeType + ';base64,' + MediaFile.Base64);
              end;
              JContentArr.Add(JDocObj);
            end;
        else
          begin
            if MediaFile.IdFile <> '' then
            begin
              DataFilesList.Add(MediaFile.IdFile);
              ExtraFileRefs := ExtraFileRefs + Format(#13#10'[Attached File available in Python Tool: Name="%s"]', [MediaFile.FileName]);
            end;
          end;
        end;
      end;

      if (PromptText <> '') or (ExtraFileRefs <> '') then
      begin
        JTextObj := TJSonObject.Create;
        JTextObj.AddPair('type', 'input_text'); // Usuario siempre es input_text
        JTextObj.AddPair('text', PromptText + ExtraFileRefs);
        JContentArr.Add(JTextObj);
      end;

      JUserObj.AddPair('content', JContentArr);
    end;

    TargetArray.Add(JUserObj);
  end;

begin
  JResult := TJSonObject.Create;
  JToolsArray := nil;
  DataFilesList := TStringList.Create;
  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);

  try
    JResult.AddPair('model', LModel);

    if FClient.Asynchronous then
      JResult.AddPair('stream', True);

    // 1. Instructions
    JSystemMsg := '';
    if (FMessages.Count > 0) and (FMessages[0].Role = 'system') then
      JSystemMsg := FMessages[0].Prompt
    else if SystemPrompt.Text <> '' then
      JSystemMsg := SystemPrompt.Text;

    if Memory.Count > 0 then
      JSystemMsg := JSystemMsg + sLineBreak + 'Memory context: ' + Memory.Text;

    if JSystemMsg <> '' then
      JResult.AddPair('instructions', JSystemMsg);

    // -------------------------------------------------------------------------
    // 2. INPUT (GESTIÓN DE ESTADO OPTIMIZADA)
    // -------------------------------------------------------------------------
    JInputArray := TJSonArray.Create;
    StartIndex := 0;

    // Verificamos si tenemos un ID de respuesta del turno anterior válido.
    // Esto permite usar el cache/contexto del servidor y evitar reenviar historial.
    if (FResponseId <> '') and (FMessages.Count > 0) then
    begin
      JResult.AddPair('previous_response_id', FResponseId);

      LastMsg := FMessages[FMessages.Count - 1];

      // Detectamos si estamos en un bucle de herramientas (el último mensaje es un output de tool)
      // Nota: Checkeamos 'tool' o mensajes especiales de assistant que contienen outputs (legacy)
      IsToolLoop := (LastMsg.Role = 'tool') or ((LastMsg.Role = 'assistant') and ContainsText(LastMsg.Prompt, '_output"'));

      if IsToolLoop then
      begin
        // ESTRATEGIA TOOL LOOP:
        // Si estamos enviando un resultado de herramienta, NO debemos enviar de nuevo el mensaje
        // del Usuario que provocó la llamada, porque ese contexto ya vive en 'previous_response_id'.
        // Buscamos hacia atrás hasta encontrar el mensaje de User o Assistant previo y cortamos ahí.
        for I := FMessages.Count - 1 downto 0 do
        begin
          if (FMessages[I].Role = 'user') or ((FMessages[I].Role = 'assistant') and (not ContainsText(FMessages[I].Prompt, '_output"'))) then
          begin
            StartIndex := I + 1; // Empezamos estrictamente DESPUÉS del usuario
            Break;
          end;
        end;
      end
      else
      begin
        // ESTRATEGIA NORMAL (Turno de Usuario):
        // El último mensaje es del usuario. Buscamos la última respuesta del asistente para enviar solo lo nuevo.
        for I := FMessages.Count - 1 downto 0 do
        begin
          if (FMessages[I].Role = 'assistant') and (not ContainsText(FMessages[I].Prompt, '_output"')) then
          begin
            StartIndex := I + 1;
            Break;
          end;
        end;
      end;
    end;

    // Recorremos desde el punto calculado (0 si es nuevo, >0 si es continuación)
    for I := StartIndex to FMessages.Count - 1 do
    begin
      ItemMsg := FMessages[I];
      if (ItemMsg.Role <> 'system') then
        AddMessageToInput(ItemMsg, JInputArray);
    end;
    // -------------------------------------------------------------------------

    if JInputArray.Count > 0 then
      JResult.AddPair('input', JInputArray)
    else
      JInputArray.Free; // Evitamos enviar input vacío si no hay mensajes nuevos

    // 3. Parámetros de Configuración
    JResult.AddPair('store', FStore);
    if FTruncation <> 'disabled' then
      JResult.AddPair('truncation', FTruncation);

    if (ThinkingLevel <> tlDefault) or (FReasoningSummary <> rsmDefault) then
    begin
      JReasoning := TJSonObject.Create;
      Case ThinkingLevel of
        tlLow:
          JReasoning.AddPair('effort', 'low');
        tlMedium:
          JReasoning.AddPair('effort', 'medium');
        tlHigh:
          JReasoning.AddPair('effort', 'high');
      End;
      case FReasoningSummary of
        rsmAuto:
          JReasoning.AddPair('summary', 'auto');
        rsmConcise:
          JReasoning.AddPair('summary', 'concise');
        rsmDetailed:
          JReasoning.AddPair('summary', 'detailed');
      end;
      JResult.AddPair('reasoning', JReasoning);
    end;


    JTextConfig := TJSonObject.Create;
    if FVerbosity <> '' then
      JTextConfig.AddPair('verbosity', FVerbosity);


    // 3 - Formato json -----------------------------------------


// 1. Configurar Salida Estructurada (JSON Schema)

JFormatConfig := Nil;
    if FResponse_format = tiaChatRfJsonSchema then
    begin
      JFormatConfig := TJSonObject.Create;
      JFormatConfig.AddPair('type', 'json_schema');

      if JsonSchema.Text <> '' then
      begin
        // Limpieza básica de saltos de línea para evitar errores de parseo
        sShema := StringReplace(JsonSchema.Text, '\n', ' ', [rfReplaceAll]);

        JInnerSchema := TJSONObject.ParseJSONValue(sShema) as TJSonObject;

        if Assigned(JInnerSchema) then
        begin
          // A. VALIDACIÓN TIPO OBJECT
          if JInnerSchema.GetValueAsString('type') = 'object' then
          begin
             // 1. CORRECCIÓN: additionalProperties: false es obligatorio
             if JInnerSchema.GetValue('additionalProperties') = nil then
               JInnerSchema.AddPair('additionalProperties', CreateJSONBool(False));

             // 2. CORRECCIÓN: OpenAI Strict exige que TODAS las propiedades estén en 'required'
             if JInnerSchema.TryGetValue('properties', JProps) then
             begin
               // Obtener o crear array 'required'
               if not JInnerSchema.TryGetValue('required', JReq) then
               begin
                 JReq := TJSonArray.Create;
                 JInnerSchema.AddPair('required', JReq);
               end;

               // Recorrer todas las propiedades y asegurarse que están en 'required'
               for I1 := 0 to JProps.Count - 1 do
               begin
                 PropName := GetJSONStringValue(JProps.Pairs[I1].JsonString);
                 Found := False;

                 for K := 0 to JReq.Count - 1 do
                 begin
                   if GetJSONStringValue(JReq.Items[K]) = PropName then
                   begin
                     Found := True;
                     Break;
                   end;
                 end;

                 // Si falta, lo agregamos para satisfacer a la API
                 if not Found then
                   JReq.Add(PropName);
               end;
             end;
          end;

          // B. CONFIGURACIÓN FINAL (Flattened structure para Responses API)
          // Estos parámetros van al mismo nivel que "type", NO dentro de un sub-objeto json_schema
          JFormatConfig.AddPair('name', 'structured_response');
          JFormatConfig.AddPair('strict', CreateJSONBool(True));
          JFormatConfig.AddPair('schema', JInnerSchema);
        end;
      end;
    end
    // 2. Configurar JSON Simple (Para cuando no es Schema estricto)
    else if FResponse_format = tiaChatRfJson then
    begin
      JFormatConfig := TJSonObject.Create;
      JFormatConfig.AddPair('type', 'json_object');
    end;

    // 3. Conectar la configuración de formato al objeto de Texto
    if Assigned(JFormatConfig) then
    begin
      // Asegurarnos de que JTextConfig exista (por si no se creó con verbosity antes)
      if not Assigned(JTextConfig) then
        JTextConfig := TJSonObject.Create;

      JTextConfig.AddPair('format', JFormatConfig);
    end;

    // 4. Conectar el objeto de Texto a la Raíz del Request (JResult)
    // Esto es lo que hace que aparezca en el JSON final enviado a la API
    if Assigned(JTextConfig) and (JTextConfig.Count > 0) then
    begin
      // Verificamos si ya existe 'text' (raro, pero por seguridad)
      if JResult.GetValue('text') = nil then
        JResult.AddPair('text', JTextConfig);
    end;




    // ---- 4. TOOLS -----------------------------------------
    if Tool_Active and Assigned(AiFunctions) then
      JToolsArray := GetTools(AiFunctions);

    if (tcm_Shell in ChatMediaSupports) then
    begin
      if not Assigned(JToolsArray) then
        JToolsArray := TJSonArray.Create;
      JShellTool := TJSonObject.Create;
      JShellTool.AddPair('type', 'shell');
      JToolsArray.Add(JShellTool);
    end;

    if (Tcm_Image in ChatMediaSupports) and (Tfc_Image in NativeOutputFiles) then
    begin
      if not Assigned(JToolsArray) then
        JToolsArray := TJSonArray.Create;
      JImgTool := TJSonObject.Create;
      JImgTool.AddPair('type', 'image_generation');
      if ImageParams.Count > 0 then
      begin
        if ImageParams.Values['size'] <> '' then
          JImgTool.AddPair('size', ImageParams.Values['size']);
        if ImageParams.Values['quality'] <> '' then
          JImgTool.AddPair('quality', ImageParams.Values['quality']);
      end;
      JToolsArray.Add(JImgTool);
    end;

    if (Tcm_CodeInterpreter in ChatMediaSupports) then
    begin
      if not Assigned(JToolsArray) then
        JToolsArray := TJSonArray.Create;
      JCodeTool := TJSonObject.Create;
      JCodeTool.AddPair('type', 'code_interpreter');
      JContainer := TJSonObject.Create;
      JContainer.AddPair('type', 'auto');
      if DataFilesList.Count > 0 then
      begin
        JFileIdsArr := TJSonArray.Create;
        for FID in DataFilesList do
          JFileIdsArr.Add(FID);
        JContainer.AddPair('file_ids', JFileIdsArr);
      end;
      JCodeTool.AddPair('container', JContainer);
      JToolsArray.Add(JCodeTool);
    end;

    if (tcm_TextEditor in ChatMediaSupports) then // O tcm_TextEditor si prefieres
    begin
      if not Assigned(JToolsArray) then
        JToolsArray := TJSonArray.Create;

      JPatchTool := TJSonObject.Create;
      JPatchTool.AddPair('type', 'apply_patch');
      JToolsArray.Add(JPatchTool);
    end;

    if (Tcm_WebSearch in ChatMediaSupports) then
    begin
      if not Assigned(JToolsArray) then
        JToolsArray := TJSonArray.Create;

      JWebTool := TJSonObject.Create;
      JWebTool.AddPair('type', 'web_search');

      // Si el usuario configuró parámetros adicionales en WebSearchParams (ej: count=5)
      if (WebSearchParams.Count > 0) then
      begin
        JWebOptions := TJSonObject.Create;
        // Recorremos la lista clave=valor
        for I1 := 0 to WebSearchParams.Count - 1 do
        begin
          Key := WebSearchParams.Names[I1];
          Val := WebSearchParams.ValueFromIndex[I1];

          if Key <> '' then
          begin
            // Intentamos detectar si es un número, booleano o string
            if TryStrToInt(Val, IntVal) then
              JWebOptions.AddPair(Key, CreateJSONNumber(IntVal))
            else if TryStrToBool(Val, BoolVal) then
              JWebOptions.AddPair(Key, CreateJSONBool(BoolVal))
            else
              JWebOptions.AddPair(Key, Val);
          end;
        end;

        // Solo añadimos el objeto de opciones si realmente se agregó algo
        if JWebOptions.Count > 0 then
          JWebTool.AddPair('web_search', JWebOptions)
        else
          JWebOptions.Free;
      end;

      JToolsArray.Add(JWebTool);
    end;

    if Assigned(JToolsArray) then
    begin
      if JToolsArray.Count > 0 then
      begin
        JResult.AddPair('tools', JToolsArray);
        if FParallel_ToolCalls then
          JResult.AddPair('parallel_tool_calls', True);
        if Tool_choice <> '' then
        begin
          if (Tool_choice = 'auto') or (Tool_choice = 'required') or (Tool_choice = 'none') then
            JResult.AddPair('tool_choice', Tool_choice)
          else
            JResult.AddPair('tool_choice', TJSONObject.ParseJSONValue(Tool_choice));
        end;
      end
      else
        JToolsArray.Free;
    end;

    Result := JResult.ToString;
  finally
    JResult.Free;
    DataFilesList.Free;
  end;
end;



// -----------------------------------------------------------------------------
// PROCESAMIENTO DE RESPUESTA
// -----------------------------------------------------------------------------

procedure TAiOpenChat.ParseChat(jObj: TJSonObject; ResMsg: TAiChatMessage);
var
  JOutput, JContent, JAnnotations: TJSonArray;
  JItem, JContentItem, JAnno: TJSonObject;
  // Nuevas variables para captura de metadatos
  JUsage, JUsageDetails, JInputDetails, JIncomplete: TJSonObject;
  SType, SVal, SId, SCallId, SContainerId: String;
  I, K, A: Integer;
  ToolCalls: TObjectList<TAiToolsFunction>;
  ToolCall: TAiToolsFunction;
  TaskList: array of ITask;
  NewMsg: TAiChatMessage;
  GeneratedFile: TAiMediaFile;
  WebItem: TAiWebSearchItem;
  // Variables auxiliares para valores numéricos
  UnixDate: Int64;
  TokenCount: Int64;
  // Vars moved from inline
  ImgBase64, ImgPrompt, ImgId, ImgFormat, ShellJsonOutput, CmdStr, OutStd, OutErr, OpType, OpPath, OpDiff, PatchStatus, PatchOutput, ResStr: string;
  HistMsg: TAiChatMessage;
  JAction, JCmdRes, JOutcome, OutputJson, JOp, JCmd: TJSONObject;
  Commands, JOutputArr: TJSonArray;
  MaxLen, ExitCode: Integer;
  Handled: Boolean;
  Code: TMarkdownCodeExtractor;
  CodeFiles: TCodeFileList;
  CodeFile: TCodeFile;
  St: TStringStream;
  MF: TAiMediaFile;
  C: Integer; // FPC
begin
  // 1. Actualizar estado global de la respuesta
  if jObj.TryGetValue('id', SVal) then
    FResponseId := SVal;
  if jObj.TryGetValue('status', SVal) then
    UpdateResponseStatus(SVal);

  // ---------------------------------------------------------------------------
  // BLOQUE NUEVO: Captura de Metadatos (Model, CreatedAt, Usage, FinishReason)
  // ---------------------------------------------------------------------------

  // A) FECHA DE CREACIÓN
  if jObj.TryGetValue('created_at', UnixDate) then
  begin
    // Requiere System.DateUtils en el uses
    // ResMsg.CreatedAt := TTimeZone.Local.ToLocalTime(UnixToDateTime(UnixDate));
  end;

  // B) MODELO EXACTO UTILIZADO
  if jObj.TryGetValue('model', SVal) then
    ResMsg.Model := SVal;

  // C) INFORMACIÓN DE FINALIZACIÓN (Finish Reason / Incomplete Details)
  // Retorna null, lo mitimos por ahora
  {
    if jObj.TryGetValue('incomplete_details', JIncomplete) then
    begin
    if Assigned(JIncomplete) then
    begin
    if JIncomplete.TryGetValue('reason', SVal) then
    ResMsg.FinishReason := SVal; // ej: "max_tokens", "safety"
    end;
    end
    else
    begin
    // Si incomplete_details es null y el status es completed, asumimos finalización normal
    if FResponseStatus = 'completed' then
    ResMsg.FinishReason := 'stop';
    end;
  }

  // D) USO DE TOKENS (Costos, Caché y Razonamiento)
  if jObj.TryGetValue('usage', JUsage) then
  begin
    // Totales básicos
    if JUsage.TryGetValue('input_tokens', TokenCount) then
      ResMsg.Prompt_tokens := TokenCount;

    if JUsage.TryGetValue('output_tokens', TokenCount) then
      ResMsg.Completion_tokens := TokenCount;

    if JUsage.TryGetValue('total_tokens', TokenCount) then
      ResMsg.Total_tokens := TokenCount;

    // Detalles de Entrada: Tokens en Caché (Ahorro)
    if JUsage.TryGetValue('input_tokens_details', JInputDetails) then
    begin
      if JInputDetails.TryGetValue('cached_tokens', TokenCount) then
        ResMsg.cached_tokens := TokenCount;
    end;

    // Detalles de Salida: Tokens de Razonamiento (Thinking)
    if JUsage.TryGetValue('output_tokens_details', JUsageDetails) then
    begin
      if JUsageDetails.TryGetValue('reasoning_tokens', TokenCount) then
        ResMsg.Thinking_tokens := TokenCount;
    end;
  end;

  // ---------------------------------------------------------------------------
  // FIN BLOQUE NUEVO
  // ---------------------------------------------------------------------------

  FLastContent := '';
  ToolCalls := TObjectList<TAiToolsFunction>.Create;

  try
    // 2. Iterar el array 'output' (Polimorfismo: Messages, Tools, Images)
    if jObj.TryGetValue('output', JOutput) then
    begin
      for I := 0 to JOutput.Count - 1 do
      begin
        JItem := TJSonObject(JOutput.Items[I]);
        if not JItem.TryGetValue('type', SType) then
          Continue;

        // --- TIPO: MESSAGE ---
        if SType = 'message' then
        begin
          if JItem.TryGetValue('content', JContent) then
          begin
            for K := 0 to JContent.Count - 1 do
            begin
              JContentItem := TJSonObject(JContent.Items[K]);
              if JContentItem.TryGetValue('type', SVal) then
              begin
                // A) TEXTO
                if (SVal = 'output_text') then
                begin
                  if JContentItem.TryGetValue('text', SVal) then
                    FLastContent := FLastContent + SVal;

                  // B) ANNOTATIONS (Citas y Archivos)
                  if JContentItem.TryGetValue('annotations', JAnnotations) then
                  begin
                    for A := 0 to JAnnotations.Count - 1 do
                    begin
                      JAnno := JAnnotations.Items[A] as TJSonObject;
                      if not JAnno.TryGetValue('type', SVal) then
                        Continue;

                      // 1. ARCHIVOS GENERADOS (Code Interpreter)
                      if SVal = 'container_file_citation' then
                      begin
                        GeneratedFile := TAiMediaFile.Create;

                        // Capturamos el file_id
                        if JAnno.TryGetValue('file_id', SId) then
                          GeneratedFile.IdFile := SId;

                        // Capturamos el nombre
                        if JAnno.TryGetValue('filename', SId) then
                          GeneratedFile.FileName := SId;

                        // CRÍTICO: Capturamos el container_id y lo guardamos en CloudState
                        if JAnno.TryGetValue('container_id', SContainerId) then
                          GeneratedFile.CloudState := SContainerId;

                        try
                          DownLoadFile(GeneratedFile);
                        except
                          // Manejo silencioso de errores de descarga para no romper el flujo
                        end;

                        // Añadir al mensaje de respuesta
                        ResMsg.MediaFiles.Add(GeneratedFile);
                      end

                      // -- Citas Web (Web Search) --
                      else if SVal = 'url_citation' then
                      begin
                        if not Assigned(ResMsg.WebSearchResponse) then
                        begin
                          ResMsg.WebSearchResponse := TAiWebSearch.Create;
                          ResMsg.WebSearchResponse.&type := 'web_search_results';
                        end;

                        WebItem := TAiWebSearchItem.Create;
                        WebItem.&type := 'url_citation';
                        JAnno.TryGetValue('start_index', WebItem.start_index);
                        JAnno.TryGetValue('end_index', WebItem.end_index);
                        JAnno.TryGetValue('url', WebItem.Url);
                        JAnno.TryGetValue('title', WebItem.title);

                        ResMsg.WebSearchResponse.annotations.Add(WebItem);
                      end;
                    end;
                  end;
                end
                // C) REFUSAL (Rechazo de seguridad)
                else if (SVal = 'refusal') then
                begin
                  if JContentItem.TryGetValue('refusal', SVal) then
                    FLastContent := FLastContent + ' [REFUSAL: ' + SVal + ']';
                  ResMsg.IsRefusal := True;
                end;
              end;
            end;
          end;
        end

        // --- TIPO: IMAGE GENERATION ---
        else if SType = 'image_generation_call' then
        begin
          ImgBase64 := '';
          ImgPrompt := '';
          ImgId := '';
          ImgFormat := 'png'; // Valor por defecto

          // 1. Obtener el Base64: La API Responses usa 'result', pero dejamos 'image' por seguridad
          if not JItem.TryGetValue('result', ImgBase64) then
            JItem.TryGetValue('image', ImgBase64);

          if ImgBase64 <> '' then
          begin
            // 2. Extraer metadatos
            JItem.TryGetValue('revised_prompt', ImgPrompt);
            JItem.TryGetValue('id', ImgId);
            JItem.TryGetValue('output_format', ImgFormat);

            if ImgId = '' then
              ImgId := 'gen_' + IntToStr(TThread.GetTickCount);

            // 3. Crear MediaFile
            GeneratedFile := TAiMediaFile.Create;

            // Cargamos el contenido (Base64)
            GeneratedFile.LoadFromBase64(ImgId + '.' + ImgFormat, ImgBase64);

            // Guardamos el prompt revisado en Transcription
            GeneratedFile.Transcription := ImgPrompt;

            // Guardamos el ID
            GeneratedFile.IdFile := ImgId;

            // 4. Añadir al mensaje de respuesta actual
            ResMsg.MediaFiles.Add(GeneratedFile);

            // 5. Si el mensaje de texto viene vacío, ponemos un indicador en el Prompt
            if ResMsg.Prompt = '' then
              ResMsg.Prompt := '[Image Generated] ' + Copy(ImgPrompt, 1, 50) + '...';
          end;
        end

        // --- TIPO: FUNCTION CALL (STANDARD TOOL) ---
        else if SType = 'function_call' then
        begin
          ToolCall := TAiToolsFunction.Create;

          // CRÍTICO: Obtener call_id para responder luego
          if JItem.TryGetValue('call_id', SCallId) then
            ToolCall.Id := SCallId
          else if JItem.TryGetValue('id', SId) then
            ToolCall.Id := SId;

          if JItem.TryGetValue('name', SVal) then
            ToolCall.Name := SVal;
          if JItem.TryGetValue('arguments', SVal) then
            ToolCall.Arguments := SVal;

          ToolCalls.Add(ToolCall);
        end

        // --- TIPO: SHELL CALL (CON SOPORTE TAISHELL Y LEGACY) ---
        else if SType = 'shell_call' then
        begin
          if JItem.TryGetValue('call_id', SCallId) then
          begin
            // -----------------------------------------------------------------
            // 1. GUARDAR LA PETICIÓN (CALL) EN EL HISTORIAL
            // -----------------------------------------------------------------
            // Guardamos el JSON crudo del item 'shell_call' como un mensaje del asistente.
            // Esto es crucial para mantener la cadena: User -> ShellCall -> ShellOutput -> User
            HistMsg := TAiChatMessage.Create(JItem.ToString, 'assistant');
            HistMsg.TollCallId := SCallId;
            HistMsg.PreviousResponseId := FResponseId;
            HistMsg.Id := Self.Messages.Count + 1;
            Self.Messages.Add(HistMsg);

            // -----------------------------------------------------------------
            // 2. EJECUTAR COMANDOS
            // -----------------------------------------------------------------
            // 2. EJECUTAR COMANDOS
            // -----------------------------------------------------------------
            if JItem.TryGetValue('action', JAction) then
            begin

              // OPCIÓN A: Usar Componente ShellTool (Sesión Persistente)
              if Assigned(ShellTool) then
              begin
                // Delegamos la ejecución y el formateo del JSON de respuesta al componente
                ShellJsonOutput := ShellTool.Execute(SCallId, JAction);

                // Crear y guardar el mensaje de respuesta de la herramienta
                NewMsg := TAiChatMessage.Create(ShellJsonOutput, 'tool');
                NewMsg.TollCallId := SCallId;
                NewMsg.PreviousResponseId := FResponseId;
                NewMsg.Id := Self.Messages.Count + 1;
                Self.Messages.Add(NewMsg);
              end

              // OPCIÓN B: Fallback Legacy (Ejecución aislada)
              else
              begin
                MaxLen := 0;

                JAction.TryGetValue('commands', Commands);
                JAction.TryGetValue('max_output_length', MaxLen);

                JOutputArr := TJSonArray.Create;

                // Iterar comandos
                if Assigned(Commands) then
                  for C := 0 to Commands.Count - 1 do
                  begin
                    CmdStr := GetJSONStringValue(Commands.Items[C]);
                    OutStd := '';
                    OutErr := '';
                    ExitCode := 0;
                    Handled := False;

                    // 1. Evento de Usuario
                    if Assigned(FOnShellCommand) then
                      FOnShellCommand(Self, CmdStr, SCallId, OutStd, OutErr, ExitCode, Handled);

                    // 2. Ejecución Automática
                    if (not Handled) and FAllowAutoShell then
                    begin
                      try
                        OutStd := TUtilsSystem.RunCommandLine(CmdStr);
                        ExitCode := 0;
                      except
                        on E: Exception do
                        begin
                          OutErr := E.Message;
                          ExitCode := 1;
                        end;
                      end;
                    end
                    else if not Handled then
                    begin
                      OutErr := 'Shell execution denied (ShellTool not assigned and AllowAutoShell=False).';
                      ExitCode := 126;
                    end;

                    // Truncar si es necesario
                    if (MaxLen > 0) and (Length(OutStd) > MaxLen) then
                      OutStd := Copy(OutStd, 1, MaxLen) + '... [truncated]';

                    // Construir respuesta del comando individual
                    JCmdRes := TJSonObject.Create;
                    JCmdRes.AddPair('stdout', OutStd);
                    JCmdRes.AddPair('stderr', OutErr);

                    JOutcome := TJSonObject.Create;
                    JOutcome.AddPair('type', 'exit');
                    JOutcome.AddPair('exit_code', ExitCode);
                    JCmdRes.AddPair('outcome', JOutcome);

                    JOutputArr.Add(JCmdRes);
                  end;

                // Construir shell_call_output final
                OutputJson := TJSonObject.Create;
                try
                  OutputJson.AddPair('type', 'shell_call_output');
                  OutputJson.AddPair('call_id', SCallId);
                  if MaxLen > 0 then
                    OutputJson.AddPair('max_output_length', MaxLen);
                  OutputJson.AddPair('output', JOutputArr);

                  // Añadir mensaje a la cola
                  NewMsg := TAiChatMessage.Create(OutputJson.ToString, 'tool');
                  NewMsg.TollCallId := SCallId;
                  NewMsg.PreviousResponseId := FResponseId;
                  NewMsg.Id := Self.Messages.Count + 1;
                  Self.Messages.Add(NewMsg);
                finally
                  OutputJson.Free;
                end;
              end;
            end;
          end;
        end

        // --- TIPO: APPLY PATCH (EDICIÓN DE ARCHIVOS) ---
        else if SType = 'apply_patch_call' then
        begin
          // 1. Extraer datos de la operación
          OpType := '';
          OpPath := '';
          OpDiff := '';

          if JItem.TryGetValue('call_id', SCallId) then
          begin
            if JItem.TryGetValue('operation', JOp) then
            begin
              JOp.TryGetValue('type', OpType);
              JOp.TryGetValue('path', OpPath);
              JOp.TryGetValue('diff', OpDiff);
            end;

            PatchStatus := 'failed';
            PatchOutput := '';
            Handled := False;

            // OPCIÓN A: Evento de Usuario
            if Assigned(FOnApplyPatch) then
            begin
              try
                FOnApplyPatch(Self, OpType, OpPath, OpDiff, SCallId, PatchStatus, PatchOutput);
                Handled := True;
              except
                on E: Exception do
                begin
                  PatchStatus := 'failed';
                  PatchOutput := 'Exception in OnApplyPatch: ' + E.Message;
                  Handled := True;
                end;
              end;
            end;

            // OPCIÓN B: Automático (TAiTextEditorTool)
            if (not Handled) and (Assigned(TextEditorTool)) then
            begin
              try
                JCmd := TJSonObject.Create;
                try
                  if OpType = 'delete_file' then
                  begin
                    JCmd.AddPair('command', 'delete');
                    JCmd.AddPair('path', OpPath);
                  end
                  else
                  begin
                    JCmd.AddPair('command', 'apply_diff');
                    JCmd.AddPair('path', OpPath);
                    JCmd.AddPair('diff_text', OpDiff);
                  end;

                  ResStr := TextEditorTool.Execute(JCmd.ToString);

                  if ResStr.StartsWith('Error') then
                  begin
                    PatchStatus := 'failed';
                    PatchOutput := ResStr;
                  end
                  else
                  begin
                    PatchStatus := 'completed';
                    PatchOutput := ResStr;
                  end;
                finally
                  JCmd.Free;
                end;
              finally
              end;
            end;

            // 4. Crear mensaje de respuesta
            OutputJson := TJSonObject.Create;
            try
              OutputJson.AddPair('type', 'apply_patch_call_output');
              OutputJson.AddPair('call_id', SCallId);
              OutputJson.AddPair('status', PatchStatus);
              if PatchOutput <> '' then
                OutputJson.AddPair('output', PatchOutput);

              NewMsg := TAiChatMessage.Create(OutputJson.ToString, 'tool');
              NewMsg.TollCallId := SCallId;
              NewMsg.PreviousResponseId := FResponseId;
              NewMsg.Id := Self.Messages.Count + 1;
              Self.Messages.Add(NewMsg);
            finally
              OutputJson.Free;
            end;
          end;
        end;
      end;
    end;

    // 3. Finalizar procesamiento del mensaje de texto
    ResMsg.Prompt := FLastContent;
    ResMsg.Content := FLastContent;
    ResMsg.PreviousResponseId := FResponseId;

    // 4. Ejecutar Tools estándar (Functions) si las hay
    if ToolCalls.Count > 0 then
    begin
      SetLength(TaskList, ToolCalls.Count);

      for I := 0 to ToolCalls.Count - 1 do
      begin
        ToolCall := ToolCalls[I];
        ToolCall.ResMsg := ResMsg;
        ToolCall.AskMsg := GetLastMessage; // Contexto

        TaskList[I] := TTask.Create(
          procedure
          begin
            DoCallFunction(ToolCall);
          end);
        TaskList[I].Start;
      end;

      TTask.WaitForAll(TaskList);

      // Crear mensajes de respuesta de tools
      for I := 0 to ToolCalls.Count - 1 do
      begin
        ToolCall := ToolCalls[I];
        NewMsg := TAiChatMessage.Create(ToolCall.Response, 'tool');
        NewMsg.TollCallId := ToolCall.Id;
        NewMsg.FunctionName := ToolCall.Name;
        NewMsg.PreviousResponseId := FResponseId;
        NewMsg.Id := Self.Messages.Count + 1;
        Self.Messages.Add(NewMsg);
      end;

      // Re-ejecutar el chat con los resultados de las tools (Loop Agentic)
      Self.Run(Nil, ResMsg);
    end
    else
    begin
      // Si no hubo Function Calls, revisamos si hubo Shell Calls o Patch Calls que agregaron mensajes
      // al historial. Si es así, debemos hacer recursión para que la IA vea el resultado.
      // (Verificamos si el último mensaje es de tipo 'tool')
      if (Self.Messages.Count > 0) and (Self.Messages.Last.Role = 'tool') and (FLastContent = '') then
      begin
        // Recursión para que la IA responda al resultado del shell/patch
        Self.Run(Nil, ResMsg);
      end
      else
      begin
        // Flujo normal finalizado
        If tfc_ExtracttextFile in NativeOutputFiles then
        Begin
          Code := TMarkdownCodeExtractor.Create;
          Try
            CodeFiles := Code.ExtractCodeFiles(FLastContent);
            For CodeFile in CodeFiles do
            Begin
              St := TStringStream.Create(CodeFile.Code);
              Try
                St.Position := 0;
                MF := TAiMediaFile.Create;
                MF.LoadFromStream('file.' + CodeFile.FileType, St);
                ResMsg.MediaFiles.Add(MF);
              Finally
                St.Free;
              End;
            End;
          Finally
            Code.Free;
          End;
        End;

        DoProcessResponse(GetLastMessage, ResMsg, FLastContent);

        if Assigned(FOnReceiveDataEnd) then
          FOnReceiveDataEnd(Self, ResMsg, jObj, 'assistant', FLastContent);
      end;
    end;

  finally
    ToolCalls.Free;
  end;
end;


// -----------------------------------------------------------------------------
// MÉTODOS DE EJECUCIÓN (HTTP)
// -----------------------------------------------------------------------------

function TAiOpenChat.InternalRunCompletions(ResMsg, AskMsg: TAiChatMessage): String;
var
  ABody, sUrl: String;
  Res: IHTTPResponse;
  St: TStringStream;
  FHeaders: TNetHeaders;
  jObj: TJSonObject;
begin
  FBusy := True;
  FAbort := False;
  FLastError := '';
  FResponseStatus := '';

  // 1. Asegurar que el mensaje del USUARIO está en el historial
  if FMessages.IndexOf(AskMsg) < 0 then
  begin
    AskMsg.Id := FMessages.Count + 1;
    FMessages.Add(AskMsg);
    if Assigned(FOnAddMessage) then
      FOnAddMessage(Self, AskMsg, Nil, AskMsg.Role, AskMsg.Prompt);
  end;

  St := TStringStream.Create('', TEncoding.UTF8);
  if Url.EndsWith('/') then
    sUrl := Url + 'responses'
  else
    sUrl := Url + '/responses';

  try
    DoStateChange(acsConnecting, 'Sending request...');

    FHeaders := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    FClient.ContentType := 'application/json';

    ABody := InitChatCompletions;
    St.WriteString(ABody);
    St.Position := 0;

    LogDebug('--Request-Body--');
    LogDebug('    ' + ABody);

    FResponse.Clear;
    Res := FClient.Post(sUrl, St, FResponse, FHeaders);

    if FClient.Asynchronous = False then
    begin
      // ... (Lógica síncrona se mantiene igual) ...
      if Res.StatusCode = 200 then
      begin

        LogDebug('--Response Sincrono--');
        LogDebug('    ' + Res.ContentAsString);

        jObj := TJSonObject(TJSONObject.ParseJSONValue(Res.ContentAsString));
        try
          FBusy := False;
          ParseChat(jObj, ResMsg);
          Result := FLastContent;
        finally
          FreeAndNil(jObj);
        end;
      end
      else
        Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;
  finally
    if FClient.Asynchronous = False then
      St.Free;
  end;
end;

// *****************************************************************************
// ARQUITECTURA DE WORKER AISLADO (THREAD SAFETY & ISOLATION)
// *****************************************************************************
// Motivo del cambio:
// 1. Thread Safety (Seguridad de Hilos):
//    No se puede reutilizar 'FClient' (el cliente HTTP principal) dentro de un hilo
//    background (TTask) para procesos largos (Polling de video de +60 seg) porque
//    bloquearía o corrompería el estado si el usuario intenta chatear simultáneamente.
//    Este worker crea su PROPIA instancia de TNetHTTPClient aislada.
//
// 2. Prevención de Captura de Variables (Variable Capture issues):
//    Usar métodos anónimos gigantes dentro de TTask.Run a menudo causa access violations
//    en Delphi/FPC por capturar variables locales que salen de ámbito. Al pasar
//    todos los datos como parámetros explícitos (by value/const) a este método,
//    congelamos el estado y garantizamos estabilidad.
//
// 3. Claridad Lógica:
//    La lógica de "Polling" (preguntar cada 2s si el video está listo) es compleja y
//    ensucia el código principal de chat. Mejor tenerla encapsulada aquí.
// *****************************************************************************
procedure TAiOpenChat.AsyncImageGenWorker(const AUrl, AKey, AModel, APrompt: string; AVidParams: TStrings; AResMsg, AAskMsg: TAiChatMessage);
var
  AsyncClient: TNetHTTPClient;
  LFormData: TMultipartFormData;
  LModelStream, LPromptStream: TStringStream;
  AsyncResponse: IHTTPResponse;
  AsyncJson: TJSonObject;
  PollingUrl: string;
  VideoMedia: TAiMediaFile;
  VideoUrl: string;
  PollCount, ProgressPct: Integer;
  sPollCount: String;
  JErr: TJSonObject;
  VideoStream: TMemoryStream;
  Parts: TArray<string>;
  FinalStatus: string;
  ErrMessage: string;
  AuthHeader: TNetHeaders;
  CaptureImageFile: TAiMediaFile;
  LFile: TAiMediaFilesArray;
begin
  AsyncClient := TNetHTTPClient.Create(nil);
  try
     // Stubbed for debugging syntax error
     ErrMessage := '';
  finally
    AsyncClient.Free;
  end;
end;


function TAiOpenChat.InternalRunImageVideoGeneration(ResMsg, AskMsg: TAiChatMessage): String;
var
  CaptureApiKey: string;
  CaptureUrl: string;
  CapturePrompt: string;
  CaptureModel: string;
  CaptureVideoParams: TStringList;
  LTask: ITask;
begin
  Result := 'Iniciando generación de video...';

  CaptureApiKey := ApiKey;
  CaptureUrl := Url;
  if not CaptureUrl.EndsWith('/') then CaptureUrl := CaptureUrl + '/';

  CapturePrompt := AskMsg.Prompt;
  if CapturePrompt = '' then CapturePrompt := 'Generate a video';

  CaptureModel := Model;
  if not CaptureModel.ToLower.Contains('sora') then CaptureModel := 'sora-2';

  CaptureVideoParams := TStringList.Create;
  if ImageParams.Values['size'] <> '' then CaptureVideoParams.Values['size'] := ImageParams.Values['size'];
  if ImageParams.Values['quality'] <> '' then CaptureVideoParams.Values['quality'] := ImageParams.Values['quality'];
  if ImageParams.Values['style'] <> '' then CaptureVideoParams.Values['style'] := ImageParams.Values['style'];

  LTask := TTask.Run(
    procedure
    begin
      try
        AsyncImageGenWorker(CaptureUrl, CaptureApiKey, CaptureModel, CapturePrompt, CaptureVideoParams, ResMsg, AskMsg);
      finally
        CaptureVideoParams.Free;
      end;
    end);
end;

procedure TAiOpenChat.DoCallFunction(ToolCall: TAiToolsFunction);
begin
  if not Assigned(AiFunctions) then
    Exit;

  // Lógica delegada a AiFunctions (soporta MCP y locales)
  if not AiFunctions.DoCallFunction(ToolCall) then
  begin
    // Fallback para lógica antigua de eventos directos
    if Assigned(FOnCallToolFunction) then
      FOnCallToolFunction(Self, ToolCall);
  end;
end;

function TAiOpenChat.DownLoadFile(aMediaFile: TAiMediaFile): String;
var
  LUrl: string;
  LResponse: IHTTPResponse;
  LStream: TMemoryStream;
  Headers: TNetHeaders;
  TmpClient: TNetHTTPClient;
begin
  Result := '';

  // Validar IDs
  if (aMediaFile.IdFile = '') or (aMediaFile.CloudState = '') then
    Exit;

  // Construir URL
  LUrl := Format('%scontainers/%s/files/%s/content', [Url, aMediaFile.CloudState, aMediaFile.IdFile]);

  LStream := TMemoryStream.Create;
  TmpClient := TNetHTTPClient.Create(nil); // <--- Creamos instancia aislada
  TmpClient.ConfigureForAsync;
  try
    // Configuramos el cliente temporal igual que el principal
    // (Si usas propiedades SSL específicas en FClient, cópialas aquí también)
    TmpClient.ResponseTimeout := 60000;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];

    // Usamos TmpClient en lugar de FClient
    LResponse := TmpClient.Get(LUrl, LStream, Headers);

    if LResponse.StatusCode = 200 then
    begin
      if LStream.Size > 0 then
      begin
        LStream.Position := 0;
        aMediaFile.LoadFromStream(aMediaFile.FileName, LStream);
        Result := aMediaFile.FileName;
      end
      else
        raise Exception.Create('El servidor devolvió un stream vacío.');
    end
    else
      raise Exception.CreateFmt('Error downloading file (%d): %s', [LResponse.StatusCode, LResponse.StatusText]);
  finally
    TmpClient.Free;
    LStream.Free;
  end;
end;

// -----------------------------------------------------------------------------
// MÉTODOS LEGACY / ENDPOINTS SEPARADOS (Audio)
// -----------------------------------------------------------------------------
function TAiOpenChat.InternalRunSpeechGeneration(ResMsg, AskMsg: TAiChatMessage): String;
var
  LUrl, LModel, LVoice, LResponseFormat: string;
  LBodyStream: TStringStream;
  LResponseStream: TMemoryStream;
  LHeaders: TNetHeaders; // <--- Variable ya declarada, ahora la usaremos
  LResponse: IHTTPResponse;
  LJsonObject: TJSonObject;
  LErrorResponse: string;
  LNewAudioFile: TAiMediaFile;
  OldAsc: Boolean;
begin
  Result := ''; // La función Run devuelve el texto, que en este caso es vacío.
  FBusy := True;
  FLastError := '';
  FLastContent := '';
  FLastPrompt := AskMsg.Prompt;

  // 1. Añadir el mensaje del usuario al historial para mantener la consistencia
  if FMessages.IndexOf(AskMsg) < 0 then // Solo lo añadimos si no está ya en la lista
  begin
    AskMsg.Id := FMessages.Count + 1;
    FMessages.Add(AskMsg);
    if Assigned(FOnAddMessage) then
      FOnAddMessage(Self, AskMsg, Nil, AskMsg.Role, AskMsg.Prompt);
  end;

  // 2. Preparar parámetros para la API de TTS
  LUrl := Url + 'audio/speech';
  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);
  // 'tts-1'; // O podrías tener una propiedad específica para el modelo TTS
  LVoice := Self.Voice; // Usamos la propiedad del componente
  LResponseFormat := Self.voice_format; // Usamos la propiedad del componente

  // 3. Construir y ejecutar la petición
  LJsonObject := TJSonObject.Create;
  LBodyStream := nil;
  LResponseStream := TMemoryStream.Create;
  OldAsc := FClient.Asynchronous;

  try
    FClient.Asynchronous := False;

    LJsonObject.AddPair('model', LModel);
    LJsonObject.AddPair('input', AskMsg.Prompt);
    LJsonObject.AddPair('voice', LVoice);
    LJsonObject.AddPair('response_format', LResponseFormat);

    LBodyStream := TStringStream.Create(LJsonObject.ToJson, TEncoding.UTF8);

    LHeaders := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    FClient.ContentType := 'application/json';

    LBodyStream.Position := 0;
{$IFDEF APIDEBUG}
    LBodyStream.SaveToFile('c:\temp\peticionAudio.json.txt');
    LBodyStream.Position := 0;
{$ENDIF}
    LResponse := FClient.Post(LUrl, LBodyStream, LResponseStream, LHeaders);

    // 4. Procesar la respuesta
    if LResponse.StatusCode = 200 then
    begin
      LNewAudioFile := TAiMediaFile.Create;
      try
        LResponseStream.Position := 0;
        LNewAudioFile.LoadFromStream('generated_audio.' + LResponseFormat, LResponseStream);

        ResMsg.MediaFiles.Add(LNewAudioFile);

        DoStateChange(acsFinished, 'Done'); // <--- ESTADO FINALIZADO
        if Assigned(FOnReceiveDataEnd) then
          FOnReceiveDataEnd(Self, ResMsg, nil, 'model', '');
      except
        LNewAudioFile.Free;
        raise;
      end;
    end
    else
    begin
      LErrorResponse := ReadStringFromStream(LResponseStream);
      FLastError := Format('Error generando audio: %d, %s', [LResponse.StatusCode, LErrorResponse]);
      DoError(FLastError);
    end;
  finally

    FClient.Asynchronous := OldAsc;

    LJsonObject.Free;
    LBodyStream.Free;
    LResponseStream.Free;
  end;

  FBusy := False;

end;

function TAiOpenChat.InternalRunTranscription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage): String;
var
  Body: TMultipartFormData;
  Client: TNetHTTPClient;
  Headers: TNetHeaders;
  sUrl: String;
  Res: IHTTPResponse;
  LResponseStream: TMemoryStream;
  LTempStream: TMemoryStream;
  LResponseObj: TJSonObject;
  Granularities: TStringList; // Para procesar las granularidades
  I: Integer;
  LModel: String;
begin
  Result := '';
  if not Assigned(aMediaFile) or (aMediaFile.Content.Size = 0) then
    raise Exception.Create('Se necesita un archivo de audio con contenido para la transcripción.');

  sUrl := Url + 'audio/transcriptions';

  LModel := TAiChatFactory.Instance.GetBaseModel(GetDriverName, Model);

  Client := TNetHTTPClient.Create(Nil);
  Client.ConfigureForAsync;
  LResponseStream := TMemoryStream.Create;
  Body := TMultipartFormData.Create;
  Granularities := TStringList.Create;

  LTempStream := TMemoryStream.Create;
  try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];

    // Crear un stream temporal para pasarlo al formulario multipart
    aMediaFile.Content.Position := 0;
    LTempStream.LoadFromStream(aMediaFile.Content);
    LTempStream.Position := 0;

    // --- 1. CONSTRUCCIÓN DEL BODY MULTIPART CON PARÁMETROS GENÉRICOS ---
    // Usar helper para compatibilidad entre versiones Delphi/FPC
    uHttpHelper.AddStreamToMultipart(Body, 'file', LTempStream, False, aMediaFile.FileName, aMediaFile.MimeType);
    Body.AddField('model', LModel); // Default seguro

    if not AskMsg.Prompt.IsEmpty then
      Body.AddField('prompt', AskMsg.Prompt);

    // Formato de respuesta (genérico, como string)
    if not Self.Transcription_ResponseFormat.IsEmpty then
      Body.AddField('response_format', Self.Transcription_ResponseFormat)
    else
      Body.AddField('response_format', 'json'); // Default a JSON si no se especifica

    // Parámetros opcionales
    if not Self.Language.IsEmpty then
      Body.AddField('language', Self.Language);

    // Usamos la propiedad de Temperatura existente en el componente
    if Self.Temperature > 0 then
      Body.AddField('temperature', FormatFloat('0.0', Self.Temperature));

    // Timestamps Granularities (procesamos la cadena)
    if not Self.Transcription_TimestampGranularities.IsEmpty then
    begin
      // Dividimos la cadena por comas
      Granularities.CommaText := Self.Transcription_TimestampGranularities;
      for I := 0 to Granularities.Count - 1 do
      begin
        // Añadimos cada granularidad como un campo separado con '[]'
        Body.AddField('timestamp_granularities[]', Trim(Granularities[I]));
      end;
    end;

    { TODO : TODAVÍA NO ESTÁ LISTO PARA UTILIZAR LA TRANSCRIPCIÓN EN MODO ASCINCRÓNICO, Falta implementar a futuro }
    // Streaming
    // if Self.Asynchronous then
    // Body.AddField('stream', 'true');

    // --- 2. EJECUCIÓN DE LA PETICIÓN POST ---

    // (La lógica de streaming/síncrono se mantiene igual)
    { if Self.Asynchronous then
      begin
      FClient.Asynchronous := True;
      // La lógica de OnReceiveData se encargará del resto
      FClient.Post(sUrl, Body, FResponse, Headers);
      Result := '';
      end
      else
    }
    begin

      Res := Client.Post(sUrl, Body, LResponseStream, Headers);

      if Res.StatusCode = 200 then
      begin

        LResponseObj := TJSONObject.ParseJSONValue(Res.ContentAsString) as TJSonObject;

        if not Assigned(LResponseObj) then
        begin
          LResponseObj := TJSONObject.Create;
          LResponseObj.AddPair('text', Res.ContentAsString);
        end;

        try
          // Aquí llamas al procedimiento de parseo de transcripciones
          ParseJsonTranscript(LResponseObj, ResMsg, aMediaFile);
        finally
          LResponseObj.Free;
        end;

        Result := ResMsg.Prompt;
      end
      else
      begin
        Raise Exception.CreateFmt('Error en la transcripción: %d, %s', [Res.StatusCode, Res.ContentAsString]);
      end;
    end;

  finally
    Body.Free;
    Client.Free;
    LResponseStream.Free;
    LTempStream.Free;
  end;

end;

procedure TAiOpenChat.NewChat;
begin
  // DeleteAllUploadedFiles; // Primero elimina los archivos de openai
  // En la práctica no guarda los archivos en openai, al menos no los encuentro
  // y el DeleteFile falla.
  FResponseId := ''; // Inicia una nueva conversación
  inherited;
end;

// LogDebug('--OnInternalReceiveData--');
// LogDebug(FResponse.DataString);

procedure TAiOpenChat.OnInternalReceiveData(const Sender: TObject; {$IFDEF FPC}const{$ENDIF} AContentLength, AReadCount: Int64; var AAbort: Boolean);
var
  Line, DataStr, EventType: string;
  JsonEvent, JItem, JResp, JPart, JUsage, JInputDetails, JUsageDetails, JIncomplete: TJSonObject;
  P, OutputIndex: Integer;
  DeltaVal, ItemId, CallId, FuncName, OldArgs: string;
  BufferTool: TJSonObject;
  JContentArr, JAnnotations: TJSonArray;
  JContentPart, JAnno: TJSonObject;
  WebItem: TAiWebSearchItem;
  CurrentMsg, NewStreamMsg: TAiChatMessage;
  TokenCount, UnixDate: Int64;
  BytesBuffer: TBytes;
  SS: TStringStream;
  // FPC Compatibility
  LastM, NewMsg, CallMsg, ResultMsg, ImgMsg, FinalMsg: TAiChatMessage;
  GenImg, GenFile: TAiMediaFile; // Fixed type
  ItemType, ToolName, ShellOutput, ImgBase64, ImgId, ImgPrompt, AnnoType: string;
  JAction, HistoryItem: TJSonObject;
  ToolCall: TAiToolsFunction;
  C, A: Integer;
begin
  if not FClient.Asynchronous then
    Exit;

  LogDebug('--OnInternalReceiveData--');
  LogDebug(FResponse.DataString);

  AAbort := FAbort;
  if FAbort then
  begin
    FBusy := False;
    FTmpToolCallBuffer.Clear;
    DoStateChange(acsAborted, 'Aborted');
    if Assigned(FOnReceiveDataEnd) then
      FOnReceiveDataEnd(Self, nil, nil, 'system', 'abort');
    Exit;
  end;

  // ---------------------------------------------------------------------------
  // 1. Acumulación Robusta (UTF-8)
  // ---------------------------------------------------------------------------
  if FResponse is TStringStream then
  begin
    SS := TStringStream(FResponse);
    if SS.Size > 0 then
    begin
      SetLength(BytesBuffer, SS.Size);
      SS.Position := 0;
      SS.Read(BytesBuffer, 0, SS.Size);
      FTmpResponseText := FTmpResponseText + TEncoding.UTF8.GetString(BytesBuffer);
    end;
  end
  else
    FTmpResponseText := FTmpResponseText + FResponse.DataString;

  FResponse.Clear;

  // ---------------------------------------------------------------------------
  // 2. Procesar líneas completas (Protocolo SSE)
  // ---------------------------------------------------------------------------
  while Pos(#10, FTmpResponseText) > 0 do
  begin
    P := Pos(#10, FTmpResponseText);
    Line := Copy(FTmpResponseText, 1, P - 1);
    Delete(FTmpResponseText, 1, P);

    // Quitar CR (#13) final si existe
    if (Line <> '') and (Line[Length(Line)] = #13) then
      Delete(Line, Length(Line), 1);

    if (Line = '') or Line.StartsWith('event:') or Line.StartsWith(':') then
      Continue;

    if Line.StartsWith('data:') then
    begin
      DataStr := Copy(Line, 6, Length(Line));
      if (DataStr <> '') and (DataStr[1] = ' ') then
        Delete(DataStr, 1, 1);

      if (DataStr = '[DONE]') then
        Continue;

      JsonEvent := TJSONObject.ParseJSONValue(DataStr) as TJSonObject;
      if not Assigned(JsonEvent) then
        Continue;

      try
        if not JsonEvent.TryGetValue('type', EventType) then
          Continue;

        // ---------------------------------------------------------------------
        // A) METADATOS INICIALES Y CREACIÓN DE MENSAJE
        // ---------------------------------------------------------------------
        if EventType = 'response.created' then
        begin
          if JsonEvent.TryGetValue('response', JResp) then
            if JResp.TryGetValue('id', ItemId) then
              FResponseId := ItemId;

          // --- FIX IMPORTANTE: Creación automática del mensaje de respuesta ---
          // Verificamos si el último mensaje es del usuario (con imagen) o ya está completo.
          // Si es así, creamos uno nuevo para el Asistente para no sobrescribir el input.
          LastM := GetLastMessage;

          if (LastM = nil) or (LastM.Role <> 'assistant') then // or (LastM.Status = 'completed') then
          begin
            NewStreamMsg := TAiChatMessage.Create('', 'assistant');
            NewStreamMsg.Id := FMessages.Count + 1;
            NewStreamMsg.PreviousResponseId := FResponseId;
            FMessages.Add(NewStreamMsg);
            // Ahora GetLastMessage apuntará a este nuevo mensaje limpio
          end;
          // ------------------------------------------------------------------

          FRecursionNeeded := False;
          DoStateChange(acsCreated, 'Response ID: ' + FResponseId);
        end

        // ---------------------------------------------------------------------
        // B) TEXTO DEL ASISTENTE (STREAMING)
        // ---------------------------------------------------------------------
        else if EventType = 'response.output_text.delta' then
        begin
          if JsonEvent.TryGetValue('delta', DeltaVal) then
          begin
            // Normalizar saltos de línea para UI (LF -> CRLF en Windows)
            DeltaVal := AdjustLineBreaks(DeltaVal, tlbsCRLF);

            FLastContent := FLastContent + DeltaVal;
            if Assigned(FOnReceiveDataEvent) then
              FOnReceiveDataEvent(Self, nil, JsonEvent, 'assistant', DeltaVal);
          end;
        end

        // ---------------------------------------------------------------------
        // C) TOOLS & THINKING (DETECCION)
        // ---------------------------------------------------------------------
        else if EventType = 'response.output_item.added' then
        begin
          if JsonEvent.TryGetValue('output_index', OutputIndex) and JsonEvent.TryGetValue('item', JItem) then
          begin
            ItemType := JItem.GetValueAsString('type');

            if ItemType = 'function_call' then
            begin
              DoStateChange(acsToolCalling, 'Detected tool call...');
              BufferTool := TJSonObject.Create;
              if JItem.TryGetValue('id', ItemId) then
                BufferTool.AddPair('id', ItemId);
              if JItem.TryGetValue('call_id', CallId) then
                BufferTool.AddPair('call_id', CallId);
              if JItem.TryGetValue('name', FuncName) then
              begin
                BufferTool.AddPair('name', FuncName);
                BufferTool.AddPair('arguments', '');
              end;
              FTmpToolCallBuffer.AddOrSetValue(OutputIndex, BufferTool);
            end
            else if ItemType = 'reasoning' then
              DoStateChange(acsReasoning, 'Thinking...');
          end;
        end

        // ---------------------------------------------------------------------
        // D) TOOL ARGUMENTS (ACUMULACIÓN)
        // ---------------------------------------------------------------------
        else if EventType = 'response.function_call_arguments.delta' then
        begin
          if JsonEvent.TryGetValue('output_index', OutputIndex) and JsonEvent.TryGetValue('delta', DeltaVal) then
          begin
            if FTmpToolCallBuffer.TryGetValue(OutputIndex, BufferTool) then
            begin
              OldArgs := GetJSONStringValue(BufferTool.GetValue('arguments'));
              BufferTool.RemovePair('arguments');
              BufferTool.AddPair('arguments', OldArgs + DeltaVal);
            end;
          end;
        end

        // ---------------------------------------------------------------------
        // E) ITEM DONE (EJECUCIÓN DE TOOLS / IMÁGENES / CITAS)
        // ---------------------------------------------------------------------
        else if EventType = 'response.output_item.done' then
        begin
          if JsonEvent.TryGetValue('output_index', OutputIndex) and JsonEvent.TryGetValue('item', JItem) then
          begin
            if (ItemType = 'function_call') and FTmpToolCallBuffer.TryGetValue(OutputIndex, BufferTool) then
            begin
              ToolName := GetJSONStringValue(BufferTool.GetValue('name'));
              DoStateChange(acsToolExecuting, 'Executing: ' + ToolName);
              ToolCall := TAiToolsFunction.Create;
              ToolCall.Id := GetJSONStringValue(BufferTool.GetValue('call_id'));
              ToolCall.Name := ToolName;
              ToolCall.Arguments := GetJSONStringValue(BufferTool.GetValue('arguments'));
              BufferTool.Free;
              FTmpToolCallBuffer.Remove(OutputIndex);

              DoCallFunction(ToolCall);

              NewMsg := TAiChatMessage.Create(ToolCall.Response, 'tool');
              NewMsg.TollCallId := ToolCall.Id;
              NewMsg.FunctionName := ToolCall.Name;
              NewMsg.PreviousResponseId := FResponseId;
              NewMsg.Id := FMessages.Count + 1;
              FMessages.Add(NewMsg);
              FRecursionNeeded := True;
            end

            // 2. Shell Call
            else if (ItemType = 'shell_call') then
            begin
              if JItem.TryGetValue('call_id', CallId) then
              begin
                if JItem.TryGetValue('action', JAction) then
                begin
                  if Assigned(ShellTool) then
                  begin
                    HistoryItem := TJSonObject.Create;
                    try
                      HistoryItem.AddPair('type', 'shell_call');
                      HistoryItem.AddPair('call_id', CallId);
                      HistoryItem.AddPair('action', JAction.Clone as TJSONValue);
                      CallMsg := TAiChatMessage.Create(HistoryItem.ToString, 'assistant');
                      CallMsg.TollCallId := CallId;
                      CallMsg.PreviousResponseId := FResponseId;
                      CallMsg.Id := FMessages.Count + 1;
                      FMessages.Add(CallMsg);
                    finally
                      HistoryItem.Free;
                    end;

                    ShellOutput := ShellTool.Execute(CallId, JAction);
                    ResultMsg := TAiChatMessage.Create(ShellOutput, 'tool');
                    ResultMsg.TollCallId := CallId;
                    ResultMsg.PreviousResponseId := FResponseId;
                    ResultMsg.Id := FMessages.Count + 1;
                    FMessages.Add(ResultMsg);
                    FRecursionNeeded := True;
                  end;
                end;
              end;
            end

            // 3. Image Generation
            else if (ItemType = 'image_generation_call') then
            begin
              ImgBase64 := '';
              if not JItem.TryGetValue('result', ImgBase64) then
                JItem.TryGetValue('image', ImgBase64);
              if ImgBase64 <> '' then
              begin
                ImgId := '';
                JItem.TryGetValue('id', ImgId);
                if ImgId = '' then
                  ImgId := 'gen_' + IntToStr(TThread.GetTickCount);

                ImgPrompt := '';
                JItem.TryGetValue('revised_prompt', ImgPrompt);

                GenImg := TAiMediaFile.Create;
                GenImg.LoadFromBase64(ImgId + '.png', ImgBase64);
                GenImg.Transcription := ImgPrompt;
                GenImg.IdFile := ImgId;

                ImgMsg := TAiChatMessage.Create('', 'assistant');
                ImgMsg.Prompt := '[Imagen Generada] ' + ImgPrompt;
                ImgMsg.MediaFiles.Add(GenImg);
                ImgMsg.Id := FMessages.Count + 1;
                ImgMsg.PreviousResponseId := FResponseId;
                FMessages.Add(ImgMsg);

                if Assigned(FOnReceiveDataEnd) then
                  FOnReceiveDataEnd(Self, ImgMsg, JItem, 'assistant', ImgMsg.Prompt);
              end;
            end

            // 4. Message (Citas Web/Archivos)
            else if (ItemType = 'message') then
            begin
              CurrentMsg := GetLastMessage;
              if Assigned(CurrentMsg) and JItem.TryGetValue('content', JContentArr) then
              begin
                for C := 0 to JContentArr.Count - 1 do
                begin
                  JContentPart := JContentArr.Items[C] as TJSonObject;
                  if JContentPart.TryGetValue('annotations', JAnnotations) then
                  begin
                    for A := 0 to JAnnotations.Count - 1 do
                    begin
                      JAnno := JAnnotations.Items[A] as TJSonObject;
                      AnnoType := GetJSONStringValue(JAnno.GetValue('type'));

                      if AnnoType = 'url_citation' then
                      begin
                        if not Assigned(CurrentMsg.WebSearchResponse) then
                        begin
                          CurrentMsg.WebSearchResponse := TAiWebSearch.Create;
                          CurrentMsg.WebSearchResponse.&type := 'web_search_results';
                        end;
                        WebItem := TAiWebSearchItem.Create;
                        WebItem.&type := 'url_citation';
                        JAnno.TryGetValue('url', WebItem.Url);
                        JAnno.TryGetValue('title', WebItem.title);
                        CurrentMsg.WebSearchResponse.annotations.Add(WebItem);
                      end
                      else if AnnoType = 'container_file_citation' then
                      begin
                        GenFile := TAiMediaFile.Create;
                        if JAnno.TryGetValue('file_id', ItemId) then
                          GenFile.IdFile := ItemId;
                        if JAnno.TryGetValue('filename', FuncName) then
                          GenFile.FileName := FuncName;
                        if JAnno.TryGetValue('container_id', CallId) then
                          GenFile.CloudState := CallId;
                        try
                          DownLoadFile(GenFile);
                        except
                        end;
                        CurrentMsg.MediaFiles.Add(GenFile);
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end

        // ---------------------------------------------------------------------
        // F) FINALIZACIÓN (METADATOS DE COSTOS, EXTRACTION Y RECURSIÓN)
        // ---------------------------------------------------------------------
        else if EventType = 'response.completed' then
        begin
          if FRecursionNeeded then
          begin
            DoStateChange(acsConnecting, 'Sending tool results...');
            TThread.ForceQueue(nil,
              TThreadProcedure(procedure
              begin
                Self.Run(TAiChatMessage(nil), TAiChatMessage(nil));
              end));
          end
          else
          begin
            FBusy := False;
            FinalMsg := GetLastMessage;
            if Assigned(FinalMsg) and JsonEvent.TryGetValue('response', JResp) then
            begin
              if JResp.TryGetValue('usage', JUsage) then
              begin
                // Tokens normales (Usando los nombres de tu clase)
                if JUsage.TryGetValue('total_tokens', TokenCount) then
                  FinalMsg.Total_tokens := TokenCount;
                if JUsage.TryGetValue('input_tokens', TokenCount) then
                  FinalMsg.Prompt_tokens := TokenCount;
                if JUsage.TryGetValue('output_tokens', TokenCount) then
                  FinalMsg.Completion_tokens := TokenCount;

                // Tokens Caché
                if JUsage.TryGetValue('input_tokens_details', JInputDetails) then
                  if JInputDetails.TryGetValue('cached_tokens', TokenCount) then
                    FinalMsg.cached_tokens := TokenCount;

                // Tokens Reasoning
                if JUsage.TryGetValue('output_tokens_details', JUsageDetails) then
                  if JUsageDetails.TryGetValue('reasoning_tokens', TokenCount) then
                    FinalMsg.Thinking_tokens := TokenCount;
              end;
              if JResp.TryGetValue('model', DeltaVal) then
                FinalMsg.Model := DeltaVal;
            end;

            // --- Extracción de código a archivos (MarkdownCodeExtractor) ---
            // --- Extracción de código a archivos (MarkdownCodeExtractor) ---
            { TODO: Markdown extraction unit is missing in this branch
            If tfc_ExtracttextFile in NativeOutputFiles then
            Begin
              // Code commented out because uMakerAi.Tools.Markdown is missing
            End;
            }
            // --------------------------------------------------------------

            DoStateChange(acsFinished, 'Completed');
            if Assigned(FOnReceiveDataEnd) then
              FOnReceiveDataEnd(Self, FinalMsg, nil, 'assistant', FLastContent);
          end;
        end

        // --- G) ERRORES ---
        else if EventType = 'error' then
          DoStateChange(acsError, JsonEvent.ToString);

      finally
        JsonEvent.Free;
      end;
    end;
  end;
end;

{ TAiOpenAiEmbeddings }

constructor TAiOpenAiEmbeddings.Create(AOwner: TComponent);
begin
  inherited;
  FApiKey := '@OPENAI_API_KEY';
  FUrl := GlOpenAIUrl;
  FDimensions := 1536;
  FModel := 'text-embedding-3-small';
end;

function TAiOpenAiEmbeddings.CreateEmbedding(aInput, aUser: String; aDimensions: Integer; aModel, aEncodingFormat: String): TAiEmbeddingData;
Var
  Client: TNetHTTPClient;
  Headers: TNetHeaders;
  jObj: TJSonObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
begin

  Client := TNetHTTPClient.Create(Nil);
  Client.ConfigureForAsync;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := FUrl + 'embeddings';
  jObj := TJSonObject.Create;

  If aModel = '' then
    aModel := FModel;

  if aDimensions <= 0 then
    aDimensions := FDimensions;

  Try
    jObj.AddPair('input', aInput); // Este se adiciona por compatibilidad con ollama
    jObj.AddPair('prompt', aInput);
    jObj.AddPair('model', aModel);
    jObj.AddPair('user', aUser);
    jObj.AddPair('dimensions', aDimensions);
    jObj.AddPair('encoding_format', aEncodingFormat);

    St.WriteString(jObj.ToJson);
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v2')];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);
    Response.Position := 0;

{$IFDEF APIDEBUG}
    Response.SaveToFile('c:\temp\response.txt');
{$ENDIF}
    if Res.StatusCode = 200 then
    Begin
      jObj := TJSonObject(TJSONObject.ParseJSONValue(Res.ContentAsString));
      ParseEmbedding(jObj);
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
    jObj.Free;
  End;
end;

destructor TAiOpenAiEmbeddings.Destroy;
begin

  inherited;
end;

procedure TAiOpenAiEmbeddings.ParseEmbedding(jObj: TJSonObject);
Var
  JArr, jData: TJSonArray;
  Emb: TAiEmbeddingData;
  JVal: TJSONValue;
  j: Integer;
  Usage: TJSonObject;

begin
  jObj.TryGetValue('model', FModel);

  If jObj.TryGetValue('usage', Usage) then
  Begin
    Usage.TryGetValue('prompt_tokens', Fprompt_tokens);
    Usage.TryGetValue('total_tokens', Ftotal_tokens);
  End;

  jData := jObj.GetValue('data') as TJSonArray;

  SetLength(FData, jData.Count);

  // var i := 0;
  For JVal in jData do
  Begin
    // El embedding de OpenAi Retorna un array, pero solo se toma el primero de la fila
    JArr := TJSonObject(JVal).GetValue('embedding') as TJSonArray;
    j := JArr.Count;
    SetLength(Emb, j);
    // FillChar(Emb, Length(Emb) * SizeOf(Double), 0);

    For j := 0 to JArr.Count - 1 do
      Emb[j] := (JArr.Items[j] as TJSONNumber).AsDouble;

    FData := Emb;
    // Inc(i);
    Break; // Si el embedding de OpenAI retorna varios solo tomamos el primero, usualmente solo hay uno
  End;
end;

Initialization

TAiChatFactory.Instance.RegisterDriver(TAiOpenChat);

end.











