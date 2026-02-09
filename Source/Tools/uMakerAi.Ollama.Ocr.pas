unit uMakerAi.Ollama.Ocr;

{$INCLUDE ../CompilerDirectives.inc}

interface

uses
  {$IFDEF FPC}
  Classes, SysUtils, StrUtils, Generics.Collections, Types, Variants, SyncObjs, Math,
  {$ELSE}
  System.SysUtils, System.Classes, System.JSON, System.Net.HttpClient,
  System.Net.HttpClientComponent, System.Net.URLClient, System.Threading,
  System.StrUtils,
  {$ENDIF}
  uMakerAi.Core,
  uMakerAi.Chat.Tools,
  uMakerAi.Chat.Messages,
  uJsonHelper, uHttpHelper, uSysUtilsHelper, uBase64Helper, uThreadingHelper, uRttiHelper;

type
  { TAiOllamaOcrTool: Herramienta de OCR utilizando modelos de visión en Ollama }

  TAiOllamaOcrTool = class(TAiVisionToolBase)
  private
    FUrl: string;
    FModel: string;
    FPrompt: string;
    FKeepAlive: string;
    FStream: Boolean;
    FApiKey: string;
    FTimeout: Integer;
    function GetApiKey: string;
    procedure SetTimeout(const Value: Integer);
  protected
    procedure ExecuteImageDescription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage); override;
    function InternalRunOllamaOCR(aMediaFile: TAiMediaFile; ResMsg: TAiChatMessage; const AOverridePrompt: string): string;
  public
    constructor Create(AOwner: TComponent); override;

    { Métodos Estáticos }
    class function ExtractText(AMediaFile: TAiMediaFile; const APrompt: string = ''; const AUrl: string = ''): string;
    class function ExtractTextFromFile(const AFilePath: string; const APrompt: string = ''; const AUrl: string = ''): string;
    class function ExtractTextFromStream(AStream: TStream; const AFileName: string; const APrompt: string = ''; const AUrl: string = ''): string;

  published
    property ApiKey: string read FApiKey write FApiKey;
    property Url: string read FUrl write FUrl;
    property Model: string read FModel write FModel;
    property KeepAlive: string read FKeepAlive write FKeepAlive;
    property Stream: Boolean read FStream write FStream default False;
    property Prompt: string read FPrompt write FPrompt;
    Property Timeout : Integer read FTimeout write SetTimeout;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiOllamaOcrTool]);
end;

{ TAiOllamaOcrTool }

constructor TAiOllamaOcrTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Importante: La URL base. El InternalRun se encargará de asegurar el endpoint /api/chat
  FUrl := 'http://localhost:11434/';
  FModel := 'deepseek-ocr:latest';
  FPrompt := '<|grounding|>Convert the document to markdown';
  FKeepAlive := '1m';
  FTimeout := 60000;
  FStream := False;
end;

function TAiOllamaOcrTool.GetApiKey: string;
begin
  if (csDesigning in ComponentState) then Exit(FApiKey);
  if FApiKey.StartsWith('@') then
    Result := CompatGetEnvVar(Copy(FApiKey, 2, MaxInt))
  else
    Result := FApiKey;
end;

procedure TAiOllamaOcrTool.ExecuteImageDescription(aMediaFile: TAiMediaFile; ResMsg, AskMsg: TAiChatMessage);
begin
  if IsAsync then
    TTask.Run(procedure begin InternalRunOllamaOCR(aMediaFile, ResMsg, AskMsg.Prompt); end)
  else
    InternalRunOllamaOCR(aMediaFile, ResMsg, AskMsg.Prompt);
end;

function TAiOllamaOcrTool.InternalRunOllamaOCR(aMediaFile: TAiMediaFile; ResMsg: TAiChatMessage; const AOverridePrompt: string): string;
var
  HTTP: TNetHTTPClient;
  LBody: TStringStream;
  LRequestJson, LOptions, LMessage, LMsgResponse: TJSONObject;
  LMessagesArray, LImagesArray: TJSONArray;
  LResponse: IHTTPResponse;
  LResponseJson: TJSONObject;
  LFinalPrompt, LContent, LActualApiKey, LFullUrl: string;
begin
  Result := '';
  if not Assigned(aMediaFile) or not Assigned(ResMsg) then Exit;

  // 1. Asegurar Endpoint correcto (/api/chat)
  LFullUrl := FUrl + 'api/chat';

  LFinalPrompt := IfThen(AOverridePrompt.IsEmpty, FPrompt, AOverridePrompt);
  if LFinalPrompt.IsEmpty then LFinalPrompt := '<|grounding|>Convert the document to markdown';

  //El prompt para DeepSeek OCR es muy estricto,  debe ir un #10 y luego el texto sin espacio
  //El texto del prompt debe ser en chino o en inglés no acepta prompts en otro idioma
  //El texto del pdf o imágen si puede estar en español o en otros idiomas
  //se utiliza por defecto <|grounding|>  para que extraiga también los boundings de los textos
  //Los boundings son proporcionales, no corresponden al valor real, es necesario escalarlos de acuerdo
  //al tamaño de la imagen
  //LFinalPrompt := ' Free OCR';  //Ok algunos documentos
  //LFinalPrompt := #10+'<|grounding|>Convert the document to markdown'; //el más preciso extrae los boundings y los textos con presición

  LFinalPrompt := #10 + LFinalPrompt;

  HTTP := TNetHTTPClient.Create(nil);
  HTTP.ConfigureForAsync;
  LRequestJson := TJSONObject.Create;
  try
    LActualApiKey := GetApiKey;

    // A. Payload Raíz
    LRequestJson.AddPair('model', FModel);
    LRequestJson.AddPair('stream', CreateJSONBool(FStream));
    LRequestJson.AddPair('keep_alive', FKeepAlive);

    // B. Estructura de Mensajes (Igual a tu TAiOllamaChat.GetMessages)
    LMessagesArray := TJSONArray.Create;
    LMessage := TJSONObject.Create;
    LMessage.AddPair('role', 'user');
    LMessage.AddPair('content', LFinalPrompt);

    LImagesArray := TJSONArray.Create;
    LImagesArray.Add(aMediaFile.Base64);
    LMessage.AddPair('images', LImagesArray);

    LMessagesArray.Add(LMessage);
    LRequestJson.AddPair('messages', LMessagesArray);

    // C. Opciones (Crucial para DeepSeek-OCR)
    LOptions := TJSONObject.Create;
    LOptions.AddPair('temperature', CreateJSONNumber(0.12));
    LOptions.AddPair('num_predict', CreateJSONNumber(16000));
    LOptions.AddPair('top_p', CreateJSONNumber(1.0));
    LRequestJson.AddPair('options', LOptions);


    LBody := TStringStream.Create(LRequestJson.ToJSON, TEncoding.UTF8);
    try
      HTTP.ContentType := 'application/json';
      HTTP.ResponseTimeout := FTimeOut; // 5 minutos (como en tu Chat)

      if not LActualApiKey.IsEmpty then
        HTTP.SetHeader('Authorization', 'Bearer ' + LActualApiKey);

      ReportState(acsWriting, 'Ollama analizando imagen...');
      LResponse := HTTP.Post(LFullUrl, LBody);

       if LResponse.StatusCode = 200 then
      begin
        LResponseJson := TJSONObject.ParseJSONValue(LResponse.ContentAsString) as TJSONObject;
        try
          if Assigned(LResponseJson) then
          begin
            // Ollama /api/chat devuelve { "message": { "content": "..." } }
            if LResponseJson.TryGetValue('message', LMsgResponse) then
              LContent := LMsgResponse.GetValueAsString('content', '')
            else
              LContent := LResponseJson.GetValueAsString('response', '');

            ResMsg.Content := LContent;
            ResMsg.Role := 'assistant';
            ResMsg.Model := FModel;

            aMediaFile.Procesado := True;
            aMediaFile.Transcription := LContent;
            Result := LContent;

            ReportDataEnd(ResMsg, 'assistant', LContent, LResponseJson);
            ReportState(acsFinished, 'OCR Completado');
          end;
        finally
          LResponseJson.Free;
        end;
      end
      else
        ReportError(Format('Ollama HTTP %d: %s', [LResponse.StatusCode, LResponse.ContentAsString]));
    finally
      LBody.Free;
    end;
  finally
    LRequestJson.Free;
    HTTP.Free;
  end;
end;

procedure TAiOllamaOcrTool.SetTimeout(const Value: Integer);
begin
  If Value <= 30000 then
     FTimeOut := 30000
  Else
    FTimeout := Value;
end;

{ --- MÉTODOS ESTÁTICOS --- }

class function TAiOllamaOcrTool.ExtractText(AMediaFile: TAiMediaFile; const APrompt, AUrl: string): string;
var
  LInstance: TAiOllamaOcrTool;
  LResMsg: TAiChatMessage;
begin
  Result := '';
  if not Assigned(AMediaFile) then Exit;
  LInstance := TAiOllamaOcrTool.Create(nil);
  LResMsg := TAiChatMessage.Create('', 'assistant');
  LResMsg.Prompt := '';
  LResMsg.Role := 'assistant';
  try
    if AUrl <> '' then LInstance.Url := AUrl;
    Result := LInstance.InternalRunOllamaOCR(AMediaFile, LResMsg, APrompt);
  finally
    LResMsg.Free;
    LInstance.Free;
  end;
end;

class function TAiOllamaOcrTool.ExtractTextFromFile(const AFilePath, APrompt, AUrl: string): string;
var
  LMedia: TAiMediaFile;
begin
  Result := '';
  if not FileExists(AFilePath) then Exit;
  LMedia := TAiMediaFile.Create;
  try
    LMedia.LoadFromFile(AFilePath);
    Result := ExtractText(LMedia, APrompt, AUrl);
  finally
    LMedia.Free;
  end;
end;

class function TAiOllamaOcrTool.ExtractTextFromStream(AStream: TStream; const AFileName, APrompt, AUrl: string): string;
var
  LMedia: TAiMediaFile;
  LMemStream: TMemoryStream;
begin
  Result := '';
  if not Assigned(AStream) then Exit;
  LMedia := TAiMediaFile.Create;
  LMemStream := TMemoryStream.Create;
  try
    AStream.Position := 0;
    LMemStream.LoadFromStream(AStream);
    LMedia.LoadFromStream(AFileName, LMemStream);
    Result := ExtractText(LMedia, APrompt, AUrl);
  finally
    LMemStream.Free;
    LMedia.Free;
  end;
end;

end.
