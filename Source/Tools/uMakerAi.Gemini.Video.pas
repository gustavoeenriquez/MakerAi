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


unit uMakerAi.Gemini.Video;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Net.HttpClient,
  System.Net.HttpClientComponent, System.Net.URLClient, System.Threading,
  System.IOUtils, uMakerAi.Core, uMakerAi.Chat.Tools, uMakerAi.Chat, uMakerAi.Chat.Messages;

type
  { Enums para una mejor experiencia en el Inspector de Objetos }
  TVeoAspectRatio = (arDefault, ar16_9, ar9_16);
  TVeoResolution = (vrDefault, vr720p, vr1080p);
  TVeoPersonGeneration = (pgDefault, pgAllowAll, pgAllowAdult, pgDontAllow);

  //[ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidOSX64 or pidiOSArm64 or pidAndroidArm64)]
  TAiGeminiVideoTool = class(TAiVideoToolBase)
  private
    FApiKey: string;
    FModel: string;
    FUrl: string;
    FAspectRatio: TVeoAspectRatio;
    FResolution: TVeoResolution;
    FDurationSeconds: Integer;
    FPersonGeneration: TVeoPersonGeneration;
    FNegativePrompt: string;
    FSeed: Int64;

    function GetApiKey: string;
    function BuildParametersJson: TJSONObject;
    function DownloadVideo(const AVideoUri: string): TAiMediaFile;
  protected
    { Implementación de IAiVideoTool }
    procedure ExecuteVideoGeneration(ResMsg, AskMsg: TAiChatMessage); override;

    { Lógica interna de ejecución y Polling }
    procedure InternalRunVeo(AResMsg, AAskMsg: TAiChatMessage);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ApiKey: string read GetApiKey write FApiKey;
    property Model: string read FModel write FModel;
    property Url: string read FUrl write FUrl;

    { Propiedades de configuración de Veo }
    property AspectRatio: TVeoAspectRatio read FAspectRatio write FAspectRatio default ar16_9;
    property Resolution: TVeoResolution read FResolution write FResolution default vr720p;
    property DurationSeconds: Integer read FDurationSeconds write FDurationSeconds default 8;
    property PersonGeneration: TVeoPersonGeneration read FPersonGeneration write FPersonGeneration default pgAllowAll;
    property NegativePrompt: string read FNegativePrompt write FNegativePrompt;
    property Seed: Int64 read FSeed write FSeed default 0;
  end;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiGeminiVideoTool]);
end;


{ TAiGeminiVideoTool }

constructor TAiGeminiVideoTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FApiKey := '@GEMINI_API_KEY';
  FModel := 'veo-3.1-generate-preview';
  FUrl := 'https://generativelanguage.googleapis.com/v1beta/';
  FAspectRatio := ar16_9;
  FResolution := vr720p;
  FDurationSeconds := 8;
  FPersonGeneration := pgAllowAll;
end;

function TAiGeminiVideoTool.GetApiKey: string;
begin
  if (csDesigning in ComponentState) or (csDestroying in ComponentState) then
    Exit(FApiKey);

  if (FApiKey <> '') and (FApiKey.StartsWith('@')) then
    Result := GetEnvironmentVariable(Copy(FApiKey, 2, MaxInt))
  else
    Result := FApiKey;
end;

function TAiGeminiVideoTool.BuildParametersJson: TJSONObject;
begin
  Result := TJSONObject.Create;
  case FAspectRatio of
    ar16_9: Result.AddPair('aspectRatio', '16:9');
    ar9_16: Result.AddPair('aspectRatio', '9:16');
  end;

  case FResolution of
    vr720p: Result.AddPair('resolution', '720p');
    vr1080p: Result.AddPair('resolution', '1080p');
  end;

  if FDurationSeconds > 0 then
    Result.AddPair('durationSeconds', TJSONNumber.Create(FDurationSeconds));

  case FPersonGeneration of
    pgAllowAll: Result.AddPair('personGeneration', 'allow_all');
    pgAllowAdult: Result.AddPair('personGeneration', 'allow_adult');
    pgDontAllow: Result.AddPair('personGeneration', 'dont_allow');
  end;

  if not FNegativePrompt.IsEmpty then
    Result.AddPair('negativePrompt', FNegativePrompt);

  if FSeed > 0 then
    Result.AddPair('seed', TJSONNumber.Create(FSeed));
end;

procedure TAiGeminiVideoTool.ExecuteVideoGeneration(ResMsg, AskMsg: TAiChatMessage);
begin
  // Veo SIEMPRE debe ser asíncrono debido a que tarda minutos
  TTask.Run(procedure
  begin
    try
      InternalRunVeo(TAiChatMessage(ResMsg), TAiChatMessage(AskMsg));
    except
      on E: Exception do ReportError('Error en Veo Tool: ' + E.Message, E);
    end;
  end);
end;

procedure TAiGeminiVideoTool.InternalRunVeo(AResMsg, AAskMsg: TAiChatMessage);
var
  HTTP: TNetHTTPClient;
  LUrl, LOpName, LPollingUrl, LVideoUri: string;
  LRequest, LInstance, LImagePart: TJSONObject;
  LInstances: TJSONArray;
  LResponse: IHTTPResponse;
  LBody: TStringStream;
  LJSON, LErrorObj: TJSONObject;
  LIsDone: Boolean;
  LMediaArr: TAiMediaFilesArray;
  LVideoFile: TAiMediaFile;
begin
  HTTP := TNetHTTPClient.Create(nil);
  LRequest := TJSONObject.Create;
  try
    // 1. Construir Petición Inicial
    LInstances := TJSONArray.Create;
    LInstance := TJSONObject.Create;
    LInstance.AddPair('prompt', AAskMsg.Prompt);

    // Soporte para Image-to-Video (si hay una imagen en el mensaje)
    LMediaArr := AAskMsg.MediaFiles.GetMediaList([Tfc_Image], False);
    if Length(LMediaArr) > 0 then
    begin
      LImagePart := TJSONObject.Create;
      if not LMediaArr[0].UrlMedia.IsEmpty then
        LImagePart.AddPair('uri', LMediaArr[0].UrlMedia)
      else
        LImagePart.AddPair('bytesBase64Encoded', LMediaArr[0].Base64);

      LImagePart.AddPair('mimeType', LMediaArr[0].MimeType);
      LInstance.AddPair('image', LImagePart);
    end;

    LInstances.Add(LInstance);
    LRequest.AddPair('instances', LInstances);
    LRequest.AddPair('parameters', BuildParametersJson);

    // 2. Iniciar Operación (predictLongRunning)
    LUrl := Format('%smodels/%s:predictLongRunning?key=%s', [FUrl, FModel, GetApiKey]);
    LBody := TStringStream.Create(LRequest.ToJSON, TEncoding.UTF8);
    HTTP.ContentType := 'application/json';

    ReportState(acsReasoning, 'Iniciando generación de video Veo...');
    LResponse := HTTP.Post(LUrl, LBody);

    if LResponse.StatusCode <> 200 then
      raise Exception.Create('Error iniciando Veo: ' + LResponse.ContentAsString);

    LJSON := TJSONObject.ParseJSONValue(LResponse.ContentAsString) as TJSONObject;
    LOpName := LJSON.GetValue<string>('name', '');
    LJSON.Free;

    if LOpName.IsEmpty then
      raise Exception.Create('No se recibió el nombre de la operación.');

    // 3. Bucle de Polling
    LPollingUrl := FUrl + LOpName + '?key=' + GetApiKey;
    LIsDone := False;

    while not LIsDone do
    begin
      Sleep(10000); // Esperar 10 segundos
      ReportState(acsReasoning, 'Procesando video en los servidores de Google...');

      LResponse := HTTP.Get(LPollingUrl);
      if LResponse.StatusCode = 200 then
      begin
        LJSON := TJSONObject.ParseJSONValue(LResponse.ContentAsString) as TJSONObject;
        try
          LJSON.TryGetValue<Boolean>('done', LIsDone);
          if LIsDone then
          begin
            if LJSON.TryGetValue<TJSONObject>('error', LErrorObj) then
              raise Exception.Create('Error en la operación: ' + LErrorObj.ToJSON);

            LVideoUri := LJSON.GetValue<string>('response.generateVideoResponse.generatedSamples[0].video.uri', '');
          end;
        finally
          LJSON.Free;
        end;
      end;
    end;

    // 4. Descargar Video Final
    if not LVideoUri.IsEmpty then
    begin
      ReportState(acsWriting, 'Descargando video generado...');
      LVideoFile := DownloadVideo(LVideoUri);
      if Assigned(LVideoFile) then
      begin
        AResMsg.MediaFiles.Add(LVideoFile);
        ReportDataEnd(AResMsg, 'assistant', '[Video generado exitosamente]');
      end;
    end;

  finally
    LRequest.Free;
    HTTP.Free;
    LBody.Free;
  end;
end;

function TAiGeminiVideoTool.DownloadVideo(const AVideoUri: string): TAiMediaFile;
var
  HTTP: TNetHTTPClient;
  Resp: IHTTPResponse;
  Stream: TMemoryStream;
  Headers: TNetHeaders;
begin
  Result := nil;
  HTTP := TNetHTTPClient.Create(nil);
  Stream := TMemoryStream.Create;
  try
    Headers := [TNetHeader.Create('x-goog-api-key', GetApiKey)];
    Resp := HTTP.Get(AVideoUri, Stream, Headers);
    if Resp.StatusCode = 200 then
    begin
      Stream.Position := 0;
      Result := TAiMediaFile.Create;
      Result.UrlMedia := AVideoUri;
      Result.LoadFromStream('veo_video.mp4', Stream);
    end;
  finally
    HTTP.Free;
    if not Assigned(Result) then Stream.Free;
  end;
end;

end.
