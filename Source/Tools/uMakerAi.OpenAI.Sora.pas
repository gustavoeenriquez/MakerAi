// MIT License
//
// Copyright (c) 2024 Gustavo Enríquez
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
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.OpenAI.Sora;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Threading,
  System.JSON, System.Net.HttpClient, System.Net.URLClient,
  uMakerAi.Core, System.Net.HttpClientComponent;

type
  // Enums para los parámetros de Sora
  TSoraModel = (smSora2, smCustom);
  TSoraResolution = (srDefault, sr1280x720, sr720x1280, sr1920x1080, sr1080x1920);

  // Eventos
  TProgressEvent = procedure(Sender: TObject; const StatusMessage: string) of object;
  TGenerationSuccessEvent = procedure(Sender: TObject; ResultVideo: TAiMediaFile) of object;
  TGenerationErrorEvent = procedure(Sender: TObject; const ErrorMessage: string) of object;

  // --- Componente Especializado para SORA ---
  TAiSoraGenerator = class(TComponent)
  private
    // Propiedades
    FApiKey: string;
    FModel: TSoraModel;
    FCustomModelName: string;
    FResolution: TSoraResolution;
    FSeconds: Integer;
    FSeed: Int64;

    // Eventos
    FOnProgress: TProgressEvent;
    FOnSuccess: TGenerationSuccessEvent;
    FOnError: TGenerationErrorEvent;

    // Métodos internos
    function GetEffectiveModelName: string;
    function GetResolutionString: string;
    procedure DoError(const AMessage: string);
    procedure DoProgress(const AMessage: string);
    procedure DoSuccess(AVideoFile: TAiMediaFile);

    procedure InternalExecuteGeneration(const APrompt: string; AInputImage: TAiMediaFile = nil);
    procedure InternalExecuteRemix(const APrompt: string; const AVideoIdToRemix: string);

    procedure PollAndDownloadVideoJob(AHttpClient: TNetHTTPClient; const AVideoJobId: string; const AInitialStatus: string; const AHeaders: TNetHeaders);
    function DownloadVideoContent(AHttpClient: TNetHTTPClient; const AVideoJobId: string; const AHeaders: TNetHeaders): TAiMediaFile;
    function GetApiKey: string;

  public
    constructor Create(AOwner: TComponent); override;

    // Métodos Públicos Asíncronos
    function GenerateFromText(const APrompt: string): ITask;
    function GenerateFromImage(const APrompt: string; AImage: TAiMediaFile): ITask;
    function RemixVideo(const APrompt: string; const AOriginalVideoId: string): ITask;

  published
    property ApiKey: string read GetApiKey write FApiKey;

    property Model: TSoraModel read FModel write FModel default TSoraModel.smSora2;
    property CustomModelName: string read FCustomModelName write FCustomModelName;

    property Resolution: TSoraResolution read FResolution write FResolution default TSoraResolution.sr720x1280;
    property Seconds: Integer read FSeconds write FSeconds default 4;
    property Seed: Int64 read FSeed write FSeed default 0;

    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property OnSuccess: TGenerationSuccessEvent read FOnSuccess write FOnSuccess;
    property OnError: TGenerationErrorEvent read FOnError write FOnError;
  end;

procedure Register;

implementation

uses
  System.IOUtils, System.NetConsts, System.NetEncoding, REST.JSON, System.Net.Mime;

const
  OPENAI_API_BASE_URL = 'https://api.openai.com/v1/';

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiSoraGenerator]);
end;

{ TAiSoraGenerator }

constructor TAiSoraGenerator.Create(AOwner: TComponent);
begin
  inherited;
  FApiKey := '@OPENAI_API_KEY';

  FModel := TSoraModel.smSora2;
  FResolution := TSoraResolution.sr720x1280;
  FSeconds := 4;
  FSeed := 0;
end;

procedure TAiSoraGenerator.DoError(const AMessage: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FOnError) then
        FOnError(Self, AMessage);
    end);
end;

procedure TAiSoraGenerator.DoProgress(const AMessage: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FOnProgress) then
        FOnProgress(Self, AMessage);
    end);
end;

procedure TAiSoraGenerator.DoSuccess(AVideoFile: TAiMediaFile);
begin
  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FOnSuccess) then
        FOnSuccess(Self, AVideoFile);
    end);
end;

function TAiSoraGenerator.DownloadVideoContent(AHttpClient: TNetHTTPClient; const AVideoJobId: string; const AHeaders: TNetHeaders): TAiMediaFile;
var
  LUrl: string;
  LResponse: IHTTPResponse;
  LVideoStream: TMemoryStream;
begin
  Result := nil;
  LUrl := TPath.Combine(OPENAI_API_BASE_URL, 'videos/' + AVideoJobId + '/content');
  LVideoStream := TMemoryStream.Create;
  try
    LResponse := AHttpClient.Get(LUrl, LVideoStream, AHeaders);
    if LResponse.StatusCode = 200 then
    begin
      LVideoStream.Position := 0;
      Result := TAiMediaFile.Create;
      Result.CloudName := AVideoJobId;
      Result.LoadFromStream(AVideoJobId + '.mp4', LVideoStream);
    end
    else
    begin
      raise Exception.CreateFmt('Error downloading video content: %d %s', [LResponse.StatusCode, LResponse.ContentAsString(TEncoding.UTF8)]);
    end;
  finally
    if Result = nil then
      LVideoStream.Free;
  end;
end;

function TAiSoraGenerator.GenerateFromImage(const APrompt: string; AImage: TAiMediaFile): ITask;
var
  LImageClone: TAiMediaFile;
begin
  if not Assigned(AImage) or not Assigned(AImage.Content) or (AImage.Content.Size = 0) then
    raise Exception.Create('An image with content is required.');

  LImageClone := TAiMediaFile.Create;
  LImageClone.Assign(AImage);

  Result := TTask.Run(
    procedure
    begin
      try
        InternalExecuteGeneration(APrompt, LImageClone);
      finally
        LImageClone.Free;
      end;
    end);
end;

function TAiSoraGenerator.GenerateFromText(const APrompt: string): ITask;
begin
  if APrompt.IsEmpty then
    raise Exception.Create('A non-empty prompt is required.');

  Result := TTask.Run(
    procedure
    begin
      InternalExecuteGeneration(APrompt);
    end);
end;

function TAiSoraGenerator.GetApiKey: string;
Begin
  if (csDesigning in ComponentState) or (csDestroying in ComponentState) then
  begin
    Result := FApiKey;
    Exit;
  end;

  if (FApiKey <> '') and (Copy(FApiKey, 1, 1) = '@') then
    Result := GetEnvironmentVariable(Copy(FApiKey, 2, Length(FApiKey)))
  else
    Result := FApiKey;
end;

function TAiSoraGenerator.GetEffectiveModelName: string;
begin
  if not FCustomModelName.IsEmpty then
  begin
    Result := FCustomModelName;
    Exit;
  end;

  case FModel of
    smSora2: Result := 'sora-2';
  else
    raise Exception.Create('Model must be specified.');
  end;
end;

function TAiSoraGenerator.GetResolutionString: string;
begin
  Result := '';
  case FResolution of
    sr1280x720: Result := '1280x720';
    sr720x1280: Result := '720x1280';
    sr1920x1080: Result := '1920x1080';
    sr1080x1920: Result := '1080x1920';
  end;
end;

procedure TAiSoraGenerator.InternalExecuteGeneration(const APrompt: string; AInputImage: TAiMediaFile);
var
  LHttpClient: TNetHTTPClient;
  LUrl: string;
  LResponse: IHTTPResponse;
  LFormData: TMultipartFormData;
  LResponseObj: TJSONObject;
  LVideoJobId, LStatus: string;
  LHeaders: TNetHeaders;
begin
  LHttpClient := TNetHTTPClient.Create(nil);
  LFormData := TMultipartFormData.Create;
  try
    try
      LUrl := TPath.Combine(OPENAI_API_BASE_URL, 'videos');

      LHeaders := [TNetHeader.Create('Authorization', 'Bearer ' + Self.ApiKey)];

      LFormData.AddField('prompt', APrompt);
      LFormData.AddField('model', GetEffectiveModelName);
      if FSeconds > 0 then
        LFormData.AddField('seconds', FSeconds.ToString);
      var LSizeStr := GetResolutionString;
      if not LSizeStr.IsEmpty then
        LFormData.AddField('size', LSizeStr);

      if Assigned(AInputImage) then
      begin
        AInputImage.Content.Position := 0;
        LFormData.AddStream('input_reference', AInputImage.Content, False, AInputImage.Filename, AInputImage.MimeType);
      end;

      LResponse := LHttpClient.Post(LUrl, LFormData, nil, LHeaders);

      if (LResponse.StatusCode <> 200) then
        raise Exception.CreateFmt('Error starting video generation: %d, %s', [LResponse.StatusCode, LResponse.ContentAsString]);

      LResponseObj := TJSONObject.ParseJSONValue(LResponse.ContentAsString) as TJSONObject;
      try
        if not LResponseObj.TryGetValue<string>('id', LVideoJobId) then
          raise Exception.Create('Video Job ID not found in API response.');
        LResponseObj.TryGetValue<string>('status', LStatus);
      finally
        LResponseObj.Free;
      end;

      PollAndDownloadVideoJob(LHttpClient, LVideoJobId, LStatus, LHeaders);

    except
      on E: Exception do
        DoError(E.Message);
    end;
  finally
    LFormData.Free;
    LHttpClient.Free;
  end;
end;

procedure TAiSoraGenerator.InternalExecuteRemix(const APrompt: string; const AVideoIdToRemix: string);
var
  LHttpClient: TNetHTTPClient;
  LUrl: string;
  LResponse: IHTTPResponse;
  LBody: TJSONObject;
  LBodyStream: TStringStream;
  LResponseObj: TJSONObject;
  LVideoJobId, LStatus: string;
  LHeaders: TNetHeaders;
begin
  LHttpClient := TNetHTTPClient.Create(nil);
  try
    try
      LUrl := TPath.Combine(OPENAI_API_BASE_URL, 'videos/' + AVideoIdToRemix + '/remix');
      LHttpClient.ContentType := 'application/json';

      LHeaders := [TNetHeader.Create('Authorization', 'Bearer ' + Self.ApiKey)];

      LBody := TJSONObject.Create;
      LBody.AddPair('prompt', TJSONString.Create(APrompt));

      LBodyStream := TStringStream.Create(LBody.ToJSON, TEncoding.UTF8);
      try
        LResponse := LHttpClient.Post(LUrl, LBodyStream, nil, LHeaders);
      finally
        LBodyStream.Free;
        LBody.Free;
      end;

      if (LResponse.StatusCode <> 200) then
        raise Exception.CreateFmt('Error starting video remix: %d, %s', [LResponse.StatusCode, LResponse.ContentAsString]);

      LResponseObj := TJSONObject.ParseJSONValue(LResponse.ContentAsString) as TJSONObject;
      try
        if not LResponseObj.TryGetValue<string>('id', LVideoJobId) then
          raise Exception.Create('New Video Job ID not found in remix API response.');
        LResponseObj.TryGetValue<string>('status', LStatus);
      finally
        LResponseObj.Free;
      end;

      PollAndDownloadVideoJob(LHttpClient, LVideoJobId, LStatus, LHeaders);

    except
      on E: Exception do
        DoError(E.Message);
    end;
  finally
    LHttpClient.Free;
  end;
end;

procedure TAiSoraGenerator.PollAndDownloadVideoJob(AHttpClient: TNetHTTPClient; const AVideoJobId: string; const AInitialStatus: string; const AHeaders: TNetHeaders);
var
  LPollingUrl, LStatus: string;
  LResponse: IHTTPResponse;
  LPollingResponse, LErrorObj: TJSONObject;
  LPollCount, LProgress: Integer;
  LResultVideo: TAiMediaFile;
  LErrorMessage: string;
begin
  LResultVideo := nil;
  try
    LPollingUrl := TPath.Combine(OPENAI_API_BASE_URL, 'videos/' + AVideoJobId);
    LStatus := AInitialStatus;
    LProgress := 0;

    DoProgress(Format('Video job created (ID: %s). Initial status: %s', [AVideoJobId, LStatus]));

    LPollCount := 0;
    while True do
    begin
      Inc(LPollCount);

      if LPollCount > 1 then
        Sleep(5000);

      LResponse := AHttpClient.Get(LPollingUrl, nil, AHeaders);
      if LResponse.StatusCode <> 200 then
        raise Exception.CreateFmt('Error during polling status for job %s: %d, %s', [AVideoJobId, LResponse.StatusCode, LResponse.ContentAsString]);

      LPollingResponse := TJSONObject.ParseJSONValue(LResponse.ContentAsString) as TJSONObject;
      try
        LPollingResponse.TryGetValue<string>('status', LStatus);
        LPollingResponse.TryGetValue<Integer>('progress', LProgress);
        DoProgress(Format('Checking status... [%d%%] %s', [LProgress, LStatus]));

        if LStatus.Equals('completed') then
        begin
          break;
        end
        else if LStatus.Equals('failed') then
        begin
          LErrorMessage := 'Video generation failed with status: "failed".';
          if LPollingResponse.TryGetValue<TJSONObject>('error', LErrorObj) then
          begin
            LErrorMessage := 'API Error: ' + LErrorObj.GetValue<string>('message', LErrorObj.ToJSON);
          end;
          raise Exception.Create(LErrorMessage);
        end;
      finally
        LPollingResponse.Free;
      end;
    end;

    DoProgress('Video generation complete. Downloading content...');
    LResultVideo := DownloadVideoContent(AHttpClient, AVideoJobId, AHeaders);
    DoSuccess(LResultVideo);

  except
    on E: Exception do
      DoError(E.Message);
  end;
end;

function TAiSoraGenerator.RemixVideo(const APrompt: string; const AOriginalVideoId: string): ITask;
begin
  if APrompt.IsEmpty or AOriginalVideoId.IsEmpty then
    raise Exception.Create('A prompt and the original video ID are required for remixing.');

  Result := TTask.Run(
    procedure
    begin
      InternalExecuteRemix(APrompt, AOriginalVideoId);
    end);
end;

end.
