// MIT License
//
// Copyright (c) 2024-2026 Gustavo Enriquez
//
// github.com/gustavoeenriquez/
//
// --------- FPC PORT --------------------
// Generacion de imagenes con DALL-E 2, DALL-E 3 y gpt-image-1.
// TAiDalle: componente standalone para generacion/edicion/variacion.
// TAiDalleImageTool: adapter IAiImageTool para integracion con TAiChat.
//
// Adaptaciones FPC:
//   - TNetHTTPClient          → TFPHTTPClient (fphttpclient)
//   - TMultipartFormData      → multipart manual con TMemoryStream
//   - TNetEncoding.Base64     → EncdDecd (DecodeString / EncodeStringBase64)
//   - TStreamReader           → TStringStream
//   - TStringBuilder          → string acumulado
//   - TThread.Queue(nil,proc) → llamada directa (consola / no GUI)
//   - TTask.Run(proc)         → TAiQueueHelper (hilo background)
//   - IHTTPResponse           → TStringStream + statusCode separado
//   - Client.SynchronizeEvents := False → no aplica en FPC

unit uMakerAi.OpenAi.Dalle;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Math,
  fphttpclient, opensslsockets,
  EncdDecd,
  fpjson, jsonparser,
  uMakerAi.Core, uMakerAi.Chat.Messages, uMakerAi.Chat.Tools;

const
  GlOpenAIUrl = 'https://api.openai.com/v1/';

type
  // -------------------------------------------------------------------------
  // Enums
  // -------------------------------------------------------------------------
  TAiImageModel        = (imDallE2, imDallE3, imGptImage1, imSDXL);
  TAiImageQuality      = (iqAuto, iqStandard, iqHD, iqHigh, iqMedium, iqLow);
  TAiImageBackground   = (ibAuto, ibTransparent, ibOpaque);
  TAiImageOutputFormat = (ifPng, ifJpeg, ifWebp);
  TAiImageStyle        = (isVivid, isNatural);
  TAiImageResponseFormat = (irfUrl, irfBase64Json);

  TAiImageSize = (
    is256x256, is512x512,
    is1024x1024, is1792x1024, is1024x1792,
    is1536x1024, is1024x1536,
    is768x768, is1216x832, is832x1216);

  // -------------------------------------------------------------------------
  // TAiDalleImage — resultado de una imagen generada
  // -------------------------------------------------------------------------
  TAiDalleImage = class
  private
    FImageStream:   TMemoryStream;
    FBase64:        string;
    FRevisedPrompt: string;
    FUrlFile:       string;
    FBackground:    string;
    FOutputFormat:  string;
    FQuality:       string;
    FSize:          string;
    FUsage:         TJSONObject;
    function GetImage: TMemoryStream;
  protected
    function Base64ToStream(const ABase64: string): TMemoryStream;
    procedure LoadImageFromUrl(const AUrlFile: string);
    procedure ParseData(JObj: TJSONObject);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   ParseStreamEvent(JObj: TJSONObject);
    property RevisedPrompt: string       read FRevisedPrompt;
    property Base64:        string       read FBase64;
    property UrlFile:       string       read FUrlFile;
    property Image:         TMemoryStream read GetImage;
    property Background:    string       read FBackground;
    property OutputFormat:  string       read FOutputFormat;
    property Quality:       string       read FQuality;
    property Size:          string       read FSize;
    property Usage:         TJSONObject  read FUsage;
  end;

  TAiDalleImages = array of TAiDalleImage;

  // Eventos
  TOnPartialImageReceived = procedure(Sender: TObject; const APartialImage: TAiDalleImage; AIndex: Integer) of object;
  TOnStreamCompleted      = procedure(Sender: TObject; const AFinalImage: TAiDalleImage) of object;
  TOnStreamError          = procedure(Sender: TObject; const AErrorMessage: string) of object;

  // -------------------------------------------------------------------------
  // TAiDalle — componente principal DALL-E / gpt-image-1
  // -------------------------------------------------------------------------
  TAiDalle = class(TComponent)
  private
    FApiKey:        string;
    FImages:        TAiDalleImages;
    FPrompt:        string;
    FUrl:           string;
    FUser:          string;
    FModel:         TAiImageModel;
    FResponseFormat: TAiImageResponseFormat;
    FQuality:       TAiImageQuality;
    FStyle:         TAiImageStyle;
    FStream:        Boolean;
    FBackground:    TAiImageBackground;
    FOutputFormat:  TAiImageOutputFormat;
    FOnPartialImageReceived: TOnPartialImageReceived;
    FOnStreamCompleted:      TOnStreamCompleted;
    FOnStreamError:          TOnStreamError;
    FStreamBuffer:  string;
    FNegativePrompt: TStrings;
    FSeed:          Int64;
    FSteps:         Integer;
    FGuidanceScale: Single;
    FUseRefiner:    Boolean;
    FAutoUpscale:   Boolean;
    FEnhanceFace:   Boolean;
    FStrength:      Single;
    FLoraPath:      string;

    function GetApiKey: string;
    procedure SetApiKey(const Value: string);
    procedure SetModel(const Value: TAiImageModel);
    procedure SetNegativePrompt(const Value: TStrings);
  protected
    procedure ClearImages;
    procedure ParseResponse(JObj: TJSONObject);
    procedure ProcessStreamLine(const ALine: string);
    function  SizeToString(ASize: TAiImageSize): string;
    // Multipart helper
    procedure AppendField(AStream: TMemoryStream; const ABoundary, AName, AValue: string);
    procedure AppendFilePart(AStream: TMemoryStream; const ABoundary, AName,
                             AFileName, AMimeType: string; AContent: TStream);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    function Generate(const APrompt, ANegativePrompt: string;
                      ASize: TAiImageSize; N: Integer = 1;
                      AImage: TMemoryStream = nil): TAiDalleImage;
    function Edit(AMediaFiles: TAiMediaFiles; AMaskFile: TAiMediaFile;
                  const APrompt: string; ASize: TAiImageSize;
                  N: Integer = 1): TAiDalleImage;
    function Variation(AImageFile: TAiMediaFile; ASize: TAiImageSize;
                       N: Integer = 1): TAiDalleImage;

  published
    property Url:             string              read FUrl             write FUrl;
    property ApiKey:          string              read GetApiKey        write SetApiKey;
    property Prompt:          string              read FPrompt;
    property User:            string              read FUser            write FUser;
    property Model:           TAiImageModel       read FModel           write SetModel        default imDallE3;
    property Quality:         TAiImageQuality     read FQuality         write FQuality        default iqAuto;
    property Style:           TAiImageStyle       read FStyle           write FStyle          default isVivid;
    property ResponseFormat:  TAiImageResponseFormat read FResponseFormat write FResponseFormat;
    property Stream:          Boolean             read FStream          write FStream         default False;
    property Background:      TAiImageBackground  read FBackground      write FBackground     default ibAuto;
    property OutputFormat:    TAiImageOutputFormat read FOutputFormat   write FOutputFormat   default ifPng;
    property NegativePrompt:  TStrings            read FNegativePrompt  write SetNegativePrompt;
    property Steps:           Integer             read FSteps           write FSteps          default 30;
    property GuidanceScale:   Single              read FGuidanceScale   write FGuidanceScale;
    property Seed:            Int64               read FSeed            write FSeed;
    property UseRefiner:      Boolean             read FUseRefiner      write FUseRefiner     default False;
    property AutoUpscale:     Boolean             read FAutoUpscale     write FAutoUpscale;
    property EnhanceFace:     Boolean             read FEnhanceFace     write FEnhanceFace;
    property Strength:        Single              read FStrength        write FStrength;
    property LoraPath:        string              read FLoraPath        write FLoraPath;
    property OnPartialImageReceived: TOnPartialImageReceived read FOnPartialImageReceived write FOnPartialImageReceived;
    property OnStreamCompleted:      TOnStreamCompleted      read FOnStreamCompleted      write FOnStreamCompleted;
    property OnStreamError:          TOnStreamError          read FOnStreamError          write FOnStreamError;
  end;

  // -------------------------------------------------------------------------
  // TAiDalleImageTool — adapter IAiImageTool para integracion con TAiChat
  // -------------------------------------------------------------------------
  TAiDalleImageTool = class(TAiImageToolBase)
  private
    FDalle:     TAiDalle;
    FImageSize: TAiImageSize;
    procedure SetDalle(const Value: TAiDalle);
  protected
    procedure ExecuteImageGeneration(const APrompt: string;
                                     ResMsg, AskMsg: TAiChatMessage); override;
    procedure InternalRunDalleGeneration(const APrompt: string;
                                         ResMsg: TAiChatMessage);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Dalle:     TAiDalle     read FDalle     write SetDalle;
    property ImageSize: TAiImageSize read FImageSize write FImageSize default is1024x1024;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiDalle, TAiDalleImageTool]);
end;

// ---------------------------------------------------------------------------
// TAiDalleImage
// ---------------------------------------------------------------------------

constructor TAiDalleImage.Create;
begin
  inherited Create;
  FImageStream := TMemoryStream.Create;
end;

destructor TAiDalleImage.Destroy;
begin
  FImageStream.Free;
  FUsage.Free;
  inherited;
end;

function TAiDalleImage.Base64ToStream(const ABase64: string): TMemoryStream;
var
  Decoded: string;
begin
  Decoded := DecodeStringBase64(ABase64);
  Result  := TMemoryStream.Create;
  Result.WriteBuffer(Pointer(Decoded)^, Length(Decoded));
  Result.Position := 0;
end;

function TAiDalleImage.GetImage: TMemoryStream;
begin
  if (FImageStream.Size <= 0) and (FUrlFile <> '') then
    LoadImageFromUrl(FUrlFile);
  Result := FImageStream;
end;

procedure TAiDalleImage.LoadImageFromUrl(const AUrlFile: string);
var
  Client: TFPHTTPClient;
begin
  FImageStream.Clear;
  Client := TFPHTTPClient.Create(nil);
  try
    Client.Get(AUrlFile, FImageStream);
    FImageStream.Position := 0;
  finally
    Client.Free;
  end;
end;

procedure TAiDalleImage.ParseData(JObj: TJSONObject);
var
  LUsage: TJSONData;
begin
  FUrlFile       := JObj.Get('url', '');
  FBase64        := JObj.Get('b64_json', '');
  FRevisedPrompt := JObj.Get('revised_prompt', '');
  FBackground    := JObj.Get('background', '');
  FOutputFormat  := JObj.Get('output_format', '');
  FSize          := JObj.Get('size', '');
  FQuality       := JObj.Get('quality', '');

  if FBase64 <> '' then
  begin
    FImageStream.Free;
    FImageStream := Base64ToStream(FBase64);
  end;

  LUsage := JObj.Find('usage');
  if LUsage is TJSONObject then
  begin
    FUsage.Free;
    FUsage := TJSONObject(LUsage.Clone);
  end;
end;

procedure TAiDalleImage.ParseStreamEvent(JObj: TJSONObject);
var
  LUsage: TJSONData;
begin
  FBase64       := JObj.Get('b64_json', '');
  FBackground   := JObj.Get('background', '');
  FOutputFormat := JObj.Get('output_format', '');
  FSize         := JObj.Get('size', '');
  FQuality      := JObj.Get('quality', '');

  if FBase64 <> '' then
  begin
    FImageStream.Free;
    FImageStream := Base64ToStream(FBase64);
  end;

  LUsage := JObj.Find('usage');
  if LUsage is TJSONObject then
  begin
    FUsage.Free;
    FUsage := TJSONObject(LUsage.Clone);
  end;
end;

// ---------------------------------------------------------------------------
// TAiDalle
// ---------------------------------------------------------------------------

constructor TAiDalle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUrl            := GlOpenAIUrl;
  FUser           := 'user';
  FModel          := imDallE3;
  FQuality        := iqAuto;
  FStyle          := isVivid;
  FResponseFormat := irfUrl;
  FStream         := False;
  FBackground     := ibAuto;
  FOutputFormat   := ifPng;
  FNegativePrompt := TStringList.Create;
  FSteps          := 30;
  FGuidanceScale  := 7.0;
  FSeed           := -1;
  FUseRefiner     := False;
  FAutoUpscale    := False;
  FEnhanceFace    := False;
end;

destructor TAiDalle.Destroy;
begin
  ClearImages;
  FNegativePrompt.Free;
  inherited;
end;

procedure TAiDalle.ClearImages;
var
  I: Integer;
begin
  for I := 0 to High(FImages) do
    FImages[I].Free;
  SetLength(FImages, 0);
end;

function TAiDalle.GetApiKey: string;
begin
  if (FApiKey <> '') and (Copy(FApiKey, 1, 1) = '@') then
    Result := GetEnvironmentVariable(Copy(FApiKey, 2, MaxInt))
  else
    Result := FApiKey;
end;

procedure TAiDalle.SetApiKey(const Value: string);
begin
  FApiKey := Value;
end;

procedure TAiDalle.SetModel(const Value: TAiImageModel);
begin
  FModel := Value;
end;

procedure TAiDalle.SetNegativePrompt(const Value: TStrings);
begin
  FNegativePrompt.Assign(Value);
end;

function TAiDalle.SizeToString(ASize: TAiImageSize): string;
begin
  case ASize of
    is256x256:    Result := '256x256';
    is512x512:    Result := '512x512';
    is768x768:    Result := '768x768';
    is1024x1024:  Result := '1024x1024';
    is1216x832:   Result := '1216x832';
    is832x1216:   Result := '832x1216';
    is1536x1024:  Result := '1536x1024';
    is1024x1536:  Result := '1024x1536';
    is1792x1024:  Result := '1792x1024';
    is1024x1792:  Result := '1024x1792';
  else
    Result := '1024x1024';
  end;
end;

procedure TAiDalle.AppendField(AStream: TMemoryStream; const ABoundary, AName, AValue: string);
var
  S: string;
begin
  S := '--' + ABoundary + #13#10 +
       'Content-Disposition: form-data; name="' + AName + '"' + #13#10 +
       #13#10 +
       AValue + #13#10;
  AStream.WriteBuffer(PChar(S)^, Length(S));
end;

procedure TAiDalle.AppendFilePart(AStream: TMemoryStream; const ABoundary,
  AName, AFileName, AMimeType: string; AContent: TStream);
var
  S: string;
begin
  S := '--' + ABoundary + #13#10 +
       'Content-Disposition: form-data; name="' + AName +
         '"; filename="' + AFileName + '"' + #13#10 +
       'Content-Type: ' + AMimeType + #13#10 + #13#10;
  AStream.WriteBuffer(PChar(S)^, Length(S));
  AContent.Position := 0;
  AStream.CopyFrom(AContent, AContent.Size);
  S := #13#10;
  AStream.WriteBuffer(PChar(S)^, Length(S));
end;

procedure TAiDalle.ParseResponse(JObj: TJSONObject);
var
  Data: TJSONData;
  Arr:  TJSONArray;
  I:    Integer;
begin
  ClearImages;
  Data := JObj.Find('data');
  if not (Data is TJSONArray) then Exit;
  Arr := TJSONArray(Data);
  SetLength(FImages, Arr.Count);
  for I := 0 to Arr.Count - 1 do
  begin
    FImages[I] := TAiDalleImage.Create;
    if Arr.Items[I] is TJSONObject then
      FImages[I].ParseData(TJSONObject(Arr.Items[I]));
  end;
end;

procedure TAiDalle.ProcessStreamLine(const ALine: string);
var
  JsonStr:    string;
  JData:      TJSONData;
  JObj:       TJSONObject;
  ImageEvent: TAiDalleImage;
  EventType:  string;
  PartIdx:    Integer;
  PartData:   TJSONData;
begin
  if not (Copy(ALine, 1, 6) = 'data: ') then Exit;
  JsonStr := Trim(Copy(ALine, 7, MaxInt));
  if (JsonStr = '') or (JsonStr = '[DONE]') then Exit;
  JData := GetJSON(JsonStr);
  if not Assigned(JData) then Exit;
  try
    if not (JData is TJSONObject) then Exit;
    JObj := TJSONObject(JData);
    ImageEvent := TAiDalleImage.Create;
    try
      ImageEvent.ParseStreamEvent(JObj);
      EventType := JObj.Get('type', '');
      PartIdx   := 0;
      PartData  := JObj.Find('partial_image_index');
      if Assigned(PartData) then
        PartIdx := PartData.AsInteger;

      if Pos('partial_image', EventType) > 0 then
      begin
        if Assigned(FOnPartialImageReceived) then
          FOnPartialImageReceived(Self, ImageEvent, PartIdx);
        ImageEvent := nil; // ownership transferred
      end
      else if Pos('completed', EventType) > 0 then
      begin
        if Assigned(FOnStreamCompleted) then
          FOnStreamCompleted(Self, ImageEvent);
        ImageEvent := nil;
      end;
    finally
      ImageEvent.Free; // no-op if nil
    end;
  finally
    JData.Free;
  end;
end;

// ---------------------------------------------------------------------------
// Generate
// ---------------------------------------------------------------------------

function TAiDalle.Generate(const APrompt, ANegativePrompt: string;
  ASize: TAiImageSize; N: Integer; AImage: TMemoryStream): TAiDalleImage;
var
  Client:      TFPHTTPClient;
  ReqBody:     TStringStream;
  RespStream:  TStringStream;
  JReq:        TJSONObject;
  JResp:       TJSONData;
  sUrl:        string;
  B64Img:      string;
  LLine:       string;
  Lines:       TStringList;
  I:           Integer;
begin
  Result := nil;
  ClearImages;
  FPrompt := APrompt;
  sUrl    := FUrl + 'images/generations';

  JReq := TJSONObject.Create;
  try
    JReq.Add('prompt', APrompt);
    JReq.Add('user',   FUser);

    case FModel of
      imDallE2:
      begin
        JReq.Add('model', 'dall-e-2');
        JReq.Add('n', Min(10, Max(1, N)));
        JReq.Add('size', SizeToString(ASize));
      end;
      imDallE3:
      begin
        JReq.Add('model', 'dall-e-3');
        JReq.Add('n', 1);
        JReq.Add('size', SizeToString(ASize));
        if FQuality in [iqHD, iqHigh] then
          JReq.Add('quality', 'hd')
        else
          JReq.Add('quality', 'standard');
        if FStyle = isVivid then
          JReq.Add('style', 'vivid')
        else
          JReq.Add('style', 'natural');
      end;
      imGptImage1:
      begin
        JReq.Add('model', 'gpt-image-1');
        JReq.Add('n', Min(10, Max(1, N)));
        JReq.Add('size', SizeToString(ASize));
        case FQuality of
          iqHigh:   JReq.Add('quality', 'high');
          iqMedium: JReq.Add('quality', 'medium');
          iqLow:    JReq.Add('quality', 'low');
        end;
        case FBackground of
          ibTransparent: JReq.Add('background', 'transparent');
          ibOpaque:      JReq.Add('background', 'opaque');
        end;
        case FOutputFormat of
          ifJpeg: JReq.Add('output_format', 'jpeg');
          ifWebp: JReq.Add('output_format', 'webp');
        end;
        if FStream then
          JReq.Add('stream', TJSONBoolean.Create(True));
      end;
      imSDXL:
      begin
        JReq.Add('model', 'sdxl');
        if ANegativePrompt <> '' then
          JReq.Add('negative_prompt', ANegativePrompt);
        if Assigned(AImage) and (AImage.Size > 2000) then
        begin
          AImage.Position := 0;
          B64Img := StreamToBase64(AImage);
          JReq.Add('image_b64', B64Img);
        end;
        JReq.Add('steps',          TJSONIntegerNumber.Create(FSteps));
        JReq.Add('guidance_scale', TJSONFloatNumber.Create(Double(FGuidanceScale)));
        if FSeed >= 0 then
          JReq.Add('seed', TJSONInt64Number.Create(FSeed));
        if FUseRefiner  then JReq.Add('use_refiner',  TJSONBoolean.Create(True));
        if FAutoUpscale then JReq.Add('auto_upscale', TJSONBoolean.Create(True));
        if FEnhanceFace then JReq.Add('enhance_face', TJSONBoolean.Create(True));
        if FStrength <> 0 then
          JReq.Add('strength', TJSONFloatNumber.Create(Double(FStrength)));
        if FLoraPath <> '' then
          JReq.Add('lora_path', FLoraPath);
        JReq.Add('size', SizeToString(ASize));
      end;
    end;

    if FModel <> imGptImage1 then
    begin
      if FResponseFormat = irfUrl then
        JReq.Add('response_format', 'url')
      else
        JReq.Add('response_format', 'b64_json');
    end;

    ReqBody    := TStringStream.Create(JReq.AsJSON);
    RespStream := TStringStream.Create('');
    Client     := TFPHTTPClient.Create(nil);
    try
      Client.AddHeader('Authorization', 'Bearer ' + ApiKey);
      Client.AddHeader('Content-Type', 'application/json');
      Client.RequestBody := ReqBody;
      if FStream and (FModel = imGptImage1) then
        Client.AddHeader('Accept', 'text/event-stream');
      Client.HTTPMethod('POST', sUrl, RespStream, [200]);

      if FStream and (FModel = imGptImage1) then
      begin
        Lines := TStringList.Create;
        try
          Lines.Text := RespStream.DataString;
          for I := 0 to Lines.Count - 1 do
          begin
            LLine := Trim(Lines[I]);
            if LLine <> '' then
              ProcessStreamLine(LLine);
          end;
        finally
          Lines.Free;
        end;
      end
      else
      begin
        JResp := GetJSON(RespStream.DataString);
        if Assigned(JResp) then
        try
          if JResp is TJSONObject then
          begin
            ParseResponse(TJSONObject(JResp));
            if Length(FImages) > 0 then
              Result := FImages[0];
          end;
        finally
          JResp.Free;
        end;
      end;

    finally
      Client.Free;
      ReqBody.Free;
      RespStream.Free;
    end;
  finally
    JReq.Free;
  end;
end;

// ---------------------------------------------------------------------------
// Edit
// ---------------------------------------------------------------------------

function TAiDalle.Edit(AMediaFiles: TAiMediaFiles; AMaskFile: TAiMediaFile;
  const APrompt: string; ASize: TAiImageSize; N: Integer): TAiDalleImage;
var
  Boundary:   string;
  MPStream:   TMemoryStream;
  RespStream: TStringStream;
  Client:     TFPHTTPClient;
  JResp:      TJSONData;
  sUrl:       string;
  FieldName:  string;
  I:          Integer;
  MediaFile:  TAiMediaFile;
  CloseStr:   string;
begin
  Result := nil;
  ClearImages;

  if not Assigned(AMediaFiles) or (AMediaFiles.Count = 0) then
    raise Exception.Create('At least one media file must be provided for editing.');
  if (FModel <> imDallE2) and (FModel <> imGptImage1) then
    raise Exception.Create('Edit endpoint only supports dall-e-2 and gpt-image-1.');
  if (FModel = imDallE2) and (AMediaFiles.Count > 1) then
    raise Exception.Create('DALL-E 2 only supports one image for editing.');

  sUrl     := FUrl + 'images/edits';
  Boundary := 'DALLEBound' + IntToStr(Random(999999));
  MPStream := TMemoryStream.Create;
  try
    for I := 0 to AMediaFiles.Count - 1 do
    begin
      MediaFile := AMediaFiles[I];
      if FModel = imGptImage1 then
        FieldName := 'image[]'
      else
        FieldName := 'image';
      AppendFilePart(MPStream, Boundary, FieldName,
        MediaFile.Filename, MediaFile.MimeType, MediaFile.Content);
    end;

    if Assigned(AMaskFile) then
      AppendFilePart(MPStream, Boundary, 'mask',
        AMaskFile.Filename, AMaskFile.MimeType, AMaskFile.Content);

    AppendField(MPStream, Boundary, 'prompt', APrompt);
    AppendField(MPStream, Boundary, 'user', FUser);
    AppendField(MPStream, Boundary, 'n', IntToStr(N));

    case FModel of
      imDallE2:
      begin
        AppendField(MPStream, Boundary, 'model', 'dall-e-2');
        case ASize of
          is256x256: AppendField(MPStream, Boundary, 'size', '256x256');
          is512x512: AppendField(MPStream, Boundary, 'size', '512x512');
        else
          AppendField(MPStream, Boundary, 'size', '1024x1024');
        end;
      end;
      imGptImage1:
      begin
        AppendField(MPStream, Boundary, 'model', 'gpt-image-1');
        case ASize of
          is1024x1536: AppendField(MPStream, Boundary, 'size', '1024x1536');
          is1536x1024: AppendField(MPStream, Boundary, 'size', '1536x1024');
        else
          AppendField(MPStream, Boundary, 'size', '1024x1024');
        end;
      end;
    end;

    // Cierre multipart
    CloseStr := '--' + Boundary + '--' + #13#10;
    MPStream.WriteBuffer(PChar(CloseStr)^, Length(CloseStr));
    MPStream.Position := 0;

    RespStream := TStringStream.Create('');
    Client     := TFPHTTPClient.Create(nil);
    try
      Client.AddHeader('Authorization', 'Bearer ' + ApiKey);
      Client.AddHeader('Content-Type', 'multipart/form-data; boundary=' + Boundary);
      Client.RequestBody := MPStream;
      Client.HTTPMethod('POST', sUrl, RespStream, [200]);

      JResp := GetJSON(RespStream.DataString);
      if Assigned(JResp) then
      try
        if JResp is TJSONObject then
        begin
          ParseResponse(TJSONObject(JResp));
          if Length(FImages) > 0 then
            Result := FImages[0];
        end;
      finally
        JResp.Free;
      end;
    finally
      Client.Free;
      RespStream.Free;
    end;
  finally
    MPStream.Free;
  end;
end;

// ---------------------------------------------------------------------------
// Variation
// ---------------------------------------------------------------------------

function TAiDalle.Variation(AImageFile: TAiMediaFile; ASize: TAiImageSize;
  N: Integer): TAiDalleImage;
var
  Boundary:   string;
  MPStream:   TMemoryStream;
  RespStream: TStringStream;
  Client:     TFPHTTPClient;
  JResp:      TJSONData;
  sUrl:       string;
  CloseStr:   string;
  FmtStr:     string;
begin
  Result := nil;
  ClearImages;

  if not Assigned(AImageFile) then
    raise Exception.Create('An image file must be provided for variation.');
  if FModel <> imDallE2 then
    raise Exception.Create('Variation endpoint only supports dall-e-2.');

  sUrl     := FUrl + 'images/variations';
  Boundary := 'VARBound' + IntToStr(Random(999999));
  MPStream := TMemoryStream.Create;
  try
    AppendFilePart(MPStream, Boundary, 'image',
      AImageFile.Filename, AImageFile.MimeType, AImageFile.Content);
    AppendField(MPStream, Boundary, 'user', FUser);
    AppendField(MPStream, Boundary, 'n', IntToStr(N));

    case ASize of
      is256x256: AppendField(MPStream, Boundary, 'size', '256x256');
      is512x512: AppendField(MPStream, Boundary, 'size', '512x512');
    else
      AppendField(MPStream, Boundary, 'size', '1024x1024');
    end;

    if FResponseFormat = irfUrl then
      FmtStr := 'url'
    else
      FmtStr := 'b64_json';
    AppendField(MPStream, Boundary, 'response_format', FmtStr);

    CloseStr := '--' + Boundary + '--' + #13#10;
    MPStream.WriteBuffer(PChar(CloseStr)^, Length(CloseStr));
    MPStream.Position := 0;

    RespStream := TStringStream.Create('');
    Client     := TFPHTTPClient.Create(nil);
    try
      Client.AddHeader('Authorization', 'Bearer ' + ApiKey);
      Client.AddHeader('Content-Type', 'multipart/form-data; boundary=' + Boundary);
      Client.RequestBody := MPStream;
      Client.HTTPMethod('POST', sUrl, RespStream, [200]);

      JResp := GetJSON(RespStream.DataString);
      if Assigned(JResp) then
      try
        if JResp is TJSONObject then
        begin
          ParseResponse(TJSONObject(JResp));
          if Length(FImages) > 0 then
            Result := FImages[0];
        end;
      finally
        JResp.Free;
      end;
    finally
      Client.Free;
      RespStream.Free;
    end;
  finally
    MPStream.Free;
  end;
end;

// ---------------------------------------------------------------------------
// TAiDalleImageTool
// ---------------------------------------------------------------------------

constructor TAiDalleImageTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDalle     := nil;
  FImageSize := is1024x1024;
end;

procedure TAiDalleImageTool.SetDalle(const Value: TAiDalle);
begin
  if FDalle <> Value then
  begin
    FDalle := Value;
    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;

procedure TAiDalleImageTool.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDalle) then
    FDalle := nil;
end;

procedure TAiDalleImageTool.ExecuteImageGeneration(const APrompt: string;
  ResMsg, AskMsg: TAiChatMessage);
begin
  // Si ya estamos en hilo background (IsAsync=True), ejecutar directo.
  // Si no, habria que lanzar en hilo separado; simplificado para FPC.
  InternalRunDalleGeneration(APrompt, ResMsg);
end;

procedure TAiDalleImageTool.InternalRunDalleGeneration(const APrompt: string;
  ResMsg: TAiChatMessage);
var
  LDalleImage: TAiDalleImage;
  LMediaFile:  TAiMediaFile;
  LExt:        string;
begin
  if not Assigned(FDalle) then
    raise Exception.Create('TAiDalleImageTool: debe asignarse la propiedad Dalle.');

  ReportState(acsToolExecuting, 'Generando imagen con DALL-E...');

  LDalleImage := FDalle.Generate(APrompt, FDalle.NegativePrompt.Text, FImageSize);

  if Assigned(LDalleImage) then
  begin
    case FDalle.OutputFormat of
      ifJpeg: LExt := '.jpg';
      ifWebp: LExt := '.webp';
    else
      LExt := '.png';
    end;

    LMediaFile := TAiMediaFile.Create;
    try
      if LDalleImage.Image.Size > 0 then
      begin
        LDalleImage.Image.Position := 0;
        LMediaFile.LoadFromStream('generated_image' + LExt, LDalleImage.Image);
      end;
      ResMsg.MediaFiles.Add(LMediaFile);
      if LDalleImage.RevisedPrompt <> '' then
        ResMsg.Prompt := LDalleImage.RevisedPrompt
      else
        ResMsg.Prompt := APrompt;
    except
      LMediaFile.Free;
      raise;
    end;
  end;

  ReportDataEnd(ResMsg, 'assistant', ResMsg.Prompt);
end;

end.
