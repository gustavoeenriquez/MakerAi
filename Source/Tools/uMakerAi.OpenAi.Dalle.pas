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
// Nombre: Gustavo Enríquez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/


// --- Modificaciones ----
// 30/08/2024 -- Mejora en la función Edit

// --------- CAMBIOS --------------------
// 4/11/2025 - Refactorización completa para soportar dall-e-2, dall-e-3, y gpt-image-1.
// 4/11/2025 - Añadido soporte para streaming con eventos (OnPartialImageReceived, OnStreamCompleted).
// 4/11/2025 - Integración con uMakerAi.Core: Edit y Variation ahora usan TAiMediaFile.
// 4/11/2025 - Modernización de enums y nombres de propiedades para mayor claridad.

unit uMakerAi.OpenAi.Dalle;

{$INCLUDE ../CompilerDirectives.inc}

interface

uses
  {$IFDEF FPC}
  Classes, SysUtils, StrUtils, Generics.Collections, Types, Variants, SyncObjs, Math,
  {$ELSE}
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Threading, System.Variants, System.Net.Mime, System.IOUtils,
  System.Generics.Collections, System.NetEncoding, System.JSON, System.StrUtils,
  System.Math,
  System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent,
  REST.JSON, REST.Types, REST.Client,
  {$ENDIF}
  uMakerAi.Core, uJsonHelper, uHttpHelper, uSysUtilsHelper, uBase64Helper, uThreadingHelper, uRttiHelper;

const
  GlOpenAIUrl = 'https://api.openai.com/v1/';

type
  TAiDalleImage = class(TObject)
  private
    FImageStream: TMemoryStream;
    FBase64: string;
    FRevisedPrompt: string;
    FUrlFile: string;
    FBackground: string;
    FOutputFormat: string;
    FQuality: string;
    FSize: string;
    FUsage: TJSONObject;
    function GetImage: TMemoryStream;
  protected
    function Base64ToStream(const ABase64: string): TMemoryStream;
    procedure LoadImageFromUrl(const AUrlFile: string);
    procedure ParseData(JObj: TJSONObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseStreamEvent(JObj: TJSONObject);
    property RevisedPrompt: string read FRevisedPrompt;
    property Base64: string read FBase64;
    property UrlFile: string read FUrlFile;
    property Image: TMemoryStream read GetImage;
    property Background: string read FBackground;
    property OutputFormat: string read FOutputFormat;
    property Quality: string read FQuality;
    property Size: string read FSize;
    property Usage: TJSONObject read FUsage;
  end;

  TAiDalleImages = array of TAiDalleImage;

  // Eventos para el streaming
  TOnPartialImageReceived = procedure(Sender: TObject; const APartialImage: TAiDalleImage; AIndex: Integer) of object;
  TOnStreamCompleted = procedure(Sender: TObject; const AFinalImage: TAiDalleImage) of object;
  TOnStreamError = procedure(Sender: TObject; const AErrorMessage: string) of object;

  // Enums para los parámetros de la API
  TAiImageModel = (imDallE2, imDallE3, imGptImage1, imSDXL);

  TAiImageQuality = (iqAuto, iqStandard, iqHD, iqHigh, iqMedium, iqLow);
  TAiImageBackground = (ibAuto, ibTransparent, ibOpaque);
  TAiImageOutputFormat = (ifPng, ifJpeg, ifWebp);
  TAiImageStyle = (isVivid, isNatural);
  TAiImageResponseFormat = (irfUrl, irfBase64Json);

  TAiImageSize = (is256x256, is512x512, is1024x1024, is1792x1024, is1024x1792, is1536x1024, is1024x1536,
    // SDXL-friendly
    is768x768, is1216x832, is832x1216);

  TAiDalle = class(TComponent)
  private
    FApiKey: string;
    FImages: TAiDalleImages;
    FPrompt: string;
    FUrl: string;
    FUser: string;
    FModel: TAiImageModel;
    FResponseFormat: TAiImageResponseFormat;
    FQuality: TAiImageQuality;
    FStyle: TAiImageStyle;
    FStream: Boolean;
    FBackground: TAiImageBackground;
    FOutputFormat: TAiImageOutputFormat;
    FOnPartialImageReceived: TOnPartialImageReceived;
    FOnStreamCompleted: TOnStreamCompleted;
    FOnStreamError: TOnStreamError;
    FStreamBuffer: TStringBuilder;
    FBytesProcessed: Int64;
    FActiveResponseStream: TMemoryStream;
    FNegativePrompt: TStrings;
    FSeed: Int64;
    FSteps: Integer;
    FGuidanceScale: Single;
    FUseRefiner: Boolean;
    FSampler: string; // opcional

    function GetApiKey: string;
    procedure SetApiKey(const Value: string);
    procedure SetModel(const Value: TAiImageModel);
    procedure SetQuality(const Value: TAiImageQuality);
    procedure SetResponseFormat(const Value: TAiImageResponseFormat);
    procedure SetStyle(const Value: TAiImageStyle);
    procedure SetStream(const Value: Boolean);
    procedure SetBackground(const Value: TAiImageBackground);
    procedure SetOutputFormat(const Value: TAiImageOutputFormat);
    procedure SetNegativePrompt(const Value: TStrings);
  protected
    procedure ClearImages;
    procedure ParseResponse(JObj: TJSONObject);
    procedure HandleStreamData(const Sender: TObject; {$IFDEF FPC}const{$ENDIF} AContentLength, AReadCount: Int64; var AAbort: Boolean);
    procedure ProcessStreamBuffer;
    function SizeToString(ASize: TAiImageSize): string;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    function Generate(const aPrompt, aNegativePrompt: string; ASize: TAiImageSize; N: Integer = 1; aImage: TMemoryStream = Nil): TAiDalleImage;
    function Edit(aMediaFiles: TAiMediaFiles; aMaskFile: TAiMediaFile; const aPrompt: string; ASize: TAiImageSize; N: Integer = 1): TAiDalleImage;
    function Variation(aImageFile: TAiMediaFile; ASize: TAiImageSize; N: Integer = 1): TAiDalleImage;
    function Upscale(aImage: TAiMediaFile; AScale: Integer = 2; AFaceEnhance: Boolean = False): TAiMediaFile;

  published
    property Url: string read FUrl write FUrl;
    property ApiKey: string read GetApiKey write SetApiKey;
    property Prompt: string read FPrompt;
    property User: string read FUser write FUser;
    property Model: TAiImageModel read FModel write SetModel default TAiImageModel.imDallE3;
    property Quality: TAiImageQuality read FQuality write SetQuality default TAiImageQuality.iqAuto;
    property Style: TAiImageStyle read FStyle write SetStyle default TAiImageStyle.isVivid;
    property ResponseFormat: TAiImageResponseFormat read FResponseFormat write SetResponseFormat;
    property Stream: Boolean read FStream write SetStream default False;
    property Background: TAiImageBackground read FBackground write SetBackground default TAiImageBackground.ibAuto;
    property OutputFormat: TAiImageOutputFormat read FOutputFormat write SetOutputFormat default TAiImageOutputFormat.ifPng;
    property NegativePrompt: TStrings read FNegativePrompt write SetNegativePrompt;

    // SDXL propierties

    property Steps: Integer read FSteps write FSteps default 30;
    property GuidanceScale: Single read FGuidanceScale write FGuidanceScale;
    property Seed: Int64 read FSeed write FSeed Default -1;
    property UseRefiner: Boolean read FUseRefiner write FUseRefiner default False;

    // Eventos
    property OnPartialImageReceived: TOnPartialImageReceived read FOnPartialImageReceived write FOnPartialImageReceived;
    property OnStreamCompleted: TOnStreamCompleted read FOnStreamCompleted write FOnStreamCompleted;
    property OnStreamError: TOnStreamError read FOnStreamError write FOnStreamError;
  end;

procedure Register;

implementation

type
  // Helper class for synchronizing events in FPC/Delphi cross-platform way
  TStreamCallbackRunner = class
  private
    FDalle: TAiDalle;
    FImage: TAiDalleImage;
    FIndex: Integer;
    FErrorMsg: string;
  public
    constructor Create(ADalle: TAiDalle; AImage: TAiDalleImage; AIndex: Integer); overload;
    constructor Create(ADalle: TAiDalle; AImage: TAiDalleImage); overload;
    constructor Create(ADalle: TAiDalle; const AErrorMsg: string); overload;
    
    procedure DoPartial;
    procedure DoComplete;
    procedure DoError;
  end;

constructor TStreamCallbackRunner.Create(ADalle: TAiDalle; AImage: TAiDalleImage; AIndex: Integer);
begin
  FDalle := ADalle;
  FImage := AImage;
  FIndex := AIndex;
end;

constructor TStreamCallbackRunner.Create(ADalle: TAiDalle; AImage: TAiDalleImage);
begin
  FDalle := ADalle;
  FImage := AImage;
  FIndex := 0;
end;

constructor TStreamCallbackRunner.Create(ADalle: TAiDalle; const AErrorMsg: string);
begin
  FDalle := ADalle;
  FErrorMsg := AErrorMsg;
end;

procedure TStreamCallbackRunner.DoPartial;
begin
  if Assigned(FDalle.OnPartialImageReceived) then
    FDalle.OnPartialImageReceived(FDalle, FImage, FIndex);
  // The logic implies ownership is passed, or not?
  // We mirror the original logic. 
  Self.Free;
end;

procedure TStreamCallbackRunner.DoComplete;
begin
  if Assigned(FDalle.OnStreamCompleted) then
    FDalle.OnStreamCompleted(FDalle, FImage);
  Self.Free;
end;

procedure TStreamCallbackRunner.DoError;
begin
  if Assigned(FDalle.OnStreamError) then
    FDalle.OnStreamError(FDalle, FErrorMsg);
  Self.Free;
end;

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiDalle]);
end;

{ TAiDalleImage }

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
begin
  Result := TBytesStream.Create(TNetEncoding.Base64.DecodeStringToBytes(ABase64));
end;

function TAiDalleImage.GetImage: TMemoryStream;
begin
  if (FImageStream.Size <= 0) and (FUrlFile <> '') then
    LoadImageFromUrl(FUrlFile);
  Result := FImageStream;
end;

procedure TAiDalleImage.LoadImageFromUrl(const AUrlFile: string);
var
  NetHttp: TNetHTTPClient;
begin
  FImageStream.Clear;
  NetHttp := TNetHTTPClient.Create(nil);
  NetHttp.ConfigureForAsync;
  try
    NetHttp.Get(AUrlFile, FImageStream);
    FImageStream.Position := 0;
  finally
    NetHttp.Free;
  end;
end;

procedure TAiDalleImage.ParseData(JObj: TJSONObject);
begin
  JObj.TryGetValue('url', FUrlFile);
  if JObj.TryGetValue('b64_json', FBase64) then
  begin
    FImageStream.Free;
    FImageStream := Base64ToStream(FBase64);
  end;
  JObj.TryGetValue('revised_prompt', FRevisedPrompt);
  JObj.TryGetValue('background', FBackground);
  JObj.TryGetValue('output_format', FOutputFormat);
  JObj.TryGetValue('size', FSize);
  JObj.TryGetValue('quality', FQuality);
  if JObj.TryGetValue('usage', FUsage) then
    FUsage := TJSONObject(FUsage.Clone);
end;

procedure TAiDalleImage.ParseStreamEvent(JObj: TJSONObject);
begin
  if JObj.TryGetValue('b64_json', FBase64) then
  begin
    FImageStream.Free;
    FImageStream := Base64ToStream(FBase64);
  end;
  JObj.TryGetValue('background', FBackground);
  JObj.TryGetValue('output_format', FOutputFormat);
  JObj.TryGetValue('size', FSize);
  JObj.TryGetValue('quality', FQuality);
  if JObj.TryGetValue('usage', FUsage) then
    FUsage := TJSONObject(FUsage.Clone);
end;

{ TAiDalle }

constructor TAiDalle.Create(aOwner: TComponent);
begin
  inherited;
  FUrl := GlOpenAIUrl;
  FUser := 'user';
  FModel := imDallE3;
  FQuality := iqAuto;
  FStyle := isVivid;
  FResponseFormat := TAiImageResponseFormat.irfUrl;
  FStream := False;
  FBackground := ibAuto;
  FOutputFormat := ifPng;
  FStreamBuffer := TStringBuilder.Create;
  FNegativePrompt := TStringList.Create;

  FSteps := 30;
  FGuidanceScale := 7.0;
  FSeed := -1; // -1 = random
  FUseRefiner := False;

end;

destructor TAiDalle.Destroy;
begin
  ClearImages;
  FStreamBuffer.Free;
  FNegativePrompt.Free;
  inherited;
end;

procedure TAiDalle.ClearImages;
var
  i: Integer;
begin
  for i := 0 to High(FImages) do
    FImages[i].Free;
  SetLength(FImages, 0);
end;

function TAiDalle.Edit(aMediaFiles: TAiMediaFiles; aMaskFile: TAiMediaFile; const aPrompt: string; ASize: TAiImageSize; N: Integer): TAiDalleImage;
var
  Body: TMultipartFormData;
  Client: TNetHTTPClient;
  sUrl: string;
  Res: IHTTPResponse;
  JObj: TJSONObject;
  MediaFile: TAiMediaFile;
begin
  Result := nil;
  ClearImages;

  if not Assigned(aMediaFiles) or (aMediaFiles.Count = 0) then
    raise Exception.Create('At least one media file must be provided for editing.');
  if (FModel <> imDallE2) and (FModel <> imGptImage1) then
    raise Exception.Create('Edit endpoint only supports dall-e-2 and gpt-image-1 models.');
  if (FModel = imDallE2) and (aMediaFiles.Count > 1) then
    raise Exception.Create('DALL-E 2 only supports one image for editing.');

  sUrl := FUrl + 'images/edits';
  Client := TNetHTTPClient.Create(nil);
  Client.ConfigureForAsync;
  Body := TMultipartFormData.Create;
  try
    for MediaFile in aMediaFiles do
    begin
      MediaFile.Content.Position := 0;
      if FModel = imGptImage1 then
        AddStreamToMultipart(Body, 'image[]', MediaFile.Content, False, MediaFile.Filename, '')
      else
        AddStreamToMultipart(Body, 'image', MediaFile.Content, False, MediaFile.Filename, '');
    end;

    if Assigned(aMaskFile) then
    begin
      aMaskFile.Content.Position := 0;
      AddStreamToMultipart(Body, 'mask', aMaskFile.Content, False, aMaskFile.Filename, '');
    end;

    Body.AddField('prompt', aPrompt);
    Body.AddField('user', FUser);
    Body.AddField('n', N.ToString);

    case FModel of
      imDallE2:
        begin
          Body.AddField('model', 'dall-e-2');
          case ASize of
            is256x256:
              Body.AddField('size', '256x256');
            is512x512:
              Body.AddField('size', '512x512');
          else
            Body.AddField('size', '1024x1024');
          end;
        end;
      imGptImage1:
        begin
          Body.AddField('model', 'gpt-image-1');
          case ASize of
            is1024x1536:
              Body.AddField('size', '1024x1536');
            is1536x1024:
              Body.AddField('size', '1536x1024');
          else
            Body.AddField('size', '1024x1024');
          end;
        end;
    end;

    Client.SetHeader('Authorization', 'Bearer ' + ApiKey);
    Res := Client.Post(sUrl, Body);

    if Res.StatusCode = 200 then
    begin
      JObj := TJSONObject.ParseJSONValue(Res.ContentAsString) as TJSONObject;
      try
        ParseResponse(JObj);
        if Length(FImages) > 0 then
          Result := FImages[0];
      finally
        JObj.Free;
      end;
    end
    else
      raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
  finally
    Client.Free;
    Body.Free;
  end;
end;

function TAiDalle.Generate(const aPrompt, aNegativePrompt: string; ASize: TAiImageSize; N: Integer; aImage: TMemoryStream): TAiDalleImage;
var
  Client: TNetHTTPClient;
  JObj: TJSONObject;
  Res: IHTTPResponse;
  ContentStream: TStringStream;
  ResponseStream: TMemoryStream; // Usado solo para llamadas no-streaming
  sUrl: string;
  AbortFlag: Boolean; // Variable para el parámetro 'var'
  // Delphi: TStreamReader para lectura robusta. FPC usa TStringStream como alternativa
  {$IFNDEF FPC}
  StreamReader: TStreamReader; // Para leer la respuesta de forma robusta
  {$ELSE}
  SS: TStringStream;
  {$ENDIF}
begin
  Result := nil;
  ClearImages;
  FPrompt := aPrompt;
  sUrl := FUrl + 'images/generations';
  Client := TNetHTTPClient.Create(nil);
  Client.ConfigureForAsync;
  JObj := TJSONObject.Create;
  ContentStream := TStringStream.Create('', TEncoding.UTF8);
  FActiveResponseStream := nil;
  try
    JObj.AddPair('prompt', aPrompt).AddPair('user', FUser);
    case FModel of
      imDallE2:
        begin
          JObj.AddPair('model', 'dall-e-2').AddPair('n', Min(10, Max(1, N)));
          JObj.AddPair('size', SizeToString(aSize));
        end;
      imDallE3:
        begin
          JObj.AddPair('model', 'dall-e-3').AddPair('n', 1);
          JObj.AddPair('size', SizeToString(aSize));
          if FQuality in [iqHD, iqHigh] then
            JObj.AddPair('quality', 'hd')
          else
            JObj.AddPair('quality', 'standard');
          if FStyle = isVivid then
            JObj.AddPair('style', 'vivid')
          else
            JObj.AddPair('style', 'natural');
        end;
      imGptImage1:
        begin
          JObj.AddPair('model', 'gpt-image-1').AddPair('n', Min(10, Max(1, N)));
          JObj.AddPair('size', SizeToString(aSize));

          case FQuality of
            iqHigh:
              JObj.AddPair('quality', 'high');
            iqMedium:
              JObj.AddPair('quality', 'medium');
            iqLow:
              JObj.AddPair('quality', 'low');
          end;
          case FBackground of
            ibTransparent:
              JObj.AddPair('background', 'transparent');
            ibOpaque:
              JObj.AddPair('background', 'opaque');
          end;
          case FOutputFormat of
            ifJpeg:
              JObj.AddPair('output_format', 'jpeg');
            ifWebp:
              JObj.AddPair('output_format', 'webp');
          end;
          if FStream then
            JObj.AddPair('stream', CreateJSONBool(True));
        end;

      imSDXL:
        begin
          JObj.AddPair('model', 'sdxl');
          JObj.AddPair('prompt', Trim(StringReplace(aPrompt, #$D#$A, ' \n', [rfReplaceAll])));

          if aNegativePrompt <> '' then
            JObj.AddPair('negative_prompt', Trim(StringReplace(aNegativePrompt, #$D#$A, ' \n', [rfReplaceAll])));

          If Assigned(aImage) and (aImage.Size > 2000) then
            JObj.AddPair('image', TNetEncoding.Base64.EncodeBytesToString(aImage.Memory, aImage.Size));

          JObj.AddPair('steps', CreateJSONNumber(FSteps));
          JObj.AddPair('guidance_scale', CreateJSONNumber(FGuidanceScale));

          if FSeed >= 0 then
            JObj.AddPair('seed', CreateJSONNumber(FSeed));

          if FUseRefiner then
            JObj.AddPair('use_refiner', CreateJSONBool(True));

          JObj.AddPair('size', SizeToString(ASize));
        end;
    end;

    if FModel <> imGptImage1 then
    begin
      if FResponseFormat = irfUrl then
        JObj.AddPair('response_format', 'url')
      else
        JObj.AddPair('response_format', 'b64_json');
    end;

    ContentStream.WriteString(JObj.ToJSON);
    ContentStream.Position := 0;
    Client.SetHeader('Authorization', 'Bearer ' + ApiKey);
    Client.ContentType := 'application/json';

    if FStream and (FModel = imGptImage1) then
    begin
      Client.Accept := 'text/event-stream';
      FStreamBuffer.Clear;
      FBytesProcessed := 0;
      Client.OnReceiveData := HandleStreamData;

      FActiveResponseStream := TMemoryStream.Create;

      Client.Post(sUrl, ContentStream, FActiveResponseStream);

      // Llamada final para procesar los datos restantes
      if Assigned(FActiveResponseStream) and (FActiveResponseStream.Size > FBytesProcessed) then
      begin
        AbortFlag := False;
        HandleStreamData(Client, FActiveResponseStream.Size, FActiveResponseStream.Size, AbortFlag);
      end;
    end
    else
    begin
      // Para llamadas no-streaming, usamos una variable local
      ResponseStream := TMemoryStream.Create;
      try
        Res := Client.Post(sUrl, ContentStream, ResponseStream);
        ResponseStream.Position := 0;

        if Res.StatusCode = 200 then
        begin
          JObj.Free;
          {$IFDEF FPC}
          SS := TStringStream.Create('');
          try
            SS.CopyFrom(ResponseStream, 0);
            JObj := TJSONObject.ParseJSONValue(SS.DataString) as TJSONObject;
          finally
            SS.Free;
          end;
          {$ELSE}
          StreamReader := TStreamReader.Create(ResponseStream, TEncoding.UTF8);
          try
            JObj := TJSONObject.ParseJSONValue(StreamReader.ReadToEnd) as TJSONObject;
          finally
            StreamReader.Free;
          end;
          {$ENDIF}

          try
            ParseResponse(JObj);
            if Length(FImages) > 0 then
              Result := FImages[0];
          finally
            JObj.Free;
          end;
        end
        else
        begin
          {$IFDEF FPC}
          SS := TStringStream.Create('');
          try
            SS.CopyFrom(ResponseStream, 0);
            raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, SS.DataString]);
          finally
            SS.Free;
          end;
          {$ELSE}
          StreamReader := TStreamReader.Create(ResponseStream, TEncoding.UTF8);
          try
            raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, StreamReader.ReadToEnd]);
          finally
            StreamReader.Free;
          end;
          {$ENDIF}
        end;
      finally
        ResponseStream.Free;
      end;
    end;
  finally
    Client.Free;
    ContentStream.Free;

    // Solo liberamos el stream activo si fue creado (en modo streaming)
    if Assigned(FActiveResponseStream) then
    begin
      FActiveResponseStream.Free;
      FActiveResponseStream := nil;
    end;

    // El JObj de la petición se libera aquí solo si no es streaming,
    // porque en streaming ya se habría liberado antes del bloque finally.
    // if not(FStream and (FModel = imGptImage1)) then
    // JObj.Free;
  end;
end;

function TAiDalle.GetApiKey: string;
begin
  if (csDesigning in ComponentState) then
    Result := FApiKey
  else if (FApiKey <> '') and (FApiKey.StartsWith('@')) then
    Result := CompatGetEnvVar(FApiKey.Substring(1))
  else
    Result := FApiKey;
end;

procedure TAiDalle.HandleStreamData(const Sender: TObject; {$IFDEF FPC}const{$ENDIF} AContentLength, AReadCount: Int64; var AAbort: Boolean);
var
  NewBytes: TBytes;
  NewDataSize: Int64;
begin
  // Verificación de seguridad: solo proceder si tenemos un stream activo
  if not Assigned(FActiveResponseStream) then
    Exit;

  NewDataSize := AReadCount - FBytesProcessed;

  if NewDataSize > 0 then
  begin
    FActiveResponseStream.Position := FBytesProcessed;
    SetLength(NewBytes, NewDataSize);
    FActiveResponseStream.ReadBuffer(Pointer(NewBytes)^, NewDataSize);

    FStreamBuffer.Append(TEncoding.UTF8.GetString(NewBytes));
    ProcessStreamBuffer;

    FBytesProcessed := AReadCount;
  end;
end;

procedure TAiDalle.ProcessStreamBuffer;
var
  BufferContent, Line, JsonStr: string;
  Lines: TArray<string>;
  JObj: TJSONValue;
  ImageEvent: TAiDalleImage;
  PartialImageIndex: Integer;
  EventType: string;
  Runner: TStreamCallbackRunner;
  LErrorMsg: string;
begin
  BufferContent := FStreamBuffer.ToString;
  if not BufferContent.Contains(#10) then
    Exit; // No complete line yet

  Lines := CompatSplit(BufferContent, [#10], TStringSplitOptions.ExcludeEmpty);
  FStreamBuffer.Clear;

  for Line in Lines do
  begin
    LErrorMsg := ''; // Reset error flag para cada línea
    if Line.Trim.EndsWith('}') then // A potentially complete JSON line
    begin
      if Line.StartsWith('data: ') then
      begin
        JsonStr := Line.Substring(Length('data: '));
        if JsonStr.Trim.Equals('[DONE]') then
          continue;

        try
          JObj := TJSONObject.ParseJSONValue(JsonStr);
          if JObj is TJSONObject then
          begin
            ImageEvent := TAiDalleImage.Create;
            try
              ImageEvent.ParseStreamEvent(JObj as TJSONObject);
              if (JObj as TJSONObject).TryGetValue('type', EventType) then
              begin
                QueueInMainThread(
                  procedure
                  begin
                    // NOTE: The event handler is responsible for freeing the ImageEvent object.
                    // Adaptación para simular el comportamiento específico de cada plataforma si fuera necesario,
                    // pero mantenemos la lógica unificada aquí.
                    // Para FPC, QueueInMainThread adapta el TThreadProc.
                    
                    if EventType.Contains('partial_image') then
                    begin
                      (JObj as TJSONObject).TryGetValue('partial_image_index', PartialImageIndex);
                      if Assigned(FOnPartialImageReceived) then
                        FOnPartialImageReceived(Self, ImageEvent, PartialImageIndex);
                    end
                    else if EventType.Contains('completed') then
                    begin
                      if Assigned(FOnStreamCompleted) then
                        FOnStreamCompleted(Self, ImageEvent);
                    end
                    else
                      ImageEvent.Free; // Free if not passed to an event
                  end);
              end
              else
                ImageEvent.Free;
            except
              ImageEvent.Free; // Free on parsing error
              raise;
            end;
          end;
        except
          on E: Exception do
          begin
            // COMPAT FPC: Las closures anónimas NO son soportadas dentro de
            // bloques except en FPC 3.3. Se captura el mensaje y se reporta
            // DESPUÉS del bloque except usando el flag LErrorMsg.
            LErrorMsg := 'Error parsing stream event: ' + E.Message;
          end;
        end;

        // Reportar error fuera del except para compatibilidad con FPC 3.3
        if LErrorMsg <> '' then
        begin
          QueueInMainThread(
            procedure
            begin
              if Assigned(FOnStreamError) then
                FOnStreamError(Self, LErrorMsg);
            end);
          LErrorMsg := ''; // Reset para el siguiente ciclo
        end;
      end;
    end
    else
    begin // Incomplete line, put it back in the buffer
      FStreamBuffer.Append(Line + #10);
    end;
  end;
end;

procedure TAiDalle.ParseResponse(JObj: TJSONObject);
var
  Data: TJSONArray;
  i: Integer;
begin
  ClearImages;
  if JObj.TryGetValue('data', Data) then
  begin
    SetLength(FImages, Data.Count);
    for i := 0 to Data.Count - 1 do
    begin
      FImages[i] := TAiDalleImage.Create;
      FImages[i].ParseData(Data.Items[i] as TJSONObject);
    end;
  end;
end;

procedure TAiDalle.SetApiKey(const Value: string);
begin
  FApiKey := Value;
end;

procedure TAiDalle.SetBackground(const Value: TAiImageBackground);
begin
  FBackground := Value;
end;

procedure TAiDalle.SetModel(const Value: TAiImageModel);
begin
  FModel := Value;
end;

procedure TAiDalle.SetNegativePrompt(const Value: TStrings);
begin
  FNegativePrompt.Assign(Value);
end;

procedure TAiDalle.SetOutputFormat(const Value: TAiImageOutputFormat);
begin
  FOutputFormat := Value;
end;

procedure TAiDalle.SetQuality(const Value: TAiImageQuality);
begin
  FQuality := Value;
end;

procedure TAiDalle.SetResponseFormat(const Value: TAiImageResponseFormat);
begin
  FResponseFormat := Value;
end;

procedure TAiDalle.SetStream(const Value: Boolean);
begin
  FStream := Value;
end;

procedure TAiDalle.SetStyle(const Value: TAiImageStyle);
begin
  FStyle := Value;
end;

function TAiDalle.SizeToString(ASize: TAiImageSize): string;
begin
  case ASize of
    is256x256:
      Result := '256x256';
    is512x512:
      Result := '512x512';
    is768x768:
      Result := '768x768';

    is1024x1024:
      Result := '1024x1024';

    is1216x832:
      Result := '1216x832';
    is832x1216:
      Result := '832x1216';

    is1536x1024:
      Result := '1536x1024';
    is1024x1536:
      Result := '1024x1536';

    is1792x1024:
      Result := '1792x1024';
    is1024x1792:
      Result := '1024x1792';
  else
    Result := '1024x1024'; // fallback seguro
  end;
end;

function TAiDalle.Upscale(
  aImage: TAiMediaFile;
  AScale: Integer;
  AFaceEnhance: Boolean
): TAiMediaFile;
var
  Client: TNetHTTPClient;
  Req, ImageObj, RespImage: TJSONObject;
  Res: IHTTPResponse;
  sUrl, B64: string;
begin
  Result := nil;

  if not Assigned(aImage) or (aImage.Content.Size = 0) then
    raise Exception.Create('Image is required for upscale.');

  Client := TNetHTTPClient.Create(nil);
  Req := TJSONObject.Create;
  try
    // URL
    sUrl := FUrl + 'images/upscale';

    // Modelo
    Req.AddPair('model', 'real-esrgan');
    Req.AddPair('scale', CreateJSONNumber(AScale));
    Req.AddPair('face_enhance', CreateJSONBool(AFaceEnhance));

    // Imagen
    ImageObj := TJSONObject.Create;
    ImageObj.AddPair('b64', aImage.Base64);
    ImageObj.AddPair('mime', aImage.MimeType);
    ImageObj.AddPair('filename', aImage.Filename);
    Req.AddPair('image', ImageObj);

    // Headers
    // COMPAT: SetHeader en vez de CustomHeaders[] (Delphi 10.2 no soporta acceso indexado)
    Client.SetHeader('Authorization', 'Bearer ' + ApiKey);
    Client.ContentType := 'application/json';

    // POST
    Res := Client.Post(sUrl, TStringStream.Create(Req.ToString, TEncoding.UTF8));

    if Res.StatusCode <> 200 then
      raise Exception.CreateFmt('Upscale error %d: %s',
        [Res.StatusCode, Res.ContentAsString]);

    // Parse response
    Req.Free;
    Req := TJSONObject.ParseJSONValue(Res.ContentAsString) as TJSONObject;

    // COMPAT: Usar getters explícitos en vez de GetValue<T> genérico (KI master_portability_patterns §JSON)
    RespImage := Req.GetValueAsObject('image');

    Result := TAiMediaFile.Create;
    Result.LoadFromBase64(
      RespImage.GetValueAsString('filename'),
      RespImage.GetValueAsString('b64')
    );

  finally
    Req.Free;
    Client.Free;
  end;
end;

function TAiDalle.Variation(aImageFile: TAiMediaFile; ASize: TAiImageSize; N: Integer): TAiDalleImage;

var
  Body: TMultipartFormData;
  Client: TNetHTTPClient;
  sUrl: string;
  Res: IHTTPResponse;
  JObj: TJSONObject;
begin
  Result := nil;
  ClearImages;

  if not Assigned(aImageFile) then
    raise Exception.Create('An image file must be provided for variation.');
  if FModel <> imDallE2 then
    raise Exception.Create('Variation endpoint only supports dall-e-2 model.');

  sUrl := FUrl + 'images/variations';
  Client := TNetHTTPClient.Create(nil);
  Client.ConfigureForAsync;
  Body := TMultipartFormData.Create;
  try
    aImageFile.Content.Position := 0;
    AddStreamToMultipart(Body, 'image', aImageFile.Content, False, aImageFile.Filename, '');
    Body.AddField('user', FUser);
    Body.AddField('n', N.ToString);

    case aSize of
      is256x256:
        Body.AddField('size', '256x256');
      is512x512:
        Body.AddField('size', '512x512');
    else
      Body.AddField('size', '1024x1024');
    end;

    if ResponseFormat = irfUrl then
      Body.AddField('response_format', 'url')
    else
      Body.AddField('response_format', 'b64_json');

    Client.SetHeader('Authorization', 'Bearer ' + ApiKey);
    Res := Client.Post(sUrl, Body);

    if Res.StatusCode = 200 then
    begin
      JObj := TJSONObject.ParseJSONValue(Res.ContentAsString) as TJSONObject;
      try
        ParseResponse(JObj);
        if Length(FImages) > 0 then
          Result := FImages[0];
      finally
        JObj.Free;
      end;
    end
    else
      raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
  finally
    Client.Free;
    Body.Free;
  end;
end;

end.
