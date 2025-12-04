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


// --- Modificaciones ----
// 30/08/2024 -- Mejora en la función Edit

// --------- CAMBIOS --------------------
// 4/11/2025 - Refactorización completa para soportar dall-e-2, dall-e-3, y gpt-image-1.
// 4/11/2025 - Añadido soporte para streaming con eventos (OnPartialImageReceived, OnStreamCompleted).
// 4/11/2025 - Integración con uMakerAi.Core: Edit y Variation ahora usan TAiMediaFile.
// 4/11/2025 - Modernización de enums y nombres de propiedades para mayor claridad.

unit uMakerAi.OpenAi.Dalle;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Threading, System.Variants, System.Net.Mime, System.IOUtils,
  System.Generics.Collections, System.NetEncoding, System.JSON, System.StrUtils,
  System.Math,
  System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent,
  REST.JSON, REST.Types, REST.Client,
  // Dependencia clave de la librería central
  uMakerAi.Core;

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
  TAiImageModel = (imDallE2, imDallE3, imGptImage1);
  TAiImageQuality = (iqAuto, iqStandard, iqHD, iqHigh, iqMedium, iqLow);
  TAiImageBackground = (ibAuto, ibTransparent, ibOpaque);
  TAiImageOutputFormat = (ifPng, ifJpeg, ifWebp);
  TAiImageStyle = (isVivid, isNatural);
  TAiImageResponseFormat = (irfUrl, irfBase64Json);

  TAiImageSize = (is256x256, // Solo para DALL-E 2
    is512x512, // Solo para DALL-E 2
    is1024x1024, // Soportado por TODOS los modelos
    is1792x1024, // Solo para DALL-E 3 (Formato panorámico/horizontal)
    is1024x1792, // Solo para DALL-E 3 (Formato retrato/vertical)
    is1536x1024, // Solo para gpt-image-1 (Formato panorámico/horizontal)
    is1024x1536 // Solo para gpt-image-1 (Formato retrato/vertical)
    );

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

    function GetApiKey: string;
    procedure SetApiKey(const Value: string);
    procedure SetModel(const Value: TAiImageModel);
    procedure SetQuality(const Value: TAiImageQuality);
    procedure SetResponseFormat(const Value: TAiImageResponseFormat);
    procedure SetStyle(const Value: TAiImageStyle);
    procedure SetStream(const Value: Boolean);
    procedure SetBackground(const Value: TAiImageBackground);
    procedure SetOutputFormat(const Value: TAiImageOutputFormat);
  protected
    procedure ClearImages;
    procedure ParseResponse(JObj: TJSONObject);
    procedure HandleStreamData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean);
    procedure ProcessStreamBuffer;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    function Generate(const aPrompt: string; aSize: TAiImageSize; N: Integer = 1): TAiDalleImage;
    function Edit(aMediaFiles: TAiMediaFiles; aMaskFile: TAiMediaFile; const aPrompt: string; aSize: TAiImageSize; N: Integer = 1): TAiDalleImage;
    function Variation(aImageFile: TAiMediaFile; aSize: TAiImageSize; N: Integer = 1): TAiDalleImage;

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

    // Eventos
    property OnPartialImageReceived: TOnPartialImageReceived read FOnPartialImageReceived write FOnPartialImageReceived;
    property OnStreamCompleted: TOnStreamCompleted read FOnStreamCompleted write FOnStreamCompleted;
    property OnStreamError: TOnStreamError read FOnStreamError write FOnStreamError;
  end;

implementation

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
  try
    NetHttp.Get(AUrlFile, FImageStream);
    FImageStream.Position := 0;
  finally
    NetHttp.Free;
  end;
end;

procedure TAiDalleImage.ParseData(JObj: TJSONObject);
begin
  JObj.TryGetValue<string>('url', FUrlFile);
  if JObj.TryGetValue<string>('b64_json', FBase64) then
  begin
    FImageStream.Free;
    FImageStream := Base64ToStream(FBase64);
  end;
  JObj.TryGetValue<string>('revised_prompt', FRevisedPrompt);
  JObj.TryGetValue<string>('background', FBackground);
  JObj.TryGetValue<string>('output_format', FOutputFormat);
  JObj.TryGetValue<string>('size', FSize);
  JObj.TryGetValue<string>('quality', FQuality);
  if JObj.TryGetValue<TJSONObject>('usage', FUsage) then
    FUsage := TJSONObject(FUsage.Clone);
end;

procedure TAiDalleImage.ParseStreamEvent(JObj: TJSONObject);
begin
  if JObj.TryGetValue<string>('b64_json', FBase64) then
  begin
    FImageStream.Free;
    FImageStream := Base64ToStream(FBase64);
  end;
  JObj.TryGetValue<string>('background', FBackground);
  JObj.TryGetValue<string>('output_format', FOutputFormat);
  JObj.TryGetValue<string>('size', FSize);
  JObj.TryGetValue<string>('quality', FQuality);
  if JObj.TryGetValue<TJSONObject>('usage', FUsage) then
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
end;

destructor TAiDalle.Destroy;
begin
  ClearImages;
  FStreamBuffer.Free;
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

function TAiDalle.Edit(aMediaFiles: TAiMediaFiles; aMaskFile: TAiMediaFile; const aPrompt: string; aSize: TAiImageSize; N: Integer): TAiDalleImage;
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
  Body := TMultipartFormData.Create;
  try
    for MediaFile in aMediaFiles do
    begin
      MediaFile.Content.Position := 0;
      if FModel = imGptImage1 then
        Body.AddStream('image[]', MediaFile.Content, False, MediaFile.Filename)
      else
        Body.AddStream('image', MediaFile.Content, False, MediaFile.Filename);
    end;

    if Assigned(aMaskFile) then
    begin
      aMaskFile.Content.Position := 0;
      Body.AddStream('mask', aMaskFile.Content, False, aMaskFile.Filename);
    end;

    Body.AddField('prompt', aPrompt);
    Body.AddField('user', FUser);
    Body.AddField('n', N.ToString);

    case FModel of
      imDallE2:
        begin
          Body.AddField('model', 'dall-e-2');
          case aSize of
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
          case aSize of
            is1024x1536:
              Body.AddField('size', '1024x1536');
            is1536x1024:
              Body.AddField('size', '1536x1024');
          else
            Body.AddField('size', '1024x1024');
          end;
        end;
    end;

    Client.CustomHeaders['Authorization'] := 'Bearer ' + ApiKey;
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

function TAiDalle.Generate(const aPrompt: string; aSize: TAiImageSize; N: Integer): TAiDalleImage;
var
  Client: TNetHTTPClient;
  JObj: TJSONObject;
  Res: IHTTPResponse;
  ContentStream: TStringStream;
  ResponseStream: TMemoryStream; // Usado solo para llamadas no-streaming
  sUrl: string;
  AbortFlag: Boolean; // Variable para el parámetro 'var'
  StreamReader: TStreamReader; // Para leer la respuesta de forma robusta
begin
  Result := nil;
  ClearImages;
  FPrompt := aPrompt;
  sUrl := FUrl + 'images/generations';
  Client := TNetHTTPClient.Create(nil);
  JObj := TJSONObject.Create;
  ContentStream := TStringStream.Create('', TEncoding.UTF8);
  FActiveResponseStream := nil;
  try
    JObj.AddPair('prompt', aPrompt).AddPair('user', FUser);
    case FModel of
      imDallE2:
        begin
          JObj.AddPair('model', 'dall-e-2').AddPair('n', Min(10, Max(1, N)));
          case aSize of
            is256x256:
              JObj.AddPair('size', '256x256');
            is512x512:
              JObj.AddPair('size', '512x512');
          else
            JObj.AddPair('size', '1024x1024');
          end;
        end;
      imDallE3:
        begin
          JObj.AddPair('model', 'dall-e-3').AddPair('n', 1);
          case aSize of
            is1792x1024:
              JObj.AddPair('size', '1792x1024');
            is1024x1792:
              JObj.AddPair('size', '1024x1792');
          else
            JObj.AddPair('size', '1024x1024');
          end;
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
          case aSize of
            is1536x1024:
              JObj.AddPair('size', '1536x1024');
            is1024x1536:
              JObj.AddPair('size', '1024x1536');
          else
            JObj.AddPair('size', '1024x1024');
          end;
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
            JObj.AddPair('stream', TJSONBool.Create(True));
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
    Client.CustomHeaders['Authorization'] := 'Bearer ' + ApiKey;
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
          StreamReader := TStreamReader.Create(ResponseStream, TEncoding.UTF8);
          try
            JObj := TJSONObject.ParseJSONValue(StreamReader.ReadToEnd) as TJSONObject;
          finally
            StreamReader.Free;
          end;

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
          StreamReader := TStreamReader.Create(ResponseStream, TEncoding.UTF8);
          try
            raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, StreamReader.ReadToEnd]);
          finally
            StreamReader.Free;
          end;
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
    if not(FStream and (FModel = imGptImage1)) then
      JObj.Free;
  end;
end;

function TAiDalle.GetApiKey: string;
begin
  if (csDesigning in ComponentState) then
    Result := FApiKey
  else if (FApiKey <> '') and (FApiKey.StartsWith('@')) then
    Result := GetEnvironmentVariable(FApiKey.Substring(1))
  else
    Result := FApiKey;
end;

procedure TAiDalle.HandleStreamData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean);
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
begin
  BufferContent := FStreamBuffer.ToString;
  if not BufferContent.Contains(#10) then
    Exit; // No complete line yet

  Lines := BufferContent.Split([#10], TStringSplitOptions.ExcludeEmpty);
  FStreamBuffer.Clear;

  for Line in Lines do
  begin
    if Line.Trim.EndsWith('}') then // A potentially complete JSON line
    begin
      if Line.StartsWith('data: ') then
      begin
        JsonStr := Line.Substring('data: '.Length);
        if JsonStr.Trim.Equals('[DONE]') then
          continue;

        try
          JObj := TJSONObject.ParseJSONValue(JsonStr);
          if JObj is TJSONObject then
          begin
            ImageEvent := TAiDalleImage.Create;
            try
              ImageEvent.ParseStreamEvent(JObj as TJSONObject);
              if (JObj as TJSONObject).TryGetValue<string>('type', EventType) then
              begin
                TThread.Queue(nil,
                  procedure
                  begin
                    // NOTE: The event handler is responsible for freeing the ImageEvent object.
                    if EventType.Contains('partial_image') then
                    begin
                      (JObj as TJSONObject).TryGetValue<Integer>('partial_image_index', PartialImageIndex);
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
            TThread.Queue(nil,
              procedure
              begin
                if Assigned(FOnStreamError) then
                  FOnStreamError(Self, 'Error parsing stream event: ' + E.Message);
              end);
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
  if JObj.TryGetValue<TJSONArray>('data', Data) then
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

function TAiDalle.Variation(aImageFile: TAiMediaFile; aSize: TAiImageSize; N: Integer): TAiDalleImage;
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
  Body := TMultipartFormData.Create;
  try
    aImageFile.Content.Position := 0;
    Body.AddStream('image', aImageFile.Content, False, aImageFile.Filename);
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

    Client.CustomHeaders['Authorization'] := 'Bearer ' + ApiKey;
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
