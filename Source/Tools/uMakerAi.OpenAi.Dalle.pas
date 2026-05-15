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
// Nombre: Gustavo Enr?quez
// Redes Sociales:
// - Email: gustavoeenriquez@gmail.com

// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - Telegram: https://t.me/MakerAi_Delphi_Suite_English

// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/


// --- Modificaciones ----
// 30/08/2024 -- Mejora en la funci?n Edit

// --------- CAMBIOS --------------------
// 4/11/2025  - Refactorizaci?n completa para soportar dall-e-2, dall-e-3, y gpt-image-1.
// 4/11/2025  - A?adido soporte para streaming con eventos (OnPartialImageReceived, OnStreamCompleted).
// 4/11/2025  - Integraci?n con uMakerAi.Core: Edit y Variation ahora usan TAiMediaFile.
// 4/11/2025  - Modernizaci?n de enums y nombres de propiedades para mayor claridad.
// 27/04/2026 - Soporte completo para gpt-image-2, gpt-image-1-mini, gpt-image-1.5, chatgpt-image-latest.
// 27/04/2026 - Nuevos par?metros: output_compression, moderation, partial_images, input_fidelity.
// 27/04/2026 - Fix: response_format excluido para todos los modelos GPT Image.
// 27/04/2026 - Fix: streaming bloqueado para gpt-image-2 (no soportado por la API).
// 27/04/2026 - Nuevo tama?o isAuto para modelos GPT Image.
// 28/04/2026 - gpt-image-2: n limitado a 8 (no 10), background transparent no soportado,
//              input_fidelity excluido en edits (auto-high-fidelity siempre).
// 28/04/2026 - Nuevo tama?o is3840x2160 (4K UHD) para gpt-image-2.

unit uMakerAi.OpenAi.Dalle;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Threading, System.Variants, System.Net.Mime, System.IOUtils,
  System.Generics.Collections, System.NetEncoding, System.JSON, System.StrUtils,
  System.Math,
  System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent,
  REST.JSON, REST.Types, REST.Client,
{$IF CompilerVersion < 35}
  uJSONHelper,
{$ENDIF}
  // Dependencia clave de la librer?a central
  uMakerAi.Core, uMakerAi.Chat.Messages, uMakerAi.Chat.Tools;

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

  // Modelos soportados
  // Nota: gpt-image-2 NO soporta streaming (SupportsStreaming = False para ese modelo)
  TAiImageModel = (
    imDallE2,             // dall-e-2
    imDallE3,             // dall-e-3
    imGptImage1,          // gpt-image-1
    imGptImage1Mini,      // gpt-image-1-mini  (m?s r?pido y econ?mico)
    imGptImage15,         // gpt-image-1.5
    imGptImage2,          // gpt-image-2       (SIN streaming)
    imChatGptImageLatest, // chatgpt-image-latest (alias al modelo m?s reciente)
    imSDXL                // SDXL (endpoint personalizado)
  );

  TAiImageQuality    = (iqAuto, iqStandard, iqHD, iqHigh, iqMedium, iqLow);
  TAiImageBackground = (ibAuto, ibTransparent, ibOpaque);
  TAiImageOutputFormat = (ifPng, ifJpeg, ifWebp);
  TAiImageStyle      = (isVivid, isNatural);
  TAiImageResponseFormat = (irfUrl, irfBase64Json);

  // Solo modelos GPT Image: 'auto' desactiva moderaci?n extra
  TAiImageModeration = (imodAuto, imodLow);

  // Para el endpoint /images/edits: controla cu?nto se preserva la imagen original
  TAiInputFidelity = (ifdDefault, ifdHigh, ifdLow);

  TAiImageSize = (
    is256x256,   // dall-e-2
    is512x512,   // dall-e-2
    is1024x1024, // todos
    is1792x1024, // dall-e-3 landscape
    is1024x1792, // dall-e-3 portrait
    is1536x1024, // gpt-image-* landscape
    is1024x1536, // gpt-image-* portrait
    // SDXL-friendly
    is768x768,
    is1216x832,
    is832x1216,
    // Solo modelos GPT Image
    isAuto,
    // gpt-image-2: alta resoluci?n (experimental: >2K)
    is3840x2160  // 4K UHD (m?x pixel budget: 8.294.400)
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
    FNegativePrompt: TStrings;
    FSeed: Int64;
    FSteps: Integer;
    FGuidanceScale: Single;
    FUseRefiner: Boolean;
    FSampler: string;
    FEnhanceFace: Boolean;
    FAutoUpscale: Boolean;
    FStrength: Single;
    FLoraPath: String;
    // Nuevos par?metros (v3.4)
    FOutputCompression: Integer;    // 0 = no enviar; 1-100 = comprimir (jpeg/webp)
    FModeration: TAiImageModeration;
    FPartialImages: Integer;        // 0-3 im?genes parciales en streaming
    FInputFidelity: TAiInputFidelity;

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
    procedure SetAutoUpscale(const Value: Boolean);
    procedure SetEnhanceFace(const Value: Boolean);
    procedure SetLoraPath(const Value: String);
    procedure SetStrength(const Value: Single);
    procedure SetOutputCompression(const Value: Integer);
    procedure SetModeration(const Value: TAiImageModeration);
    procedure SetPartialImages(const Value: Integer);
    procedure SetInputFidelity(const Value: TAiInputFidelity);
  protected
    procedure ClearImages;
    procedure ParseResponse(JObj: TJSONObject);
    procedure HandleStreamData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean);
    procedure ProcessStreamBuffer;
    function SizeToString(ASize: TAiImageSize): string;
    // Helpers de modelo
    function IsGptImageModel: Boolean;
    function SupportsStreaming: Boolean;
    function ModelToString: string;
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
    property OutputCompression: Integer read FOutputCompression write SetOutputCompression default 0;
    property Moderation: TAiImageModeration read FModeration write SetModeration default TAiImageModeration.imodAuto;
    property PartialImages: Integer read FPartialImages write SetPartialImages default 1;
    property InputFidelity: TAiInputFidelity read FInputFidelity write SetInputFidelity default TAiInputFidelity.ifdDefault;
    property NegativePrompt: TStrings read FNegativePrompt write SetNegativePrompt;

    // SDXL properties
    property Steps: Integer read FSteps write FSteps default 30;
    property GuidanceScale: Single read FGuidanceScale write FGuidanceScale;
    property Seed: Int64 read FSeed write FSeed Default -1;
    property UseRefiner: Boolean read FUseRefiner write FUseRefiner default False;
    property AutoUpscale: Boolean read FAutoUpscale write SetAutoUpscale;
    Property EnhanceFace: Boolean read FEnhanceFace write SetEnhanceFace;
    Property Strength: Single read FStrength write SetStrength;
    Property LoraPath: String read FLoraPath write SetLoraPath;

    // Eventos
    property OnPartialImageReceived: TOnPartialImageReceived read FOnPartialImageReceived write FOnPartialImageReceived;
    property OnStreamCompleted: TOnStreamCompleted read FOnStreamCompleted write FOnStreamCompleted;
    property OnStreamError: TOnStreamError read FOnStreamError write FOnStreamError;
  end;

  { TAiDalleImageTool: ImageTool aut?nomo para generaci?n de im?genes con DALL-E/gpt-image-1.
    Crea internamente su propio TAiDalle — no requiere un componente externo.
    Configura ApiKey, Model e ImageSize y as?gnalo a ChatTools.ImageTool. }
  TAiDalleImageTool = class(TAiImageToolBase)
  private
    FDalle    : TAiDalle;
    FImageSize: TAiImageSize;
    function  GetApiKey: string;
    procedure SetApiKey(const Value: string);
    function  GetUrl: string;
    procedure SetUrl(const Value: string);
    function  GetModel: TAiImageModel;
    procedure SetModel(const Value: TAiImageModel);
    function  GetQuality: TAiImageQuality;
    procedure SetQuality(const Value: TAiImageQuality);
    function  GetStyle: TAiImageStyle;
    procedure SetStyle(const Value: TAiImageStyle);
    function  GetOutputFormat: TAiImageOutputFormat;
    procedure SetOutputFormat(const Value: TAiImageOutputFormat);
    function  GetBackground: TAiImageBackground;
    procedure SetBackground(const Value: TAiImageBackground);
    function  GetNegativePrompt: TStrings;
    procedure SetNegativePrompt(const Value: TStrings);
    function  GetOutputCompression: Integer;
    procedure SetOutputCompression(const Value: Integer);
    function  GetModeration: TAiImageModeration;
    procedure SetModeration(const Value: TAiImageModeration);
    function  GetInputFidelity: TAiInputFidelity;
    procedure SetInputFidelity(const Value: TAiInputFidelity);
    procedure SetDalle(const Value: TAiDalle);
  protected
    procedure ExecuteImageGeneration(const APrompt: string; ResMsg, AskMsg: TAiChatMessage); override;
    procedure InternalRunDalleGeneration(const APrompt: string; ResMsg: TAiChatMessage);
  public
    constructor Create(AOwner: TComponent); override;
    { Permite inyectar un TAiDalle ya configurado. El caller sigue siendo due?o
      del objeto y debe liberarlo. El FDalle interno original queda en la lista
      de componentes hijos y ser? liberado autom?ticamente por el framework. }
    property Dalle: TAiDalle read FDalle write SetDalle;
  published
    property ApiKey            : string               read GetApiKey            write SetApiKey;
    property Url               : string               read GetUrl               write SetUrl;
    property Model             : TAiImageModel        read GetModel             write SetModel        default imDallE3;
    property Quality           : TAiImageQuality      read GetQuality           write SetQuality      default iqAuto;
    property Style             : TAiImageStyle        read GetStyle             write SetStyle        default isVivid;
    property OutputFormat      : TAiImageOutputFormat read GetOutputFormat      write SetOutputFormat default ifPng;
    property Background        : TAiImageBackground   read GetBackground        write SetBackground   default ibAuto;
    property OutputCompression : Integer              read GetOutputCompression write SetOutputCompression default 0;
    property Moderation        : TAiImageModeration   read GetModeration        write SetModeration   default imodAuto;
    property InputFidelity     : TAiInputFidelity     read GetInputFidelity     write SetInputFidelity default ifdDefault;
    property NegativePrompt    : TStrings             read GetNegativePrompt   write SetNegativePrompt;
    property ImageSize         : TAiImageSize         read FImageSize          write FImageSize      default is1024x1024;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiDalle, TAiDalleImageTool]);
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
var
  B64: string;
begin
  B64 := '';
  if not JObj.TryGetValue<string>('b64_json', B64) then
    JObj.TryGetValue<string>('partial_image_b64', B64);
  if B64 <> '' then
  begin
    FBase64 := B64;
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
  FNegativePrompt := TStringList.Create;

  FSteps := 30;
  FGuidanceScale := 7.0;
  FSeed := -1;
  FUseRefiner := False;
  FAutoUpscale := False;
  FEnhanceFace := False;

  // v3.4 new defaults
  FOutputCompression := 0;
  FModeration := imodAuto;
  FPartialImages := 1;
  FInputFidelity := ifdDefault;
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

// ---------------------------------------------------------------------------
//  Helpers de modelo
// ---------------------------------------------------------------------------

function TAiDalle.IsGptImageModel: Boolean;
begin
  Result := FModel in [imGptImage1, imGptImage1Mini, imGptImage15,
                       imGptImage2, imChatGptImageLatest];
end;

// gpt-image-2 NO soporta streaming segun la documentacion oficial
function TAiDalle.SupportsStreaming: Boolean;
begin
  Result := FModel in [imGptImage1, imGptImage1Mini, imGptImage15,
                       imChatGptImageLatest];
end;

function TAiDalle.ModelToString: string;
begin
  case FModel of
    imDallE2:             Result := 'dall-e-2';
    imDallE3:             Result := 'dall-e-3';
    imGptImage1:          Result := 'gpt-image-1';
    imGptImage1Mini:      Result := 'gpt-image-1-mini';
    imGptImage15:         Result := 'gpt-image-1.5';
    imGptImage2:          Result := 'gpt-image-2';
    imChatGptImageLatest: Result := 'chatgpt-image-latest';
    imSDXL:               Result := 'sdxl';
  else
    Result := 'gpt-image-1';
  end;
end;

// ---------------------------------------------------------------------------
//  Generate
// ---------------------------------------------------------------------------

function TAiDalle.Generate(const aPrompt, aNegativePrompt: string; ASize: TAiImageSize;
  N: Integer; aImage: TMemoryStream): TAiDalleImage;
var
  Client: TNetHTTPClient;
  JObj: TJSONObject;
  Res: IHTTPResponse;
  ContentStream: TStringStream;
  ResponseStream: TMemoryStream;
  AbortFlag: Boolean;
  StreamReader: TStreamReader;
begin
  Result := nil;
  ClearImages;
  FPrompt := aPrompt;

  Client := TNetHTTPClient.Create(nil);
  ContentStream := TStringStream.Create('', TEncoding.UTF8);
  FActiveResponseStream := nil;
  JObj := TJSONObject.Create;
  try
    // Prompt y user (comunes a todos los modelos)
    JObj.AddPair('prompt', aPrompt).AddPair('user', FUser);

    case FModel of

      // ── DALL-E 2 ────────────────────────────────────────────────────────
      imDallE2:
      begin
        JObj.AddPair('model', 'dall-e-2');
        JObj.AddPair('n', TJSONNumber.Create(Min(10, Max(1, N))));
        JObj.AddPair('size', SizeToString(ASize));
      end;

      // ── DALL-E 3 ────────────────────────────────────────────────────────
      imDallE3:
      begin
        JObj.AddPair('model', 'dall-e-3').AddPair('n', TJSONNumber.Create(1));
        JObj.AddPair('size', SizeToString(ASize));
        if FQuality in [iqHD, iqHigh] then
          JObj.AddPair('quality', 'hd')
        else
          JObj.AddPair('quality', 'standard');
        if FStyle = isVivid then
          JObj.AddPair('style', 'vivid')
        else
          JObj.AddPair('style', 'natural');
      end;

      // ── Familia GPT Image (gpt-image-1/mini/1.5/2/chatgpt-image-latest) ─
      imGptImage1, imGptImage1Mini, imGptImage15, imGptImage2, imChatGptImageLatest:
      begin
        JObj.AddPair('model', ModelToString);
        // gpt-image-2 soporta n=1..8; resto de la familia hasta 10
        if FModel = imGptImage2 then
          JObj.AddPair('n', TJSONNumber.Create(Min(8, Max(1, N))))
        else
          JObj.AddPair('n', TJSONNumber.Create(Min(10, Max(1, N))));
        JObj.AddPair('size', SizeToString(ASize));

        case FQuality of
          iqHigh:   JObj.AddPair('quality', 'high');
          iqMedium: JObj.AddPair('quality', 'medium');
          iqLow:    JObj.AddPair('quality', 'low');
          // iqAuto: no enviar, la API usa su default
        end;

        case FBackground of
          ibTransparent:
            // gpt-image-2 no soporta fondo transparente
            if FModel <> imGptImage2 then
              JObj.AddPair('background', 'transparent');
          ibOpaque: JObj.AddPair('background', 'opaque');
          // ibAuto: no enviar
        end;

        case FOutputFormat of
          ifJpeg: JObj.AddPair('output_format', 'jpeg');
          ifWebp: JObj.AddPair('output_format', 'webp');
          // ifPng: default, no enviar
        end;

        // Compresi?n solo para jpeg/webp y cuando se especifica
        if (FOutputCompression > 0) and (FOutputFormat in [ifJpeg, ifWebp]) then
          JObj.AddPair('output_compression', TJSONNumber.Create(
            Max(0, Min(100, FOutputCompression))));

        // Moderaci?n
        if FModeration = imodLow then
          JObj.AddPair('moderation', 'low');

        // Streaming solo en modelos que lo soportan (NO gpt-image-2)
        if FStream and SupportsStreaming then
        begin
          JObj.AddPair('stream', TJSONBool.Create(True));
          if FPartialImages > 0 then
            JObj.AddPair('partial_images', TJSONNumber.Create(
              Max(0, Min(3, FPartialImages))));
        end;
      end;

      // ── SDXL (endpoint personalizado) ───────────────────────────────────
      imSDXL:
      begin
        JObj.AddPair('model', 'sdxl');
        JObj.AddPair('prompt', Trim(StringReplace(aPrompt, #$D#$A, ' \n', [rfReplaceAll])));

        if aNegativePrompt <> '' then
          JObj.AddPair('negative_prompt', Trim(StringReplace(aNegativePrompt, #$D#$A, ' \n', [rfReplaceAll])));

        if Assigned(aImage) and (aImage.Size > 2000) then
          JObj.AddPair('image_b64', TNetEncoding.Base64.EncodeBytesToString(aImage.Memory, aImage.Size));

        JObj.AddPair('steps', TJSONNumber.Create(FSteps));
        JObj.AddPair('guidance_scale', TJSONNumber.Create(FGuidanceScale));

        if FSeed >= 0 then
          JObj.AddPair('seed', TJSONNumber.Create(FSeed));

        if FUseRefiner then
          JObj.AddPair('use_refiner', TJSONBool.Create(True));

        JObj.AddPair('size', SizeToString(ASize));

        if FAutoUpscale then
          JObj.AddPair('auto_upscale', TJSONBool.Create(True));

        if FEnhanceFace then
          JObj.AddPair('enhance_face', TJSONBool.Create(True));

        if FStrength <> 0 then
          JObj.AddPair('strength', FStrength);

        if FLoraPath <> '' then
          JObj.AddPair('lora_path', FLoraPath);
      end;

    end; // case FModel

    // response_format solo para modelos NO-GPT (dall-e-2, dall-e-3, sdxl)
    if not IsGptImageModel then
    begin
      if FResponseFormat = irfUrl then
        JObj.AddPair('response_format', 'url')
      else
        JObj.AddPair('response_format', 'b64_json');
    end;

    ContentStream.WriteString(JObj.ToJSON);
    ContentStream.Position := 0;
    FreeAndNil(JObj); // request JObj ya serializado, liberamos

    Client.CustomHeaders['Authorization'] := 'Bearer ' + ApiKey;
    Client.ContentType := 'application/json';

    // ── Ruta streaming ──────────────────────────────────────────────────────
    if FStream and SupportsStreaming then
    begin
      Client.Accept := 'text/event-stream';
      FStreamBuffer.Clear;
      FBytesProcessed := 0;
      Client.OnReceiveData := HandleStreamData;

      FActiveResponseStream := TMemoryStream.Create;
      Client.Post(FUrl + 'images/generations', ContentStream, FActiveResponseStream);

      // DEBUG: dump raw streaming response
      if Assigned(FActiveResponseStream) and (FActiveResponseStream.Size > 0) then
      begin
        FActiveResponseStream.Position := 0;
        var DbgReader := TStreamReader.Create(FActiveResponseStream, TEncoding.UTF8);
        try
          var DbgContent := DbgReader.ReadToEnd;
          TFile.WriteAllText(TPath.GetTempPath + 'dalle_stream_debug.txt',
            DbgContent, TEncoding.UTF8);
        finally
          DbgReader.Free;
        end;
      end;

      if Assigned(FActiveResponseStream) and
         (FActiveResponseStream.Size > FBytesProcessed) then
      begin
        AbortFlag := False;
        HandleStreamData(Client, FActiveResponseStream.Size,
          FActiveResponseStream.Size, AbortFlag);
      end;
    end
    // ── Ruta sin streaming ──────────────────────────────────────────────────
    else
    begin
      ResponseStream := TMemoryStream.Create;
      try
        Res := Client.Post(FUrl + 'images/generations', ContentStream, ResponseStream);
        ResponseStream.Position := 0;

        if Res.StatusCode = 200 then
        begin
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
            FreeAndNil(JObj);
          end;
        end
        else
        begin
          StreamReader := TStreamReader.Create(ResponseStream, TEncoding.UTF8);
          try
            raise Exception.CreateFmt('Error Received: %d, %s',
              [Res.StatusCode, StreamReader.ReadToEnd]);
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
    JObj.Free; // nil-safe: cubre el caso de excepci?n antes de FreeAndNil
    if Assigned(FActiveResponseStream) then
    begin
      FActiveResponseStream.Free;
      FActiveResponseStream := nil;
    end;
  end;
end;

// ---------------------------------------------------------------------------
//  Edit
// ---------------------------------------------------------------------------

function TAiDalle.Edit(aMediaFiles: TAiMediaFiles; aMaskFile: TAiMediaFile;
  const aPrompt: string; ASize: TAiImageSize; N: Integer): TAiDalleImage;
var
  Body: TMultipartFormData;
  Client: TNetHTTPClient;
  Res: IHTTPResponse;
  JObj: TJSONObject;
  MediaFile: TAiMediaFile;
begin
  Result := nil;
  ClearImages;

  if not Assigned(aMediaFiles) or (aMediaFiles.Count = 0) then
    raise Exception.Create('At least one media file must be provided for editing.');

  if not (FModel in [imDallE2, imGptImage1, imGptImage1Mini, imGptImage15,
                     imGptImage2, imChatGptImageLatest]) then
    raise Exception.Create('Edit endpoint supports dall-e-2 and all gpt-image-* models.');

  if (FModel = imDallE2) and (aMediaFiles.Count > 1) then
    raise Exception.Create('DALL-E 2 only supports one image for editing.');

  Client := TNetHTTPClient.Create(nil);
  Body := TMultipartFormData.Create;
  try
    // Im?genes de entrada
    for MediaFile in aMediaFiles do
    begin
      MediaFile.Content.Position := 0;
{$IF CompilerVersion < 35}
      if IsGptImageModel then
        Body.AddStream('image[]', MediaFile.Content, MediaFile.Filename)
      else
        Body.AddStream('image', MediaFile.Content, MediaFile.Filename);
{$ELSE}
      if IsGptImageModel then
        Body.AddStream('image[]', MediaFile.Content, False, MediaFile.Filename)
      else
        Body.AddStream('image', MediaFile.Content, False, MediaFile.Filename);
{$ENDIF}
    end;

    // M?scara opcional
    if Assigned(aMaskFile) then
    begin
      aMaskFile.Content.Position := 0;
{$IF CompilerVersion < 35}
      Body.AddStream('mask', aMaskFile.Content, aMaskFile.Filename);
{$ELSE}
      Body.AddStream('mask', aMaskFile.Content, False, aMaskFile.Filename);
{$ENDIF}
    end;

    Body.AddField('prompt', aPrompt);
    Body.AddField('user', FUser);
    Body.AddField('n', N.ToString);

    // Modelo y tama?o
    case FModel of
      imDallE2:
      begin
        Body.AddField('model', 'dall-e-2');
        case ASize of
          is256x256: Body.AddField('size', '256x256');
          is512x512: Body.AddField('size', '512x512');
        else         Body.AddField('size', '1024x1024');
        end;
      end;
    else
      // Todos los modelos GPT Image
      Body.AddField('model', ModelToString);
      Body.AddField('size', SizeToString(ASize));

      case FQuality of
        iqHigh:   Body.AddField('quality', 'high');
        iqMedium: Body.AddField('quality', 'medium');
        iqLow:    Body.AddField('quality', 'low');
      end;

      case FBackground of
        ibTransparent:
          // gpt-image-2 no soporta fondo transparente
          if FModel <> imGptImage2 then
            Body.AddField('background', 'transparent');
        ibOpaque: Body.AddField('background', 'opaque');
      end;

      case FOutputFormat of
        ifJpeg: Body.AddField('output_format', 'jpeg');
        ifWebp: Body.AddField('output_format', 'webp');
      end;

      // gpt-image-2: siempre alta fidelidad autom?ticamente — omitir el par?metro
      // Resto de modelos: controla cu?nto preservar de la imagen original
      if FModel <> imGptImage2 then
        case FInputFidelity of
          ifdHigh: Body.AddField('input_fidelity', 'high');
          ifdLow:  Body.AddField('input_fidelity', 'low');
        end;

      if (FOutputCompression > 0) and (FOutputFormat in [ifJpeg, ifWebp]) then
        Body.AddField('output_compression',
          IntToStr(Max(0, Min(100, FOutputCompression))));

      if FModeration = imodLow then
        Body.AddField('moderation', 'low');
    end;

    Client.CustomHeaders['Authorization'] := 'Bearer ' + ApiKey;
    Res := Client.Post(FUrl + 'images/edits', Body);

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
      raise Exception.CreateFmt('Error Received: %d, %s',
        [Res.StatusCode, Res.ContentAsString]);
  finally
    Client.Free;
    Body.Free;
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

procedure TAiDalle.HandleStreamData(const Sender: TObject;
  AContentLength, AReadCount: Int64; var AAbort: Boolean);
var
  NewBytes: TBytes;
  NewDataSize: Int64;
begin
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
  JVal: TJSONValue;
  Img: TAiDalleImage;
  EvType: string;
  EvIdx: Integer;
begin
  BufferContent := FStreamBuffer.ToString;
  if not BufferContent.Contains(#10) then
    Exit;

  Lines := BufferContent.Split([#10], TStringSplitOptions.ExcludeEmpty);
  FStreamBuffer.Clear;

  for Line in Lines do
  begin
    if not Line.Trim.EndsWith('}') then
    begin
      FStreamBuffer.Append(Line + #10);
      Continue;
    end;

    if not Line.TrimLeft.StartsWith('data: ') then
      Continue;

    JsonStr := Line.TrimLeft.Substring('data: '.Length).Trim;
    if JsonStr.Equals('[DONE]') then
      Continue;

    JVal := nil;
    Img := nil;
    try
      try
        JVal := TJSONObject.ParseJSONValue(JsonStr);
        if not (JVal is TJSONObject) then
          Continue;

        EvType := '';
        EvIdx  := 0;
        (JVal as TJSONObject).TryGetValue<string>('type', EvType);
        (JVal as TJSONObject).TryGetValue<Integer>('partial_image_index', EvIdx);

        Img := TAiDalleImage.Create;
        Img.ParseStreamEvent(JVal as TJSONObject);

        if EvType = '' then
        begin
          FreeAndNil(Img);
          Continue;
        end;

        // Inline vars: each loop iteration creates independent closure cells
        var LocalType  := EvType;
        var LocalIdx   := EvIdx;
        var LocalImage := Img;
        Img := nil; // ownership transferred to closure

        if LocalType.Contains('partial_image') then
        begin
          if Assigned(FOnPartialImageReceived) then
            FOnPartialImageReceived(Self, LocalImage, LocalIdx)
          else
            FreeAndNil(LocalImage);
        end
        else if LocalType.Contains('completed') or
                LocalType.Contains('image_generation_result') or
                LocalType.Contains('output_item.done') or
                LocalType.Contains('response.done') then
        begin
          if Assigned(FOnStreamCompleted) then
            FOnStreamCompleted(Self, LocalImage)
          else
            FreeAndNil(LocalImage);
        end
        else
          FreeAndNil(LocalImage);

      except
        on E: Exception do
        begin
          FreeAndNil(Img);
          if Assigned(FOnStreamError) then
            FOnStreamError(Self, 'Error parsing stream event: ' + E.Message);
        end;
      end;
    finally
      FreeAndNil(JVal);
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

procedure TAiDalle.SetAutoUpscale(const Value: Boolean);
begin
  FAutoUpscale := Value;
end;

procedure TAiDalle.SetBackground(const Value: TAiImageBackground);
begin
  FBackground := Value;
end;

procedure TAiDalle.SetEnhanceFace(const Value: Boolean);
begin
  FEnhanceFace := Value;
end;

procedure TAiDalle.SetInputFidelity(const Value: TAiInputFidelity);
begin
  FInputFidelity := Value;
end;

procedure TAiDalle.SetLoraPath(const Value: String);
begin
  FLoraPath := Value;
end;

procedure TAiDalle.SetModel(const Value: TAiImageModel);
begin
  FModel := Value;
end;

procedure TAiDalle.SetModeration(const Value: TAiImageModeration);
begin
  FModeration := Value;
end;

procedure TAiDalle.SetNegativePrompt(const Value: TStrings);
begin
  FNegativePrompt.Assign(Value);
end;

procedure TAiDalle.SetOutputCompression(const Value: Integer);
begin
  FOutputCompression := Max(0, Min(100, Value));
end;

procedure TAiDalle.SetOutputFormat(const Value: TAiImageOutputFormat);
begin
  FOutputFormat := Value;
end;

procedure TAiDalle.SetPartialImages(const Value: Integer);
begin
  FPartialImages := Max(0, Min(3, Value));
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

procedure TAiDalle.SetStrength(const Value: Single);
begin
  FStrength := Value;
end;

procedure TAiDalle.SetStyle(const Value: TAiImageStyle);
begin
  FStyle := Value;
end;

function TAiDalle.SizeToString(ASize: TAiImageSize): string;
begin
  case ASize of
    is256x256:  Result := '256x256';
    is512x512:  Result := '512x512';
    is768x768:  Result := '768x768';
    is1024x1024: Result := '1024x1024';
    is1216x832: Result := '1216x832';
    is832x1216: Result := '832x1216';
    is1536x1024: Result := '1536x1024';
    is1024x1536: Result := '1024x1536';
    is1792x1024: Result := '1792x1024';
    is1024x1792: Result := '1024x1792';
    isAuto:      Result := 'auto';
    is3840x2160: Result := '3840x2160';
  else
    Result := '1024x1024';
  end;
end;

function TAiDalle.Upscale(aImage: TAiMediaFile; AScale: Integer; AFaceEnhance: Boolean): TAiMediaFile;
var
  Client: TNetHTTPClient;
  Req, ImageObj, RespImage: TJSONObject;
  Res: IHTTPResponse;
begin
  Result := nil;

  if not Assigned(aImage) or (aImage.Content.Size = 0) then
    raise Exception.Create('Image is required for upscale.');

  Client := TNetHTTPClient.Create(nil);
  Req := TJSONObject.Create;
  try
    Req.AddPair('model', 'real-esrgan');
    Req.AddPair('scale', TJSONNumber.Create(AScale));
    Req.AddPair('face_enhance', TJSONBool.Create(AFaceEnhance));

    ImageObj := TJSONObject.Create;
    ImageObj.AddPair('b64', aImage.Base64);
    ImageObj.AddPair('mime', aImage.MimeType);
    ImageObj.AddPair('filename', aImage.Filename);
    Req.AddPair('image', ImageObj);

    Client.CustomHeaders['Authorization'] := 'Bearer ' + ApiKey;
    Client.CustomHeaders['Content-Type'] := 'application/json';

    Res := Client.Post(FUrl + 'images/upscale',
      TStringStream.Create(Req.ToString, TEncoding.UTF8));

    if Res.StatusCode <> 200 then
      raise Exception.CreateFmt('Upscale error %d: %s',
        [Res.StatusCode, Res.ContentAsString]);

    FreeAndNil(Req);
    Req := TJSONObject.ParseJSONValue(Res.ContentAsString) as TJSONObject;

    RespImage := Req.GetValue<TJSONObject>('image');
    Result := TAiMediaFile.Create;
    Result.LoadFromBase64(RespImage.GetValue<string>('filename'),
      RespImage.GetValue<string>('b64'));
  finally
    Req.Free;
    Client.Free;
  end;
end;

function TAiDalle.Variation(aImageFile: TAiMediaFile; ASize: TAiImageSize; N: Integer): TAiDalleImage;
var
  Body: TMultipartFormData;
  Client: TNetHTTPClient;
  Res: IHTTPResponse;
  JObj: TJSONObject;
begin
  Result := nil;
  ClearImages;

  if not Assigned(aImageFile) then
    raise Exception.Create('An image file must be provided for variation.');
  if FModel <> imDallE2 then
    raise Exception.Create('Variation endpoint only supports dall-e-2 model.');

  Client := TNetHTTPClient.Create(nil);
  Body := TMultipartFormData.Create;
  try
    aImageFile.Content.Position := 0;
{$IF CompilerVersion < 35}
    Body.AddStream('image', aImageFile.Content, aImageFile.Filename);
{$ELSE}
    Body.AddStream('image', aImageFile.Content, False, aImageFile.Filename);
{$ENDIF}
    Body.AddField('user', FUser);
    Body.AddField('n', N.ToString);

    case ASize of
      is256x256: Body.AddField('size', '256x256');
      is512x512: Body.AddField('size', '512x512');
    else         Body.AddField('size', '1024x1024');
    end;

    if ResponseFormat = irfUrl then
      Body.AddField('response_format', 'url')
    else
      Body.AddField('response_format', 'b64_json');

    Client.CustomHeaders['Authorization'] := 'Bearer ' + ApiKey;
    Res := Client.Post(FUrl + 'images/variations', Body);

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
      raise Exception.CreateFmt('Error Received: %d, %s',
        [Res.StatusCode, Res.ContentAsString]);
  finally
    Client.Free;
    Body.Free;
  end;
end;

{ TAiDalleImageTool }

constructor TAiDalleImageTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDalle     := TAiDalle.Create(Self);
  FImageSize := is1024x1024;
end;

function TAiDalleImageTool.GetApiKey: string;
begin Result := FDalle.ApiKey; end;
procedure TAiDalleImageTool.SetApiKey(const Value: string);
begin FDalle.ApiKey := Value; end;

function TAiDalleImageTool.GetUrl: string;
begin Result := FDalle.Url; end;
procedure TAiDalleImageTool.SetUrl(const Value: string);
begin FDalle.Url := Value; end;

function TAiDalleImageTool.GetModel: TAiImageModel;
begin Result := FDalle.Model; end;
procedure TAiDalleImageTool.SetModel(const Value: TAiImageModel);
begin FDalle.Model := Value; end;

function TAiDalleImageTool.GetQuality: TAiImageQuality;
begin Result := FDalle.Quality; end;
procedure TAiDalleImageTool.SetQuality(const Value: TAiImageQuality);
begin FDalle.Quality := Value; end;

function TAiDalleImageTool.GetStyle: TAiImageStyle;
begin Result := FDalle.Style; end;
procedure TAiDalleImageTool.SetStyle(const Value: TAiImageStyle);
begin FDalle.Style := Value; end;

function TAiDalleImageTool.GetOutputFormat: TAiImageOutputFormat;
begin Result := FDalle.OutputFormat; end;
procedure TAiDalleImageTool.SetOutputFormat(const Value: TAiImageOutputFormat);
begin FDalle.OutputFormat := Value; end;

function TAiDalleImageTool.GetBackground: TAiImageBackground;
begin Result := FDalle.Background; end;
procedure TAiDalleImageTool.SetBackground(const Value: TAiImageBackground);
begin FDalle.Background := Value; end;

function TAiDalleImageTool.GetOutputCompression: Integer;
begin Result := FDalle.OutputCompression; end;
procedure TAiDalleImageTool.SetOutputCompression(const Value: Integer);
begin FDalle.OutputCompression := Value; end;

function TAiDalleImageTool.GetModeration: TAiImageModeration;
begin Result := FDalle.Moderation; end;
procedure TAiDalleImageTool.SetModeration(const Value: TAiImageModeration);
begin FDalle.Moderation := Value; end;

function TAiDalleImageTool.GetInputFidelity: TAiInputFidelity;
begin Result := FDalle.InputFidelity; end;
procedure TAiDalleImageTool.SetInputFidelity(const Value: TAiInputFidelity);
begin FDalle.InputFidelity := Value; end;

function TAiDalleImageTool.GetNegativePrompt: TStrings;
begin Result := FDalle.NegativePrompt; end;
procedure TAiDalleImageTool.SetNegativePrompt(const Value: TStrings);
begin FDalle.NegativePrompt := Value; end;

procedure TAiDalleImageTool.SetDalle(const Value: TAiDalle);
begin
  // El FDalle interno (creado con Self como owner) sigue en la lista de hijos
  // y ser? liberado por el framework. Apuntamos a la instancia inyectada.
  FDalle := Value;
end;

procedure TAiDalleImageTool.ExecuteImageGeneration(const APrompt: string;
  ResMsg, AskMsg: TAiChatMessage);
begin
  InternalRunDalleGeneration(APrompt, ResMsg);
end;

procedure TAiDalleImageTool.InternalRunDalleGeneration(const APrompt: string;
  ResMsg: TAiChatMessage);
var
  LDalleImage: TAiDalleImage;
  LMediaFile: TAiMediaFile;
  LExt: string;
begin
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
