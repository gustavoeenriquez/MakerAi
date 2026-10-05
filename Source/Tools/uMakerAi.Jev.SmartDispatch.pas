// MIT License
//
// MakerAI - Jev: clasificador de SmartDispatch con el modelo Jev (TypeSafe AI)
//
// Nombre: Gustavo Enriquez
// - Email: gustavoeenriquez@gmail.com
// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.Jev.SmartDispatch;

// -----------------------------------------------------------------------------
// TAiJevDispatchClassifier: se asigna a ChatTools.DispatchClassifier de un chat
// en ChatMode = cmSmartDispatch y reemplaza el pase 1 (un LLM que clasifica la
// peticion en IMAGEGEN / VIDEOGEN / TTS / WEBSEARCH / CHAT) por una pregunta
// Choice a Jev.
//
//   Chat.ChatMode := cmSmartDispatch;
//   Chat.ChatTools.ImageTool := MiHerramientaDeImagen;
//   Chat.ChatTools.DispatchClassifier := JevDispatch;   // este componente
//
// Solo se ofrecen a Jev los tags cuya tool esta asignada (los decide el chat).
// Si solo queda CHAT no se llama a Jev: la respuesta es CHAT directamente.
// Con confianza < MinConfidence devuelve '' y el chat hace su pase 1 por LLM,
// igual que sin clasificador; un error tambien cae ahi (el chat lo absorbe).
//
// Calibrado contra jev-1.13.0 (sep 27/2026): 15/15 sobre peticiones en
// espanol e ingles, incluida "describe como se veria un gato" -> CHAT (0.72).
// -----------------------------------------------------------------------------

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  uMakerAi.Chat.Tools, uMakerAi.Jev;

type
  TAiJevDispatchClassifier = class(TAiDispatchClassifierBase)
  private
    FUsage: TAiJevUsageMeter;
    FPricePerMillionInput: Double;
    FPricePerMillionOutput: Double;
    FOnUsage: TAiJevUsageEvent;
    FJev: TAiJev;
    FOwnJev: TAiJev;
    FApiKey: string;
    FModel: string;
    FUrl: string; // '' = TypeSafe (JEV_DEFAULT_URL)
    FMinConfidence: Double;
    FTagDescriptions: TStrings;
    FLastTag: string;
    FLastConfidence: Double;
    procedure SetJev(const Value: TAiJev);
    procedure SetTagDescriptions(const Value: TStrings);
    function ActiveJev: TAiJev;
    function DescriptionOf(const ATag: string): string;
    function GetUsage: TAiJevUsage;
  protected
    function ClassifyDispatch(const APrompt: string; const ATags: TArray<string>): string; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Consumo de Jev acumulado desde Create o ResetUsage (seguro entre hilos)
    property Usage: TAiJevUsage read GetUsage;
    procedure ResetUsage;
    // Ultima eleccion de Jev aunque no superara MinConfidence ('' si no se consulto)
    property LastTag: string read FLastTag;
    property LastConfidence: Double read FLastConfidence;
  published
    // Opcional: TAiJev externo (compartido o doble de pruebas); si se asigna,
    // ApiKey y Model de este componente se ignoran
    property Jev: TAiJev read FJev write SetJev;
    property ApiKey: string read FApiKey write FApiKey;
    property Model: string read FModel write FModel;
    // Servidor System One: '' = TypeSafe; 'http://localhost:11434/v1/' = Ollama
    // (con Model := 'nimble', 'clef-flash'...). Ignorada si Jev esta asignado
    property Url: string read FUrl write FUrl;
    // Por debajo, el chat clasifica con el LLM como siempre
    property MinConfidence: Double read FMinConfidence write FMinConfidence;
    // Opcional: 'TAG=descripcion' para reemplazar la descripcion por defecto de un tag
    property TagDescriptions: TStrings read FTagDescriptions write SetTagDescriptions;
    // Precios para CostUSD de Usage/OnUsage (US$ por millon de tokens; hoy la
    // salida no se cobra)
    property PricePerMillionInput: Double read FPricePerMillionInput write FPricePerMillionInput;
    property PricePerMillionOutput: Double read FPricePerMillionOutput write FPricePerMillionOutput;
    // Una vez por operacion con el consumo de esa operacion. Sincrono, en el hilo
    // que la ejecuto (en un servidor: el de la peticion, para cobrarle al cliente)
    property OnUsage: TAiJevUsageEvent read FOnUsage write FOnUsage;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiJevDispatchClassifier]);
end;

{ TAiJevDispatchClassifier }

constructor TAiJevDispatchClassifier.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUsage := TAiJevUsageMeter.Create;
  FPricePerMillionInput := JEV_PRICE_PER_MILLION_INPUT;
  FApiKey := '@TYPESAFE_API_KEY';
  FModel := 'jev-1.13.0';
  FMinConfidence := 0.6;
  FTagDescriptions := TStringList.Create;
end;

destructor TAiJevDispatchClassifier.Destroy;
begin
  FTagDescriptions.Free;
  FOwnJev.Free;
  FUsage.Free;
  inherited;
end;

procedure TAiJevDispatchClassifier.SetJev(const Value: TAiJev);
begin
  if FJev = Value then
    Exit;
  if Assigned(FJev) then
    FJev.RemoveFreeNotification(Self);
  FJev := Value;
  if Assigned(FJev) then
    FJev.FreeNotification(Self);
end;

procedure TAiJevDispatchClassifier.SetTagDescriptions(const Value: TStrings);
begin
  FTagDescriptions.Assign(Value);
end;

procedure TAiJevDispatchClassifier.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FJev) then
    FJev := nil;
end;

function TAiJevDispatchClassifier.ActiveJev: TAiJev;
begin
  if Assigned(FJev) then
    Exit(FJev);
  if not Assigned(FOwnJev) then
    FOwnJev := TAiJev.Create(nil);
  FOwnJev.ApiKey := FApiKey;
  FOwnJev.Model := FModel;
  FOwnJev.Url := JevUrlOrDefault(FUrl);
  Result := FOwnJev;
end;

function TAiJevDispatchClassifier.DescriptionOf(const ATag: string): string;
begin
  Result := Trim(FTagDescriptions.Values[ATag]);
  if Result <> '' then
    Exit;
  // Redaccion calibrada contra jev-1.13.0 (15/15, espanol e ingles)
  if ATag = 'IMAGEGEN' then
    Result := 'The user asks to create, draw or generate an image, picture, logo or illustration'
  else if ATag = 'VIDEOGEN' then
    Result := 'The user asks to create or generate a video or animation'
  else if ATag = 'TTS' then
    Result := 'The user asks to read a text aloud or to produce spoken audio / a voice recording'
  else if ATag = 'WEBSEARCH' then
    Result := 'The user needs recent news, current events, live prices or information that must be looked up on the web'
  else if ATag = 'CHAT' then
    Result := 'Anything else: conversation, questions, explanations, writing, code, calculations'
  else
    Result := '';
end;

function TAiJevDispatchClassifier.ClassifyDispatch(const APrompt: string;
  const ATags: TArray<string>): string;
var
  Q: TAiJevQuestions;
  Options: TArray<string>;
  State: TJSONObject;
  R: TAiJevResult;
  i: Integer;
begin
  Result := '';
  FLastTag := '';
  FLastConfidence := 0;

  // Un solo tag posible: no hay nada que clasificar
  if Length(ATags) = 1 then
    Exit(ATags[0]);
  if Length(ATags) = 0 then
    Exit;

  SetLength(Options, Length(ATags));
  for i := 0 to High(ATags) do
    if DescriptionOf(ATags[i]) <> '' then
      Options[i] := ATags[i] + '=' + DescriptionOf(ATags[i])
    else
      Options[i] := ATags[i];

  Q := TAiJevQuestions.Create(nil);
  try
    Q.AddChoice('tag', 'Which handler should process `request`?', Options);
    State := TJSONObject.Create;
    try
      State.AddPair('request', APrompt);
      R := ActiveJev.Ask(State, Q);
    finally
      State.Free;
    end;
    try
      JevReportResult(Self, R, FUsage, JevAdapterInputPrice(FJev, FUrl, FPricePerMillionInput), FPricePerMillionOutput, FOnUsage);
      FLastTag := R['tag'].Choice;
      FLastConfidence := R['tag'].Confidence;
      if FLastConfidence >= FMinConfidence then
        Result := FLastTag;
    finally
      R.Free;
    end;
  finally
    Q.Free;
  end;
end;

function TAiJevDispatchClassifier.GetUsage: TAiJevUsage;
begin
  Result := FUsage.Snapshot(JevAdapterInputPrice(FJev, FUrl, FPricePerMillionInput), FPricePerMillionOutput);
end;

procedure TAiJevDispatchClassifier.ResetUsage;
begin
  FUsage.Reset;
end;

end.
