// MIT License
//
// MakerAI - Jev: etiquetado masivo con el modelo Jev (TypeSafe AI)
//
// Nombre: Gustavo Enriquez
// - Email: gustavoeenriquez@gmail.com
// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.Jev.Batch;

// -----------------------------------------------------------------------------
// TAiJevBatchLabeler: aplica las mismas preguntas (Questions) a muchas filas,
// una llamada a Jev por fila, en paralelo, y devuelve un reporte con la
// etiqueta, la confianza y las alternativas de cada fila, el costo total y la
// lista de filas dudosas para revision humana.
//
//   Labeler.Questions.AddChoice('categoria', 'Que categoria tiene `item`?',
//     ['spam=Publicidad no solicitada', 'urgente=Requiere accion hoy', 'archivo']);
//   Report := Labeler.Run(Textos);            // el llamador libera el reporte
//   for Item in Report.NeedsReview do ...      // lo que conviene revisar a mano
//
// Entrada: textos (cada uno viaja como {ItemField: texto}) o estados JSON ya
// armados, para filas con varios campos ({movimiento, tipo, ...}).
//
// La etiqueta es la respuesta de LabelQuestion (vacio = la primera pregunta
// Choice). Las demas preguntas se responden en la misma llamada y quedan en
// Item.Result. Una fila con confianza < ReviewThreshold queda marcada para
// revision; una fila con error guarda el mensaje y el lote sigue.
//
// Concurrencia: MaxParallel llamadas simultaneas, cada una con su propio
// TAiJev. Con un Jev externo asignado va en serie (el componente compartido
// no es seguro entre hilos). OnProgress se dispara desde los hilos del pool:
// para tocar la UI, usar TThread.Queue en el handler. Cancel detiene el lote;
// las filas no procesadas quedan con Error = 'cancelled'.
//
// Validado sep 28/2026 reproduciendo en Delphi el experimento de clasificar
// 126 movimientos contra 225 cuentas del PUC (demo 087).
// -----------------------------------------------------------------------------

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections,
  System.Threading, System.SyncObjs, System.Diagnostics,
  uMakerAi.Jev;

type
  TAiJevBatchItem = class
  private
    FIndex: Integer;
    FInput: string;
    FChoice: string;
    FConfidence: Double;
    FTop: TArray<string>;
    FNeedsReview: Boolean;
    FError: string;
    FResult: TAiJevResult;
  public
    destructor Destroy; override;
    // Posicion en la entrada (0..N-1)
    property Index: Integer read FIndex;
    // Texto de la fila, o el state JSON serializado
    property Input: string read FInput;
    // Etiqueta: la eleccion de LabelQuestion
    property Choice: string read FChoice;
    property Confidence: Double read FConfidence;
    // Las 3 etiquetas mas probables (sugerencias para quien revise)
    property Top: TArray<string> read FTop;
    property NeedsReview: Boolean read FNeedsReview;
    // '' si la fila se proceso bien
    property Error: string read FError;
    // Respuesta completa (todas las preguntas); nil si hubo error
    property Result: TAiJevResult read FResult;
  end;

  TAiJevBatchReport = class
  private
    FItems: TObjectList<TAiJevBatchItem>;
    FMeter: TAiJevUsageMeter; // consumo de este lote (para OnUsage)
    FInputTokens: Int64;
    FCostUSD: Double;
    FElapsedMs: Int64;
    FCancelled: Boolean;
    function GetErrorCount: Integer;
    function GetReviewCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    // Filas con confianza baja (sin error), en orden de entrada
    function NeedsReview: TArray<TAiJevBatchItem>;
    // index;label;confidence;review;top;error;input (UTF-8, separador ';')
    procedure SaveToCSV(const AFileName: string);
    property Items: TObjectList<TAiJevBatchItem> read FItems;
    property InputTokens: Int64 read FInputTokens;
    property CostUSD: Double read FCostUSD;
    property ElapsedMs: Int64 read FElapsedMs;
    property Cancelled: Boolean read FCancelled;
    property ErrorCount: Integer read GetErrorCount;
    property ReviewCount: Integer read GetReviewCount;
  end;

  // Se dispara desde los hilos del pool; usar TThread.Queue para la UI
  TAiJevBatchProgressEvent = procedure(Sender: TObject; ADone, ATotal: Integer) of object;

  TAiJevBatchLabeler = class(TComponent)
  private
    FUsage: TAiJevUsageMeter;
    FPricePerMillionOutput: Double;
    FOnUsage: TAiJevUsageEvent;
    FJev: TAiJev;
    FApiKey: string;
    FModel: string;
    FUrl: string; // '' = TypeSafe (JEV_DEFAULT_URL)
    FQuestions: TAiJevQuestions;
    FItemField: string;
    FLabelQuestion: string;
    FReviewThreshold: Double;
    FMaxParallel: Integer;
    FPricePerMillion: Double;
    FCancelled: Boolean;
    FDone: Integer;
    FOnProgress: TAiJevBatchProgressEvent;
    procedure SetJev(const Value: TAiJev);
    procedure SetQuestions(const Value: TAiJevQuestions);
    function NewJev: TAiJev;
    function ResolveLabelQuestion: string;
    procedure ProcessItem(AJev: TAiJev; AItem: TAiJevBatchItem; AState: TJSONValue;
      const ALabelQ: string; AReport: TAiJevBatchReport; ATotal: Integer);
    function InternalRun(const AStates: TArray<TJSONValue>; const AInputs: TArray<string>): TAiJevBatchReport;
    function GetUsage: TAiJevUsage;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Consumo de Jev acumulado desde Create o ResetUsage (seguro entre hilos)
    property Usage: TAiJevUsage read GetUsage;
    procedure ResetUsage;
    // Cada texto viaja como {ItemField: texto}. El llamador libera el reporte.
    function Run(const ATexts: TArray<string>): TAiJevBatchReport; overload;
    // Estados JSON ya armados (no toma posesion). El llamador libera el reporte.
    function Run(const AStates: TArray<TJSONObject>): TAiJevBatchReport; overload;
    // Detiene el lote en curso (seguro desde otro hilo)
    procedure Cancel;
  published
    // Opcional: TAiJev externo (compartido o doble de pruebas); si se asigna,
    // ApiKey y Model se ignoran y las filas se procesan en serie
    property Jev: TAiJev read FJev write SetJev;
    property ApiKey: string read FApiKey write FApiKey;
    property Model: string read FModel write FModel;
    // Servidor System One: '' = TypeSafe; 'http://localhost:11434/v1/' = Ollama
    // (con Model := 'nimble', 'clef-flash'...). Ignorada si Jev esta asignado
    property Url: string read FUrl write FUrl;
    // Preguntas que se aplican a cada fila
    property Questions: TAiJevQuestions read FQuestions write SetQuestions;
    // Nombre del campo con el que viaja cada texto (Run con textos)
    property ItemField: string read FItemField write FItemField;
    // Pregunta que da la etiqueta; vacio = la primera Choice
    property LabelQuestion: string read FLabelQuestion write FLabelQuestion;
    property ReviewThreshold: Double read FReviewThreshold write FReviewThreshold;
    property MaxParallel: Integer read FMaxParallel write FMaxParallel default 8;
    // US$ por millon de tokens de entrada (para CostUSD)
    property PricePerMillion: Double read FPricePerMillion write FPricePerMillion;
    property OnProgress: TAiJevBatchProgressEvent read FOnProgress write FOnProgress;
    // Precios para CostUSD de Usage/OnUsage (US$ por millon de tokens; hoy la
    // salida no se cobra)
    property PricePerMillionOutput: Double read FPricePerMillionOutput write FPricePerMillionOutput;
    // Una vez por operacion con el consumo de esa operacion. Sincrono, en el hilo
    // que la ejecuto (en un servidor: el de la peticion, para cobrarle al cliente)
    property OnUsage: TAiJevUsageEvent read FOnUsage write FOnUsage;
  end;

procedure Register;

implementation

uses
  System.StrUtils;

procedure Register;
begin
  RegisterComponents('MakerAI', [TAiJevBatchLabeler]);
end;

{ TAiJevBatchItem }

destructor TAiJevBatchItem.Destroy;
begin
  FResult.Free;
  inherited;
end;

{ TAiJevBatchReport }

constructor TAiJevBatchReport.Create;
begin
  inherited Create;
  FItems := TObjectList<TAiJevBatchItem>.Create(True);
  FMeter := TAiJevUsageMeter.Create;
end;

destructor TAiJevBatchReport.Destroy;
begin
  FItems.Free;
  FMeter.Free;
  inherited;
end;

function TAiJevBatchReport.GetErrorCount: Integer;
var
  It: TAiJevBatchItem;
begin
  Result := 0;
  for It in FItems do
    if It.FError <> '' then
      Inc(Result);
end;

function TAiJevBatchReport.GetReviewCount: Integer;
var
  It: TAiJevBatchItem;
begin
  Result := 0;
  for It in FItems do
    if It.FNeedsReview then
      Inc(Result);
end;

function TAiJevBatchReport.NeedsReview: TArray<TAiJevBatchItem>;
var
  L: TList<TAiJevBatchItem>;
  It: TAiJevBatchItem;
begin
  L := TList<TAiJevBatchItem>.Create;
  try
    for It in FItems do
      if It.FNeedsReview then
        L.Add(It);
    Result := L.ToArray;
  finally
    L.Free;
  end;
end;

procedure TAiJevBatchReport.SaveToCSV(const AFileName: string);
var
  SL: TStringList;
  It: TAiJevBatchItem;

  function Q(const S: string): string;
  begin
    Result := '"' + StringReplace(S, '"', '""', [rfReplaceAll]) + '"';
  end;

begin
  SL := TStringList.Create;
  try
    SL.Add('index;label;confidence;review;top;error;input');
    for It in FItems do
      SL.Add(Format('%d;%s;%s;%s;%s;%s;%s', [It.FIndex, Q(It.FChoice),
        FormatFloat('0.00', It.FConfidence, TFormatSettings.Invariant),
        IfThen(It.FNeedsReview, 'si', 'no'), Q(string.Join(',', It.FTop)), Q(It.FError),
        Q(StringReplace(It.FInput, sLineBreak, ' ', [rfReplaceAll]))]));
    SL.SaveToFile(AFileName, TEncoding.UTF8);
  finally
    SL.Free;
  end;
end;

{ TAiJevBatchLabeler }

constructor TAiJevBatchLabeler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUsage := TAiJevUsageMeter.Create;
  FApiKey := '@TYPESAFE_API_KEY';
  FModel := 'jev-1.13.0';
  FQuestions := TAiJevQuestions.Create(Self);
  FItemField := 'item';
  FReviewThreshold := 0.6;
  FMaxParallel := 8;
  FPricePerMillion := 0.042;
end;

destructor TAiJevBatchLabeler.Destroy;
begin
  FQuestions.Free;
  FUsage.Free;
  inherited;
end;

procedure TAiJevBatchLabeler.SetJev(const Value: TAiJev);
begin
  if FJev = Value then
    Exit;
  if Assigned(FJev) then
    FJev.RemoveFreeNotification(Self);
  FJev := Value;
  if Assigned(FJev) then
    FJev.FreeNotification(Self);
end;

procedure TAiJevBatchLabeler.SetQuestions(const Value: TAiJevQuestions);
begin
  FQuestions.Assign(Value);
end;

procedure TAiJevBatchLabeler.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FJev) then
    FJev := nil;
end;

function TAiJevBatchLabeler.NewJev: TAiJev;
begin
  Result := TAiJev.Create(nil);
  Result.ApiKey := FApiKey;
  Result.Model := FModel;
  Result.Url := JevUrlOrDefault(FUrl);
end;

procedure TAiJevBatchLabeler.Cancel;
begin
  FCancelled := True;
end;

function TAiJevBatchLabeler.ResolveLabelQuestion: string;
var
  i: Integer;
begin
  if FLabelQuestion <> '' then
  begin
    if FQuestions.Find(FLabelQuestion) = nil then
      raise EAiJevError.CreateFmt('JevBatch: LabelQuestion "%s" no esta en Questions', [FLabelQuestion]);
    Exit(FLabelQuestion);
  end;
  for i := 0 to FQuestions.Count - 1 do
    if FQuestions[i].Kind = jqChoice then
      Exit(FQuestions[i].Name);
  Result := ''; // sin Choice: no hay etiqueta, solo Item.Result
end;

procedure TAiJevBatchLabeler.ProcessItem(AJev: TAiJev; AItem: TAiJevBatchItem; AState: TJSONValue;
  const ALabelQ: string; AReport: TAiJevBatchReport; ATotal: Integer);
var
  R: TAiJevResult;
  Done: Integer;
begin
  if FCancelled then
    AItem.FError := 'cancelled'
  else
    try
      R := AJev.Ask(AState, FQuestions);
      AItem.FResult := R;
      TInterlocked.Add(AReport.FInputTokens, Int64(R.InputTokens));
      AReport.FMeter.Add(R);
      if ALabelQ <> '' then
      begin
        AItem.FChoice := R[ALabelQ].Choice;
        AItem.FConfidence := R[ALabelQ].Confidence;
        AItem.FTop := R[ALabelQ].Top(3);
        AItem.FNeedsReview := AItem.FConfidence < FReviewThreshold;
      end;
    except
      on E: Exception do
        AItem.FError := E.Message; // la fila falla, el lote sigue
    end;

  Done := TInterlocked.Increment(FDone);
  if Assigned(FOnProgress) then
    FOnProgress(Self, Done, ATotal);
end;

function TAiJevBatchLabeler.InternalRun(const AStates: TArray<TJSONValue>;
  const AInputs: TArray<string>): TAiJevBatchReport;
var
  Report: TAiJevBatchReport;
  LabelQ: string;
  Pool: TThreadPool;
  SW: TStopwatch;
  i, Total: Integer;
  It: TAiJevBatchItem;
begin
  FQuestions.Validate; // errores de configuracion antes de gastar una llamada
  LabelQ := ResolveLabelQuestion;
  FCancelled := False;
  FDone := 0;
  Total := Length(AStates);

  Report := TAiJevBatchReport.Create;
  try
    for i := 0 to Total - 1 do
    begin
      It := TAiJevBatchItem.Create;
      It.FIndex := i;
      It.FInput := AInputs[i];
      Report.FItems.Add(It);
    end;

    SW := TStopwatch.StartNew;
    if Assigned(FJev) or (FMaxParallel <= 1) or (Total <= 1) then
    begin
      for i := 0 to Total - 1 do
        if Assigned(FJev) then
          ProcessItem(FJev, Report.FItems[i], AStates[i], LabelQ, Report, Total)
        else
        begin
          var J := NewJev;
          try
            ProcessItem(J, Report.FItems[i], AStates[i], LabelQ, Report, Total);
          finally
            J.Free;
          end;
        end;
    end
    else
    begin
      Pool := TThreadPool.Create;
      try
        Pool.SetMinWorkerThreads(1);
        Pool.SetMaxWorkerThreads(FMaxParallel);
        TParallel.For(0, Total - 1,
          procedure(AIndex: Integer)
          var
            J: TAiJev;
          begin
            J := NewJev; // un TAiJev por fila: el componente no es seguro entre hilos
            try
              ProcessItem(J, Report.FItems[AIndex], AStates[AIndex], LabelQ, Report, Total);
            finally
              J.Free;
            end;
          end, Pool);
      finally
        Pool.Free;
      end;
    end;
    SW.Stop;

    Report.FElapsedMs := SW.ElapsedMilliseconds;
    Report.FCancelled := FCancelled;
    Report.FCostUSD := Report.FInputTokens * JevAdapterInputPrice(FJev, FUrl, FPricePerMillion) / 1E6;
    // Un OnUsage por lote. PricePerMillion (historico) es el precio de entrada
    JevReportOperation(Self, Report.FMeter, FUsage, JevAdapterInputPrice(FJev, FUrl, FPricePerMillion), FPricePerMillionOutput, FOnUsage);
  except
    Report.Free;
    raise;
  end;
  Result := Report;
end;

function TAiJevBatchLabeler.Run(const ATexts: TArray<string>): TAiJevBatchReport;
var
  States: TArray<TJSONValue>;
  i: Integer;
begin
  SetLength(States, Length(ATexts));
  try
    for i := 0 to High(ATexts) do
      States[i] := TJSONObject.Create.AddPair(FItemField, ATexts[i]);
    Result := InternalRun(States, ATexts);
  finally
    for i := 0 to High(States) do
      States[i].Free;
  end;
end;

function TAiJevBatchLabeler.Run(const AStates: TArray<TJSONObject>): TAiJevBatchReport;
var
  States: TArray<TJSONValue>;
  Inputs: TArray<string>;
  i: Integer;
begin
  SetLength(States, Length(AStates));
  SetLength(Inputs, Length(AStates));
  for i := 0 to High(AStates) do
  begin
    States[i] := AStates[i];
    Inputs[i] := AStates[i].ToJSON;
  end;
  Result := InternalRun(States, Inputs);
end;

function TAiJevBatchLabeler.GetUsage: TAiJevUsage;
begin
  Result := FUsage.Snapshot(JevAdapterInputPrice(FJev, FUrl, FPricePerMillion), FPricePerMillionOutput);
end;

procedure TAiJevBatchLabeler.ResetUsage;
begin
  FUsage.Reset;
end;

end.
