// MIT License
//
// MakerAI - Agentes: TAiJevRouterTool, enrutamiento de grafos con Jev (TypeSafe AI)
//
// Nombre: Gustavo Enriquez
// - Email: gustavoeenriquez@gmail.com
// - Telegram: https://t.me/MakerAi_Suite_Delphi
// - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
// - Youtube: https://www.youtube.com/@cimamaker3945
// - GitHub: https://github.com/gustavoeenriquez/

unit uMakerAi.Agents.Tools.JevRouter;

// -----------------------------------------------------------------------------
// TAiJevRouterTool: tool de nodo que decide la siguiente ruta del grafo.
//
// Pregunta a Jev (ver uMakerAi.Jev) cual de las Routes corresponde al input
// del nodo y escribe la clave elegida en Blackboard[RouteKey] (por defecto
// 'next_route'), que es lo que lee un TAIAgentsLink en modo lmConditional.
// No cambia el motor: el link condicional existente hace el resto.
//
//   Router --(lmConditional)--> 'contable'   -> nodo del agente contable
//                            -> 'tributario' -> nodo del agente tributario
//                            -> NextNo       -> fallback (confianza baja o error)
//
// En GraphBuilder las claves de ruta son los idStr de los puertos de salida
// del nodo router: nombrar los puertos igual que las Routes.
//
// Routes: una linea por ruta, 'clave=descripcion'. La descripcion es lo que
//   Jev lee para decidir; escribirla como se le explicaria a una recepcionista.
// Flags: preguntas si/no opcionales, una por linea, 'nombre=pregunta'. Viajan
//   en la MISMA llamada que la ruta. Su probabilidad queda en el blackboard y
//   sirve para lmExpression (p.ej. 'Router.jev.fuentes >= 0.5') o para OnRoute.
//
// Claves que escribe en el blackboard (prefijo '<NombreDelNodo>.jev.'):
//   choice      ruta con mayor probabilidad (aunque no supere MinConfidence)
//   confidence  confianza de esa eleccion, '0.00'..'1.00' (punto decimal)
//   top         las 3 rutas mas probables separadas por comas
//   <flag>      probabilidad de cada flag, '0.00'..'1.00'
//   error       mensaje si la llamada fallo; vacio si fue bien
// y RouteKey con la ruta final.
//
// Ruta final: Choice si su confianza >= MinConfidence; si no, FallbackRoute.
// OnRoute puede cambiarla (p.ej. sufijo '_rag' si un flag lo pide). Un error
// de red o de configuracion NO rompe el grafo: se registra en '.jev.error' y
// se usa FallbackRoute. Con FallbackRoute vacio, el link condicional cae a
// NextNo (en GraphBuilder, el puerto out_failure).
//
// El output del nodo es el input sin cambios: el agente siguiente recibe la
// consulta original.
// -----------------------------------------------------------------------------

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  uMakerAi.Agents, uMakerAi.Agents.Attributes, uMakerAi.Agents.EngineRegistry,
  uMakerAi.Jev;

type
  // Permite ajustar la ruta final con toda la respuesta de Jev a la vista
  TAiJevRouteEvent = procedure(Sender: TObject; ANode: TAIAgentsNode;
    AResult: TAiJevResult; var ARoute: string) of object;

  [TToolAttribute('JevRouter',
                  'Elige la siguiente ruta del grafo con el modelo Jev (TypeSafe AI)',
                  'Control')]
  TAiJevRouterTool = class(TAiToolBase)
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
    FInstructions: string;
    FRoutes: string;
    FFlags: string;
    FInputField: string;
    FRouteKey: string;
    FFallbackRoute: string;
    FMinConfidence: Double;
    FOnRoute: TAiJevRouteEvent;
    procedure SetJev(const Value: TAiJev);
    function ActiveJev: TAiJev;
    function GetUsage: TAiJevUsage;
  protected
    procedure Execute(ANode: TAIAgentsNode; const AInput: string;
      var AOutput: string); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public const
    ROUTE_QUESTION = 'route'; // nombre reservado de la pregunta de ruta
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Consumo de Jev acumulado desde Create o ResetUsage (seguro entre hilos)
    property Usage: TAiJevUsage read GetUsage;
    procedure ResetUsage;

    // Atajos para armar Routes / Flags desde codigo
    procedure AddRoute(const AKey, ADescription: string);
    procedure AddFlag(const AName, AQuestion: string);
    // Preguntas que se envian a Jev (ruta + flags). El llamador libera el resultado.
    function BuildQuestions: TAiJevQuestions;

    // Opcional: un TAiJev externo (compartido, o un doble de pruebas). Si se
    // asigna, ApiKey y Model de esta tool se ignoran.
    property Jev: TAiJev read FJev write SetJev;
    property OnRoute: TAiJevRouteEvent read FOnRoute write FOnRoute;
  published
    // [TSecret]: nunca se serializa ni se acepta desde el JSON del grafo
    [TSecret('apiKey')]
    property ApiKey: string read FApiKey write FApiKey;

    [TToolParameterAttribute('Modelo', 'Version de Jev; fijarla antes de calibrar MinConfidence',
                             'jev-1.13.0')]
    property Model: string read FModel write FModel;
    // Servidor System One: '' = TypeSafe; 'http://localhost:11434/v1/' = Ollama
    // (con Model := 'nimble', 'clef-flash'...). Ignorada si Jev esta asignado
    property Url: string read FUrl write FUrl;

    [TToolParameterAttribute('Rutas', 'Una por linea: clave=descripcion. La clave es el valor que ' +
                             'recibe el link condicional (idStr del puerto en GraphBuilder)', '')]
    property Routes: string read FRoutes write FRoutes;

    [TToolParameterAttribute('Instrucciones', 'Pregunta de enrutamiento. Vacio = una pregunta ' +
                             'generica sobre el campo InputField', '')]
    property Instructions: string read FInstructions write FInstructions;

    [TToolParameterAttribute('Flags', 'Opcional. Una por linea: nombre=pregunta si/no. Su ' +
                             'probabilidad queda en <Nodo>.jev.<nombre>', '')]
    property Flags: string read FFlags write FFlags;

    [TToolParameterAttribute('Campo del input', 'Nombre con el que el input del nodo viaja en el ' +
                             'state de Jev; las preguntas lo citan entre backticks', 'consulta')]
    property InputField: string read FInputField write FInputField;

    [TToolParameterAttribute('Clave de ruta', 'Clave del blackboard que lee el link condicional ' +
                             '(su ConditionalKey)', 'next_route')]
    property RouteKey: string read FRouteKey write FRouteKey;

    [TToolParameterAttribute('Ruta de respaldo', 'Ruta si la confianza es baja o Jev falla. ' +
                             'Vacio = el link cae a NextNo (out_failure)', '')]
    property FallbackRoute: string read FFallbackRoute write FFallbackRoute;

    [TToolParameterAttribute('Confianza minima', 'Por debajo se usa FallbackRoute', '0.5')]
    property MinConfidence: Double read FMinConfidence write FMinConfidence;
    // Precios para CostUSD de Usage/OnUsage (US$ por millon de tokens; hoy la
    // salida no se cobra)
    property PricePerMillionInput: Double read FPricePerMillionInput write FPricePerMillionInput;
    property PricePerMillionOutput: Double read FPricePerMillionOutput write FPricePerMillionOutput;
    // Una vez por operacion con el consumo de esa operacion. Sincrono, en el hilo
    // que la ejecuto (en un servidor: el de la peticion, para cobrarle al cliente)
    property OnUsage: TAiJevUsageEvent read FOnUsage write FOnUsage;
  end;

implementation

function Invariant(AValue: Double): string;
begin
  Result := FormatFloat('0.00', AValue, TFormatSettings.Invariant);
end;

{ TAiJevRouterTool }

constructor TAiJevRouterTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUsage := TAiJevUsageMeter.Create;
  FPricePerMillionInput := JEV_PRICE_PER_MILLION_INPUT;
  FApiKey := '@TYPESAFE_API_KEY';
  FModel := 'jev-1.13.0';
  FInputField := 'consulta';
  FRouteKey := 'next_route';
  FMinConfidence := 0.5;
end;

destructor TAiJevRouterTool.Destroy;
begin
  FOwnJev.Free;
  FUsage.Free;
  inherited;
end;

procedure TAiJevRouterTool.SetJev(const Value: TAiJev);
begin
  if FJev = Value then
    Exit;
  if Assigned(FJev) then
    FJev.RemoveFreeNotification(Self);
  FJev := Value;
  if Assigned(FJev) then
    FJev.FreeNotification(Self);
end;

procedure TAiJevRouterTool.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FJev) then
    FJev := nil;
end;

function TAiJevRouterTool.ActiveJev: TAiJev;
begin
  if Assigned(FJev) then
    Exit(FJev);
  if not Assigned(FOwnJev) then
    FOwnJev := TAiJev.Create(nil);
  // Se refresca en cada uso: ApiKey/Model pueden cambiar entre ejecuciones
  FOwnJev.ApiKey := FApiKey;
  FOwnJev.Model := FModel;
  FOwnJev.Url := JevUrlOrDefault(FUrl);
  Result := FOwnJev;
end;

procedure TAiJevRouterTool.AddRoute(const AKey, ADescription: string);
begin
  if FRoutes <> '' then
    FRoutes := FRoutes + sLineBreak;
  if ADescription <> '' then
    FRoutes := FRoutes + AKey + '=' + ADescription
  else
    FRoutes := FRoutes + AKey;
end;

procedure TAiJevRouterTool.AddFlag(const AName, AQuestion: string);
begin
  if FFlags <> '' then
    FFlags := FFlags + sLineBreak;
  FFlags := FFlags + AName + '=' + AQuestion;
end;

function TAiJevRouterTool.BuildQuestions: TAiJevQuestions;
var
  Lines: TStringList;
  Q: TAiJevQuestion;
  i, P: Integer;
  Line, Instr: string;
begin
  Result := TAiJevQuestions.Create(nil);
  try
    Instr := FInstructions;
    if Trim(Instr) = '' then
      // Redaccion probada contra jev-1.13.0: la variante "Que ruta o especialista
      // debe atender" bajaba la confianza de 0.68 a 0.44 en la misma consulta
      Instr := 'Que especialista debe responder `' + FInputField + '`?';

    Q := Result.Add;
    Q.Name := ROUTE_QUESTION;
    Q.Kind := jqChoice;
    Q.Instructions := Instr;
    Q.Criteria.Text := FRoutes;

    Lines := TStringList.Create;
    try
      Lines.Text := FFlags;
      for i := 0 to Lines.Count - 1 do
      begin
        Line := Trim(Lines[i]);
        if Line = '' then
          Continue;
        P := Pos('=', Line);
        if P = 0 then
          raise EAiJevError.CreateFmt('JevRouter: el flag "%s" no tiene la forma nombre=pregunta', [Line]);
        if SameText(Trim(Copy(Line, 1, P - 1)), ROUTE_QUESTION) then
          raise EAiJevError.CreateFmt('JevRouter: "%s" es un nombre reservado para flags', [ROUTE_QUESTION]);
        Result.AddNoul(Trim(Copy(Line, 1, P - 1)), Trim(Copy(Line, P + 1, MaxInt)));
      end;
    finally
      Lines.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure TAiJevRouterTool.Execute(ANode: TAIAgentsNode; const AInput: string;
  var AOutput: string);
var
  Q: TAiJevQuestions;
  State: TJSONObject;
  R: TAiJevResult;
  Ans: TAiJevAnswer;
  BB: TAIBlackboard;
  Route, Prefix: string;
  i: Integer;
begin
  // Pass-through: el agente siguiente recibe la consulta original
  AOutput := AInput;
  if not Assigned(ANode) or not Assigned(ANode.Graph) then
    Exit;

  BB := ANode.Graph.Blackboard;
  Prefix := ANode.Name + '.jev.';
  Route := FFallbackRoute;
  try
    Q := BuildQuestions;
    try
      State := TJSONObject.Create;
      try
        State.AddPair(FInputField, AInput);
        R := ActiveJev.Ask(State, Q);
      finally
        State.Free;
      end;
      try
        JevReportResult(Self, R, FUsage, JevAdapterInputPrice(FJev, FUrl, FPricePerMillionInput), FPricePerMillionOutput, FOnUsage);
        Ans := R[ROUTE_QUESTION];
        BB.SetString(Prefix + 'choice', Ans.Choice);
        BB.SetString(Prefix + 'confidence', Invariant(Ans.Confidence));
        BB.SetString(Prefix + 'top', string.Join(',', Ans.Top(3)));
        for i := 0 to Q.Count - 1 do
          if Q[i].Name <> ROUTE_QUESTION then
            BB.SetString(Prefix + Q[i].Name, Invariant(R[Q[i].Name].Noul));

        if Ans.Confidence >= FMinConfidence then
          Route := Ans.Choice;
        if Assigned(FOnRoute) then
          FOnRoute(Self, ANode, R, Route);
      finally
        R.Free;
      end;
    finally
      Q.Free;
    end;
    BB.SetString(Prefix + 'error', '');
  except
    on E: Exception do
    begin
      // Un router caido no debe tumbar el grafo: se toma la ruta de respaldo
      BB.SetString(Prefix + 'error', E.Message);
      ANode.Print('JevRouter: ' + E.Message);
      Route := FFallbackRoute;
    end;
  end;
  BB.SetString(FRouteKey, Route);
end;

function TAiJevRouterTool.GetUsage: TAiJevUsage;
begin
  Result := FUsage.Snapshot(JevAdapterInputPrice(FJev, FUrl, FPricePerMillionInput), FPricePerMillionOutput);
end;

procedure TAiJevRouterTool.ResetUsage;
begin
  FUsage.Reset;
end;

initialization
  TEngineRegistry.Instance.RegisterTool(
    TAiJevRouterTool, 'uMakerAi.Agents.Tools.JevRouter');

end.
