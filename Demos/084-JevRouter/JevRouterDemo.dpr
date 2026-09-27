program JevRouterDemo;

// =============================================================================
// DEMO 084 - Enrutar consultas a agentes especializados con Jev (TypeSafe AI)
// =============================================================================
// Jev no genera texto: responde preguntas tipadas con probabilidades
// calibradas. Aqui decide, ANTES de gastar un LLM, que agente especializado
// debe atender cada consulta y si ese agente necesita su version con RAG
// (lenta, cita normas) o basta la rapida.
//
// Una sola llamada a Jev por consulta, con tres clases de pregunta:
//   dominio          Choice : que especialista (relativo: cual de todos)
//   toca_<dominio>   Noul   : uno por dominio (absoluto: detecta consultas
//                             que cruzan dos areas, p.ej. contable+tributario)
//   necesita_fuentes Noul   : si hay que citar una norma, tarifa o plazo
//
// La decision final la toma el CODIGO (EnrutarConsulta): umbrales, catalogo de
// agentes y reglas quedan a la vista y se cambian sin tocar las preguntas.
//
// Bloque B muestra los atajos Choose y Noul para una sola pregunta.
//
// Bloque C hace lo mismo DENTRO de un grafo de agentes: un nodo con
// TAiJevRouterTool escribe la ruta en el blackboard y un link lmConditional
// la sigue. OnRoute combina la ruta con el flag 'fuentes' para elegir entre
// el agente contable rapido y el normativo; la confianza baja cae a NextNo.
//
// Requiere la variable de entorno TYPESAFE_API_KEY (https://console.typesafe.ai/keys).
// Costo: ~700 tokens de entrada por consulta a US$0.042 por millon.
//
// Modos de uso:
//   JevRouterDemo.exe                         -> consultas de ejemplo
//   JevRouterDemo.exe "consulta 1" "consulta 2" -> tus propias consultas
// =============================================================================

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Generics.Collections,
  System.StrUtils,
  uMakerAi.Jev in '..\..\Source\Tools\uMakerAi.Jev.pas',
  uMakerAi.Agents,
  uMakerAi.Agents.Tools.JevRouter in '..\..\Source\Agents\uMakerAi.Agents.Tools.JevRouter.pas';

type
  TAgente = record
    Nombre: string;
    Dominio: string;
    ConRag: Boolean;
  end;

  // Los eventos del framework son 'of object': los handlers viven en una clase
  TGrafoHandlers = class
  public
    // Nodo de agente: en una app real aqui iria el LLM (con o sin RAG)
    procedure AgenteExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink;
      Input: string; var Output: string);
    procedure FinExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink;
      Input: string; var Output: string);
    // contable + flag 'fuentes' alto -> contable_normativo; si no, contable_rapido
    procedure AjustarRuta(Sender: TObject; ANode: TAIAgentsNode;
      AResult: TAiJevResult; var ARoute: string);
  end;

const
  // Lo que Jev lee para elegir: descripciones como se las darias a una recepcionista
  DOMINIOS: array[0..4] of string = (
    'contable=Registro contable: asientos, cuentas del PUC, comprobantes, conciliaciones, estados financieros, NIIF',
    'tributario=Impuestos en Colombia: renta, IVA, retencion en la fuente, ICA, declaraciones, DIAN, facturacion electronica',
    'legal=Derecho comercial y civil: contratos, sociedades, cobro juridico, demandas, responsabilidad',
    'laboral=Relacion con empleados: nomina, prestaciones sociales, seguridad social, contratos laborales, despidos',
    'general=Cualquier otra cosa: saludos, uso del sistema, preguntas que no son de un area especializada');

  // Catalogo de agentes: un dominio puede tener version rapida (sin RAG) y con RAG
  AGENTES: array[0..5] of TAgente = (
    (Nombre: 'contable_rapido';    Dominio: 'contable';   ConRag: False),
    (Nombre: 'contable_normativo'; Dominio: 'contable';   ConRag: True),
    (Nombre: 'tributario';         Dominio: 'tributario'; ConRag: True),
    (Nombre: 'legal';              Dominio: 'legal';      ConRag: True),
    (Nombre: 'laboral';            Dominio: 'laboral';    ConRag: False),
    (Nombre: 'general';            Dominio: 'general';    ConRag: False));

  UMBRAL_DOMINIO = 0.5;  // por debajo: no esta claro de que trata -> general
  UMBRAL_RAG     = 0.5;
  UMBRAL_CRUCE   = 0.7;  // otro dominio con Noul alto -> consulta compuesta

  CONSULTAS_EJEMPLO: array[0..7] of string = (
    'Buenos dias, como cambio mi contrasena del sistema?',
    'Como registro el pago de la nomina de septiembre?',
    'Que dice la NIC 16 sobre la vida util de los activos?',
    'Cual es la tarifa de retencion en la fuente por honorarios para personas naturales?',
    'Un cliente no me paga una factura de hace 8 meses, que puedo hacer?',
    'Vendi un activo fijo con utilidad, como lo contabilizo y cuanto impuesto pago por la ganancia ocasional?',
    'Voy a despedir a un trabajador sin justa causa, cuanto le debo pagar?',
    'me salio un error raro');

var
  TotalTokens: Int64 = 0;

function Clave(const AOpcion: string): string;
begin
  Result := AOpcion.Substring(0, AOpcion.IndexOf('='));
end;

function Descripcion(const AOpcion: string): string;
begin
  Result := AOpcion.Substring(AOpcion.IndexOf('=') + 1);
end;

// Las preguntas se arman una vez y se reutilizan para todas las consultas
procedure ArmarPreguntas(AQ: TAiJevQuestions);
var
  D: string;
begin
  AQ.AddChoice('dominio', 'Que especialista debe responder `consulta`?', DOMINIOS);
  for D in DOMINIOS do
    if Clave(D) <> 'general' then
      AQ.AddNoul('toca_' + Clave(D), 'Responder `consulta` requiere conocimiento de este tema: ' +
        Descripcion(D) + '?');
  AQ.AddNoul('necesita_fuentes',
    'Responder bien `consulta` exige citar o verificar una norma, articulo, resolucion, tarifa, ' +
    'plazo o documento especifico, en lugar de explicar un concepto general?',
    'Depende de un texto normativo o documento concreto y vigente',
    'Se responde con conocimiento general o un procedimiento conocido');
end;

// Reglas en codigo: faciles de auditar y de cambiar
procedure EnrutarConsulta(AJev: TAiJev; AQ: TAiJevQuestions; const AConsulta: string);
var
  State: TJSONObject;
  R: TAiJevResult;
  Dominio, Agente, Tambien, D: string;
  NecesitaRag: Boolean;
  i: Integer;
begin
  State := TJSONObject.Create;
  try
    State.AddPair('consulta', AConsulta);
    R := AJev.Ask(State, AQ);
  finally
    State.Free;
  end;
  try
    Inc(TotalTokens, R.InputTokens);

    Dominio := R['dominio'].Choice;
    if R['dominio'].Confidence < UMBRAL_DOMINIO then
      Dominio := 'general';
    NecesitaRag := R['necesita_fuentes'].Noul >= UMBRAL_RAG;

    // Agente del dominio: con RAG si hace falta y existe; si no, el que haya
    Agente := '';
    for i := Low(AGENTES) to High(AGENTES) do
      if (AGENTES[i].Dominio = Dominio) and (AGENTES[i].ConRag = NecesitaRag) then
        Agente := AGENTES[i].Nombre;
    if Agente = '' then
      for i := Low(AGENTES) to High(AGENTES) do
        if AGENTES[i].Dominio = Dominio then
        begin
          Agente := AGENTES[i].Nombre;
          if NecesitaRag and not AGENTES[i].ConRag then
            Agente := Agente + ' (pide fuentes y no tiene RAG)';
          Break;
        end;

    // Consultas que cruzan dominios
    Tambien := '';
    for D in DOMINIOS do
      if (Clave(D) <> 'general') and (Clave(D) <> Dominio) and
         (R['toca_' + Clave(D)].Noul >= UMBRAL_CRUCE) then
        Tambien := Tambien + ' ' + Clave(D);

    Writeln(AConsulta);
    Writeln(Format('  -> %s   (dominio %s, confianza %.2f, necesita fuentes %.2f)',
      [Agente, R['dominio'].Choice, R['dominio'].Confidence, R['necesita_fuentes'].Noul]));
    if Tambien <> '' then
      Writeln('     consultar tambien:' + Tambien);
    Writeln('     alternativas: ' + string.Join(', ', R['dominio'].Top(3)));
    Writeln;
  finally
    R.Free;
  end;
end;

procedure BloqueAtajos(AJev: TAiJev);
var
  Conf, P: Double;
  Tarea: string;
begin
  Writeln('--- B. Atajos para una sola pregunta ---');
  Tarea := AJev.Choose('Traduce al ingles: la reunion se aplaza para el lunes',
    'Que tipo de tarea pide el texto?', ['conversacion', 'redaccion', 'codigo', 'analisis'], Conf);
  Writeln(Format('Choose -> %s (confianza %.2f)', [Tarea, Conf]));

  P := AJev.Noul('Mi pago lleva 3 dias fallando y necesito resolverlo hoy',
    'El mensaje expresa urgencia?');
  Writeln(Format('Noul   -> urgencia %.2f', [P]));
  Writeln;
end;

{ TGrafoHandlers }

procedure TGrafoHandlers.AgenteExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink;
  Input: string; var Output: string);
begin
  Output := Format('%s atiende: "%s"', [Node.Name, Input]);
end;

procedure TGrafoHandlers.FinExec(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink;
  Input: string; var Output: string);
begin
  Output := Input;
end;

procedure TGrafoHandlers.AjustarRuta(Sender: TObject; ANode: TAIAgentsNode;
  AResult: TAiJevResult; var ARoute: string);
begin
  if ARoute = 'contable' then
    if AResult['fuentes'].Noul >= UMBRAL_RAG then
      ARoute := 'contable_normativo'
    else
      ARoute := 'contable_rapido';
end;

// Bloque C: el enrutado dentro de un grafo. El nodo Recepcion no tiene
// OnExecute: su Tool (TAiJevRouterTool) escribe 'next_route' y el link
// lmConditional lo sigue. Sin coincidencia o con confianza baja -> NextNo.
procedure BloqueGrafo(const AConsultas: array of string);
var
  Grafo: TAIAgentManager;
  Handlers: TGrafoHandlers;
  Router: TAiJevRouterTool;
  Targets: TDictionary<string, string>;
  D, C: string;
  i: Integer;
begin
  Writeln('--- C. El mismo enrutado dentro de un grafo de agentes ---');
  Writeln;
  Grafo := TAIAgentManager.Create(nil);
  Handlers := TGrafoHandlers.Create;
  try
    Grafo.AddNode('Recepcion', nil).AddNode('humano', Handlers.AgenteExec).AddNode('Fin', Handlers.FinExec);
    for i := Low(AGENTES) to High(AGENTES) do
      Grafo.AddNode(AGENTES[i].Nombre, Handlers.AgenteExec);

    Router := TAiJevRouterTool.Create(Grafo);
    for D in DOMINIOS do
      Router.AddRoute(Clave(D), Descripcion(D));
    Router.AddFlag('fuentes', 'Responder bien `consulta` exige citar una norma, articulo, tarifa o plazo concreto?');
    // Misma pregunta que el bloque A: la redaccion mueve la confianza
    Router.Instructions := 'Que especialista debe responder `consulta`?';
    Router.MinConfidence := UMBRAL_DOMINIO;
    Router.OnRoute := Handlers.AjustarRuta;
    Grafo.FindNode('Recepcion').Tool := Router;

    // Clave de ruta -> nodo. 'contable' no aparece: OnRoute lo convierte en
    // contable_rapido o contable_normativo.
    Targets := TDictionary<string, string>.Create;
    try
      for i := Low(AGENTES) to High(AGENTES) do
        Targets.Add(AGENTES[i].Nombre, AGENTES[i].Nombre);
      Grafo.AddConditionalEdge('Recepcion', 'Enrutador', Targets);
    finally
      Targets.Free;
    end;
    Grafo.FindNode('Recepcion').Next.NextNo := Grafo.FindNode('humano');
    for i := Low(AGENTES) to High(AGENTES) do
      Grafo.AddEdge(AGENTES[i].Nombre, 'Fin');
    Grafo.AddEdge('humano', 'Fin');
    Grafo.SetEntryPoint('Recepcion').SetFinishPoint('Fin');

    for C in AConsultas do
    begin
      // Run con semilla: cada consulta arranca con el blackboard limpio
      Grafo.Run(C, procedure(B: TAIBlackboard) begin end);
      while Grafo.Busy do
      begin
        CheckSynchronize;
        Sleep(20);
      end;
      CheckSynchronize;

      Writeln(Grafo.EndNode.Output);
      Writeln(Format('  ruta=%s  eleccion=%s  confianza=%s  fuentes=%s%s', [
        Grafo.Blackboard.GetString('next_route'),
        Grafo.Blackboard.GetString('Recepcion.jev.choice'),
        Grafo.Blackboard.GetString('Recepcion.jev.confidence'),
        Grafo.Blackboard.GetString('Recepcion.jev.fuentes'),
        IfThen(Grafo.Blackboard.GetString('Recepcion.jev.error') <> '',
          '  error=' + Grafo.Blackboard.GetString('Recepcion.jev.error'), '')]));
      Writeln;
    end;
  finally
    Grafo.Free;
    Handlers.Free;
  end;
end;

var
  Jev: TAiJev;
  Preguntas: TAiJevQuestions;
  i: Integer;
begin
  try
    if GetEnvironmentVariable('TYPESAFE_API_KEY') = '' then
    begin
      Writeln('Falta la variable de entorno TYPESAFE_API_KEY (https://console.typesafe.ai/keys).');
      ExitCode := 2;
      Exit;
    end;

    Jev := TAiJev.Create(nil);
    Preguntas := TAiJevQuestions.Create(nil);
    try
      // Model queda en 'jev-1.13.0': los umbrales de arriba se calibran contra esa version
      ArmarPreguntas(Preguntas);

      Writeln('--- A. Enrutar consultas a agentes especializados ---');
      Writeln;
      if ParamCount > 0 then
        for i := 1 to ParamCount do
          EnrutarConsulta(Jev, Preguntas, ParamStr(i))
      else
        for i := Low(CONSULTAS_EJEMPLO) to High(CONSULTAS_EJEMPLO) do
          EnrutarConsulta(Jev, Preguntas, CONSULTAS_EJEMPLO[i]);

      BloqueAtajos(Jev);

      Writeln(Format('Tokens de entrada del bloque A: %d  (~US$%.5f)',
        [TotalTokens, TotalTokens * 0.042 / 1E6]));
      Writeln;

      if ParamCount > 0 then
      begin
        var Propias: TArray<string>;
        SetLength(Propias, ParamCount);
        for i := 1 to ParamCount do
          Propias[i - 1] := ParamStr(i);
        BloqueGrafo(Propias);
      end
      else
        BloqueGrafo(CONSULTAS_EJEMPLO);
    finally
      Preguntas.Free;
      Jev.Free;
    end;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ExitCode := 2;
    end;
  end;
end.
