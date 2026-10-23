program EnrutadorLadera;

// =============================================================================
// DEMO 105 - Enrutar cada mensaje al agente indicado con Jev
// =============================================================================
// A Café La Ladera le escriben por pedidos, reclamos, compras al por mayor y
// preguntas sobre los cafés. Antes de gastar un modelo de lenguaje hay que
// decidir quién atiende cada mensaje. Jev (TypeSafe AI) no genera texto:
// responde preguntas tipadas con probabilidades calibradas, en milisegundos y
// por una fracción del costo.
//
//   Recepcion (TAiJevRouterTool) --(lmConditional)--> Pedidos
//                                                  -> Reclamos
//                                                  -> Mayorista
//                                                  -> Carta
//                                                  -> Persona  (otros temas)
//                                --(NextNo)--------> Persona  (confianza baja)
//
//   - Routes  : una ruta por agente, descrita como se le explicaría a una
//               recepcionista. Jev elige con su confianza.
//   - Flags   : preguntas sí/no en la MISMA llamada (aquí, si es urgente).
//   - MinConfidence: por debajo, el mensaje va a una persona.
//   - Usage   : peticiones, tokens y costo de Jev.
//
// Los agentes son nodos simples para ver solo la decisión; en la tienda cada uno
// sería un TLLMNode como el del demo 102.
//
// Requiere TYPESAFE_API_KEY (no usa OpenAI). Empresa y clientes ficticios.
// =============================================================================

{$APPTYPE CONSOLE}
{$R *.res}

uses
  {$IFDEF MSWINDOWS} Winapi.Windows, {$ENDIF}
  System.SysUtils,
  System.Classes,
  System.Diagnostics,
  System.Generics.Collections,
  uMakerAi.Jev,
  uMakerAi.Agents,
  uMakerAi.Agents.Tools.JevRouter;

const
  MENSAJES: array [0 .. 4] of string = (
    '¿Ya salió mi pedido 1047? Lo necesito para el sábado.',
    'El café me llegó molido y lo pedí en grano. ¡Qué mal!',
    'Somos un hotel y queremos 20 kilos al mes. ¿Qué precio nos dan?',
    '¿Cuál es menos ácido, el Huila Reserva o el Sierra Nevada?',
    'Quiero proponerle al dueño una alianza para exportar a Japón.');

  // clave=descripción: lo que Jev lee para decidir
  RUTAS: array [0 .. 4] of string = (
    'Pedidos=Estado, despacho o entrega de un pedido ya hecho',
    'Reclamos=Un producto o una entrega salió mal y el cliente se queja',
    'Mayorista=Compras grandes por kilos para hoteles, restaurantes u oficinas',
    'Carta=Preguntas sobre los cafés de la tienda: sabor, acidez, preparación, precio por bolsa',
    'Persona=Otros temas para el dueño: alianzas, proveedores, prensa, empleo');

type
  TRecepcion = class
  public
    procedure Atender(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure Fin(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
  end;

procedure TRecepcion.Atender(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string;
  var Output: string);
begin
  Output := 'atiende ' + Node.Name;
end;

procedure TRecepcion.Fin(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string;
  var Output: string);
begin
  Output := Input;
end;

function Clave(const AOpcion: string): string;
begin
  Result := AOpcion.Substring(0, AOpcion.IndexOf('='));
end;

function Descripcion(const AOpcion: string): string;
begin
  Result := AOpcion.Substring(AOpcion.IndexOf('=') + 1);
end;

var
  Recepcion: TRecepcion;
  Grafo: TAIAgentManager;
  Router: TAiJevRouterTool;
  Destinos: TDictionary<string, string>;
  R, Mensaje: string;
  Reloj: TStopwatch;
  Uso: TAiJevUsage;
var
  BufSalida: array [0 .. 0] of Byte;

begin
  {$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);
  SetTextCodePage(Output, CP_UTF8);
  {$ENDIF}
  // Salida a un pipe (grabaciones, logs): el RTL la junta en bloques de 128 bytes.
  // Con un buffer de 1 byte cada caracter sale al escribirlo.
  SetTextBuf(Output, BufSalida, 1);
  if GetEnvironmentVariable('TYPESAFE_API_KEY') = '' then
  begin
    Writeln('Falta la variable de entorno TYPESAFE_API_KEY (https://console.typesafe.ai/keys).');
    ExitCode := 1;
    Exit;
  end;

  Recepcion := TRecepcion.Create;
  Grafo := TAIAgentManager.Create(nil);
  try
    try
      Grafo.AddNode('Recepcion', nil)
        .AddNode('Fin', Recepcion.Fin);
      for R in RUTAS do
        Grafo.AddNode(Clave(R), Recepcion.Atender);

      // La recepción la hace Jev: una llamada elige la ruta y responde los flags
      Router := TAiJevRouterTool.Create(Grafo);
      for R in RUTAS do
        Router.AddRoute(Clave(R), Descripcion(R));
      Router.AddFlag('urgente', '¿El cliente necesita una respuesta urgente?');
      Router.Instructions := '¿Qué agente de Café La Ladera debe atender `consulta`?';
      Router.MinConfidence := 0.6;   // por debajo, lo atiende una persona
      Grafo.FindNode('Recepcion').Tool := Router;

      // Ruta elegida -> nodo; sin ruta segura -> NextNo (Persona)
      Destinos := TDictionary<string, string>.Create;
      try
        for R in RUTAS do
          Destinos.Add(Clave(R), Clave(R));
        Grafo.AddConditionalEdge('Recepcion', 'Enrutador', Destinos);
      finally
        Destinos.Free;
      end;
      Grafo.FindNode('Recepcion').Next.NextNo := Grafo.FindNode('Persona');
      for R in RUTAS do
        Grafo.AddEdge(Clave(R), 'Fin');
      Grafo.SetEntryPoint('Recepcion')
        .SetFinishPoint('Fin');

      for Mensaje in MENSAJES do
      begin
        Reloj := TStopwatch.StartNew;
        // Run con semilla: cada mensaje arranca con el blackboard limpio
        Grafo.Run(Mensaje, procedure(B: TAIBlackboard) begin end);
        while Grafo.Busy do
          CheckSynchronize(20);
        CheckSynchronize;
        Reloj.Stop;

        Writeln('Cliente: ', Mensaje);
        Writeln(Format('  -> %s   (confianza %s, urgente %s, %d ms)', [Grafo.EndNode.Output.Replace('atiende ', ''),
          Grafo.Blackboard.GetString('Recepcion.jev.confidence'),
          Grafo.Blackboard.GetString('Recepcion.jev.urgente'), Reloj.ElapsedMilliseconds]));
        Writeln('     alternativas: ', Grafo.Blackboard.GetString('Recepcion.jev.top'));
        if Grafo.Blackboard.GetString('Recepcion.jev.error') <> '' then
          Writeln('     error: ', Grafo.Blackboard.GetString('Recepcion.jev.error'));
        Writeln;
      end;

      Uso := Router.Usage;
      Writeln(Format('Jev: %d llamadas, %d tokens de entrada, US$ %.6f en total.',
        [Uso.Requests, Uso.InputTokens, Uso.CostUSD]));
      ExitCode := 0;
    except
      on E: Exception do
      begin
        Writeln('ERROR: ', E.ClassName, ': ', E.Message);
        ExitCode := 2;
      end;
    end;
  finally
    Grafo.Free;
    Recepcion.Free;
  end;
end.
