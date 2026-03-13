program demo_agents;
{$mode objfpc}{$H+}

// Demo: Framework de Agentes — TAIAgentManager / TAIAgents
//
// Escenarios que demuestra:
//   1. Grafo lineal simple (sync):  Inicio → Procesador → Fin
//   2. Grafo condicional (sync):    Router → NodoSi | Nodono
//   3. Grafo asincrono:             OnFinish notifica cuando termina
//
// No require llamadas externas a LLM; los nodos usan handlers locales.
//
// Compilar con:
//   fpc demo_agents.pas -Fu../Source/Core -Fu../Source/Chat -Fu../Source/Agents

uses
  SysUtils, Classes, SyncObjs,
  generics.collections,
  uMakerAi.Core,
  uMakerAi.Chat.Messages,
  uMakerAi.Agents;

// ---------------------------------------------------------------------------
// Handlers de nodos (procedure of object → necesitamos un objeto contenedor)
// ---------------------------------------------------------------------------
type
  TDemoHandlers = class
    procedure OnInicio(Node, Before: TAIAgentsNode; Link: TAIAgentsLink;
                       Input: string; var Output: string);
    procedure OnProcesador(Node, Before: TAIAgentsNode; Link: TAIAgentsLink;
                           Input: string; var Output: string);
    procedure OnFin(Node, Before: TAIAgentsNode; Link: TAIAgentsLink;
                    Input: string; var Output: string);
    procedure OnRouter(Node, Before: TAIAgentsNode; Link: TAIAgentsLink;
                       Input: string; var Output: string);
    procedure OnNodoSi(Node, Before: TAIAgentsNode; Link: TAIAgentsLink;
                       Input: string; var Output: string);
    procedure OnNodoNo(Node, Before: TAIAgentsNode; Link: TAIAgentsLink;
                       Input: string; var Output: string);
    procedure OnPrint(Sender: TObject; Value: string);
    procedure OnEnd(Node: TAIAgentsNode; Value: string);
    procedure OnFinish(Sender: TObject; const Input, Output: string;
                       Status: TAgentExecutionStatus; E: Exception);
  end;

procedure TDemoHandlers.OnInicio(Node, Before: TAIAgentsNode;
  Link: TAIAgentsLink; Input: string; var Output: string);
begin
  WriteLn('  [Inicio] recibido: "', Input, '"');
  Output := 'Hola desde Inicio. Tu mensaje fue: ' + Input;
end;

procedure TDemoHandlers.OnProcesador(Node, Before: TAIAgentsNode;
  Link: TAIAgentsLink; Input: string; var Output: string);
begin
  WriteLn('  [Procesador] procesando: "', Input, '"');
  Output := UpperCase(Input);
end;

procedure TDemoHandlers.OnFin(Node, Before: TAIAgentsNode;
  Link: TAIAgentsLink; Input: string; var Output: string);
begin
  WriteLn('  [Fin] resultado final: "', Input, '"');
  Output := Input;
end;

// Router: decide el camino segun si el input contiene 'si'
procedure TDemoHandlers.OnRouter(Node, Before: TAIAgentsNode;
  Link: TAIAgentsLink; Input: string; var Output: string);
var
  LRoute: string;
begin
  WriteLn('  [Router] input: "', Input, '"');
  if Pos('si', LowerCase(Input)) > 0 then
    LRoute := 'rama_si'
  else
    LRoute := 'rama_no';
  WriteLn('  [Router] ruta elegida: ', LRoute);
  // Almacenar la ruta en el Blackboard para que el Link condicional la lea
  Node.Graph.Blackboard.SetString('next_route', LRoute);
  Output := Input;
end;

procedure TDemoHandlers.OnNodoSi(Node, Before: TAIAgentsNode;
  Link: TAIAgentsLink; Input: string; var Output: string);
begin
  WriteLn('  [NodoSi] ejecutado — aprobado');
  Output := 'APROBADO: ' + Input;
end;

procedure TDemoHandlers.OnNodoNo(Node, Before: TAIAgentsNode;
  Link: TAIAgentsLink; Input: string; var Output: string);
begin
  WriteLn('  [NodoNo] ejecutado — rechazado');
  Output := 'RECHAZADO: ' + Input;
end;

procedure TDemoHandlers.OnPrint(Sender: TObject; Value: string);
begin
  WriteLn('  [LOG] ', Value);
end;

procedure TDemoHandlers.OnEnd(Node: TAIAgentsNode; Value: string);
begin
  WriteLn('  [OnEnd] nodo=', Node.Name, ' output="', Value, '"');
end;

procedure TDemoHandlers.OnFinish(Sender: TObject; const Input, Output: string;
  Status: TAgentExecutionStatus; E: Exception);
const
  StatusNames: array[TAgentExecutionStatus] of string =
    ('Unknown','Running','Completed','Error','Timeout','Aborted','Suspended');
begin
  WriteLn('  [OnFinish] status=', StatusNames[Status],
          ' output="', Output, '"');
  if Assigned(E) then
    WriteLn('  [OnFinish] error=', E.Message);
end;

// ---------------------------------------------------------------------------
// Demo 1: Grafo lineal — Inicio → Procesador → Fin (sincrono)
// ---------------------------------------------------------------------------
procedure Demo1_Lineal(Handlers: TDemoHandlers);
var
  Agents: TAIAgents;
  Result: string;
begin
  WriteLn('=== Demo 1: Grafo lineal (sincrono) ===');

  Agents := TAIAgents.Create(nil);
  try
    Agents.Asynchronous := False;
    Agents.OnPrint      := @Handlers.OnPrint;
    Agents.OnEnd        := @Handlers.OnEnd;

    Agents
      .AddNode('Inicio',      @Handlers.OnInicio)
      .AddNode('Procesador',  @Handlers.OnProcesador)
      .AddNode('Fin',         @Handlers.OnFin)
      .AddEdge('Inicio', 'Procesador')
      .AddEdge('Procesador', 'Fin')
      .SetEntryPoint('Inicio')
      .SetFinishPoint('Fin')
      .Compile;

    Result := Agents.Run('hola mundo');
    WriteLn('  Resultado: "', Result, '"');
  finally
    Agents.Free;
  end;
  WriteLn;
end;

// ---------------------------------------------------------------------------
// Demo 2: Grafo condicional — Router → NodoSi | NodoNo (sincrono)
// ---------------------------------------------------------------------------
procedure Demo2_Condicional(Handlers: TDemoHandlers);
var
  Agents:  TAIAgents;
  Targets: specialize TDictionary<string, string>;
  Result:  string;
begin
  WriteLn('=== Demo 2: Grafo condicional (sincrono) ===');

  Agents  := TAIAgents.Create(nil);
  Targets := specialize TDictionary<string, string>.Create;
  try
    Agents.Asynchronous := False;
    Agents.OnPrint      := @Handlers.OnPrint;
    Agents.OnEnd        := @Handlers.OnEnd;

    Targets.Add('rama_si', 'NodoSi');
    Targets.Add('rama_no', 'NodoNo');

    Agents
      .AddNode('Router',  @Handlers.OnRouter)
      .AddNode('NodoSi',  @Handlers.OnNodoSi)
      .AddNode('NodoNo',  @Handlers.OnNodoNo)
      .AddConditionalEdge('Router', 'Link_Router', Targets)
      .SetEntryPoint('Router')
      .SetFinishPoint('NodoSi')
      .SetFinishPoint('NodoNo')
      .Compile;

    WriteLn('  -- input con "si" --');
    Result := Agents.Run('quiero aprobar esto si');
    WriteLn('  Resultado: "', Result, '"');
    WriteLn;

    WriteLn('  -- input sin "si" --');
    Result := Agents.Run('rechazar esto por favor');
    WriteLn('  Resultado: "', Result, '"');
  finally
    Targets.Free;
    Agents.Free;
  end;
  WriteLn;
end;

// ---------------------------------------------------------------------------
// Demo 3: Grafo asincrono — polling en Busy hasta completar
// ---------------------------------------------------------------------------
procedure Demo3_Asincrono(Handlers: TDemoHandlers);
var
  Agents:    TAIAgents;
  StartTick: QWord;
begin
  WriteLn('=== Demo 3: Grafo lineal (asincrono) ===');

  Agents := TAIAgents.Create(nil);
  try
    Agents.Asynchronous := True;
    Agents.OnPrint      := @Handlers.OnPrint;
    Agents.OnEnd        := @Handlers.OnEnd;
    Agents.OnFinish     := @Handlers.OnFinish;

    Agents
      .AddNode('Inicio',     @Handlers.OnInicio)
      .AddNode('Procesador', @Handlers.OnProcesador)
      .AddNode('Fin',        @Handlers.OnFin)
      .AddEdge('Inicio', 'Procesador')
      .AddEdge('Procesador', 'Fin')
      .SetEntryPoint('Inicio')
      .SetFinishPoint('Fin')
      .Compile;

    WriteLn('  Lanzando ejecucion asincrona...');
    Agents.Run('ejecucion asincrona');

    // Esperar hasta que el grafo termine (max 10s)
    StartTick := GetTickCount64;
    while Agents.Busy do
    begin
      Write('.');
      Sleep(100);
      if GetTickCount64 - StartTick > 10000 then
      begin
        WriteLn;
        WriteLn('  Timeout esperando respuesta asincrona');
        Agents.Abort;
        Break;
      end;
    end;
    WriteLn;
  finally
    Agents.Free;
  end;
  WriteLn;
end;

// ---------------------------------------------------------------------------
// Programa principal
// ---------------------------------------------------------------------------
var
  Handlers: TDemoHandlers;

begin
  WriteLn('=== MakerAI FPC — Demo Agents ===');
  WriteLn;

  Handlers := TDemoHandlers.Create;
  try
    Demo1_Lineal(Handlers);
    Demo2_Condicional(Handlers);
    Demo3_Asincrono(Handlers);

    WriteLn('Todos los demos completados correctamente.');
  finally
    Handlers.Free;
  end;
end.
