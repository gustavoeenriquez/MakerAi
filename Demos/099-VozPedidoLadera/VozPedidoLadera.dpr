program VozPedidoLadera;

// =============================================================================
// DEMO 099 - La cima: voz que delega en un grafo federado
// =============================================================================
// Una clienta llama a Cafe La Ladera y pide cafe hablando. Lo que pasa:
//
//   voz (GPT-Live)
//     -> DelegateChat: un chat con la herramienta 'pedidos_ladera'
//          -> TAiA2AAgentTool: llama por A2A al AGENTE DE PEDIDOS
//               grafo: Recibir -> [Inventario || Cotizar] -> Consolidar
//                        Inventario es un nodo FEDERADO: TAiA2ARemoteAgentTool
//                        le pregunta por A2A al AGENTE DE BODEGA (otro grafo)
//               Consolidar -> 'faltantes > 0' ? Avisar : Confirmar -> Responder
//     <- la respuesta vuelve por la misma cadena y la voz la dice
//
// Tres agentes, dos protocolos de delegacion (DelegateChat y A2A) y un grafo
// que delega un nodo en otro grafo. En un solo proceso para el demo; en la
// vida real Bodega y Pedidos corren en otros servidores (solo cambia la URL).
//
// Uso:
//   VozPedidoLadera.exe                       microfono y parlantes (Enter para salir)
//   VozPedidoLadera.exe --audio pedido.pcm    voz desde un archivo PCM16 24 kHz mono
//                                             (prueba sin microfono; no reproduce audio)
//   ... --salida llamada.pcm                  guarda las dos voces (clienta y Lucia) en un PCM16
//                                             24 kHz mono, alineadas con el reloj de la corrida
//   VozPedidoLadera.exe --texto "Quiero 2 bolsas de Huila Reserva para Pitalito"
//                                             sin voz: el chat delegado directo
//
// Requiere OPENAI_API_KEY. Empresa, productos y precios ficticios.
// =============================================================================

{$APPTYPE CONSOLE}
{$R *.res}

uses
  {$IFDEF MSWINDOWS} Winapi.Windows, {$ENDIF}
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.StrUtils,
  System.IOUtils,
  System.Diagnostics,
  System.Math,
  uMakerAi.Agents,
  uMakerAi.A2A.Server,
  uMakerAi.A2A.Client,
  uMakerAi.Tools.Functions,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations,   // registra todos los drivers
  uMakerAi.Realtime,
  uMakerAi.Realtime.OpenAI.Live,
  uMakerAi.Utils.AudioCapture,
  uMakerAi.Utils.AudioPlayback;

const
  PUERTO_BODEGA = 8301;
  PUERTO_PEDIDOS = 8302;
  CIUDAD_LOCAL = 'Pitalito';

type
  TProducto = record
    Nombre: string;
    Precio, Existencias: Integer;
  end;

const
  CATALOGO: array [0 .. 3] of TProducto = (
    (Nombre: 'Huila Reserva'; Precio: 38000; Existencias: 24),
    (Nombre: 'Nariño Altura'; Precio: 42000; Existencias: 10),
    (Nombre: 'Sierra Nevada Orgánico'; Precio: 36000; Existencias: 0),
    (Nombre: 'Descafeinado Suave'; Precio: 34000; Existencias: 15));

type
  // Nodos de los dos grafos y eventos de la voz (los eventos son "of object")
  TLadera = class
  private
    FLock: TCriticalSection;
  public
    UltimaRespuesta: string;
    Textos: Integer;          // textos completos del asistente recibidos
    Delegaciones: Integer;
    SesionLista, Cerrada: Boolean;
    Reloj: TStopwatch;
    Salida: TFileStream;      // --salida: las voces, para un video o una revision
    Mezcla: TArray<Integer>;  // las dos voces sumadas, en muestras de 24 kHz
    FinVoz: array [Boolean] of Int64;   // donde termina lo grabado de cada voz (True = Lucia)
    procedure GrabarAudio(const AData: TBytes; ALucia: Boolean);
    constructor Create;
    destructor Destroy; override;
    procedure Log(const S: string);
    // Agente de Bodega
    procedure Consultar(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    // Agente de Pedidos
    procedure Recibir(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure Cotizar(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure Consolidar(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure Avisar(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure Confirmar(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    procedure Responder(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
    // Voz
    procedure VozLista(Sender: TObject);
    procedure VozUsuario(Sender: TObject; const Transcript, ItemId: string);
    procedure VozAsistente(Sender: TObject; const AText: string);
    procedure VozDelegacion(Sender: TObject; const DelegationId, Context: string);
    procedure VozError(Sender: TObject; const ErrorMsg, ErrorCode: string);
    procedure VozCerrada(Sender: TObject; const Reason: string; Seconds: Double);
    procedure VozAudio(Sender: TObject; const AData: TBytes);
  end;

var
  Player: TAiAudioPlayer;
  Live: TAiOpenAiLiveChat;

constructor TLadera.Create;
begin
  inherited;
  FLock := TCriticalSection.Create;
  Reloj := TStopwatch.StartNew;
end;

destructor TLadera.Destroy;
var
  Pcm: TBytes;
  I: Integer;
begin
  if Salida <> nil then
  begin
    SetLength(Pcm, Length(Mezcla) * 2);
    for I := 0 to High(Mezcla) do
      PSmallInt(@Pcm[I * 2])^ := EnsureRange(Mezcla[I], -32768, 32767);
    if Length(Pcm) > 0 then
      Salida.WriteBuffer(Pcm[0], Length(Pcm));
    Salida.Free;
  end;
  FLock.Free;
  inherited;
end;

// Suma una voz a la mezcla donde suena en el tiempo de la corrida. Cada voz
// lleva su propia posicion: la clienta habla seguido desde su primer trozo y
// Lucia suena al llegar o al terminar lo anterior, como en un parlante (su voz
// llega mas rapido que tiempo real). GPT-Live manda audio en silencio mientras
// escucha: con una sola posicion para las dos voces, esos trozos partian la
// voz de la clienta.
procedure TLadera.GrabarAudio(const AData: TBytes; ALucia: Boolean);
var
  Inicio: Int64;
  I, N: Integer;
begin
  if (Salida = nil) or (Length(AData) = 0) then
    Exit;
  Inicio := FinVoz[ALucia];
  if ALucia or (Inicio = 0) then
    Inicio := Max(Inicio, Reloj.ElapsedMilliseconds * 24);
  N := Length(AData) div 2;
  if Inicio + N > Length(Mezcla) then
    SetLength(Mezcla, Inicio + N);   // lo nuevo queda en cero (silencio)
  for I := 0 to N - 1 do
    Inc(Mezcla[Inicio + I], PSmallInt(@AData[I * 2])^);
  FinVoz[ALucia] := Inicio + N;
end;

procedure TLadera.Log(const S: string);
begin
  // Los agentes corren en hilos del servidor A2A: la consola va con candado
  FLock.Enter;
  try
    Writeln(Format('%5.1fs  %s', [Reloj.ElapsedMilliseconds / 1000, S]));
  finally
    FLock.Leave;
  end;
end;

function Pesos(AValor: Integer): string;
begin
  Result := FormatFloat('#,##0', AValor).Replace(',', '.') + ' COP';
end;

function BuscarProducto(const ANombre: string; out P: TProducto): Boolean;
var
  X: TProducto;
begin
  for X in CATALOGO do
    if SameText(X.Nombre, ANombre.Trim) then
    begin
      P := X;
      Exit(True);
    end;
  Result := False;
end;

// "2 Huila Reserva" -> cantidad y nombre
procedure PartirItem(const AItem: string; out ACant: Integer; out ANombre: string);
var
  S: string;
begin
  S := AItem.Trim;
  ACant := StrToIntDef(S.Split([' '])[0], 1);
  ANombre := S.Substring(S.IndexOf(' ') + 1).Trim;
end;

{ ---- Agente de Bodega: dice que hay y que no --------------------------------- }

procedure TLadera.Consultar(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
var
  Item, Nombre, Agotados: string;
  Cant: Integer;
  P: TProducto;
begin
  Log('        [Bodega] consulta por A2A: ' + Input);
  Agotados := '';
  for Item in Input.Split([',']) do
  begin
    PartirItem(Item, Cant, Nombre);
    if not BuscarProducto(Nombre, P) or (P.Existencias < Cant) then
      Agotados := Agotados + IfThen(Agotados <> '', ', ') + Nombre;
  end;
  if Agotados = '' then
    Output := 'DISPONIBLE'
  else
    Output := 'AGOTADO: ' + Agotados;
end;

{ ---- Agente de Pedidos -------------------------------------------------------- }

procedure TLadera.Recibir(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
var
  Partes: TArray<string>;
begin
  // Formato que pide la herramienta: "cantidad producto, ... | ciudad"
  Log('      [Pedidos] pedido recibido por A2A: ' + Input);
  Partes := Input.Split(['|']);
  Node.Graph.Blackboard.SetString('items', Partes[0].Trim);
  if Length(Partes) > 1 then
    Node.Graph.Blackboard.SetString('ciudad', Partes[1].Trim)
  else
    Node.Graph.Blackboard.SetString('ciudad', CIUDAD_LOCAL);
  // Las dos ramas reciben los productos; Inventario se los manda a la Bodega
  Output := Partes[0].Trim;
end;

procedure TLadera.Cotizar(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
var
  Item, Nombre: string;
  Cant, Subtotal: Integer;
  P: TProducto;
begin
  Subtotal := 0;
  for Item in Node.Graph.Blackboard.GetString('items').Split([',']) do
  begin
    PartirItem(Item, Cant, Nombre);
    if BuscarProducto(Nombre, P) then
      Inc(Subtotal, Cant * P.Precio);
  end;
  if SameText(Node.Graph.Blackboard.GetString('ciudad'), CIUDAD_LOCAL) then
    Node.Graph.Blackboard.SetInteger('envio', 5000)
  else
    Node.Graph.Blackboard.SetInteger('envio', 15000);
  Node.Graph.Blackboard.SetInteger('subtotal', Subtotal);
  Output := 'subtotal=' + IntToStr(Subtotal);
end;

procedure TLadera.Consolidar(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
var
  BB: TAIBlackboard;
  Bodega: string;
begin
  // jmAll: Inventario (la Bodega, por A2A) y Cotizar ya terminaron
  BB := Node.Graph.Blackboard;
  Bodega := Node.Graph.FindNode('Inventario').Output;
  if Bodega.Contains('AGOTADO') then
  begin
    BB.SetInteger('faltantes', 1);
    BB.SetString('agotados', Bodega.Substring(Bodega.IndexOf(':') + 1).Trim);
  end
  else
    BB.SetInteger('faltantes', 0);
  BB.SetInteger('total', BB.GetInteger('subtotal') + BB.GetInteger('envio'));
  Log(Format('      [Pedidos] bodega: %s; total %s', [Bodega, Pesos(BB.GetInteger('total'))]));
  Output := Input;
end;

procedure TLadera.Avisar(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  Output := Format('NO SE PUEDE DESPACHAR: agotado %s. Disponibles: Huila Reserva, Nariño Altura, ' +
    'Descafeinado Suave.', [Node.Graph.Blackboard.GetString('agotados')]);
end;

procedure TLadera.Confirmar(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
var
  BB: TAIBlackboard;
begin
  BB := Node.Graph.Blackboard;
  Output := Format('PEDIDO CONFIRMADO: %s. Subtotal %s, envío %s, total %s. Entrega en %s %s.',
    [BB.GetString('items'), Pesos(BB.GetInteger('subtotal')), Pesos(BB.GetInteger('envio')),
     Pesos(BB.GetInteger('total')), BB.GetString('ciudad'),
     IfThen(SameText(BB.GetString('ciudad'), CIUDAD_LOCAL), 'hoy con mensajero', 'en 2 a 3 días')]);
end;

procedure TLadera.Responder(Node, BeforeNode: TAIAgentsNode; Link: TAIAgentsLink; Input: string; var Output: string);
begin
  Output := Input;
  Log('      [Pedidos] respuesta: ' + Output);
end;

{ ---- Eventos de la voz ---------------------------------------------------------- }

procedure TLadera.VozLista(Sender: TObject);
begin
  SesionLista := True;
  Log('[voz] sesión lista');
end;

procedure TLadera.VozUsuario(Sender: TObject; const Transcript, ItemId: string);
begin
  Log('[voz] clienta: ' + Transcript.Trim);
end;

procedure TLadera.VozAsistente(Sender: TObject; const AText: string);
begin
  UltimaRespuesta := AText.Trim;
  Inc(Textos);
  Log('[voz] Lucía: ' + UltimaRespuesta);
end;

procedure TLadera.VozDelegacion(Sender: TObject; const DelegationId, Context: string);
begin
  Inc(Delegaciones);
  Log('  [delegación] al chat con la herramienta pedidos_ladera');
end;

procedure TLadera.VozError(Sender: TObject; const ErrorMsg, ErrorCode: string);
begin
  Log('[voz] ERROR ' + ErrorCode + ': ' + ErrorMsg);
end;

procedure TLadera.VozCerrada(Sender: TObject; const Reason: string; Seconds: Double);
begin
  Cerrada := True;
  Log(Format('[voz] sesión cerrada (%s, %.0f s de audio)', [Reason, Seconds]));
end;

procedure TLadera.VozAudio(Sender: TObject; const AData: TBytes);
begin
  // La voz de Lucia, a los parlantes (en la prueba con archivo no se reproduce)
  GrabarAudio(AData, True);
  if Player.Active then
    Player.PlayPCM16(AData, Live.TargetSampleRate, 1);
end;

{ ---- Utilidades ------------------------------------------------------------------ }

function ArgValue(const AName, ADefault: string): string;
var
  I: Integer;
begin
  Result := ADefault;
  for I := 1 to ParamCount - 1 do
    if SameText(ParamStr(I), AName) then
      Exit(ParamStr(I + 1));
end;

// Espera atendiendo lo que los componentes encolan para el hilo principal
procedure Bombear(AMs: Integer);
var
  T: TStopwatch;
begin
  T := TStopwatch.StartNew;
  while T.ElapsedMilliseconds < AMs do
    CheckSynchronize(20);
end;

// El microfono: un hilo lee Enter mientras el principal atiende la voz
var
  SalirPorEnter: Boolean;

procedure EsperarEnter;
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      Readln;
      SalirPorEnter := True;
    end).Start;
  while not SalirPorEnter do
    CheckSynchronize(20);
end;

// Alimenta la sesion con un archivo PCM16 24 kHz mono, a ritmo real
procedure HablarDesdeArchivo(ALadera: TLadera; const AArchivo: string);
const
  BLOQUE = 4800;   // 100 ms a 24 kHz, 16 bits
var
  Datos, Trozo, Silencio: TBytes;
  Pos, Esperado, Textos: Integer;
  T: TStopwatch;
begin
  Datos := TFile.ReadAllBytes(AArchivo);
  if AArchivo.EndsWith('.wav', True) then
    Datos := Copy(Datos, 44, MaxInt);   // cabecera WAV estandar
  ALadera.Log(Format('[voz] enviando %s (%.1f s de audio)', [ExtractFileName(AArchivo), Length(Datos) / 48000]));
  Pos := 0;
  while Pos < Length(Datos) do
  begin
    Trozo := Copy(Datos, Pos, BLOQUE);
    Live.SendAudioChunk(Trozo);
    ALadera.GrabarAudio(Trozo, False);   // la voz de la clienta, en su momento
    Inc(Pos, BLOQUE);
    Bombear(100);
  end;
  // Silencio para que el VAD del servidor cierre el turno
  SetLength(Silencio, BLOQUE);
  FillChar(Silencio[0], BLOQUE, 0);
  for Esperado := 1 to 15 do
  begin
    Live.SendAudioChunk(Silencio);
    Bombear(100);
  end;
  // Espera la respuesta final: despues de delegar, 10 s sin texto nuevo del
  // asistente (cada texto reinicia la cuenta, para no cortarle la frase)
  T := TStopwatch.StartNew;
  Esperado := 0;
  Textos := ALadera.Textos;
  while T.ElapsedMilliseconds < 90000 do
  begin
    Live.SendAudioChunk(Silencio);   // la sesion sigue "oyendo" silencio
    Bombear(100);
    if ALadera.Textos <> Textos then
    begin
      Textos := ALadera.Textos;
      Esperado := 0;
    end
    else if (ALadera.Delegaciones > 0) and (ALadera.Textos > 0) then
    begin
      Inc(Esperado);
      if Esperado > 100 then
        Break;
    end;
  end;
end;

var
  Ladera: TLadera;
  Bodega, Pedidos: TAIAgentManager;
  SrvBodega, SrvPedidos: TAiA2AServer;
  Federado: TAiA2ARemoteAgentTool;
  Funcs: TAiFunctions;
  HerramientaPedidos: TAiA2AAgentTool;
  Chat: TAiChatConnection;
  Capture: TAiAudioCapture;
  Texto, Audio: string;
var
  BufSalida: array [0 .. 0] of Byte;

begin
  {$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);
  SetTextCodePage(Output, CP_UTF8);
  {$ENDIF}
  // Salida a un pipe (grabaciones, logs): el RTL la junta en bloques de 128 bytes y
  // el streaming se veria de golpe. Con un buffer de 1 byte cada caracter sale al
  // escribirlo (en una consola real ya se vaciaba en cada Write).
  SetTextBuf(Output, BufSalida, 1);
  if GetEnvironmentVariable('OPENAI_API_KEY') = '' then
  begin
    Writeln('Falta la variable OPENAI_API_KEY.');
    ExitCode := 1;
    Exit;
  end;
  Texto := ArgValue('--texto', '');
  Audio := ArgValue('--audio', '');

  Ladera := TLadera.Create;
  if ArgValue('--salida', '') <> '' then
    Ladera.Salida := TFileStream.Create(ArgValue('--salida', ''), fmCreate);
  Bodega := TAIAgentManager.Create(nil);
  Pedidos := TAIAgentManager.Create(nil);
  SrvBodega := TAiA2AServer.Create(nil);
  SrvPedidos := TAiA2AServer.Create(nil);
  Funcs := TAiFunctions.Create(nil);
  HerramientaPedidos := TAiA2AAgentTool.Create(nil);
  Chat := TAiChatConnection.Create(nil);
  Live := TAiOpenAiLiveChat.Create(nil);
  Capture := TAiAudioCapture.Create(nil);
  Player := TAiAudioPlayer.Create(nil);
  try
    try
      // --- 1. Agente de Bodega: un grafo de un nodo, publicado por A2A ----------
      Bodega.Name := 'AgenteBodega';
      Bodega.AddNode('Consultar', Ladera.Consultar).SetEntryPoint('Consultar').SetFinishPoint('Consultar');
      SrvBodega.AgentManager := Bodega;
      SrvBodega.AgentName := 'Bodega de Café La Ladera';
      SrvBodega.AgentDescription := 'Existencias de cada café';
      SrvBodega.Port := PUERTO_BODEGA;
      SrvBodega.Active := True;

      // --- 2. Agente de Pedidos: su nodo Inventario es la Bodega (federación) ----
      Pedidos.Name := 'AgentePedidos';
      Pedidos.AddNode('Recibir', Ladera.Recibir)
        .AddNode('Inventario', nil)
        .AddNode('Cotizar', Ladera.Cotizar)
        .AddNode('Consolidar', Ladera.Consolidar)
        .AddNode('Avisar', Ladera.Avisar)
        .AddNode('Confirmar', Ladera.Confirmar)
        .AddNode('Responder', Ladera.Responder);
      Federado := TAiA2ARemoteAgentTool.Create(Pedidos);
      Federado.AgentUrl := Format('http://localhost:%d', [PUERTO_BODEGA]);
      Pedidos.FindNode('Inventario').Tool := Federado;   // el nodo lo ejecuta otro agente
      Pedidos.AddEdge('Recibir', 'Inventario').AddEdge('Recibir', 'Cotizar')
        .AddEdge('Inventario', 'Consolidar').AddEdge('Cotizar', 'Consolidar')
        .SetJoinMode('Consolidar', jmAll)
        .AddExpressionEdge('Consolidar', 'Avisar', 'faltantes > 0')
        .AddFallbackEdge('Consolidar', 'Confirmar')
        .AddEdge('Avisar', 'Responder').AddEdge('Confirmar', 'Responder')
        .SetEntryPoint('Recibir').SetFinishPoint('Responder');
      SrvPedidos.AgentManager := Pedidos;
      SrvPedidos.AgentName := 'Pedidos de Café La Ladera';
      SrvPedidos.AgentDescription := 'Verifica existencias, cotiza y confirma pedidos de café';
      SrvPedidos.Port := PUERTO_PEDIDOS;
      SrvPedidos.Active := True;
      Ladera.Log(Format('Agentes A2A: Bodega en :%d, Pedidos en :%d', [PUERTO_BODEGA, PUERTO_PEDIDOS]));

      // --- 3. El chat delegado: ve al agente de Pedidos como una herramienta -----
      HerramientaPedidos.AgentUrl := Format('http://localhost:%d', [PUERTO_PEDIDOS]);
      HerramientaPedidos.ToolName := 'pedidos_ladera';
      HerramientaPedidos.Description :=
        'Verifica existencias, cotiza y confirma un pedido de café. El parámetro query va con el ' +
        'formato exacto "cantidad producto, cantidad producto | ciudad", con los nombres del catálogo: ' +
        'Huila Reserva, Nariño Altura, Sierra Nevada Orgánico, Descafeinado Suave. ' +
        'Ejemplo: "2 Huila Reserva, 1 Nariño Altura | Pitalito".';
      HerramientaPedidos.Functions := Funcs;
      Chat.DriverName := 'OpenAi';
      Chat.Params.Values['ApiKey'] := '@OPENAI_API_KEY';
      Chat.Params.Values['Asynchronous'] := 'False';
      Chat.AiFunctions := Funcs;
      Chat.Tool_Active := True;
      Chat.SystemPrompt.Text :=
        'Eres el backend de pedidos de Café La Ladera detrás de un asistente de voz. Para cualquier pedido ' +
        'usa la herramienta pedidos_ladera; si falta la ciudad, usa Pitalito. Responde con el resultado, ' +
        'breve, sin formato ni listas, en español: lo vas a decir en voz alta.';

      if Texto <> '' then
      begin
        // --- Sin voz: la cadena chat -> Pedidos -> Bodega ------------------------
        Ladera.Log('Clienta (texto): ' + Texto);
        Ladera.Log('Lucía: ' + Chat.AddMessageAndRun(Texto, 'user', []).Trim);
        Exit;
      end;

      // --- 4. La voz: GPT-Live delega el razonamiento en ese chat --------------
      Live.Instructions :=
        'Eres Lucía, de Café La Ladera, atendiendo pedidos por teléfono. Habla en español, cálida y breve. ' +
        'Para tomar o confirmar un pedido, delega: el sistema verifica existencias y precios. ' +
        'No inventes precios.';
      Live.DelegateChat := Chat;
      Live.InputSampleRate := 24000;
      Live.OnSessionReady := Ladera.VozLista;
      Live.OnTranscriptCompleted := Ladera.VozUsuario;
      Live.OnAssistantText := Ladera.VozAsistente;
      Live.OnDelegation := Ladera.VozDelegacion;
      Live.OnError := Ladera.VozError;
      Live.OnSessionClosed := Ladera.VozCerrada;
      Live.OnAudioChunk := Ladera.VozAudio;
      Live.Connect;
      while not Ladera.SesionLista and not Ladera.Cerrada do
        CheckSynchronize(20);

      if Audio <> '' then
        HablarDesdeArchivo(Ladera, Audio)
      else
      begin
        // Microfono -> sesion, y la voz de Lucia -> parlantes
        Player.Active := True;
        Capture.Source := asMicrophone;
        Capture.OutputSampleRate := 24000;
        Capture.OutputChannels := 1;
        Capture.RealtimeSTT := Live;
        Capture.Active := True;
        Ladera.Log('Hable cuando quiera (use audífonos). Enter para terminar.');
        EsperarEnter;
        Capture.Active := False;
      end;
      Live.Disconnect;
      Bombear(500);
    except
      on E: Exception do
        Ladera.Log(E.ClassName + ': ' + E.Message);
    end;
  finally
    Capture.Free;
    Live.Free;
    Player.Free;
    Chat.Free;
    HerramientaPedidos.Free;
    Funcs.Free;
    SrvPedidos.Active := False;
    SrvBodega.Active := False;
    SrvPedidos.Free;
    SrvBodega.Free;
    Pedidos.Free;
    Bodega.Free;
    Ladera.Free;
  end;
end.
