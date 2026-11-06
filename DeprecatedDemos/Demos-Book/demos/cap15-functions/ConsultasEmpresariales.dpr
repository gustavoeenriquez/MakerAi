program ConsultasEmpresariales;

{$APPTYPE CONSOLE}

// =============================================================================
// MakerAI — Capítulo 15: Function Calling
// Demo: Sistema de consultas empresariales
// =============================================================================
// Muestra cómo el LLM invoca funciones Delphi reales para responder preguntas
// de negocio: stock, precios y fecha/hora actuales.
//
// Provider: Ollama (local, sin API key)
// Modelo  : qwen2.5:latest   ← soporta function calling nativo
//           llama3.3:latest  ← alternativa
//
// Conceptos demostrados:
//   - TAiFunctions con tres herramientas registradas
//   - Handler por función (OnAction) — patrón recomendado
//   - ToolCall.Params.Values para leer argumentos (forma cómoda)
//   - Bucle interactivo: el usuario escribe preguntas en lenguaje natural
//   - El modelo decide cuándo y con qué argumentos llamar cada función
// =============================================================================

uses
  System.SysUtils,
  System.StrUtils,
  uMakerAi.Chat,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Messages,
  uMakerAi.Tools.Functions,
  uMakerAi.Chat.Ollama;   // DriverName = 'Ollama' — auto-registro en el factory

const
  DRIVER = 'Ollama';
  MODEL  = 'gemma4:e4b';   // Soporta tool calling nativo en Ollama
  // Alternativas si el modelo no está instalado:
  //   'gemma4:e2b'    — más ligero
  //   'llama3.2:latest' — si se instala con: ollama pull llama3.2
  //   'qwen2.5:latest'  — si se instala con: ollama pull qwen2.5

// =============================================================================
//  TConsultasDemo
// =============================================================================
type
  TConsultasDemo = class
  private
    FAiFuncs: TAiFunctions;

    procedure OnFnStock(Sender: TObject; FA: TFunctionActionItem;
      FN: String; TC: TAiToolsFunction; var Handled: Boolean);
    procedure OnFnPrecio(Sender: TObject; FA: TFunctionActionItem;
      FN: String; TC: TAiToolsFunction; var Handled: Boolean);
    procedure OnFnFecha(Sender: TObject; FA: TFunctionActionItem;
      FN: String; TC: TAiToolsFunction; var Handled: Boolean);

    procedure RegistrarFunciones;
  public
    procedure Run;
  end;

// =============================================================================
//  Handlers de las herramientas
// =============================================================================

procedure TConsultasDemo.OnFnStock(Sender: TObject; FA: TFunctionActionItem;
  FN: String; TC: TAiToolsFunction; var Handled: Boolean);
var
  Codigo: string;
begin
  Codigo := TC.Params.Values['codigo_producto'];
  WriteLn('  [Tool] ConsultarStock: ', Codigo);

  case IndexText(Codigo, ['PROD001', 'PROD002', 'PROD003']) of
    0: TC.Response := '{"stock":150,"estado":"disponible","ubicacion":"Almacen A"}';
    1: TC.Response := '{"stock":3,"estado":"critico","ubicacion":"Almacen B"}';
    2: TC.Response := '{"stock":0,"estado":"agotado","ubicacion":"N/A"}';
  else
    TC.Response := '{"stock":0,"estado":"no_encontrado"}';
  end;

  Handled := True;
end;

procedure TConsultasDemo.OnFnPrecio(Sender: TObject; FA: TFunctionActionItem;
  FN: String; TC: TAiToolsFunction; var Handled: Boolean);
var
  Codigo: string;
begin
  Codigo := TC.Params.Values['codigo_producto'];
  WriteLn('  [Tool] ConsultarPrecio: ', Codigo);

  case IndexText(Codigo, ['PROD001', 'PROD002', 'PROD003']) of
    0: TC.Response := '{"precio":299.99,"moneda":"USD","descuento_pct":0}';
    1: TC.Response := '{"precio":1250.00,"moneda":"USD","descuento_pct":10}';
    2: TC.Response := '{"precio":89.50,"moneda":"USD","descuento_pct":0}';
  else
    TC.Response := '{"precio":0,"error":"no_encontrado"}';
  end;

  Handled := True;
end;

procedure TConsultasDemo.OnFnFecha(Sender: TObject; FA: TFunctionActionItem;
  FN: String; TC: TAiToolsFunction; var Handled: Boolean);
begin
  WriteLn('  [Tool] ObtenerFechaActual');
  TC.Response := Format('{"fecha":"%s","hora":"%s","dia_semana":"%s"}',
    [FormatDateTime('yyyy-mm-dd', Now),
     FormatDateTime('hh:nn:ss', Now),
     FormatDateTime('dddd', Now)]);
  Handled := True;
end;

// =============================================================================
//  Registro de funciones
// =============================================================================

procedure TConsultasDemo.RegistrarFunciones;
var
  LFn   : TFunctionActionItem;
  LParam: TFunctionParamsItem;
begin
  // ── ConsultarStock ─────────────────────────────────────────────────────────
  LFn := FAiFuncs.Functions.AddFunction('ConsultarStock', True, OnFnStock);
  LFn.Description.Text :=
    'Consulta el stock disponible de un producto dado su código. ' +
    'Devuelve cantidad en stock, estado (disponible/critico/agotado) y ubicación.';
  LParam := LFn.Parameters.Add;
  LParam.Name             := 'codigo_producto';
  LParam.ParamType        := ptString;
  LParam.Required         := True;
  LParam.Description.Text := 'Código del producto (ej: PROD001, PROD002, PROD003)';

  // ── ConsultarPrecio ────────────────────────────────────────────────────────
  LFn := FAiFuncs.Functions.AddFunction('ConsultarPrecio', True, OnFnPrecio);
  LFn.Description.Text :=
    'Obtiene el precio de venta de un producto en USD. ' +
    'Incluye descuento activo si lo hay.';
  LParam := LFn.Parameters.Add;
  LParam.Name             := 'codigo_producto';
  LParam.ParamType        := ptString;
  LParam.Required         := True;
  LParam.Description.Text := 'Código del producto';

  // ── ObtenerFechaActual ─────────────────────────────────────────────────────
  LFn := FAiFuncs.Functions.AddFunction('ObtenerFechaActual', True, OnFnFecha);
  LFn.Description.Text :=
    'Obtiene la fecha y hora actuales del sistema. ' +
    'Usar cuando el usuario pregunte por la fecha, hora o día de la semana.';
  // Sin parámetros
end;

// =============================================================================
//  Run
// =============================================================================

procedure TConsultasDemo.Run;
var
  Conn    : TAiChatConnection;
  Pregunta: string;
  Resp    : string;
begin
  FAiFuncs := TAiFunctions.Create(nil);
  Conn     := TAiChatConnection.Create(nil);
  try
    RegistrarFunciones;

    Conn.DriverName := DRIVER;
    Conn.Model      := MODEL;
    Conn.Params.Values['Asynchronous'] := 'False';
    Conn.Params.Values['Tool_Active']  := 'True';
    Conn.Params.Values['Max_Tokens']   := '1024';
    Conn.AiFunctions := FAiFuncs;

    Conn.SystemPrompt.Text :=
      'Eres un asistente empresarial. Tienes acceso a herramientas para ' +
      'consultar stock, precios y fecha/hora. Úsalas cuando el usuario ' +
      'necesite esa información. Responde siempre en español.';

    WriteLn('');
    WriteLn('=== Cap. 15 — Function Calling con Ollama ===');
    WriteLn('Driver : ', DRIVER);
    WriteLn('Modelo : ', MODEL);
    WriteLn('Tools  : ConsultarStock, ConsultarPrecio, ObtenerFechaActual');
    WriteLn('Productos disponibles: PROD001, PROD002, PROD003');
    WriteLn('Escribe "salir" para terminar.');
    WriteLn('');

    repeat
      Write('Tú > ');
      ReadLn(Pregunta);
      Pregunta := Trim(Pregunta);

      if Pregunta = '' then Continue;
      if SameText(Pregunta, 'salir') then Break;

      WriteLn('');
      try
        Resp := Conn.AddMessageAndRun(Pregunta, 'user', []);
        WriteLn('IA  > ', Resp);
      except
        on E: Exception do
          WriteLn('ERROR: ', E.ClassName, ' — ', E.Message);
      end;
      WriteLn('');

    until False;

  finally
    Conn.Free;
    FAiFuncs.Free;
  end;
end;

// =============================================================================
begin
  try
    with TConsultasDemo.Create do
    try
      Run;
    finally
      Free;
    end;
  except
    on E: Exception do
      WriteLn('ERROR FATAL: ', E.ClassName, ' — ', E.Message);
  end;
  WriteLn('Presiona Enter para salir...');
  ReadLn;
end.
