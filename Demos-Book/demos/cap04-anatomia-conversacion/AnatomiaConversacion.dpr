program AnatomiaConversacion;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — Capítulo 4: Anatomía de una Conversación
// =============================================================================
// Ilustra los mecanismos fundamentales de la gestión de conversaciones con LLMs.
// Un modelo no tiene memoria propia: "recuerda" porque recibe el historial
// completo en cada llamada. El desarrollador controla ese historial.
//
// Demuestra:
//   - Demo 1: Conversación multi-turno — 3 turnos donde cada pregunta de
//             seguimiento aprovecha el contexto de los turnos anteriores.
//   - Demo 2: Pre-siembra con AddMessage — inyectar ejemplos (few-shot) al
//             historial SIN llamar al modelo. Luego una sola llamada clasifica.
//   - Demo 3: Persistencia del historial — guardar a JSON, limpiar memoria
//             y recargar para continuar la conversación donde se dejó.
//
// API usada:
//   Conn.AddMessageAndRun(texto, rol, [])  → agrega mensaje y llama al modelo
//   Conn.AddMessage(texto, rol)            → agrega al historial SIN llamar
//   Conn.Messages.Count                   → mensajes acumulados
//   Conn.NewChat                          → limpia historial (libera objetos)
//   Conn.Messages.SaveToFile(ruta)        → persiste historial en JSON
//   Conn.Messages.LoadFromFile(ruta)      → restaura historial desde JSON
//   Conn.Prompt_tokens / Completion_tokens / Total_tokens → uso de tokens
//
// Configurar antes de compilar:
//   Define GROQ_API_KEY como variable de entorno del sistema.
// =============================================================================

uses
  System.SysUtils,
  uMakerAi.Chat,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Groq;    // DriverName = 'Groq' — rápido y gratuito

const
  DRIVER  = 'Groq';
  MODEL   = 'llama-3.3-70b-versatile';
  API_KEY = '@GROQ_API_KEY';

procedure Separador(const Titulo: string);
begin
  WriteLn('');
  WriteLn(StringOfChar('-', 62));
  WriteLn('  ', Titulo);
  WriteLn(StringOfChar('-', 62));
end;

// =============================================================================
//  Demo 1 — Conversación multi-turno con contexto
//
//  En cada turno el modelo recibe el historial completo:
//    Turno 1: [system] + [user T1]
//    Turno 2: [system] + [user T1] + [assistant T1] + [user T2]
//    Turno 3: [system] + [user T1] + [assistant T1] + ... + [user T3]
//
//  Por eso puede responder "¿Cómo lo depuro?" sin repetir el contexto.
// =============================================================================
procedure Demo1_MultiTurno(Conn: TAiChatConnection);
var
  R: string;
begin
  Separador('Demo 1 — Conversación multi-turno');

  Conn.SystemPrompt.Text :=
    'Eres un asistente de soporte técnico Delphi. Responde en español, conciso.';
  Conn.NewChat;   // NewChat libera correctamente los objetos anteriores

  WriteLn('[Turno 1] "Mi aplicación se cuelga al iniciar"');
  R := Conn.AddMessageAndRun('Mi aplicación se cuelga al iniciar', 'user', []);
  WriteLn('          Asistente: ', R);
  WriteLn('          [Historial: ', Conn.Messages.Count, ' mensajes]');

  WriteLn('');
  WriteLn('[Turno 2] "Es Delphi 12, error EAccessViolation"');
  R := Conn.AddMessageAndRun('Es Delphi 12, error EAccessViolation', 'user', []);
  WriteLn('          Asistente: ', R);
  WriteLn('          [Historial: ', Conn.Messages.Count, ' mensajes]');

  WriteLn('');
  WriteLn('[Turno 3] "¿Cómo lo depuro?" — sin repetir el contexto');
  R := Conn.AddMessageAndRun('¿Cómo lo depuro?', 'user', []);
  WriteLn('          Asistente: ', R);
  WriteLn('          [Historial: ', Conn.Messages.Count, ' mensajes |',
          ' Tokens entrada: ', Conn.Prompt_tokens,
          ', salida: ', Conn.Completion_tokens,
          ', total: ', Conn.Total_tokens, ']');
end;

// =============================================================================
//  Demo 2 — Pre-siembra con AddMessage (few-shot conversacional)
//
//  AddMessage agrega mensajes al historial SIN llamar al modelo.
//  Patrón: inyectar 2-3 pares ejemplo/respuesta → luego preguntar real.
//  El modelo ya "conoce" el formato sin necesidad de explicárselo.
// =============================================================================
procedure Demo2_FewShot(Conn: TAiChatConnection);
var
  R: string;
begin
  Separador('Demo 2 — Pre-siembra con AddMessage (few-shot)');

  Conn.SystemPrompt.Text :=
    'Eres un clasificador de tickets de soporte. ' +
    'Responde ÚNICAMENTE con JSON válido: {"categoria":"...","urgencia":"ALTA|MEDIA|BAJA"}.';
  Conn.NewChat;

  // AddMessage: agrega al historial SIN costo de inferencia
  WriteLn('Inyectando 2 ejemplos de clasificación (sin llamar al modelo)...');
  Conn.AddMessage('Mi impresora no imprime', 'user');
  Conn.AddMessage('{"categoria":"HARDWARE","urgencia":"MEDIA"}', 'assistant');
  Conn.AddMessage('No puedo acceder a mi cuenta', 'user');
  Conn.AddMessage('{"categoria":"ACCESO","urgencia":"ALTA"}', 'assistant');
  WriteLn('[Historial: ', Conn.Messages.Count, ' mensajes — son los ejemplos]');

  // Primera y única llamada real al modelo: ya conoce el formato esperado
  WriteLn('');
  WriteLn('Clasificando: "La aplicación se cierra al generar reportes PDF"');
  R := Conn.AddMessageAndRun(
    'La aplicación se cierra sola al generar reportes PDF', 'user', []);
  WriteLn('Respuesta   : ', R);
  WriteLn('[Historial  : ', Conn.Messages.Count, ' mensajes en total]');
end;

// =============================================================================
//  Demo 3 — Persistencia del historial (save / load)
//
//  Guarda la conversación en JSON, limpia la memoria y la recarga para
//  continuar el hilo exactamente donde se dejó — como si no hubiera parado.
// =============================================================================
procedure Demo3_Persistencia(Conn: TAiChatConnection);
const
  ARCHIVO = 'historial_cap04.json';
var
  R: string;
begin
  Separador('Demo 3 — Persistencia del historial (save / load)');

  Conn.SystemPrompt.Text :=
    'Eres un asistente amable. Responde en español, una sola oración.';
  Conn.NewChat;

  // Construir historial con 2 turnos
  Conn.AddMessageAndRun('¿Cuál es la capital de Francia?', 'user', []);
  Conn.AddMessageAndRun('¿Y cuántos habitantes tiene?', 'user', []);
  WriteLn('Historial creado con ', Conn.Messages.Count, ' mensajes.');

  // Guardar en disco
  Conn.Messages.SaveToFile(ARCHIVO);
  WriteLn('Guardado en: ', ARCHIVO);

  // Limpiar y verificar que se perdió el contexto
  Conn.NewChat;
  WriteLn('Después de NewChat — mensajes en memoria: ', Conn.Messages.Count);

  // Restaurar desde disco
  Conn.Messages.LoadFromFile(ARCHIVO);
  WriteLn('Después de LoadFromFile — mensajes restaurados: ', Conn.Messages.Count);

  // Continuar la conversación con contexto completo
  WriteLn('');
  WriteLn('Continuando con contexto restaurado: "¿Y el río que la cruza?"');
  R := Conn.AddMessageAndRun('¿Y el río que la cruza?', 'user', []);
  WriteLn('Respuesta: ', R);
  WriteLn('[El modelo respondió con el contexto de los 2 turnos anteriores]');
end;

// =============================================================================
//  MAIN
// =============================================================================
procedure RunDemo;
var
  Conn: TAiChatConnection;
begin
  WriteLn('=== MakerAI — Capítulo 4: Anatomía de una Conversación ===');
  WriteLn('Driver: ', DRIVER, ' | Modelo: ', MODEL);

  Conn := TAiChatConnection.Create(nil);
  try
    Conn.DriverName := DRIVER;
    Conn.Model      := MODEL;

    Conn.Params.Values['ApiKey']       := API_KEY;
    Conn.Params.Values['Asynchronous'] := 'False';
    Conn.Params.Values['Max_Tokens']   := '512';

    Demo1_MultiTurno(Conn);
    Demo2_FewShot(Conn);
    Demo3_Persistencia(Conn);

    WriteLn('');
    WriteLn(StringOfChar('=', 62));
    WriteLn('  Demo completado.');
    WriteLn(StringOfChar('=', 62));
  finally
    Conn.Free;
  end;
end;

begin
  try
    RunDemo;
  except
    on E: Exception do
      WriteLn('ERROR: ', E.ClassName, ' — ', E.Message);
  end;
  WriteLn('');
  WriteLn('Presiona Enter para salir...');
  ReadLn;
end.
