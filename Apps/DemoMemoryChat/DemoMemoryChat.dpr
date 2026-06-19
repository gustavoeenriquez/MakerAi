program DemoMemoryChat;

{$APPTYPE CONSOLE}

// =============================================================================
// MakerAI — DemoMemoryChat
// =============================================================================
// REPL interactivo que demuestra TAiMemory + TAiChatConnection integrados.
//
// Cada prompt del usuario:
//   1. Construye un bloque de contexto desde la BD de memoria
//   2. Lo inyecta automáticamente en el prompt via PersistentMemory
//   3. Llama al LLM y muestra la respuesta en streaming
//   4. Si AutoStoreMemories=True, guarda el prompt original en memoria
//
// Comandos especiales:
//   /quit              — salir
//   /new               — nueva sesión de chat (conserva la memoria)
//   /stats             — estadísticas de la BD de memoria
//   /memories [query]  — últimas memorias o búsqueda
//   /store <texto>     — guardar memoria manualmente
//   /importance N      — importancia para el próximo /store (1-10, default 5)
//   /debug             — mostrar el contexto que se inyectaría en el siguiente prompt
//
// Configurar proveedor y API key con variables de entorno o al inicio del REPL.
// =============================================================================

uses
  System.SysUtils,
  // FireDAC SQLite (requerido por TAiMemory)
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Phys,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef,
  FireDAC.DApt,
  FireDAC.VCLUI.Wait,
  // Drivers de chat disponibles
  uMakerAi.Chat.Claude,
  uMakerAi.Chat.OpenAi,
  uMakerAi.Chat.Gemini,
  uMakerAi.Chat.Groq,
  uMakerAi.Chat.Ollama,
  // Demo
  uDemoMemoryChat in 'uDemoMemoryChat.pas';

begin
  try
    RunDemo;
  except
    on E: Exception do
    begin
      Writeln('FATAL: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
