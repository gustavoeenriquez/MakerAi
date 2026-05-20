program DriverRegistration;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 01-Chat / 08-DriverRegistration
// =============================================================================
// Demuestra por qué GetDriversNames() retorna TODOS los providers aunque
// el uses solo mencione uMakerAi.Chat.Ollama explícitamente.
//
// Causa raíz:
//   uMakerAi.Chat.AiConnection  (interface uses)
//     └─ uMakerAi.Chat.Initializations
//          └─ implementation uses todos los drivers:
//               OpenAI, Claude, Gemini, Ollama, Groq, DeepSeek, ...
//          Cada uno ejecuta su bloque initialization que llama
//          TAiChatFactory.Instance.RegisterDriver(...)
//
// El import de uMakerAi.Chat.Ollama en el uses del programa es redundante
// (no hace daño, pero no es necesario — Initializations ya lo registra).
//
// Conceptos que cubre:
//   - Cómo funciona el auto-registro de drivers por initialization sections
//   - Por qué TAiChatConnection siempre tiene todos los drivers disponibles
//   - Diferencia entre "qué importé" y "qué quedó registrado"
//   - Cómo usar solo Ollama en runtime aunque todos estén registrados
// =============================================================================

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  uMakerAi.Chat,
  uMakerAi.Chat.AiConnection,
  // ── Solo importamos Ollama explícitamente (como en el ejemplo del estudiante)
  // Aun así, GetDriversNames() retornará TODOS los providers.
  // Ver la explicación en la sección PARTE 2 del demo.
  uMakerAi.Chat.Ollama,
  uMakerAI.ParamsRegistry,
  uMakerAi.Tools.Functions;

// =============================================================================
//  PARTE 1: Mostrar todos los drivers registrados
// =============================================================================

procedure MostrarDriversRegistrados;
var
  Conn    : TAiChatConnection;
  Drivers : TStringList;
  i       : Integer;
begin
  Writeln('=== PARTE 1: Drivers registrados en el factory ===');
  Writeln;
  Writeln('  uses del programa solo declara explicitamente: uMakerAi.Chat.Ollama');
  Writeln('  Aun asi, los drivers registrados son:');
  Writeln;

  Conn := TAiChatConnection.Create(nil);
  try
    Drivers := Conn.GetDriversNames;
    try
      for i := 0 to Drivers.Count - 1 do
        Writeln('    [', i + 1, '] ', Drivers[i]);
      Writeln;
      Writeln('  Total: ', Drivers.Count, ' drivers registrados.');
    finally
      Drivers.Free;
    end;
  finally
    Conn.Free;
  end;
end;

// =============================================================================
//  PARTE 2: Explicar por qué pasa esto
// =============================================================================

procedure ExplicarCausa;
begin
  Writeln;
  Writeln('=== PARTE 2: Por que pasa esto? ===');
  Writeln;
  Writeln('  Cada driver se auto-registra en su propio bloque initialization:');
  Writeln;
  Writeln('  Tu programa');
  Writeln('    uses uMakerAi.Chat.Ollama');
  Writeln('      -> initialization: TAiChatFactory.RegisterDriver(TAiOllamaChat)');
  Writeln('         = solo Ollama queda en el listado');
  Writeln;
  Writeln('  Si agregas mas drivers al uses, cada uno se registra:');
  Writeln('    uses uMakerAi.Chat.Ollama, uMakerAi.Chat.OpenAi, uMakerAi.Chat.Gemini');
  Writeln('      -> listado: Ollama, OpenAi, Gemini');
  Writeln;
  Writeln('  Si quieres TODOS los drivers de una vez, agrega el convenio:');
  Writeln('    uses uMakerAi.Chat.Initializations');
  Writeln('      -> carga todos los drivers + configura params de modelos especificos');
  Writeln('         (qwen3 reasoning, gemma3 vision, o3 thinking level, etc.)');
end;

// =============================================================================
//  PARTE 3: Demostrar que aun con todo registrado, solo usamos Ollama
// =============================================================================

procedure DemostrarUsoSoloOllama;
var
  Conn: TAiChatConnection;
begin
  Writeln;
  Writeln('=== PARTE 3: Usar solo Ollama aunque todos esten registrados ===');
  Writeln;
  Writeln('  Todos los drivers registrados no significan que se usen todos.');
  Writeln('  Con DriverName := ''Ollama'' solo Ollama procesa tus mensajes.');
  Writeln;

  Conn := TAiChatConnection.Create(nil);
  try
    Conn.DriverName := 'Ollama';
    Conn.Model      := 'llama3.2:latest';
    Conn.Params.Values['ApiKey']       := '';
    Conn.Params.Values['Asynchronous'] := 'False';
    Conn.Params.Values['Max_Tokens']   := '256';
    Conn.SystemPrompt.Text             := 'Responde siempre muy brevemente, en una frase.';

    Writeln('  Driver activo: ', Conn.DriverName);
    Writeln('  Modelo activo: ', Conn.Model);
    Writeln;
    Writeln('  (Nota: este demo no hace la llamada real a Ollama para no');
    Writeln('  requerir que el servidor este corriendo. La configuracion');
    Writeln('  es correcta — solo falta un Ollama local activo.)');
  finally
    Conn.Free;
  end;
end;

// =============================================================================
//  MAIN
// =============================================================================

begin
  try
    MostrarDriversRegistrados;
    ExplicarCausa;
    DemostrarUsoSoloOllama;
  except
    on E: Exception do
      Writeln('ERROR: ', E.ClassName, ' — ', E.Message);
  end;
  Writeln;
  Writeln('Presiona Enter para salir...');
  Readln;
end.
