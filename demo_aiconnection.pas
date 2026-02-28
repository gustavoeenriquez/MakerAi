program demo_aiconnection;
{$mode objfpc}{$H+}

// Demo TAiChatConnection — conector universal que cambia de driver en runtime
//
// TAiChatConnection actua como un router: elige el driver en runtime via
// DriverName, aplica parametros via RTTI (TypInfo) y delega todo al driver.
//
// Este demo demuestra:
//   1. Crear TAiChatConnection sin especificar driver a priori
//   2. Asignar DriverName='Ollama' en runtime
//   3. Configurar parametros y llamar AddMessageAndRun
//   4. Cambiar de driver a 'GenericLLM' sin cambiar el codigo consumidor
//   5. Usar GetAvailableDrivers para listar todos los drivers registrados
//
// Requisito:
//   - Ollama corriendo en http://127.0.0.1:11434 con algun modelo (ej. gemma3:1b)
//   - O cambiar DriverName a otro driver disponible

uses
  SysUtils, Classes,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations;

var
  Conn    : TAiChatConnection;
  Drivers : TStringList;
  Resp    : string;
  I       : Integer;

begin
  WriteLn('=== MakerAI FPC — Demo TAiChatConnection (Universal Connector) ===');
  WriteLn;

  Conn := TAiChatConnection.Create(nil);
  try
    // 1. Listar drivers disponibles
    Drivers := Conn.GetDriversNames;
    try
      WriteLn('Drivers disponibles (', Drivers.Count, '):');
      for I := 0 to Drivers.Count - 1 do
        WriteLn('  [', I, '] ', Drivers[I]);
    finally
      Drivers.Free;
    end;
    WriteLn;

    // 2. Configurar Ollama
    WriteLn('--- Configurando Ollama ---');
    Conn.DriverName := 'Ollama';

    // Ajustes via Params (RTTI injection)
    Conn.Params.Values['Model']        := 'gemma3:1b';
    Conn.Params.Values['Max_tokens']   := '128';
    Conn.Params.Values['Temperature']  := '0.7';
    Conn.Params.Values['Asynchronous'] := 'False';
    WriteLn('Driver  : ', Conn.DriverName);
    WriteLn('Driver activo: ', Conn.AiChat.GetDriverName);
    WriteLn('Modelo  : ', Conn.AiChat.Model);
    WriteLn('URL     : ', Conn.AiChat.Url);
    WriteLn;

    // 3. Llamada sincrona
    WriteLn('>>> Pregunta: En una frase, que es Ollama?');
    try
      Resp := Conn.AddMessageAndRun('En una frase, que es Ollama?', 'user');
      WriteLn('<<< Respuesta: ', Resp);
      WriteLn('Tokens — Prompt: ', Conn.Prompt_tokens,
              '  Completion: ',    Conn.Completion_tokens,
              '  Total: ',         Conn.Total_tokens);
    except
      on E: Exception do
        WriteLn('[ERROR Ollama] ', E.Message);
    end;
    WriteLn;

    // 4. Cambiar de driver a GenericLLM (misma interfaz, diferente backend)
    WriteLn('--- Cambiando a GenericLLM ---');
    Conn.DriverName := 'GenericLLM';
    Conn.Params.Values['Url']   := 'http://127.0.0.1:11434/v1/';
    Conn.Params.Values['Model'] := 'gemma3:1b';
    Conn.Params.Values['Max_tokens']   := '64';
    Conn.Params.Values['Asynchronous'] := 'False';
    WriteLn('Driver  : ', Conn.DriverName);
    WriteLn('Driver activo: ', Conn.AiChat.GetDriverName);
    WriteLn('Modelo  : ', Conn.AiChat.Model);
    WriteLn('URL     : ', Conn.AiChat.Url);
    WriteLn;

    // Nota: Ollama endpoint /v1/chat/completions (OpenAI-compat)
    WriteLn('>>> Pregunta: Di "Hola desde GenericLLM" en una sola linea.');
    try
      Resp := Conn.AddMessageAndRun(
          'Di "Hola desde GenericLLM" en una sola linea.', 'user');
      WriteLn('<<< Respuesta: ', Resp);
    except
      on E: Exception do
        WriteLn('[ERROR GenericLLM] ', E.Message);
    end;

    WriteLn;
    WriteLn('Demo finalizado. TAiChatConnection cambio de driver sin modificar');
    WriteLn('el codigo consumidor. El patron Facade funciona correctamente.');

  finally
    Conn.Free;
  end;
end.
