program AllCapabilities;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 02-ChatTools / 06-AllCapabilities
// =============================================================================
// Muestra cómo configurar TAiChatConnection con múltiples capacidades y cómo
// el sistema de Gap (SessionCaps - ModelCaps) activa automáticamente los
// ChatTools bridge correspondientes.
//
// Capacidades demostradas:
//   cap_Image      — análisis de imágenes (Vision)
//   cap_WebSearch  — búsqueda web
//   cap_Pdf        — extracción de texto de PDFs
//   cap_GenImage   — generación de imágenes
//   cap_GenAudio   — síntesis de voz (TTS)
//
// Para ejecutar los modos que requieren API keys externas, configure las
// variables de entorno correspondientes (OPENAI_API_KEY, GEMINI_API_KEY, etc.)
// =============================================================================

uses
  System.SysUtils,
  System.Classes,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations;

procedure PrintCapabilities(const ATitle: string; ACaps: TAiCapabilities);
const
  CapNames: array[TAiCapability] of string = (
    'Image', 'Audio', 'Video', 'Pdf', 'WebSearch',
    'Reasoning', 'CodeInterpreter', 'Memory', 'TextEditor',
    'ComputerUse', 'Shell',
    'GenImage', 'GenAudio', 'GenVideo', 'GenReport', 'ExtractCode'
  );
var
  Cap: TAiCapability;
  Parts: TArray<string>;
  i: Integer;
begin
  SetLength(Parts, 0);
  for Cap := Low(TAiCapability) to High(TAiCapability) do
    if Cap in ACaps then
    begin
      SetLength(Parts, Length(Parts) + 1);
      Parts[High(Parts)] := CapNames[Cap];
    end;
  if Length(Parts) = 0 then
    Writeln('  ', ATitle, ': []')
  else
    Writeln('  ', ATitle, ': [', string.Join(', ', Parts), ']');
end;

procedure DemoCapabilityConfig(const ADriver, AModel: string;
  AModelCaps, ASessionCaps: TAiCapabilities);
var
  Conn: TAiChatConnection;
  Gap: TAiCapabilities;
begin
  Writeln;
  Writeln('Driver: ', ADriver, ' | Model: ', AModel);
  Writeln(StringOfChar('-', 60));

  Conn := TAiChatConnection.Create(nil);
  try
    Conn.DriverName := ADriver;
    Conn.Model      := AModel;
    Conn.Params.Values['ApiKey'] := '@' + UpperCase(ADriver) + '_API_KEY';

    Conn.ModelConfig.ModelCaps   := AModelCaps;
    Conn.ModelConfig.SessionCaps := ASessionCaps;

    PrintCapabilities('ModelCaps  ', AModelCaps);
    PrintCapabilities('SessionCaps', ASessionCaps);

    Gap := ASessionCaps - AModelCaps;
    PrintCapabilities('Gap (bridges)', Gap);

    if Gap = [] then
      Writeln('  -> Sin bridges necesarios (todo nativo)')
    else
      Writeln('  -> Bridges activados automaticamente por el motor');
  finally
    Conn.Free;
  end;
end;

begin
  Writeln('=============================================================');
  Writeln('  MakerAI - AllCapabilities: patrones de configuracion');
  Writeln('=============================================================');

  // Gemini con visión y búsqueda nativas, TTS via bridge
  DemoCapabilityConfig(
    'Gemini', 'gemini-2.0-flash',
    [cap_Image, cap_WebSearch],
    [cap_Image, cap_WebSearch, cap_GenAudio]
  );

  // Ollama local — sin capacidades nativas, todo via bridges
  DemoCapabilityConfig(
    'Ollama', 'gemma3:4b',
    [],
    [cap_Image, cap_WebSearch, cap_GenImage]
  );

  // OpenAI con visión nativa, generación de imagen via bridge DALL-E
  DemoCapabilityConfig(
    'OpenAI', 'gpt-4.1',
    [cap_Image, cap_WebSearch],
    [cap_Image, cap_WebSearch, cap_GenImage]
  );

  // Claude con visión nativa y razonamiento
  DemoCapabilityConfig(
    'Claude', 'claude-opus-4-7',
    [cap_Image, cap_Reasoning],
    [cap_Image, cap_Reasoning, cap_WebSearch]
  );

  Writeln;
  Writeln('=============================================================');
  Writeln('Consulta los demos individuales (03-Vision, 05-WebSearch,');
  Writeln('07-AudioBridge, 09-ImageGeneration) para ejemplos completos.');
  Writeln('=============================================================');

  Write('Presione Enter para salir...');
  Readln;
end.
