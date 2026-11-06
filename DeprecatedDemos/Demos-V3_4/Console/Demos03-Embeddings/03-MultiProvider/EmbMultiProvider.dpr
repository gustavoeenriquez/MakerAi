program EmbMultiProvider;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 03-Embeddings / 03-MultiProvider
// =============================================================================
// Genera el mismo embedding con distintos proveedores y compara:
// dimensiones del vector, tiempo de respuesta y similitud entre proveedores.
//
// Conceptos que cubre:
//   - TAiEmbeddingConnection como interfaz universal para embeddings
//   - Cambio de proveedor en tiempo de ejecución (igual que TAiChatConnection)
//   - Diferencias de dimensiones entre modelos (512, 768, 1536, 3072...)
//   - Los embeddings de diferentes modelos NO son directamente comparables
//   - Caso de uso: elegir proveedor según costo/velocidad/calidad
//
// Proveedores y modelos por defecto:
//   OpenAI  → text-embedding-3-small  (1536 dims)
//   Gemini  → models/text-embedding-004 (768 dims)
//   Ollama  → nomic-embed-text  (768 dims, local)
// =============================================================================

uses
  System.SysUtils,
  System.Diagnostics,
  uMakerAi.Embeddings.core,
  uMakerAi.Embeddings.Connection,
  uMakerAi.Embeddings.OpenAi,
  uMakerAi.Embeddings.Gemini,
  uMakerAi.Embeddings.Ollama;

// Textos de prueba — similares entre sí
const
  TEXT_A = 'Artificial intelligence is transforming software development';
  TEXT_B = 'Machine learning and AI are revolutionizing tech';

type
  TProviderConfig = record
    Driver : String;
    Model  : String;
    ApiKey : String;
    Url    : String;
    Enabled: Boolean;
  end;

const
  PROVIDERS: array[0..2] of TProviderConfig = (
    (Driver: 'OpenAI';  Model: 'text-embedding-3-small';      ApiKey: '@OPENAI_API_KEY';  Url: ''; Enabled: True),
    (Driver: 'Gemini';  Model: 'models/text-embedding-004';   ApiKey: '@GEMINI_API_KEY';  Url: ''; Enabled: True),
    (Driver: 'Ollama';  Model: 'nomic-embed-text';            ApiKey: '';                 Url: 'http://localhost:11434/api/'; Enabled: True)
  );

// =============================================================================
//  Prueba un proveedor: genera dos embeddings y calcula similitud
// =============================================================================
procedure TestProvider(const Cfg: TProviderConfig);
var
  Conn    : TAiEmbeddingConnection;
  V1, V2  : TAiEmbeddingData;
  SW      : TStopwatch;
  ElapsedA: Int64;
  Sim     : Double;
begin
  if not Cfg.Enabled then
  begin
    Writeln(Format('[%s] — omitido', [Cfg.Driver]));
    Exit;
  end;

  Writeln(StringOfChar('-', 60));
  Writeln(Format('Proveedor : %s', [Cfg.Driver]));
  Writeln(Format('Modelo    : %s', [Cfg.Model]));

  Conn := TAiEmbeddingConnection.Create(nil);
  try
    Conn.DriverName := Cfg.Driver;
    Conn.Model      := Cfg.Model;
    Conn.ApiKey     := Cfg.ApiKey;
    if Cfg.Url <> '' then
      Conn.Url := Cfg.Url;

    try
      // Embedding A
      SW := TStopwatch.StartNew;
      V1 := Conn.CreateEmbedding(TEXT_A, 'user');
      ElapsedA := SW.ElapsedMilliseconds;

      // Embedding B
      V2 := Conn.CreateEmbedding(TEXT_B, 'user');

      Sim := TAiEmbeddingsCore.CosineSimilarity(V1, V2);

      Writeln(Format('Dimensiones: %d', [Length(V1)]));
      Writeln(Format('Tiempo (A) : %d ms', [ElapsedA]));
      Writeln(Format('Similitud  : %.4f  (A↔B, misma semántica)', [Sim]));
      Writeln(Format('Tokens     : %d', [Conn.total_tokens]));
    except
      on E: Exception do
        Writeln(Format('ERROR     : %s', [E.Message]));
    end;
  finally
    Conn.Free;
  end;
  Writeln;
end;

// =============================================================================
//  DEMO
// =============================================================================
procedure RunDemo;
var
  I: Integer;
begin
  Writeln('=== EmbMultiProvider ===');
  Writeln('Mismos dos textos, distintos proveedores de embeddings.');
  Writeln;
  Writeln('Texto A: "', TEXT_A, '"');
  Writeln('Texto B: "', TEXT_B, '"');
  Writeln;
  Writeln('Nota: dimensiones y similitudes VARÍAN por modelo.');
  Writeln('      NO comparar vectors de modelos distintos entre sí.');
  Writeln;

  for I := Low(PROVIDERS) to High(PROVIDERS) do
    TestProvider(PROVIDERS[I]);

  Writeln(StringOfChar('=', 60));
  Writeln('Resumen:');
  Writeln('  - Cada modelo tiene sus propias dimensiones y escala.');
  Writeln('  - Para producción: elige UN modelo y úsalo consistentemente.');
  Writeln('  - OpenAI: alta calidad, pago. Ollama: local, gratis.');
end;

begin
  try
    RunDemo;
  except
    on E: Exception do
      Writeln('ERROR: ', E.ClassName, ' - ', E.Message);
  end;
  Writeln;
  Writeln('Presiona Enter para salir...');
  Readln;
end.
