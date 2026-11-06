program SemanticSearch;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 03-Embeddings / 05-SemanticSearch
// =============================================================================
// Pipeline completo de busqueda semantica en 5 fases + FAQ Bot.
// Contrasta busqueda sintactica vs semantica y demuestra que los embeddings
// encuentran significado, no solo texto exacto.
//
// Conceptos que cubre (secciones 21.0 y 21.10 del libro):
//   - Busqueda sintactica vs semantica (contraste practico)
//   - Fase 1: Preparacion de documentos
//   - Fase 2-3: Vectorizacion e Indexacion con TAiEmbeddingConnection
//   - Fase 4: Vectorizacion de la consulta del usuario
//   - Fase 5: Busqueda, ranking y Top-K con FindTopK
//   - FAQ Bot: responder preguntas en lenguaje natural
//   - Advertencia: modelo de embedding inmutable en el indice
//
// Proveedor: Ollama local (sin costo, sin API key)
//   Modelo principal: mxbai-embed-large:latest
//   Alternativas:     snowflake-arctic-embed:latest
//                     embeddinggemma:latest
// =============================================================================

uses
  System.SysUtils,
  System.Math,
  System.StrUtils,
  uMakerAi.Embeddings.core,
  uMakerAi.Embeddings.Connection,
  uMakerAi.Embeddings.Ollama;

const
  DRIVER = 'Ollama';
  MODEL  = 'mxbai-embed-large:latest';
  URL    = 'http://localhost:11434/api/';

// =============================================================================
//  Tipos locales
// =============================================================================
type
  TDocumento = record
    Id: Integer;
    Titulo: String;
    Contenido: String;
  end;

  TDocVectorizado = record
    Doc: TDocumento;
    Vector: TAiEmbeddingData;
  end;

// =============================================================================
//  Base de documentos (libro sec. 21.10.2)
// =============================================================================
const
  DOCS: array[0..6] of TDocumento = (
    (Id:1; Titulo:'El sistema cardiovascular';
     Contenido:'El corazon bombea sangre hacia todos los organos del cuerpo humano'),
    (Id:2; Titulo:'El ciclo del agua';
     Contenido:'El agua se evapora de los oceanos, forma nubes y cae como lluvia'),
    (Id:3; Titulo:'Profesiones medicas';
     Contenido:'Los medicos, enfermeras y especialistas trabajan en hospitales'),
    (Id:4; Titulo:'Transporte urbano';
     Contenido:'Los autos, autobuses y trenes transportan personas en la ciudad'),
    (Id:5; Titulo:'Programacion en Delphi';
     Contenido:'Delphi es un lenguaje de programacion orientado a objetos para Windows'),
    (Id:6; Titulo:'Inteligencia Artificial';
     Contenido:'Los modelos de lenguaje grande procesan texto usando redes neuronales'),
    (Id:7; Titulo:'El ciclo de carbono';
     Contenido:'Las plantas absorben CO2, los animales lo liberan al respirar')
  );

// =============================================================================
//  FAQ de ejemplo (libro sec. 21.10.8)
// =============================================================================
type
  TFAQItem = record
    Pregunta: String;
    Respuesta: String;
  end;

const
  FAQ: array[0..4] of TFAQItem = (
    (Pregunta:'Como cambio mi contrasena?';
     Respuesta:'Ve a Configuracion > Seguridad > Cambiar contrasena'),
    (Pregunta:'Como descargo mis datos?';
     Respuesta:'En Configuracion > Privacidad > Exportar mis datos'),
    (Pregunta:'Cual es el horario de atencion?';
     Respuesta:'Lunes a viernes 9 AM a 6 PM, sabados 10 AM a 3 PM'),
    (Pregunta:'Como contacto soporte?';
     Respuesta:'Email: soporte@empresa.com | Tel: +52-55-1234-5678'),
    (Pregunta:'Como cancelo mi suscripcion?';
     Respuesta:'En Configuracion > Suscripcion > Cancelar plan')
  );

// -----------------------------------------------------------------------------
//  Helper
// -----------------------------------------------------------------------------
procedure Titulo(const S: String);
begin
  Writeln;
  Writeln(StringOfChar('=', 62));
  Writeln(S);
  Writeln(StringOfChar('=', 62));
end;

// =============================================================================
//  Seccion 0 — Busqueda sintactica vs semantica (libro sec. 21.0)
// =============================================================================
procedure MostrarContraste;
begin
  Titulo('Sec. 21.0 — Busqueda sintactica vs semantica');

  Writeln('  Busqueda SINTACTICA busca texto exacto:');
  Writeln;

  var Termino := 'medico';
  var Sinonimos: TArray<String> := ['doctor', 'galeano', 'cardiologo', 'enfermero', 'clinico'];
  for var S in Sinonimos do
    Writeln(Format('    "%s" = "%s" -> %s', [Termino, S,
      IfThen(SameText(S, Termino), 'ENCONTRADO', 'no encontrado')]));

  Writeln;
  Writeln('  La busqueda SEMANTICA encontraria todos los anteriores');
  Writeln('  porque entiende que son sinonimos. Lo demostramos abajo.');
end;

// =============================================================================
//  Fases 1-3 — Preparacion, vectorizacion e indexacion (libro sec. 21.10.2-3)
// =============================================================================
function CrearIndice(Conn: TAiEmbeddingConnection): TArray<TDocVectorizado>;
var
  I: Integer;
begin
  Titulo('Sec. 21.10.2-21.10.3 — Fases 1-3: Vectorizacion e Indexacion');

  Writeln(Format('  Indexando %d documentos con %s/%s ...', [Length(DOCS), DRIVER, MODEL]));
  Writeln(Format('  Modelo usado: %s  <- NO cambiar al hacer consultas', [MODEL]));
  Writeln;

  SetLength(Result, Length(DOCS));
  for I := 0 to High(DOCS) do
  begin
    Result[I].Doc := DOCS[I];
    var Texto := DOCS[I].Titulo + '. ' + DOCS[I].Contenido;
    Result[I].Vector := Conn.CreateEmbedding(Texto, 'user');
    Writeln(Format('  [%d/%d] (%d dims) %s',
      [I+1, Length(DOCS), Length(Result[I].Vector), DOCS[I].Titulo]));
  end;
end;

// =============================================================================
//  Fases 4-5 — Consulta y ranking (libro sec. 21.10.4-5)
// =============================================================================
procedure BuscarEn(Conn: TAiEmbeddingConnection;
  const Indice: TArray<TDocVectorizado>;
  const Consulta: String; TopK: Integer = 3);
var
  I: Integer;
  Candidatos: TAiEmbeddingList;
begin
  Writeln(Format('  Consulta: "%s"', [Consulta]));

  var QVec := Conn.CreateEmbedding(Consulta, 'user');

  SetLength(Candidatos, Length(Indice));
  for I := 0 to High(Indice) do
    Candidatos[I] := Indice[I].Vector;

  var Res := TAiEmbeddingsCore.FindTopK(QVec, Candidatos, TopK);

  for I := 0 to High(Res) do
    Writeln(Format('    [%d] %.4f — %s', [I+1, Res[I].Score, Indice[Res[I].Index].Doc.Titulo]));
  Writeln;
end;

procedure SeccionBusqueda(Conn: TAiEmbeddingConnection;
  const Indice: TArray<TDocVectorizado>);
begin
  Titulo('Sec. 21.10.4-21.10.5 — Fases 4-5: Consulta y Ranking');
  Writeln('  Notar: las consultas usan sinonimos, no texto exacto de los documentos');
  Writeln;

  BuscarEn(Conn, Indice, 'doctor y hospital', 3);
  BuscarEn(Conn, Indice, 'el corazon y la sangre', 3);
  BuscarEn(Conn, Indice, 'lluvia y oceano', 3);
  BuscarEn(Conn, Indice, 'software y codigo fuente', 3);
  BuscarEn(Conn, Indice, 'CO2 y fotosintesis', 3);
end;

// =============================================================================
//  FAQ Bot (libro sec. 21.10.8)
// =============================================================================
procedure SeccionFAQBot(Conn: TAiEmbeddingConnection);
var
  Vectores: TAiEmbeddingList;
  I: Integer;
begin
  Titulo('Sec. 21.10.8 — FAQ Bot: respuestas sin coincidencia exacta de texto');

  Writeln('  Indexando preguntas del FAQ...');
  SetLength(Vectores, Length(FAQ));
  for I := 0 to High(FAQ) do
  begin
    Vectores[I] := Conn.CreateEmbedding(FAQ[I].Pregunta, 'user');
    Writeln(Format('  [%d] %s', [I+1, FAQ[I].Pregunta]));
  end;

  Writeln;
  Writeln('  Respondiendo preguntas del usuario (formuladas diferente):');
  Writeln;

  var Preguntas: TArray<String> := [
    'Olvide mi password, que hago?',
    'quiero exportar mi informacion personal',
    'a que hora abren la oficina?',
    'necesito hablar con alguien de ayuda',
    'ya no quiero el servicio'
  ];

  for var Pregunta in Preguntas do
  begin
    var QVec := Conn.CreateEmbedding(Pregunta, 'user');
    var Res  := TAiEmbeddingsCore.FindTopK(QVec, Vectores, 1);

    if Length(Res) > 0 then
    begin
      var Idx  := Res[0].Index;
      var Conf := Res[0].Score;
      Writeln(Format('  Usuario  : "%s"', [Pregunta]));
      Writeln(Format('  FAQ match: "%s"  (confianza: %.0f%%)', [FAQ[Idx].Pregunta, Conf*100]));
      Writeln(Format('  Respuesta: %s', [FAQ[Idx].Respuesta]));
      Writeln;
    end;
  end;
end;

// =============================================================================
//  PROGRAMA PRINCIPAL
// =============================================================================
procedure RunDemo;
var
  Conn: TAiEmbeddingConnection;
begin
  Writeln('=== SemanticSearch — Pipeline completo de busqueda semantica ===');
  Writeln('Driver: ', DRIVER, ' | Modelo: ', MODEL);

  Conn := TAiEmbeddingConnection.Create(nil);
  try
    Conn.DriverName := DRIVER;
    Conn.Model      := MODEL;
    Conn.Url        := URL;

    MostrarContraste;

    var Indice := CrearIndice(Conn);

    SeccionBusqueda(Conn, Indice);
    SeccionFAQBot(Conn);

    Titulo('Resumen');
    Writeln('  Pipeline de 5 fases ejecutado con exito');
    Writeln('  La busqueda semantica encontro documentos por significado,');
    Writeln('  no por coincidencia exacta de texto.');
    Writeln('  El FAQ Bot respondio sin necesitar texto identico al indexado.');
    Writeln(Format('  Modelo: %s — reindexar si cambia el modelo', [MODEL]));

  finally
    Conn.Free;
  end;
end;

begin
  try
    RunDemo;
  except
    on E: Exception do
      Writeln('ERROR: ', E.ClassName, ' — ', E.Message);
  end;
  Writeln;
  Writeln('Presiona Enter para salir...');
  Readln;
end.
