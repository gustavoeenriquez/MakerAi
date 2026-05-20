program VectorOps;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 03-Embeddings / 04-VectorOps
// =============================================================================
// Demuestra las operaciones algebraicas con vectores de embeddings:
// producto escalar, similitud coseno, distancia L2, suma, resta,
// promediado, interpolacion y normalizacion, con la analogia clasica
// rey - hombre + mujer aprox reina.
//
// Conceptos que cubre (seccion 21.3 a 21.6 del libro):
//   - DotProduct, CosineSimilarity, EuclideanDistance
//   - Promediar embeddings (AverageEmbedding)
//   - Sumar y restar (VectorAdd, VectorSubtract)
//   - Analogia clasica: rey - hombre + mujer aprox reina
//   - Interpolacion manual entre dos conceptos
//   - Normalizacion (Normalize)
//
// Proveedor: Ollama local (sin costo, sin API key)
//   Modelo principal: mxbai-embed-large:latest
//   Alternativas:     snowflake-arctic-embed:latest
//                     embeddinggemma:latest
// =============================================================================

uses
  System.SysUtils,
  System.Math,
  uMakerAi.Embeddings.core,
  uMakerAi.Embeddings.Connection,
  uMakerAi.Embeddings.Ollama;

const
  DRIVER = 'Ollama';
  MODEL  = 'mxbai-embed-large:latest';
  URL    = 'http://localhost:11434/api/';

// -----------------------------------------------------------------------------
//  Helpers de presentacion
// -----------------------------------------------------------------------------
procedure Titulo(const S: String);
begin
  Writeln;
  Writeln(StringOfChar('-', 62));
  Writeln(S);
  Writeln(StringOfChar('-', 62));
end;

function NivelSim(Sim: Double): String;
begin
  if Sim >= 0.90 then Result := 'muy alto'
  else if Sim >= 0.70 then Result := 'alto'
  else if Sim >= 0.50 then Result := 'medio'
  else if Sim >= 0.30 then Result := 'bajo'
  else Result := 'muy bajo';
end;

var GDimsLog: Boolean = False;

function E(Conn: TAiEmbeddingConnection; const Texto: String): TAiEmbeddingData;
begin
  Result := Conn.CreateEmbedding(Texto, 'user');
  if not GDimsLog and (Length(Result) > 0) then
  begin
    Writeln(Format('  [modelo: %s  dims: %d]', [MODEL, Length(Result)]));
    GDimsLog := True;
  end;
end;

// =============================================================================
//  Seccion 1 — Producto escalar y metricas basicas (libro sec. 21.3-21.5)
// =============================================================================
procedure SeccionMetricas(Conn: TAiEmbeddingConnection);
var
  VFr, VDr, VAv: TAiEmbeddingData;
begin
  Titulo('Sec. 21.3-21.5  Producto escalar, similitud coseno y distancia L2');

  Writeln('  Vectorizando: "medico", "doctor", "avion" ...');
  VFr := E(Conn, 'medico');
  VDr := E(Conn, 'doctor');
  VAv := E(Conn, 'avion');
  Writeln;

  Writeln(Format('  DotProduct(medico, doctor) = %.4f', [TAiEmbeddingsCore.DotProduct(VFr, VDr)]));
  Writeln(Format('  DotProduct(medico, avion)  = %.4f', [TAiEmbeddingsCore.DotProduct(VFr, VAv)]));
  Writeln;

  var SimMD := TAiEmbeddingsCore.CosineSimilarity(VFr, VDr);
  var SimMA := TAiEmbeddingsCore.CosineSimilarity(VFr, VAv);

  Writeln(Format('  CosineSimilarity(medico, doctor) = %.4f  [%s]', [SimMD, NivelSim(SimMD)]));
  Writeln(Format('  CosineSimilarity(medico, avion)  = %.4f  [%s]', [SimMA, NivelSim(SimMA)]));
  Writeln;

  Writeln(Format('  EuclideanDistance(medico, doctor) = %.4f  (menor = mas cerca)', [TAiEmbeddingsCore.EuclideanDistance(VFr, VDr)]));
  Writeln(Format('  EuclideanDistance(medico, avion)  = %.4f', [TAiEmbeddingsCore.EuclideanDistance(VFr, VAv)]));
end;

// =============================================================================
//  Seccion 2 — Promediar embeddings (libro sec. 21.6 op. 1)
// =============================================================================
procedure SeccionPromediar(Conn: TAiEmbeddingConnection);
var
  Palabras: TArray<String>;
  Vectores: TAiEmbeddingList;
  Promedio: TAiEmbeddingData;
  I: Integer;
begin
  Titulo('Sec. 21.6  Operacion 1 — Promediar (tema general de un grupo)');

  Palabras := ['perro', 'gato', 'caballo', 'conejo', 'pajaro'];
  Writeln(Format('  Promediando %d animales: %s', [Length(Palabras), String.Join(', ', Palabras)]));

  SetLength(Vectores, Length(Palabras));
  for I := 0 to High(Palabras) do
    Vectores[I] := E(Conn, Palabras[I]);

  Promedio := TAiEmbeddingsCore.AverageEmbedding(Vectores);

  Writeln;
  Writeln('  Similitud del vector-promedio con cada animal:');
  for I := 0 to High(Palabras) do
    Writeln(Format('    %s: %.4f', [Palabras[I], TAiEmbeddingsCore.CosineSimilarity(Promedio, Vectores[I])]));

  Writeln;
  var VAnimal := E(Conn, 'animal domestico');
  var VCoche  := E(Conn, 'automovil');
  Writeln('  Comparacion con palabras externas:');
  Writeln(Format('    "animal domestico": %.4f  (esperado: alto)', [TAiEmbeddingsCore.CosineSimilarity(Promedio, VAnimal)]));
  Writeln(Format('    "automovil":        %.4f  (esperado: bajo)', [TAiEmbeddingsCore.CosineSimilarity(Promedio, VCoche)]));
end;

// =============================================================================
//  Seccion 3 — Sumar embeddings: buscar A AND B (libro sec. 21.6 op. 2)
// =============================================================================
procedure SeccionSuma(Conn: TAiEmbeddingConnection);
var
  VGato, VRojo, VCombinado: TAiEmbeddingData;
  Candidatos: TArray<String>;
  Vectores: TAiEmbeddingList;
  I: Integer;
begin
  Titulo('Sec. 21.6  Operacion 2 — Sumar: gato + rojo (AND semantico)');

  Writeln('  Buscamos documentos que hablen de AMBOS conceptos: gato AND rojo');
  Candidatos := ['gato rojo', 'gato azul', 'perro rojo', 'auto rojo', 'gato negro', 'flor roja'];

  VGato      := E(Conn, 'gato');
  VRojo      := E(Conn, 'rojo');
  VCombinado := TAiEmbeddingsCore.VectorAdd(VGato, VRojo);

  SetLength(Vectores, Length(Candidatos));
  for I := 0 to High(Candidatos) do
    Vectores[I] := E(Conn, Candidatos[I]);

  var Res := TAiEmbeddingsCore.FindTopK(VCombinado, Vectores, 3);

  Writeln;
  Writeln('  Top 3 mas cercanos al vector (gato + rojo):');
  for I := 0 to High(Res) do
    Writeln(Format('    [%d] %.4f — %s', [I+1, Res[I].Score, Candidatos[Res[I].Index]]));
end;

// =============================================================================
//  Seccion 4 — Analogia: rey - hombre + mujer aprox reina (libro sec. 21.6 op. 3)
// =============================================================================
procedure SeccionAnalogia(Conn: TAiEmbeddingConnection);
var
  VRey, VHombre, VMujer, VAnalogy: TAiEmbeddingData;
  Candidatos: TArray<String>;
  Vectores: TAiEmbeddingList;
  I: Integer;
begin
  Titulo('Sec. 21.6  Operacion 3 — Analogia: rey - hombre + mujer aprox reina');

  Writeln('  Formula:  VAnalogy = VectorAdd(VectorSubtract(rey, hombre), mujer)');
  Writeln('  Resultado esperado: reina (o terminos semanticamente equivalentes)');
  Writeln;

  VRey    := E(Conn, 'rey');
  VHombre := E(Conn, 'hombre');
  VMujer  := E(Conn, 'mujer');

  // rey - hombre + mujer
  VAnalogy := TAiEmbeddingsCore.VectorAdd(
    TAiEmbeddingsCore.VectorSubtract(VRey, VHombre),
    VMujer
  );

  Candidatos := ['reina', 'princesa', 'principe', 'rey', 'senora', 'jefa', 'mujer', 'dama'];
  SetLength(Vectores, Length(Candidatos));
  for I := 0 to High(Candidatos) do
    Vectores[I] := E(Conn, Candidatos[I]);

  var Res := TAiEmbeddingsCore.FindTopK(VAnalogy, Vectores, 5);

  Writeln('  Top 5 candidatos mas cercanos al vector-analogia:');
  for I := 0 to High(Res) do
    Writeln(Format('    [%d] %.4f — %s', [I+1, Res[I].Score, Candidatos[Res[I].Index]]));
  Writeln;
  Writeln('  Nota: el resultado varia segun el modelo. Modelos con mayor');
  Writeln('  vocabulario semantico ubican "reina" en el top 1 o 2.');
end;

// =============================================================================
//  Seccion 5 — Interpolacion (libro sec. 21.6 op. 4)
// =============================================================================
procedure SeccionInterpolacion(Conn: TAiEmbeddingConnection);
var
  VGato, VPerro, VMezcla: TAiEmbeddingData;
  I, J: Integer;
  Alpha, Beta: Double;
begin
  Titulo('Sec. 21.6  Operacion 4 — Interpolacion: transicion gato a perro');

  VGato  := E(Conn, 'gato');
  VPerro := E(Conn, 'perro');

  Writeln(Format('  %-6s  %-14s  %-14s', ['Alpha', 'Sim(gato)', 'Sim(perro)']));
  Writeln('  ' + StringOfChar('-', 38));

  for I := 0 to 5 do
  begin
    Alpha := I / 5.0;
    Beta  := 1.0 - Alpha;

    // alpha * gato + (1-alpha) * perro
    SetLength(VMezcla, Length(VGato));
    for J := 0 to High(VGato) do
      VMezcla[J] := Alpha * VGato[J] + Beta * VPerro[J];

    var SG := TAiEmbeddingsCore.CosineSimilarity(VMezcla, VGato);
    var SP := TAiEmbeddingsCore.CosineSimilarity(VMezcla, VPerro);
    Writeln(Format('  %-6.1f  %-14.4f  %-14.4f', [Alpha, SG, SP]));
  end;

  Writeln;
  Writeln('  alpha=0.0 -> 100% perro | alpha=1.0 -> 100% gato');
end;

// =============================================================================
//  Seccion 6 — Normalizacion (libro sec. 21.6 op. 5)
// =============================================================================
procedure SeccionNormalizacion(Conn: TAiEmbeddingConnection);
var
  V: TAiEmbeddingData;
begin
  Titulo('Sec. 21.6  Operacion 5 — Normalizacion');

  V := E(Conn, 'inteligencia artificial');
  Writeln(Format('  Magnitud antes de normalizar : %.6f', [TAiEmbeddingsCore.Magnitude(V)]));

  TAiEmbeddingsCore.Normalize(V);
  Writeln(Format('  Magnitud despues de normalizar: %.6f  (debe ser ~1.0)', [TAiEmbeddingsCore.Magnitude(V)]));
  Writeln;
  Writeln('  OpenAI devuelve vectores ya normalizados (magnitud ~1.0).');
  Writeln('  Ollama y otros pueden tener magnitudes distintas.');
end;

// =============================================================================
//  PROGRAMA PRINCIPAL
// =============================================================================
procedure RunDemo;
var
  Conn: TAiEmbeddingConnection;
begin
  Writeln('=== VectorOps — Operaciones algebraicas con embeddings ===');
  Writeln('Driver: ', DRIVER, ' | Modelo: ', MODEL);

  Conn := TAiEmbeddingConnection.Create(nil);
  try
    Conn.DriverName := DRIVER;
    Conn.Model      := MODEL;
    Conn.Url        := URL;

    SeccionMetricas(Conn);
    SeccionPromediar(Conn);
    SeccionSuma(Conn);
    SeccionAnalogia(Conn);
    SeccionInterpolacion(Conn);
    SeccionNormalizacion(Conn);

    Writeln;
    Writeln('=== Demo completado ===');
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
