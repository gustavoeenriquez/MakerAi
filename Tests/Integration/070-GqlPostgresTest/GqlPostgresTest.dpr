program GqlPostgresTest;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerGQL contra PostgreSQL real (TAiRagGraphPostgresDriver).
//
// Cada fase usa un TAiRagGraph NUEVO (memoria vacia) sobre las mismas tablas, asi
// lo que se lee viene de la base y lo que se escribe se comprueba con SQL directo:
//   FASE 1  CREATE desde MakerGQL -> filas, propiedades jsonb y embeddings en la base
//   FASE 2  lecturas sobre un grafo recien abierto (etiquetas, recorridos en los dos
//           sentidos, parametros, BETWEEN, agregados, OPTIONAL MATCH, EXISTS,
//           longitud variable, SIMILAR TO, SHORTEST PATH, MATCH (n) y SHOW SCHEMA)
//   FASE 3  escrituras sobre otro grafo recien abierto (SET propiedad y etiqueta,
//           MERGE, SET en relacion, REMOVE, DELETE de relacion, DETACH DELETE)
//   FASE 4  un grafo nuevo ve lo escrito en la fase 3
//
// Conexion: variables PGHOST, PGPORT, PGDATABASE, PGUSER, PGPASSWORD. Crea las tablas
// gqltest_nodes, gqltest_nodes_chunks y gqltest_edges (vector de 4) y las borra al
// final (--keep las deja). Embeddings falsos por palabras clave: no gasta.
// Codigo de salida 0 si todo pasa.
// =============================================================================

uses
  System.SysUtils,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Data.DB,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Def, FireDAC.Stan.Async,
  FireDAC.Phys.Intf, FireDAC.Phys, FireDAC.Phys.PG, FireDAC.Phys.PGDef,
  FireDAC.ConsoleUI.Wait, FireDAC.DApt, FireDAC.Comp.Client,
  uMakerAi.Embeddings.core,
  uMakerAi.RAG.Graph.Core,
  uMakerAi.RAG.Graph.Driver.Postgres;

const
  TABLE_BASE = 'gqltest_';
  ENTIDAD = 'GQLTEST';

type
  THandlers = class
    procedure KeywordEmbedding(Sender: TObject; const aInput, aUser, aModel, aEncodingFormat: String;
      aDimensions: Integer; var aEmbedding: TAiEmbeddingData);
  end;

var
  Conn: TFDConnection;
  PGLink: TFDPhysPgDriverLink;
  Handlers: THandlers;
  Passed, Failed: Integer;

procedure THandlers.KeywordEmbedding(Sender: TObject; const aInput, aUser, aModel, aEncodingFormat: String;
  aDimensions: Integer; var aEmbedding: TAiEmbeddingData);
var
  T: string;
begin
  T := LowerCase(aInput);
  SetLength(aEmbedding, 4);
  aEmbedding[0] := Ord(Pos('chocolate', T) > 0);
  aEmbedding[1] := Ord(Pos('frut', T) > 0);
  aEmbedding[2] := Ord(Pos('floral', T) > 0);
  aEmbedding[3] := 0.1;
end;

function FindLibPq: string;
var
  V: Integer;
begin
  for V := 18 downto 12 do
  begin
    Result := Format('C:\Program Files\PostgreSQL\%d\bin\libpq.dll', [V]);
    if FileExists(Result) then
      Exit;
  end;
  Result := '';
end;

procedure Check(const AName: string; ACond: Boolean; const ADetail: string = '');
begin
  if ACond then
  begin
    Inc(Passed);
    Writeln('  PASS  ', AName);
  end
  else
  begin
    Inc(Failed);
    Writeln('  FAIL  ', AName);
    if ADetail <> '' then
      Writeln('        ', ADetail.Replace(sLineBreak, sLineBreak + '        '));
  end;
end;

function Has(const AText, ASub: string): Boolean;
begin
  Result := Pos(ASub, AText) > 0;
end;

function Scalar(const ASQL: string): string;
var
  V: Variant;
begin
  V := Conn.ExecSQLScalar(ASQL);
  if VarIsNull(V) or VarIsEmpty(V) then
    Result := '<null>'
  else
    Result := VarToStr(V);
end;

function Tbl(const ASuffix: string): string;
begin
  Result := TABLE_BASE + ASuffix;
end;

// Un grafo nuevo (memoria vacia) con su propio driver sobre las mismas tablas
function NewGraph(out ADriver: TAiRagGraphPostgresDriver; out AEmb: TAiEmbeddingsCore): TAiRagGraph;
begin
  AEmb := TAiEmbeddingsCore.Create(nil);
  AEmb.Model := 'fake';
  AEmb.Dimensions := 4;
  AEmb.OnGetEmbedding := Handlers.KeywordEmbedding;
  ADriver := TAiRagGraphPostgresDriver.Create(nil);
  ADriver.Connection := Conn;
  ADriver.TableName := TABLE_BASE;
  ADriver.CurrentEntidad := ENTIDAD;
  Result := TAiRagGraph.Create(nil);
  Result.Embeddings := AEmb;
  Result.Driver := ADriver;
  Result.AllowWrite := True;
end;

procedure FreeGraph(G: TAiRagGraph; D: TAiRagGraphPostgresDriver; E: TAiEmbeddingsCore);
begin
  G.Free;
  D.Free;
  E.Free;
end;

// Ejecuta y devuelve el texto; un error queda como 'ERROR: ...'
function Q(G: TAiRagGraph; const ACode: string): string; overload;
begin
  try
    Result := G.ExecuteMakerGQL(ACode);
  except
    on E: Exception do
      Result := 'ERROR: ' + E.ClassName + ': ' + E.Message;
  end;
end;

function Q(G: TAiRagGraph; const ACode: string; const AParams: array of Variant): string; overload;
begin
  try
    Result := G.ExecuteMakerGQL(ACode, AParams);
  except
    on E: Exception do
      Result := 'ERROR: ' + E.ClassName + ': ' + E.Message;
  end;
end;

procedure Connect;
begin
  PGLink := TFDPhysPgDriverLink.Create(nil);
  PGLink.VendorLib := FindLibPq;
  Conn := TFDConnection.Create(nil);
  Conn.LoginPrompt := False;
  Conn.Params.DriverID := 'PG';
  Conn.Params.Values['Server'] := GetEnvironmentVariable('PGHOST');
  if GetEnvironmentVariable('PGPORT') <> '' then
    Conn.Params.Values['Port'] := GetEnvironmentVariable('PGPORT');
  Conn.Params.Database := GetEnvironmentVariable('PGDATABASE');
  Conn.Params.UserName := GetEnvironmentVariable('PGUSER');
  Conn.Params.Password := GetEnvironmentVariable('PGPASSWORD');
  Conn.Params.Values['CharacterSet'] := 'UTF8';
  Conn.Connected := True;
end;

procedure DropTables;
begin
  Conn.ExecSQL('DROP TABLE IF EXISTS public.' + Tbl('edges'));
  Conn.ExecSQL('DROP TABLE IF EXISTS public.' + Tbl('nodes_chunks'));
  Conn.ExecSQL('DROP TABLE IF EXISTS public.' + Tbl('nodes'));
end;

procedure CreateTables;
var
  D: TAiRagGraphPostgresDriver;
begin
  DropTables;
  D := TAiRagGraphPostgresDriver.Create(nil);
  try
    D.Connection := Conn;
    D.CreateSchema(TABLE_BASE, 4);
  finally
    D.Free;
  end;
end;

// -----------------------------------------------------------------------------
procedure Fase1_Create;
var
  G: TAiRagGraph;
  D: TAiRagGraphPostgresDriver;
  E: TAiEmbeddingsCore;
  R: string;
begin
  Writeln('FASE 1: CREATE desde MakerGQL');
  G := NewGraph(D, E);
  try
    R := Q(G, 'CREATE (:Producto {id:''p1'', name:''Huila Reserva'', precio:42000, origen:''Huila'', notas:''chocolate y panela''}), ' +
      '(:Producto {id:''p2'', name:''Narino Alto'', precio:38000, origen:''Narino'', notas:''frutos rojos''}), ' +
      '(:Producto {id:''p3'', name:''Tolima Suave'', precio:25000, origen:''Tolima'', notas:''floral''}), ' +
      '(:Acidez {id:''a1'', name:''Alta''}), (:Acidez {id:''a2'', name:''Baja''}), ' +
      '(:Persona {id:''u1'', name:''Ana''}), (:Persona {id:''u2'', name:''Luis''})');
    Check('CREATE de 7 nodos', Has(R, 'Nodes created: 7'), R);

    R := Q(G, 'MATCH (c:Producto), (a:Acidez) WHERE (c.name = ''Huila Reserva'' OR c.name = ''Narino Alto'') AND a.name = ''Alta'' ' +
      'CREATE (c)-[:TIENE_ACIDEZ]->(a)');
    Check('CREATE de 2 relaciones TIENE_ACIDEZ (Alta)', Has(R, 'relationships created: 2'), R);
    R := Q(G, 'MATCH (c:Producto {name:''Tolima Suave''}), (a:Acidez {name:''Baja''}) CREATE (c)-[:TIENE_ACIDEZ]->(a)');
    Check('CREATE de 1 relacion TIENE_ACIDEZ (Baja)', Has(R, 'relationships created: 1'), R);
    R := Q(G, 'MATCH (u:Persona {name:''Ana''}), (c:Producto {name:''Huila Reserva''}) CREATE (u)-[:COMPRO {cantidad:2}]->(c)');
    Check('CREATE Ana-COMPRO->Huila', Has(R, 'relationships created: 1'), R);
    R := Q(G, 'MATCH (u:Persona {name:''Ana''}), (c:Producto {name:''Tolima Suave''}) CREATE (u)-[:COMPRO {cantidad:1}]->(c)');
    Check('CREATE Ana-COMPRO->Tolima', Has(R, 'relationships created: 1'), R);
    R := Q(G, 'MATCH (u:Persona {name:''Luis''}), (c:Producto {name:''Huila Reserva''}) CREATE (u)-[:COMPRO {cantidad:3}]->(c)');
    Check('CREATE Luis-COMPRO->Huila', Has(R, 'relationships created: 1'), R);
    R := Q(G, 'MATCH (a:Persona {name:''Ana''}), (b:Persona {name:''Luis''}) CREATE (a)-[:AMIGO_DE]->(b)');
    Check('CREATE Ana-AMIGO_DE->Luis', Has(R, 'relationships created: 1'), R);
  finally
    FreeGraph(G, D, E);
  end;

  // Lo que quedo en la base
  R := Scalar(Format('SELECT count(*) FROM %s WHERE entidad = ''%s''', [Tbl('nodes'), ENTIDAD]));
  Check('SQL: 7 filas de nodos', R = '7', 'count = ' + R);
  R := Scalar(Format('SELECT count(*) FROM %s WHERE entidad = ''%s''', [Tbl('edges'), ENTIDAD]));
  Check('SQL: 7 filas de relaciones', R = '7', 'count = ' + R);
  R := Scalar(Format('SELECT (properties->>''precio'')::numeric FROM %s WHERE entidad = ''%s'' AND id = ''p1''', [Tbl('nodes'), ENTIDAD]));
  Check('SQL: p1.precio = 42000 en el jsonb', R = '42000', 'precio = ' + R);
  R := Scalar(Format('SELECT count(*) FROM %s WHERE entidad = ''%s'' AND embedding IS NULL', [Tbl('nodes'), ENTIDAD]));
  Check('SQL: todos los nodos con embedding', R = '0', 'sin embedding = ' + R);
  R := Scalar(Format('SELECT (properties->>''cantidad'')::numeric FROM %s e WHERE entidad = ''%s'' AND source_node_id = ''u2''', [Tbl('edges'), ENTIDAD]));
  Check('SQL: Luis-COMPRO.cantidad = 3', R = '3', 'cantidad = ' + R);
end;

// -----------------------------------------------------------------------------
procedure Fase2_Lecturas;
var
  G: TAiRagGraph;
  D: TAiRagGraphPostgresDriver;
  E: TAiEmbeddingsCore;
  R: string;
begin
  Writeln('FASE 2: lecturas sobre un grafo recien abierto');

  // Antes de cargar nada: lo que no tiene etiqueta y el esquema
  G := NewGraph(D, E);
  try
    R := Q(G, 'MATCH (n) RETURN count(n) AS total');
    Check('MATCH (n) sin etiqueta cuenta los 7 nodos de la base', Has(R, '| 7 '), R);
  finally
    FreeGraph(G, D, E);
  end;
  G := NewGraph(D, E);
  try
    R := Q(G, 'SHOW SCHEMA');
    Check('SHOW SCHEMA en un grafo recien abierto (contado en la base)',
      Has(R, '- Producto (3)') and Has(R, '- Persona (2)') and Has(R, '(:Persona)-[:COMPRO]->(:Producto) (3)') and
      Has(R, '(:Producto)-[:TIENE_ACIDEZ]->(:Acidez) (3)'), R);
    Check('SHOW SCHEMA no cargo el grafo en memoria', G.NodeCount = 0, 'NodeCount = ' + IntToStr(G.NodeCount));
    R := Q(G, 'SHOW PROPERTIES OF producto');
    Check('SHOW PROPERTIES OF producto (sin distinguir mayusculas)', Has(R, 'precio (3)') and Has(R, 'origen (3)') and
      not Has(R, 'cantidad'), R);
    R := Q(G, 'SHOW PROPERTIES');
    Check('SHOW PROPERTIES: Acidez sin propiedades y COMPRO con cantidad', Has(R, '- :Acidez: (none)') and
      Has(R, '- [:COMPRO]: cantidad (3)'), R);
  finally
    FreeGraph(G, D, E);
  end;
  G := NewGraph(D, E);
  try
    R := Q(G, 'GET COMMUNITIES');
    Check('GET COMMUNITIES en un grafo recien abierto ve los 7 nodos', Has(R, 'Huila Reserva') and Has(R, 'Narino Alto') and
      Has(R, 'Tolima Suave') and Has(R, 'Alta') and Has(R, 'Baja') and Has(R, 'Ana') and Has(R, 'Luis'), R);
  finally
    FreeGraph(G, D, E);
  end;

  G := NewGraph(D, E);
  try
    R := Q(G, 'MATCH (c:Producto)-[:TIENE_ACIDEZ]->(a:Acidez) WHERE a.name = $nivel RETURN c.name ORDER BY c.name', ['nivel', 'Alta']);
    Check('Recorrido hacia adelante con $parametro', Has(R, 'Huila Reserva') and Has(R, 'Narino Alto') and not Has(R, 'Tolima'), R);

    R := Q(G, 'MATCH (c:Producto) WHERE c.precio BETWEEN 30000 AND 45000 RETURN count(c) AS n');
    Check('BETWEEN sobre un numero del jsonb', Has(R, '| 2 '), R);

    R := Q(G, 'MATCH (c:Producto) WHERE c.precio > 30000 RETURN sum(c.precio) AS total');
    Check('sum() sobre numeros del jsonb', Has(R, '80000'), R);

    R := Q(G, 'MATCH (u:Persona)-[:COMPRO]->(c:Producto) RETURN u.name AS persona, count(c) AS compras ORDER BY compras DESC, persona');
    Check('Agregado por persona', Has(R, '| Ana | 2 ') and Has(R, '| Luis | 1 '), R);
  finally
    FreeGraph(G, D, E);
  end;

  // Recorrido hacia atras: las relaciones entrantes de Huila solo estan en la base
  G := NewGraph(D, E);
  try
    R := Q(G, 'MATCH (c:Producto {name:''Huila Reserva''})<-[r:COMPRO]-(u) RETURN u.name AS quien, r.cantidad AS cant ORDER BY quien');
    Check('Recorrido hacia atras desde la base (con propiedad de la relacion)', Has(R, '| Ana | 2 ') and Has(R, '| Luis | 3 '), R);
  finally
    FreeGraph(G, D, E);
  end;

  G := NewGraph(D, E);
  try
    R := Q(G, 'MATCH (c:Producto) OPTIONAL MATCH (u:Persona)-[:COMPRO]->(c) RETURN c.name AS cafe, count(u) AS n ORDER BY cafe');
    Check('OPTIONAL MATCH con conteo (Narino sin compras)',
      Has(R, '| Huila Reserva | 2 ') and Has(R, '| Narino Alto | 0 ') and Has(R, '| Tolima Suave | 1 '), R);

    R := Q(G, 'MATCH (c:Producto) WHERE NOT EXISTS { (:Persona)-[:COMPRO]->(c) } RETURN c.name');
    Check('NOT EXISTS { patron }', Has(R, 'Narino Alto') and not Has(R, 'Huila') and not Has(R, 'Tolima'), R);

    R := Q(G, 'MATCH (a:Persona {name:''Ana''})-[:AMIGO_DE|COMPRO*1..2]->(x) RETURN DISTINCT x.name AS nombre ORDER BY nombre');
    Check('Longitud variable con tipos alternativos',
      Has(R, '| Luis ') and Has(R, '| Huila Reserva ') and Has(R, '| Tolima Suave '), R);

    R := Q(G, 'MATCH (c:Producto) WHERE c SIMILAR TO ''un cafe con notas de chocolate'' RETURN c.name');
    Check('SIMILAR TO con el embedding leido de la base', Has(R, 'Huila Reserva') and not Has(R, 'Narino'), R);

    R := Q(G, 'SHORTEST PATH (:Persona {name:''Luis''}) TO (:Acidez {name:''Alta''})');
    Check('SHORTEST PATH', Has(R, 'Luis') and Has(R, 'Huila Reserva') and Has(R, 'Alta'), R);
  finally
    FreeGraph(G, D, E);
  end;
end;

// -----------------------------------------------------------------------------
procedure Fase3_Escrituras;
var
  G: TAiRagGraph;
  D: TAiRagGraphPostgresDriver;
  E: TAiEmbeddingsCore;
  R, R2: string;
begin
  Writeln('FASE 3: escrituras sobre un grafo recien abierto');
  G := NewGraph(D, E);
  try
    R := Q(G, 'MATCH (c:Producto {name:''Tolima Suave''}) SET c.precio = $p', ['p', 27000]);
    Check('SET propiedad', Has(R, 'properties set: 1'), R);
    R := Scalar(Format('SELECT (properties->>''precio'')::numeric FROM %s WHERE entidad = ''%s'' AND id = ''p3''', [Tbl('nodes'), ENTIDAD]));
    Check('SQL: p3.precio = 27000', R = '27000', 'precio = ' + R);

    R := Q(G, 'MATCH (c:Producto {name:''Narino Alto''}) SET c:Agotado');
    Check('SET etiqueta', Has(R, 'labels set: 1'), R);
    R := Scalar(Format('SELECT node_label FROM %s WHERE entidad = ''%s'' AND id = ''p2''', [Tbl('nodes'), ENTIDAD]));
    Check('SQL: p2.node_label = Agotado', R = 'Agotado', 'node_label = ' + R);

    R := Q(G, 'MATCH (c:Producto {name:''Huila Reserva''}) SET c.name = ''Huila Reserva Especial''');
    R := Scalar(Format('SELECT name FROM %s WHERE entidad = ''%s'' AND id = ''p1''', [Tbl('nodes'), ENTIDAD]));
    Check('SQL: SET c.name cambia el nombre', R = 'Huila Reserva Especial', 'name = ' + R);

    R := Q(G, 'MATCH (c:Producto {name:''Tolima Suave''}) REMOVE c.origen');
    R := Scalar(Format('SELECT coalesce(properties->>''origen'', ''<sin origen>'') FROM %s WHERE entidad = ''%s'' AND id = ''p3''', [Tbl('nodes'), ENTIDAD]));
    Check('SQL: REMOVE quita la propiedad', R = '<sin origen>', 'origen = ' + R);

    R2 := Q(G, 'MATCH (u:Persona {name:''Ana''})-[r:COMPRO]->(c:Producto {name:''Tolima Suave''}) SET r.cantidad = 5');
    R := Scalar(Format('SELECT (properties->>''cantidad'')::numeric FROM %s WHERE entidad = ''%s'' AND source_node_id = ''u1'' AND target_node_id = ''p3''', [Tbl('edges'), ENTIDAD]));
    Check('SQL: SET en una relacion', R = '5', 'cantidad = ' + R + sLineBreak + R2);

    R := Q(G, 'MERGE (a:Acidez {name:''Media''}) ON CREATE SET a.orden = 2');
    Check('MERGE que crea', Has(R, 'Nodes created: 1'), R);

    R := Q(G, 'MATCH (u:Persona {name:''Luis''}) DELETE u');
    Check('DELETE sin DETACH de un nodo con relaciones es error', R.StartsWith('ERROR'), R);

    R := Q(G, 'MATCH (u:Persona {name:''Ana''})-[r:COMPRO]->(c:Producto {name:''Tolima Suave''}) DELETE r');
    Check('DELETE de una relacion', Has(R, 'relationships deleted: 1'), R);
    R := Scalar(Format('SELECT count(*) FROM %s WHERE entidad = ''%s'' AND source_node_id = ''u1'' AND target_node_id = ''p3''', [Tbl('edges'), ENTIDAD]));
    Check('SQL: la relacion borrada ya no esta', R = '0', 'count = ' + R);
  finally
    FreeGraph(G, D, E);
  end;

  // MERGE desde otro grafo recien abierto: debe encontrar el nodo en la base
  G := NewGraph(D, E);
  try
    R := Q(G, 'MERGE (a:Acidez {name:''Media''}) ON MATCH SET a.orden = 3');
    Check('MERGE en un grafo recien abierto encuentra el nodo de la base', Has(R, 'Nodes created: 0'), R);
    R := Scalar(Format('SELECT count(*) FROM %s WHERE entidad = ''%s'' AND name = ''Media''', [Tbl('nodes'), ENTIDAD]));
    Check('SQL: un solo nodo Media', R = '1', 'count = ' + R);
    R := Scalar(Format('SELECT (properties->>''orden'')::numeric FROM %s WHERE entidad = ''%s'' AND name = ''Media''', [Tbl('nodes'), ENTIDAD]));
    Check('SQL: ON MATCH SET guardado', R = '3', 'orden = ' + R);
  finally
    FreeGraph(G, D, E);
  end;

  // DETACH DELETE desde un grafo recien abierto: las relaciones de Luis solo estan en la base
  G := NewGraph(D, E);
  try
    R := Q(G, 'MATCH (u:Persona {name:''Luis''}) DETACH DELETE u');
    Check('DETACH DELETE', Has(R, 'nodes deleted: 1') and Has(R, 'relationships deleted: 2'), R);
  finally
    FreeGraph(G, D, E);
  end;
  R := Scalar(Format('SELECT count(*) FROM %s WHERE entidad = ''%s'' AND id = ''u2''', [Tbl('nodes'), ENTIDAD]));
  Check('SQL: Luis ya no esta', R = '0', 'count = ' + R);
  R := Scalar(Format('SELECT count(*) FROM %s WHERE entidad = ''%s'' AND (source_node_id = ''u2'' OR target_node_id = ''u2'')', [Tbl('edges'), ENTIDAD]));
  Check('SQL: sin relaciones de Luis', R = '0', 'count = ' + R);
end;

// -----------------------------------------------------------------------------
procedure Fase4_Relectura;
var
  G: TAiRagGraph;
  D: TAiRagGraphPostgresDriver;
  E: TAiEmbeddingsCore;
  R: string;
begin
  Writeln('FASE 4: un grafo nuevo ve lo escrito');
  G := NewGraph(D, E);
  try
    R := Q(G, 'MATCH (c:Producto {name:''Tolima Suave''}) RETURN c.precio AS precio');
    Check('Precio nuevo de Tolima', Has(R, '27000'), R);
    R := Q(G, 'MATCH (c:Agotado) RETURN c.name');
    Check('Narino con la etiqueta Agotado', Has(R, 'Narino Alto'), R);
    R := Q(G, 'MATCH (c:Producto) RETURN count(c) AS n');
    Check('Quedan 2 Producto (Narino paso a Agotado)', Has(R, '| 2 '), R);
    R := Q(G, 'MATCH (u:Persona) RETURN count(u) AS n');
    Check('Queda 1 Persona', Has(R, '| 1 '), R);
    R := Q(G, 'MATCH (c:Producto) WHERE c SIMILAR TO ''notas de chocolate'' RETURN c.name');
    Check('SIMILAR TO con el embedding recalculado tras el SET', Has(R, 'Huila Reserva Especial'), R);
  finally
    FreeGraph(G, D, E);
  end;
end;

var
  Keep: Boolean;
begin
  ReportMemoryLeaksOnShutdown := False;
  Keep := FindCmdLineSwitch('keep', ['-', '/'], True);
  Handlers := THandlers.Create;
  try
    try
      Connect;
      Writeln('Conectado a ', Conn.Params.Database, ' en ', Conn.Params.Values['Server']);
      CreateTables;
      try
        Fase1_Create;
        Fase2_Lecturas;
        Fase3_Escrituras;
        Fase4_Relectura;
      finally
        if not Keep then
          DropTables;
      end;
    except
      on E: Exception do
      begin
        Inc(Failed);
        Writeln('ABORTADO: ', E.ClassName, ': ', E.Message);
      end;
    end;
    Writeln;
    Writeln(Format('Resultado: %d pasan, %d fallan', [Passed, Failed]));
  finally
    Conn.Free;
    PGLink.Free;
    Handlers.Free;
  end;
  if Failed > 0 then
    ExitCode := 1;
end.
