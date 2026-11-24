program VqlSQLiteTest;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// VQL contra SQLite (TAiRAGVectorSQLiteDriver): la misma consulta debe dar el mismo
// resultado en memoria y en la base. Mismas consultas que 071 (PostgreSQL).
//
// Carga los mismos documentos en un TAiRAGVector en memoria y en otro con el driver de
// SQLite, corre cada consulta en los dos y compara los autores encontrados (los
// embeddings son todos iguales, asi que decide el WHERE). Ademas, algunas consultas
// tienen el resultado esperado escrito, para que no puedan estar mal las dos a la vez.
//
// Base: un archivo vqltest.db en la carpeta temporal (SQLite enlazado en el exe, no hace
// falta instalar nada). Lo borra al final (-keep lo deja). Sin API keys. Exit 0 si todo pasa.
// =============================================================================

uses
  System.SysUtils,
  System.Classes,
  System.Variants,
  System.DateUtils,
  System.IOUtils,
  Data.DB,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Def, FireDAC.Stan.Async,
  FireDAC.Phys.Intf, FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.Stan.ExprFuncs,
  FireDAC.ConsoleUI.Wait, FireDAC.DApt, FireDAC.Comp.Client,
  uMakerAi.Embeddings.core,
  uMakerAi.RAG.MetaData,
  uMakerAi.RAG.Vectors,
  uMakerAi.RAG.Vectors.Index,
  uMakerAi.RAG.Vector.Driver.SQLite;

const
  TABLE_NAME = 'vqltest';
  ENTIDAD = 'VQLTEST';

type
  THandlers = class
    procedure Emb(Sender: TObject; const aInput, aUser, aModel, aEncodingFormat: String;
      aDimensions: Integer; var aEmbedding: TAiEmbeddingData);
  end;

var
  Conn: TFDConnection;
  DbFile: string;
  Handlers: THandlers;
  Emb: TAiEmbeddingsCore;
  Mem, Lite: TAiRAGVector;
  Drv: TAiRAGVectorSQLiteDriver;
  Passed, Failed: Integer;

procedure THandlers.Emb(Sender: TObject; const aInput, aUser, aModel, aEncodingFormat: String;
  aDimensions: Integer; var aEmbedding: TAiEmbeddingData);
begin
  SetLength(aEmbedding, 4);
  aEmbedding[0] := 0.5;
  aEmbedding[1] := 0.5;
  aEmbedding[2] := 0.5;
  aEmbedding[3] := 0.5;
end;

procedure Connect;
begin
  DbFile := TPath.Combine(TPath.GetTempPath, 'vqltest.db');
  if TFile.Exists(DbFile) then
    TFile.Delete(DbFile);
  Conn := TFDConnection.Create(nil);
  Conn.LoginPrompt := False;
  Conn.Params.DriverID := 'SQLite';
  Conn.Params.Database := DbFile;
  Conn.Params.Values['StringFormat'] := 'Unicode';
  Conn.Connected := True;
end;

// Un documento en los dos vectores. Los pares nombre/valor van a los metadatos
procedure AddDoc(const AText: string; const AProps: array of Variant);
var
  M: TAiEmbeddingMetaData;
  I: Integer;
  N: TAiEmbeddingNode;
begin
  M := TAiEmbeddingMetaData.Create;
  try
    I := 0;
    while I < High(AProps) do
    begin
      M[VarToStr(AProps[I])] := AProps[I + 1];
      Inc(I, 2);
    end;
    Mem.AddItem(AText, M);
    N := Lite.AddItem(AText, M);
    // Con driver, AddItem guarda en la base y el nodo queda del que llama
    if Assigned(N) and (Lite.Items.IndexOf(N) < 0) then
      N.Free;
  finally
    M.Free;
  end;
end;

function Names(V: TAiRAGVector; const AVql: string; const AParams: array of Variant): string;
var
  R: TAiRAGVector;
  L: TStringList;
  I: Integer;
begin
  R := nil;
  L := TStringList.Create;
  try
    try
      V.ExecuteVQL(AVql, AParams, R);
      if Assigned(R) then
        for I := 0 to R.Count - 1 do
          L.Add(VarToStr(R.Items[I].MetaData['autor']));
      L.Sort;
      L.StrictDelimiter := True;
      Result := '[' + L.CommaText + ']';
    except
      on E: Exception do
        Result := 'ERROR ' + E.Message.Split([#10, #13])[0];
    end;
  finally
    L.Free;
    R.Free;
  end;
end;

// Misma consulta en memoria y en la base; AExpected <> '' tambien fija el resultado
procedure Check(const AVql: string; const AParams: array of Variant; const AExpected: string = '');
var
  RM, RP: string;
  Ok: Boolean;
begin
  RM := Names(Mem, AVql, AParams);
  RP := Names(Lite, AVql, AParams);
  Ok := (RM = RP) and ((AExpected = '') or (RM = AExpected));
  if Ok then
  begin
    Inc(Passed);
    Writeln('  PASS  ', AVql, '  ', RM);
  end
  else
  begin
    Inc(Failed);
    Writeln('  FAIL  ', AVql);
    Writeln('        memoria:    ', RM);
    Writeln('        sqlite:     ', RP);
    if AExpected <> '' then
      Writeln('        esperado:   ', AExpected);
  end;
end;

procedure CheckLite(const AVql, AExpected: string);
var
  R: string;
begin
  R := Names(Lite, AVql, []);
  if R = AExpected then
  begin
    Inc(Passed);
    Writeln('  PASS  ', AVql, '  ', R);
  end
  else
  begin
    Inc(Failed);
    Writeln('  FAIL  ', AVql);
    Writeln('        sqlite:     ', R);
    Writeln('        esperado:   ', AExpected);
  end;
end;

procedure Run;
begin
  // Datos: numeros (enteros y decimales), numeros en texto, mayusculas, listas, booleanos,
  // fechas, claves que faltan, un precio que no es numero y una ruta con barras
  AddDoc('uno', ['autor', 'Ana', 'estado', 'activo', 'precio', 100, 'codigo', '10', 'tags', VarArrayOf(['cafe', 'te']),
    'destacado', True, 'fecha', EncodeDate(2026, 1, 15), 'nota', 'Café de altura',
    'medida', '1e3', 'patron', 'a*b']);
  AddDoc('dos', ['autor', 'Bruno', 'estado', 'borrador', 'precio', 200, 'codigo', '9', 'tags', VarArrayOf(['mate']),
    'destacado', False, 'fecha', EncodeDate(2025, 12, 1), 'nota', 'Notas frutales',
    'medida', ' 42 ', 'patron', 'x[1]']);
  AddDoc('tres', ['autor', 'carla', 'estado', 'activo', 'precio', 300.5, 'codigo', '25', 'tags', VarArrayOf(['te']),
    'destacado', True, 'fecha', EncodeDate(2026, 3, 1), 'medida', '.5', 'patron', '50%']);
  AddDoc('cuatro', ['autor', 'Dario', 'precio', 150.75, 'medida', '1.2.3', 'patron', 'x1']);
  AddDoc('cinco', ['autor', 'O''Brien', 'estado', 'Activo', 'precio', -5, 'codigo', '-3', 'ruta', 'C:\docs\nuevo',
    'medida', '2026-11-24']);
  AddDoc('seis', ['autor', 'Fede', 'estado', 'activo', 'precio', 'N/A', 'codigo', 'abc', 'medida', 'A-10']);

  Writeln('Igualdad, mayusculas y listas');
  Check('SEARCH ''x''', [], '[Ana,Bruno,carla,Dario,Fede,O''Brien]');
  Check('SEARCH ''x'' WHERE estado = ''activo''', [], '[Ana,carla,Fede]');
  Check('SEARCH ''x'' WHERE estado <> ''activo''', [], '[Bruno,O''Brien]');
  Check('SEARCH ''x'' WHERE autor IN (''ana'', ''Bruno'')', [], '[Bruno]');
  Check('SEARCH ''x'' WHERE autor NOT IN (''Ana'', ''Bruno'')', []);
  Check('SEARCH ''x'' WHERE autor = ''O''''Brien''', [], '[O''Brien]');
  Check('SEARCH ''x'' WHERE ruta = ''C:\docs\nuevo''', [], '[O''Brien]');
  Check('SEARCH ''x'' WHERE destacado = true', [], '[Ana,carla]');
  Check('SEARCH ''x'' WHERE destacado = false', [], '[Bruno]');

  Writeln('Numeros');
  Check('SEARCH ''x'' WHERE precio > 150', [], '[Bruno,carla,Dario]');
  Check('SEARCH ''x'' WHERE precio >= 100 AND precio <= 200', []);
  Check('SEARCH ''x'' WHERE precio = 300.5', [], '[carla]');
  Check('SEARCH ''x'' WHERE precio < 0', [], '[O''Brien]');
  Check('SEARCH ''x'' WHERE precio BETWEEN 150 AND 250', [], '[Bruno,Dario]');
  Check('SEARCH ''x'' WHERE precio NOT BETWEEN 150 AND 250', []);
  Check('SEARCH ''x'' WHERE codigo > 9', [], '[Ana,carla]');
  Check('SEARCH ''x'' WHERE codigo > ''9''', []);
  Check('SEARCH ''x'' WHERE codigo IN (10, 25)', [], '[Ana,carla]');
  Check('SEARCH ''x'' WHERE precio = ''N/A''', []);

  Writeln('Texto');
  Check('SEARCH ''x'' WHERE autor >= ''B''', []);
  Check('SEARCH ''x'' WHERE autor < ''carla''', []);
  Check('SEARCH ''x'' WHERE autor LIKE ''a%''', [], '[]');
  Check('SEARCH ''x'' WHERE autor ILIKE ''a%''', [], '[Ana]');
  Check('SEARCH ''x'' WHERE autor LIKE ''_runo''', [], '[Bruno]');
  Check('SEARCH ''x'' WHERE autor LIKE ''O''''%''', [], '[O''Brien]');
  Check('SEARCH ''x'' WHERE nota CONTAINS ''ALTURA''', [], '[Ana]');
  Check('SEARCH ''x'' WHERE nota CONTAINS ''%''', [], '[]');
  Check('SEARCH ''x'' WHERE estado STARTS_WITH ''ACT''', [], '[Ana,carla,Fede,O''Brien]');
  Check('SEARCH ''x'' WHERE autor ENDS_WITH ''O''', [], '[Bruno,Dario]');
  Check('SEARCH ''x'' WHERE text CONTAINS ''tre''', [], '[carla]');

  Writeln('Listas (tags)');
  Check('SEARCH ''x'' WHERE tags CONTAINS ''te''', [], '[Ana,carla]');
  Check('SEARCH ''x'' WHERE tags EXISTS_ANY (''te'', ''mate'')', [], '[Ana,Bruno,carla]');
  Check('SEARCH ''x'' WHERE tags EXISTS_ALL (''cafe'', ''te'')', [], '[Ana]');
  Check('SEARCH ''x'' WHERE tags NOT EXISTS_ANY (''mate'')', [], '[Ana,carla]');

  Writeln('Existencia y NOT');
  Check('SEARCH ''x'' WHERE estado IS NULL', [], '[Dario]');
  Check('SEARCH ''x'' WHERE codigo IS NOT NULL', []);
  Check('SEARCH ''x'' WHERE EXISTS(ruta)', [], '[O''Brien]');
  Check('SEARCH ''x'' WHERE NOT EXISTS(estado)', [], '[Dario]');
  Check('SEARCH ''x'' WHERE estado NOT LIKE ''b%''', [], '[Ana,carla,Fede,O''Brien]');
  Check('SEARCH ''x'' WHERE estado NOT CONTAINS ''act''', [], '[Bruno]');
  Check('SEARCH ''x'' WHERE NOT (estado = ''activo'' OR precio > 250)', []);
  Check('SEARCH ''x'' WHERE NOT estado = ''activo'' AND precio < 300', []);

  Writeln('Fechas');
  Check('SEARCH ''x'' WHERE fecha > ''2026-01-01''', [], '[Ana,carla]');
  Check('SEARCH ''x'' WHERE fecha > $d', ['d', EncodeDate(2026, 1, 31)], '[carla]');
  Check('SEARCH ''x'' WHERE fecha BETWEEN ''2025-11-01'' AND ''2026-01-31''', []);

  Writeln('Casos de borde: numeros escritos como texto y comodines');
  Check('SEARCH ''x'' WHERE medida > 40', [], '[Ana,Bruno]');
  Check('SEARCH ''x'' WHERE medida < 1', [], '[carla]');
  Check('SEARCH ''x'' WHERE medida = 42', [], '[Bruno]');
  Check('SEARCH ''x'' WHERE medida IN (''42'')', [], '[Bruno]');
  Check('SEARCH ''x'' WHERE medida > ''2026-01-01''', []);
  Check('SEARCH ''x'' WHERE patron LIKE ''a*b''', [], '[Ana]');
  Check('SEARCH ''x'' WHERE patron LIKE ''x[1]''', [], '[Bruno]');
  Check('SEARCH ''x'' WHERE patron LIKE ''x_''', [], '[Dario]');
  Check('SEARCH ''x'' WHERE patron LIKE ''%\%''', [], '[carla]');
  Check('SEARCH ''x'' WHERE patron CONTAINS ''[''', [], '[Bruno]');

  // Busqueda lexica (FTS5, JOIN con la tabla): solo en SQLite, contra el resultado escrito.
  // Antes 'text' en el WHERE daba "ambiguous column name: content"
  Writeln('FTS5 (USING BM25 / HYBRID) con filtro');
  CheckLite('SEARCH ''tres'' USING BM25 WHERE text CONTAINS ''tre''', '[carla]');
  CheckLite('SEARCH ''tres'' USING BM25 WHERE estado = ''borrador''', '[]');
  CheckLite('SEARCH ''tres'' USING HYBRID WHERE tags CONTAINS ''te'' AND precio > 200', '[carla]');

  Writeln('Parametros');
  Check('SEARCH $q WHERE autor = $a', ['q', 'x', 'a', 'Ana'' OR ''1''=''1'], '[]');
  Check('SEARCH ''x'' WHERE autor IN $l', ['l', VarArrayOf(['Ana', 'carla'])], '[Ana,carla]');
  Check('SEARCH ''x'' WHERE precio > $p AND estado = $e', ['p', 99.5, 'e', 'activo'], '[Ana,carla]');
end;

var
  Keep: Boolean;
begin
  Keep := FindCmdLineSwitch('keep', ['-', '/'], True);
  Handlers := THandlers.Create;
  Emb := TAiEmbeddingsCore.Create(nil);
  Mem := TAiRAGVector.Create(nil);
  Lite := TAiRAGVector.Create(nil);
  Drv := TAiRAGVectorSQLiteDriver.Create(nil);
  try
    try
      Emb.Model := 'fake';
      Emb.Dimensions := 4;
      Emb.OnGetEmbedding := Handlers.Emb;
      Mem.InMemoryIndexType := TAIBasicIndex;
      Mem.Embeddings := Emb;

      Connect;
      Writeln('SQLite ', DbFile);
      Drv.Connection := Conn;
      Drv.CreateSchema(TABLE_NAME);
      Drv.CurrentEntidad := ENTIDAD;
      Lite.Embeddings := Emb;
      Lite.Entidad := ENTIDAD;
      Lite.Driver := Drv;
      try
        Run;
      finally
        if not Keep then
        begin
          Conn.Connected := False;
          TFile.Delete(DbFile);
        end;
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
    Lite.Free;
    Mem.Free;
    Drv.Free;
    Emb.Free;
    Conn.Free;
    Handlers.Free;
  end;
  if Failed > 0 then
    ExitCode := 1;
end.
