program VqlPostgresTest;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// VQL contra PostgreSQL real (TAiRAGVectorPostgresDriver): la misma consulta debe
// dar el mismo resultado en memoria y en la base.
//
// Carga los mismos documentos en un TAiRAGVector en memoria y en otro con el driver de
// PostgreSQL, corre cada consulta en los dos y compara los autores encontrados (los
// embeddings son todos iguales, asi que decide el WHERE). Ademas, algunas consultas
// tienen el resultado esperado escrito, para que no puedan estar mal las dos a la vez.
//
// Conexion: variables PGHOST, PGPORT, PGDATABASE, PGUSER, PGPASSWORD. Crea la tabla
// vqltest y la borra al final (-keep la deja). Sin API keys. Exit 0 si todo pasa.
// =============================================================================

uses
  System.SysUtils,
  System.Classes,
  System.Variants,
  System.DateUtils,
  Data.DB,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Def, FireDAC.Stan.Async,
  FireDAC.Phys.Intf, FireDAC.Phys, FireDAC.Phys.PG, FireDAC.Phys.PGDef,
  FireDAC.ConsoleUI.Wait, FireDAC.DApt, FireDAC.Comp.Client,
  uMakerAi.Embeddings.core,
  uMakerAi.RAG.MetaData,
  uMakerAi.RAG.Vectors,
  uMakerAi.RAG.Vectors.Index,
  uMakerAi.RAG.Vector.Driver.Postgres;

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
  PGLink: TFDPhysPgDriverLink;
  Handlers: THandlers;
  Emb: TAiEmbeddingsCore;
  Mem, Pg: TAiRAGVector;
  Drv: TAiRAGVectorPostgresDriver;
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
    N := Pg.AddItem(AText, M);
    // Con driver, AddItem guarda en la base y el nodo queda del que llama
    if Assigned(N) and (Pg.Items.IndexOf(N) < 0) then
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
  RP := Names(Pg, AVql, AParams);
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
    Writeln('        postgresql: ', RP);
    if AExpected <> '' then
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
  Pg := TAiRAGVector.Create(nil);
  Drv := TAiRAGVectorPostgresDriver.Create(nil);
  try
    try
      Emb.Model := 'fake';
      Emb.Dimensions := 4;
      Emb.OnGetEmbedding := Handlers.Emb;
      Mem.InMemoryIndexType := TAIBasicIndex;
      Mem.Embeddings := Emb;

      Connect;
      Writeln('Conectado a ', Conn.Params.Database, ' en ', Conn.Params.Values['Server']);
      Conn.ExecSQL('DROP TABLE IF EXISTS public.' + TABLE_NAME);
      Drv.Connection := Conn;
      Drv.CreateSchema(TABLE_NAME, 4);
      Drv.CurrentEntidad := ENTIDAD;
      Pg.Embeddings := Emb;
      Pg.Entidad := ENTIDAD;
      Pg.Driver := Drv;
      try
        Run;
      finally
        if not Keep then
          Conn.ExecSQL('DROP TABLE IF EXISTS public.' + TABLE_NAME);
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
    Pg.Free;
    Mem.Free;
    Drv.Free;
    Emb.Free;
    Conn.Free;
    PGLink.Free;
    Handlers.Free;
  end;
  if Failed > 0 then
    ExitCode := 1;
end.
