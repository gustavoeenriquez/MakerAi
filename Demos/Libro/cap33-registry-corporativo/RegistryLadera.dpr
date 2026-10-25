program RegistryLadera;

// =============================================================================
// LIBRO "IA con Delphi" - capitulo 33: el registry corporativo, desde Delphi
// =============================================================================
// Un registry PPM propio (PPMServer + PostgreSQL) corriendo en la red de la empresa; aqui, en
// esta misma maquina en el puerto 8766 (ver registro-local\LEEME.md). Lo que cambia en el codigo
// es UNA propiedad: PPMRegistryUrl (o el parametro ARegistryUrl de LoadFromPPM).
//   --salud      GET /v1/health del registry corporativo (y del publico, para comparar)
//   --prompt     TAiPrompts contra el corporativo: SearchPPM, LoadFromPPM('lucia-barista') y una
//                respuesta real de gpt-6-sol con ese prompt de sistema
//   --skill      TAiSkills.LoadFromPPM('skill-cotizacion-mayorista', url corporativa)
//   (sin argumentos: todo). REGISTRY_LADERA = URL del corporativo (default http://127.0.0.1:8766).
// Empresa, productos y precios ficticios.
// =============================================================================

{$APPTYPE CONSOLE}
{$R *.res}

uses
  {$IFDEF MSWINDOWS} Winapi.Windows, {$ENDIF}
  System.SysUtils,
  System.Classes,
  System.StrUtils,
  System.JSON,
  System.Diagnostics,
  System.Net.HttpClient,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations,
  uMakerAi.Prompts,
  uMakerAi.Tools.Functions,
  uMakerAi.Tools.Skills;

var
  BufSalida: array [0 .. 0] of Byte;
  Corporativo: string;

function Tiene(const AArg: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to ParamCount do
    if SameText(ParamStr(I), AArg) then
      Exit(True);
end;

procedure Titulo(const ATexto: string);
begin
  Writeln;
  Writeln('== ' + ATexto + ' ' + StringOfChar('=', 76 - Length(ATexto)));
end;

function Resumen(const ATexto: string; AMax: Integer = 160): string;
begin
  Result := ATexto.Replace(#13, ' ').Replace(#10, ' ').Replace('  ', ' ').Trim;
  if Length(Result) > AMax then
    Result := Copy(Result, 1, AMax) + '…';
end;

function Salud(const AUrl: string): string;
var
  H: THTTPClient;
  R: IHTTPResponse;
  Reloj: TStopwatch;
begin
  H := THTTPClient.Create;
  try
    H.ConnectionTimeout := 5000;
    H.ResponseTimeout := 5000;
    Reloj := TStopwatch.StartNew;
    try
      R := H.Get(AUrl + '/v1/health');
      Result := Format('%s -> %d %s (%d ms)', [AUrl, R.StatusCode, Resumen(R.ContentAsString, 80), Reloj.ElapsedMilliseconds]);
    except
      on E: Exception do
        Result := Format('%s -> sin respuesta (%s)', [AUrl, E.Message]);
    end;
  finally
    H.Free;
  end;
end;

procedure SaludDeLosDos;
begin
  Titulo('A. Salud: el registry corporativo y el público');
  Writeln('  corporativo: ' + Salud(Corporativo));
  Writeln('  público:     ' + Salud('https://registry.cimamaker.com'));
end;

procedure Prompt;
var
  P: TAiPrompts;
  J: TJSONObject;
  Lista: TJSONArray;
  Item: TAiPromptItem;
  Conn: TAiChatConnection;
  Sistema, R: string;
  I: Integer;
  Reloj: TStopwatch;
begin
  Titulo('B. El prompt de Lucía, publicado en el registry corporativo');
  P := TAiPrompts.Create(nil);
  try
    P.PPMRegistryUrl := Corporativo;        // la unica linea que cambia respecto al capitulo 32
    Writeln('  PPMRegistryUrl = ' + P.PPMRegistryUrl);
    J := P.SearchPPM('lucia', 'prompt');
    try
      if Assigned(J) and J.TryGetValue<TJSONArray>('packages', Lista) then
        for I := 0 to Lista.Count - 1 do
          Writeln(Format('  SearchPPM: %s v%s · %s', [Lista.Items[I].GetValue<string>('name', ''),
            Lista.Items[I].GetValue<string>('version', ''), Resumen(Lista.Items[I].GetValue<string>('description', ''), 90)]))
      else
        Writeln('  SearchPPM: sin respuesta');
    finally
      J.Free;
    end;
    Reloj := TStopwatch.StartNew;
    Item := P.LoadFromPPM('lucia-barista');
    if not Assigned(Item) then
    begin
      Writeln('  LoadFromPPM: no está lucia-barista en ' + Corporativo);
      Exit;
    end;
    Writeln(Format('  LoadFromPPM(''lucia-barista''): %d líneas en %d ms · %s', [Item.Strings.Count, Reloj.ElapsedMilliseconds, Item.Descripcion]));
    Sistema := P.GetTemplate('lucia-barista', ['cliente=Marta', 'cafe=Huila Reserva']);
    Writeln('  con variables: ' + Resumen(Sistema, 140));
    if GetEnvironmentVariable('OPENAI_API_KEY') = '' then
      Exit;
    Conn := TAiChatConnection.Create(nil);
    try
      Conn.DriverName := 'OpenAi';
      Conn.Model := 'gpt-6-sol';
      Conn.Params.Values['ApiKey'] := '@OPENAI_API_KEY';
      Conn.Params.Values['Asynchronous'] := 'False';
      Conn.SystemPrompt.Text := Sistema;    // el prompt que vino del registry, con sus variables
      Reloj := TStopwatch.StartNew;
      R := Conn.AddMessageAndRun('Hola, ¿qué me recomiendas y cuánto me sale con envío a Cali?', 'user', []);
      Writeln(Format('  Lucía (%.1f s): %s', [Reloj.ElapsedMilliseconds / 1000, Resumen(R, 300)]));
    finally
      Conn.Free;
    end;
  finally
    P.Free;
  end;
end;

procedure Skill;
var
  Fn: TAiFunctions;
  Sk: TAiSkills;
  It: TAiSkillItem;
  Reloj: TStopwatch;
begin
  Titulo('C. Un skill de la tienda, desde el registry corporativo');
  Fn := TAiFunctions.Create(nil);
  Sk := TAiSkills.Create(nil);
  try
    Sk.Functions := Fn;
    Reloj := TStopwatch.StartNew;
    try
      It := Sk.LoadFromPPM('skill-cotizacion-mayorista', Corporativo);   // el registry va como parametro
      Writeln(Format('  cargado "%s" en %d ms · catálogo: %s', [It.Name, Reloj.ElapsedMilliseconds, Resumen(Sk.Catalog, 150)]));
    except
      on E: Exception do
        Writeln('  no se pudo cargar: ' + Resumen(E.Message, 200));
    end;
  finally
    Sk.Free;
    Fn.Free;
  end;
end;

var
  Todo: Boolean;

begin
  {$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);
  SetTextCodePage(Output, CP_UTF8);
  {$ENDIF}
  SetTextBuf(Output, BufSalida, 1);
  Corporativo := GetEnvironmentVariable('REGISTRY_LADERA');
  if Corporativo = '' then
    Corporativo := 'http://127.0.0.1:8766';
  try
    Todo := not (Tiene('--salud') or Tiene('--prompt') or Tiene('--skill'));
    if Todo or Tiene('--salud') then
      SaludDeLosDos;
    if Todo or Tiene('--prompt') then
      Prompt;
    if Todo or Tiene('--skill') then
      Skill;
  except
    on E: Exception do
      Writeln(E.ClassName + ': ' + E.Message);
  end;
end.
