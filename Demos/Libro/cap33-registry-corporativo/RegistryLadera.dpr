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
//   --automcp    AutoMCP contra el corporativo (AutoMCPConfig.RegistryUrl): Lucia busca, instala y
//                usa mcp-bodega-ladera (el servidor MCP del demo 100, publicado como paquete), solo
//                dentro de AutoMCPConfig.Allowed. Requiere OPENAI_API_KEY
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
  uMakerAi.Tools.Skills,
  uMakerAi.Guardrails;

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

{ ---------------------------------------------------------------------------- D: AutoMCP }

type
  TOyente = class
    procedure Log(Sender: TObject; const Msg: string);
    procedure Revisar(Sender: TObject; const AToolName, AArguments: string; var AAllow: Boolean; var AReason: string);
    procedure Instalar(Sender: TObject; const APkgName: string; var AAllow: Boolean);
  end;

procedure TOyente.Log(Sender: TObject; const Msg: string);
begin
  if Msg.StartsWith('[AutoMCP]') then
    Writeln('  ' + Msg);
end;

procedure TOyente.Revisar(Sender: TObject; const AToolName, AArguments: string; var AAllow: Boolean; var AReason: string);
begin
  Writeln('  [tool] ' + AToolName + ' ' + Resumen(AArguments, 150));
  AAllow := True;
end;

procedure TOyente.Instalar(Sender: TObject; const APkgName: string; var AAllow: Boolean);
begin
  // Con el evento conectado, SU decision es la final: aqui se aplica Allowed a mano (como en el cap 32)
  AAllow := (Sender as TAiFunctions).AutoMCPConfig.Allowed.IndexOf(APkgName) >= 0;
  Writeln('  [OnAutoMCPRequest] "' + APkgName + '": ' + IfThen(AAllow, 'permitido (está en Allowed)', 'rechazado (no está en Allowed)'));
end;

procedure AutoMCPCorporativo;
const
  PREGUNTA = '¿Cuántos kilos de Huila Reserva nos quedan en la bodega?';
var
  Fn: TAiFunctions;
  G: TAiGuardrails;
  Oy: TOyente;
  Conn: TAiChatConnection;
  R: string;
  Reloj: TStopwatch;
begin
  Titulo('D. AutoMCP contra el registry corporativo: la bodega de la empresa');
  if GetEnvironmentVariable('OPENAI_API_KEY') = '' then
  begin
    Writeln('  (sin OPENAI_API_KEY)');
    Exit;
  end;
  Fn := TAiFunctions.Create(nil);
  G := TAiGuardrails.Create(nil);
  Oy := TOyente.Create;
  Conn := nil;
  try
    G.OnCheckToolCall := Oy.Revisar;
    Fn.Guardrails := G;
    Fn.OnLog := Oy.Log;
    Fn.OnAutoMCPRequest := Oy.Instalar;
    Fn.AutoMCPConfig.RegistryUrl := Corporativo;        // el mismo AutoMCP del cap 32, otro registry
    Fn.AutoMCPConfig.Allowed.Add('mcp-bodega-ladera');
    Fn.AutoMCPConfig.Active := True;                    // ppm_search, ppm_install y call_mcp_tool
    Writeln('  AutoMCPConfig.RegistryUrl = ' + Fn.AutoMCPConfig.RegistryUrl);
    Conn := TAiChatConnection.Create(nil);
    Conn.DriverName := 'OpenAi';
    Conn.Model := 'gpt-6-sol';
    Conn.Params.Values['ApiKey'] := '@OPENAI_API_KEY';
    Conn.Params.Values['Asynchronous'] := 'False';
    Conn.AiFunctions := Fn;
    Conn.Tool_Active := True;
    Conn.SystemPrompt.Text := 'Eres Lucía, la barista virtual de Café La Ladera (Pitalito, Huila, Colombia). ' +
      'Responde en español, breve, en texto plano.' + sLineBreak + Fn.GetAutoMCPSystemPrompt;
    Writeln('  pregunta: ' + PREGUNTA);
    Reloj := TStopwatch.StartNew;
    R := Conn.AddMessageAndRun(PREGUNTA, 'user', []);
    Writeln(Format('  Lucía (%.1f s): %s', [Reloj.ElapsedMilliseconds / 1000, Resumen(R, 400)]));
  finally
    Conn.Free;
    Fn.Free;
    G.Free;
    Oy.Free;
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
    Todo := not (Tiene('--salud') or Tiene('--prompt') or Tiene('--skill') or Tiene('--automcp'));
    if Todo or Tiene('--salud') then
      SaludDeLosDos;
    if Todo or Tiene('--prompt') then
      Prompt;
    if Todo or Tiene('--skill') then
      Skill;
    if Todo or Tiene('--automcp') then
      AutoMCPCorporativo;
  except
    on E: Exception do
      Writeln(E.ClassName + ': ' + E.Message);
  end;
end.
