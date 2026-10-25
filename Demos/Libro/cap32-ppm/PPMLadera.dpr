program PPMLadera;

// =============================================================================
// LIBRO "IA con Delphi" - capitulo 32: PPM, el registry del ecosistema, en consola
// =============================================================================
// Lo que se puede traer del registry (https://registry.cimamaker.com) sin salir de Delphi:
//   --prompts   TAiPrompts: plantilla local, SearchPPM y LoadFromPPM (un prompt con variables)
//   --skill     TAiSkills.LoadFromPPM: un SKILL.md del registry, en el catalogo y bajo demanda
//   --mcp       TAiFunctions.InstallMCPFromPPM: el desarrollador instala un servidor MCP (el clima)
//               y Lucia lo usa como una herramienta mas
//   --automcp   AutoMCP: Lucia busca, instala y usa la herramienta ella misma, solo dentro de
//               AutoMCPConfig.Allowed; OnAutoMCPRequest ve cada instalacion
//   (sin argumentos: todo). Requiere OPENAI_API_KEY para --mcp y --automcp. Ficticio todo.
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
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations,
  uMakerAi.Prompts,
  uMakerAi.Tools.Functions,
  uMakerAi.Tools.Skills,
  uMakerAi.Guardrails;

const
  LUCIA = 'Eres Lucía, la barista virtual de Café La Ladera (Pitalito, Huila, Colombia). Responde en español, breve, en texto plano.';

var
  BufSalida: array [0 .. 0] of Byte;

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

function Resumen(const ATexto: string; AMax: Integer = 120): string;
begin
  Result := ATexto.Replace(#13, ' ').Replace(#10, ' ').Replace('  ', ' ').Trim;
  if Length(Result) > AMax then
    Result := Copy(Result, 1, AMax) + '…';
end;

{ ---------------------------------------------------------------------------- A: prompts }

procedure Prompts;
var
  P: TAiPrompts;
  J: TJSONObject;
  Lista: TJSONArray;
  I: Integer;
  Item: TAiPromptItem;
  Reloj: TStopwatch;
begin
  Titulo('A. Prompts: una plantilla local y una del registry');
  P := TAiPrompts.Create(nil);
  try
    // 1. Local: la misma pieza del capitulo 5, con variables <#nombre>
    P.AddString('cotizacion', 'Cotización para <#Cliente>: <#Cantidad> bolsas de <#Cafe>. Responde con el total y la fecha de entrega.');
    Writeln('  local:    ' + P.GetTemplate('cotizacion', ['Cliente=Hotel Sierra', 'Cantidad=12', 'Cafe=Huila Reserva']));
    // 2. Buscar en el registry (tipo prompt)
    Writeln('  registry: ' + P.PPMRegistryUrl);
    Reloj := TStopwatch.StartNew;
    J := P.SearchPPM('code', 'prompt');
    try
      if Assigned(J) and J.TryGetValue<TJSONArray>('packages', Lista) then
      begin
        Writeln(Format('  SearchPPM(''code'', ''prompt''): %d paquetes en %d ms', [Lista.Count, Reloj.ElapsedMilliseconds]));
        for I := 0 to Lista.Count - 1 do
          Writeln(Format('    %-16s v%-6s %s', [Lista.Items[I].GetValue<string>('name', ''), Lista.Items[I].GetValue<string>('version', ''),
            Resumen(Lista.Items[I].GetValue<string>('description', ''), 70)]));
      end
      else
        Writeln('  sin respuesta del registry');
    finally
      J.Free;
    end;
    // 3. Traer uno: baja el texto, convierte {{var}} en <#var> y queda como un item mas
    Reloj := TStopwatch.StartNew;
    Item := P.LoadFromPPM('commit-message');
    if not Assigned(Item) then
    begin
      Writeln('  LoadFromPPM: no se encontró commit-message');
      Exit;
    end;
    Writeln(Format('  LoadFromPPM(''commit-message''): %s · %d líneas · %d ms', [Item.Descripcion, Item.Strings.Count, Reloj.ElapsedMilliseconds]));
    Writeln('  con variables: ' + Resumen(P.GetTemplate('commit-message', ['commit_type=feat', 'scope=pedidos',
      'change_summary=la voz avisa cuando el encargado aprueba']), 300));
  finally
    P.Free;
  end;
end;

{ ---------------------------------------------------------------------------- B: skill }

procedure Skill;
var
  Fn: TAiFunctions;
  Sk: TAiSkills;
  It: TAiSkillItem;
  R: string;
  Reloj: TStopwatch;
begin
  Titulo('B. Un SKILL.md del registry, bajo demanda (TAiSkills.LoadFromPPM)');
  Fn := TAiFunctions.Create(nil);
  Sk := TAiSkills.Create(nil);
  try
    Sk.Functions := Fn;              // registra use_skill / read_skill_file, como en el capitulo 17
    Reloj := TStopwatch.StartNew;
    It := Sk.LoadFromPPM('skill-contabilidad');
    Writeln(Format('  cargado "%s" desde el registry en %d ms', [It.Name, Reloj.ElapsedMilliseconds]));
    Writeln('  catálogo que ve el modelo:');
    Writeln('    ' + Resumen(Sk.Catalog, 200));
    R := Sk.ExecuteUseSkill(It.Name);   // lo que use_skill le entregaria al modelo
    Writeln(Format('  use_skill(%s): %d caracteres; empieza: %s', [It.Name, Length(R), Resumen(R, 160)]));
    Writeln('  (un skill de terceros se marca como tal al modelo: "Third-party skill from the PPM registry")');
  finally
    Sk.Free;
    Fn.Free;
  end;
end;

{ ---------------------------------------------------------------------------- C y D: MCP }

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
  // La guardia del capitulo 22 ve TODAS las llamadas, tambien las de AutoMCP y las del MCP instalado
  Writeln('  [tool] ' + AToolName + ' ' + Resumen(AArguments, 150));
  AAllow := True;
end;

procedure TOyente.Instalar(Sender: TObject; const APkgName: string; var AAllow: Boolean);
begin
  // Se dispara por cada candidato de ppm_search y antes de cada ppm_install. OJO: si el evento esta
  // conectado, SU decision es la final y Allowed/Blocked no se consultan: aqui se aplica la lista a mano.
  AAllow := (Sender as TAiFunctions).AutoMCPConfig.Allowed.IndexOf(APkgName) >= 0;
  Writeln('  [OnAutoMCPRequest] "' + APkgName + '": ' + IfThen(AAllow, 'permitido (está en Allowed)', 'rechazado (no está en Allowed)'));
end;

function Conexion(Fn: TAiFunctions; const ASistema: string): TAiChatConnection;
begin
  Result := TAiChatConnection.Create(nil);
  Result.DriverName := 'OpenAi';
  Result.Model := 'gpt-6-sol';
  Result.Params.Values['ApiKey'] := '@OPENAI_API_KEY';
  Result.Params.Values['Asynchronous'] := 'False';
  Result.AiFunctions := Fn;
  Result.Tool_Active := True;
  Result.SystemPrompt.Text := ASistema;
end;

procedure MCP;
var
  Fn: TAiFunctions;
  G: TAiGuardrails;
  Oy: TOyente;
  Conn: TAiChatConnection;
  Item: TMCPClientItem;
  R: string;
  Reloj: TStopwatch;
begin
  Titulo('C. Un MCP del registry, instalado por el desarrollador (InstallMCPFromPPM)');
  Fn := TAiFunctions.Create(nil);
  G := TAiGuardrails.Create(nil);
  Oy := TOyente.Create;
  Conn := nil;
  try
    G.OnCheckToolCall := Oy.Revisar;
    Fn.Guardrails := G;
    Fn.OnLog := Oy.Log;
    // Baja el .paipkg, extrae el binario en ~\.ppm\mcp\<nombre> y lo deja como cliente MCP stdio de este TAiFunctions
    Reloj := TStopwatch.StartNew;
    Item := Fn.InstallMCPFromPPM('mcp-openmeteo');
    if not Assigned(Item) then
    begin
      Writeln('  no se pudo instalar mcp-openmeteo');
      Exit;
    end;
    Writeln(Format('  instalado "%s" en %.1f s; sus herramientas llegan al modelo con el prefijo del servidor',
      [Item.Name, Reloj.ElapsedMilliseconds / 1000]));
    Conn := Conexion(Fn, LUCIA + ' Tienes una herramienta de clima: úsala en vez de adivinar.');
    Reloj := TStopwatch.StartNew;
    R := Conn.AddMessageAndRun('¿Cómo está el clima ahora mismo en Pitalito, Huila?', 'user', []);
    Writeln(Format('  Lucía (%.1f s): %s', [Reloj.ElapsedMilliseconds / 1000, Resumen(R, 300)]));
  finally
    Conn.Free;
    Fn.Free;
    G.Free;
    Oy.Free;
  end;
end;

procedure AutoMCP;
var
  Fn: TAiFunctions;
  G: TAiGuardrails;
  Oy: TOyente;
  Conn: TAiChatConnection;
  R: string;
  Reloj: TStopwatch;
begin
  Titulo('D. AutoMCP: el modelo busca, instala y usa; solo lo que diga Allowed');
  Fn := TAiFunctions.Create(nil);
  G := TAiGuardrails.Create(nil);
  Oy := TOyente.Create;
  Conn := nil;
  try
    G.OnCheckToolCall := Oy.Revisar;
    Fn.Guardrails := G;
    Fn.OnLog := Oy.Log;
    Fn.OnAutoMCPRequest := Oy.Instalar;
    Fn.AutoMCPConfig.Allowed.Add('mcp-openmeteo');   // lista blanca: nada mas se instala
    Fn.AutoMCPConfig.Active := True;                 // expone ppm_search, ppm_install y call_mcp_tool
    Conn := Conexion(Fn, LUCIA + sLineBreak + Fn.GetAutoMCPSystemPrompt);
    Writeln('  pregunta: ¿Va a llover mañana en Pitalito, Huila? Es para saber si despachamos los pedidos.');
    Reloj := TStopwatch.StartNew;
    R := Conn.AddMessageAndRun('¿Va a llover mañana en Pitalito, Huila? Es para saber si despachamos los pedidos.', 'user', []);
    Writeln(Format('  Lucía (%.1f s): %s', [Reloj.ElapsedMilliseconds / 1000, Resumen(R, 400)]));
    Writeln(Format('  tokens del turno (todas las rondas): %d en total, %d de salida', [Conn.Total_tokens, Conn.Completion_tokens]));
  finally
    Conn.Free;
    Fn.Free;
    G.Free;
    Oy.Free;
  end;
end;

{ ---------------------------------------------------------------------------- }

var
  Todo: Boolean;

begin
  {$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);
  SetTextCodePage(Output, CP_UTF8);
  {$ENDIF}
  SetTextBuf(Output, BufSalida, 1);
  try
    Todo := not (Tiene('--prompts') or Tiene('--skill') or Tiene('--mcp') or Tiene('--automcp'));
    if Todo or Tiene('--prompts') then
      Prompts;
    if Todo or Tiene('--skill') then
      Skill;
    if GetEnvironmentVariable('OPENAI_API_KEY') = '' then
    begin
      Writeln('(sin OPENAI_API_KEY: se omiten C y D)');
      Exit;
    end;
    if Todo or Tiene('--mcp') then
      MCP;
    if Todo or Tiene('--automcp') then
      AutoMCP;
  except
    on E: Exception do
      Writeln(E.ClassName + ': ' + E.Message);
  end;
end.
