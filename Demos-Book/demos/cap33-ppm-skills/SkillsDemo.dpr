program SkillsDemo;

{$APPTYPE CONSOLE}

// =============================================================================
// MakerAI — Capítulo 33: Skills PPM para Claude Code y agentes Delphi
// =============================================================================
//
// Demuestra las tres operaciones clave con TAiPrompts + PPM:
//
//   1. DescubrirSkills  — SearchPPM(type='skill') lista el catálogo del registry
//   2. CargarSkill      — LoadSkillFromPPM descarga y parsea frontmatter YAML
//   3. UsarSkillComoAgente — Strings.Text como SystemPrompt en TAiChatConnection
//
// TAiPrompts.LoadSkillFromPPM:
//   - Llama GET /v1/packages/{name}/{version}/skill
//   - Extrae del frontmatter YAML: model, allowed-tools
//   - Almacena SOLO el cuerpo Markdown en Strings.Text (sin frontmatter)
//   - Expone SkillModel y SkillAllowedTools en el item resultante
//
// Requiere: GROQ_API_KEY (para la parte 3)
// =============================================================================

uses
  System.SysUtils, System.JSON,
  uMakerAi.Prompts,         // TAiPrompts, TAiPromptItem
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Groq;       // auto-registra driver 'Groq'

// -----------------------------------------------------------------------------
//  1. Descubrir skills disponibles en el registry PPM
// -----------------------------------------------------------------------------
procedure DescubrirSkills(Prompts: TAiPrompts);
var
  Json    : TJSONObject;
  Pkgs    : TJSONArray;
  Pkg     : TJSONObject;
  Name    : string;
  Desc    : string;
  I       : Integer;
begin
  WriteLn('--- 1. Descubrir skills en el registry ---');
  Json := Prompts.SearchPPM('', 'skill', 1, 10);  // query vacío = todos los skills
  if not Assigned(Json) then
  begin
    WriteLn('  No se pudo conectar al registry.');
    WriteLn;
    Exit;
  end;
  try
    if Json.TryGetValue<TJSONArray>('packages', Pkgs) then
    begin
      WriteLn('  Skills disponibles: ', Pkgs.Count);
      for I := 0 to Pkgs.Count - 1 do
      begin
        Pkg  := Pkgs.Items[I] as TJSONObject;
        Name := '';
        Desc := '';
        Pkg.TryGetValue<String>('name',        Name);
        Pkg.TryGetValue<String>('description', Desc);
        WriteLn('  ', I + 1, '. ', Name);
        WriteLn('     ', Desc);
      end;
    end
    else
      WriteLn('  (sin resultados)');
  finally
    Json.Free;
  end;
  WriteLn;
end;

// -----------------------------------------------------------------------------
//  2. Cargar skill: frontmatter parseado, cuerpo en Strings.Text
// -----------------------------------------------------------------------------
procedure CargarSkill(Prompts: TAiPrompts; const AName: string);
var
  Skill: TAiPromptItem;
  N, I : Integer;
begin
  WriteLn('--- 2. Cargar skill: ', AName, ' ---');
  Skill := Prompts.LoadSkillFromPPM(AName);
  if not Assigned(Skill) then
  begin
    WriteLn('  No encontrado o sin conexión.');
    WriteLn;
    Exit;
  end;

  WriteLn('  Nombre     : ', Skill.Nombre);
  WriteLn('  Descripción: ', Skill.Descripcion);
  WriteLn('  SkillModel : ', Skill.SkillModel);
  WriteLn('  AllowedTools: ', Skill.SkillAllowedTools);
  WriteLn('  Líneas en cuerpo: ', Skill.Strings.Count);
  WriteLn;
  WriteLn('  Primeras 6 líneas del cuerpo (sin frontmatter):');
  N := Skill.Strings.Count;
  if N > 6 then N := 6;
  for I := 0 to N - 1 do
    WriteLn('  | ', Skill.Strings[I]);
  if Skill.Strings.Count > 6 then
    WriteLn('  | [...', Skill.Strings.Count - 6, ' líneas más]');
  WriteLn;
end;

// -----------------------------------------------------------------------------
//  3. Usar el skill como system prompt en un agente MakerAI
// -----------------------------------------------------------------------------
procedure UsarSkillComoAgente(Prompts: TAiPrompts; const AName: string);
var
  Skill   : TAiPromptItem;
  AiConn  : TAiChatConnection;
  Modelo  : string;
  Revision: string;
const
  // Código Delphi deliberadamente inseguro (SQL injection) para que el skill lo detecte
  CODIGO_SQL_INJECTION =
    'procedure TForm1.ButtonClick(Sender: TObject);' + sLineBreak +
    'var S: string;' + sLineBreak +
    'begin' + sLineBreak +
    '  S := Edit1.Text;' + sLineBreak +
    '  Query1.SQL.Text := ''SELECT * FROM clientes WHERE nombre = '' + S;' + sLineBreak +
    '  Query1.Open;' + sLineBreak +
    'end;';
begin
  WriteLn('--- 3. Skill como system prompt del agente (Groq) ---');

  // El skill ya está cacheado en Prompts.Items; solo lo buscamos por nombre
  Skill := Prompts.LoadSkillFromPPM(AName);
  if (Skill = nil) or (Skill.Strings.Count = 0) then
  begin
    WriteLn('  Skill no disponible, omitiendo.');
    WriteLn;
    Exit;
  end;

  // El skill recomienda claude-opus-4-6; usamos Groq como driver local de bajo costo.
  // Para producción: driver='Claude', Model=Skill.SkillModel
  Modelo := 'llama-3.3-70b-versatile';

  WriteLn('  Skill        : ', Skill.Nombre);
  WriteLn('  Driver usado : Groq (', Modelo, ')');
  WriteLn('  Skill sugiere: ', Skill.SkillModel);
  WriteLn('  Tools (info) : ', Skill.SkillAllowedTools);
  WriteLn;

  AiConn := TAiChatConnection.Create(nil);
  try
    AiConn.DriverName                    := 'Groq';
    AiConn.Params.Values['ApiKey']       := '@GROQ_API_KEY';
    AiConn.Model                         := Modelo;
    AiConn.Params.Values['Asynchronous'] := 'False';
    AiConn.SystemPrompt.Text             := Skill.Strings.Text; // cuerpo sin frontmatter

    WriteLn('  Código a revisar:');
    WriteLn('  ', StringReplace(CODIGO_SQL_INJECTION, sLineBreak, sLineBreak + '  ', []));
    WriteLn;
    WriteLn('  Revisión del agente:');
    Revision := AiConn.AddMessageAndRun(
      'Revisa el siguiente código Delphi e identifica problemas:' +
      sLineBreak + CODIGO_SQL_INJECTION, 'user', []);
    WriteLn(Revision);
  finally
    AiConn.Free;
  end;
  WriteLn;
end;

// =============================================================================
const
  SKILL_NAME = 'skill-code-review';

begin
  WriteLn('==============================================');
  WriteLn('  Cap. 33 — Skills PPM para Claude Code');
  WriteLn('==============================================');
  WriteLn;

  var Prompts := TAiPrompts.Create(nil);
  try
    // 1. Descubrir skills en el registry
    DescubrirSkills(Prompts);

    // 2. Cargar skill con frontmatter parseado
    CargarSkill(Prompts, SKILL_NAME);

    // 3. Usar skill como agente (requiere GROQ_API_KEY)
    if GetEnvironmentVariable('GROQ_API_KEY') <> '' then
      UsarSkillComoAgente(Prompts, SKILL_NAME)
    else
      WriteLn('SKIP parte 3: GROQ_API_KEY no configurada.');

  finally
    Prompts.Free;
  end;

  WriteLn;
  WriteLn('Para activar en Claude Code:');
  WriteLn('  ppm install ', SKILL_NAME);
  WriteLn('  ppm skill install-claude');
  WriteLn('  -> /', SKILL_NAME, ' disponible en Claude Code');
  WriteLn;
  WriteLn('Demo completado. Presiona Enter para salir.');
  ReadLn;
end.
