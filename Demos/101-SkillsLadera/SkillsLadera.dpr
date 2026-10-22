program SkillsLadera;

// =============================================================================
// DEMO 101 - Skills de Café La Ladera
// =============================================================================
// Un skill es una carpeta con un SKILL.md: frontmatter (nombre y cuándo usarlo)
// + las instrucciones. Es el formato de las Agent Skills (Claude Code) y del
// registry PPM. La carpeta skills\ trae tres de la tienda:
//
//   cotizacion-mayorista  precios, descuentos y una plantilla (plantilla.md)
//   respuesta-reclamo     cómo le contesta Lucía a un cliente molesto
//   ficha-de-cata         el formato de la ficha de un café para la carta
//
// TAiSkills los ofrece BAJO DEMANDA: el modelo ve solo el catálogo (una línea
// por skill) y carga el que necesita con use_skill; si las instrucciones lo
// piden, lee los archivos de apoyo con read_skill_file. Una pregunta que no
// corresponde a ningún skill no carga nada.
//
// Requiere OPENAI_API_KEY. Empresa, productos y precios ficticios.
// =============================================================================

{$APPTYPE CONSOLE}
{$R *.res}

uses
  {$IFDEF MSWINDOWS} Winapi.Windows, {$ENDIF}
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  uMakerAi.Chat.Messages,
  uMakerAi.Tools.Functions,
  uMakerAi.Tools.Skills,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations;   // registra todos los drivers

const
  PREGUNTAS: array [0 .. 3] of string = (
    'El Hotel Casa Verde quiere 15 kilos de Sierra Nevada Orgánico. Prepárale la cotización.',
    'Una clienta escribe: "Pedí el Huila Reserva en grano y me llegó molido". Contéstale.',
    'Escribe la ficha de cata del Nariño Altura para la carta.',
    '¿Cómo se dice "café de especialidad" en inglés? Responde solo la traducción.');

type
  // Los eventos del framework son 'of object': los handlers viven en una clase
  TEventos = class
  public
    Skills: TAiSkills;
    procedure SkillCargado(Sender: TObject; ASkill: TAiSkillItem);
    procedure LeerArchivo(Sender: TObject; FunctionAction: TFunctionActionItem;
      FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
  end;

procedure TEventos.SkillCargado(Sender: TObject; ASkill: TAiSkillItem);
begin
  Writeln('   [skill] use_skill ', ASkill.Name);
end;

// Muestra la lectura y la delega en TAiSkills (ExecuteReadFile es público)
procedure TEventos.LeerArchivo(Sender: TObject; FunctionAction: TFunctionActionItem;
  FunctionName: string; ToolCall: TAiToolsFunction; var Handled: Boolean);
begin
  Handled := True;
  Writeln('   [skill] read_skill_file ', ToolCall.Params.Values['name'], '/',
    ToolCall.Params.Values['path']);
  try
    ToolCall.Response := Skills.ExecuteReadFile(ToolCall.Params.Values['name'],
      ToolCall.Params.Values['path']);
  except
    on E: Exception do
      ToolCall.Response := 'Error: ' + E.Message;
  end;
end;

// La carpeta skills\ está junto al .dpr; el .exe queda en Win64\Release
function BuscarCarpetaSkills: string;
var
  Dir: string;
  I: Integer;
begin
  Dir := ExtractFilePath(ParamStr(0));
  for I := 0 to 3 do
  begin
    Result := TPath.Combine(Dir, 'skills');
    if TDirectory.Exists(Result) then
      Exit;
    Dir := TPath.GetDirectoryName(ExcludeTrailingPathDelimiter(Dir));
  end;
  raise Exception.Create('No se encontró la carpeta skills\ del demo');
end;

var
  Eventos: TEventos;
  Funciones: TAiFunctions;
  Skills: TAiSkills;
  Chat: TAiChatConnection;
  Carpeta, Pregunta, R: string;
  I, Completos: Integer;
var
  BufSalida: array [0 .. 0] of Byte;

begin
  {$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);
  SetTextCodePage(Output, CP_UTF8);
  {$ENDIF}
  // Salida a un pipe (grabaciones, logs): el RTL la junta en bloques de 128 bytes.
  // Con un buffer de 1 byte cada caracter sale al escribirlo.
  SetTextBuf(Output, BufSalida, 1);
  if GetEnvironmentVariable('OPENAI_API_KEY') = '' then
  begin
    Writeln('Falta la variable OPENAI_API_KEY.');
    ExitCode := 1;
    Exit;
  end;

  Eventos := TEventos.Create;
  Funciones := TAiFunctions.Create(nil);
  Skills := TAiSkills.Create(nil);
  Chat := TAiChatConnection.Create(nil);
  try
    try
      // Los skills se registran como dos funciones (use_skill, read_skill_file)
      // en el TAiFunctions del chat
      Carpeta := BuscarCarpetaSkills;
      Skills.OnSkillLoaded := Eventos.SkillCargado;
      Skills.Functions := Funciones;
      Writeln('Skills en la carpeta skills\: ', Skills.LoadFromFolder(Carpeta));
      Eventos.Skills := Skills;
      Funciones.Functions.GetFunction(Skills.ReadFunctionName).OnAction := Eventos.LeerArchivo;

      // Lo que viaja en cada turno es el catálogo, no las instrucciones
      Completos := 0;
      for I := 0 to Skills.Skills.Count - 1 do
        Inc(Completos, Skills.Skills[I].Instructions.Text.Length);
      Writeln('Catálogo que ve el modelo (', Skills.Catalog.Length, ' caracteres; los skills completos suman ',
        Completos, '):');
      Writeln(Skills.Catalog.Trim);
      Writeln;

      Chat.DriverName := 'OpenAi';
      Chat.Params.Values['ApiKey'] := '@OPENAI_API_KEY';
      Chat.Params.Values['Asynchronous'] := 'False';
      Chat.Tool_Active := True;
      Chat.SystemPrompt.Text :=
        'Eres Lucía, la asistente de Café La Ladera. Responde en español y en texto plano ' +
        'sin Markdown: la respuesta sale en una consola.';
      Chat.AiFunctions := Funciones;

      for Pregunta in PREGUNTAS do
      begin
        // Cada petición en un chat nuevo: el skill se carga porque la petición lo pide
        Chat.NewChat;
        Writeln('Tú: ', Pregunta);
        R := Chat.AddMessageAndRun(Pregunta, 'user', []);
        if R = '' then
          R := '(sin respuesta) ' + Chat.LastError;
        Writeln('Lucía: ', AdjustLineBreaks(R.Trim).Replace(sLineBreak, sLineBreak + '       '));
        Writeln;
      end;
      ExitCode := 0;
    except
      on E: Exception do
      begin
        Writeln('ERROR: ', E.ClassName, ': ', E.Message);
        ExitCode := 2;
      end;
    end;
  finally
    Chat.AiFunctions := nil;
    Chat.Free;
    Skills.Free;
    Funciones.Free;
    Eventos.Free;
  end;
end.
