program JevDispatchGuardDemo;

// =============================================================================
// DEMO 085 - Jev en SmartDispatch y en Guardrails
// =============================================================================
// Dos decisiones que hoy cuestan un LLM completo o una lista que no alcanza:
//
//   A. SmartDispatch: TAiJevDispatchClassifier, asignado a
//      ChatTools.DispatchClassifier, decide si la peticion va a IMAGEGEN,
//      VIDEOGEN, TTS, WEBSEARCH o CHAT sin el pase de clasificacion por LLM.
//      Aqui se llama directo (via IAiDispatchClassifier) para no exigir las
//      API keys de las tools; en una app lo llama el chat.
//
//   B. Guardrails: TAiJevGuardrailClassifier, asignado a
//      TAiGuardrails.Classifier, juzga el riesgo de cada tool call que las
//      listas dejaron pasar. Las listas atrapan lo enumerable ('rm -rf'); Jev
//      atrapa lo que no, como un correo con una clave a un externo.
//
// Requiere la variable de entorno TYPESAFE_API_KEY (https://console.typesafe.ai/keys).
// Costo: unos centavos de centavo por corrida.
// =============================================================================

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  uMakerAi.Chat.Tools,
  uMakerAi.Guardrails,
  uMakerAi.Jev in '..\..\Source\Tools\uMakerAi.Jev.pas',
  uMakerAi.Jev.SmartDispatch in '..\..\Source\Tools\uMakerAi.Jev.SmartDispatch.pas',
  uMakerAi.Jev.Guardrails in '..\..\Source\Tools\uMakerAi.Jev.Guardrails.pas';

const
  PETICIONES: array[0..7] of string = (
    'dibuja un gato rojo sobre un tejado',
    'hazme un video corto de olas en la playa',
    'Lee en voz alta este parrafo: la reunion se aplaza para el lunes',
    'A cuanto esta el dolar hoy en Colombia?',
    'Explicame que es una partida doble',
    'Describe como se veria un gato rojo sobre un tejado',
    'Escribe una funcion en Delphi que invierta un string',
    'latest AI news');

  // Tags con tool asignada en el chat de ejemplo (el chat los calcula solo)
  TAGS: TArray<string> = ['IMAGEGEN', 'VIDEOGEN', 'TTS', 'WEBSEARCH', 'CHAT'];

type
  TLlamada = record
    Tool, Args: string;
  end;

const
  LLAMADAS: array[0..7] of TLlamada = (
    (Tool: 'read_file';      Args: '{"path":"C:/proyectos/app/readme.md"}'),
    (Tool: 'sql_query';      Args: '{"sql":"SELECT nombre, saldo FROM clientes WHERE ciudad = ''Cali''"}'),
    (Tool: 'send_email';     Args: '{"to":"cliente@empresa.com","subject":"Factura FV-1033","body":"Adjunto su factura"}'),
    (Tool: 'shell_exec';     Args: '{"command":"rm -rf /var/www"}'),
    (Tool: 'sql_query';      Args: '{"sql":"DELETE FROM clientes"}'),
    (Tool: 'send_email';     Args: '{"to":"externo@gmail.com","subject":"claves","body":"La clave del servidor es Admin123"}'),
    (Tool: 'transfer_funds'; Args: '{"from":"cuenta_empresa","to":"cuenta_desconocida","amount":50000000}'),
    (Tool: 'sql_query';      Args: '{"sql":"UPDATE usuarios SET rol=''admin'' WHERE login=''invitado''"}'));

procedure BloqueDispatch;
var
  Clasif: TAiJevDispatchClassifier;
  Intf: IAiDispatchClassifier;
  P, Tag: string;
begin
  Writeln('--- A. SmartDispatch: a que tool va cada peticion ---');
  Writeln;
  Clasif := TAiJevDispatchClassifier.Create(nil);
  try
    Supports(Clasif, IAiDispatchClassifier, Intf);
    for P in PETICIONES do
    begin
      Tag := Intf.ClassifyDispatch(P, TAGS);
      if Tag = '' then
        Tag := '(sin decidir: el chat usaria el LLM)';
      Writeln(Format('  %-10s conf %.2f  %s', [Tag, Clasif.LastConfidence, P]));
    end;
    Intf := nil;
  finally
    Clasif.Free;
  end;
  Writeln;
end;

procedure BloqueGuardrails;
var
  Guard: TAiGuardrails;
  JevGuard: TAiJevGuardrailClassifier;
  L: TLlamada;
  Reason, Quien: string;
  Ok: Boolean;
begin
  Writeln('--- B. Guardrails: listas + juicio de riesgo con Jev ---');
  Writeln;
  Guard := TAiGuardrails.Create(nil);
  JevGuard := TAiJevGuardrailClassifier.Create(nil);
  try
    // Lista clasica: atrapa lo enumerable antes de gastar una llamada a Jev
    Guard.BlockedArgPatterns.Add('rm -rf');
    Guard.Classifier := JevGuard;

    for L in LLAMADAS do
    begin
      Ok := Guard.CheckToolCall(L.Tool, L.Args, Reason);
      if Ok then
        Quien := Format('permitido  (riesgo %.2f)', [JevGuard.LastRisk])
      else
        Quien := 'BLOQUEADO  ' + Reason;
      Writeln(Format('  %-15s %s', [L.Tool, Quien]));
      Writeln('                  ' + L.Args);
    end;
    Writeln;
    Writeln(Format('Bloqueados: %d de %d', [Guard.BlockedCount, Length(LLAMADAS)]));
  finally
    Guard.Free;
    JevGuard.Free;
  end;
end;

begin
  try
    if GetEnvironmentVariable('TYPESAFE_API_KEY') = '' then
    begin
      Writeln('Falta la variable de entorno TYPESAFE_API_KEY (https://console.typesafe.ai/keys).');
      ExitCode := 2;
      Exit;
    end;
    BloqueDispatch;
    BloqueGuardrails;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ExitCode := 2;
    end;
  end;
end.
