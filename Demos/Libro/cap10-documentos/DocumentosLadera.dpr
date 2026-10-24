program DocumentosLadera;

// =============================================================================
// LIBRO "IA con Delphi", capitulo 10 - Documentos: el PDF llega al modelo
// =============================================================================
// La factura de un proveedor (media\factura.pdf) con tres cerebros:
//   A. OpenAI gpt-6-sol: lee el PDF por si mismo (cap_Pdf en ModelCaps).
//   B. Claude: tambien lo lee por si mismo.
//   C. Groq gpt-oss-20b: solo texto. Sin cap_Pdf y sin un PdfTool en ChatTools,
//      el framework avisa del hueco en vez de mandar un archivo que el modelo
//      no puede leer.
//
// Uso: DocumentosLadera.exe [media\factura.pdf]
// Empresa, productos y precios ficticios.
// =============================================================================

{$APPTYPE CONSOLE}
{$R *.res}

uses
  {$IFDEF MSWINDOWS} Winapi.Windows, {$ENDIF}
  System.SysUtils, System.Classes, System.Diagnostics, System.IOUtils,
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations;

const
  PROMPT_SISTEMA =
    'Eres Lucía, la barista virtual de Café La Ladera, una tienda de café de especialidad colombiano.' + sLineBreak +
    'Si te mandan un PDF (una factura, una ficha técnica), resume lo importante: cifras, fechas, totales.' + sLineBreak +
    'Responde en español, breve. Solo hablas de café y de la tienda.';
  PREGUNTA = 'Esta es la factura de un proveedor. ¿Cuánto debemos pagar, cuándo vence y qué compramos?';

procedure Titulo(const S: string);
begin
  Writeln('==== ', S, ' ', StringOfChar('=', 62 - Length(S)));
  Writeln;
end;

procedure Preguntar(const ADriver, AModel, ARuta: string; const AModelCaps, ASessionCaps: TAiCapabilities);
var
  Conn: TAiChatConnection;
  Pdf: TAiMediaFile;
  R: string;
  Reloj: TStopwatch;
begin
  Conn := TAiChatConnection.Create(nil);
  Pdf := TAiMediaFile.Create;
  try
    Conn.DriverName := ADriver;
    Conn.Model := AModel;
    Conn.Params.Values['Asynchronous'] := 'False';
    Conn.SystemPrompt.Text := PROMPT_SISTEMA;
    Conn.ModelCaps := AModelCaps;       // lo que el modelo lee por si mismo (fija el conjunto completo)
    Conn.SessionCaps := ASessionCaps;   // lo que la sesion necesita
    Pdf.LoadFromFile(ARuta);                 // FileCategory = Tfc_Pdf, MimeType = application/pdf
    Writeln(Format('-- %s / %s', [Conn.DriverName, Conn.AiChat.Model]));
    Reloj := TStopwatch.StartNew;
    // El PDF viaja con la pregunta. El mensaje se queda con el archivo y lo libera
    // con el historial: no hay que liberarlo aparte.
    R := Conn.AddMessageAndRun(PREGUNTA, 'user', [Pdf]);
    Writeln(Format('   ModelCaps con cap_Pdf: %s · SessionCaps con cap_Pdf: %s',
      [BoolToStr(cap_Pdf in Conn.ModelCaps, True), BoolToStr(cap_Pdf in Conn.SessionCaps, True)]));
    if R = '' then
      Writeln('   [sin respuesta] ', Conn.LastError.Replace(sLineBreak, ' ').Substring(0, 220))
    else
      Writeln('   ', R.Trim.Replace(sLineBreak, sLineBreak + '   '));
    Writeln(Format('   (%d ms, %d/%d tokens)', [Reloj.ElapsedMilliseconds, Conn.Prompt_tokens, Conn.Completion_tokens]));
    Writeln;
  finally
    Conn.Free;
  end;
end;

var
  Ruta: string;

begin
  {$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);
  SetTextCodePage(Output, CP_UTF8);
  {$ENDIF}
  if ParamCount >= 1 then
    Ruta := ParamStr(1)
  else
    Ruta := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), '..\..\media\factura.pdf');
  Ruta := TPath.GetFullPath(Ruta);
  Writeln('PDF: ', Ruta, ' (', TFile.GetSize(Ruta), ' bytes)');
  Writeln('Tú: ', PREGUNTA);
  Writeln;
  try
    Titulo('A. OpenAI lee el PDF por sí mismo');
    Preguntar('OpenAi', '', Ruta, [cap_Image, cap_Pdf, cap_Reasoning], [cap_Image, cap_Pdf, cap_Reasoning]);
    Titulo('B. Claude también');
    Preguntar('Claude', '', Ruta, [cap_Image, cap_Pdf, cap_Reasoning], [cap_Image, cap_Pdf, cap_Reasoning]);
    Titulo('C. Un modelo de solo texto: la sesión pide PDF y no hay puente');
    Preguntar('Groq', '', Ruta, [], [cap_Pdf]);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
