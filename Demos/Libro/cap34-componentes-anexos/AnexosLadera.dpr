program AnexosLadera;

// =============================================================================
// LIBRO "IA con Delphi" - capitulo 34: los componentes anexos, en consola
// =============================================================================
// Dos repositorios que acompanan a MakerAI, con cosas de la tienda:
//   delphi-libraries (Delphi puro, sin DLL, CC0): audio (WAV/FLAC/Vorbis/Opus/MP3/G.722/G.729),
//   pdf (lector/escritor/extractor, AcroForms, visor FMX), extract (13 formatos a Markdown) y
//   barcode (QR generador/lector).
//   MakerAi.ChatTools (MIT): 29 ChatTools para el hueco de capacidades; aqui TAiNativePdfTool.
//   --audio     inspecciona y transcodifica las voces del capitulo 27 (WAV -> FLAC, remuestreo a 48 kHz)
//   --pdf       la factura del capitulo 10: paginas, texto, busqueda con posicion
//   --extract   manual (txt), tripletas (json), skill (md) y la factura (pdf) a Markdown
//   --qr        el QR del Club La Ladera, a PNG
//   --chattool  TAiNativePdfTool como PdfTool de un modelo SIN cap_Pdf (deepseek-flash):
//               la factura entra como texto extraido en local, sin API de documentos
//   (sin argumentos: todo). --chattool requiere DEEPSEEK_API_KEY. Ficticio todo.
// Compilar: DELPHI_LIBRARIES y MAKERAI_CHATTOOLS apuntan a los repos (ver el .dproj).
// =============================================================================

{$APPTYPE CONSOLE}
{$R *.res}

uses
  {$IFDEF MSWINDOWS} Winapi.Windows, {$ENDIF}
  System.SysUtils,
  System.Classes,
  System.StrUtils,
  System.IOUtils,
  System.Diagnostics,
  System.TypInfo,
  // delphi-libraries
  AudioTypes,
  AudioCodec,
  AudioResampler,
  uPDF.Types,
  uPDF.Document,
  uPDF.TextExtractor,
  uPDF.TextSearch,
  uExtract.Engine,
  uExtract.Result,
  QRTypes,
  QREncoder,
  QRImagePNG,
  // MakerAI y su anexo
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Initializations,
  uMakerAi.ChatTools.NativePdf;

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

function Carpeta: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
end;

function Entrada(const ANombre: string): string;
begin
  Result := Carpeta + 'entrada\' + ANombre;
end;

function Salida(const ANombre: string): string;
begin
  Result := Carpeta + 'salida';
  ForceDirectories(Result);
  Result := TPath.Combine(Result, ANombre);
end;

function Resumen(const ATexto: string; AMax: Integer = 160): string;
begin
  Result := ATexto.Replace(#13, ' ').Replace(#10, ' ').Replace('  ', ' ').Trim;
  if Length(Result) > AMax then
    Result := Copy(Result, 1, AMax) + '…';
end;

function KB(const AArchivo: string): Integer;
begin
  Result := Integer(TFile.GetSize(AArchivo) div 1024);
end;

{ ---------------------------------------------------------------------------- A: audio }

procedure Audio;
const
  VOCES: array [0 .. 1] of string = ('llamada-pedido.wav', 'llamada-despedida.wav');
var
  Nombre: string;
  Dec: IAudioDecoder;
  Enc: IAudioEncoder;
  Res: TAudioResampler;
  Buf: TAudioBuffer;
  Info: TAudioInfo;
  Reloj: TStopwatch;
  Frames: Int64;
begin
  Titulo('A. Audio (delphi-libraries/audio): las voces del capítulo 27');
  for Nombre in VOCES do
  begin
    // El factory detecta el formato por los primeros bytes; Info trae todo sin decodificar
    Dec := CreateAudioDecoderFromFile(Entrada(Nombre));
    if (Dec = nil) or not Dec.Ready then
    begin
      Writeln('  ' + Nombre + ': formato no soportado o archivo ausente');
      Continue;
    end;
    Info := Dec.Info;
    // El WAV no declara duracion en la cabecera (DurationMs = -1): se calcula de los bytes de datos
    Writeln(Format('  %-22s %s · %d Hz · %d canal(es) · %d bits · %s · %d KB', [Nombre,
      GetEnumName(TypeInfo(TAudioFormat), Ord(Info.Format)), Info.SampleRate, Info.Channels, Info.BitDepth,
      IfThen(Info.DurationMs > 0, Format('%.1f s', [Info.DurationMs / 1000]), 'duración no declarada'), KB(Entrada(Nombre))]));
  end;
  // Transcodificar la primera: WAV -> FLAC (sin pérdida), bloque a bloque, en Delphi puro
  Nombre := VOCES[0];
  Dec := CreateAudioDecoderFromFile(Entrada(Nombre));
  if (Dec = nil) or not Dec.Ready then
    Exit;
  Reloj := TStopwatch.StartNew;
  Enc := CreateFLACEncoderToFile(Salida('llamada-pedido.flac'), Dec.Info.SampleRate, Dec.Info.Channels, 16);
  Frames := 0;
  while Dec.Decode(Buf) = adrOK do
  begin
    Enc.Encode(Buf);
    Inc(Frames, AudioBufferFrameCount(Buf));   // TAudioBuffer es array of TArray<Single> (planar)
  end;
  Enc.Finalize;
  Writeln(Format('  WAV -> FLAC: %d muestras (%.1f s) · %d KB -> %d KB · %d ms', [Frames, Frames / Dec.Info.SampleRate,
    KB(Entrada(Nombre)), KB(Salida('llamada-pedido.flac')), Reloj.ElapsedMilliseconds]));
  // Remuestreo 24 kHz -> 48 kHz (FIR polifásico): la voz lista para lo que pida 48 kHz
  Reloj := TStopwatch.StartNew;
  Dec := CreateAudioDecoderFromFile(Entrada(Nombre));
  Res := TAudioResampler.Create(Dec.Info.SampleRate, 48000, Dec.Info.Channels);
  try
    Enc := CreateWAVEncoderToFile(Salida('llamada-pedido-48k.wav'), 48000, Dec.Info.Channels);
    while Dec.Decode(Buf) = adrOK do
      Enc.Encode(Res.Resample(Buf));
    Enc.Finalize;
  finally
    Res.Free;
  end;
  Writeln(Format('  WAV 24 kHz -> WAV 48 kHz (remuestreo): %d KB · %d ms', [KB(Salida('llamada-pedido-48k.wav')), Reloj.ElapsedMilliseconds]));
  // El codificador MP3 de la libreria (MPEG-1 Layer III, 32/44,1/48 kHz) devolvio -1 con esta voz
  // mono de 48 kHz, en VBR y en CBR: queda pendiente en delphi-libraries; el remuestreo si sirve
  // para cualquier destino que pida 48 kHz (un TTS, una llamada, un codec).
end;

{ ---------------------------------------------------------------------------- B: pdf }

procedure Pdf;
var
  Doc: TPDFDocument;
  Extr: TPDFTextExtractor;
  Busq: TPDFTextSearch;
  M: TPDFSearchMatch;
  Texto: string;
  Reloj: TStopwatch;
begin
  Titulo('B. PDF (delphi-libraries/pdf): la factura del capítulo 10, sin ningún API');
  Doc := TPDFDocument.Create;
  try
    Reloj := TStopwatch.StartNew;
    Doc.LoadFromFile(Entrada('factura.pdf'));
    Writeln(Format('  %d página(s) · PDF %d.%d · cargado en %d ms', [Doc.PageCount, Doc.Version.Major, Doc.Version.Minor, Reloj.ElapsedMilliseconds]));
    Extr := TPDFTextExtractor.Create(Doc);
    try
      Reloj := TStopwatch.StartNew;
      Texto := Extr.ExtractAllText;
      Writeln(Format('  texto: %d caracteres en %d ms · empieza: %s', [Length(Texto), Reloj.ElapsedMilliseconds, Resumen(Texto, 120)]));
    finally
      Extr.Free;
    end;
    Busq := TPDFTextSearch.Create(Doc);
    try
      for M in Busq.Search('Total') do
        Writeln(Format('  "Total" en la página %d, en (%.0f, %.0f) puntos', [M.PageIndex + 1, M.Bounds.LLX, M.Bounds.LLY]));
    finally
      Busq.Free;
    end;
  finally
    Doc.Free;
  end;
end;

{ ---------------------------------------------------------------------------- C: extract }

procedure Extract;
const
  ARCHIVOS: array [0 .. 3] of string = ('ladera_tienda.txt', 'tripletas.json', 'SKILL.md', 'factura.pdf');
var
  Motor: TAiExtractLib;
  R: TConversionResult;
  Nombre: string;
  Reloj: TStopwatch;
begin
  Titulo('C. Extract (delphi-libraries/extract): cualquier archivo, a Markdown para el modelo');
  Motor := TAiExtractLib.Create;   // registra los 13 convertidores
  try
    for Nombre in ARCHIVOS do
    begin
      Reloj := TStopwatch.StartNew;
      R := Motor.ConvertFile(Entrada(Nombre));
      if R.Success then
        Writeln(Format('  %-18s -> %5d caracteres de Markdown en %3d ms · %s', [Nombre, Length(R.Markdown), Reloj.ElapsedMilliseconds,
          Resumen(R.Markdown, 80)]))
      else
        Writeln(Format('  %-18s -> error: %s', [Nombre, R.ErrorMessage]));
    end;
  finally
    Motor.Free;
  end;
end;

{ ---------------------------------------------------------------------------- D: QR }

procedure QR;
var
  Codigo: TQRCode;
  Reloj: TStopwatch;
begin
  Titulo('D. QR (delphi-libraries/barcode): el enlace del Club La Ladera para la bolsa');
  Reloj := TStopwatch.StartNew;
  Codigo := TQRCode.EncodeText('https://cafelaladera.co/club', qeMedium);   // si cabe, el codificador sube el ECC
  try
    QRSavePNG(Codigo, Salida('club-la-ladera.png'), 8, 4);
    Writeln(Format('  versión %d · %dx%d módulos · ECC %s · %d ms · %s', [Codigo.Version, Codigo.Size, Codigo.Size,
      GetEnumName(TypeInfo(TQRErrorLevel), Ord(Codigo.ErrorLevel)), Reloj.ElapsedMilliseconds, Salida('club-la-ladera.png')]));
  finally
    Codigo.Free;
  end;
end;

{ ---------------------------------------------------------------------------- E: ChatTool }

procedure ChatTool;
var
  Conn: TAiChatConnection;
  Tool: TAiNativePdfTool;
  MF: TAiMediaFile;
  R: string;
  Reloj: TStopwatch;
begin
  Titulo('E. TAiNativePdfTool (MakerAi.ChatTools): un modelo sin PDF lee la factura');
  if GetEnvironmentVariable('DEEPSEEK_API_KEY') = '' then
  begin
    Writeln('  (sin DEEPSEEK_API_KEY)');
    Exit;
  end;
  Conn := TAiChatConnection.Create(nil);
  Tool := TAiNativePdfTool.Create(nil);
  try
    // deepseek-flash no lee PDF (ModelCaps = []). La sesion lo pide (cap_Pdf) y el hueco lo cubre
    // el PdfTool: la libreria pdf extrae el texto en local y el modelo recibe texto.
    Conn.DriverName := 'DeepSeek';
    Conn.Model := 'deepseek-flash';
    Conn.Params.Values['ApiKey'] := '@DEEPSEEK_API_KEY';
    Conn.Params.Values['Asynchronous'] := 'False';
    Conn.SessionCaps := [cap_Pdf];
    Conn.ChatTools.PdfTool := Tool;
    Conn.SystemPrompt.Text := 'Eres Lucía, de Café La Ladera. Responde en español, breve.';
    MF := TAiMediaFile.Create;     // pasa a ser del mensaje
    MF.LoadFromFile(Entrada('factura.pdf'));
    Reloj := TStopwatch.StartNew;
    R := Conn.AddMessageAndRun('¿Quién emite esta factura, qué se compró y cuál es el total?', 'user', [MF]);
    if (R = '') and (Conn.LastError <> '') then
      Writeln('  error: ' + Resumen(Conn.LastError, 200))
    else
      Writeln(Format('  %s / %s (%.1f s · %d tokens de entrada): %s', [Conn.DriverName, Conn.AiChat.Model,
        Reloj.ElapsedMilliseconds / 1000, Conn.Prompt_tokens, Resumen(R, 300)]));
  finally
    Conn.Free;
    Tool.Free;
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
    Todo := not (Tiene('--audio') or Tiene('--pdf') or Tiene('--extract') or Tiene('--qr') or Tiene('--chattool'));
    if Todo or Tiene('--audio') then
      Audio;
    if Todo or Tiene('--pdf') then
      Pdf;
    if Todo or Tiene('--extract') then
      Extract;
    if Todo or Tiene('--qr') then
      QR;
    if Todo or Tiene('--chattool') then
      ChatTool;
  except
    on E: Exception do
      Writeln(E.ClassName + ': ' + E.Message);
  end;
end.
