program ImagenLadera;

// =============================================================================
// LIBRO "IA con Delphi" - capitulo 31: imagenes y video, en consola
// =============================================================================
// La misma llamada de siempre (AddMessageAndRun) con modelos que no conversan sino que generan.
// El catalogo trae para ellos ModelCaps=[] y SessionCaps=[cap_GenImage] o [cap_GenVideo]: ese
// hueco manda la peticion al endpoint de imagenes o al job de video y el resultado vuelve como
// TAiMediaFile en el ultimo mensaje.
//   --imagen    gpt-image-2.5-flare por el conector: la foto de producto de la bolsa y una version
//               con fondo transparente para la tienda en linea (ImageParams)
//   --directo   TAiDalle sin conector: Generate con streaming de imagenes parciales
//   --grok      grok-imagine-image por el conector (otro proveedor, mismo hueco)
//   --gemini    gemini-3.1-flash-image: generacion NATIVA (el modelo de chat devuelve imagen)
//   --video     grok-imagine-video: job asincrono, 5 segundos de video, un minuto de espera
//   (sin argumentos: todo). Las salidas quedan en salida\ junto al exe.
// Requiere OPENAI_API_KEY; GROK_API_KEY y GEMINI_API_KEY para sus secciones. Ficticio todo.
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
  uMakerAi.Core,
  uMakerAi.Chat,
  uMakerAi.Chat.Messages,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.Grok,
  uMakerAi.OpenAi.Dalle,
  uMakerAi.Chat.Initializations;

const
  BOLSA = 'Fotografía de producto para una tienda en línea: una bolsa de café de 340 g, color kraft con etiqueta ' +
    'granate que dice "Huila Reserva · Café La Ladera", de pie sobre una mesa de madera clara, con algunos granos de café ' +
    'tostado alrededor y una taza de café al fondo desenfocada. Luz natural suave de ventana, fondo neutro, estilo realista.';
  REDES = 'Ilustración plana y cálida para redes sociales: una taza de café humeante sobre una montaña verde con niebla ' +
    'al amanecer, en tonos granate, crema y verde. Sin texto.';
  ESCENA = 'Un café de filtro cayendo lentamente sobre una taza de cerámica, en una mesa de madera, con luz cálida de la ' +
    'mañana y vapor subiendo; la cámara se acerca despacio.';

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

function Salida(const ANombre: string): string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'salida';
  ForceDirectories(Result);
  Result := TPath.Combine(Result, ANombre);
end;

function Conexion(const ADriver, AModelo, AKey: string): TAiChatConnection;
begin
  Result := TAiChatConnection.Create(nil);
  Result.DriverName := ADriver;
  Result.Model := AModelo;
  Result.Params.Values['ApiKey'] := AKey;
  Result.Params.Values['Asynchronous'] := 'False';
  Result.AiChat.ResponseTimeOut := 300000;   // generar tarda: imagenes hasta un minuto, video varios
end;

// Pide una imagen o un video por el conector y guarda el primer archivo que vuelva
function Generar(Conn: TAiChatConnection; const APrompt, AArchivo: string): Boolean;
var
  R: string;
  Msg: TAiChatMessage;
  Reloj: TStopwatch;
begin
  Result := False;
  Reloj := TStopwatch.StartNew;
  try
    R := Conn.AddMessageAndRun(APrompt, 'user', []);
  except
    on E: Exception do
    begin
      Writeln('  error: ' + E.Message);
      Exit;
    end;
  end;
  if Conn.LastError <> '' then
  begin
    Writeln('  error: ' + Copy(Conn.LastError.Replace(#13, ' ').Replace(#10, ' '), 1, 220));
    Exit;
  end;
  Msg := Conn.AiChat.GetLastMessage;
  if (not Assigned(Msg)) or (Msg.MediaFiles.Count = 0) then
  begin
    Writeln('  sin archivo en la respuesta: ' + Copy(R, 1, 200));
    Exit;
  end;
  Msg.MediaFiles[0].SaveToFile(Salida(AArchivo));
  Writeln(Format('  %s · %d KB · %.1f s · %s', [Conn.AiChat.Model, Msg.MediaFiles[0].Content.Size div 1024,
    Reloj.ElapsedMilliseconds / 1000, Salida(AArchivo)]));
  if (R <> '') and (R <> APrompt) then
    Writeln('  respuesta: ' + Copy(R.Replace(#13, ' ').Replace(#10, ' '), 1, 160));
  Result := True;
end;

{ ---------------------------------------------------------------------------- A: por el conector }

procedure Imagen;
var
  Conn: TAiChatConnection;
begin
  Titulo('A. gpt-image-2.5-flare por el conector: ModelCaps=[] + SessionCaps=[cap_GenImage]');
  Conn := Conexion('OpenAi', 'gpt-image-2.5-flare', '@OPENAI_API_KEY');
  try
    // Los parametros del endpoint viajan en ImageParams: calidad, tamano, fondo, formato
    Conn.ImageParams.Params.Values['quality'] := 'medium';
    Conn.ImageParams.Params.Values['size'] := '1024x1024';
    Writeln('  foto de producto (calidad medium, 1024x1024)');
    Generar(Conn, BOLSA, 'bolsa.png');
    Conn.NewChat;
    Conn.ImageParams.Params.Values['background'] := 'transparent';   // PNG con canal alfa real (2.5)
    Conn.ImageParams.Params.Values['output_format'] := 'png';
    Writeln('  la misma bolsa sin fondo, para la tienda en línea (background=transparent)');
    Generar(Conn, 'Solo la bolsa de café kraft con la etiqueta granate "Huila Reserva · Café La Ladera", de frente, ' +
      'sin mesa ni fondo: fondo transparente.', 'bolsa_sin_fondo.png');
  finally
    Conn.Free;
  end;
end;

{ ---------------------------------------------------------------------------- B: TAiDalle directo }

type
  TOyente = class
    Parciales: Integer;
    procedure Parcial(Sender: TObject; const APartialImage: TAiDalleImage; AIndex: Integer);
    procedure Listo(Sender: TObject; const AFinalImage: TAiDalleImage);
  end;

procedure TOyente.Parcial(Sender: TObject; const APartialImage: TAiDalleImage; AIndex: Integer);
begin
  Inc(Parciales);
  APartialImage.Image.SaveToFile(Salida(Format('redes_parcial_%d.png', [AIndex + 1])));
  Writeln(Format('  parcial %d: %d KB', [AIndex + 1, APartialImage.Image.Size div 1024]));
end;

procedure TOyente.Listo(Sender: TObject; const AFinalImage: TAiDalleImage);
begin
  Writeln('  streaming terminado');
end;

procedure Directo;
var
  Dalle: TAiDalle;
  Img: TAiDalleImage;
  Oyente: TOyente;
  Reloj: TStopwatch;
begin
  Titulo('B. TAiDalle sin conector: Generate, con el prompt revisado y el uso');
  Dalle := TAiDalle.Create(nil);
  Oyente := TOyente.Create;
  try
    Dalle.ApiKey := '@OPENAI_API_KEY';
    Dalle.Model := imGptImage25Flare;
    Dalle.Quality := iqMedium;
    // Stream := True con PartialImages manda versiones intermedias por OnPartialImageReceived; en esta
    // edicion, con gpt-image-2.5-flare, el API las envio pero el componente no disparo los eventos
    // (pendiente de revision): se usa la llamada completa.
    Dalle.Stream := False;
    Dalle.OnPartialImageReceived := Oyente.Parcial;
    Dalle.OnStreamCompleted := Oyente.Listo;
    Reloj := TStopwatch.StartNew;
    Img := Dalle.Generate(REDES, '', is1024x1024);
    try
      if Assigned(Img) then
      begin
        Img.Image.SaveToFile(Salida('redes.png'));
        Writeln(Format('  final: %d KB · %.1f s · %s', [Img.Image.Size div 1024, Reloj.ElapsedMilliseconds / 1000, Salida('redes.png')]));
        if Img.RevisedPrompt <> '' then
          Writeln('  prompt revisado: ' + Copy(Img.RevisedPrompt, 1, 160));
        if Assigned(Img.Usage) then
          Writeln('  usage: ' + Img.Usage.ToJSON);
      end
      else
        Writeln('  sin imagen');
    finally
      Img.Free;
    end;
  finally
    Oyente.Free;
    Dalle.Free;
  end;
end;

{ ---------------------------------------------------------------------------- C, D, E }

procedure Grok;
var
  Conn: TAiChatConnection;
begin
  Titulo('C. grok-imagine-image por el conector: otro proveedor, el mismo hueco');
  if GetEnvironmentVariable('GROK_API_KEY') = '' then
  begin
    Writeln('  (sin GROK_API_KEY)');
    Exit;
  end;
  Conn := Conexion('Grok', 'grok-imagine-image', '@GROK_API_KEY');
  try
    Generar(Conn, REDES, 'redes_grok.png');
  finally
    Conn.Free;
  end;
end;

procedure Gemini;
var
  Conn: TAiChatConnection;
begin
  Titulo('D. gemini-3.1-flash-image: generacion nativa (ModelCaps=[cap_Image, cap_GenImage], sin hueco)');
  if GetEnvironmentVariable('GEMINI_API_KEY') = '' then
  begin
    Writeln('  (sin GEMINI_API_KEY)');
    Exit;
  end;
  Conn := Conexion('Gemini', 'gemini-3.1-flash-image', '@GEMINI_API_KEY');
  try
    Generar(Conn, REDES, 'redes_gemini.png');
  finally
    Conn.Free;
  end;
end;

procedure Video;
var
  Conn: TAiChatConnection;
begin
  Titulo('E. grok-imagine-video: job asincrono, 5 segundos de video');
  if GetEnvironmentVariable('GROK_API_KEY') = '' then
  begin
    Writeln('  (sin GROK_API_KEY)');
    Exit;
  end;
  Conn := Conexion('Grok', 'grok-imagine-video', '@GROK_API_KEY');
  try
    (Conn.AiChat as TAiGrokChat).VideoDurationSeconds := 5;   // hasta 15; se cobra por segundo
    Writeln('  el driver abre el job, consulta cada 3 s y descarga el MP4 cuando termina...');
    Generar(Conn, ESCENA, 'cafe.mp4');
  finally
    Conn.Free;
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
  if GetEnvironmentVariable('OPENAI_API_KEY') = '' then
  begin
    Writeln('Falta OPENAI_API_KEY.');
    ExitCode := 1;
    Exit;
  end;
  try
    Todo := not (Tiene('--imagen') or Tiene('--directo') or Tiene('--grok') or Tiene('--gemini') or Tiene('--video'));
    if Todo or Tiene('--imagen') then
      Imagen;
    if Todo or Tiene('--directo') then
      Directo;
    if Todo or Tiene('--grok') then
      Grok;
    if Todo or Tiene('--gemini') then
      Gemini;
    if Todo or Tiene('--video') then
      Video;
  except
    on E: Exception do
      Writeln(E.ClassName + ': ' + E.Message);
  end;
end.
