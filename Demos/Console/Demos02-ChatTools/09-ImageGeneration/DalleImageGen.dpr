program DalleImageGen;

{$APPTYPE CONSOLE}
{$R *.res}

// =============================================================================
// MakerAI — 02-ChatTools / 09-ImageGeneration
// =============================================================================
// Demo de generaci?n de im?genes con TAiDalle.
//
// Cubre los nuevos modelos y par?metros de la API OpenAI Images (v2):
//
//   Test 1 — gpt-image-1      : PNG, quality=high, background=opaque
//   Test 2 — gpt-image-1-mini : JPEG, quality=medium, output_compression=80
//   Test 3 — gpt-image-2      : PNG, quality=high (SIN streaming)
//   Test 4 — gpt-image-1      : Streaming con partial_images=2
//   Test 5 — dall-e-3         : Generaci?n cl?sica
//   Test 6 — gpt-image-1      : size=auto, moderation=low
//
// Las im?genes se guardan en la carpeta TEMP del sistema.
// Requisito: variable de entorno OPENAI_API_KEY definida.
//
// Nota sobre streaming en consola:
//   TAiDalle usa TThread.Queue para disparar los callbacks de streaming.
//   En apps de consola (sin message pump), los callbacks se procesan
//   al llamar CheckSynchronize() despu?s de que Generate() retorna.
// =============================================================================

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  uMakerAi.Core,
  uMakerAi.Chat.Messages,
  uMakerAi.OpenAi.Dalle;

const
  API_KEY = '@OPENAI_API_KEY';

// =============================================================================
//  Helpers globales
// =============================================================================

function OutputDir: string;
begin
  Result := TPath.GetTempPath;
end;

procedure SaveImage(const AFilename: string; AStream: TMemoryStream);
var
  FullPath: string;
begin
  if AStream.Size = 0 then Exit;
  FullPath := TPath.Combine(OutputDir, AFilename);
  AStream.Position := 0;
  AStream.SaveToFile(FullPath);
  Writeln('    Guardada: ', FullPath);
end;

procedure PrintUsage(AImage: TAiDalleImage);
var
  InputTok, OutputTok, TotalTok: Integer;
begin
  if not Assigned(AImage.Usage) then Exit;
  InputTok  := 0;
  OutputTok := 0;
  TotalTok  := 0;
  AImage.Usage.TryGetValue<Integer>('input_tokens', InputTok);
  AImage.Usage.TryGetValue<Integer>('output_tokens', OutputTok);
  AImage.Usage.TryGetValue<Integer>('total_tokens', TotalTok);
  Writeln(Format('    Tokens — entrada: %d  salida: %d  total: %d',
    [InputTok, OutputTok, TotalTok]));
end;

// =============================================================================
//  Test 1 — gpt-image-1: PNG de alta calidad
// =============================================================================

procedure Test1_GptImage1_PNG;
var
  Dalle: TAiDalle;
  Img: TAiDalleImage;
begin
  Writeln;
  Writeln('=== Test 1: gpt-image-1 — PNG, quality=high ===');
  Dalle := TAiDalle.Create(nil);
  try
    Dalle.ApiKey       := API_KEY;
    Dalle.Model        := imGptImage1;
    Dalle.Quality      := iqHigh;
    Dalle.Background   := ibOpaque;
    Dalle.OutputFormat := ifPng;

    Write('  Generando... ');
    Img := Dalle.Generate(
      'A serene Japanese garden at sunset with a koi pond, cherry blossoms, ' +
      'and a wooden bridge. Photorealistic, high detail.',
      '', is1024x1024);

    if Assigned(Img) then
    begin
      Writeln('OK  (', Img.Image.Size, ' bytes)');
      if Img.RevisedPrompt <> '' then
        Writeln('  Revised: ', Copy(Img.RevisedPrompt, 1, 80), '...');
      PrintUsage(Img);
      SaveImage('test1_gptimage1_hq.png', Img.Image);
    end
    else
      Writeln('ERROR: respuesta vacia');
  finally
    Dalle.Free;
  end;
end;

// =============================================================================
//  Test 2 — gpt-image-1-mini: JPEG con compresi?n
// =============================================================================

procedure Test2_GptImage1Mini_JPEG;
var
  Dalle: TAiDalle;
  Img: TAiDalleImage;
begin
  Writeln;
  Writeln('=== Test 2: gpt-image-1-mini — JPEG, quality=medium, compression=80 ===');
  Dalle := TAiDalle.Create(nil);
  try
    Dalle.ApiKey            := API_KEY;
    Dalle.Model             := imGptImage1Mini;
    Dalle.Quality           := iqMedium;
    Dalle.OutputFormat      := ifJpeg;
    Dalle.OutputCompression := 80;

    Write('  Generando... ');
    Img := Dalle.Generate(
      'A minimalist logo for a tech startup: clean lines, blue gradient, ' +
      'abstract letter "M" made of circuit board traces.',
      '', is1024x1024);

    if Assigned(Img) then
    begin
      Writeln('OK  (', Img.Image.Size, ' bytes)');
      PrintUsage(Img);
      SaveImage('test2_gptimage1mini_medium.jpg', Img.Image);
    end
    else
      Writeln('ERROR: respuesta vacia');
  finally
    Dalle.Free;
  end;
end;

// =============================================================================
//  Test 3 — gpt-image-2: sin streaming (la API no lo soporta)
// =============================================================================

procedure Test3_GptImage2;
var
  Dalle: TAiDalle;
  Img: TAiDalleImage;
begin
  Writeln;
  Writeln('=== Test 3: gpt-image-2 — PNG, quality=high (sin streaming) ===');
  Writeln('  (Requiere verificacion de organizacion en platform.openai.com)');
  Dalle := TAiDalle.Create(nil);
  try
    Dalle.ApiKey       := API_KEY;
    Dalle.Model        := imGptImage2;
    Dalle.Quality      := iqHigh;
    Dalle.OutputFormat := ifPng;
    Dalle.Stream       := False; // gpt-image-2 no soporta streaming

    Write('  Generando... ');
    try
      Img := Dalle.Generate(
        'An elegant product photo of a luxury wristwatch on a marble surface, ' +
        'dramatic studio lighting, ultra-sharp focus.',
        '', is1024x1024);

      if Assigned(Img) then
      begin
        Writeln('OK  (', Img.Image.Size, ' bytes)');
        PrintUsage(Img);
        SaveImage('test3_gptimage2.png', Img.Image);
      end
      else
        Writeln('ERROR: respuesta vacia');
    except
      on E: Exception do
        Writeln('SKIP — ', E.Message);
    end;
  finally
    Dalle.Free;
  end;
end;

// =============================================================================
//  Test 4 — gpt-image-1: streaming con partial_images
//  Los eventos "of object" requieren metodos de clase; usamos TStreamingTest.
// =============================================================================

type
  TStreamingTest = class
  private
    FPartialCount  : Integer;
    FFinalReceived : Boolean;
    procedure OnPartial(Sender: TObject; const AImage: TAiDalleImage; AIndex: Integer);
    procedure OnCompleted(Sender: TObject; const AImage: TAiDalleImage);
    procedure OnError(Sender: TObject; const AMsg: string);
  public
    procedure Run;
  end;

procedure TStreamingTest.OnPartial(Sender: TObject;
  const AImage: TAiDalleImage; AIndex: Integer);
begin
  Inc(FPartialCount);
  Writeln(Format('    [Parcial #%d] %d bytes', [AIndex, AImage.Image.Size]));
  SaveImage(Format('test4_partial_%d.webp', [AIndex]), AImage.Image);
  AImage.Free; // responsabilidad del handler
end;

procedure TStreamingTest.OnCompleted(Sender: TObject;
  const AImage: TAiDalleImage);
begin
  FFinalReceived := True;
  Writeln('    [Final] ', AImage.Image.Size, ' bytes');
  SaveImage('test4_final.webp', AImage.Image);
  AImage.Free;
end;

procedure TStreamingTest.OnError(Sender: TObject; const AMsg: string);
begin
  Writeln('    [Error streaming] ', AMsg);
end;

procedure TStreamingTest.Run;
var
  Dalle: TAiDalle;
begin
  Writeln;
  Writeln('=== Test 4: gpt-image-1 — Streaming, partial_images=2 ===');
  Writeln('  Callbacks directos desde el hilo de Generate().');

  FPartialCount  := 0;
  FFinalReceived := False;

  Dalle := TAiDalle.Create(nil);
  try
    Dalle.ApiKey        := API_KEY;
    Dalle.Model         := imGptImage1;
    Dalle.Quality       := iqMedium;
    Dalle.OutputFormat  := ifWebp;
    Dalle.Stream        := True;
    Dalle.PartialImages := 2;

    Dalle.OnPartialImageReceived := OnPartial;
    Dalle.OnStreamCompleted      := OnCompleted;
    Dalle.OnStreamError          := OnError;

    Write('  Generando (streaming)... ');
    Dalle.Generate(
      'A futuristic cityscape at night with neon lights reflecting on wet streets, ' +
      'flying vehicles, and holographic advertisements. Cyberpunk aesthetic.',
      '', is1024x1024);

    Writeln('OK');
    Writeln(Format('  Parciales: %d | Final recibido: %s',
      [FPartialCount, BoolToStr(FFinalReceived, True)]));
  finally
    Dalle.Free;
  end;
end;

// =============================================================================
//  Test 5 — dall-e-3: modo cl?sico
// =============================================================================

procedure Test5_DallE3;
var
  Dalle: TAiDalle;
  Img: TAiDalleImage;
begin
  Writeln;
  Writeln('=== Test 5: dall-e-3 — HD, style=natural, size=1792x1024 ===');
  Dalle := TAiDalle.Create(nil);
  try
    Dalle.ApiKey         := API_KEY;
    Dalle.Model          := imDallE3;
    Dalle.Quality        := iqHD;
    Dalle.Style          := isNatural;
    Dalle.ResponseFormat := irfBase64Json;

    Write('  Generando... ');
    Img := Dalle.Generate(
      'A vast mountain landscape with a crystal-clear lake in the foreground, ' +
      'golden hour light, dramatic clouds. Photorealistic.',
      '', is1792x1024);

    if Assigned(Img) then
    begin
      Writeln('OK  (', Img.Image.Size, ' bytes)');
      if Img.RevisedPrompt <> '' then
        Writeln('  Revised: ', Copy(Img.RevisedPrompt, 1, 80), '...');
      SaveImage('test5_dalle3_hd.png', Img.Image);
    end
    else
      Writeln('ERROR: respuesta vacia');
  finally
    Dalle.Free;
  end;
end;

// =============================================================================
//  Test 6 — gpt-image-1: size=auto + moderation=low
// =============================================================================

procedure Test6_AutoSize_Moderation;
var
  Dalle: TAiDalle;
  Img: TAiDalleImage;
begin
  Writeln;
  Writeln('=== Test 6: gpt-image-1 — size=auto, moderation=low ===');
  Dalle := TAiDalle.Create(nil);
  try
    Dalle.ApiKey     := API_KEY;
    Dalle.Model      := imGptImage1;
    Dalle.Quality    := iqMedium;
    Dalle.Moderation := imodLow;

    Write('  Generando con size=auto... ');
    Img := Dalle.Generate(
      'A fantasy warrior character in full plate armor, dynamic pose, ' +
      'detailed illustration style.',
      '', isAuto);

    if Assigned(Img) then
    begin
      Writeln('OK  (', Img.Image.Size, ' bytes)');
      Writeln('  Size devuelto: ', Img.Size);
      PrintUsage(Img);
      SaveImage('test6_autosize.png', Img.Image);
    end
    else
      Writeln('ERROR: respuesta vacia');
  finally
    Dalle.Free;
  end;
end;

// =============================================================================
//  Punto de entrada
// =============================================================================

var
  StreamingTest: TStreamingTest;

begin
  Writeln('================================================');
  Writeln(' MakerAI — Demo 09: Generacion de Imagenes');
  Writeln(' gpt-image-1 / gpt-image-1-mini / gpt-image-2');
  Writeln(' gpt-image-1.5 / chatgpt-image-latest / dall-e-3');
  Writeln('================================================');
  Writeln('Salida: ', OutputDir);

  try
    Test1_GptImage1_PNG;
    Test2_GptImage1Mini_JPEG;
    Test3_GptImage2;

    StreamingTest := TStreamingTest.Create;
    try
      StreamingTest.Run;
    finally
      StreamingTest.Free;
    end;

    Test5_DallE3;
    Test6_AutoSize_Moderation;

  except
    on E: Exception do
      Writeln('ERROR FATAL: ', E.ClassName, ' — ', E.Message);
  end;

  Writeln;
  Writeln('================================================');
  Writeln('Demo finalizado. Presiona Enter para salir...');
  Readln;
end.
