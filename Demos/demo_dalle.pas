program demo_dalle;
{$mode objfpc}{$H+}

// Demo: TAiDalle — generacion de imagenes con DALL-E 3 y DALL-E 2
//
// Escenarios:
//   1. DALL-E 3 (1024x1024, URL) — descarga automatica, guarda PNG
//   2. DALL-E 2 (512x512, b64_json) — imagen en base64, guarda PNG
//   3. Muestra metadatos: revised_prompt, quality, size
//
// Requisitos:
//   Variable de entorno OPENAI_API_KEY con clave valida.
//
// Compilar con:
//   fpc demo_dalle.pas -Fu../Source/Core -Fu../Source/Chat -Fu../Source/Tools

uses
  SysUtils, Classes,
  uMakerAi.Core,
  uMakerAi.OpenAi.Dalle;

// ---------------------------------------------------------------------------
// Guarda la imagen del resultado en disco
// ---------------------------------------------------------------------------
procedure SaveDalleImage(Img: TAiDalleImage; const OutFile: string);
var
  F: TFileStream;
  S: TMemoryStream;
begin
  S := Img.Image; // descarga desde URL si es necesario
  if Assigned(S) and (S.Size > 0) then
  begin
    F := TFileStream.Create(OutFile, fmCreate);
    try
      S.Position := 0;
      F.CopyFrom(S, S.Size);
    finally
      F.Free;
    end;
    WriteLn('  Guardado: ', OutFile, '  (', S.Size div 1024, ' KB)');
  end
  else
    WriteLn('  Sin imagen en el resultado.');
end;

// ---------------------------------------------------------------------------
var
  Dalle: TAiDalle;
  Img:   TAiDalleImage;

begin
  WriteLn('=== MakerAI FPC — Demo DALL-E ===');
  WriteLn;

  Dalle := TAiDalle.Create(nil);
  try
    Dalle.ApiKey := '@OPENAI_API_KEY';

    // -----------------------------------------------------------------------
    // Escenario 1: DALL-E 3 con respuesta URL (la imagen se descarga al leer Img.Image)
    // -----------------------------------------------------------------------
    WriteLn('--- Escenario 1: DALL-E 3  1024x1024  formato URL ---');
    Dalle.Model          := imDallE3;
    Dalle.Quality        := iqStandard;
    Dalle.Style          := isVivid;
    Dalle.ResponseFormat := irfUrl;

    WriteLn('  Prompt: "A futuristic city at sunset with flying cars, pixel art style"');
    WriteLn('  Generando... (puede tardar varios segundos)');

    Img := Dalle.Generate(
      'A futuristic city at sunset with flying cars, pixel art style',
      '', is1024x1024);
    try
      if Img.RevisedPrompt <> '' then
        WriteLn('  Revised prompt: ', Copy(Img.RevisedPrompt, 1, 100), '...');
      WriteLn('  URL devuelta: ', Copy(Img.UrlFile, 1, 80), '...');
      SaveDalleImage(Img, 'dalle3_output.png');
    finally
      Img.Free;
    end;
    WriteLn;

    // -----------------------------------------------------------------------
    // Escenario 2: DALL-E 2 con respuesta b64_json (imagen directa en memoria)
    // -----------------------------------------------------------------------
    WriteLn('--- Escenario 2: DALL-E 2  512x512  formato b64_json ---');
    Dalle.Model          := imDallE2;
    Dalle.Quality        := iqStandard;
    Dalle.ResponseFormat := irfBase64Json;

    WriteLn('  Prompt: "A simple red apple on a white background, watercolor style"');
    WriteLn('  Generando...');

    Img := Dalle.Generate(
      'A simple red apple on a white background, watercolor style',
      '', is512x512);
    try
      WriteLn('  Base64 bytes recibidos: ', Length(Img.Base64));
      SaveDalleImage(Img, 'dalle2_output.png');
    finally
      Img.Free;
    end;
    WriteLn;

    // -----------------------------------------------------------------------
    // Escenario 3: DALL-E 3 HD calidad
    // -----------------------------------------------------------------------
    WriteLn('--- Escenario 3: DALL-E 3  HD  1024x1792 (portrait) ---');
    Dalle.Model          := imDallE3;
    Dalle.Quality        := iqHD;
    Dalle.Style          := isNatural;
    Dalle.ResponseFormat := irfBase64Json;

    WriteLn('  Prompt: "A majestic lighthouse on a rocky coast at dawn"');
    WriteLn('  Generando (HD tarda mas)...');

    Img := Dalle.Generate(
      'A majestic lighthouse on a rocky coast at dawn',
      '', is1024x1792);
    try
      if Img.RevisedPrompt <> '' then
        WriteLn('  Revised prompt: ', Copy(Img.RevisedPrompt, 1, 100), '...');
      SaveDalleImage(Img, 'dalle3_hd_output.png');
    finally
      Img.Free;
    end;
    WriteLn;

  finally
    Dalle.Free;
  end;

  WriteLn('Demo DALL-E finalizado correctamente.');
end.
