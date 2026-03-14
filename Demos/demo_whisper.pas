program demo_whisper;
{$mode objfpc}{$H+}

// Demo: TAIWhisper — Text-to-Speech y Speech-to-Text con OpenAI Whisper
//
// Escenarios:
//   1. TTS  — convierte texto a voz MP3 y guarda en disco
//   2. STT  — transcribe el MP3 generado de vuelta a texto
//   3. Voces distintas — genera con voz alternativa
//
// El demo hace un ciclo completo:  texto -> MP3 -> texto transcripto
// Asi se puede verificar TTS y STT sin archivos de audio externos.
//
// Requisitos:
//   Variable de entorno OPENAI_API_KEY con clave valida.
//
// Compilar con:
//   fpc demo_whisper.pas -Fu../Source/Core -Fu../Source/Chat -Fu../Source/Tools

uses
  SysUtils, Classes,
  uMakerAi.Core,
  uMakerAi.Chat.Messages,
  uMakerAi.Whisper;

// ---------------------------------------------------------------------------
// Guarda TMemoryStream en disco
// ---------------------------------------------------------------------------
procedure SaveStream(AStream: TMemoryStream; const AFile: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(AFile, fmCreate);
  try
    AStream.Position := 0;
    F.CopyFrom(AStream, AStream.Size);
  finally
    F.Free;
  end;
  WriteLn('  Guardado: ', AFile, '  (', AStream.Size div 1024, ' KB)');
end;

// ---------------------------------------------------------------------------
// Carga un archivo de audio en un TMemoryStream
// ---------------------------------------------------------------------------
function LoadStream(const AFile: string): TMemoryStream;
begin
  Result := TMemoryStream.Create;
  Result.LoadFromFile(AFile);
  Result.Position := 0;
end;

// ---------------------------------------------------------------------------
var
  Whisper:  TAIWhisper;
  AudioOut: TMemoryStream;
  AudioIn:  TMemoryStream;
  Transcript: string;

const
  TTS_FILE_1 = 'whisper_nova.mp3';
  TTS_FILE_2 = 'whisper_onyx.mp3';

  TEXT_1 = 'Hello! This is a test of the OpenAI text-to-speech engine using the FPC MakerAI library.';
  TEXT_2 = 'La inteligencia artificial esta transformando el mundo del desarrollo de software.';

begin
  WriteLn('=== MakerAI FPC — Demo Whisper TTS + STT ===');
  WriteLn;

  Whisper := TAIWhisper.Create(nil);
  try
    Whisper.ApiKey := '@OPENAI_API_KEY';
    Whisper.Model  := 'whisper-1';
    Whisper.Format := 'mp3';

    // -----------------------------------------------------------------------
    // Escenario 1: TTS con voz "nova" (por defecto)
    // -----------------------------------------------------------------------
    WriteLn('--- Escenario 1: TTS voz "nova" ---');
    Whisper.Voice   := 'nova';
    Whisper.Quality := 'tts-1';
    Whisper.Speed   := 1.0;

    WriteLn('  Texto: "', TEXT_1, '"');
    WriteLn('  Generando voz...');

    AudioOut := Whisper.Speech(TEXT_1);
    try
      SaveStream(AudioOut, TTS_FILE_1);
    finally
      AudioOut.Free;
    end;
    WriteLn;

    // -----------------------------------------------------------------------
    // Escenario 2: STT — transcribir el audio generado
    // -----------------------------------------------------------------------
    WriteLn('--- Escenario 2: STT (transcripcion del MP3 anterior) ---');
    Whisper.ResponseFormat := 'text';
    Whisper.Languaje       := 'en';

    AudioIn := LoadStream(TTS_FILE_1);
    try
      WriteLn('  Transcribiendo ', TTS_FILE_1, '...');
      Transcript := Whisper.Transcription(AudioIn, TTS_FILE_1, '');
      WriteLn('  Transcripcion: "', Trim(Transcript), '"');
    finally
      AudioIn.Free;
    end;
    WriteLn;

    // -----------------------------------------------------------------------
    // Escenario 3: TTS con voz "onyx" (grave) + velocidad reducida
    // -----------------------------------------------------------------------
    WriteLn('--- Escenario 3: TTS voz "onyx" velocidad 0.8 ---');
    Whisper.Voice   := 'onyx';
    Whisper.Quality := 'tts-1-hd';
    Whisper.Speed   := 0.8;

    WriteLn('  Texto: "', TEXT_2, '"');
    WriteLn('  Generando voz...');

    AudioOut := Whisper.Speech(TEXT_2, 'onyx');
    try
      SaveStream(AudioOut, TTS_FILE_2);
    finally
      AudioOut.Free;
    end;
    WriteLn;

    // -----------------------------------------------------------------------
    // Escenario 4: STT del segundo audio, sin especificar idioma
    // (Whisper detecta el idioma automaticamente)
    // -----------------------------------------------------------------------
    WriteLn('--- Escenario 4: STT idioma auto-detectado ---');
    Whisper.Languaje := '';  // Whisper detecta el idioma

    AudioIn := LoadStream(TTS_FILE_2);
    try
      WriteLn('  Transcribiendo ', TTS_FILE_2, '...');
      Transcript := Whisper.Transcription(AudioIn, TTS_FILE_2, '');
      WriteLn('  Transcripcion: "', Trim(Transcript), '"');
    finally
      AudioIn.Free;
    end;
    WriteLn;

    // -----------------------------------------------------------------------
    // Escenario 5: Traduccion a ingles del audio en espanol
    // -----------------------------------------------------------------------
    WriteLn('--- Escenario 5: Traduccion a ingles ---');

    AudioIn := LoadStream(TTS_FILE_2);
    try
      WriteLn('  Traduciendo ', TTS_FILE_2, ' al ingles...');
      Transcript := Whisper.Translation(AudioIn, TTS_FILE_2, '');
      WriteLn('  Traduccion: "', Trim(Transcript), '"');
    finally
      AudioIn.Free;
    end;
    WriteLn;

    // -----------------------------------------------------------------------
    // Resumen de archivos generados
    // -----------------------------------------------------------------------
    WriteLn('Archivos generados:');
    if FileExists(TTS_FILE_1) then
      WriteLn('  ', TTS_FILE_1, ' — ', FileSize(TTS_FILE_1), ' bytes');
    if FileExists(TTS_FILE_2) then
      WriteLn('  ', TTS_FILE_2, ' — ', FileSize(TTS_FILE_2), ' bytes');

  finally
    Whisper.Free;
  end;

  WriteLn;
  WriteLn('Demo Whisper finalizado correctamente.');
end.
