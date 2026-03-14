program demo_openai_audio;
{$mode objfpc}{$H+}

// Demo: TAiOpenAiAudio — TTS y STT con API moderna de OpenAI
//
// Diferencias con TAIWhisper (demo_whisper.pas):
//   - Enums tipados para modelos, voces y formatos (no strings libres)
//   - SpeechStreamed dispara OnAudioChunkReceived al completar
//   - Transcribe devuelve TTranscriptionResult con texto + duracion + idioma
//   - TranslateToEnglish con TTranscriptionResult
//
// Escenarios:
//   1. TTS con diferentes modelos: tts-1, tts-1-hd, gpt-4o-mini-tts
//   2. TTS Streaming (OnAudioChunkReceived)
//   3. STT con Transcribe — TTranscriptionResult
//   4. STT con idioma auto-detectado
//   5. TranslateToEnglish
//
// Requisitos: OPENAI_API_KEY
//
// Compilar:
//   fpc demo_openai_audio.pas -Fu../Source/Core -Fu../Source/Chat -Fu../Source/Tools

uses
  SysUtils, Classes,
  uMakerAi.Core,
  uMakerAi.OpenAI.Audio;

// ---------------------------------------------------------------------------
// Guarda TStream en disco
// ---------------------------------------------------------------------------
procedure SaveStream(AStream: TStream; const AFile: string);
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

function LoadMediaFile(const AFile: string): TAiMediaFile;
begin
  Result := TAiMediaFile.Create;
  Result.LoadFromFile(AFile);
end;

// ---------------------------------------------------------------------------
// Handlers de streaming
// ---------------------------------------------------------------------------
var
  GChunksReceived: Integer = 0;
  GTotalBytes:     Int64   = 0;

procedure OnChunk(Sender: TObject; const AChunk: TBytes);
begin
  Inc(GChunksReceived);
  Inc(GTotalBytes, Length(AChunk));
  Write('.');
end;

procedure OnSpeechDone(Sender: TObject);
begin
  WriteLn;
  WriteLn('  Streaming completado — chunks=', GChunksReceived,
          '  bytes totales=', GTotalBytes);
end;

procedure OnTranscriptionDone(Sender: TObject;
  const AResult: TTranscriptionResult);
begin
  WriteLn('  [Evento] OnTranscriptionCompleted: "',
    Copy(AResult.Text, 1, 80), '"');
end;

// ---------------------------------------------------------------------------
var
  Audio: TAiOpenAiAudio;
  Snd:   TMemoryStream;
  Res:   TTranscriptionResult;
  MF:    TAiMediaFile;

const
  EN_TEXT = 'The future of artificial intelligence is both exciting and challenging.';
  ES_TEXT = 'La inteligencia artificial esta transformando el mundo moderno.';
  F_TTS1  = 'audio_tts1_alloy.mp3';
  F_TTS_HD = 'audio_tts1hd_coral.mp3';
  F_GPT4O = 'audio_gpt4o_nova.mp3';
  F_STREAM = 'audio_streamed.mp3';

begin
  WriteLn('=== MakerAI FPC — Demo OpenAI Audio (TAiOpenAiAudio) ===');
  WriteLn;

  Audio := TAiOpenAiAudio.Create(nil);
  try
    Audio.ApiKey := '@OPENAI_API_KEY';

    // -----------------------------------------------------------------------
    // Escenario 1: TTS con tts-1 voz alloy
    // -----------------------------------------------------------------------
    WriteLn('--- Esc 1: TTS  tts-1  voz Alloy ---');
    Audio.TTSModel          := tts_1;
    Audio.TTSVoice          := tvAlloy;
    Audio.TTSResponseFormat := trfMp3;
    Audio.TTSSpeed          := 1.0;

    WriteLn('  Texto: "', EN_TEXT, '"');
    Snd := Audio.Speech(EN_TEXT);
    try
      SaveStream(Snd, F_TTS1);
    finally
      Snd.Free;
    end;
    WriteLn;

    // -----------------------------------------------------------------------
    // Escenario 2: TTS HD con voz Coral
    // -----------------------------------------------------------------------
    WriteLn('--- Esc 2: TTS  tts-1-hd  voz Coral  velocidad 0.9 ---');
    Audio.TTSModel  := tts_1_hd;
    Audio.TTSVoice  := tvCoral;
    Audio.TTSSpeed  := 0.9;

    WriteLn('  Texto: "', EN_TEXT, '"');
    Snd := Audio.Speech(EN_TEXT);
    try
      SaveStream(Snd, F_TTS_HD);
    finally
      Snd.Free;
    end;
    WriteLn;

    // -----------------------------------------------------------------------
    // Escenario 3: TTS con gpt-4o-mini-tts + instrucciones de estilo
    // -----------------------------------------------------------------------
    WriteLn('--- Esc 3: TTS  gpt-4o-mini-tts  voz Nova + instrucciones ---');
    Audio.TTSModel        := gpt_4o_mini_tts;
    Audio.TTSVoice        := tvNova;
    Audio.TTSInstructions := 'Speak in an enthusiastic, energetic tone';

    WriteLn('  Texto: "', EN_TEXT, '"');
    Snd := Audio.Speech(EN_TEXT);
    try
      SaveStream(Snd, F_GPT4O);
    finally
      Snd.Free;
    end;
    WriteLn;

    // -----------------------------------------------------------------------
    // Escenario 4: SpeechStreamed — audio entregado via evento
    // -----------------------------------------------------------------------
    WriteLn('--- Esc 4: SpeechStreamed  (OnAudioChunkReceived) ---');
    Audio.TTSModel            := tts_1;
    Audio.TTSVoice            := tvShimmer;
    Audio.TTSInstructions     := '';
    Audio.OnAudioChunkReceived := @OnChunk;
    Audio.OnSpeechCompleted   := @OnSpeechDone;

    GChunksReceived := 0;
    GTotalBytes     := 0;

    WriteLn('  Texto: "', EN_TEXT, '"');
    Write('  Recibiendo chunks: ');
    // SpeechStreamed internamente hace POST completo y dispara el evento
    Audio.SpeechStreamed(EN_TEXT);
    // Guardar el audio recibido: reconstruir desde Speech normal
    Snd := Audio.Speech(EN_TEXT);
    try
      SaveStream(Snd, F_STREAM);
    finally
      Snd.Free;
    end;
    WriteLn;

    // -----------------------------------------------------------------------
    // Escenario 5: STT — Transcribe con TTranscriptionResult
    // -----------------------------------------------------------------------
    WriteLn('--- Esc 5: STT  Transcribe  (whisper-1) ---');
    Audio.TranscriptionModel          := tmWhisper1;
    Audio.TranscriptionResponseFormat := trfText;
    Audio.TranscriptionLanguage       := 'en';

    MF := LoadMediaFile(F_TTS1);
    try
      WriteLn('  Archivo: ', F_TTS1);
      Res := Audio.Transcribe(MF);
      try
        WriteLn('  Texto transcripto: "', Trim(Res.Text), '"');
      finally
        Res.Free;
      end;
    finally
      MF.Free;
    end;
    WriteLn;

    // -----------------------------------------------------------------------
    // Escenario 6: STT con verbose_json — incluye duracion e idioma
    // -----------------------------------------------------------------------
    WriteLn('--- Esc 6: STT  verbose_json (duracion + idioma) ---');
    Audio.TranscriptionResponseFormat := trfVerboseJson;
    Audio.TranscriptionLanguage       := '';  // auto-detect

    MF := LoadMediaFile(F_TTS1);
    try
      Res := Audio.Transcribe(MF);
      try
        WriteLn('  Texto:    "', Trim(Res.Text), '"');
        WriteLn('  Idioma:   ', Res.Language);
        WriteLn('  Duracion: ', FormatFloat('0.00', Res.Duration), ' s');
      finally
        Res.Free;
      end;
    finally
      MF.Free;
    end;
    WriteLn;

    // -----------------------------------------------------------------------
    // Escenario 7: TranslateToEnglish (audio en espanol -> texto en ingles)
    // -----------------------------------------------------------------------
    WriteLn('--- Esc 7: TranslateToEnglish ---');
    Audio.TTSModel   := tts_1;
    Audio.TTSVoice   := tvAlloy;
    Audio.TTSSpeed   := 1.0;

    // Generar audio en espanol
    Snd := Audio.Speech(ES_TEXT);
    try
      Snd.SaveToFile('audio_es.mp3');
    finally
      Snd.Free;
    end;

    MF := LoadMediaFile('audio_es.mp3');
    try
      WriteLn('  Audio: "', ES_TEXT, '"');
      Res := Audio.TranslateToEnglish(MF);
      try
        WriteLn('  Traduccion: "', Trim(Res.Text), '"');
      finally
        Res.Free;
      end;
    finally
      MF.Free;
    end;

  finally
    Audio.Free;
  end;

  WriteLn;
  WriteLn('Archivos generados: ', F_TTS1, ', ', F_TTS_HD, ', ',
          F_GPT4O, ', ', F_STREAM, ', audio_es.mp3');
  WriteLn('Demo OpenAI Audio finalizado.');
end.
