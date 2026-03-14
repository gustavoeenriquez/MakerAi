program demo_gemini_speech;
{$mode objfpc}{$H+}

// Demo: TAiGeminiSpeechTool — TTS y transcripcion via API de Gemini
//
// Escenarios:
//   1. TTS simple con GenerateSpeech (class function) — voz Puck
//   2. TTS con voz alternativa — Charon (grave)
//   3. TTS multi-hablante — "Anya=Kore, Liam=Puck"
//   4. Transcripcion del audio generado (ciclo completo TTS->STT)
//   5. AudioProfile + Scene + DirectorsNotes (prompt de direccion)
//
// Nota:
//   TTS devuelve PCM raw que se convierte automaticamente a WAV.
//   Modelo TTS por defecto: gemini-2.5-flash-preview-tts
//   Modelo transcripcion: gemini-2.0-flash
//
// Requisitos: GEMINI_API_KEY
//
// Compilar:
//   fpc demo_gemini_speech.pas
//     -Fu../Source/Core -Fu../Source/Chat -Fu../Source/Tools -Fu../Source/Utils

uses
  SysUtils, Classes,
  uMakerAi.Core,
  uMakerAi.Chat.Messages,
  uMakerAi.Gemini.Speech;

// ---------------------------------------------------------------------------
// Subclase para exponer InternalRunGeminiTranscription (protected) en el demo
// ---------------------------------------------------------------------------
type
  TGeminiSpeechAccess = class(TAiGeminiSpeechTool)
  public
    function Transcribe(aMediaFile: TAiMediaFile): string;
  end;

function TGeminiSpeechAccess.Transcribe(aMediaFile: TAiMediaFile): string;
begin
  Result := InternalRunGeminiTranscription(aMediaFile);
end;

// ---------------------------------------------------------------------------
procedure SaveMediaFile(MF: TAiMediaFile; const AFile: string);
var
  F: TFileStream;
begin
  if not Assigned(MF) or (MF.Content.Size = 0) then
  begin
    WriteLn('  Sin audio en el resultado.');
    Exit;
  end;
  F := TFileStream.Create(AFile, fmCreate);
  try
    MF.Content.Position := 0;
    F.CopyFrom(MF.Content, MF.Content.Size);
  finally
    F.Free;
  end;
  WriteLn('  Guardado: ', AFile, '  (', MF.Content.Size div 1024, ' KB)');
end;

// ---------------------------------------------------------------------------
var
  MF:      TAiMediaFile;
  Speech:  TGeminiSpeechAccess;
  Transcript: string;
  AudioIn: TAiMediaFile;

const
  API_KEY = '@GEMINI_API_KEY';

  TEXT_1 = 'Welcome to the future of artificial intelligence with Google Gemini.';
  TEXT_2 = 'La inteligencia artificial esta cambiando el mundo para siempre.';
  TEXT_DIALOG = 'Anya: Hello Liam, how are you today? Liam: I am doing great, thank you for asking!';

begin
  WriteLn('=== MakerAI FPC — Demo Gemini Speech ===');
  WriteLn;

  // -------------------------------------------------------------------------
  // Escenario 1: TTS simple con voz Puck (class function, sin instancia)
  // -------------------------------------------------------------------------
  WriteLn('--- Esc 1: TTS simple  voz Puck ---');
  WriteLn('  Texto: "', TEXT_1, '"');
  WriteLn('  Generando... (convierte PCM -> WAV automaticamente)');

  MF := TAiGeminiSpeechTool.GenerateSpeech(
    API_KEY, TEXT_1, 'Puck', nil, nil, nil);
  try
    SaveMediaFile(MF, 'gemini_puck.wav');
  finally
    MF.Free;
  end;
  WriteLn;

  // -------------------------------------------------------------------------
  // Escenario 2: Voz Charon (grave, masculina)
  // -------------------------------------------------------------------------
  WriteLn('--- Esc 2: TTS  voz Charon ---');
  WriteLn('  Texto: "', TEXT_2, '"');

  MF := TAiGeminiSpeechTool.GenerateSpeech(
    API_KEY, TEXT_2, 'Charon', nil, nil, nil);
  try
    SaveMediaFile(MF, 'gemini_charon.wav');
  finally
    MF.Free;
  end;
  WriteLn;

  // -------------------------------------------------------------------------
  // Escenario 3: Multi-hablante  Anya=Kore, Liam=Puck
  // -------------------------------------------------------------------------
  WriteLn('--- Esc 3: TTS multi-hablante  "Anya=Kore, Liam=Puck" ---');
  WriteLn('  Texto: "', TEXT_DIALOG, '"');

  MF := TAiGeminiSpeechTool.GenerateSpeech(
    API_KEY, TEXT_DIALOG, 'Anya=Kore, Liam=Puck', nil, nil, nil);
  try
    SaveMediaFile(MF, 'gemini_multi.wav');
  finally
    MF.Free;
  end;
  WriteLn;

  // -------------------------------------------------------------------------
  // Escenario 4: Transcripcion del WAV generado (ciclo completo)
  // -------------------------------------------------------------------------
  WriteLn('--- Esc 4: Transcripcion del audio generado (TTS -> STT) ---');
  if FileExists('gemini_puck.wav') then
  begin
    Speech := TGeminiSpeechAccess.Create(nil);
    try
      Speech.ApiKey := API_KEY;
      Speech.TranscriptionModel  := 'gemini-2.0-flash';
      Speech.TranscriptionPrompt := 'Transcribe this audio accurately.';

      AudioIn := TAiMediaFile.Create;
      try
        AudioIn.LoadFromFile('gemini_puck.wav');
        WriteLn('  Transcribiendo gemini_puck.wav...');
        Transcript := Speech.Transcribe(AudioIn);
        WriteLn('  Transcripcion: "', Trim(Transcript), '"');
      finally
        AudioIn.Free;
      end;
    finally
      Speech.Free;
    end;
  end
  else
    WriteLn('  (omitido: gemini_puck.wav no existe)');
  WriteLn;

  // -------------------------------------------------------------------------
  // Escenario 5: TTS con AudioProfile + DirectorsNotes (prompt de direccion)
  // -------------------------------------------------------------------------
  WriteLn('--- Esc 5: TTS con AudioProfile y DirectorsNotes ---');
  var
    Profile: TStringList;
    Notes:   TStringList;
  Profile := TStringList.Create;
  Notes   := TStringList.Create;
  try
    Profile.Add('Voice: calm, professional narrator');
    Profile.Add('Pace: measured and clear');
    Notes.Add('Pause slightly between sentences.');
    Notes.Add('Emphasize key technical terms.');

    WriteLn('  AudioProfile: calm professional narrator');
    WriteLn('  Texto: "', TEXT_1, '"');

    MF := TAiGeminiSpeechTool.GenerateSpeech(
      API_KEY, TEXT_1, 'Fenrir', Profile, nil, Notes);
    try
      SaveMediaFile(MF, 'gemini_directed.wav');
    finally
      MF.Free;
    end;
  finally
    Profile.Free;
    Notes.Free;
  end;
  WriteLn;

  WriteLn('Archivos generados: gemini_puck.wav, gemini_charon.wav,');
  WriteLn('                    gemini_multi.wav, gemini_directed.wav');
  WriteLn('Demo Gemini Speech finalizado.');
end.
