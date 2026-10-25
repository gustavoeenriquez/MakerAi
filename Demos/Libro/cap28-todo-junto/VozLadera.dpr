program VozLadera;

// =============================================================================
// LIBRO "IA con Delphi" - capitulo 27: la voz por lotes, en consola
// =============================================================================
// Las dos mitades de la voz sin tiempo real, con TAiOpenAiAudio:
//   --transcribir archivo.wav         STT: gpt-transcribe (el audio entra, sale texto)
//   --decir "texto" salida.wav        TTS: gpt-4o-mini-tts (el texto entra, sale audio WAV)
//   --decir "texto" salida.wav --voz coral --instrucciones "tono..."
// Es lo mismo que hace el puente de audio del capitulo 9 (ChatTools.SpeechTool) y lo que
// genero la voz de la clienta de las llamadas del capitulo (media\llamada-*.wav).
// Requiere OPENAI_API_KEY. Empresa, productos y precios ficticios.
// =============================================================================

{$APPTYPE CONSOLE}
{$R *.res}

uses
  {$IFDEF MSWINDOWS} Winapi.Windows, {$ENDIF}
  System.SysUtils,
  System.Classes,
  System.Diagnostics,
  System.TypInfo,
  uMakerAi.Core,
  uMakerAi.OpenAI.Audio;

function ArgValue(const AName, ADefault: string): string;
var
  I: Integer;
begin
  Result := ADefault;
  for I := 1 to ParamCount - 1 do
    if SameText(ParamStr(I), AName) then
      Exit(ParamStr(I + 1));
end;

procedure Transcribir(Audio: TAiOpenAiAudio; const AArchivo: string);
var
  MF: TAiMediaFile;
  R: TTranscriptionResult;
  Reloj: TStopwatch;
begin
  // El archivo entra como TAiMediaFile (el mismo sobre que viaja en un mensaje del chat)
  MF := TAiMediaFile.Create;
  try
    MF.LoadFromFile(AArchivo);
    Audio.TranscriptionModel := tmGptTranscribe;   // el default desde v3.9; whisper-1 se apaga en 2027
    Audio.TranscriptionLanguage := 'es';
    Audio.TranscriptionKeywords.Text := 'Huila Reserva'#13#10'Nariño Altura'#13#10'Sierra Nevada Orgánico'#13#10'Descafeinado Suave'#13#10'Pitalito';
    Writeln(Format('Transcribiendo %s (%s, %d KB) con gpt-transcribe...', [ExtractFileName(AArchivo), MF.MimeType, MF.Content.Size div 1024]));
    Reloj := TStopwatch.StartNew;
    R := Audio.Transcribe(MF);
    try
      // gpt-transcribe devuelve solo el texto (sin idioma, duracion ni marcas de tiempo: eso era de whisper-1)
      Writeln(Format('%.1f s', [Reloj.ElapsedMilliseconds / 1000]));
      Writeln('Texto: ' + R.Text);
      if R.Warning <> '' then
        Writeln('Aviso: ' + R.Warning);
    finally
      R.Free;
    end;
  finally
    MF.Free;
  end;
end;

procedure Decir(Audio: TAiOpenAiAudio; const ATexto, ASalida, AVoz, AInstrucciones: string);
var
  Stream: TMemoryStream;
  Reloj: TStopwatch;
  V: Integer;
begin
  Audio.TTSModel := gpt_4o_mini_tts;
  Audio.TTSResponseFormat := trfWav;              // tambien mp3, opus, aac, flac o pcm crudo
  V := GetEnumValue(TypeInfo(TAiTTSVoice), 'tv' + UpperCase(AVoz[1]) + LowerCase(AVoz.Substring(1)));
  if V >= 0 then
    Audio.TTSVoice := TAiTTSVoice(V);
  Audio.TTSInstructions := AInstrucciones;        // como hablar: tono, ritmo, acento (solo gpt-4o-mini-tts)
  Writeln(Format('Diciendo con gpt-4o-mini-tts (voz %s): "%s"', [AVoz, ATexto]));
  Reloj := TStopwatch.StartNew;
  Stream := Audio.Speech(ATexto);
  try
    Stream.SaveToFile(ASalida);
    Writeln(Format('%.1f s · %d KB · %s', [Reloj.ElapsedMilliseconds / 1000, Stream.Size div 1024, ASalida]));
  finally
    Stream.Free;
  end;
end;

var
  Audio: TAiOpenAiAudio;
  BufSalida: array [0 .. 0] of Byte;

begin
  {$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);
  SetTextCodePage(Output, CP_UTF8);
  {$ENDIF}
  SetTextBuf(Output, BufSalida, 1);
  if GetEnvironmentVariable('OPENAI_API_KEY') = '' then
  begin
    Writeln('Falta la variable OPENAI_API_KEY.');
    ExitCode := 1;
    Exit;
  end;
  Audio := TAiOpenAiAudio.Create(nil);
  try
    try
      Audio.ApiKey := '@OPENAI_API_KEY';
      if ArgValue('--transcribir', '') <> '' then
        Transcribir(Audio, ArgValue('--transcribir', ''))
      else if ArgValue('--decir', '') <> '' then
        Decir(Audio, ArgValue('--decir', ''), ParamStr(3), ArgValue('--voz', 'coral'),
          ArgValue('--instrucciones', 'Barista colombiana, cálida, hablando por teléfono.'))
      else
        Writeln('Uso: VozLadera --transcribir archivo.wav | --decir "texto" salida.wav [--voz coral] [--instrucciones "..."]');
    except
      on E: Exception do
        Writeln(E.ClassName + ': ' + E.Message);
    end;
  finally
    Audio.Free;
  end;
end.
