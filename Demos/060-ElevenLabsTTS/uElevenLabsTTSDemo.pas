unit uElevenLabsTTSDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.IOUtils,
  System.JSON,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Edit, FMX.Memo, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Media, FMX.ListBox,
  uMakerAi.Core, uMakerAi.Chat.Messages,
  uMakerAi.ElevenLabs.Speech;

type
  TFrmElevenLabs = class(TForm)
    // --- TTS ---
    LblTexto: TLabel;
    MemoTexto: TMemo;
    LblVoiceId: TLabel;
    EdVoiceId: TEdit;
    LblModelo: TLabel;
    CbModelo: TComboBox;
    LblFormato: TLabel;
    CbFormato: TComboBox;
    BtnGenerar: TButton;
    BtnReproducir: TButton;
    BtnGuardar: TButton;
    LblTTSStatus: TLabel;
    // --- STT ---
    LblSeparador: TLabel;
    LblSTT: TLabel;
    BtnCargar: TButton;
    LblAudio: TLabel;
    BtnTranscribir: TButton;
    MemoSTT: TMemo;
    // --- General ---
    LblStatus: TLabel;
    MediaPlayer: TMediaPlayer;
    OpenDlg: TOpenDialog;
    SaveDlg: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnGenerarClick(Sender: TObject);
    procedure BtnReproducirClick(Sender: TObject);
    procedure BtnGuardarClick(Sender: TObject);
    procedure BtnCargarClick(Sender: TObject);
    procedure BtnTranscribirClick(Sender: TObject);
  private
    FTool: TAiElevenLabsSpeechTool;
    FLastAudioFile: TAiMediaFile;
    FLoadedAudioFile: TAiMediaFile;
    FTempAudioPath: string;
    procedure SetStatus(const Msg: string);
    procedure EnableButtons(AEnabled: Boolean);
  end;

var
  FrmElevenLabs: TFrmElevenLabs;

implementation

{$R *.fmx}

{ TFrmElevenLabs }

procedure TFrmElevenLabs.FormCreate(Sender: TObject);
begin
  FTool := TAiElevenLabsSpeechTool.Create(Self);

  // Modelos disponibles
  CbModelo.Items.Add('eleven_multilingual_v2');
  CbModelo.Items.Add('eleven_flash_v2_5');
  CbModelo.Items.Add('eleven_turbo_v2_5');
  CbModelo.Items.Add('eleven_turbo_v2');
  CbModelo.Items.Add('eleven_multilingual_v1');
  CbModelo.ItemIndex := 0;

  // Formatos de salida
  CbFormato.Items.Add('mp3_44100_128 (default)');
  CbFormato.Items.Add('mp3_44100_192 (alta calidad)');
  CbFormato.Items.Add('pcm_24000 (WAV)');
  CbFormato.Items.Add('pcm_44100 (WAV alta calidad)');
  CbFormato.ItemIndex := 0;

  EdVoiceId.Text := '21m00Tcm4TlvDq8ikWAM'; // Rachel (default)
  MemoTexto.Text := 'Hola! Soy una voz generada por ElevenLabs usando el framework MakerAI para Delphi.';
  FTempAudioPath := TPath.Combine(TPath.GetTempPath, 'elevenlabs_demo_audio.mp3');
end;

procedure TFrmElevenLabs.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FLastAudioFile);
  FreeAndNil(FLoadedAudioFile);
  if TFile.Exists(FTempAudioPath) then
    TFile.Delete(FTempAudioPath);
end;

procedure TFrmElevenLabs.SetStatus(const Msg: string);
begin
  TThread.Queue(nil, procedure
  begin
    LblStatus.Text := Msg;
  end);
end;

procedure TFrmElevenLabs.EnableButtons(AEnabled: Boolean);
begin
  TThread.Queue(nil, procedure
  begin
    BtnGenerar.Enabled    := AEnabled;
    BtnTranscribir.Enabled := AEnabled;
  end);
end;

{ --- TTS --- }

procedure TFrmElevenLabs.BtnGenerarClick(Sender: TObject);
var
  LText, LVoiceId, LModel, LFormatExtension: string;
begin
  LText    := MemoTexto.Text.Trim;
  LVoiceId := EdVoiceId.Text.Trim;
  LModel   := CbModelo.Items[CbModelo.ItemIndex];

  if LText = '' then
  begin
    LblTTSStatus.Text := 'Ingresa texto para sintetizar.';
    Exit;
  end;
  if LVoiceId = '' then
  begin
    LblTTSStatus.Text := 'Ingresa un Voice ID valido.';
    Exit;
  end;

  // GenerateSpeech usa MP3 por defecto; para formatos alternativos
  // configurar FTool.OutputFormat y llamar metodo de instancia.
  case CbFormato.ItemIndex of
    2, 3: LFormatExtension := 'wav';
  else    LFormatExtension := 'mp3';
  end;

  FTempAudioPath := TPath.Combine(TPath.GetTempPath,
    'elevenlabs_demo_audio.' + LFormatExtension);

  EnableButtons(False);
  LblTTSStatus.Text := 'Generando voz...';
  BtnReproducir.Enabled := False;
  BtnGuardar.Enabled    := False;

  TThread.CreateAnonymousThread(procedure
  var
    LFile: TAiMediaFile;
  begin
    try
      LFile := TAiElevenLabsSpeechTool.GenerateSpeech(
        '@ELEVENLABS_API_KEY', LVoiceId, LModel, LText);

      TThread.Queue(nil, procedure
      begin
        FreeAndNil(FLastAudioFile);
        FLastAudioFile := LFile;

        if Assigned(FLastAudioFile) then
        begin
          // Guardar en temp para reproducir
          FLastAudioFile.Content.Position := 0;
          FLastAudioFile.Content.SaveToFile(FTempAudioPath);

          LblTTSStatus.Text := Format('Audio generado: %s (%d bytes)',
            [ExtractFileName(FTempAudioPath), FLastAudioFile.Content.Size]);
          BtnReproducir.Enabled := True;
          BtnGuardar.Enabled    := True;
          SetStatus('TTS completado correctamente.');
        end
        else
        begin
          LblTTSStatus.Text := 'Error: no se genero audio.';
          SetStatus('Error al generar voz.');
        end;
        EnableButtons(True);
      end);
    except
      on E: Exception do
        TThread.Queue(nil, procedure
        begin
          LblTTSStatus.Text := 'Error: ' + E.Message;
          SetStatus('Error TTS: ' + E.Message);
          EnableButtons(True);
        end);
    end;
  end).Start;
end;

procedure TFrmElevenLabs.BtnReproducirClick(Sender: TObject);
begin
  if not TFile.Exists(FTempAudioPath) then
  begin
    LblTTSStatus.Text := 'No hay audio generado para reproducir.';
    Exit;
  end;
  try
    MediaPlayer.Stop;
    MediaPlayer.FileName := FTempAudioPath;
    MediaPlayer.Play;
    SetStatus('Reproduciendo: ' + ExtractFileName(FTempAudioPath));
  except
    on E: Exception do
      SetStatus('Error al reproducir: ' + E.Message);
  end;
end;

procedure TFrmElevenLabs.BtnGuardarClick(Sender: TObject);
begin
  if not Assigned(FLastAudioFile) then
  begin
    SetStatus('No hay audio para guardar.');
    Exit;
  end;
  SaveDlg.FileName    := 'elevenlabs_audio';
  SaveDlg.DefaultExt  := ExtractFileExt(FTempAudioPath).TrimLeft(['.']);
  SaveDlg.Filter      := 'Audio MP3|*.mp3|Audio WAV|*.wav|Todos|*.*';
  if SaveDlg.Execute then
  begin
    FLastAudioFile.Content.Position := 0;
    FLastAudioFile.Content.SaveToFile(SaveDlg.FileName);
    SetStatus('Audio guardado en: ' + SaveDlg.FileName);
  end;
end;

{ --- STT --- }

procedure TFrmElevenLabs.BtnCargarClick(Sender: TObject);
begin
  OpenDlg.Filter := 'Audio|*.mp3;*.wav;*.ogg;*.m4a;*.flac;*.webm|Todos|*.*';
  if OpenDlg.Execute then
  begin
    FreeAndNil(FLoadedAudioFile);
    FLoadedAudioFile := TAiMediaFile.Create;
    FLoadedAudioFile.LoadFromFile(OpenDlg.FileName);
    LblAudio.Text := ExtractFileName(OpenDlg.FileName) +
      Format(' (%d KB)', [FLoadedAudioFile.Content.Size div 1024]);
    BtnTranscribir.Enabled := True;
    SetStatus('Audio cargado: ' + OpenDlg.FileName);
  end;
end;

procedure TFrmElevenLabs.BtnTranscribirClick(Sender: TObject);
begin
  if not Assigned(FLoadedAudioFile) then
  begin
    SetStatus('Carga un archivo de audio primero.');
    Exit;
  end;

  EnableButtons(False);
  MemoSTT.Text := 'Transcribiendo...';
  SetStatus('Enviando audio a ElevenLabs Scribe...');

  TThread.CreateAnonymousThread(procedure
  var
    LText: string;
    LTool: TAiElevenLabsSpeechTool;
  begin
    LTool := TAiElevenLabsSpeechTool.Create(nil);
    try
      try
        LText := LTool.Transcribe(FLoadedAudioFile);
        TThread.Queue(nil, procedure
        begin
          MemoSTT.Text := LText;
          SetStatus('Transcripcion completada.');
          EnableButtons(True);
        end);
      except
        on E: Exception do
          TThread.Queue(nil, procedure
          begin
            MemoSTT.Text := 'Error: ' + E.Message;
            SetStatus('Error en transcripcion: ' + E.Message);
            EnableButtons(True);
          end);
      end;
    finally
      LTool.Free;
    end;
  end).Start;
end;

end.
