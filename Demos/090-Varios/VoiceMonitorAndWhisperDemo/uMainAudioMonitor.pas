unit uMainAudioMonitor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  System.Threading, Winapi.CommCtrl, System.Math, System.AnsiStrings,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  uMakerAi.Utils.VoiceMonitor, // Nuestra unidad de monitor
{$IFDEF MSWINDOWS}
  Winapi.MMSystem,
{$ENDIF}
  Vcl.StdCtrls, System.IOUtils, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Samples.Spin, uMakerAi.Whisper, uMakerAi.Chat.Tools;

type
  TForm6 = class(TForm)
    ButtonStart: TButton;
    ButtonStop: TButton;
    MemoLog: TMemo;
    ProgressBarLevel: TProgressBar;
    AnimationTimer: TTimer;
    Label1: TLabel;
    SpinEditSilence: TSpinEdit;
    Label3: TLabel;
    FloatSpinEditStart: TSpinEdit;
    Label4: TLabel;
    FloatSpinEditStop: TSpinEdit;
    Label5: TLabel;
    SpinEditWakeWord: TSpinEdit;
    AIVoiceMonitor: TAIVoiceMonitor;
    Label2: TLabel;
    BtnRefreshDevices: TButton;
    OpenDialog1: TOpenDialog;
    MemoTranscription: TMemo;
    ComboBoxDevices: TComboBox;
    LabelCurrentDevice: TLabel;
    ChWakeUp: TCheckBox;
    // NUEVO: Controles para configurar la duración de la palabra de activación
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure SpinEditSilenceChange(Sender: TObject);
    procedure FloatSpinEditStartChange(Sender: TObject);
    procedure FloatSpinEditStopChange(Sender: TObject);
    procedure AnimationTimerTimer(Sender: TObject);
    // NUEVO: Evento para el nuevo control
    procedure SpinEditWakeWordChange(Sender: TObject);
    procedure AIVoiceMonitorChangeState(Sender: TObject; aState, aIsValidForIA: Boolean; aStream: TMemoryStream);
    procedure AIVoiceMonitorCalibrated(Sender: TObject; const aNoiseLevel, aSensitivity, aStopSensitivity: Integer);
    procedure AIVoiceMonitorError(Sender: TObject; const ErrorMessage: string);
    procedure AIVoiceMonitorUpdate(Sender: TObject; const aSoundLevel: Int64);
    procedure AIVoiceMonitorWakeWordCheck(Sender: TObject; aWakeWordStream: TMemoryStream);
    procedure AIVoiceMonitorTranscriptionFragment(Sender: TObject; aFragmentStream: TMemoryStream);
    procedure AIVoiceMonitorSpeechEnd(Sender: TObject; aIsValidForIA: Boolean; aStream: TMemoryStream);
    procedure ComboBoxDevicesChange(Sender: TObject);
    procedure BtnRefreshDevicesClick(Sender: TObject);
    procedure ChWakeUpClick(Sender: TObject);
  private
    // FAudioMonitor: TAIVoiceMonitor;
    FMaxLevelSeen: Int64;
    FCurrentSoundLevel: Int64;
    procedure ProcessVoiceCommand(const Command: String);
    procedure SendToAI(const Command: String);
    procedure LoadAudioDevices;
    procedure UpdateCurrentDeviceLabel;
    Function NewWhisper: TAIWhisper;
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation

{$R *.dfm}
// --- Implementación de los Manejadores de Eventos del TVoiceMonitor ---

procedure TForm6.FormCreate(Sender: TObject);
begin

  // Prepara el directorio para guardar los archivos WAV.
  try
    TDirectory.CreateDirectory('c:\temp');
  except
    on E: Exception do
      ShowMessage('No se pudo crear el directorio c:\temp. Por favor, créelo manualmente. Error: ' + E.Message);
  end;

  // --- Configuración de la Interfaz de Usuario ---
  FMaxLevelSeen := 8000;
  Label1.Caption := 'Pausa para silencio (ms):';
  SpinEditSilence.MinValue := 300;
  SpinEditSilence.MaxValue := 5000;
  SpinEditSilence.Value := DEFAULT_SILENCE_DURATION_MS * 2;
  AIVoiceMonitor.SilenceDuration := SpinEditSilence.Value;

  Label3.Caption := 'Mult. Inicio:';
  FloatSpinEditStart.MinValue := 15; // Representa 1.5
  FloatSpinEditStart.MaxValue := 100; // Representa 10.0
  FloatSpinEditStart.Value := Round(AIVoiceMonitor.SensitivityMultiplier * 10);

  Label4.Caption := 'Mult. Parada:';
  FloatSpinEditStop.MinValue := 10; // Representa 1.0
  FloatSpinEditStop.MaxValue := 50; // Representa 5.0
  FloatSpinEditStop.Value := Round(AIVoiceMonitor.StopSensitivityMultiplier * 10);

  // NUEVO: Configurar control para duración de palabra de activación.
  Label5.Caption := 'Duración Wake Word (ms):';
  SpinEditWakeWord.MinValue := 500;
  SpinEditWakeWord.MaxValue := 5000;
  SpinEditWakeWord.Value := DEFAULT_WAKE_WORD_DURATION_MS;
  AIVoiceMonitor.WakeWordDurationMs := SpinEditWakeWord.Value;

  ChWakeUp.Checked := AIVoiceMonitor.WakeWordActive;


  LoadAudioDevices;
  UpdateCurrentDeviceLabel;


  ProgressBarLevel.Min := 0;
  ProgressBarLevel.Max := FMaxLevelSeen;
  ButtonStop.Enabled := False;
  Self.Caption := 'Monitor Inactivo';
end;

procedure TForm6.FormDestroy(Sender: TObject);
begin
  AnimationTimer.Enabled := False;
end;

procedure TForm6.LoadAudioDevices;
var
  Devices: TArray<TWaveInDeviceInfo>;
  I: Integer;
  DeviceID: UINT;
begin
  // Guardar la selección actual si es posible
  // (Aunque al recargar solemos querer ver el estado real del componente)

  ComboBoxDevices.Items.Clear;

  // Obtener la lista del componente (que ahora YA INCLUYE el Default en la posición 0)
  Devices := TAIVoiceMonitor.GetWaveInDevices;

  for I := 0 to High(Devices) do
  begin
    // Guardamos el ID en el objeto del Item.
    // Hacemos cast a NativeInt para compatibilidad 32/64 bits.
    ComboBoxDevices.Items.AddObject(Devices[I].DeviceName, TObject(NativeInt(Devices[I].DeviceID)));
  end;

  // Seleccionar en el ComboBox el dispositivo que tiene el componente configurado actualmente
  if ComboBoxDevices.Items.Count > 0 then
  begin
    ComboBoxDevices.ItemIndex := 0; // Por defecto al primero (WAVE_MAPPER)

    for I := 0 to ComboBoxDevices.Items.Count - 1 do
    begin
      DeviceID := UINT(NativeInt(ComboBoxDevices.Items.Objects[I]));
      if DeviceID = AIVoiceMonitor.DeviceID then
      begin
        ComboBoxDevices.ItemIndex := I;
        Break;
      end;
    end;
  end;

  MemoLog.Lines.Add(Format('Se encontraron %d dispositivos de audio.', [Length(Devices)]));
end;

function TForm6.NewWhisper: TAIWhisper;
begin
  Result := TAIWhisper.Create(Nil);
  Result.ApiKey := 'xxxyyy';
  Result.url := 'http://192.168.3.121:5150/v1/';
  Result.Languaje := 'es';
  Result.Model := 'openai/whisper-large-v3-turbo';
end;

procedure TForm6.BtnRefreshDevicesClick(Sender: TObject);
begin
  LoadAudioDevices;
  UpdateCurrentDeviceLabel;
  MemoLog.Lines.Add('Lista de dispositivos actualizada.');
end;

procedure TForm6.ButtonStartClick(Sender: TObject);
begin
  MemoLog.Clear;
  MemoLog.Lines.Add('Activando monitor... Por favor, guarde silencio para la calibración.');

  FCurrentSoundLevel := 0;
  FMaxLevelSeen := 8000; // Resetear el máximo para que la barra se ajuste
  ProgressBarLevel.Max := FMaxLevelSeen;
  AnimationTimer.Enabled := True;

  // Deshabilitar controles
  SpinEditSilence.Enabled := False;
  FloatSpinEditStart.Enabled := False;
  FloatSpinEditStop.Enabled := False;
  SpinEditWakeWord.Enabled := False; // Deshabilitar el nuevo control
  ButtonStart.Enabled := False;
  ButtonStop.Enabled := True;
  ComboBoxDevices.Enabled := False;

  AIVoiceMonitor.Active := True;
end;

procedure TForm6.ButtonStopClick(Sender: TObject);
begin
  AIVoiceMonitor.Active := False;
  AnimationTimer.Enabled := False;
  MemoLog.Lines.Add('Monitor desactivado.');

  // Habilitar controles
  ButtonStart.Enabled := True;
  ButtonStop.Enabled := False;
  SpinEditSilence.Enabled := True;
  FloatSpinEditStart.Enabled := True;
  FloatSpinEditStop.Enabled := True;
  SpinEditWakeWord.Enabled := True; // Habilitar el nuevo control
  ComboBoxDevices.Enabled := True;

  ProgressBarLevel.Position := 0;
  Self.Caption := 'Monitor Inactivo';
end;

procedure TForm6.ChWakeUpClick(Sender: TObject);
begin
  AIVoiceMonitor.WakeWordActive := ChWakeUp.Checked;
end;

procedure TForm6.ComboBoxDevicesChange(Sender: TObject);
var
  SelectedDeviceID: UINT;
begin
  if ComboBoxDevices.ItemIndex < 0 then
    Exit;

  // Recuperar el ID del objeto guardado
  SelectedDeviceID := UINT(NativeInt(ComboBoxDevices.Items.Objects[ComboBoxDevices.ItemIndex]));

  try
    // Si el monitor está activo, debemos detenerlo antes de cambiar (el componente lanzaría error si no)
    if AIVoiceMonitor.Active then
    begin
      AIVoiceMonitor.Active := False;
      AIVoiceMonitor.DeviceID := SelectedDeviceID;
      UpdateCurrentDeviceLabel;
      MemoLog.Lines.Add('Monitor detenido para cambiar dispositivo.');
      // Opcional: Reiniciar automáticamente
      // AIVoiceMonitor.Active := True;
    end
    else
    begin
      AIVoiceMonitor.DeviceID := SelectedDeviceID;
      UpdateCurrentDeviceLabel;
    end;

    MemoLog.Lines.Add(Format('Dispositivo seleccionado: %s', [ComboBoxDevices.Items[ComboBoxDevices.ItemIndex]]));

  except
    on E: Exception do
    begin
      MemoLog.Lines.Add('Error al cambiar dispositivo: ' + E.Message);
      ShowMessage('Error: ' + E.Message);
    end;
  end;
end;


// --- Eventos de Controles de UI ---

procedure TForm6.SpinEditSilenceChange(Sender: TObject);
begin
  AIVoiceMonitor.SilenceDuration := SpinEditSilence.Value;
end;

procedure TForm6.FloatSpinEditStartChange(Sender: TObject);
begin
  AIVoiceMonitor.SensitivityMultiplier := (FloatSpinEditStart.Value / 10);
end;

procedure TForm6.FloatSpinEditStopChange(Sender: TObject);
begin
  AIVoiceMonitor.StopSensitivityMultiplier := (FloatSpinEditStop.Value / 10);
end;

procedure TForm6.SpinEditWakeWordChange(Sender: TObject);
begin
  AIVoiceMonitor.WakeWordDurationMs := SpinEditWakeWord.Value;
end;

procedure TForm6.UpdateCurrentDeviceLabel;
begin
  if Assigned(AIVoiceMonitor) then
    LabelCurrentDevice.Caption := 'Dispositivo actual: ' + AIVoiceMonitor.CurrentDeviceName
  else
    LabelCurrentDevice.Caption := 'Dispositivo actual: ---';
end;

procedure TForm6.AIVoiceMonitorCalibrated(Sender: TObject; const aNoiseLevel, aSensitivity, aStopSensitivity: Integer);
begin
  MemoLog.Lines.Add('--- Calibración Finalizada ---');
  MemoLog.Lines.Add(Format('Nivel de ruido base: %d', [aNoiseLevel]));
  MemoLog.Lines.Add(Format('Umbral de INICIO ajustado a: %d', [aSensitivity]));
  MemoLog.Lines.Add(Format('Umbral de PARADA ajustado a: %d', [aStopSensitivity]));
  MemoLog.Lines.Add('¡Listo! Esperando palabra de activación...');
end;

procedure TForm6.AIVoiceMonitorChangeState(Sender: TObject; aState, aIsValidForIA: Boolean; aStream: TMemoryStream);
var
  LocalStream: TMemoryStream;
begin
  if aState then
  begin
    // El monitor ha detectado que el usuario ha empezado a hablar.
    MemoLog.Lines.Add('-> Empezaste a hablar...');
  end
  else
  begin
    // El monitor ha detectado silencio y ha terminado la grabación.
    MemoLog.Lines.Add('<- Dejaste de hablar. Fin del segmento de audio.');

    // Crear una copia local del stream ANTES de que sea liberado por el llamador
    LocalStream := TMemoryStream.Create;
    try
      if Assigned(aStream) then
      begin
        aStream.Position := 0; // Asegurar que estamos al inicio
        LocalStream.CopyFrom(aStream, 0); // Copiar todo el contenido
        LocalStream.Position := 0; // Resetear posición para uso posterior
      end;

      // Aquí está la lógica clave: solo procesamos el audio completo si la palabra de activación fue validada.
      if aIsValidForIA then
      begin
        MemoLog.Lines.Add('   >> ¡PALABRA CLAVE CORRECTA! Procesando el comando completo...');
        // En una aplicación real, aquí es donde enviarías el 'LocalStream' completo a Whisper
        // para transcribir el comando que sigue a la palabra de activación.
        LocalStream.SaveToFile('c:\temp\grabacion_COMPLETA_VALIDA.wav');
        MemoLog.Lines.Add('      Audio completo guardado en: c:\temp\grabacion_COMPLETA_VALIDA.wav');
      end
      else
      begin
        MemoLog.Lines.Add('   -- Palabra clave incorrecta o no detectada. Audio descartado.');
        // Opcional: puedes guardar el audio descartado para fines de depuración.
        LocalStream.SaveToFile('c:\temp\grabacion_descartada.wav');
      end;
    finally
      // Liberamos nuestra copia local
      LocalStream.Free;
    end;
    // NOTA: NO liberamos aStream aquí, es responsabilidad del llamador (TAIVoiceMonitor)
  end;
end;

procedure TForm6.AIVoiceMonitorError(Sender: TObject; const ErrorMessage: string);
begin
  MemoLog.Lines.Add('ERROR: ' + ErrorMessage);
  ShowMessage('Ocurrió un error en el monitor de audio: ' + sLineBreak + ErrorMessage);
  ButtonStopClick(Sender);
end;

procedure TForm6.AIVoiceMonitorSpeechEnd(Sender: TObject; aIsValidForIA: Boolean; aStream: TMemoryStream);
var
  LocalStream: TMemoryStream;
  TranscriptionResult: String;
  LWhisper: TAIWhisper;
begin
  // Crear una copia local del stream para uso seguro
  LocalStream := TMemoryStream.Create;
  try
    // Copiar el contenido del stream recibido
    if Assigned(aStream) then
    begin
      aStream.Position := 0;
      LocalStream.CopyFrom(aStream, 0);
      LocalStream.Position := 0;
    end;

    // Verificar que tenemos datos válidos
    if LocalStream.Size = 0 then
    begin
      MemoLog.Lines.Add('--- Advertencia: Stream de audio vacío ---');
      Exit;
    end;

    try
      // Verificar si la wake word fue validada
      if aIsValidForIA then
      begin
        MemoLog.Lines.Add('');
        MemoLog.Lines.Add('========================================');
        MemoLog.Lines.Add('   PROCESANDO COMANDO COMPLETO');
        MemoLog.Lines.Add('   Wake Word VALIDADA ✓');
        MemoLog.Lines.Add('   Tamaño de audio: ' + FormatFloat('#,##0', LocalStream.Size) + ' bytes');
        MemoLog.Lines.Add('========================================');

        // Opcional: Guardar el audio completo validado para depuración
{$IFDEF DEBUG}
        LocalStream.SaveToFile('c:\temp\comando_completo_' + FormatDateTime('yyyymmdd_hhnnss', Now) + '.wav');
{$ENDIF}
        // Enviar el audio completo a Whisper para transcripción
        LWhisper := NewWhisper;
        Try
          TranscriptionResult := LWhisper.Transcription(LocalStream, 'comando_completo.wav', 'transcribe al español');
        Finally
          LWhisper.Free;
        End;

        // Mostrar el resultado de la transcripción
        if TranscriptionResult <> '' then
        begin
          MemoLog.Lines.Add('');
          MemoLog.Lines.Add('>>> TRANSCRIPCIÓN COMPLETA:');
          MemoLog.Lines.Add(TranscriptionResult);
          MemoLog.Lines.Add('');

          // Aquí puedes procesar el comando completo
          // Por ejemplo: enviar a GPT, ejecutar acción, etc.
          ProcessVoiceCommand(TranscriptionResult);
        end
        else
        begin
          MemoLog.Lines.Add('⚠ No se pudo transcribir el audio');
        end;
      end
      else
      begin
        MemoLog.Lines.Add('');
        MemoLog.Lines.Add('--- Audio descartado (Wake Word NO validada) ---');
        MemoLog.Lines.Add('   Tamaño: ' + FormatFloat('#,##0', LocalStream.Size) + ' bytes');

        // ---la transcripción del total del audio

        LWhisper := NewWhisper;
        Try
          TranscriptionResult := LWhisper.Transcription(LocalStream, 'fragment.wav', 'transcribe al español');
        Finally
          LWhisper.Free;
        End;

        // Mostrar el resultado de la transcripción del fragmento
        if TranscriptionResult <> '' then
        begin
          MemoLog.Lines.Add('--- total audio transcrito ---');
          MemoLog.Lines.Add(TranscriptionResult);
          MemoLog.Lines.Add('');

          MemoTranscription.Lines.Add('--- total audio transcrito ---');
          MemoTranscription.Lines.Add('');
          MemoTranscription.Lines.Text := MemoTranscription.Lines.Text + ' -- ' + TranscriptionResult;
        end;

        // Opcional: Guardar audio descartado para análisis
{$IFDEF DEBUG}
        LocalStream.SaveToFile('c:\temp\descartado_' + FormatDateTime('yyyymmdd_hhnnss', Now) + '.wav');
        MemoLog.Lines.Add('   Guardado en: c:\temp\descartado_*.wav (para análisis)');
{$ENDIF}
      end;

    except
      on E: Exception do
      begin
        MemoLog.Lines.Add('');
        MemoLog.Lines.Add('❌ ERROR al procesar el audio final:');
        MemoLog.Lines.Add('   ' + E.Message);
      end;
    end;

  finally
    // Liberar nuestra copia local
    LocalStream.Free;
  end;

  // Auto-scroll al final del memo
  MemoLog.SelStart := Length(MemoLog.Text);
  MemoLog.Perform(EM_SCROLLCARET, 0, 0);
end;

// --- FUNCIÓN AUXILIAR PARA PROCESAR COMANDOS ---
procedure TForm6.ProcessVoiceCommand(const Command: String);
var
  LowerCommand: String;
begin
  LowerCommand := LowerCase(Trim(Command));

  MemoLog.Lines.Add('');
  MemoLog.Lines.Add('🎯 Procesando comando...');

  // Aquí puedes implementar tu lógica de procesamiento
  // Ejemplos:

  if Pos('abrir', LowerCommand) > 0 then
  begin
    MemoLog.Lines.Add('   → Comando de apertura detectado');
    // Implementar lógica de apertura
  end
  else if Pos('cerrar', LowerCommand) > 0 then
  begin
    MemoLog.Lines.Add('   → Comando de cierre detectado');
    // Implementar lógica de cierre
  end
  else if Pos('buscar', LowerCommand) > 0 then
  begin
    MemoLog.Lines.Add('   → Comando de búsqueda detectado');
    // Implementar lógica de búsqueda
  end
  else
  begin
    MemoLog.Lines.Add('   → Enviando a IA para procesamiento general...');
    // Enviar a GPT u otro LLM para procesamiento
    SendToAI(Command);
  end;
end;

// --- FUNCIÓN AUXILIAR PARA ENVIAR A IA ---
procedure TForm6.SendToAI(const Command: String);
begin
  // Aquí implementarías la llamada a tu LLM preferido
  // Ejemplo con OpenAI, Claude, etc.

  MemoLog.Lines.Add('   → Procesando con IA...');

  // TODO: Implementar llamada a API de IA
  // Ejemplo:
  // var Response: String;
  // Response := OpenAI.Chat(Command);
  // MemoLog.Lines.Add('   Respuesta IA: ' + Response);
end;

procedure TForm6.AIVoiceMonitorTranscriptionFragment(Sender: TObject; aFragmentStream: TMemoryStream);
var
  LocalStream: TMemoryStream;
  TranscriptionResult: String;
  LWhisper: TAIWhisper;
begin
  // Crear una copia local del stream para uso seguro
  LocalStream := TMemoryStream.Create;
  try
    // Copiar el contenido del fragmento recibido
    if Assigned(aFragmentStream) then
    begin
      aFragmentStream.Position := 0;
      LocalStream.CopyFrom(aFragmentStream, 0);
      LocalStream.Position := 0;
    end;

    // Verificar que tenemos datos válidos
    if LocalStream.Size = 0 then
      Exit;

    try
      // Enviar el fragmento a Whisper para transcripción
      // Nota: No necesitamos guardar el archivo, enviamos el stream directamente
      LWhisper := NewWhisper;;
      Try
        TranscriptionResult := LWhisper.Transcription(LocalStream, 'fragment.wav', // Nombre virtual del archivo
          'transcribe al español');

      Finally
        LWhisper.Free;
      End;

      // Mostrar el resultado de la transcripción del fragmento
      if TranscriptionResult <> '' then
      begin
        MemoLog.Lines.Add('--- Fragmento transcrito ---');
        MemoLog.Lines.Add(TranscriptionResult);
        MemoLog.Lines.Add('');

        MemoTranscription.Lines.Add(TranscriptionResult);
      end;
    except
      on E: Exception do
      begin
        MemoLog.Lines.Add('Error al transcribir fragmento: ' + E.Message);
      end;
    end;
  finally
    // Siempre liberar nuestra copia local
    LocalStream.Free;
  end;
end;

procedure TForm6.AIVoiceMonitorUpdate(Sender: TObject; const aSoundLevel: Int64);
begin
  FCurrentSoundLevel := aSoundLevel;
end;



{
procedure TForm6.AIVoiceMonitorWakeWordCheck(Sender: TObject; aWakeWordStream: TMemoryStream; var IsValid: Boolean);

var
  Res: string;
  TargetWakeWord: string;
  LWhisper: TAIWhisper;
  DurationMs: Int64;
  BytesPerSec: Integer;
  Words: TArray<string>;
  Word: string;
  CleanWord, CleanTarget: string;
  Distance: Integer;
  DetectedWord: string; // Para mostrar en el log cuál disparó
begin
  // 1. Validación de seguridad
  if (aWakeWordStream = nil) or (aWakeWordStream.Size < 100) then
  begin
    IsValid := False;
    Exit;
  end;

  // Cálculo de duración
  BytesPerSec := AIVoiceMonitor.SampleRate * AIVoiceMonitor.Channels * (AIVoiceMonitor.BitsPerSample div 8);
  if BytesPerSec > 0 then DurationMs := (aWakeWordStream.Size * 1000) div BytesPerSec else DurationMs := 0;

  TThread.Queue(nil, procedure
  begin
    MemoLog.Lines.Add(Format('--- Verificando Wake Word (%d ms) ---', [DurationMs]));
  end);

  // 2. REINICIAR POSICIÓN
  aWakeWordStream.Position := 0;

  try
    LWhisper := NewWhisper;
    try
      // Transcribir audio
      Res := LWhisper.Transcription(aWakeWordStream, 'wakeword.wav', '');
    finally
      LWhisper.Free;
    end;

    // 3. LIMPIEZA INICIAL
    Res := Trim(Res);

    // Obtener Target y limpiarlo también
    TargetWakeWord := Trim(AIVoiceMonitor.WakeWord);
    if TargetWakeWord = '' then TargetWakeWord := 'andrea';

    // Normalizamos el objetivo (sin acentos, minúsculas)
    CleanTarget := LowerCase( TAIVoiceMonitor.RemoveAccents(TargetWakeWord));

    // 4. LÓGICA DE DETECCIÓN DIFUSA (FUZZY MATCHING)
    IsValid := False;
    DetectedWord := '';

    // Dividimos la frase en palabras (separando por espacios y puntuación)
    Words := Res.Split([' ', ',', '.', '!', '?', ';', ':'], TStringSplitOptions.ExcludeEmpty);

    for Word in Words do
    begin
      // Limpiamos la palabra capturada (sin acentos, minúsculas)
      CleanWord := LowerCase(TAIVoiceMonitor.RemoveAccents(Word));

      // A) Coincidencia exacta
      if CleanWord = CleanTarget then
      begin
        IsValid := True;
        DetectedWord := Word;
        Break;
      end;

      // B) Coincidencia aproximada (Levenshtein)
      // Calculamos la distancia
      Distance := TAIVoiceMonitor.LevenshteinDistance(CleanWord, CleanTarget);

      // Permitimos hasta 2 errores (ej: "Andrea" vs "Andreas" es 1, "Andrea" vs "Andre" es 1)
      // Ajustamos tolerancia: Si la palabra es muy corta (<=4), solo permitimos 1 error.
      if (Length(CleanTarget) > 4) and (Distance <= 2) then
      begin
        IsValid := True;
        DetectedWord := Word;
        Break;
      end
      else if (Length(CleanTarget) <= 4) and (Distance <= 1) then
      begin
        IsValid := True;
        DetectedWord := Word;
        Break;
      end;
    end;

    // 5. LOG DE RESULTADO
    var ValidoLocal := IsValid; // Capturar variable para thread anónimo
    var ResLocal := Res;
    var TargetLocal := TargetWakeWord;
    var DetectedLocal := DetectedWord;

    TThread.Queue(nil, procedure
    begin
      if ValidoLocal then
        MemoLog.Lines.Add(Format('  [OK] ACTIVADO! Detecté "%s" (similar a "%s") en la frase: "%s"', [DetectedLocal, TargetLocal, ResLocal]))
      else
        MemoLog.Lines.Add(Format('  [X] Ignorado. Frase: "%s" (Ninguna palabra similar a "%s")', [ResLocal, TargetLocal]));
    end);

  except
    on E: Exception do
    begin
      IsValid := False;
      TThread.Queue(nil, procedure
      begin
        MemoLog.Lines.Add('Error en WakeWord: ' + E.Message);
      end);
    end;
  end;
end;
}


procedure TForm6.AIVoiceMonitorWakeWordCheck(Sender: TObject; aWakeWordStream: TMemoryStream);
var
  StreamCopy: TMemoryStream;
begin
  // Creamos una copia del stream porque el original se liberará al salir de este evento
  StreamCopy := TMemoryStream.Create;
  StreamCopy.CopyFrom(aWakeWordStream, 0);
  StreamCopy.Position := 0;

  // Lanzamos la validación pesada en un hilo aparte
  TTask.Run(procedure
    var
      Res, TargetWakeWord, CleanTarget, CleanWord, DetectedWord: string;
      LWhisper: TAIWhisper;
      Words: TArray<string>;
      Word: string;
      Distance: Integer;
      IsValidResult: Boolean;
    begin
      try
        try
          LWhisper := NewWhisper; // Tu función creadora
          try
            Res := LWhisper.Transcription(StreamCopy, 'wakeword.wav', '');
          finally
            LWhisper.Free;
          end;

          // --- Lógica de limpieza y comparación ---
          Res := Trim(Res);
          TargetWakeWord := Trim(AIVoiceMonitor.WakeWord);
          if TargetWakeWord = '' then TargetWakeWord := 'andrea';

          //RemoveAccents es solo una estrategía en el idioma español, cambiar para otros idiomas
          CleanTarget := LowerCase(TAIVoiceMonitor.RemoveAccents(TargetWakeWord));

          IsValidResult := False;
          DetectedWord := '';
          Words := Res.Split([' ', ',', '.', '!', '?', ';', ':'], TStringSplitOptions.ExcludeEmpty);

          for Word in Words do
          begin
            CleanWord := LowerCase(TAIVoiceMonitor.RemoveAccents(Word));
            Distance := TAIVoiceMonitor.LevenshteinDistance(CleanWord, CleanTarget);

            if (CleanWord = CleanTarget) or
               ((Length(CleanTarget) > 4) and (Distance <= 2)) or
               ((Length(CleanTarget) <= 4) and (Distance <= 1)) then
            begin
              IsValidResult := True;
              DetectedWord := Word;
              Break;
            end;
          end;

          // --- NOTIFICAR AL COMPONENTE EL RESULTADO ---
          AIVoiceMonitor.ConfirmWakeWord(IsValidResult);

          // Log en el hilo principal
          TThread.Queue(nil, procedure
          begin
            if IsValidResult then
              MemoLog.Lines.Add('WakeWord detectada: ' + DetectedWord)
            else
              MemoLog.Lines.Add('WakeWord no reconocida en: ' + Res);
          end);

        finally
          StreamCopy.Free;
        end;
      except
        on E: Exception do
          TThread.Queue(nil, procedure begin MemoLog.Lines.Add('Error: ' + E.Message); end);
      end;
    end);
end;


procedure TForm6.AnimationTimerTimer(Sender: TObject);
const
  SMOOTHING_FACTOR = 0.2;
  PBST_NORMAL = 1;
  PBST_ERROR = 2;
  PBST_PAUSED = 3;
var
  CurrentPos, TargetPos, NewPos: Int64;
  StateStr: string;
begin
  if AIVoiceMonitor.Active then
    TargetPos := FCurrentSoundLevel
  else
    TargetPos := 0;

  if TargetPos > ProgressBarLevel.Max then
    ProgressBarLevel.Max := Round(TargetPos * 1.2); // Dar un margen

  CurrentPos := ProgressBarLevel.Position;
  NewPos := CurrentPos + Round((TargetPos - CurrentPos) * SMOOTHING_FACTOR);
  ProgressBarLevel.Position := Min(NewPos, ProgressBarLevel.Max);

  case AIVoiceMonitor.State of
    msCalibrating:
      StateStr := 'Calibrando...';
    msMonitoring:
      StateStr := 'Monitorizando';
    msRequestingPermission:
      StateStr := 'Pidiendo permiso...';
    msError:
      StateStr := 'Error';
  else
    StateStr := 'Inactivo';
  end;

  Self.Caption := Format('Estado: %s | Nivel: %d | Umbrales: %d / %d | Hablando: %s', [StateStr, TargetPos, AIVoiceMonitor.Sensitivity, AIVoiceMonitor.StopSensitivity, BoolToStr(AIVoiceMonitor.IsSpeaking, True)]);
end;

end.
