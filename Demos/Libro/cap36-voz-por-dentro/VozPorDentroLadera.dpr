program VozPorDentroLadera;

{
  Capitulo 36 - La voz por dentro.

  Lo que el capitulo 27 dio por hecho: de donde entra el audio y por donde sale.
    --dispositivos  lista microfonos, dispositivos de loopback y parlantes (GetAudioDevices /
                    GetPlaybackDevices) con su EndpointId, el valor que va en DeviceId.
    --loopback      captura lo que SUENA en un dispositivo de salida (Source = asLoopback) y lo
                    transcribe en vivo con TAiOpenAiRealtimeSTT (gpt-live-transcribe); a mitad de
                    camino Muted = True: la captura sigue, pero manda silencio.
    --llamada       un cliente llama en ingles: su voz entra por un cable virtual (VB-CABLE) y
                    TAiOpenAiRealtimeTranslate la vuelve texto Y voz en espanol; Lucia contesta en
                    espanol y el mismo driver, por TAiRealtimeConnection, la vuelve ingles.
  Sin argumentos corre los tres. Nada suena en los parlantes: todo el audio va por el cable
  virtual (CABLE Input -> CABLE Output), que es como se conecta una app a una reunion o a una
  central telefonica. Si no hay VB-CABLE se usa el dispositivo predeterminado (y si suena).
  Requiere OPENAI_API_KEY. Las voces de media\ son TTS (gpt-4o-mini-tts).
  Cafe La Ladera: empresa, productos y precios ficticios.
}

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.StrUtils,
  System.Diagnostics,
  Winapi.Windows,
  uMakerAi.Utils.AudioCapture,
  uMakerAi.Utils.AudioPlayback,
  uMakerAi.Realtime,
  uMakerAi.Realtime.OpenAI,
  uMakerAi.Realtime.AiConnection;

type
  TOido = class
  public
    Rotulo: string;
    Origen, Traduccion: string;   // lineas en curso
    Voz: TBytes;                  // audio traducido (PCM16 24 kHz)
    AudioListo: Boolean;
    Reloj: TStopwatch;
    PrimerDelta: Int64;           // ms desde que empezo a sonar hasta el primer texto
    procedure Listo(Sender: TObject);
    procedure HablaEmpieza(Sender: TObject; AudioMs: Int64; const ItemId: string);
    procedure HablaTermina(Sender: TObject; AudioMs: Int64; const ItemId: string);
    procedure Delta(Sender: TObject; const ADelta: string);
    procedure Completo(Sender: TObject; const Transcript, ItemId: string);
    procedure TraduccionDelta(Sender: TObject; const ADelta: string);
    procedure TraduccionTexto(Sender: TObject; const AText: string);
    procedure Audio(Sender: TObject; const AData: TBytes);
    procedure AudioFin(Sender: TObject);
    procedure Error(Sender: TObject; const ErrorMsg, ErrorCode: string);
    procedure ErrorAudio(Sender: TObject; const ErrorMessage: string);
  end;

procedure Log(const S: string);
begin
  Writeln(FormatDateTime('hh:nn:ss.zzz  ', Now), S);
end;

function Tiene(const AArg: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to ParamCount do
    if SameText(ParamStr(I), AArg) then
      Exit(True);
end;

procedure TOido.Listo(Sender: TObject);
begin
  Log(Rotulo + ' sesión lista');
end;

procedure TOido.HablaEmpieza(Sender: TObject; AudioMs: Int64; const ItemId: string);
begin
  Log(Format('%s  voz detectada (a los %d ms de audio)', [Rotulo, AudioMs]));
end;

procedure TOido.HablaTermina(Sender: TObject; AudioMs: Int64; const ItemId: string);
begin
  Log(Format('%s  silencio (a los %d ms de audio)', [Rotulo, AudioMs]));
end;

function FraseCompleta(const S: string): Boolean;
begin
  // Los deltas llegan palabra a palabra: se imprime cada frase cuando cierra
  Result := (S <> '') and CharInSet(S[High(S)], ['.', '?', '!']);
end;

procedure TOido.Delta(Sender: TObject; const ADelta: string);
begin
  if (PrimerDelta = 0) and Reloj.IsRunning then
    PrimerDelta := Reloj.ElapsedMilliseconds;
  Origen := Origen + ADelta;
  if FraseCompleta(Origen) then
  begin
    Log(Rotulo + '  «' + Trim(Origen) + '»');
    Origen := '';
  end;
end;

procedure TOido.Completo(Sender: TObject; const Transcript, ItemId: string);
begin
  // El STT en vivo entrega la frase cerrada aqui (los deltas son parciales)
  if Trim(Transcript) <> '' then
    Log(Rotulo + '  «' + Trim(Transcript) + '»');
  Origen := '';
end;

procedure TOido.TraduccionDelta(Sender: TObject; const ADelta: string);
begin
  if (PrimerDelta = 0) and Reloj.IsRunning then
    PrimerDelta := Reloj.ElapsedMilliseconds;
  Traduccion := Traduccion + ADelta;
  if FraseCompleta(Traduccion) then
  begin
    Log(Rotulo + '  → «' + Trim(Traduccion) + '»');
    Traduccion := '';
  end;
end;

procedure TOido.TraduccionTexto(Sender: TObject; const AText: string);
begin
  if Trim(Traduccion) <> '' then
    Log(Rotulo + '  → «' + Trim(Traduccion) + '»');
  Traduccion := '';
end;

procedure TOido.Audio(Sender: TObject; const AData: TBytes);
begin
  Voz := Voz + AData;    // PCM16 24 kHz mono: la voz traducida, tal como la manda el servidor
end;

procedure TOido.AudioFin(Sender: TObject);
begin
  AudioListo := True;
end;

procedure TOido.Error(Sender: TObject; const ErrorMsg, ErrorCode: string);
begin
  Log(Format('%s  ERROR [%s] %s', [Rotulo, ErrorCode, ErrorMsg]));
end;

procedure TOido.ErrorAudio(Sender: TObject; const ErrorMessage: string);
begin
  Log(Rotulo + '  ERROR de audio: ' + ErrorMessage);
end;

{ ---------------------------------------------------------------------------- utiles }

function Carpeta(const ASub: string): string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), ASub);
  ForceDirectories(Result);
end;

function LeerWav(const ARuta: string): TBytes;
begin
  // WAV PCM16 24 kHz mono con cabecera de 44 bytes (las voces TTS del libro)
  Result := TFile.ReadAllBytes(ARuta);
  Result := Copy(Result, 44, MaxInt);
end;

procedure GuardarWav(const ARuta: string; const APcm: TBytes; AHz: Integer);
var
  S: TFileStream;
  procedure W32(V: Cardinal); begin S.WriteBuffer(V, 4); end;
  procedure W16(V: Word); begin S.WriteBuffer(V, 2); end;
  procedure Tag(const T: AnsiString); begin S.WriteBuffer(T[1], 4); end;
begin
  S := TFileStream.Create(ARuta, fmCreate);
  try
    Tag('RIFF'); W32(36 + Length(APcm)); Tag('WAVE');
    Tag('fmt '); W32(16); W16(1); W16(1); W32(AHz); W32(AHz * 2); W16(2); W16(16);
    Tag('data'); W32(Length(APcm));
    if Length(APcm) > 0 then
      S.WriteBuffer(APcm[0], Length(APcm));
  finally
    S.Free;
  end;
end;

procedure Esperar(AMs: Integer);
var
  Fin: Int64;
begin
  // Los eventos de audio y realtime llegan por TThread.Queue: en consola hay que bombearlos
  Fin := TThread.GetTickCount64 + AMs;
  while TThread.GetTickCount64 < Fin do
    CheckSynchronize(20);
end;

procedure EsperarFinReproduccion(APlayer: TAiAudioPlayer; AMaxMs: Integer);
var
  Fin: Int64;
begin
  Fin := TThread.GetTickCount64 + AMaxMs;
  Esperar(300);
  while APlayer.IsPlaying and (TThread.GetTickCount64 < Fin) do
    CheckSynchronize(20);
end;

function BuscarSalida(const APista: string; out AId, ANombre: string): Boolean;
var
  D: TAiAudioDeviceInfo;
begin
  Result := False;
  AId := '';
  ANombre := '(predeterminado)';
  for D in TAiAudioPlayer.GetPlaybackDevices do
    if Pos(LowerCase(APista), LowerCase(D.DeviceName)) > 0 then
    begin
      AId := D.EndpointId;
      ANombre := D.DeviceName;
      Exit(True);
    end;
end;

function BuscarEntrada(const APista: string; out AId, ANombre: string): Boolean;
var
  D: TAiAudioDeviceInfo;
begin
  Result := False;
  AId := '';
  ANombre := '(predeterminado)';
  for D in TAiAudioCapture.GetAudioDevices(asMicrophone) do
    if Pos(LowerCase(APista), LowerCase(D.DeviceName)) > 0 then
    begin
      AId := D.EndpointId;
      ANombre := D.DeviceName;
      Exit(True);
    end;
end;

{ ---------------------------------------------------------------------------- A: dispositivos }

procedure Dispositivos;
var
  D: TAiAudioDeviceInfo;
begin
  Writeln;
  Writeln('== A. Dispositivos (el EndpointId es lo que va en DeviceId; vacío = predeterminado) ==');
  Writeln('  Micrófonos (Source = asMicrophone):');
  for D in TAiAudioCapture.GetAudioDevices(asMicrophone) do
    Writeln(Format('    %s %-44s %s', [IfThen(D.IsDefault, '*', ' '), D.DeviceName, Copy(D.EndpointId, 1, 40) + '…']));
  Writeln('  Loopback (Source = asLoopback: lo que SUENA en una salida):');
  for D in TAiAudioCapture.GetAudioDevices(asLoopback) do
    Writeln(Format('    %s %-44s %s', [IfThen(D.IsDefault, '*', ' '), D.DeviceName, Copy(D.EndpointId, 1, 40) + '…']));
  Writeln('  Parlantes (TAiAudioPlayer.DeviceId):');
  for D in TAiAudioPlayer.GetPlaybackDevices do
    Writeln(Format('    %s %-44s %s', [IfThen(D.IsDefault, '*', ' '), D.DeviceName, Copy(D.EndpointId, 1, 40) + '…']));
end;

{ ---------------------------------------------------------------------------- B: loopback + STT continuo + Muted }

procedure Loopback;
var
  Player: TAiAudioPlayer;
  Captura: TAiAudioCapture;
  STT: TAiOpenAiRealtimeSTT;
  Oido: TOido;
  IdSalida, NombreSalida: string;
  Voz: TBytes;
begin
  Writeln;
  Writeln('== B. Loopback: transcribir lo que suena, y callar a voluntad (Muted) ==');
  BuscarSalida('CABLE Input', IdSalida, NombreSalida);
  Writeln('  salida que se reproduce y se escucha: ' + NombreSalida);
  Oido := TOido.Create;
  Player := TAiAudioPlayer.Create(nil);
  Captura := TAiAudioCapture.Create(nil);
  STT := TAiOpenAiRealtimeSTT.Create(nil);
  try
    Oido.Rotulo := '[oído]';
    // El transcriptor en vivo: VAD del servidor cierra cada frase y OnTranscriptCompleted la entrega
    STT.ApiKey := '@OPENAI_API_KEY';
    STT.TranscriptionModel := otmGptLiveTranscribe;
    STT.Language := 'es';
    STT.InputSampleRate := 24000;
    // Los nombres propios de la tienda: sin esto, la primera corrida oyo "Whey la Reserva"
    STT.TranscriptionKeywords.Add('Huila Reserva');
    STT.TranscriptionKeywords.Add('Nariño Altura');
    STT.TranscriptionKeywords.Add('Café La Ladera');
    STT.OnSessionReady := Oido.Listo;
    STT.OnSpeechStarted := Oido.HablaEmpieza;
    STT.OnSpeechStopped := Oido.HablaTermina;
    STT.OnTranscriptDelta := Oido.Delta;
    STT.OnTranscriptCompleted := Oido.Completo;
    STT.OnError := Oido.Error;
    // La captura: Source = asLoopback sobre la MISMA salida en la que se reproduce
    Captura.Source := asLoopback;
    Captura.DeviceId := IdSalida;
    Captura.OutputSampleRate := 24000;
    Captura.OutputChannels := 1;
    Captura.RealtimeSTT := STT;          // cada chunk va derecho al driver
    Captura.OnError := Oido.ErrorAudio;
    Player.DeviceId := IdSalida;
    Player.OnError := Oido.ErrorAudio;

    STT.Connect;
    Esperar(1500);
    Player.Active := True;
    Captura.Active := True;
    Esperar(500);
    Log(Format('  captura %s: nativo %d Hz x %d → %d Hz mono', [NombreSalida, Captura.NativeSampleRate,
      Captura.NativeChannels, Captura.EffectiveSampleRate]));

    // 1) La clienta pide (8,6 s): se transcribe
    Voz := LeerWav(TPath.Combine(Carpeta('media'), 'llamada-pedido.wav'));
    Log(Format('  suena llamada-pedido.wav (%.1f s)', [Length(Voz) / 48000]));
    Oido.Reloj := TStopwatch.StartNew;
    Player.PlayPCM16(Voz, 24000, 1);
    EsperarFinReproduccion(Player, 15000);
    Esperar(2500);
    Log(Format('  primer texto a los %d ms de empezar a sonar', [Oido.PrimerDelta]));

    // 2) Muted: la tienda esta hablando (su propio TTS suena por aqui) y no hay que transcribirla
    Captura.Muted := True;
    Voz := LeerWav(TPath.Combine(Carpeta('media'), 'llamada-despedida.wav'));
    Log(Format('  Muted = True; suena llamada-despedida.wav (%.1f s): no debería transcribirse', [Length(Voz) / 48000]));
    Player.PlayPCM16(Voz, 24000, 1);
    EsperarFinReproduccion(Player, 15000);
    Esperar(2500);
    Captura.Muted := False;
    Log('  Muted = False');
    Esperar(1000);

    Captura.Active := False;
    Player.Active := False;
    STT.Disconnect;
    Esperar(500);
  finally
    Captura.Free;
    Player.Free;
    STT.Free;
    Oido.Free;
  end;
end;

{ ---------------------------------------------------------------------------- C: la llamada en ingles por el cable }

procedure Llamada;
var
  Player: TAiAudioPlayer;
  Captura: TAiAudioCapture;
  Tr: TAiOpenAiRealtimeTranslate;
  Conn: TAiRealtimeConnection;
  Cliente, Lucia: TOido;
  IdIn, NombreIn, IdOut, NombreOut: string;
  Voz: TBytes;
  HayCable: Boolean;
begin
  Writeln;
  Writeln('== C. Un cliente llama en inglés: el cable virtual y la traducción en vivo ==');
  HayCable := BuscarSalida('CABLE Input', IdIn, NombreIn) and BuscarEntrada('CABLE Output', IdOut, NombreOut);
  if HayCable then
    Writeln('  cable: la voz se reproduce en «' + NombreIn + '» y se captura en «' + NombreOut + '»')
  else
    Writeln('  sin VB-CABLE: se usa el parlante y el micrófono predeterminados (sonará)');

  Cliente := TOido.Create;
  Lucia := TOido.Create;
  Player := TAiAudioPlayer.Create(nil);
  Captura := TAiAudioCapture.Create(nil);
  Tr := TAiOpenAiRealtimeTranslate.Create(nil);
  Conn := TAiRealtimeConnection.Create(nil);
  try
    Cliente.Rotulo := '[cliente]';
    Lucia.Rotulo := '[Lucía]  ';
    Player.DeviceId := IdIn;
    Player.OnError := Cliente.ErrorAudio;
    Captura.Source := asMicrophone;        // el otro extremo del cable es un "micrófono"
    Captura.DeviceId := IdOut;
    Captura.OutputSampleRate := 24000;
    Captura.OutputChannels := 1;
    Captura.OnError := Cliente.ErrorAudio;

    // --- 1) El cliente habla en ingles: el driver directo, traduce a espanol ---
    Tr.ApiKey := '@OPENAI_API_KEY';
    Tr.TargetLanguage := 'es';
    Tr.SourceTranscription := True;        // tambien lo que dijo, en su idioma
    Tr.InputSampleRate := 24000;
    Tr.OnSessionReady := Cliente.Listo;
    Tr.OnTranscriptDelta := Cliente.Delta;
    Tr.OnTranscriptCompleted := Cliente.Completo;
    Tr.OnAssistantTextDelta := Cliente.TraduccionDelta;
    Tr.OnAssistantText := Cliente.TraduccionTexto;
    Tr.OnAudioChunk := Cliente.Audio;      // la voz traducida (24 kHz): aqui se guarda; en la app sonaria
    Tr.OnAudioDone := Cliente.AudioFin;
    Tr.OnError := Cliente.Error;
    Captura.RealtimeSTT := Tr;
    Tr.Connect;
    Esperar(1500);
    Player.Active := True;
    Captura.Active := True;
    Esperar(500);
    Voz := LeerWav(TPath.Combine(Carpeta('media'), 'cliente-ingles.wav'));
    Log(Format('  el cliente habla (cliente-ingles.wav, %.1f s)', [Length(Voz) / 48000]));
    Cliente.Reloj := TStopwatch.StartNew;
    Player.PlayPCM16(Voz, 24000, 1);
    EsperarFinReproduccion(Player, 20000);
    Esperar(4000);
    if Trim(Cliente.Origen) <> '' then Log(Cliente.Rotulo + '  «' + Trim(Cliente.Origen) + '»');
    if Trim(Cliente.Traduccion) <> '' then Log(Cliente.Rotulo + '  → «' + Trim(Cliente.Traduccion) + '»');
    Log(Format('  primera traducción a los %d ms; voz traducida: %.1f s de audio', [Cliente.PrimerDelta,
      Length(Cliente.Voz) / 48000]));
    GuardarWav(TPath.Combine(Carpeta('salida'), 'cliente-en-espanol.wav'), Cliente.Voz, 24000);
    Captura.Active := False;
    Tr.Disconnect;
    Esperar(500);

    // --- 2) Lucia contesta en espanol: el mismo driver por el conector universal, a ingles ---
    Conn.DriverName := 'OpenAiTranslate';
    Conn.ApiKey := '@OPENAI_API_KEY';
    Conn.InputSampleRate := 24000;
    Conn.DriverParams.Values['TargetLanguage'] := 'en';       // lo propio del driver va en DriverParams
    Conn.DriverParams.Values['SourceTranscription'] := 'True';
    Conn.OnSessionReady := Lucia.Listo;
    Conn.OnTranscriptCompleted := Lucia.Completo;
    Conn.OnAssistantTextDelta := Lucia.TraduccionDelta;
    Conn.OnAssistantText := Lucia.TraduccionTexto;
    Conn.OnAudioChunk := Lucia.Audio;
    Conn.OnAudioDone := Lucia.AudioFin;
    Conn.OnError := Lucia.Error;
    Captura.RealtimeSTT := Conn;
    Conn.Connect;
    Esperar(1500);
    Captura.Active := True;
    Esperar(500);
    Voz := LeerWav(TPath.Combine(Carpeta('media'), 'lucia-respuesta.wav'));
    Log(Format('  Lucía contesta (lucia-respuesta.wav, %.1f s)', [Length(Voz) / 48000]));
    Lucia.Reloj := TStopwatch.StartNew;
    Player.PlayPCM16(Voz, 24000, 1);
    EsperarFinReproduccion(Player, 25000);
    Esperar(4000);
    if Trim(Lucia.Origen) <> '' then Log(Lucia.Rotulo + '  «' + Trim(Lucia.Origen) + '»');
    if Trim(Lucia.Traduccion) <> '' then Log(Lucia.Rotulo + '  → «' + Trim(Lucia.Traduccion) + '»');
    Log(Format('  primera traducción a los %d ms; voz traducida: %.1f s de audio', [Lucia.PrimerDelta,
      Length(Lucia.Voz) / 48000]));
    GuardarWav(TPath.Combine(Carpeta('salida'), 'lucia-en-ingles.wav'), Lucia.Voz, 24000);
    Captura.Active := False;
    Player.Active := False;
    Conn.Disconnect;
    Esperar(500);
    Writeln('  voces traducidas guardadas en salida\ (en la app irían a un TAiAudioPlayer: al parlante o al cable)');
  finally
    Captura.Free;
    Player.Free;
    Tr.Free;
    Conn.Free;
    Cliente.Free;
    Lucia.Free;
  end;
end;

begin
  SetConsoleOutputCP(CP_UTF8);
  try
    if GetEnvironmentVariable('OPENAI_API_KEY') = '' then
    begin
      Writeln('Falta OPENAI_API_KEY');
      Halt(2);
    end;
    if (ParamCount = 0) or Tiene('--dispositivos') then
      Dispositivos;
    if (ParamCount = 0) or Tiene('--loopback') then
      Loopback;
    if (ParamCount = 0) or Tiene('--llamada') then
      Llamada;
  except
    on E: Exception do
    begin
      Writeln('ERROR: ', E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
