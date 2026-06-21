unit uMainVoiceBridgeUI;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.IOUtils, System.JSON, System.Threading, System.SyncObjs,
  System.Generics.Collections, System.Math, System.StrUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Layouts, FMX.StdCtrls, FMX.Edit, FMX.Memo, FMX.Memo.Types,
  FMX.Objects, FMX.ListBox, FMX.Controls.Presentation, FMX.ScrollBox,
  uMakerAi.Core,
  uMakerAi.Utils.AudioCapture,
  uMakerAi.Utils.AudioPlayback,
  uMakerAi.Realtime,
  uMakerAi.Realtime.OpenAI,
  uMakerAi.OpenAI.Audio,
  uMakerAi.Chat.AiConnection,
  uMakerAi.Chat.OpenAi,
  uMakerAi.Utils.PcmToWav;

const
  TRANSLATOR_DRIVER = 'OpenAi';
  TRANSLATOR_MODEL  = 'gpt-4o-mini';
  TRANSLATOR_APIKEY = '@OPENAI_API_KEY';
  STT_APIKEY        = '@OPENAI_API_KEY';
  TTS_PCM_RATE      = 24000;
  MEETING_HINT      = 'cable input';
  MAX_SPEAKERS      = 4;
  MIN_SAMPLE_SEC    = 2.0;
  MAX_SAMPLE_SEC    = 9.0;
  SEG_SAMPLE_RATE   = 16000;
  SEG_START_LEVEL   = 400;
  SEG_STOP_LEVEL    = 250;
  SEG_SILENCE_MS    = 900;
  SEG_MIN_MS        = 700;
  SEG_MAX_MS        = 25000;
  SEG_PREBUF_MS     = 400;
  ECHO_CACHE        = 5;

  SPEAKER_VOICES: array[0..MAX_SPEAKERS-1] of TAiTTSVoice = (
    TAiTTSVoice.tvNova, TAiTTSVoice.tvShimmer,
    TAiTTSVoice.tvCoral, TAiTTSVoice.tvSage);

type
  TSpeechSegmenter = class
  private
    FOnSegment:  TProc<TBytes>;
    FBuffer:     TMemoryStream;
    FPreBuffer:  TMemoryStream;
    FInSpeech:   Boolean;
    FSilenceMs:  Integer;
    FBytesPerMs: Integer;
    procedure CloseSegment;
  public
    constructor Create(aOnSegment: TProc<TBytes>);
    destructor  Destroy; override;
    procedure   Feed(const aChunk: TBytes; aChunkMs: Integer);
  end;

  TFormVoiceBridge = class(TForm)
    LayMain:         TLayout;
    LayTop:          TLayout;
    LblLoopback:     TLabel;
    CboLoopback:     TComboBox;
    LblMic:          TLabel;
    CboMic:          TComboBox;
    LblTTSLocal:     TLabel;
    CboTTSLocal:     TComboBox;
    LblTTSReun:      TLabel;
    CboTTSReun:      TComboBox;
    LblDe:           TLabel;
    EdtLangSrc:      TEdit;
    LblA:            TLabel;
    EdtLangDst:      TEdit;
    LblCable:        TLabel;
    BtnStart:        TButton;
    RectTopSep:      TRectangle;
    LayBottom:       TLayout;
    LblStatus:       TLabel;
    LayMiddle:       TLayout;
    LayYo:           TLayout;
    RectYoHeader:    TRectangle;
    LblYoHeader:     TLabel;
    MemoYo:          TMemo;
    RectMiddleSep:   TRectangle;
    LayEllos:        TLayout;
    RectEllosHeader: TRectangle;
    LblEllosHeader:  TLabel;
    LaySpeakers:     TLayout;
    LaySpk1:         TLayout;
    LblSpeaker1:     TLabel;
    EdtSpeaker1:     TEdit;
    LaySpk2:         TLayout;
    LblSpeaker2:     TLabel;
    EdtSpeaker2:     TEdit;
    LaySpk3:         TLayout;
    LblSpeaker3:     TLabel;
    EdtSpeaker3:     TEdit;
    LaySpk4:         TLayout;
    LblSpeaker4:     TLabel;
    EdtSpeaker4:     TEdit;
    MemoEllos:       TMemo;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BtnStartClick(Sender: TObject);
    procedure EdtSpeaker1ChangeTracking(Sender: TObject);
    procedure EdtSpeaker2ChangeTracking(Sender: TObject);
    procedure EdtSpeaker3ChangeTracking(Sender: TObject);
    procedure EdtSpeaker4ChangeTracking(Sender: TObject);

  private
    FCaptMic:     TAiAudioCapture;
    FSttMic:      TAiOpenAiRealtimeSTT;
    FChatYo:      TAiChatConnection;
    FTTS:         TAiOpenAiAudio;
    FPlayerCable: TAiAudioPlayer;

    FCaptLoop:    TAiAudioCapture;
    FSegmenter:   TSpeechSegmenter;
    FAudioApi:    TAiOpenAiAudio;
    FChatEllos:   TAiChatConnection;
    FPlayerLocal: TAiAudioPlayer;

    FRunning:  Boolean;
    FClosing:  Boolean;
    FCableId:  string;
    FHasCable: Boolean;

    FSpeakerNames:   array[0..MAX_SPEAKERS-1] of string;
    FSpeakerSamples: array[0..MAX_SPEAKERS-1] of TBytes;

    FLastCableTexts: TList<string>;
    FSegLock:        TCriticalSection;
    FChatLock:       TCriticalSection;

    FLoopbackIds: TArray<string>;
    FMicIds:      TArray<string>;
    FTTSLocalIds: TArray<string>;
    FTTSReunIds:  TArray<string>;

    procedure PopulateDevices;
    procedure CheckCable;
    procedure StartBridge;
    procedure StopBridge;

    procedure OnSttSessionReady(Sender: TObject);
    procedure OnSttTranscript(Sender: TObject; const aTranscript, aItemId: string);
    procedure OnSttError(Sender: TObject; const aMsg, aCode: string);
    procedure OnCaptMicError(Sender: TObject; const aMsg: string);
    procedure OnPlayerCableState(Sender: TObject; aIsPlaying: Boolean);
    procedure OnPlayerCableError(Sender: TObject; const aMsg: string);

    procedure OnLoopbackData(Sender: TObject; const aBuffer: TBytes; aSampleRate, aChannels: Integer);
    procedure OnCaptLoopError(Sender: TObject; const aMsg: string);
    procedure OnPlayerLocalState(Sender: TObject; aIsPlaying: Boolean);
    procedure OnPlayerLocalError(Sender: TObject; const aMsg: string);
    procedure ProcessSegment(const aPcm: TBytes);
    procedure ProcessSegmentWork(const aPcm: TBytes);
    function  TryEnrollSpeaker(const aPcm: TBytes; const aRawSpeaker: string;
      const aSegments: TDiarizedSegments; out aName: string): Boolean;
    procedure RebuildSpeakerRegistry;

    procedure LogYo(const aText: string);
    procedure LogEllos(const aText: string);
    procedure SetStatus(const aText: string);
    function  DeviceId(aCbo: TComboBox; const aIds: TArray<string>): string;
    function  SpeakerIndex(const aApiLabel: string): Integer;
    procedure EmitSegmentGroup(const aGroupSpk, aGroupTxt: string);
    procedure ReconnectSTT;
    procedure RenameSpeaker(aIdx: Integer; const aNewName: string);
    function  IsEcho(const aText: string): Boolean;
    procedure AddEchoCache(const aText: string);
    function  GetSpeakerEdit(aIdx: Integer): TEdit;
  end;

var
  FormVoiceBridge: TFormVoiceBridge;

implementation

{$R *.fmx}

{ TSpeechSegmenter }

constructor TSpeechSegmenter.Create(aOnSegment: TProc<TBytes>);
begin
  inherited Create;
  FOnSegment  := aOnSegment;
  FBuffer     := TMemoryStream.Create;
  FPreBuffer  := TMemoryStream.Create;
  FBytesPerMs := (SEG_SAMPLE_RATE * 2) div 1000;
end;

destructor TSpeechSegmenter.Destroy;
begin
  FBuffer.Free;
  FPreBuffer.Free;
  inherited;
end;

procedure TSpeechSegmenter.CloseSegment;
var
  Pcm: TBytes;
begin
  if FBuffer.Size >= SEG_MIN_MS * FBytesPerMs then
  begin
    SetLength(Pcm, FBuffer.Size);
    FBuffer.Position := 0;
    FBuffer.ReadBuffer(Pcm[0], FBuffer.Size);
    if Assigned(FOnSegment) then
      FOnSegment(Pcm);
  end;
  FBuffer.Clear;
  FPreBuffer.Clear;
  FInSpeech  := False;
  FSilenceMs := 0;
end;

procedure TSpeechSegmenter.Feed(const aChunk: TBytes; aChunkMs: Integer);
var
  I, N:   Integer;
  Sum:    Int64;
  Level:  Int64;
  MaxPre: Int64;
  Tmp:    TMemoryStream;
begin
  if Length(aChunk) = 0 then Exit;
  N   := Length(aChunk) div 2;
  Sum := 0;
  for I := 0 to N - 1 do
    Sum := Sum + Abs(PSmallInt(@aChunk[I * 2])^);
  Level := Sum div N;

  if not FInSpeech then
  begin
    FPreBuffer.Position := FPreBuffer.Size;
    FPreBuffer.WriteBuffer(aChunk[0], Length(aChunk));
    MaxPre := SEG_PREBUF_MS * FBytesPerMs;
    if FPreBuffer.Size > MaxPre then
    begin
      Tmp := TMemoryStream.Create;
      try
        FPreBuffer.Position := FPreBuffer.Size - MaxPre;
        Tmp.CopyFrom(FPreBuffer, MaxPre);
        FPreBuffer.Clear;
        Tmp.Position := 0;
        FPreBuffer.CopyFrom(Tmp, 0);
      finally
        Tmp.Free;
      end;
    end;
    if Level >= SEG_START_LEVEL then
    begin
      FInSpeech  := True;
      FSilenceMs := 0;
      FBuffer.Clear;
      FPreBuffer.Position := 0;
      FBuffer.CopyFrom(FPreBuffer, 0);
      FPreBuffer.Clear;
    end;
  end
  else
  begin
    FBuffer.Position := FBuffer.Size;
    FBuffer.WriteBuffer(aChunk[0], Length(aChunk));
    if Level < SEG_STOP_LEVEL then
      Inc(FSilenceMs, aChunkMs)
    else
      FSilenceMs := 0;
    if (FSilenceMs >= SEG_SILENCE_MS) or
       (FBuffer.Size >= SEG_MAX_MS * FBytesPerMs) then
      CloseSegment;
  end;
end;

{ TFormVoiceBridge }

procedure TFormVoiceBridge.FormCreate(Sender: TObject);
begin
  FSegLock        := TCriticalSection.Create;
  FChatLock       := TCriticalSection.Create;
  FLastCableTexts := TList<string>.Create;
  PopulateDevices;
  CheckCable;
end;

procedure TFormVoiceBridge.FormDestroy(Sender: TObject);
begin
  FClosing := True;
  if FRunning then StopBridge;
  FLastCableTexts.Free;
  FChatLock.Free;
  FSegLock.Free;
end;

procedure TFormVoiceBridge.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FRunning then
  begin
    FClosing := True;
    StopBridge;
  end;
  CanClose := True;
end;

procedure TFormVoiceBridge.PopulateDevices;
var
  D: TAiAudioDeviceInfo;
begin
  CboLoopback.Items.Clear;
  SetLength(FLoopbackIds, 0);
  for D in TAiAudioCapture.GetAudioDevices(asLoopback) do
  begin
    CboLoopback.Items.Add(D.DeviceName + IfThen(D.IsDefault, ' (predeterminado)', ''));
    FLoopbackIds := FLoopbackIds + [D.EndpointId];
    if D.IsDefault then CboLoopback.ItemIndex := CboLoopback.Items.Count - 1;
  end;

  CboMic.Items.Clear;
  SetLength(FMicIds, 0);
  for D in TAiAudioCapture.GetAudioDevices(asMicrophone) do
  begin
    CboMic.Items.Add(D.DeviceName + IfThen(D.IsDefault, ' (predeterminado)', ''));
    FMicIds := FMicIds + [D.EndpointId];
    if D.IsDefault then CboMic.ItemIndex := CboMic.Items.Count - 1;
  end;

  CboTTSLocal.Items.Clear;
  SetLength(FTTSLocalIds, 0);
  for D in TAiAudioPlayer.GetPlaybackDevices do
  begin
    CboTTSLocal.Items.Add(D.DeviceName + IfThen(D.IsDefault, ' (predeterminado)', ''));
    FTTSLocalIds := FTTSLocalIds + [D.EndpointId];
    if D.IsDefault then CboTTSLocal.ItemIndex := CboTTSLocal.Items.Count - 1;
  end;

  CboTTSReun.Items.Clear;
  SetLength(FTTSReunIds, 0);
  for D in TAiAudioPlayer.GetPlaybackDevices do
  begin
    CboTTSReun.Items.Add(D.DeviceName + IfThen(D.IsDefault, ' (predeterminado)', ''));
    FTTSReunIds := FTTSReunIds + [D.EndpointId];
    if Pos(MEETING_HINT, LowerCase(D.DeviceName)) > 0 then
      CboTTSReun.ItemIndex := CboTTSReun.Items.Count - 1
    else if D.IsDefault and (CboTTSReun.ItemIndex < 0) then
      CboTTSReun.ItemIndex := CboTTSReun.Items.Count - 1;
  end;
end;

procedure TFormVoiceBridge.CheckCable;
var
  D:         TAiAudioDeviceInfo;
  IsDefault: Boolean;
begin
  FCableId  := '';
  FHasCable := False;
  IsDefault := False;

  for D in TAiAudioPlayer.GetPlaybackDevices do
    if Pos(MEETING_HINT, LowerCase(D.DeviceName)) > 0 then
    begin
      FCableId  := D.EndpointId;
      FHasCable := True;
      IsDefault := D.IsDefault;
      Break;
    end;

  if FHasCable then
  begin
    if IsDefault then
      LblCable.Text :=
        'ATENCION: CABLE Input es la salida predeterminada. ' +
        'Cambia la salida a tus auriculares en Configuracion de Sonido.'
    else
      LblCable.Text := 'VB-CABLE detectado. Mic de reunion = CABLE Output. OK.';
  end
  else
    LblCable.Text :=
      'VB-CABLE no encontrado. Modo prueba: TTS sonara por dispositivo predeterminado.';
end;

function TFormVoiceBridge.DeviceId(aCbo: TComboBox; const aIds: TArray<string>): string;
begin
  Result := '';
  if (aCbo.ItemIndex >= 0) and (aCbo.ItemIndex < Length(aIds)) then
    Result := aIds[aCbo.ItemIndex];
end;

procedure TFormVoiceBridge.StartBridge;
var
  LLoopbackId, LMicId, LTTSLocalId, LCableId: string;
begin
  LLoopbackId  := DeviceId(CboLoopback, FLoopbackIds);
  LMicId       := DeviceId(CboMic,      FMicIds);
  LTTSLocalId  := DeviceId(CboTTSLocal, FTTSLocalIds);
  LCableId     := IfThen(FHasCable, FCableId, DeviceId(CboTTSReun, FTTSReunIds));

  FChatEllos := TAiChatConnection.Create(nil);
  FChatEllos.DriverName := TRANSLATOR_DRIVER;
  FChatEllos.Model      := TRANSLATOR_MODEL;
  FChatEllos.Params.Values['Asynchronous'] := 'False';
  FChatEllos.Params.Values['ApiKey']       := TRANSLATOR_APIKEY;

  FAudioApi := TAiOpenAiAudio.Create(nil);
  FAudioApi.ApiKey                      := '@OPENAI_API_KEY';
  FAudioApi.TranscriptionModel          := TAiTranscriptionModel.tmGpt4oDiarize;
  FAudioApi.TranscriptionResponseFormat := TAiTranscriptionResponseFormat.trfDiarizedJson;
  FAudioApi.TTSModel                    := TAiTTSModel.gpt_4o_mini_tts;
  FAudioApi.TTSResponseFormat           := TAiTTSResponseFormat.trfPcm;
  FAudioApi.TTSSpeed                    := 1.0;

  FPlayerLocal := TAiAudioPlayer.Create(nil);
  FPlayerLocal.DeviceId      := LTTSLocalId;
  FPlayerLocal.OnStateChange := OnPlayerLocalState;
  FPlayerLocal.OnError       := OnPlayerLocalError;

  FSegmenter := TSpeechSegmenter.Create(
    procedure(aPcm: TBytes)
    begin
      ProcessSegment(aPcm);
    end);

  FCaptLoop := TAiAudioCapture.Create(nil);
  FCaptLoop.Source           := asLoopback;
  FCaptLoop.DeviceId         := LLoopbackId;
  FCaptLoop.OutputSampleRate := SEG_SAMPLE_RATE;
  FCaptLoop.OutputChannels   := 1;
  FCaptLoop.OnData           := OnLoopbackData;
  FCaptLoop.OnError          := OnCaptLoopError;

  FChatYo := TAiChatConnection.Create(nil);
  FChatYo.DriverName := TRANSLATOR_DRIVER;
  FChatYo.Model      := TRANSLATOR_MODEL;
  FChatYo.Params.Values['Asynchronous'] := 'False';
  FChatYo.Params.Values['ApiKey']       := TRANSLATOR_APIKEY;

  FTTS := TAiOpenAiAudio.Create(nil);
  FTTS.ApiKey            := '@OPENAI_API_KEY';
  FTTS.TTSModel          := TAiTTSModel.gpt_4o_mini_tts;
  FTTS.TTSVoice          := TAiTTSVoice.tvOnyx;
  FTTS.TTSResponseFormat := TAiTTSResponseFormat.trfPcm;
  FTTS.TTSSpeed          := 1.0;

  FPlayerCable := TAiAudioPlayer.Create(nil);
  FPlayerCable.DeviceId      := LCableId;
  FPlayerCable.OnStateChange := OnPlayerCableState;
  FPlayerCable.OnError       := OnPlayerCableError;

  FSttMic := TAiOpenAiRealtimeSTT.Create(nil);
  FSttMic.ApiKey                := STT_APIKEY;
  FSttMic.Language              := EdtLangSrc.Text;
  FSttMic.OnSessionReady        := OnSttSessionReady;
  FSttMic.OnTranscriptCompleted := OnSttTranscript;
  FSttMic.OnError               := OnSttError;

  FCaptMic := TAiAudioCapture.Create(nil);
  FCaptMic.Source           := asMicrophone;
  FCaptMic.DeviceId         := LMicId;
  FCaptMic.OutputSampleRate := 16000;
  FCaptMic.OutputChannels   := 1;
  FCaptMic.RealtimeSTT      := FSttMic;
  FCaptMic.OnError          := OnCaptMicError;

  FPlayerLocal.Active  := True;
  FPlayerCable.Active  := True;
  FSttMic.Connect;
  FCaptMic.Active  := True;
  FCaptLoop.Active := True;

  FRunning := True;
  BtnStart.Text := 'DETENER';
  SetStatus('Puente activo. STT conectando...');
end;

procedure TFormVoiceBridge.StopBridge;
begin
  FClosing := True;
  FRunning := False;

  if Assigned(FCaptLoop) then FCaptLoop.Active := False;
  if Assigned(FCaptMic)  then FCaptMic.Active  := False;
  if Assigned(FSttMic) and FSttMic.IsConnected then FSttMic.Disconnect;
  if Assigned(FPlayerLocal) then FPlayerLocal.Active := False;
  if Assigned(FPlayerCable) then FPlayerCable.Active := False;

  FreeAndNil(FCaptLoop);
  FreeAndNil(FSegmenter);
  FreeAndNil(FCaptMic);
  FreeAndNil(FSttMic);
  FreeAndNil(FPlayerLocal);
  FreeAndNil(FPlayerCable);
  FreeAndNil(FAudioApi);
  FreeAndNil(FTTS);
  FreeAndNil(FChatEllos);
  FreeAndNil(FChatYo);

  FillChar(FSpeakerNames,   SizeOf(FSpeakerNames),   0);
  FillChar(FSpeakerSamples, SizeOf(FSpeakerSamples), 0);

  FClosing := False;
  BtnStart.Text := 'INICIAR';
  SetStatus('Puente detenido.');
end;

procedure TFormVoiceBridge.BtnStartClick(Sender: TObject);
begin
  if FRunning then StopBridge else StartBridge;
end;

{ YO side callbacks }

procedure TFormVoiceBridge.OnSttSessionReady(Sender: TObject);
begin
  TThread.Queue(nil, procedure begin SetStatus('STT conectado y listo.'); end);
end;

procedure TFormVoiceBridge.OnSttError(Sender: TObject; const aMsg, aCode: string);
begin
  TThread.Queue(nil, procedure
  begin
    SetStatus('STT error [' + aCode + ']: ' + aMsg);
    if FRunning and not FClosing then
      ReconnectSTT;
  end);
end;

procedure TFormVoiceBridge.OnCaptMicError(Sender: TObject; const aMsg: string);
begin
  TThread.Queue(nil, procedure begin SetStatus('Error mic: ' + aMsg); end);
end;

procedure TFormVoiceBridge.OnPlayerCableError(Sender: TObject; const aMsg: string);
begin
  TThread.Queue(nil, procedure begin SetStatus('Error player cable: ' + aMsg); end);
end;

procedure TFormVoiceBridge.OnPlayerCableState(Sender: TObject; aIsPlaying: Boolean);
begin
  if not aIsPlaying and not FHasCable and Assigned(FCaptLoop) then
    FCaptLoop.Muted := False;
end;

procedure TFormVoiceBridge.OnSttTranscript(Sender: TObject; const aTranscript, aItemId: string);
var
  LText: string;
begin
  LText := Trim(aTranscript);
  if LText = '' then Exit;

  if IsEcho(LText) then
  begin
    TThread.Queue(nil, procedure
    begin
      SetStatus('ECO detectado: el microfono captura el audio del TTS. Usa auriculares.');
    end);
    Exit;
  end;

  TThread.Queue(nil, procedure begin LogYo(LText); end);

  TTask.Run(procedure
  var
    LPrompt, LTrans: string;
    LStream:         TMemoryStream;
    LPcm:            TBytes;
  begin
    try
      FChatLock.Enter;
      try
        if FClosing then Exit;
        FChatYo.NewChat;
        LPrompt := Format(
          'Translate the following text to %s. Reply with ONLY the translation, ' +
          'no comments or quotes:'#10'%s', [EdtLangDst.Text, LText]);
        LTrans := Trim(FChatYo.AddMessageAndRun(LPrompt, 'user', []));
        if LTrans = '' then Exit;
        TThread.Queue(nil, procedure begin LogYo('   -> ' + LTrans); end);

        LStream := FTTS.Speech(LTrans);
        try
          if (LStream = nil) or (LStream.Size = 0) then Exit;
          SetLength(LPcm, LStream.Size);
          LStream.Position := 0;
          LStream.ReadBuffer(LPcm[0], LStream.Size);
        finally
          LStream.Free;
        end;

        if FClosing or not Assigned(FPlayerCable) or not FPlayerCable.Active then Exit;
        AddEchoCache(LTrans);
        if not FHasCable and Assigned(FCaptLoop) then
          FCaptLoop.Muted := True;
        FPlayerCable.PlayPCM16(LPcm, TTS_PCM_RATE, 1);
      finally
        FChatLock.Leave;
      end;
    except
      on E: Exception do
        TThread.Queue(nil, procedure begin SetStatus('Error YO TTS: ' + E.Message); end);
    end;
  end);
end;

{ ELLOS side callbacks }

procedure TFormVoiceBridge.OnLoopbackData(Sender: TObject; const aBuffer: TBytes;
  aSampleRate, aChannels: Integer);
begin
  if not FClosing and Assigned(FSegmenter) then
    FSegmenter.Feed(aBuffer, FCaptLoop.ChunkDurationMs);
end;

procedure TFormVoiceBridge.OnCaptLoopError(Sender: TObject; const aMsg: string);
begin
  TThread.Queue(nil, procedure begin SetStatus('Error loopback: ' + aMsg); end);
end;

procedure TFormVoiceBridge.OnPlayerLocalState(Sender: TObject; aIsPlaying: Boolean);
begin
  if not aIsPlaying and Assigned(FCaptLoop) then
    FCaptLoop.Muted := False;
end;

procedure TFormVoiceBridge.OnPlayerLocalError(Sender: TObject; const aMsg: string);
begin
  TThread.Queue(nil, procedure begin SetStatus('Error player local: ' + aMsg); end);
end;

procedure TFormVoiceBridge.ProcessSegment(const aPcm: TBytes);
var
  LData: TBytes;
begin
  LData := aPcm;
  TTask.Run(procedure
  begin
    try
      ProcessSegmentWork(LData);
    except
      on E: Exception do
        TThread.Queue(nil, procedure begin SetStatus('Error diarizando: ' + E.Message); end);
    end;
  end);
end;

function TFormVoiceBridge.TryEnrollSpeaker(const aPcm: TBytes; const aRawSpeaker: string;
  const aSegments: TDiarizedSegments; out aName: string): Boolean;
var
  I:                           Integer;
  RunStart, RunEnd:            Double;
  BestStart, BestEnd:          Double;
  InRun:                       Boolean;
  StartByte, LenBytes:         Integer;
  LPcm, LWav:                  TMemoryStream;
  LWavBytes:                   TBytes;
  LIdx:                        Integer;
begin
  Result := False;
  aName  := '';
  if not Assigned(FAudioApi) or (FAudioApi.KnownSpeakerCount >= MAX_SPEAKERS) then Exit;

  BestStart := 0; BestEnd := 0;
  InRun     := False; RunStart := 0; RunEnd := 0;
  for I := 0 to High(aSegments) do
  begin
    if SameText(aSegments[I].Speaker, aRawSpeaker) then
    begin
      if not InRun then begin InRun := True; RunStart := aSegments[I].StartTime; end;
      RunEnd := aSegments[I].EndTime;
    end
    else if InRun then
    begin
      if (RunEnd - RunStart) > (BestEnd - BestStart) then
      begin BestStart := RunStart; BestEnd := RunEnd; end;
      InRun := False;
    end;
  end;
  if InRun and ((RunEnd - RunStart) > (BestEnd - BestStart)) then
  begin BestStart := RunStart; BestEnd := RunEnd; end;

  if (BestEnd - BestStart) < MIN_SAMPLE_SEC then Exit;
  if (BestEnd - BestStart) > MAX_SAMPLE_SEC then BestEnd := BestStart + MAX_SAMPLE_SEC;

  StartByte := Round(BestStart * SEG_SAMPLE_RATE) * 2;
  LenBytes  := Round((BestEnd - BestStart) * SEG_SAMPLE_RATE) * 2;
  if StartByte + LenBytes > Length(aPcm) then
    LenBytes := Length(aPcm) - StartByte;
  if LenBytes <= 0 then Exit;

  LPcm := TMemoryStream.Create;
  try
    LPcm.WriteBuffer(aPcm[StartByte], LenBytes);
    if not ConvertPCMStreamToWAVStream(LPcm, LWav, SEG_SAMPLE_RATE, 1, 16) then Exit;
    try
      SetLength(LWavBytes, LWav.Size);
      LWav.Position := 0;
      LWav.ReadBuffer(LWavBytes[0], LWav.Size);
      LWav.Position := 0;

      aName := Format('Hablante %d', [FAudioApi.KnownSpeakerCount + 1]);
      FAudioApi.AddKnownSpeaker(aName, LWav, 'audio/wav');

      LIdx := FAudioApi.KnownSpeakerCount - 1;
      FSpeakerSamples[LIdx] := LWavBytes;
      FSpeakerNames[LIdx]   := aName;

      var LNameCopy := aName;
      TThread.Queue(nil, procedure
      var
        LEdit: TEdit;
      begin
        LEdit := GetSpeakerEdit(LIdx);
        if Assigned(LEdit) then LEdit.Text := LNameCopy;
        LogEllos('(nuevo hablante registrado: ' + LNameCopy + ')');
      end);
      Result := True;
    finally
      LWav.Free;
    end;
  finally
    LPcm.Free;
  end;
end;

procedure TFormVoiceBridge.RebuildSpeakerRegistry;
var
  I:       Integer;
  LStream: TMemoryStream;
begin
  if not Assigned(FAudioApi) then Exit;
  FAudioApi.ClearKnownSpeakers;
  for I := 0 to MAX_SPEAKERS - 1 do
    if (Length(FSpeakerSamples[I]) > 0) and (FSpeakerNames[I] <> '') then
    begin
      LStream := TMemoryStream.Create;
      try
        LStream.WriteBuffer(FSpeakerSamples[I][0], Length(FSpeakerSamples[I]));
        LStream.Position := 0;
        FAudioApi.AddKnownSpeaker(FSpeakerNames[I], LStream, 'audio/wav');
      finally
        LStream.Free;
      end;
    end;
end;

procedure TFormVoiceBridge.EmitSegmentGroup(const aGroupSpk, aGroupTxt: string);
var
  LTranslated: string;
  LTtsStream:  TMemoryStream;
  LTtsPcm:     TBytes;
  LVoiceIdx:   Integer;
begin
  if (aGroupTxt = '') or FClosing then Exit;
  TThread.Queue(nil, procedure begin
    LogEllos(aGroupSpk + ': ' + aGroupTxt);
  end);

  FChatEllos.NewChat;
  LTranslated := Trim(FChatEllos.AddMessageAndRun(
    Format('Traduce al %s el siguiente texto. Responde UNICAMENTE con la traduccion, ' +
      'sin comentarios ni comillas:'#10'%s', [EdtLangDst.Text, aGroupTxt]),
    'user', []));
  if LTranslated = '' then Exit;

  TThread.Queue(nil, procedure begin
    LogEllos('   -> ' + LTranslated);
  end);

  LVoiceIdx := SpeakerIndex(aGroupSpk);
  if (LVoiceIdx >= 0) and (LVoiceIdx <= High(SPEAKER_VOICES)) then
    FAudioApi.TTSVoice := SPEAKER_VOICES[LVoiceIdx]
  else
    FAudioApi.TTSVoice := TAiTTSVoice.tvNova;

  LTtsStream := FAudioApi.Speech(LTranslated);
  try
    if (LTtsStream <> nil) and (LTtsStream.Size > 0) and
       not FClosing and Assigned(FPlayerLocal) and FPlayerLocal.Active then
    begin
      SetLength(LTtsPcm, LTtsStream.Size);
      LTtsStream.Position := 0;
      LTtsStream.ReadBuffer(LTtsPcm[0], LTtsStream.Size);
      if Assigned(FCaptLoop) then FCaptLoop.Muted := True;
      FPlayerLocal.PlayPCM16(LTtsPcm, TTS_PCM_RATE, 1);
    end;
  finally
    LTtsStream.Free;
  end;
end;

procedure TFormVoiceBridge.ProcessSegmentWork(const aPcm: TBytes);
var
  LPcmStream, LWavStream: TMemoryStream;
  LMedia:                 TAiMediaFile;
  LRes:                   TTranscriptionResult;
  LBatch:                 TDictionary<string, string>;
  I:                      Integer;
  LIdx:                   Integer;
  LRaw, LDisplay:         string;
  LGroupSpk, LGroupTxt:   string;

begin
  FSegLock.Enter;
  try
    if FClosing then Exit;

    LPcmStream := TMemoryStream.Create;
    try
      LPcmStream.WriteBuffer(aPcm[0], Length(aPcm));
      if not ConvertPCMStreamToWAVStream(LPcmStream, LWavStream, SEG_SAMPLE_RATE, 1, 16) then Exit;
    finally
      LPcmStream.Free;
    end;

    LMedia := TAiMediaFile.Create;
    try
      LMedia.LoadFromStream('segment.wav', LWavStream);
    finally
      LWavStream.Free;
    end;

    try
      try
        LRes := FAudioApi.Transcribe(LMedia);
      except
        on E: Exception do
        begin
          if Pos('known_speaker', LowerCase(E.Message)) > 0 then
          begin
            TThread.Queue(nil, procedure begin
              SetStatus('Muestras de hablantes rechazadas; reiniciando registro...');
            end);
            FAudioApi.ClearKnownSpeakers;
            FillChar(FSpeakerNames,   SizeOf(FSpeakerNames),   0);
            FillChar(FSpeakerSamples, SizeOf(FSpeakerSamples), 0);
            TThread.Queue(nil, procedure
            var
              I2:    Integer;
              LEdit: TEdit;
            begin
              for I2 := 0 to MAX_SPEAKERS - 1 do
              begin
                LEdit := GetSpeakerEdit(I2);
                if Assigned(LEdit) then LEdit.Text := '';
              end;
            end);
            LRes := FAudioApi.Transcribe(LMedia);
          end
          else raise;
        end;
      end;
    finally
      LMedia.Free;
    end;

    try
      if Length(LRes.Segments) = 0 then
      begin
        if Trim(LRes.Text) <> '' then
          TThread.Queue(nil, procedure begin LogEllos('(sin hablantes) ' + Trim(LRes.Text)); end);
        Exit;
      end;

      LBatch := TDictionary<string, string>.Create;
      try
        for I := 0 to High(LRes.Segments) do
        begin
          LRaw := LRes.Segments[I].Speaker;
          if (LRaw = '') or LBatch.ContainsKey(LRaw) then Continue;
          LIdx := SpeakerIndex(LRaw);
          if LIdx >= 0 then
            LBatch.Add(LRaw, FSpeakerNames[LIdx])
          else if TryEnrollSpeaker(aPcm, LRaw, LRes.Segments, LDisplay) then
            LBatch.Add(LRaw, LDisplay)
          else
            LBatch.Add(LRaw, 'Hablante ' + LRaw);
        end;

        LGroupSpk := ''; LGroupTxt := '';
        for I := 0 to High(LRes.Segments) do
        begin
          LRaw := LRes.Segments[I].Speaker;
          if not LBatch.TryGetValue(LRaw, LDisplay) then LDisplay := 'Hablante ?';
          if LDisplay <> LGroupSpk then
          begin
            EmitSegmentGroup(LGroupSpk, LGroupTxt);
            LGroupSpk := LDisplay;
            LGroupTxt := Trim(LRes.Segments[I].Text);
          end
          else
            LGroupTxt := LGroupTxt + ' ' + Trim(LRes.Segments[I].Text);
        end;
        EmitSegmentGroup(LGroupSpk, LGroupTxt);
      finally
        LBatch.Free;
      end;
    finally
      LRes.Free;
    end;
  finally
    FSegLock.Leave;
  end;
end;

{ Speaker management }

function TFormVoiceBridge.SpeakerIndex(const aApiLabel: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to MAX_SPEAKERS - 1 do
    if SameText(FSpeakerNames[I], aApiLabel) then Exit(I);
end;

function TFormVoiceBridge.GetSpeakerEdit(aIdx: Integer): TEdit;
begin
  case aIdx of
    0: Result := EdtSpeaker1;
    1: Result := EdtSpeaker2;
    2: Result := EdtSpeaker3;
    3: Result := EdtSpeaker4;
  else Result := nil;
  end;
end;

procedure TFormVoiceBridge.RenameSpeaker(aIdx: Integer; const aNewName: string);
begin
  if (aIdx < 0) or (aIdx >= MAX_SPEAKERS) then Exit;
  if aNewName <> '' then
    FSpeakerNames[aIdx] := aNewName
  else
    FSpeakerNames[aIdx] := Format('Hablante %d', [aIdx + 1]);
  FSegLock.Enter;
  try
    RebuildSpeakerRegistry;
  finally
    FSegLock.Leave;
  end;
end;

procedure TFormVoiceBridge.EdtSpeaker1ChangeTracking(Sender: TObject);
begin RenameSpeaker(0, EdtSpeaker1.Text); end;

procedure TFormVoiceBridge.EdtSpeaker2ChangeTracking(Sender: TObject);
begin RenameSpeaker(1, EdtSpeaker2.Text); end;

procedure TFormVoiceBridge.EdtSpeaker3ChangeTracking(Sender: TObject);
begin RenameSpeaker(2, EdtSpeaker3.Text); end;

procedure TFormVoiceBridge.EdtSpeaker4ChangeTracking(Sender: TObject);
begin RenameSpeaker(3, EdtSpeaker4.Text); end;

{ Echo detection }

function TFormVoiceBridge.IsEcho(const aText: string): Boolean;
var
  LNorm, LCached: string;
begin
  Result := False;
  LNorm := LowerCase(Trim(aText));
  if Length(LNorm) < 10 then Exit;
  FSegLock.Enter;
  try
    for LCached in FLastCableTexts do
      if Pos(LNorm, LowerCase(LCached)) > 0 then
        Exit(True);
  finally
    FSegLock.Leave;
  end;
end;

procedure TFormVoiceBridge.AddEchoCache(const aText: string);
begin
  FSegLock.Enter;
  try
    FLastCableTexts.Add(aText);
    while FLastCableTexts.Count > ECHO_CACHE do
      FLastCableTexts.Delete(0);
  finally
    FSegLock.Leave;
  end;
end;

procedure TFormVoiceBridge.ReconnectSTT;
begin
  if FClosing or not FRunning then Exit;
  TTask.Run(procedure
  begin
    Sleep(2000);
    TThread.Queue(nil, procedure
    begin
      if FClosing or not FRunning or not Assigned(FSttMic) then Exit;
      try
        if FSttMic.IsConnected then FSttMic.Disconnect;
        FSttMic.Connect;
        SetStatus('STT reconectando...');
      except
        on E: Exception do SetStatus('Error reconectando STT: ' + E.Message);
      end;
    end);
  end);
end;

{ UI helpers }

procedure TFormVoiceBridge.LogYo(const aText: string);
begin
  MemoYo.Lines.Add(aText);
  MemoYo.GoToTextEnd;
end;

procedure TFormVoiceBridge.LogEllos(const aText: string);
begin
  MemoEllos.Lines.Add(aText);
  MemoEllos.GoToTextEnd;
end;

procedure TFormVoiceBridge.SetStatus(const aText: string);
begin
  LblStatus.Text := aText;
end;

end.
