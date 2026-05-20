object Form6: TForm6
  Left = 0
  Top = 0
  Caption = 'Form6'
  ClientHeight = 947
  ClientWidth = 1267
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object Label1: TLabel
    Left = 408
    Top = 13
    Width = 34
    Height = 15
    Caption = 'Label1'
  end
  object Label3: TLabel
    Left = 408
    Top = 45
    Width = 31
    Height = 15
    Caption = 'StartX'
  end
  object Label4: TLabel
    Left = 408
    Top = 77
    Width = 31
    Height = 15
    Caption = 'StopX'
  end
  object Label5: TLabel
    Left = 408
    Top = 107
    Width = 31
    Height = 15
    Caption = 'StopX'
  end
  object Label2: TLabel
    Left = 53
    Top = 124
    Width = 156
    Height = 15
    Caption = 'TrackBar de monitor de audio'
  end
  object LabelCurrentDevice: TLabel
    Left = 832
    Top = 43
    Width = 103
    Height = 15
    Caption = 'LabelCurrentDevice'
  end
  object ButtonStart: TButton
    Left = 48
    Top = 39
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = ButtonStartClick
  end
  object ButtonStop: TButton
    Left = 160
    Top = 41
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 1
    OnClick = ButtonStopClick
  end
  object MemoLog: TMemo
    Left = 48
    Top = 168
    Width = 537
    Height = 289
    Lines.Strings = (
      'MemoLog')
    TabOrder = 2
  end
  object ProgressBarLevel: TProgressBar
    Left = 53
    Top = 145
    Width = 156
    Height = 17
    TabOrder = 3
  end
  object SpinEditSilence: TSpinEdit
    Left = 624
    Top = 8
    Width = 121
    Height = 24
    MaxValue = 0
    MinValue = 0
    TabOrder = 4
    Value = 0
    OnChange = SpinEditSilenceChange
  end
  object FloatSpinEditStart: TSpinEdit
    Left = 624
    Top = 40
    Width = 121
    Height = 24
    MaxValue = 0
    MinValue = 0
    TabOrder = 5
    Value = 0
    OnChange = FloatSpinEditStartChange
  end
  object FloatSpinEditStop: TSpinEdit
    Left = 624
    Top = 72
    Width = 121
    Height = 24
    MaxValue = 0
    MinValue = 0
    TabOrder = 6
    Value = 0
    OnChange = FloatSpinEditStopChange
  end
  object SpinEditWakeWord: TSpinEdit
    Left = 624
    Top = 102
    Width = 121
    Height = 24
    MaxValue = 0
    MinValue = 0
    TabOrder = 7
    Value = 0
    OnChange = SpinEditWakeWordChange
  end
  object BtnRefreshDevices: TButton
    Left = 832
    Top = 77
    Width = 273
    Height = 25
    Caption = 'BtnRefreshDevices'
    TabOrder = 8
    OnClick = BtnRefreshDevicesClick
  end
  object MemoTranscription: TMemo
    Left = 72
    Top = 552
    Width = 537
    Height = 289
    Lines.Strings = (
      'MemoTranscription')
    TabOrder = 9
  end
  object ComboBoxDevices: TComboBox
    Left = 832
    Top = 10
    Width = 273
    Height = 23
    TabOrder = 10
    Text = 'ComboBoxDevices'
    OnChange = ComboBoxDevicesChange
  end
  object ChWakeUp: TCheckBox
    Left = 408
    Top = 128
    Width = 177
    Height = 17
    Caption = 'WakeUp Detect'
    TabOrder = 11
    OnClick = ChWakeUpClick
  end
  object AnimationTimer: TTimer
    Interval = 17
    OnTimer = AnimationTimerTimer
    Left = 392
    Top = 288
  end
  object AIVoiceMonitor: TAIVoiceMonitor
    SilenceDuration = 300
    SensitivityMultiplier = 2.000000000000000000
    StopSensitivityMultiplier = 1.500000000000000000
    WakeWordDurationMs = 2000
    TranscriptionMaxWaitMs = 5000
    FragmentSplitRatio = 0.350000000000000000
    OnChangeState = AIVoiceMonitorChangeState
    OnCalibrated = AIVoiceMonitorCalibrated
    OnUpdate = AIVoiceMonitorUpdate
    OnError = AIVoiceMonitorError
    OnWakeWordCheck = AIVoiceMonitorWakeWordCheck
    OnSpeechEnd = AIVoiceMonitorSpeechEnd
    OnTranscriptionFragment = AIVoiceMonitorTranscriptionFragment
    WakeWord = 'andrea'
    WakeWordActive = False
    PreBufferDurationMs = 700
    Left = 688
    Top = 304
  end
  object OpenDialog1: TOpenDialog
    Left = 816
    Top = 272
  end
end
