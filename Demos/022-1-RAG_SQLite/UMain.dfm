object FMain: TFMain
  Left = 0
  Top = 0
  Caption = 'SQLITE - RAG - MAKERAI'
  ClientHeight = 562
  ClientWidth = 795
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object labTipo: TLabel
    Left = 150
    Top = 139
    Width = 100
    Height = 14
    Alignment = taRightJustify
    Caption = 'Tipo documento'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label1: TLabel
    Left = 634
    Top = 286
    Width = 79
    Height = 15
    Alignment = taRightJustify
    Caption = 'Near registers'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object butConect: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Conectar'
    TabOrder = 0
    OnClick = butConectClick
  end
  object memoInfo: TMemo
    Left = 8
    Top = 39
    Width = 779
    Height = 89
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object butCreate: TButton
    Left = 97
    Top = 8
    Width = 104
    Height = 25
    Caption = 'Create tables'
    Enabled = False
    TabOrder = 2
    OnClick = butCreateClick
  end
  object memData: TMemo
    Left = 8
    Top = 168
    Width = 779
    Height = 97
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
  end
  object seTipo: TSpinEdit
    Left = 256
    Top = 134
    Width = 60
    Height = 28
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Verdana'
    Font.Style = []
    MaxValue = 0
    MinValue = 0
    ParentFont = False
    TabOrder = 4
    Value = 0
  end
  object butAddEmbedding: TButton
    Left = 8
    Top = 134
    Width = 120
    Height = 25
    Caption = 'Add Embedding'
    Enabled = False
    TabOrder = 5
    OnClick = butAddEmbeddingClick
  end
  object DBGridQy: TDBGrid
    Left = 0
    Top = 316
    Width = 795
    Height = 140
    Align = alBottom
    DataSource = DSQy
    TabOrder = 6
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = 'Segoe UI'
    TitleFont.Style = []
  end
  object butSearch: TButton
    Left = 8
    Top = 285
    Width = 75
    Height = 25
    Caption = 'Search'
    TabOrder = 7
    OnClick = butSearchClick
  end
  object edSearch: TEdit
    Left = 89
    Top = 286
    Width = 216
    Height = 23
    TabOrder = 8
  end
  object rgDistance: TRadioGroup
    Left = 311
    Top = 271
    Width = 251
    Height = 38
    Caption = 'C'#225'lculo distancia'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Cosine'
      'L2 (eucl'#237'dea)')
    TabOrder = 9
  end
  object seK: TSpinEdit
    Left = 719
    Top = 281
    Width = 68
    Height = 28
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    MaxValue = 10
    MinValue = 1
    ParentFont = False
    TabOrder = 10
    Value = 2
  end
  object DBMemContent: TDBMemo
    Left = 0
    Top = 456
    Width = 795
    Height = 106
    Align = alBottom
    DataField = 'content'
    DataSource = DSQy
    Font.Charset = ANSI_CHARSET
    Font.Color = clMaroon
    Font.Height = -13
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
    TabOrder = 11
    StyleElements = [seClient, seBorder]
  end
  object FDConnection: TFDConnection
    Params.Strings = (
      'Database=vectores.db'
      'Extensions=True'
      'DriverID=SQLite')
    LoginPrompt = False
    AfterConnect = FDConnectionAfterConnect
    Left = 72
    Top = 64
  end
  object FDGUIxWaitCursor: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 184
    Top = 64
  end
  object FDPhysSQLiteDriverLink: TFDPhysSQLiteDriverLink
    Left = 320
    Top = 64
  end
  object AiOpenAiEmbeddings: TAiOpenAiEmbeddings
    Model = 'text-embedding-3-small'
    Dimensions = 1536
    ApiKey = '@OPENAI_API_KEY'
    Url = 'https://api.openai.com/v1/'
    Left = 72
    Top = 200
  end
  object FDQy: TFDQuery
    Connection = FDConnection
    SQL.Strings = (
      'SELECT doc_id, content, distance FROM vec_docs')
    Left = 464
    Top = 200
  end
  object DSQy: TDataSource
    DataSet = FDQy
    Left = 536
    Top = 200
  end
end
