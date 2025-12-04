object Form14: TForm14
  Left = 0
  Top = 0
  Caption = 'Form14'
  ClientHeight = 443
  ClientWidth = 902
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Label1: TLabel
    Left = 64
    Top = 51
    Width = 123
    Height = 15
    Caption = 'Text from Embedding 1'
  end
  object Label2: TLabel
    Left = 488
    Top = 51
    Width = 106
    Height = 15
    Caption = 'Vector Embedding 1'
  end
  object Label3: TLabel
    Left = 64
    Top = 207
    Width = 123
    Height = 15
    Caption = 'Text from Embedding 2'
  end
  object Label4: TLabel
    Left = 488
    Top = 207
    Width = 106
    Height = 15
    Caption = 'Vector Embedding 1'
  end
  object Memo1: TMemo
    Left = 64
    Top = 72
    Width = 377
    Height = 133
    TabOrder = 0
  end
  object Memo2: TMemo
    Left = 488
    Top = 72
    Width = 377
    Height = 133
    TabOrder = 1
  end
  object Memo3: TMemo
    Left = 64
    Top = 224
    Width = 377
    Height = 133
    TabOrder = 2
  end
  object Memo4: TMemo
    Left = 488
    Top = 224
    Width = 377
    Height = 133
    TabOrder = 3
  end
  object Button2: TButton
    Left = 64
    Top = 376
    Width = 265
    Height = 25
    Caption = 'Compare Embeddings'
    TabOrder = 4
    OnClick = Button2Click
  end
  object AiOllamaEmbeddings1: TAiOllamaEmbeddings
    Model = 'mxbai-embed-large:latest'
    Dimensions = 1024
    ApiKey = '@OLLAMA_API_KEY'
    Url = 'http://localhost:11434/'
    Left = 448
    Top = 312
  end
end
