object Form20: TForm20
  Left = 0
  Top = 0
  Caption = 'Form20'
  ClientHeight = 737
  ClientWidth = 1359
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object BtnAddNodoManual: TButton
    Left = 32
    Top = 32
    Width = 297
    Height = 25
    Caption = 'BtnAddNodoManual'
    TabOrder = 0
    OnClick = BtnAddNodoManualClick
  end
  object BtnBusquedaSemantica: TButton
    Left = 968
    Top = 40
    Width = 193
    Height = 25
    Caption = 'BtnBusquedaSemantica'
    TabOrder = 1
    OnClick = BtnBusquedaSemanticaClick
  end
  object BtnBusquedaEstructural: TButton
    Left = 968
    Top = 88
    Width = 193
    Height = 25
    Caption = 'BtnBusquedaEstructural'
    TabOrder = 2
    OnClick = BtnBusquedaEstructuralClick
  end
  object BtnBusquedaHibrida: TButton
    Left = 968
    Top = 136
    Width = 193
    Height = 25
    Caption = 'BtnBusquedaHibrida'
    TabOrder = 3
    OnClick = BtnBusquedaHibridaClick
  end
  object MemoJson: TMemo
    Left = 32
    Top = 152
    Width = 369
    Height = 545
    Lines.Strings = (
      '['
      '  {'
      '    "subject": {'
      '      "name": "Steve Jobs",'
      '      "nodeLabel": "PERSONA",'
      
        '      "text": "Steve Jobs fue un empresario y magnate de los neg' +
        'ocios del sector inform'#225'tico, cofundador de Apple.",'
      '      "properties": {'
      '        "nacimiento": 1955'
      '      }'
      '    },'
      '    "predicate": {'
      '      "edgeLabel": "FUND'#211'",'
      '      "name": "fue el fundador de",'
      '      "properties": {'
      '        "a'#241'o": 1976'
      '      }'
      '    },'
      '    "object": {'
      '      "name": "Apple Inc.",'
      '      "nodeLabel": "EMPRESA",'
      
        '      "text": "Apple Inc. es una empresa tecnol'#243'gica multinacion' +
        'al estadounidense que dise'#241'a y produce equipos electr'#243'nicos y so' +
        'ftware.",'
      '      "properties": {'
      '        "industria": "Tecnolog'#237'a"'
      '      }'
      '    }'
      '  }'
      ']')
    ScrollBars = ssBoth
    TabOrder = 4
    WordWrap = False
  end
  object BtnAddNodoFromJson: TButton
    Left = 32
    Top = 88
    Width = 297
    Height = 25
    Caption = 'BtnAddNodoFromJson'
    TabOrder = 5
    OnClick = BtnAddNodoFromJsonClick
  end
  object MemoContexto: TMemo
    Left = 448
    Top = 224
    Width = 833
    Height = 473
    Lines.Strings = (
      'MemoContexto')
    TabOrder = 6
  end
  object Button1: TButton
    Left = 504
    Top = 136
    Width = 217
    Height = 25
    Caption = 'Button1'
    TabOrder = 7
    OnClick = Button1Click
  end
  object RAG: TAiRagGraph
    Embeddings = AiOllamaEmbeddings1
    SearchOptions.BM25Weight = 0.700000000000000000
    SearchOptions.EmbeddingWeight = 0.300000000000000000
    Left = 400
    Top = 40
  end
  object AiOllamaEmbeddings1: TAiOllamaEmbeddings
    Model = 'mxbai-embed-large:latest'
    Dimensions = 1024
    ApiKey = '@OLLAMA_API_KEY'
    Url = 'http://localhost:11434/'
    Left = 488
    Top = 72
  end
  object GraphBuilder: TAiRagGraphBuilder
    Graph = RAG
    Left = 584
    Top = 40
  end
end
