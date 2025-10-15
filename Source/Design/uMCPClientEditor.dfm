object FMCPClientEditor: TFMCPClientEditor
  Left = 0
  Top = 0
  ActiveControl = EditName
  Caption = 'MCP Client Editor'
  ClientHeight = 591
  ClientWidth = 571
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnResize = FormResize
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 571
    Height = 591
    Align = alClient
    TabOrder = 0
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 569
      Height = 97
      Align = alTop
      TabOrder = 0
      DesignSize = (
        569
        97)
      object Label1: TLabel
        Left = 297
        Top = 17
        Width = 78
        Height = 15
        Anchors = [akTop, akRight]
        Caption = 'Transport Type'
        FocusControl = EditProtocol
        ExplicitLeft = 216
      end
      object lblName: TLabel
        Left = 14
        Top = 16
        Width = 35
        Height = 15
        Caption = 'Name:'
        FocusControl = EditName
      end
      object EditProtocol: TComboBox
        Left = 388
        Top = 13
        Width = 145
        Height = 23
        Style = csDropDownList
        Anchors = [akTop, akRight]
        ItemIndex = 0
        TabOrder = 1
        Text = 'tpStdIo'
        Items.Strings = (
          'tpStdIo'
          'tpHttp'
          'tpSSE'
          'tpMakerAi')
      end
      object EditName: TEdit
        Left = 56
        Top = 13
        Width = 231
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = 'FileSystemClient'
      end
      object BtnTest: TButton
        Left = 19
        Top = 53
        Width = 94
        Height = 25
        Caption = 'Test'
        TabOrder = 2
        OnClick = BtnTestClick
      end
      object BtnSetDefaultParams: TButton
        Left = 136
        Top = 53
        Width = 139
        Height = 25
        Caption = 'Set Default Params'
        TabOrder = 3
        OnClick = BtnSetDefaultParamsClick
      end
    end
    object Panel3: TPanel
      Left = 1
      Top = 551
      Width = 569
      Height = 39
      Align = alBottom
      TabOrder = 1
      DesignSize = (
        569
        39)
      object BitBtn1: TBitBtn
        Left = 458
        Top = 6
        Width = 94
        Height = 25
        Anchors = [akTop, akRight]
        Kind = bkCancel
        NumGlyphs = 2
        TabOrder = 0
      end
      object BitBtn2: TBitBtn
        Left = 350
        Top = 6
        Width = 94
        Height = 25
        Anchors = [akTop, akRight]
        Kind = bkOK
        NumGlyphs = 2
        TabOrder = 1
      end
    end
    object Panel4: TPanel
      Left = 1
      Top = 98
      Width = 569
      Height = 453
      Align = alClient
      TabOrder = 2
      object PageControl1: TPageControl
        Left = 1
        Top = 28
        Width = 567
        Height = 424
        ActivePage = TabParams
        Align = alClient
        TabOrder = 0
        object TabParams: TTabSheet
          Caption = 'Params'
          object sgProperties: TStringGrid
            Left = 0
            Top = 0
            Width = 559
            Height = 394
            Align = alClient
            ColCount = 2
            RowCount = 10
            Options = [goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goFixedRowDefAlign]
            ScrollBars = ssVertical
            TabOrder = 0
            OnDrawCell = sgPropertiesDrawCell
          end
        end
        object TabLog: TTabSheet
          Caption = 'TabLog'
          ImageIndex = 1
          object MemoLog: TMemo
            Left = 0
            Top = 0
            Width = 559
            Height = 394
            Align = alClient
            ScrollBars = ssBoth
            TabOrder = 0
            WordWrap = False
          end
        end
      end
      object Panel5: TPanel
        Left = 1
        Top = 1
        Width = 567
        Height = 27
        Align = alTop
        TabOrder = 1
        object LblStatus: TLabel
          Left = 1
          Top = 1
          Width = 50
          Height = 15
          Align = alClient
          Caption = '   Status: '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
    end
  end
end
