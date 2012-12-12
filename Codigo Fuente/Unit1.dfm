object Form1: TForm1
  Left = 213
  Top = 169
  Width = 848
  Height = 650
  Caption = 'Buscador Cobol (by Logical) - New'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    840
    616)
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 4
    Top = 8
    Width = 549
    Height = 80
    TabOrder = 0
    object Label2: TLabel
      Left = 9
      Top = 12
      Width = 105
      Height = 13
      Caption = 'Carpeta de b'#250'squeda:'
    end
    object Label1: TLabel
      Left = 9
      Top = 45
      Width = 87
      Height = 13
      Caption = 'Patr'#243'n de archivo:'
    end
    object Label3: TLabel
      Left = 241
      Top = 45
      Width = 66
      Height = 13
      Caption = 'Ejemplo: '#39'HA*'#39
    end
    object edPath: TEdit
      Left = 117
      Top = 9
      Width = 284
      Height = 21
      TabOrder = 0
      Text = 'E:\COBOL\GLOKAL\SRC'
    end
    object ckbSubCarpetas: TCheckBox
      Left = 413
      Top = 11
      Width = 129
      Height = 17
      Caption = 'Procesar subcarpetas'
      TabOrder = 2
    end
    object ckbCBL: TCheckBox
      Left = 320
      Top = 45
      Width = 97
      Height = 17
      Caption = 'Extensi'#243'n (.CBL)'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object ckbCOB: TCheckBox
      Left = 430
      Top = 45
      Width = 97
      Height = 17
      Caption = 'Extensi'#243'n (.COB)'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object edPatron: TEdit
      Left = 117
      Top = 42
      Width = 121
      Height = 21
      TabOrder = 1
      Text = 'HA2C*'
      OnKeyPress = edPatronKeyPress
    end
  end
  object PageControl1: TPageControl
    Left = 3
    Top = 93
    Width = 835
    Height = 520
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Configurar b'#250'squeda'
      DesignSize = (
        827
        492)
      object Label4: TLabel
        Left = 5
        Top = 25
        Width = 99
        Height = 13
        Caption = 'Patr'#243'n de b'#250'squeda:'
      end
      object Memo1: TMemo
        Left = 5
        Top = 47
        Width = 815
        Height = 162
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'MOVE <any>(<isvar>:<integer>)'
          ''
          'MOVE <patron(WS-*)> TO')
        ParentFont = False
        TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Resultados de la b'#250'squeda'
      ImageIndex = 1
      DesignSize = (
        827
        492)
      object Panel3: TPanel
        Left = 0
        Top = 381
        Width = 826
        Height = 109
        Anchors = [akLeft, akRight, akBottom]
        BorderWidth = 2
        TabOrder = 0
        object lbInci: TListBox
          Left = 3
          Top = 26
          Width = 820
          Height = 80
          Align = alClient
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Courier New'
          Font.Style = []
          ItemHeight = 18
          ParentFont = False
          TabOrder = 0
        end
        object Panel4: TPanel
          Left = 3
          Top = 3
          Width = 820
          Height = 23
          Align = alTop
          Alignment = taLeftJustify
          BevelInner = bvLowered
          BevelOuter = bvNone
          Caption = 'Incidencias'
          TabOrder = 1
        end
      end
      object Panel1: TPanel
        Left = 0
        Top = 2
        Width = 827
        Height = 376
        Anchors = [akLeft, akTop, akRight, akBottom]
        BorderWidth = 2
        Caption = 'Panel1'
        TabOrder = 1
        object Splitter1: TSplitter
          Left = 232
          Top = 28
          Height = 345
        end
        object tvResult: TTreeView
          Left = 3
          Top = 28
          Width = 229
          Height = 345
          Align = alLeft
          Constraints.MinWidth = 20
          Indent = 19
          ReadOnly = True
          TabOrder = 0
          OnChange = tvResultChange
        end
        object paHeader: TPanel
          Left = 3
          Top = 3
          Width = 821
          Height = 25
          Align = alTop
          Alignment = taRightJustify
          BevelInner = bvLowered
          BevelOuter = bvNone
          TabOrder = 1
          object lHeader: TLabel
            Left = 1
            Top = 1
            Width = 34
            Height = 13
            Align = alLeft
            Caption = 'Prueba'
            Layout = tlCenter
          end
        end
        object Panel5: TPanel
          Left = 235
          Top = 28
          Width = 589
          Height = 345
          Align = alClient
          BevelOuter = bvLowered
          BorderWidth = 2
          Constraints.MinWidth = 20
          TabOrder = 2
          object WebBrowser1: TWebBrowser
            Left = 3
            Top = 3
            Width = 583
            Height = 339
            Align = alClient
            TabOrder = 0
            ControlData = {
              4C000000413C0000092300000000000000000000000000000000000000000000
              000000004C000000000000000000000001000000E0D057007335CF11AE690800
              2B2E126208000000000000004C0000000114020000000000C000000000000046
              8000000000000000000000000000000000000000000000000000000000000000
              00000000000000000100000000000000000000000000000000000000}
          end
        end
      end
    end
  end
  object Panel6: TPanel
    Left = 554
    Top = 8
    Width = 283
    Height = 80
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    object lMsg: TLabel
      Left = 121
      Top = 29
      Width = 96
      Height = 18
      Caption = 'Analizando...'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = [fsBold, fsItalic]
      ParentFont = False
      Visible = False
    end
    object btnMostrarTodos: TButton
      Left = 8
      Top = 11
      Width = 100
      Height = 25
      Caption = 'Mostrar todos'
      TabOrder = 0
      OnClick = btnMostrarTodosClick
    end
    object btnBuscarPatron: TButton
      Left = 8
      Top = 43
      Width = 100
      Height = 25
      Caption = 'Buscar Patr'#243'n'
      TabOrder = 1
      OnClick = btnBuscarPatronClick
    end
  end
end
