object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'FormMain Save To File Fattura XML'
  ClientHeight = 442
  ClientWidth = 837
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 15
  object TopPanel: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 831
    Height = 73
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 827
    object LabelPath: TLabel
      Left = 99
      Top = 12
      Width = 24
      Height = 15
      Caption = 'Path'
    end
    object ExecuteButton: TButton
      AlignWithMargins = True
      Left = 6
      Top = 6
      Width = 75
      Height = 61
      Margins.Left = 5
      Margins.Top = 5
      Margins.Bottom = 5
      Action = ExecuteAction
      Align = alLeft
      TabOrder = 0
    end
    object ExitButton: TButton
      AlignWithMargins = True
      Left = 750
      Top = 6
      Width = 75
      Height = 61
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = ExitAction
      Align = alRight
      TabOrder = 1
      ExplicitLeft = 746
    end
    object EditPath: TEdit
      Left = 129
      Top = 8
      Width = 352
      Height = 23
      TabOrder = 2
      Text = 'd:\temp\fattura.xml'
    end
    object cbIncludeHedaer: TCheckBox
      Left = 99
      Top = 45
      Width = 106
      Height = 17
      Caption = 'Include Header'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object EditEncoding: TEdit
      Left = 433
      Top = 42
      Width = 39
      Height = 23
      TabOrder = 5
      Text = 'UTF8'
    end
    object cbVersione: TComboBox
      Left = 483
      Top = 42
      Width = 82
      Height = 23
      ItemIndex = 0
      TabOrder = 6
      Text = 'FPR12'
      Items.Strings = (
        'FPR12'
        'FPA12')
    end
    object rgTextEncoder: TRadioGroup
      Left = 218
      Top = 32
      Width = 197
      Height = 34
      Caption = 'Export Text Encoced'
      Columns = 2
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Segoe UI'
      Font.Style = []
      ItemIndex = 0
      Items.Strings = (
        'Encoded'
        'Plain Text')
      ParentFont = False
      ShowFrame = False
      TabOrder = 4
    end
  end
  object mXMLView: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 82
    Width = 831
    Height = 357
    Align = alClient
    Lines.Strings = (
      '')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    ExplicitWidth = 827
    ExplicitHeight = 356
  end
  object ActionList: TActionList
    Left = 72
    Top = 114
    object ExecuteAction: TAction
      Caption = 'Execute'
      OnExecute = ExecuteActionExecute
    end
    object ExitAction: TAction
      Caption = 'Exit'
      OnExecute = ExitActionExecute
    end
  end
end
