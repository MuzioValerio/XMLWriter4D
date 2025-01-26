object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'XML Writer Example 2 Simple'
  ClientHeight = 442
  ClientWidth = 628
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object TopPanel: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 622
    Height = 40
    Align = alTop
    TabOrder = 0
    object ExecuteButton: TButton
      AlignWithMargins = True
      Left = 6
      Top = 6
      Width = 75
      Height = 28
      Margins.Left = 5
      Margins.Top = 5
      Margins.Bottom = 5
      Action = ExecuteAction
      Align = alLeft
      TabOrder = 0
    end
    object ExitButton: TButton
      AlignWithMargins = True
      Left = 541
      Top = 6
      Width = 75
      Height = 28
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = ExitAction
      Align = alRight
      TabOrder = 1
    end
    object CheckBoxDisplayText: TCheckBox
      Left = 416
      Top = 11
      Width = 118
      Height = 17
      Caption = 'View Display Text'
      TabOrder = 2
    end
  end
  object mXMLView: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 49
    Width = 622
    Height = 390
    Align = alClient
    Lines.Strings = (
      '')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
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
