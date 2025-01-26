object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'XML Writer Example 3 Save To Stream'
  ClientHeight = 511
  ClientWidth = 639
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
    Width = 633
    Height = 40
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 629
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
      Left = 552
      Top = 6
      Width = 75
      Height = 28
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = ExitAction
      Align = alRight
      TabOrder = 1
      ExplicitLeft = 548
    end
  end
  object mXMLView: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 49
    Width = 633
    Height = 459
    Align = alClient
    Lines.Strings = (
      '')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    ExplicitWidth = 629
    ExplicitHeight = 458
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
