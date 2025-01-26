object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'XML Writer Example 1 Simple Write'
  ClientHeight = 511
  ClientWidth = 758
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
    Width = 752
    Height = 67
    Align = alTop
    TabOrder = 0
    object ExecuteButton: TButton
      AlignWithMargins = True
      Left = 6
      Top = 6
      Width = 75
      Height = 55
      Margins.Left = 5
      Margins.Top = 5
      Margins.Bottom = 5
      Action = ExecuteAction
      Align = alLeft
      TabOrder = 0
    end
    object ExitButton: TButton
      AlignWithMargins = True
      Left = 671
      Top = 6
      Width = 75
      Height = 55
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Action = ExitAction
      Align = alRight
      TabOrder = 1
    end
    object EditSource: TEdit
      Left = 111
      Top = 8
      Width = 328
      Height = 23
      TabOrder = 2
    end
    object ButtonDecode: TButton
      Left = 445
      Top = 7
      Width = 75
      Height = 25
      Caption = 'Decode'
      TabOrder = 3
      OnClick = ButtonDecodeClick
    end
    object EditDest: TEdit
      Left = 255
      Top = 37
      Width = 184
      Height = 23
      TabOrder = 4
    end
    object ButtonDate: TButton
      Left = 445
      Top = 36
      Width = 75
      Height = 25
      Caption = 'ISO8601 Date'
      TabOrder = 5
      OnClick = ButtonDateClick
    end
    object DateTimePicker: TDateTimePicker
      Left = 111
      Top = 37
      Width = 94
      Height = 23
      Date = 45216.000000000000000000
      Time = 0.657306574074027600
      TabOrder = 6
    end
    object CheckBoxDisplayText: TCheckBox
      Left = 537
      Top = 15
      Width = 118
      Height = 17
      Caption = 'View Display Text'
      TabOrder = 7
    end
  end
  object mXMLView: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 76
    Width = 752
    Height = 432
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
