{-----------------------------------------------------------------------------
   Example1
   Copyright (C) 2024-2030 Muzio Valerio

   Unit Name: uFormMain
   Author:    muzio
   Date:      11-feb-2024

   Info:
   Purpose:
   History:

-----------------------------------------------------------------------------}

unit uFormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls;

type
  TFormMain = class(TForm)
    TopPanel: TPanel;
    mXMLView: TMemo;
    ExecuteButton: TButton;
    ExitButton: TButton;
    ActionList: TActionList;
    ExecuteAction: TAction;
    ExitAction: TAction;
    EditSource: TEdit;
    ButtonDecode: TButton;
    EditDest: TEdit;
    ButtonDate: TButton;
    DateTimePicker: TDateTimePicker;
    CheckBoxDisplayText: TCheckBox;
    procedure ButtonDateClick(Sender: TObject);
    procedure ButtonDecodeClick(Sender: TObject);
    procedure ExecuteActionExecute(Sender: TObject);
    procedure ExitActionExecute(Sender: TObject);
  private
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  XML.Writer.Types,
  XML.Writer;

procedure TFormMain.ButtonDateClick(Sender: TObject);
begin
  EditDest.Text := TXMLUtility.SetISO8601DateTo(DateTimePicker.Date, True);
end;

procedure TFormMain.ButtonDecodeClick(Sender: TObject);
var
  lStringConvert: string;
begin
  lStringConvert := EditSource.Text;
  lStringConvert := TXMLUtility.RemoveXMLIllegalChar(lStringConvert);
  EditDest.Text := TXMLUtility.SetXMLToDisplayText(lStringConvert, 0);
end;

procedure TFormMain.ExecuteActionExecute(Sender: TObject);
begin
  var lXMLWriter := TXMLWriter.Create;
  try
    lXMLWriter.AutoIndent := True;
    lXMLWriter.Nodes.Clear;
    lXMLWriter.Nodes.OpenNode('Developer Role="Senior Developer"');
      lXMLWriter.Nodes.AddLeafNode('FirstName').AsString('Muzio');
      lXMLWriter.Nodes.AddLeafNode('LastName').AsString('Valerio');
      lXMLWriter.Nodes.AddLeafNode('Informazioni').AsString('Qualità & attenzione hai dettagli');
      lXMLWriter.Nodes.AddLeafNode('Age').AsInteger(62);
      lXMLWriter.Nodes.AddLeafNode('Importo').AsFloat(8.90, 4);
      lXMLWriter.Nodes.AddLeafNode('DataDocumento').AsString(TXMLUtility.SetISO8601DateTo(Date));
      lXMLWriter.Nodes.AddLeafNode('DataDocumento').AsDate(Date);
      lXMLWriter.Nodes.OpenNode('Projects');
        lXMLWriter.Nodes.AddLeafNode('Language').AsString('Delphi');
        lXMLWriter.Nodes.OpenNode('ProjectNames');
          lXMLWriter.Nodes.OpenNode('XMLWriter');
            lXMLWriter.Nodes.AddLeafNode('Written').AsBoolean(True);
            lXMLWriter.Nodes.AddLeafNode('Testetd').AsBoolean(True);
          lXMLWriter.Nodes.CloseNode;
          lXMLWriter.Nodes.OpenNode('XMLReader');
            lXMLWriter.Nodes.AddLeafNode('Written').AsBoolean(True);
            lXMLWriter.Nodes.AddLeafNode('Tested').AsBoolean(True);
          lXMLWriter.Nodes.CloseNode;
        lXMLWriter.Nodes.CloseNode;
      lXMLWriter.Nodes.CloseNode;
    lXMLWriter.Nodes.CloseNode;
    if CheckBoxDisplayText.Checked then
      mXMLView.Lines.Text := lXMLWriter.DisplayText
    else
      mXMLView.Lines.Text := lXMLWriter.Text;
  finally
    FreeandNil(lXMLWriter);
  end;
end;

procedure TFormMain.ExitActionExecute(Sender: TObject);
begin
  Close;
end;

end.
