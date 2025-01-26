{-----------------------------------------------------------------------------
   Example2
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
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.Actions, Vcl.ActnList,
  XML.Writer.Types;

type
  TForm1 = class(TForm)
    TopPanel: TPanel;
    ExecuteButton: TButton;
    ExitButton: TButton;
    CheckBoxDisplayText: TCheckBox;
    mXMLView: TMemo;
    ActionList: TActionList;
    ExecuteAction: TAction;
    ExitAction: TAction;
    procedure ExecuteActionExecute(Sender: TObject);
    procedure ExitActionExecute(Sender: TObject);
  private
    procedure AddDeveloper(var aNodeList: TXMLNodeList);
    procedure AddProjects(var aNodeList: TXMLNodeList);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  XML.Writer;

procedure TForm1.AddDeveloper(var aNodeList: TXMLNodeList);
begin
  aNodeList.OpenNode('Developer Role="Senior Developer"');
  aNodeList.AddLeafNode('FirstName').AsString('Muzio');
  aNodeList.AddLeafNode('LastName').AsString('Valerio');
  aNodeList.AddLeafNode('Informazioni').AsString('Qualità & attenzione hai dettagli');
  aNodeList.AddLeafNode('Age').AsInteger(60);
  aNodeList.AddLeafNode('Importo').AsFloat(8.90, 4);
  aNodeList.AddLeafNode('DataDocumento').AsDate(Date);
  aNodeList.AddLeafNode('TimeStamp').AsDateTime(Now);
  aNodeList.AddLeafNode('Time').AsTime(Now);
  var lNode := aNodeList;
  AddProjects(lNode);
  aNodeList.CloseNode;
end;

procedure TForm1.AddProjects(var aNodeList: TXMLNodeList);
begin
  aNodeList.OpenNode('Projects');
    aNodeList.AddLeafNode('Language').AsString('Delphi');
    aNodeList.OpenNode('ProjectNames');
      aNodeList.OpenNode('XMLWriter');
        aNodeList.AddLeafNode('Written').AsBoolean(True);
        aNodeList.AddLeafNode('Testetd').AsBoolean(True);
      aNodeList.CloseNode;
      aNodeList.OpenNode('XMLReader');
        aNodeList.AddLeafNode('Written').AsBoolean(True);
        aNodeList.AddLeafNode('Tested').AsBoolean(True);
      aNodeList.CloseNode;
    aNodeList.CloseNode;
  aNodeList.CloseNode;
end;

procedure TForm1.ExecuteActionExecute(Sender: TObject);
begin
  var lXMLWriter := TXMLWriter.Create;
  try
    lXMLWriter.AutoIndent := True;
    lXMLWriter.Nodes.Clear;
    var lNodes := lXMLWriter.Nodes;
    AddDeveloper(lNodes);
    if CheckBoxDisplayText.Checked then
      mXMLView.Lines.Text := lXMLWriter.DisplayText
    else
      mXMLView.Lines.Text := lXMLWriter.Text;
  finally
    FreeAndNil(lXMLWriter);
  end;
end;

procedure TForm1.ExitActionExecute(Sender: TObject);
begin
  Close;
end;

end.
