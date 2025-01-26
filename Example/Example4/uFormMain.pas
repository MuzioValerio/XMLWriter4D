{-----------------------------------------------------------------------------
   Example4
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
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList, Vcl.StdCtrls, Vcl.ExtCtrls,
  XML.Writer.Types;

type
  TFormMain = class(TForm)
    TopPanel: TPanel;
    ExecuteButton: TButton;
    ExitButton: TButton;
    mXMLView: TMemo;
    ActionList: TActionList;
    ExecuteAction: TAction;
    ExitAction: TAction;
    LabelPath: TLabel;
    EditPath: TEdit;
    procedure ExecuteActionExecute(Sender: TObject);
    procedure ExitActionExecute(Sender: TObject);
  private
    procedure AddDeveloper(var aNodeList: TXMLNodeList);
    procedure AddProjects(var aNodeList: TXMLNodeList);
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  XML.Writer;

procedure TFormMain.AddDeveloper(var aNodeList: TXMLNodeList);
begin
  aNodeList.OpenNode('Developer').AddAttribute('Role', 'Senior Developer');
  aNodeList.AddLeafNode('FirstName First Attribute = "FirstName"').AsString('Muzio').AddAttribute('Second Attribute', 'Happy');
  aNodeList.AddLeafNode('LastName').AsString('Valerio');
  aNodeList.AddLeafNode('Informazioni').AsString('Qualità & attenzione hai dettagli');
  aNodeList.AddLeafNode('Age').AsInteger(60);
  aNodeList.AddLeafNode('Importo').AsFloat(8.90, 4);
  aNodeList.AddLeafNode('DataDocumento').AsDate(Date);
  aNodeList.AddLeafNode('TimeStamp').AsDateTime(Now);
  aNodeList.AddLeafNode('Time').AsTime(Now);
  var lNode := aNodeList;
  AddProjects(lNode);
  aNodeList.AddLeafNode('Vuoto');
  aNodeList.CloseNode;
end;

procedure TFormMain.AddProjects(var aNodeList: TXMLNodeList);
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

procedure TFormMain.ExecuteActionExecute(Sender: TObject);
var
  lStringStream: TStringStream;
begin
  var lXMLWriter := TXMLWriter.Create;
  lStringStream := TStringStream.Create;
  try
    lXMLWriter.AutoIndent := True;
    lXMLWriter.Encoding := 'UTF8';
    lXMLWriter.IncludeHeader := True;
    lXMLWriter.Nodes.Clear;
    var lNodes := lXMLWriter.Nodes;
    AddDeveloper(lNodes);
    if not String(EditPath.Text).IsEmpty then
      lXMLWriter.SaveToFile(EditPath.Text);
    lXMLWriter.SaveToStream(lStringStream);
    lStringStream.Position := 0;
    mXMLView.Lines.LoadFromStream(lStringStream);
  finally
    FreeAndNil(lStringstream);
    FreeAndNil(lXMLWriter);
  end;
end;

procedure TFormMain.ExitActionExecute(Sender: TObject);
begin
  Close;
end;

end.
