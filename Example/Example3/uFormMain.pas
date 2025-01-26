{-----------------------------------------------------------------------------
   Example3
   Copyright (C) 2024 Muzio Valerio

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

procedure TFormMain.AddProjects(var aNodeList: TXMLNodeList);
begin
  aNodeList.OpenNode('Projects');
    aNodeList.AddLeafNode('Language').AsString('Delphi');
    aNodeList.OpenNode('ProjectNames');
      aNodeList.OpenNode('XMLWriter');
        aNodeList.AddLeafNode('Written').AsBoolean(True);
        aNodeList.AddLeafNode('Testetd').AsString('In Progress');
      aNodeList.CloseNode;
      aNodeList.OpenNode('XMLReader');
        aNodeList.AddLeafNode('Written').AsBoolean(True);
        aNodeList.AddLeafNode('Tested').AsBoolean(False);
      aNodeList.CloseNode;
    aNodeList.CloseNode;
  aNodeList.CloseNode;
end;

procedure TFormMain.ExecuteActionExecute(Sender: TObject);
var
  lStringStream: TStream;
begin
  var lXMLWriter := TXMLWriter.Create;
  lStringStream := TStringStream.Create;
  try
    lXMLWriter.AutoIndent := True;
    lXMLWriter.Nodes.Clear;
    var lNodes := lXMLWriter.Nodes;
    AddDeveloper(lNodes);
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
