{-----------------------------------------------------------------------------
   Writer Lib
   Copyrigth (C) 2023-2030 Muzio Valerio

   Unit Name: XML.Writer
   Author:    muzio
   Date:      17-ott-2023

   Info:
     Rappresenta l'oggetto TXMLWriter
   Purpose:
   History:

-----------------------------------------------------------------------------}

unit XML.Writer;

interface

uses
  System.Classes, System.SysUtils,
  XML.Writer.Types;

type
  TXMLWriter = class(TObject)
  strict private
    procedure NodeToStringList(var aXML: TStringList; aNode: TXMLNode; aReplaceChars: Boolean);
  private
    FAutoIndent: Boolean;
    FEncoding: string;
    FIncludeHeader: Boolean;
    FNodes: TXMLNodeList;
    FStrings: TSTringList;

    function GetEncodingStr: string;
    function GetIndent(const aLevel: integer): string;
    function GetText(const ReplaceEscapeChars: Boolean): string;

    function GetDisplayText: string;
    function GetXMLText: string;
    procedure SetAutoIndent(const aValue: Boolean);
    procedure SetEncoding(const aValue: string);
    procedure SetIncludeHeader(const aValue: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveToFile(const aFilename: string);
    procedure SaveToStream(Stream: TStream); overload;
    procedure SaveToStream(Stream: TStream; const ReplaceEscapeChars: Boolean); overload;

    property DisplayText: string read GetDisplayText;
    property Nodes: TXMLNodeList read FNodes write FNodes;
    property Text: string read GetXMLText;

    property AutoIndent: Boolean read FAutoIndent write SetAutoIndent;
    property IncludeHeader: Boolean read FIncludeHeader write SetIncludeHeader;
    property Encoding: string read FEncoding write SetEncoding;
  end;

implementation

uses
  System.StrUtils;

resourcestring
  sXmlVersionEncoding = '<?xml version="1.0" %s?>';

{ TXMLWriter }

constructor TXMLWriter.Create;
begin
  inherited Create;
  FStrings := TStringList.Create;
  FNodes := TXMLNodeList.Create(nil);
  FIncludeHeader := True;
  FAutoIndent := True;
  FEncoding := '';
end;

destructor TXMLWriter.Destroy;
begin
  FNodes.Free;
  FStrings.Free;
  inherited Destroy;
end;

function TXMLWriter.GetDisplayText: string;
begin
  Result := GetText(True);
end;

function TXMLWriter.GetEncodingStr: string;
begin
  Result := '';
  if FEncoding <> '' then
    Result := Format(' encoding="%s"', [FEncoding]);
end;

function TXMLWriter.GetIndent(const aLevel: integer): string;
begin
  if FIncludeHeader then
  begin
    Result := ' ';
    if FAutoIndent then
      Result := StringOfChar(' ', (aLevel+1)*2);
  end
  else
  begin
    Result := '';
    if FAutoIndent then
      Result := StringOfChar(' ', aLevel*2);
  end;
end;

function TXMLWriter.GetText(const ReplaceEscapeChars: Boolean): string;
begin
  FStrings.Clear;
  if FNodes.Count = 0 then Exit;

  if FIncludeHeader then
    FStrings.Add(Format(sXmlVersionEncoding, [GetEncodingStr]));

  for var lCount := 0 to FNodes.Count-1 do
    NodeToStringList(FStrings, FNodes.Node[lCount], ReplaceEscapeChars);

  Result := FStrings.Text;
end;

function TXMLWriter.GetXMLText: string;
begin
  Result := GetText(False);
end;

procedure TXMLWriter.NodeToStringList(var aXML: TStringList; aNode: TXMLNode; aReplaceChars: Boolean);
var
  lCount: integer;
  aValue: string;
begin
  if aNode.IsLeafNode then
  begin
    if aReplaceChars then
      aValue := aNode.DisplayString
    else
      aValue := aNode.RealString;
    aXML.Add(Format('%s%s%s%s', [GetIndent(aNode.Level), aNode.OpenTag, aValue, aNode.CloseTag]));
  end
  else
  begin
    aXML.Add(GetIndent(aNode.Level) + aNode.OpenTag);
    for lCount := 0 to aNode.Children.Count-1 do
      NodeToStringList(aXML, aNode.Children.Node[lCount], aReplaceChars);
    aXML.Add(GetIndent(aNode.Level) + aNode.CloseTag);
  end;
end;

procedure TXMLWriter.SetAutoIndent(const aValue: Boolean);
begin
  FAutoIndent := aValue;
end;

procedure TXMLWriter.SaveToFile(const aFilename: string);
begin
  var lStream := TMemoryStream.Create;
  try
    SaveToStream(lStream);
    lStream.SaveToFile(aFilename);
  finally
    lStream.Free;
  end;
end;

procedure TXMLWriter.SaveToStream(Stream: TStream);
begin
  GetText(False);
  FStrings.SaveToStream(Stream);
end;

procedure TXMLWriter.SaveToStream(Stream: TStream; const ReplaceEscapeChars: Boolean);
begin
  GetText(ReplaceEscapeChars);
  FStrings.SaveToStream(Stream);
end;

procedure TXMLWriter.SetEncoding(const aValue: string);
begin
  FEncoding := aValue;
end;

procedure TXMLWriter.SetIncludeHeader(const aValue: Boolean);
begin
  FIncludeHeader := aValue;
end;

end.
