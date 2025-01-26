{-----------------------------------------------------------------------------
   XMLWriter Lib
   Copyrigth (C) 2023-2030 Muzio Valerio

   Unit Name: XML.Writer.Node
   Author:    muzio
   Date:      17-ott-2023

   Info:
   Purpose:
   History:

-----------------------------------------------------------------------------}

unit XML.Writer.Node;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections;

type
  TXMLNode = class;
  TXMLNodeList = class;

  TXMLNodeEvent = procedure(Sender: TObject; aNode: TXMLNode) of object;

  TXMLNode = class(TPersistent)
  private
    FChildren: TXMLNodeList;
    FAttributes: TDictionary<integer, TPair<string, string>>;
    FName: string;
    FParent: TXMLNode;
    FValue: string;

    function GetIsLeafNode: Boolean;
    function GetLevel: integer;
    procedure ExtractAttributeFromName(const aValue: string);

    procedure SetName(const aValue: string); virtual;
  public
    constructor Create(aParentNode: TXMLNode); virtual;
    destructor Destroy; override;
    procedure EnumerateNodes(aCallback: TXMLNodeEvent);

    function CloseTag: string;
    function OpenTag: string;

    function DisplayString: string;
    function RealString: string;

    function AsString(const aValue: string): TXMLNode;
    function AsBoolean(const aValue: Boolean): TXMLNode;
    function AsFloat(const aValue: Extended; const aDigits: Integer = 2): TXMLNode;
    function AsInteger(const aValue: Integer): TXMLNode;
    function AsDate(const aValue: TDateTime): TXMLNode;
    function AsDateTime(const aValue: TDateTime): TXMLNode;
    function AsTime(const aValue: TDateTime): TXMLNode;

    function AddAttribute(const aName, aValue: string): TXMLNode;

    property Children: TXMLNodeList read FChildren;
    property IsLeafNode: Boolean read GetIsLeafNode;
    property Level: Integer read GetLevel;
    property Name: string read FName write SetName;
    property Parent: TXMLNode read FParent;
  end;

  TXMLNodeList = class(TObject)
  private
    FCurrentNode: TXMLNode;
    FList: TList;
    function GetCount: Integer;
    function GetNode(Index: Integer): TXMLNode;
    procedure SetNode(Index: Integer; const aValue: TXMLNode);
    function GetNodeByName(aName: string): TXMLNode;
    procedure SetNodeByName(aName: string; aNode: TXMLNode);
    function GetRoot: TXMLNode;
    procedure AddNode(aNode: TXMLNode);
  public
    constructor Create(const aParent: TXMLNode);
    destructor Destroy; override;

    function AddLeafNode(const aName: string): TXMLNode;
    function OpenNode(const aName: string): TXMLNode;
    procedure CloseNode;
    procedure Clear;

    property Count: Integer read GetCount;
    property CurrentNode: TXMLNode read FCurrentNode write FCurrentNode;
    property Node[Index: Integer]: TXMLNode read GetNode write SetNode; default;
    property NodeByName[aName: string]: TXMLNode read GetNodeByName write SetNodeByName;
    property Root: TXMLNode read GetRoot;
  end;


implementation

uses
  System.StrUtils,
  XML.Utility;

{ TXMLNode }

constructor TXMLNode.Create(aParentNode: TXMLNode);
begin
  inherited Create;
  FChildren := TXMLNodeList.Create(Self);
  FAttributes := TDictionary<Integer, TPair<string, string>>.Create;
  FAttributes.TrimExcess;
  FParent := aParentNode;
end;

destructor TXMLNode.Destroy;
begin
  FChildren.Free;
  if Assigned(FAttributes) then
    FreeAndNil(FAttributes);

  inherited Destroy;
end;

function TXMLNode.DisplayString: string;
begin
  Result := FValue;
  Result := TXMLUtility.SetXMLToDisplayText(Result, 0);
end;

function TXMLNode.AddAttribute(const aName, aValue: string): TXMLNode;
var
  lPair: TPair<string, string>;
begin
  var lCount := FAttributes.Count;
  Inc(lCount);
  lPair.Create(aName, aValue);
  FAttributes.AddOrSetValue(lCount, lPair);
  Result := self;
end;

function TXMLNode.AsBoolean(const aValue: Boolean): TXMLNode;
begin
  FValue := BoolToStr(aValue, True);
  Result := self;
end;

function TXMLNode.AsFloat(const aValue: Extended; const aDigits: Integer = 2): TXMLNode;
begin
  FValue :=  TXMLUtility.safeFloatToStr(aValue, aDigits);
  Result := self;
end;

function TXMLNode.AsInteger(const aValue: Integer): TXMLNode;
begin
  FValue := IntToStr(aValue);
  Result := self;
end;

function TXMLNode.AsString(const aValue: string): TXMLNode;
begin
  FValue := aValue;
  FValue := TXMLUtility.RemoveXMLIllegalChar(FValue);
  Result := Self;
end;

function TXMLNode.AsDate(const aValue: TDateTime): TXMLNode;
begin
  FValue := TXMLUtility.SetISO8601DateTo(aValue);
  Result := self;
end;

function TXMLNode.AsDateTime(const aValue: TDateTime): TXMLNode;
begin
  FValue := TXMLUtility.SetISO8601DateTo(aValue, True);
  Result := Self;
end;

function TXMLNode.AsTime(const aValue: TDateTime): TXMLNode;
begin
  FValue := TXMLUtility.SetISO8601TimeTo(aValue);
  Result := Self;
end;

function TXMLNode.CloseTag: string;
begin
  Result := Format('</%s>', [FName]);
end;

function TXMLNode.OpenTag: string;
var
  lKeys: TArray<Integer>;
begin
  if FAttributes.Count = 0 then
    Result := Format('<%s>',[Name])
  else
  begin
    Result := '<' + Name;
    lKeys := FAttributes.Keys.ToArray;
    TArray.Sort<Integer>(lKeys);
    // Modified by muzio_dell Date 26-gen-2025
    for var lKey in lKeys do
      Result := Format('%s %s = "%s"', [Result, FAttributes.Items[lKey].Key, FAttributes.Items[lKey].Value]);
    Result := Result + '>';
  end;
end;

function TXMLNode.RealString: string;
begin
  Result := FValue;
end;

procedure TXMLNode.EnumerateNodes(aCallback: TXMLNodeEvent);
begin
  for var lCount := 0 to FChildren.Count-1 do
  begin
    if Assigned(aCallback) then
      aCallback(Self, FChildren[lCount]);
  end;
end;

procedure TXMLNode.ExtractAttributeFromName(const aValue: string);
begin
  var lName := Copy(aValue, 0, Pos('=', aValue)-1);
  var lValue := Copy(aValue, Pos('"', aValue), aValue.Length);
  lValue := lValue.Replace('"', '');
  AddAttribute(lName, lValue);
end;

function TXMLNode.GetLevel: integer;
var
  AParent: TXMLNode;
begin
  AParent := Parent;
  Result := 0;
  while AParent <> nil do
  begin
    AParent := AParent.Parent;
    Inc(Result);
  end;
end;

function TXMLNode.GetIsLeafNode: Boolean;
begin
  Result := FChildren.Count = 0;
end;

procedure TXMLNode.SetName(const aValue: string);
begin
  FName := aValue;
  if FName.IsNullOrWhiteSpace(FName) then
    Exit;

  if FName[1] = '<' then
    Delete(FName, 1, 1);
  if FName[FName.Length] = '>' then
    Delete(FName, FName.Length, 1);
  FName.Trim;

  // extract attribute if one exists...
  if Pos('=', FName) > 0 then
  begin
    var lAttribute := Copy(FName, Pos(' ', FName), FName.Length);
    ExtractAttributeFromName(lAttribute);
    FName := Copy(FName, 1, Pos(' ', FName)-1);
  end;
end;

{ TXMLNodeList }

constructor TXMLNodeList.Create(const aParent: TXMLNode);
begin
  inherited Create;
  FList := TList.Create;
  FCurrentNode := aParent;
end;

destructor TXMLNodeList.Destroy;
begin
  for var lCount := Count-1 downto 0 do
    Node[lCount].Free;
  FList.Free;
  inherited Destroy;
end;

function TXMLNodeList.AddLeafNode(const aName: string): TXMLNode;
begin
  Result := OpenNode(aName);
  CloseNode;
end;

function TXMLNodeList.OpenNode(const aName: string): TXMLNode;
begin
  Result := TXMLNode.Create(FCurrentNode);
  Result.Name := aName;
  if FCurrentNode = nil then
    AddNode(Result)
  else
    FCurrentNode.Children.AddNode(Result);

  FCurrentNode := Result;
end;

procedure TXMLNodeList.CloseNode;
begin
  FCurrentNode := FCurrentNode.Parent;
end;

procedure TXMLNodeList.Clear;
begin
  for var lCount := 0 to Pred(FList.Count) do
    FreeAndNil(Node[lCount]);

  FList.Clear;
  FCurrentNode := nil;
end;

function TXMLNodeList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TXMLNodeList.GetNode(Index: Integer): TXMLNode;
begin
  Result := TXMLNode(FList[Index]);
end;

function TXMLNodeList.GetNodeByName(aName: string): TXMLNode;
begin
  Result := nil;
  for var lCount := 0 to Count-1 do
  begin
    if Node[lCount].Name = aName then
    begin
      Result := Node[lCount];
      Exit;
    end;
  end;
end;

procedure TXMLNodeList.SetNodeByName(aName: string; aNode: TXMLNode);
begin
  for var lCount := 0 to Count-1 do
  begin
    if Node[lCount].Name = aName then
    begin
      Node[lCount] := aNode;
      Exit;
    end;
  end;
end;

function TXMLNodeList.GetRoot: TXMLNode;
begin
  Result := nil;
  if Count > 0 then Result := Node[0];
end;

procedure TXMLNodeList.AddNode(aNode: TXMLNode);
begin
  FList.Add(aNode);
end;

procedure TXMLNodeList.SetNode(Index: Integer; const aValue: TXMLNode);
begin
  FList[Index] := aValue;
end;

end.
