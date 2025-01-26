{-----------------------------------------------------------------------------
   XMLWrite Lib
   Copyrigth (C) 2023 Muzio Valerio

   Unit Name: XML.Writer.Node.Attribute
   Author:    muzio
   Date:      17-ott-2023

   Info:
   Purpose:
   History:

-----------------------------------------------------------------------------}

unit XML.Writer.Node.Attribute;

interface

uses
  System.Classes, System.SysUtils;

type
  TXMLNodeAttribute = class(TObject)
  private
    FName: string;
    FValue: string;
    procedure SetName(const aValue: string);
    procedure SetValue(const aValue: string);
  public
    property Name: string read FName write SetName;
    property Value: string read FValue write SetValue;
  end;

implementation

uses
  System.StrUtils;

{XMLNodeElement}

procedure TXMLNodeAttribute.SetName(const aValue: string);
begin
  FName := Trim(aValue);
end;

procedure TXMLNodeAttribute.SetValue(const aValue: string);
begin
  FValue := Trim(aValue);
  FValue := ReplaceText(FValue, '"', '');
end;

end.
