{-----------------------------------------------------------------------------
   XMLWrite Lib
   Copyrigth (C) 2023 Muzio Valerio

   Unit Name: XML.Utility
   Author:    muzio
   Date:      17-ott-2023

   Info:
     Rappresenta l'oggetto TXMLUtility
   Purpose:
   History:

-----------------------------------------------------------------------------}

unit XML.Utility;

interface

uses
  System.Classes, System.SysUtils;

type
  TXMLUtility = class(TObject)
  public
    class function safeStrFloatToStr(const aValue: string): string; overload; static;
    class function safeFloatToStr(const aValue: Extended; const aDigits: Integer): string; overload; static;
    class function RemoveXMLIllegalChar(const aValue: string): string; static;
    class function SetXMLToDisplayText(var aValue: string; const aTagPos: Integer): string; static;
    class function SetISO8601DateTo(const aDate: TDateTime; const UseUTC: Boolean = False): string; static;
    class function SetISO8601TimeTo(const aTime: TTime): string; static;
  end;


implementation

uses
  System.StrUtils, System.DateUtils;


class function TXMLUtility.safeStrFloatToStr(const aValue: string): string;
const
  lDefRes = '0.00';
begin
  var lTmpRes := aValue.Trim;
  lTmpRes := ReplaceText(lTmpRes, '%', '');
  lTmpRes := ReplaceText(lTmpRes, '€', '');
  lTmpRes := REplaceText(lTmpRes, '.', '');
  lTmpRes := ReplaceText(lTmpRes, ',', '.');
  if lTmpRes <> '' then
    Result := lTmpRes
  else
    Result := lDefRes;
end;

class function TXMLUtility.safeFloatToStr(const aValue: Extended; const aDigits: Integer): string;
begin
  if aValue = 0.00 then
    Exit('0.00');

  var lStrFloat := FloatTostrF(aValue, ffFixed, 18, aDigits);
  Result := safeStrFloatToStr(lStrFloat);
end;

class function TXMLUtility.RemoveXMLIllegalChar(const aValue: string): string;
begin
  var lTmpStr := aValue.Trim;
  if lTmpStr = '' then
    Exit(lTmpStr);

  for var I := 1 to lTmpStr.Length do
  begin
    var C: Char := lTmpStr[I];
    var O: Byte := Ord(C);
    case O of
      0..31: Result := Result + ' ';
      32, 46: Result := Result + lTmpStr[I];
      48..57: Result := Result + lTmpStr[I];
      65..90: Result := Result + lTmpStr[I];
      97..122: Result := Result + lTmpStr[I];
    else
      Result := Result + '&#' + O.ToString + ';';
    end;
  end;
end;

class function TXMLUtility.SetISO8601DateTo(const aDate: TDateTime; const UseUTC: Boolean = False): string;
begin
  Result := DateToISO8601(aDate, False);
  if not UseUTC then
  begin
    var lPos := Result.IndexOf('T');
    if lPos > 0 then
      Result := Result.Substring(0, lPos);
  end
  else
  begin
    var lPos := Result.IndexOf('+');
    if lPos > 0 then
      Result := Result.Substring(0, lPos-4);
  end;
end;

class function TXMLUtility.SetISO8601TimeTo(const aTime: TTime): string;
begin
  Result := TimeToStr(aTime);
end;

class function TXMLUtility.SetXMLToDisplayText(var aValue: string; const aTagPos: Integer): string;
var
  lStartIdx: Integer;
  lEndIdx: Integer;
  lNewTagPos: Integer;
begin
  var lTmpStr := aValue.Trim;
  if (lTmpStr = '') or (not lTmpStr.Contains('&#')) then
    Exit(lTmpStr);
  try
    lStartIdx := lTmpStr.IndexOf('&#', aTagPos);
    lEndIdx :=  lTmpStr.IndexOf(';', lStartIdx+1);
    var lSubStr := lTmpStr.Substring(lStartIdx, (lEndIdx-lStartIdx)+1);
    lNewTagPos := lStartIdx+1;
    var lOrdValue := lSubStr.Substring(2, lSubStr.Length-3);
    var lCharValue := Char(Ord(StrToInt(lOrdValue)));
    Delete(lTmpStr, lStartIdx+1, (lEndIdx+1)-lStartIdx);
    Insert(lCharValue, lTmpStr, lStartIdx+1);
    Result := SetXMLToDisplayText(lTmpStr, lNewTagPos);
  except
    Exit(lTmpStr);
  end;
end;

end.
