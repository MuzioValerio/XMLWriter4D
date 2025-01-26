{-----------------------------------------------------------------------------
   XMLWriter Lib
   Copyrigth (C) 2023-2030 Muzio Valerio

   Unit Name: XML.Writer.Types
   Author:    muzio
   Date:      18-ott-2023

   Info:
     Contiene le ShortCut per gli oggetti
     - TXMLUtility
     - TXMLNodeList
     - TXMLNode
   Purpose:
   History:

-----------------------------------------------------------------------------}

unit XML.Writer.Types;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  XML.Writer.Node,
  XML.Utility;

type
  TXMLNodeList = XML.Writer.Node.TXMLNodeList;
  TXMLNode = XML.Writer.Node.TXMLNode;
  TXMLUtility = XML.Utility.TXMLUtility;

implementation

end.
