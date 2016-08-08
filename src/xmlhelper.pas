unit XMLHelper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, laz2_DOM;

type

  { TXMLHelper }

TXMLHelper = class
 public
   class function CreateXmlNode(Doc: TXMLDocument; Name: String): TDOMNode;
   class function CreateXmlNode(Doc: TXMLDocument; Parent: TDOMNode; Name: String; Content: String): TDOMNode;
   class function GetXmlNode(Node: TDOMNode; NodeName: String): String;
end;


implementation

{ TXMLHelper }

class function TXMLHelper.CreateXmlNode(Doc: TXMLDocument; Name: String): TDOMNode;
var
   Node: TDOMNode;
begin
   Node:= Doc.CreateElement(Name);
   Result:= Node;
end;

class function TXMLHelper.CreateXmlNode(Doc: TXMLDocument; Parent: TDOMNode;
  Name: String; Content: String): TDOMNode;
var
   Node: TDOMNode;
begin
   Node:= CreateXmlNode(Doc, Name);
   Node.TextContent:= Content;
   Parent.AppendChild(Node);
   Result:= Node;
end;

class function TXMLHelper.GetXmlNode(Node: TDOMNode; NodeName: String): String;
var res: String;
begin
  res:= '';
  try
   if (Node.FindNode(NodeName).FirstChild <> nil) then
   begin
        res:= Node.FindNode(NodeName).FirstChild.NodeValue;
   end;
  except
    res:= '';
  end;
  Result:= res;
end;

end.

