unit XMLHelper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM;

type

  { TXMLHelper }

TXMLHelper = class
     public
           class function CreateXmlNode(Doc: TXMLDocument; Name: String): TDOMNode;
           class function CreateXmlNode(Doc: TXMLDocument; Parent: TDOMNode; Name: String; Content: String): TDOMNode;
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

end.

