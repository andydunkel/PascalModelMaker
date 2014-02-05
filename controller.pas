unit Controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uData, uConsts, XMLWrite, XMLRead, DOM, XMLHelper;

type

    { TController }

    TController = class
      private
        FTree : TDataTree;
        FObserver : IObserver;
        FCurrentNode : TNodeData;
      public
        procedure AddClass(Name : String);
        procedure AddAttribute(Node : TNodeData; Name : String);
        procedure Refresh();
        procedure UpdateModelProperties(Name: String; ObserverCode : boolean; PersistanceCode: boolean);
        procedure UpdateClassProperties(NameOfClass: String; ClassPersist : boolean);
        procedure UpdateAttrProperties(AttrName: String; AttrType: String; AttrPersist: boolean; AttrList: boolean);
        procedure SaveFile(FileName: String);
        procedure LoadFile(FileName: String);
        procedure DeleteElement(Data: TNodeData);
        constructor Create(Data: TDataTree; Observer : IObserver);
        property CurrentNode: TNodeData read FCurrentNode write FCurrentNode;
        property Tree: TDataTree read FTree write FTree;
    end;

const
  NODE_MODEL = 'model';
  ATTR_UNITNAME = 'UnitName';
  ATTR_GENERATEOBSERVERCODE='GenerateObserverCode';
  ATTR_PERSISTANCECODE = 'PersistanceCode';
  ATTR_EXPORTPATH = 'ExportPath';
  NODE_CONTENT = 'Content';
  NODE_META = 'Meta';
  ATTR_VERSION = 'Version';
  NODE_CLASSES = 'Classes';
  NODE_CLASS = 'Class';
  NODE_NAME = 'Name';
  NODE_PERSIST = 'Persist';
  NODE_ATTRIBUTES = 'Attributes';
  NODE_ATTR = 'Attr';
  NODE_ELEMENTTYPE = 'ElementType';
  NODE_LIST = 'List';

implementation

{ TController }

procedure TController.AddClass(Name: String);
var
   current : TNodeData;
begin
     current:= TNodeData.Create();
     current.ElementType:= _Class;
     current.Name:=Name;
     FTree.Classes.Add(current);
     Refresh;
end;

procedure TController.AddAttribute(Node : TNodeData; Name: String);
var
   Attr : TNodeData;
begin
     Attr:= TNodeData.Create();
     Attr.ElementType:= _Attribute;
     Attr.Name:= Name;

     Node.Children.Add(Attr);
     Refresh();
end;

procedure TController.Refresh;
begin
     if FObserver <> nil then begin
       FObserver.Refresh();
     end;
end;

procedure TController.UpdateModelProperties(Name: String;
  ObserverCode: boolean; PersistanceCode: boolean);
begin
     with FTree do begin
          UnitName:= Name;
          GenerateObserverCode:= ObserverCode;
          PersistanceCode:= PersistanceCode;
     end;
end;

procedure TController.UpdateClassProperties(NameOfClass: String; ClassPersist: boolean);
begin
     with FCurrentNode do begin
          Name:= NameOfClass;
          Persist:= ClassPersist;
     end;

     Refresh();
end;

procedure TController.UpdateAttrProperties(AttrName: String; AttrType: String;
  AttrPersist: boolean; AttrList: boolean);
begin
     with FCurrentNode do begin
          Name:= AttrName;
          DataType:= AttrType;
          Persist:= AttrPersist;
          List:= AttrList;
     end;
end;

procedure TController.SaveFile(FileName: String);
var
   Doc: TXMLDocument;
   RootNode, MetaNode, ModelNode, ClassesNode,
     AttrNode, TempNodeClass, TempNodeAttr: TDOMNode;
   i,j : integer;
begin
     Doc:= TXMLDocument.Create();

     RootNode:= TXMLHelper.CreateXmlNode(Doc, NODE_CONTENT);
     Doc.AppendChild(RootNode);

     //Create meta information
     MetaNode:= TXMLHelper.CreateXmlNode(Doc, NODE_META);
     TDOMElement(MetaNode).SetAttribute(ATTR_VERSION, '1');
     RootNode.AppendChild(MetaNode);

     //Save model
     ModelNode:= TXMLHelper.CreateXmlNode(Doc, NODE_MODEL);
     TDomElement(ModelNode).SetAttribute(ATTR_UNITNAME, FTree.UnitName);
     TDomElement(ModelNode).SetAttribute(ATTR_GENERATEOBSERVERCODE, BoolToStr(FTree.GenerateObserverCode));
     TDomElement(ModelNode).SetAttribute(ATTR_PERSISTANCECODE, BoolToStr(FTree.PersistanceCode));
     TDomElement(ModelNode).SetAttribute(ATTR_UNITNAME, FTree.UnitName);
     TDomElement(ModelNode).SetAttribute(ATTR_EXPORTPATH, FTree.ExportPath);
     RootNode.AppendChild(ModelNode);

     ClassesNode:= TXMLHelper.CreateXmlNode(Doc, NODE_CLASSES);

     //Save elements
     for i:= 0 to FTree.Classes.Count - 1 do begin
         TempNodeClass:= TXMLHelper.CreateXmlNode(Doc, NODE_CLASS);
         TXMLHelper.CreateXmlNode(Doc, TempNodeClass, NODE_NAME, FTree.Classes[i].Name);
         TXMLHelper.CreateXmlNode(Doc, TempNodeClass, NODE_PERSIST, BoolToStr(FTree.Classes[i].Persist));

         AttrNode:= TXMLHelper.CreateXmlNode(Doc, NODE_ATTRIBUTES);
         //Save attributes
         for j:= 0 to FTree.Classes[i].Children.Count - 1 do begin;
             TempNodeAttr:= TXMLHelper.CreateXmlNode(Doc, NODE_ATTR);
             TXMLHelper.CreateXmlNode(Doc, TempNodeAttr, NODE_NAME, FTree.Classes[i].Children[j].Name);
             TXMLHelper.CreateXmlNode(Doc, TempNodeAttr, NODE_ELEMENTTYPE, IntToStr(Ord(FTree.Classes[i].Children[j].ElementType)));
             TXMLHelper.CreateXmlNode(Doc, TempNodeAttr, NODE_PERSIST, BoolToStr(FTree.Classes[i].Children[j].Persist));
             TXMLHelper.CreateXmlNode(Doc, TempNodeAttr, NODE_LIST, BoolToStr(FTree.Classes[i].Children[j].List));
             AttrNode.AppendChild(TempNodeAttr);
         end;

         TempNodeClass.AppendChild(AttrNode);
         ClassesNode.AppendChild(TempNodeClass);
     end;

     ModelNode.AppendChild(ClassesNode);

     RootNode:= Doc.DocumentElement;
     WriteXMLFile(Doc, FileName);
end;

procedure TController.LoadFile(FileName: String);
var
   Doc: TXMLDocument;
   RootNode, MetaNode, ModelNode, ClassesNode,
     AttrNode, TempNodeClass, TempNodeAttr: TDOMNode;
   Clasz, Attr: TNodeData;
   i,j : integer;
   Value: String;
begin
     ReadXMLFile(Doc, FileName);
     ModelNode:= Doc.DocumentElement.FindNode(NODE_MODEL);

     //Clear existing model
     FTree:= TDataTree.Create();

     //read model data
     FTree.UnitName:= TDOMElement(ModelNode).GetAttribute(ATTR_UNITNAME);
     FTree.ExportPath:= TDOMElement(ModelNode).GetAttribute(ATTR_EXPORTPATH);
     FTree.PersistanceCode:= StrToBool(TDOMElement(ModelNode).GetAttribute(ATTR_PERSISTANCECODE));
     FTree.GenerateObserverCode:= StrToBool(TDOMElement(ModelNode).GetAttribute(ATTR_GENERATEOBSERVERCODE));

     //read classes
     ClassesNode:= ModelNode.FindNode(NODE_CLASSES);

     for i:= 0 to ClassesNode.ChildNodes.Count - 1 do begin
         TempNodeClass:= ClassesNode.ChildNodes[i];
         Clasz:= TNodeData.Create();
         Clasz.Name:= TempNodeClass.FindNode(NODE_NAME).NodeValue;
         Clasz.Persist:= StrToBool(TempNodeClass.FindNode(NODE_PERSIST).NodeValue);
         Ftree.Classes.Add(Clasz);
         //read attributes
         AttrNode:= TempNodeClass.FindNode(NODE_ATTRIBUTES);

         for j:= 0 to AttrNode.ChildNodes.Count - 1 do begin
             TempNodeAttr:= AttrNode.ChildNodes[j];
             Attr:= TNodeData.Create();
             Attr.Name:= TempNodeAttr.FindNode(NODE_NAME).NodeValue;
             Attr.ElementType:= TElementType(StrToInt(TempNodeAttr.FindNode(NODE_ELEMENTTYPE).NodeValue));
             Attr.Persist:= StrToBool(TempNodeAttr.FindNode(NODE_PERSIST).NodeValue);
             Attr.List:= StrToBool(TempNodeAttr.FindNode(NODE_List).NodeValue);
             Clasz.Children.Add(Attr);
         end;
     end;

end;

procedure TController.DeleteElement(Data: TNodeData);
var I : Integer;
begin
     if Data.ElementType = _Class then begin
        FTree.Classes.Remove(Data);
     end;

     Data.Parent:= nil;
     Data:= nil;
     Refresh();
end;

constructor TController.Create(Data: TDataTree; Observer : IObserver);
begin
     FTree := Data;
     FObserver := Observer;
     Refresh();
end;

end.

