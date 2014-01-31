unit Controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uData, uConsts, XMLWrite, DOM, XMLHelper;

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
    end;


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

     RootNode:= TXMLHelper.CreateXmlNode(Doc, 'Content');
     Doc.AppendChild(RootNode);

     //Create meta information
     MetaNode:= TXMLHelper.CreateXmlNode(Doc, 'Meta');
     TDOMElement(MetaNode).SetAttribute('Version', '1');
     RootNode.AppendChild(MetaNode);

     //Save model
     ModelNode:= TXMLHelper.CreateXmlNode(Doc, 'model');
     TDomElement(ModelNode).SetAttribute('UnitName', FTree.UnitName);
     TDomElement(ModelNode).SetAttribute('GenerateObserverCode', BoolToStr(FTree.GenerateObserverCode));
     TDomElement(ModelNode).SetAttribute('PersistanceCode', BoolToStr(FTree.PersistanceCode));
     TDomElement(ModelNode).SetAttribute('UnitName', FTree.UnitName);
     TDomElement(ModelNode).SetAttribute('ExportPath', FTree.ExportPath);
     RootNode.AppendChild(ModelNode);

     ClassesNode:= TXMLHelper.CreateXmlNode(Doc, 'Classes');

     //Save elements
     for i:= 0 to FTree.Classes.Count - 1 do begin
         TempNodeClass:= TXMLHelper.CreateXmlNode(Doc, 'Class');
         TXMLHelper.CreateXmlNode(Doc, TempNodeClass, 'Name', FTree.Classes[i].Name);
         TXMLHelper.CreateXmlNode(Doc, TempNodeClass, 'Persist', BoolToStr(FTree.Classes[i].Persist));

         AttrNode:= TXMLHelper.CreateXmlNode(Doc, 'Attributes');
         //Save attributes
         for j:= 0 to FTree.Classes[i].Children.Count - 1 do begin;
             TempNodeAttr:= TXMLHelper.CreateXmlNode(Doc, 'Attr');
             TXMLHelper.CreateXmlNode(Doc, TempNodeAttr, 'Name', FTree.Classes[i].Children[j].Name);
             TXMLHelper.CreateXmlNode(Doc, TempNodeAttr, 'ElementType', IntToStr(Ord(FTree.Classes[i].Children[j].ElementType)));
             TXMLHelper.CreateXmlNode(Doc, TempNodeAttr, 'Persist', BoolToStr(FTree.Classes[i].Children[j].Persist));
             TXMLHelper.CreateXmlNode(Doc, TempNodeAttr, 'List', BoolToStr(FTree.Classes[i].Children[j].List));
             AttrNode.AppendChild(TempNodeAttr);
         end;

         ClassesNode.AppendChild(TempNodeClass);
     end;

     ModelNode.AppendChild(ClassesNode);

     RootNode:= Doc.DocumentElement;
     WriteXMLFile(Doc, FileName);
end;

procedure TController.LoadFile(FileName: String);
begin

end;

procedure TController.DeleteElement(Data: TNodeData);
var I : Integer                            ;
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

