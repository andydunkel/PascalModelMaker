unit Controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uData, uConsts, XMLWrite, DOM;

type

    { TController }

    TController = class
      private
        FTree : TDataTree;
        FObserver : IObserver;
        FCurrentNode : TNodeData;
      public
        procedure AddClass(Name : String);
        procedure Refresh();
        procedure UpdateModelProperties(Name: String; ObserverCode : boolean; PersistanceCode: boolean);
        procedure UpdateClassProperties(NameOfClass: String; ClassPersist : boolean);
        procedure UpdateAttrProperties(AttrName: String; AttrType: String; AttrPersist: boolean; AttrList: boolean);
        procedure SaveFile(FileName: String);
        procedure LoadFile(FileName: String);
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
   RootNode, MetaNode, ModelNode: TDOMNode;
begin
     Doc:= TXMLDocument.Create();

     RootNode:= Doc.CreateElement('Content');
     Doc.AppendChild(RootNode);

     //Create meta information
     MetaNode:= Doc.CreateElement('Meta');
     TDOMElement(MetaNode).SetAttribute('Version', '1');
     RootNode.AppendChild(MetaNode);

     //Save model
     ModelNode:= Doc.CreateElement('model');
     TDomElement(ModelNode).SetAttribute('UnitName', FTree.UnitName);
     TDomElement(ModelNode).SetAttribute('GenerateObserverCode', BoolToStr(FTree.GenerateObserverCode));
     TDomElement(ModelNode).SetAttribute('PersistanceCode', BoolToStr(FTree.PersistanceCode));
     TDomElement(ModelNode).SetAttribute('UnitName', FTree.UnitName);

     //Save elements

     RootNode:= Doc.DocumentElement;

     WriteXMLFile(Doc, FileName);
end;

procedure TController.LoadFile(FileName: String);
begin

end;

constructor TController.Create(Data: TDataTree; Observer : IObserver);
begin
     FTree := Data;
     FObserver := Observer;
     Refresh();
end;

end.

