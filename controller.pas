unit Controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uData, uConsts;

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
        procedure UpdateModelProperties(Name : String; ObserverCode : boolean; PersistanceCode : boolean);
        procedure UpdateClassProperties(NameOfClass : String; ClassPersist : boolean);
        procedure UpdateAttrProperties(AttrName : String; AttrType : String; AttrPersist : boolean; AttrList : boolean);
        constructor Create(Data : TDataTree; Observer : IObserver);
        property CurrentNode : TNodeData read FCurrentNode write FCurrentNode;
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

constructor TController.Create(Data: TDataTree; Observer : IObserver);
begin
     FTree := Data;
     FObserver := Observer;
     Refresh();
end;

end.

