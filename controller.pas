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

constructor TController.Create(Data: TDataTree; Observer : IObserver);
begin
     FTree := Data;
     FObserver := Observer;
     Refresh();
end;

end.

