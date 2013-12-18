unit uData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uConsts;

type
    TNodeData = class
    protected
      FParent : TNodeData;
      FName : String;
      FDataType : String;
      FComment : String;
      FElementType : TElementType;
    published
      property Name : String read FName write FName;
      property DataType : String read FDataType write FDataType;
      property Comment : String read FComment write FComment;
      property Parent : TNodeData read FParent write FParent;
      property ElementType : TElementType read FElementType write FElementType;
end;

type

{ TDataTree }

 TDataTree = class
  protected
    FRootNode : TNodeData;
    FName : String;
    FFilePath : String;
    FSaved : boolean;
  public
    procedure AddClass(Name : String);
    constructor Create();
  published
    property RootNode : TNodeData read FRootNode write FRootNode;
    property Name : String read FName write FName;
    property FilePath : String read FFilePath write FFilePath;
    property Saved : boolean read FSaved write FSaved;
end;

implementation

{ TDataTree }

procedure TDataTree.AddClass(Name: String);
begin

end;

constructor TDataTree.Create;
begin
     Self.RootNode := TNodeData.Create();
     Self.RootNode.Name := 'Model';
     Self.Name := 'Empty Model';
     Self.Saved := false;
end;

end.

