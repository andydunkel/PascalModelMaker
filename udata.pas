unit uData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uConsts, fgl;

type
    IObserver = interface(IInterface)
      procedure Refresh();
    end;

type

    { TNodeData }

    TNodeData = class;  //forward declaration
    TDataTree = class; //forward declaration
    TChildList = specialize TFPGObjectList<TNodeData>;
    TNodeData = class
    protected
      FParent : TNodeData;
      FChildren : TChildList;
      FName : String;
      FDataType : String;
      FComment : String;
      FElementType : TElementType;
      FPersist : boolean;
      FList : boolean;
    public
      constructor Create();
    published
      property Name : String read FName write FName;
      property DataType : String read FDataType write FDataType;
      property Comment : String read FComment write FComment;
      property Parent : TNodeData read FParent write FParent;
      property ElementType : TElementType read FElementType write FElementType;
      property Children : TChildlist read FChildren;
      property Persist : boolean read FPersist write FPersist;
      property List : boolean read FList write FList;
end;


type

{ TDataTree }

 TDataTree = class
  protected
    FClasses : TChildList;
    FName : String;
    FFilePath : String;
    FSaved : boolean;
    FUnitName : String;
    FGenerateObserverCode : boolean;
    FPersistanceCode : boolean;
    FExportPath : String;
  public
    constructor Create();
  published
    property FilePath : String read FFilePath write FFilePath;
    property Saved : boolean read FSaved write FSaved;
    property Classes : TChildList read FClasses;
    property UnitName : String read FUnitName write FUnitName;
    property ExportPath : String read FExportPath write FExportPath;
    property GenerateObserverCode : boolean read FGenerateObserverCode write FGenerateObserverCode;
    property PersistanceCode : boolean read FPersistanceCode write FPersistanceCode;
end;

implementation

{ TNodeData }

constructor TNodeData.Create;
begin
  FChildren := TChildList.Create(true);
end;

{ TDataTree }


constructor TDataTree.Create;
begin
     Self.UnitName:= 'ModelUnit';
     Self.Saved:= false;
     Self.GenerateObserverCode:=true;
     Self.PersistanceCode:=false;
     FClasses:= TChildList.Create(true);
end;

end.

