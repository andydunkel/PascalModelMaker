unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynHighlighterPas, SynEdit, Forms, Controls,
  Graphics, Dialogs, ComCtrls, ExtCtrls, StdCtrls, Menus, ActnList, uData,
  Controller;

type

  { TFormMain }

  TFormMain = class(TForm, IObserver)
    actExit: TAction;
    actDeleteItem: TAction;
    actNewAttribute: TAction;
    actNewClass: TAction;
    actSave: TAction;
    actOpen: TAction;
    actNew: TAction;
    ActionList: TActionList;
    CheckboxAttrList: TCheckBox;
    CheckBoxClassPersist: TCheckBox;
    CheckBoxAttrPersist: TCheckBox;
    CheckBoxObserver: TCheckBox;
    CheckBoxPersistence: TCheckBox;
    ComboBoxAttrType: TComboBox;
    Edit1: TEdit;
    EditAttrName: TEdit;
    EditClassName: TEdit;
    ImageListTree: TImageList;
    ImageListToolbar: TImageList;
    LabelAttrName: TLabel;
    LabelAttrType: TLabel;
    LabelClassName: TLabel;
    LabelModelName: TLabel;
    MainMenu: TMainMenu;
    MenuHelp: TMenuItem;
    MenuItemInfo: TMenuItem;
    MenuItemNew: TMenuItem;
    MenuItemOpen: TMenuItem;
    MenuItemSave: TMenuItem;
    MenuItemSep1: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemFile: TMenuItem;
    PageControl: TPageControl;
    PageControlProperties: TPageControl;
    PopupMenuTree: TPopupMenu;
    Splitter: TSplitter;
    StatusBar: TStatusBar;
    Output: TSynEdit;
    SynPasSyn: TSynPasSyn;
    TabPropAttr: TTabSheet;
    TabPropClass: TTabSheet;
    TabPropModel: TTabSheet;
    TabSheetModel: TTabSheet;
    TabSheetOutput: TTabSheet;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButtonNewAttribute: TToolButton;
    ToolButtonDeleteElement: TToolButton;
    ToolButtonNewClass: TToolButton;
    ToolButton4: TToolButton;
    ToolButtonExit: TToolButton;
    ToolButtonNew: TToolButton;
    ToolButtonOpen: TToolButton;
    ToolButtonSave: TToolButton;
    TreeViewModel: TTreeView;
    procedure actExitExecute(Sender: TObject);
    procedure actNewClassExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PageControlPropertiesChange(Sender: TObject);
    procedure TreeViewModelSelectionChanged(Sender: TObject);
  private
    { private declarations }
    TreeData : TDataTree;
    Controller : TController;
    function GetNodeElement(Node: TTreeNode):TNodeData;
  public
    { public declarations }
    procedure Refresh();
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.actExitExecute(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TFormMain.actNewClassExecute(Sender: TObject);
begin
     Controller.AddClass('NewClass');
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
     TreeData:= TDataTree.Create();
     Controller := TController.Create(TreeData, Self);
end;

procedure TFormMain.PageControlPropertiesChange(Sender: TObject);
begin

end;

procedure TFormMain.TreeViewModelSelectionChanged(Sender: TObject);
var
  data : TNodeData;
begin
     if TreeViewModel.Selected <> nil then begin
       data:= GetNodeElement(TreeViewModel.Selected);
       if data <> nil then begin

       end;
     end;
end;

function TFormMain.GetNodeElement(Node: TTreeNode): TNodeData;
begin
     Result:= nil;
     if Node.Data <> nil then begin
       Result := TNodeData(Node.Data);
     end;
end;

procedure TFormMain.Refresh;
var
   nodeRoot, subNode : TTreeNode;
   i : integer;
begin
     //Root
     TreeViewModel.Items.Clear;
     nodeRoot:= TreeViewModel.Items.Add(nil, TreeData.Name);
     nodeRoot.ImageIndex:= 0;
     nodeRoot.SelectedIndex:= nodeRoot.ImageIndex;

     //Classes
     for i:= 0 to TreeData.Classes.Count - 1 do begin
         subNode:= TreeViewModel.Items.AddChild(nodeRoot, TreeData.Classes[i].Name);
         subNode.ImageIndex:= 1;
         subNode.SelectedIndex:= subNode.ImageIndex;
         subNode.Data:=Pointer(TreeData.Classes[i]);
     end;

     //Attributes

     TreeViewModel.FullExpand;
end;


end.

