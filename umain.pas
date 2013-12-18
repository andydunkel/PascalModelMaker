unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Menus, ActnList, uData;

type

  { TFormMain }

  TFormMain = class(TForm)
    actExit: TAction;
    actDeleteItem: TAction;
    actNewAttribute: TAction;
    actNewClass: TAction;
    actSave: TAction;
    actOpen: TAction;
    actNew: TAction;
    ActionList: TActionList;
    GroupBoxProperties: TGroupBox;
    ImageListTree: TImageList;
    ImageListToolbar: TImageList;
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
    PopupMenuTree: TPopupMenu;
    Splitter: TSplitter;
    StatusBar: TStatusBar;
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
  private
    { private declarations }
    TreeData : TDataTree;
    procedure Refresh();
  public
    { public declarations }
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
     TreeData.AddClass('New class');
     Refresh;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
     TreeData:= TDataTree.Create();
     Refresh;
end;

procedure TFormMain.Refresh;
var
   nodeRoot : TTreeNode;
begin
     //Rebuild the tree
     TreeViewModel.Items.Clear;
     nodeRoot:= TreeViewModel.Items.Add(nil, TreeData.Name);
     nodeRoot.ImageIndex:= 0;
     nodeRoot.SelectedIndex:= nodeRoot.ImageIndex;

end;

end.

