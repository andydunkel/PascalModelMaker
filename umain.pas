unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynHighlighterPas, SynEdit, Forms, Controls,
  Graphics, Dialogs, ComCtrls, ExtCtrls, StdCtrls, Menus, ActnList, uData,
  Controller, uConsts;

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
    EditUnitName: TEdit;
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
    Empty: TTabSheet;
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
    procedure actDeleteItemExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actNewClassExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PageControlPropertiesChange(Sender: TObject);
    procedure TreeViewModelSelectionChanged(Sender: TObject);
    procedure UpdateAttrProperties(Sender: TObject);
    procedure UpdateClassProperties(Sender: TObject);
    procedure UpdateModelTab(Sender: TObject);
  private
    { private declarations }
    TreeData : TDataTree;
    Controller : TController;
    function GetNodeElement(Node: TTreeNode):TNodeData;
    procedure HandleEnablement(Data: TNodeData);
    procedure HandleEnablementModel();
    procedure HandleEnablementClass();
    procedure HandleEnablementAttr();

  public
    { public declarations }
    procedure Refresh();
  end;

const
  TAB_EMPTY = 0;
  TAB_MODEL = 1;
  TAB_CLASS = 2;
  TAB_ATTR = 3;
  TAB_METHOD = 4;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.actExitExecute(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TFormMain.actDeleteItemExecute(Sender: TObject);
begin
  ShowMessage('Delete');
end;

procedure TFormMain.actNewClassExecute(Sender: TObject);
begin
     Controller.AddClass('NewClass');
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
     TreeData:= TDataTree.Create();
     Controller:= TController.Create(TreeData, Self);
     PageControlProperties.ShowTabs:= false;
     PageControlProperties.ActivePageIndex:= TAB_EMPTY;
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
       HandleEnablement(data);
     end;
end;

procedure TFormMain.UpdateAttrProperties(Sender: TObject);
begin
     Controller.UpdateAttrProperties(EditAttrName.Text, ComboBoxAttrType.Text
                                 , CheckBoxAttrPersist.Checked, CheckboxAttrList.Checked);
end;

procedure TFormMain.UpdateClassProperties(Sender: TObject);
begin
     Controller.UpdateClassProperties(EditClassName.Text, CheckBoxClassPersist.Checked);
end;

procedure TFormMain.UpdateModelTab(Sender: TObject);
begin
     Controller.UpdateModelProperties(EditUnitName.Text, CheckBoxObserver.Checked, CheckBoxPersistence.Checked);
end;

function TFormMain.GetNodeElement(Node: TTreeNode): TNodeData;
begin
     Result:= nil;
     if Node.Data <> nil then begin
       Result := TNodeData(Node.Data);
     end;
end;

procedure TFormMain.HandleEnablement(Data: TNodeData);
begin
     Controller.CurrentNode:= Data;

     if Data = nil then begin
       HandleEnablementModel();
       Exit;
     end;

     case Data.ElementType of
          _Class : HandleEnablementClass();
          _Attribute : HandleEnablementAttr();
     end;
end;

procedure TFormMain.HandleEnablementModel;
begin
  PageControlProperties.ActivePageIndex:= TAB_MODEL;
  EditUnitName.Text:= TreeData.UnitName;
  CheckBoxObserver.Checked:= TreeData.GenerateObserverCode;
  CheckBoxPersistence.Checked:= TreeData.PersistanceCode;
end;

procedure TFormMain.HandleEnablementClass();
begin
     actNewAttribute.Enabled:= true;
     actNewClass.Enabled:= true;
     PageControlProperties.ActivePageIndex:= TAB_CLASS;

     EditClassName.Text:= Controller.CurrentNode.Name;
     CheckBoxClassPersist.Checked:= Controller.CurrentNode.Persist;
end;

procedure TFormMain.HandleEnablementAttr();
begin
     actNewAttribute.Enabled:= false;
     actNewClass.Enabled:= false;
     PageControlProperties.ActivePageIndex:= TAB_ATTR;

     EditAttrName.Text:= Controller.CurrentNode.Name;
     ComboBoxAttrType.Text:= Controller.CurrentNode.DataType;
     CheckBoxAttrPersist.Checked:= Controller.CurrentNode.Persist;
     CheckBoxAttrList.Checked:= Controller.CurrentNode.List;
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

