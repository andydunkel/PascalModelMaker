unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Menus, ActnList;

type

  { TFormMain }

  TFormMain = class(TForm)
    actExit: TAction;
    actSave: TAction;
    actOpen: TAction;
    actNew: TAction;
    ActionList: TActionList;
    GroupBoxProperties: TGroupBox;
    ImageListTree: TImageList;
    ImageListToolbar: TImageList;
    MainMenu: TMainMenu;
    MenuItemNew: TMenuItem;
    MenuItemOpen: TMenuItem;
    MenuItemSave: TMenuItem;
    MenuItemSep1: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemFile: TMenuItem;
    PageControl: TPageControl;
    Splitter: TSplitter;
    StatusBar: TStatusBar;
    TabSheetModel: TTabSheet;
    TabSheetOutput: TTabSheet;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButtonExit: TToolButton;
    ToolButtonNew: TToolButton;
    ToolButtonOpen: TToolButton;
    ToolButtonSave: TToolButton;
    TreeViewModel: TTreeView;
    procedure actExitExecute(Sender: TObject);
  private
    { private declarations }
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

end.

