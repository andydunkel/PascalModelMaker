unit AboutDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Arrow, EditBtn, lclintf;

type

  { TAbout }

  TAbout = class(TForm)
    ButtonClose: TButton;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure ButtonCloseClick(Sender: TObject);
    procedure Label5Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  About: TAbout;

implementation

{$R *.lfm}

{ TAbout }

procedure TAbout.ButtonCloseClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TAbout.Label5Click(Sender: TObject);
begin
     OpenUrl('http://www.andydunkel.net');
end;

end.

