program DelphiModelling;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uMain, uConsts, uData, Controller, Generator, XMLHelper, AboutDialog;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TAbout, About);
  Application.Run;
end.

