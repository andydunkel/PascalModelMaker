unit Generator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uData;

type
    { TGenerator }
TGenerator = class
      public
        class function GenerateClassCode(Data: TDataTree): String;
end;


implementation

{ TGenerator }

class function TGenerator.GenerateClassCode(Data: TDataTree): String;
var
  C : String;
begin
  C:= Data.Template;
  Result:= C;
end;

end.

