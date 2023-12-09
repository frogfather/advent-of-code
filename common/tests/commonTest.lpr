program commonTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, aocUtilTests, aocUtils;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

