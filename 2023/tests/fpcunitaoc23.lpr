program fpcunitaoc23;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, day12, fileUtilities, arrayUtils, aocUtils,
  anysort, testday12;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

