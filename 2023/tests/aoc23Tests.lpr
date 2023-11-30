program aoc23Tests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, day1, unit1;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

