unit day2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics;
type

  { TDayTwo }
  TDayTwo = class(TAocPuzzle)
  private
  fName:string;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayOne }

constructor TDayTwo.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,paintbox_);
fName:= 'Day 2';
//parent loads file as string;
end;

procedure TDayTwo.runPartOne;
begin
  DebugLn('run part one');
end;

procedure TDayTwo.runPartTwo;
begin
  DebugLn('run part two');
end;

end.

