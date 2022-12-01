unit day1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics;
type
  
  { TDayOne }
  TDayOne = class(TAocPuzzle)
  private
  fName:string;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayOne }

constructor TDayOne.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,paintbox_);
//if paintbox <> nil then paintbox.OnPaint:=@doPaint;
fName:= 'Day 1';
//parent loads file as string;
end;

//Private methods
procedure TDayOne.runPartOne;
begin
  DebugLn('run part one');
end;

procedure TDayOne.runPartTwo;
begin
  DebugLn('run part two');
end;

end.

