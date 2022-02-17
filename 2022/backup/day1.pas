unit day1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, aocPuzzle,LazLogger;
type
  
  { TDayOne }
  TDayOne = class(TAocPuzzle)
  private
  procedure runPartOne; override; reintroduce;
  procedure runPartTwo; override; reintroduce;
  public
  constructor create(filename:string);
  end;

implementation

{ TDayOne }

constructor TDayOne.create(filename:string);
begin
inherited create(filename);
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

