unit day1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayOne}
  TDayOne = class(TAocPuzzle)
  private
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayOne }

constructor TDayOne.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 1',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayOne.runPartOne;
begin
  results.Clear;
end;

procedure TDayOne.runPartTwo;
begin
  results.Clear;
end;


end.

                
