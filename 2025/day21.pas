unit day21;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayTwentyOne}
  TDayTwentyOne = class(TAocPuzzle)
  private
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayTwentyOne }

constructor TDayTwentyOne.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 21',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayTwentyOne.runPartOne;
begin
  results.Clear;
end;

procedure TDayTwentyOne.runPartTwo;
begin
  results.Clear;
end;


end.

                
