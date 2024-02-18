unit day21;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayTwentyOne}
  TDayTwentyOne = class(TAocPuzzle)
  private
  procedure buildMap;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation
var
  map_: T2DStringArray;

{ TDayTwentyOne }

constructor TDayTwentyOne.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 16',paintbox_);
//parent loads the file as a string and converts to string array;
map_:=T2DStringArray.create;
end;

procedure TDayTwentyOne.runPartOne;
begin
  results.Clear;
  //Need recursive solution that avoids going back the way we've come
  buildMap;
  //first param is entry direction
  //takeStep(0);
end;

procedure TDayTwentyOne.runPartTwo;
begin
  results.Clear;
end;

procedure TDayTwentyOne.buildMap;
var
  index:integer;
begin
  setLength(map_,0);
  for index:=0 to pred(puzzleInputLines.size) do
    map_.push(puzzleInputLines[index]);

end;


end.


