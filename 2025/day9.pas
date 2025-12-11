unit day9;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayNine}
  TDayNine = class(TAocPuzzle)
  private
  procedure loadTiles;
  function getLargestArea:int64;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

var
  redTiles:TPointArray;
{ TDayNine }

procedure TDayNine.loadTiles;
var
  index:integer;
begin
  for index:=0 to pred(puzzleInputlines.size)do
    redTiles.push(TPoint.Create(puzzleInputLines[index].Split(',')[0].ToInteger,puzzleInputLines[index].Split(',')[1].ToInteger));
end;

function TDayNine.getLargestArea: int64;
var
  index1,index2:integer;
  area:int64;
begin
  result:=0;
  for index1:=0 to pred(pred(redTiles.size))do
    for index2:=index1+1 to pred(redTiles.size)do
      begin
      area:=(abs(redTiles[index1].X - redTiles[index2].X)+1)*(abs(redTiles[index1].Y - redTiles[index2].Y)+1);
      if (area > result) then
        begin
        result := area;
        results.add('largest area is now '+result.toString);
        end;
      end;
end;

constructor TDayNine.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 9',paintbox_);
//parent loads the file as a string and converts to string array;
redTiles:=TPointArray.create;
end;

procedure TDayNine.runPartOne;
var
  maxArea:Int64;
begin
  results.Clear;
  loadTiles;
  maxArea:=getLargestArea;
  results.add('Max area is '+maxArea.toString);
end;

procedure TDayNine.runPartTwo;
begin
  results.Clear;
  loadTiles;

end;


end.

                
