unit day7;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDaySeven}
  TDaySeven = class(TAocPuzzle)
  private
  procedure loadMap;
  procedure printMap;
  function startIndex:integer;
  function runBeam(start:TPoint;iteration:integer;direction:string):integer;
  function uniquePathsRecur(x,y:integer):int64;
  function uniquePaths:int64;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

var
  map:T3DStringArray;
{ TDaySeven }

procedure TDaySeven.loadMap;
var
  puzzleRow,puzzleCol:integer;
begin
  map.clear;
  //Put the puzzle info into layer 1
  for puzzleRow:=0 to pred(puzzleInputLines.size) do
    for puzzleCol:=0 to pred(puzzleInputLines[puzzleRow].Length) do
      begin
      map.setValue(puzzleCol,puzzleRow,2,'U');
      map.setValue(puzzleCol,puzzleRow,1,puzzleInputLines[puzzleRow].Substring(puzzleCol,1));
      end;
end;

procedure TDaySeven.printMap;
var
  y,z:integer;
begin
  for z:=0 to pred(map.layers) do
    for y:=0 to pred(map.rows(z))do
      results.Add(map.getValue(y,z).toString(''));
end;

function TDaySeven.startIndex: integer;
var
  index:integer;
begin
  result:=-1;
  for index:=0 to pred(map.size(1,0)) do
    if (map.getValue(index,0,1)='S') then result:=index;
end;

//Start at the startpoint and continue down until we hit a splitter
//Then increment splitter count and call this method again
//Current path ends when we hit a splitter
function TDaySeven.runBeam(start:TPoint;iteration:integer;direction:string): integer;
var
  index:TPoint;
  cellContents:string;
begin
  result:=0;
  index:=start;
  repeat
  cellContents:=map.getValue(index.X,index.Y,1);
  if (cellContents <> '^') then index.Y:=index.Y + 1;
  until (cellContents = '^') or (index.Y >= map.rows(1));
  if (cellContents = '^') then
    begin
    if map.getValue(index.X,index.Y,2) = 'U' then
      begin
      result:=1;
      map.setValue(index.X,index.Y,2,'X');
      //If we're not on the edge, move x 1 space left and run again
      if (index.X > 0) then result:=result + runBeam(TPoint.Create(index.X-1,index.Y),iteration+1,'l');
      if (index.X < pred(map.size(1,index.Y))) then result:= result + runBeam(TPoint.Create(index.X+1,index.Y),iteration+1,'r');
      end;
    end;
end;

function TDaySeven.uniquePathsRecur(x, y: integer): int64;
var
  xIndex,yIndex:integer;
  cellContents,leftPaths,rightPaths:string;
  leftPathTotal,rightPathTotal:int64;
begin
  xIndex:=x;
  yIndex:=y;

  result:=0;
  if (xIndex < 0)or(xIndex > pred(map.size(1,y))) then exit;
  cellContents:=map.getValue(xIndex,yIndex,1);

  if (cellContents = '^') then
    begin

    if (xIndex > 0) then
      begin
      leftPaths:=map.getValue(xIndex-1,yIndex,2);
      if leftPaths = 'U' then
        begin
        leftPathTotal:=uniquePathsRecur(xIndex-1,yIndex);
        map.setValue(xIndex-1,yIndex,2,leftPathTotal.ToString);
        end else leftPathTotal:=leftPaths.ToInt64;
      end else leftPathTotal:=0;

    if (xIndex < pred(map.size(1,yIndex))) then
      begin
      rightPaths:=map.getValue(xIndex+1,yIndex,2);
      if rightPaths = 'U' then
        begin
        rightPathTotal:=uniquePathsRecur(xIndex+1,yIndex);
        map.setValue(xIndex+1,yIndex,2,rightPathTotal.ToString);
        end else rightPathTotal:=rightPaths.ToInt64;
      end;
    result:= leftPathTotal + rightPathTotal;
    end
  else if yIndex = pred(map.rows(1)) then
    result:=1
  else
    result:= uniquePathsRecur(xIndex,yIndex+1);
end;

function TDaySeven.uniquePaths: int64;
begin
  result:=uniquePathsRecur(startIndex,0);
end;

constructor TDaySeven.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 7',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDaySeven.runPartOne;
var
  splitters:integer;
begin
  results.Clear;
  loadMap;
  splitters:=runBeam(TPoint.Create(startIndex,0),0,'c');
  results.add('splitters found '+splitters.toString);
end;

procedure TDaySeven.runPartTwo;
var
  totalPaths:int64;
begin
  results.Clear;
  loadMap;
  totalPaths:=uniquePaths;
  results.add('unique paths: '+totalPaths.toString);
end;


end.

                
