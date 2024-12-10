unit day10;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils, pointVisitCount;
type

  { TDayTen}
  TDayTen = class(TAocPuzzle)
  private
  procedure populateMapAndFindAllTrailheads;
  function findSumOfAllPaths:integer;
  function findPathsFrom(trailhead:TPoint):integer;
  function validPath(current,offset:TPoint;value:integer):boolean;
  function atSummit(currentPoint:TPoint):boolean;
  function findSumOfAllTrails:integer;
  function findSumOfTrailsFrom(trailhead:TPoint):integer;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayTen }
var
  map:T3DIntMap;
  queue:TPointArray;
  seen: TPointArray;
  seenCount:TPointVisitCountArray;
  trailHeads:TPointArray;
  offsets:TPointArray;
procedure TDayTen.populateMapAndFindAllTrailheads;
var
  row,col:integer;
  value:integer;
begin
  for row:=0 to pred(puzzleInputLines.size) do
    for col:=0 to pred(puzzleInputLines[row].Length) do
        begin
        value:=puzzleInputLines[row].Substring(col,1).toInteger;
        map.setValue(col,row,0,value);
        if (value = 0) then trailheads.push(TPoint.create(col,row));
        end;
end;

function TDayTen.findSumOfAllPaths: integer;
var
  index:integer;
begin
  result:=0;
  for index:=0 to pred(trailheads.size) do
    result:=result + findPathsFrom(trailheads[index]);
end;

function TDayTen.findPathsFrom(trailhead: TPoint): integer;
var
  currentPoint,offsetPoint:TPoint;
  offset:integer;
begin
  queue.clear;
  seen.clear;
  queue.push(trailhead);
  seen.push(trailhead);
  result:=0;
  while (queue.size > 0) do
    begin
    currentPoint:=queue.popLeft;
    for offset:=0 to pred(offsets.size) do
      if (validPath(currentPoint,offsets[offset],map.getValue(currentPoint.X,currentPoint.Y,0))) then
        begin
        offsetPoint:=TPoint.Create(currentPoint.X+offsets[offset].X, currentPoint.Y+offsets[offset].Y);
        seen.push(offsetPoint);
        if atSummit(offsetPoint) then result:=result+1
        else queue.push(offsetPoint);
        end;
    end;
end;

function TDayTen.validPath(current, offset: TPoint; value: integer): boolean;
var
  thisPoint:TPoint;
begin
  result:=false;
  thisPoint:=TPoint.Create(current.X+offset.X, current.Y+offset.Y);
  if (thisPoint.X < 0) or (thisPoint.Y < 0)
  or (thisPoint.Y > map.rows(0)) or (thisPoint.X > pred(map.size(0,thisPoint.Y))) then exit;
  if (map.getValue(thisPoint.X,thisPoint.Y,0) - value <> 1) then exit;
  if (seen.indexOf(thisPoint) > -1) then exit;
  result:=true;
end;

function TDayTen.atSummit(currentPoint: TPoint): boolean;
begin
  result:=map.getValue(currentPoint.X,currentPoint.Y,0) = 9;
end;

function TDayTen.findSumOfAllTrails: integer;
var
  index:integer;
begin
  result:=0;
  for index:=0 to pred(trailheads.size) do
    result:=result + findSumOfTrailsFrom(trailheads[index]);
end;

function TDayTen.findSumOfTrailsFrom(trailhead: TPoint): integer;
var
  currentPoint,offsetPoint:TPoint;
  offset:integer;
begin
  queue.clear;
  seenCount.clear;
  queue.push(trailhead);
  seenCount.addToCount(trailhead,1);
  result:=0;
  while (queue.size > 0) do
    begin
    currentPoint:=queue.popLeft;
    if (map.getValue(currentPoint.X,currentPoint.Y,0) = 9)
      then result:=result + seenCount.countOf(currentPoint);
    for offset:=0 to pred(offsets.size) do
      if (validPath(currentPoint,offsets[offset],map.getValue(currentPoint.X,currentPoint.Y,0))) then
        begin
        offsetPoint:=TPoint.Create(currentPoint.X+offsets[offset].X, currentPoint.Y+offsets[offset].Y);
        if (seenCount.indexOf(offsetPoint) = -1) then queue.push(offsetPoint);
        seenCount.addToCount(offsetPoint, seenCount.countOf(currentPoint));
        end;
    end;
end;

constructor TDayTen.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 10',paintbox_);
map:=T3DIntMap.create;
queue:=TPointArray.create;
seen:=TPointArray.create;
seenCount:=TPointVisitCountArray.create;
trailheads:=TPointArray.create;
offsets:=TPointArray.create;
offsets.push(TPoint.Create(0,-1));
offsets.push(TPoint.Create(1,0));
offsets.push(TPoint.Create(0,1));
offsets.push(TPoint.Create(-1,0));

//parent loads the file as a string and converts to string array;
end;

procedure TDayTen.runPartOne;
var
  sumOfAllPaths:integer;
begin
  results.Clear;
  map.clear;
  populateMapAndFindAllTrailheads;
  sumOfAllPaths:=findSumOfAllPaths;
  results.add('total '+sumOfAllPaths.toString);
end;

procedure TDayTen.runPartTwo;
var
  sumOfAllTrails:integer;
begin
  results.Clear;
  map.Clear;
  populateMapAndFindAllTrailheads;
  sumOfAllTrails:=findSumOfAllTrails;
  results.add('total '+sumOfAllTrails.toString);
end;


end.

                
