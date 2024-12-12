unit day12;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayTwelve}
  TDayTwelve = class(TAocPuzzle)
  private
  procedure populateMap;
  function allPlotPrices(sides:boolean=false):int64;
  function mapValue(point:TPoint):string;
  function nextRegionPrice(startAt:TPoint; sides:boolean=false):int64;
  function isOutOfRange(point:TPoint):boolean;
  function outsideEdges(point:TPoint):integer;
  function isDifferent(point:TPoint;value:string):boolean;
  function unvisitedNeighbours(point:TPoint):TPointArray;
  function regionPerimeter(region:TPointArray):int64;
  function regionSides(region:TPointArray):int64;
  function getLowLimit(region:TPointArray):TPoint;
  function getHighLimit(region:TPointArray):TPoint;

  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayTwelve }
var
  map:T2DStringArray;
  pointsSeen:TPointArray;
  queue:TPointArray;

procedure TDayTwelve.populateMap;
var
  index:integer;
begin
  for index:=0 to pred(puzzleInputLines.size) do
    map.push(puzzleInputLines[index]);
end;

function TDayTwelve.mapValue(point: TPoint): string;
begin
  result:='';
  if isOutOfRange(point) then exit;
  result:=map[point.Y][point.X];
end;


function TDayTwelve.allPlotPrices(sides:boolean=false): int64;
var
  row,col:integer;
  currentPoint:TPoint;
begin
  result:=0;
  for row:=0 to pred(map.rows) do
    for col:=0 to pred(map.size(row)) do
      begin
      currentPoint.X:= col;
      currentPoint.Y:= row;
      if (pointsSeen.indexOf(currentPoint) = -1) then
        begin
        result:=result+nextRegionPrice(currentPoint,sides);
        end;
      end;
end;

function TDayTwelve.nextRegionPrice(startAt:TPoint; sides:boolean=false):int64;
var
  area,perimeter,sideCount:int64;
  neighboursToVisit:TPointArray;
  index:integer;
  region:TPointArray;
  currentPoint:TPoint;
begin
  result:=0;
  region:=TPointArray.create;
  queue.clear;
  //put startAt in the queue and then continue until no neighbours
  queue.push(startAt);
  region.push(startAt);
  while (queue.size > 0) do
    begin
    currentPoint:=queue.popLeft;
    pointsSeen.push(currentPoint);
    neighboursToVisit:=unvisitedNeighbours(currentPoint);
    for index:= 0 to pred(neighboursToVisit.size) do
      if (region.indexOf(neighboursToVisit[index])=-1) then
        begin
        region.push(neighboursToVisit[index]);
        queue.push(neighboursToVisit[index]);
        end;
    end;
  if sides then
    begin
    sideCount:=regionSides(region);
    result:=region.size * sideCount;
    end else
    begin
    perimeter:=regionPerimeter(region);
    result:=region.size * perimeter
    end;
end;

function TDayTwelve.isOutOfRange(point: TPoint): boolean;
begin
  result:=(point.Y < 0)
        or(point.Y > pred(map.rows))
        or(point.X < 0)
        or(point.X > pred(map.size(point.Y)));
end;

function TDayTwelve.outsideEdges(point: TPoint): integer;
var
  pAbove,pBelow,pLeft,pRight:TPoint;
  currentValue:string;
begin
  result:=0;
  currentValue:=mapValue(point);
  pAbove:=TPoint.Create(point.X, point.Y-1);
  pBelow:=TPoint.Create(point.X, point.Y+1);
  pLeft:=TPoint.Create(point.X-1, point.Y);
  pRight:=TPoint.Create(point.X+1, point.Y);
  if (isOutOfRange(pAbove) or isDifferent(pAbove,currentValue)) then result:=result+1;
  if (isOutOfRange(pBelow) or isDifferent(pBelow,currentValue)) then result:=result+1;
  if (isOutOfRange(pLeft) or isDifferent(pLeft,currentValue)) then result:=result+1;
  if (isOutOfRange(pRight) or isDifferent(pRight,currentValue)) then result:=result+1;
end;

function TDayTwelve.isDifferent(point: TPoint; value: string): boolean;
begin
  result:=(mapValue(point) <> value)
end;

function TDayTwelve.unvisitedNeighbours(point: TPoint): TPointArray;
var
  pAbove,pBelow,pLeft,pRight:TPoint;
  currentValue:string;
begin
  currentValue:=mapValue(point);
  result:=TPointArray.create;
  pAbove:=TPoint.Create(point.X, point.Y-1);
  pBelow:=TPoint.Create(point.X, point.Y+1);
  pLeft:=TPoint.Create(point.X-1, point.Y);
  pRight:=TPoint.Create(point.X+1, point.Y);
  if (not isOutOfRange(pAbove))
     and(not isDifferent(pAbove,currentValue))
     and(pointsSeen.indexOf(pAbove)= -1) then result.push(pAbove);
  if (not isOutOfRange(pBelow))
     and(not isDifferent(pBelow,currentValue))
     and(pointsSeen.indexOf(pBelow)= -1) then result.push(pBelow);
  if (not isOutOfRange(pLeft))
     and(not isDifferent(pLeft,currentValue))
     and(pointsSeen.indexOf(pLeft)= -1) then result.push(pLeft);
  if (not isOutOfRange(pRight))
     and(not isDifferent(pRight,currentValue))
     and(pointsSeen.indexOf(pRight)= -1) then result.push(pRight);
end;

function TDayTwelve.regionPerimeter(region: TPointArray): int64;
var
  index:integer;
begin
  //For each item in the region find its perimeter
  result:=0;
  for index:=0 to pred(region.size)do
    result:=result + outsideEdges(region[index]);
end;

function TDayTwelve.regionSides(region: TPointArray): int64;
var
  lowLimit,highLimit,currentPos,northPos,southPos,westPos,eastPos:TPoint;
  row,col:integer;
  onNorthEdge,onSouthEdge,onWestEdge,onEastEdge:boolean;
begin
  result:=0;
  lowLimit:=getLowLimit(region);
  highLimit:=getHighLimit(region);
  currentPos:=TPoint.create(0,0);
  northPos:=TPoint.Create(0,0);
  for row:=lowLimit.Y to highLimit.Y do
    begin
    onNorthEdge:=false;
    onSouthEdge:=false;
    for col:=lowLimit.X to highLimit.X do
      begin
      currentPos.X:=col;
      currentPos.Y:=row;
      northPos.X:=col;
      northPos.Y:=row - 1;
      southPos.X:=col;
      southPos.Y:=row + 1;
      //Is this coord in the list and is it on an edge looking north?
      if (region.indexOf(currentPos) > -1)
         and (isOutOfRange(northPos) or (isDifferent(northPos,mapValue(currentPos))))
         then
         begin
         if not onNorthEdge then //If we're not already on a side then it's a new side
           begin
           onNorthEdge:=true;
           result:=result+1;
           end;
         end else onNorthEdge:=false;
      //Do the same looking south
      if (region.indexOf(currentPos) > -1)
         and (isOutOfRange(southPos) or (isDifferent(southPos,mapValue(currentPos))))
         then
         begin
         if not onSouthEdge then //If we're not already on a side then it's a new side
           begin
           onSouthEdge:=true;
           result:=result+1;
           end;
         end else onSouthEdge:=false;
      end;
    end;
  //Now do the same thing looking west and east
  for col:=lowLimit.X to highLimit.X do
    begin
    onWestEdge:=false;
    onEastEdge:=false;
    for row:= lowLimit.Y to highLimit.Y do
      begin
      currentPos.X:=col;
      currentPos.Y:=row;
      westPos.X:=col-1;
      westPos.Y:=row;
      eastPos.X:=col+1;
      eastPos.Y:=row;
      if (region.indexOf(currentPos) > -1)
         and (isOutOfRange(westPos) or (isDifferent(westPos,mapValue(currentPos))))
         then
         begin
         if not onWestEdge then //If we're not already on a side then it's a new side
           begin
           onWestEdge:=true;
           result:=result+1;
           end;
         end else onWestEdge:=false;
      //Do the same looking east
      if (region.indexOf(currentPos) > -1)
         and (isOutOfRange(eastPos) or (isDifferent(eastPos,mapValue(currentPos))))
         then
         begin
         if not onEastEdge then //If we're not already on a side then it's a new side
           begin
           onEastEdge:=true;
           result:=result+1;
           end;
         end else onEastEdge:=false;
      end;
    end;
end;

function TDayTwelve.getLowLimit(region: TPointArray): TPoint;
var
  index:integer;
begin
  result:=TPoint.Create(region[0].X,region[0].Y);
  for index:=0 to pred(region.size) do
    begin
    if (region[index].X < result.X) then result.X:=region[index].X;
    if (region[index].Y < result.Y) then result.Y:=region[index].Y;
    end;
end;

function TDayTwelve.getHighLimit(region: TPointArray): TPoint;
var
  index:integer;
begin
  result:=TPoint.Create(region[0].X,region[0].Y);
  for index:=0 to pred(region.size) do
    begin
    if (region[index].X > result.X) then result.X:=region[index].X;
    if (region[index].Y > result.Y) then result.Y:=region[index].Y;
    end;
end;

constructor TDayTwelve.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 12',paintbox_);
 map:=T2DStringArray.create;
 pointsSeen:=TPointArray.create;
 queue:=TPointArray.create;
//parent loads the file as a string and converts to string array;
end;

procedure TDayTwelve.runPartOne;
begin
  results.Clear;
  map.clear;
  populateMap;
  pointsSeen.clear;
  results.add('Total is '+allPlotPrices.toString);
end;

procedure TDayTwelve.runPartTwo;
begin
  results.Clear;
  map.clear;
  populateMap;
  pointsSeen.clear;
  results.add('Total is '+allPlotPrices(true).toString);
end;


end.

                
