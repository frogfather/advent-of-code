unit day11;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayEleven}
  TDayEleven = class(TAocPuzzle)
  private
  procedure loadMap;
  function getGalaxies:TPointArray;
  function getCrossings(inXDirection:boolean=true):TIntArray;
  function getBlankCrossings(xCrossings,yCrossings:TIntArray;start_, end_: TPoint): TPoint;
  function getDistances(galaxies:TPointArray;xCrossings,yCrossings:TIntArray;blankDuplicates:Int64):TInt64Array;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation
var
  map: TStringArray;

{ TDayEleven }

constructor TDayEleven.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 11',paintbox_);
//parent loads the file as a string and converts to string array;
map:=TStringArray.create;
end;

procedure TDayEleven.runPartOne;
var
  elementNo:integer;
  galaxies: TPointArray;
  xCrossings,yCrossings:TIntArray;
  distances:TInt64Array;
  totalDistance:integer;
begin
  results.Clear;
  loadMap;
  xCrossings:=getCrossings;
  yCrossings:=getCrossings(false);
  galaxies:=getGalaxies;
  distances:=getDistances(galaxies,xCrossings,yCrossings,1);
  totalDistance:=0;
  for elementNo:=0 to pred(distances.size) do
    totalDistance:=totalDistance + distances[elementNo];
  results.add('Total distance is '+totalDistance.ToString);
end;

procedure TDayEleven.runPartTwo;
var
  elementNo:integer;
  xCrossings,yCrossings:TIntArray;
  galaxies: TPointArray;
  distances:TInt64Array;
  totalDistance:int64;
begin
  results.Clear;
  loadMap;
  xCrossings:=getCrossings;
  yCrossings:=getCrossings(false);
  galaxies:=getGalaxies;
  distances:=getDistances(galaxies,xCrossings,yCrossings,999999);
  totalDistance:=0;
  for elementNo:=0 to pred(distances.size) do
    totalDistance:=totalDistance + distances[elementNo];
  results.add('Total distance is '+totalDistance.ToString);
end;

procedure TDayEleven.loadMap;
var
  lineNo:integer;
begin
  for lineNo:=0 to pred(puzzleInputLines.size) do
    map.push(puzzleInputLines[lineNo]);
end;

function TDayEleven.getGalaxies: TPointArray;
var
  lineNo,elementNo:integer;
begin
  result:=TPointArray.create;
  for lineNo:=0 to pred(map.size) do
    for elementNo:=0 to pred(map[0].Length) do
      if map[lineNo].Substring(elementNo,1) = '#' then result.push(TPoint.Create(elementNo,lineNo));
end;

function TDayEleven.getCrossings(inXDirection: boolean): TIntArray;
var
  elementNo,lineNo:integer;
  columnIsEmpty:boolean;
begin
  result:=TIntArray.create;

  if inXDirection then //In x direction we want the number of blank columns
  begin
  for elementNo:=0 to pred(map[0].Length) do
    begin
    columnIsEmpty:=true;
    for lineNo:=0 to pred(map.size) do
      begin
      if map[lineNo].Substring(elementNo,1) = '#' then columnIsEmpty:=false;
      end;
    if columnIsEmpty then result.push(elementNo);
    end;
  end else
  begin
  for lineNo:=0 to pred(map.size) do
    begin
    if (map[lineNo].IndexOf('#') = -1) then
    result.push(lineNo);
    end;
  end;

end;

function TDayEleven.getBlankCrossings(xCrossings,yCrossings:TIntArray;start_, end_: TPoint): TPoint;
var
  x,y:integer;
  startx,starty,endx,endy:integer;
begin
  result:=TPoint.Create(0,0);
  if (start_.X < end_.X) then
    begin
    startx:=start_.X;
    endx:=end_.X;
    end else
    begin
    startx:=end_.X;
    endx:=start_.X;
    end;
  if (start_.Y < end_.Y) then
    begin
    starty:=start_.y;
    endy:=end_.y;
    end else
    begin
    starty:=end_.y;
    endy:=start_.y;
    end;
  //For x, how many entries are between the start and end?
  for x:= 0 to pred(xCrossings.size) do
    if (xCrossings[x] > startX) and (xCrossings[x] < endX) then result.X:=result.X+1;
  for y:=0 to pred(yCrossings.size) do
    if (yCrossings[y]> startY) and (yCrossings[y] < endY) then result.Y:= result.Y+1;
end;

function TDayEleven.getDistances(galaxies:TPointArray;xCrossings,yCrossings:TIntArray;blankDuplicates:Int64):TInt64Array;
var
  galaxyNo,nextGalaxyNo:integer;
  blankCrossings:TPoint;
  xDistance,yDistance,distance:int64;
begin
  result:=TInt64Array.create;
  for galaxyno:= 0 to pred(pred(galaxies.size)) do //No need to check last galaxy
    for nextGalaxyNo:= galaxyNo+1 to pred(galaxies.size) do
    begin
    blankCrossings:=getBlankCrossings(xCrossings,yCrossings,galaxies[galaxyNo],galaxies[nextGalaxyNo]);
    xDistance:= abs(galaxies[nextGalaxyNo].X - galaxies[galaxyNo].X) + (blankCrossings.X * blankDuplicates);
    yDistance:= abs(galaxies[nextGalaxyNo].Y - galaxies[galaxyNo].Y) + (blankCrossings.y * blankDuplicates);
    distance:= xDistance+yDistance;
    result.push(distance);
    end;
end;

end.


