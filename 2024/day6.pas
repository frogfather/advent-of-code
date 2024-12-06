unit day6;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDaySix}
  TDaySix = class(TAocPuzzle)
  private
  procedure loadMap;
  procedure findAndSetGuardPos;
  procedure turnGuardRight;
  procedure moveGuard;
  procedure markVisited;
  function inBounds:boolean;
  function nextSpaceIsClear:boolean;
  function alreadyVisited:boolean;
  function guardDirectionValue:integer;
  function getUniqueVisitedTotal:integer;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

var
  visited:TIntPointMap;//keep track of direction we visited from
  map: TStringArray;
  guardDirection,guardPos:TPoint;

{ TDaySix }

procedure TDaySix.loadMap;
var
  index:integer;
begin
  for index:=0 to pred(puzzleinputLines.size) do
    map.push(puzzleInputLines[index]);
end;

procedure TDaySix.findAndSetGuardPos;
var
  index,gd:integer;
  guardDirections:TStringArray;
  guardIndex:integer;
  guardSymbol:string;
begin
  guardDirections:=TStringArray.create('^','>','<','v');
  for index:= 0 to pred(map.size) do
    for gd:=0 to pred(guardDirections.size) do
      begin
      guardIndex:=map[index].indexOf(guardDirections[gd]);
      if (guardIndex > -1) then
        begin
        //direction depends on what symbol the guard is
        guardSymbol:=map[index].Substring(guardIndex,1);
          case guardSymbol of
          '^':
            begin
            guardDirection.X:=0;
            guardDirection.Y:=-1;
            end;
          '>':
            begin
            guardDirection.X:=1;
            guardDirection.Y:=0;
            end;
          '<':
            begin
            guardDirection.X:=-1;
            guardDirection.Y:=0;
            end;
          'v':
            begin
            guardDirection.X:=0;
            guardDirection.Y:=1;
            end;
          end;
        guardPos.X:=guardIndex;
        guardPos.Y:=index;
        end;
      end;
end;
function TDaySix.inBounds: boolean;
var
  nextPosition:TPoint;
begin
  result:=false;
  nextPosition:=TPoint.Create(guardPos.X + guardDirection.X,guardPos.Y + guardDirection.Y);
  if (nextPosition.X < 0) or (nextPosition.Y < 0)
  or (nextPosition.Y > pred(map.size))
  or (nextPosition.X > pred(map[nextPosition.Y].Length)) then exit;
  result:=true;
end;

function TDaySix.nextSpaceIsClear: boolean;
begin
  result:=map[guardPos.Y+guardDirection.Y].Substring(guardPos.X+guardDirection.X,1) <> '#';
end;

procedure TDaySix.markVisited;
var
  updatedPoints:TPointArray;
begin
  if alreadyVisited then exit;
  visited.TryGetData(guardDirectionValue,updatedPoints);
  if updatedPoints.indexOf(guardPos) = -1
     then updatedPoints.push(guardPos);
  visited.AddOrSetData(guardDirectionValue, updatedPoints);
end;

function TDaySix.alreadyVisited: boolean;
var
  pointsInThisDirection:TPointArray;
begin
  visited.TryGetData(guardDirectionValue,pointsInThisDirection);
  result:= (pointsInThisDirection <> nil) and (pointsInThisDirection.indexOf(guardPos) > -1);
end;

function TDaySix.guardDirectionValue: integer;
begin
  //Guard directions: N = 0, E = 1, S = 2, W = 3;
  if (guardDirection.X = 0)and(guardDirection.Y = -1)
     then result:=0
  else
  if (guardDirection.X = 1)and(guardDirection.Y = 0)
     then result:=1
  else
  if (guardDirection.X = 0)and(guardDirection.Y = 1)
     then result:=2
  else
  if (guardDirection.X = -1)and(guardDirection.Y = 0)
     then result:=3
end;

function TDaySix.getUniqueVisitedTotal: integer;
var
  keyIndex,dataIndex:integer;
  visitedInThisDirection:TPointArray;
  uniqueVisited:TPointArray;
begin
  result:=0;
  uniqueVisited:=TPointArray.create;
  for keyIndex:=0 to pred(visited.KeySize) do
    begin
    visited.TryGetData(visited.Keys[keyIndex],visitedInThisDirection);
    if (visitedInThisDirection <> nil)
       then for dataIndex:=0 to pred(visitedInThisDirection.size) do
         begin
         if (uniqueVisited.indexOf(visitedInThisDirection[dataIndex]) = -1)
           then uniqueVisited.push(visitedInThisDirection[dataIndex]);
         end;
    end;
  result:=uniqueVisited.size;
end;

procedure TDaySix.turnGuardRight;
var
  newDirection:TPoint;
begin
  if (guardDirection.X = 0)and(guardDirection.Y = -1)
     then newDirection:=TPoint.Create(1,0)
  else
  if (guardDirection.X = 1)and(guardDirection.Y = 0)
     then newDirection:=TPoint.Create(0,1)
  else
  if (guardDirection.X = 0)and(guardDirection.Y = 1)
     then newDirection:=TPoint.Create(-1,0)
  else
  if (guardDirection.X = -1)and(guardDirection.Y = 0)
     then newDirection:=TPoint.Create(0,-1);
  guardDirection:=newDirection;
end;

procedure TDaySix.moveGuard;
begin
  guardPos.X:= guardPos.X+guardDirection.X;
  guardPos.Y:= guardPos.Y+guardDirection.Y;
end;

constructor TDaySix.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 6',paintbox_);
visited:=TIntPointMap.create;
map:=TStringArray.create;
guardDirection:=TPoint.create(0,0);
guardPos:=TPoint.Create(0,0);
//parent loads the file as a string and converts to string array;
end;

procedure TDaySix.runPartOne;
begin

  results.Clear;
  loadMap;
  findAndSetGuardPos;
  while inBounds do
    begin
    markVisited;
    while not nextSpaceIsClear do
      begin
      turnGuardRight;
      if alreadyVisited then exit; //Hopefully won't happen here
      markVisited;
      end;
    moveGuard;
    end;
  //catch the last position before we went out of bounds;
  markVisited;
  results.add(getUniqueVisitedTotal.toString);
end;

procedure TDaySix.runPartTwo;
begin
  results.Clear;
end;


end.

                
