unit day9;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, aocPuzzle, LazLogger, ExtCtrls, Graphics, arrayUtils;
type

{ TDayNine }

TDayNine = class(TAocPuzzle)
  private
    fKnots:TPointArray;
    fGrid:T3DIntMap;
    procedure setupGrid(knots:integer);
    procedure runSimulation;
    procedure moveSingleStep(direction:string);
    procedure updateKnot(knotIndex:integer);
    function visitedPoints:integer;
  public
    constructor Create(filename: string; paintbox_: TPaintbox = nil);
    procedure runPartOne; override;
    procedure runPartTwo; override;
    property map:T3DIntMap read fGrid;
  end;
implementation

{ TDayNine }

constructor TDayNine.Create(filename: string; paintbox_: TPaintbox);
begin
  inherited Create(filename,'Day 9',paintbox_);
  fGrid:=T3DIntMap.create;
end;

procedure TDayNine.setupGrid(knots:integer);
var
  headPos,headMax,headMin:TPoint;
  Index,knotIndex:integer;
  instruction:TStringArray;
begin
  //lets find out how big a map we need
  headPos.X:=0;
  headPos.Y:=0;
  headMin.X:=0;
  headMin.Y:=0;
  headMax.X:=0;
  headMax.Y:=0;
  for index:=0 to pred(puzzleInputLines.size) do
    begin
    instruction:=puzzleInputLines[index].Split(' ');
    case instruction[0] of
      'U':headPos.Y:=headPos.Y + instruction[1].tointeger;
      'D':headPos.Y:=headPos.Y - instruction[1].tointeger;
      'L':headPos.X:=headPos.X - instruction[1].tointeger;
      'R':headPos.X:=headPos.X + instruction[1].tointeger;
    end;
    if headPos.Y > headMax.Y then headMax.Y:=headPos.Y;
    if headPos.Y < headMin.Y then headMin.Y:=headPos.Y;
    if headPos.X < headMin.X then headMin.X:=headPos.X;
    if headPos.X > headMax.X then headMax.X:=headPos.X;
  end;
  setLength(fGrid,(headMax.X - headMin.X)+1,(headMax.Y - headMin.Y)+1,2);
  results.add('Grid is '+length(fgrid).toString+':'+length(fGrid[0]).toString);
  //update for part two instead of using fTail and fHead we use
  //an array of points
  for knotIndex:=0 to pred(knots) do
    fKnots.push(TPoint.Create((headMin.X * -1),(headMin.Y * -1)));
end;

procedure TDayNine.runSimulation;
var
  index:integer;
  instruction:TStringArray;
  direction:String;
  steps,stepCount,knotNumber:integer;
  tailKnot:TPoint;
begin
  tailKnot:=fknots[pred(fknots.size)];
  map[tailKnot.X][tailKnot.Y][1]:=1; //mark initial tail position
  for index:=0 to pred(puzzleInputLines.size) do
    begin
    instruction:=puzzleInputLines[index].Split(' ');
    direction:=instruction[0];
    steps:=instruction[1].ToInteger;
    for stepCount:= 0 to pred(steps) do
      begin
      moveSingleStep(direction);
      for knotNumber:= 1 to pred(fKnots.size) do updateKnot(knotNumber);
      end;
    end;
end;

procedure TDayNine.moveSingleStep(direction: string);
begin
  case direction of
    'U': fKnots[0].Y:=fKnots[0].Y+1;
    'D': fKnots[0].Y:=fKnots[0].Y-1;
    'L': fKnots[0].X:=fKnots[0].X-1;
    'R': fKnots[0].X:=fKnots[0].X+1;
  end;
end;

procedure TDayNine.updateKnot(knotIndex:integer);
var
  xDisp,yDisp:integer;
  moveX,moveY:integer;
begin
  moveX:=0;
  moveY:=0;
  xDisp:=fKnots[knotIndex-1].X - fKnots[knotIndex].X;
  yDisp:=fKnots[knotIndex-1].Y - fKnots[knotIndex].Y;
  if (abs(xDisp) <= 1) and (abs(yDisp) <= 1) then exit; //knot is close enough

  if (abs(xDisp) > 0) and (abs(yDisp) > 0) then
    begin
    if (abs(xDisp) < abs(yDisp)) then
      begin
      moveX:=xDisp;
      xDisp:=0;
      end
    else if (abs(xDisp) > abs(yDisp)) then
      begin
      moveY:=yDisp;
      yDisp:=0;
      end
    else //special case where the offset is the same (2,2)
      begin
      if xDisp > 1 then moveX:=1 else moveX:=-1;
      if yDisp > 1 then moveY:=1 else moveY:=-1;
      xDisp:=0;
      yDisp:=0;
      end;
    end;

  if (xDisp = 0) then
    begin
    moveY:= fKnots[knotIndex - 1].Y - fKnots[knotIndex].Y;
    if moveY < 0 then moveY:=moveY+1 else moveY:=moveY-1;
    end
  else if (yDisp = 0) then
    begin
    moveX:= fKnots[knotIndex - 1].X - fKnots[knotIndex].X;
    if moveX < 0 then moveX:=moveX+1 else moveX:= moveX-1;
    end;
  //if this is the last knot in the array update the map with the new position
  fKnots[knotIndex].X:=fKnots[knotIndex].X + moveX;
  fKnots[knotIndex].Y:=fKnots[knotIndex].Y + moveY;
  if (knotIndex = pred(fKnots.size)) then
    begin
      map[fKnots[knotIndex].X][fKnots[knotIndex].Y][1]:=1;
    end;
end;

function TDayNine.visitedPoints: integer;
var
  x,y:integer;
begin
  result:=0;
  for x:=0 to pred(length(map)) do
    for y:=0 to pred(length(map[0])) do
    if (map[x][y][1] = 1) then result:= result + 1;
end;

procedure TDayNine.runPartOne;
begin
  setUpGrid(2);
  runSimulation;
  results.add('VisitedPoints '+visitedPoints.ToString);
end;

procedure TDayNine.runPartTwo;
begin
  setUpGrid(10);
  runSimulation;
  results.add('VisitedPoints '+visitedPoints.ToString);
end;

end.

