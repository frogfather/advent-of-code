unit day9;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, aocPuzzle, LazLogger, ExtCtrls, Graphics, arrayUtils;
type

{ TDayNine }

TDayNine = class(TAocPuzzle)
  private
    fHead:TPoint;
    fTail:TPoint;
    fGrid:T3DIntMap;
    procedure setupGrid;
    procedure runSimulation;
    procedure moveSingleStep(direction:string);
    procedure updateTail;
    function visitedPoints:integer;
  public
    constructor Create(filename: string; paintbox_: TPaintbox = nil);
    procedure runPartOne; override;
    procedure runPartTwo; override;
    property head:TPoint read fHead write fHead;
    property tail:TPoint read fTail write fTail;
    property map:T3DIntMap read fGrid;
  end;
implementation

{ TDayNine }

constructor TDayNine.Create(filename: string; paintbox_: TPaintbox);
begin
  inherited Create(filename,'Day 9',paintbox_);
  fGrid:=T3DIntMap.create;
end;

procedure TDayNine.setupGrid;
var
  headPos,headMax,headMin:TPoint;
  Index:integer;
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
  fhead.X:=(headMin.X * -1);
  fhead.Y:=(headMin.Y * -1);
  tail:=head; //check these aren't reference types - fairly sure they aren't
end;

procedure TDayNine.runSimulation;
var
  index:integer;
  instruction:TStringArray;
  direction:String;
  steps,stepCount:integer;
begin
  map[head.X][head.Y][0]:=1; //head position
  map[tail.X][tail.Y][1]:=1; //mark tail position
  for index:=0 to pred(puzzleInputLines.size) do
    begin
    results.add('instruction: '+puzzleInputLines[index]);
    instruction:=puzzleInputLines[index].Split(' ');
    direction:=instruction[0];
    steps:=instruction[1].ToInteger;
    for stepCount:= 0 to pred(steps) do
      begin
      moveSingleStep(direction);
      updateTail;
      end;
    end;
end;

procedure TDayNine.moveSingleStep(direction: string);
begin
  case direction of
    'U': fHead.Y:=fHead.Y+1;
    'D': fHead.Y:=fHead.Y-1;
    'L': fHead.X:=fHead.X-1;
    'R': fHead.X:=fHead.X+1;
  end;
  map[head.X][head.Y][0]:=1;
  results.add('head is now '+head.X.ToString+':'+head.Y.ToString);
end;

procedure TDayNine.updateTail;
var
  xDisp,yDisp:integer;
  moveX,moveY:integer;
begin
  moveX:=0;
  moveY:=0;
  xDisp:=head.X - tail.X;
  yDisp:=head.Y - tail.Y;
  if (abs(xDisp) <= 1) and (abs(yDisp) <= 1) then exit; //tail is close enough

  if (abs(xDisp) > 0) and (abs(yDisp) > 0) then
    begin
    if (abs(xDisp) < abs(yDisp)) then
      begin
      moveX:=xDisp;
      xDisp:=0;
      end else
      begin
      moveY:=yDisp;
      yDisp:=0;
      end;
    end;

  if (xDisp = 0) then
    begin
    moveY:= head.Y - tail.Y;
    if moveY < 0 then moveY:=moveY+1 else moveY:=moveY-1;
    end
  else if (yDisp = 0) then
    begin
    moveX:= head.X - tail.X;
    if moveX < 0 then moveX:=moveX+1 else moveX:= moveX-1;
    end;
  //now update the map with the new tail position
  fTail.X:=fTail.X + moveX;
  fTail.Y:=fTail.Y + moveY;
  map[tail.X][tail.Y][1]:=1;
  results.add('tail is now '+tail.X.ToString+':'+tail.Y.ToString)
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
  setUpGrid;
  runSimulation;
  results.add('VisitedPoints '+visitedPoints.ToString);
end;

procedure TDayNine.runPartTwo;
begin

end;

end.

