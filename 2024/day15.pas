unit day15;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayFifteen}
  TDayFifteen = class(TAocPuzzle)
  private
  procedure loadPuzzle;
  procedure runPuzzle;
  function findRobotPos:TPoint;
  function nearestSpaceToRobotInGivenDirection(robotPos:TPoint;direction:String):TPoint;
  procedure moveRobotAndBoxes(robotPos,spacePos,offset:TPoint);
  function getRobotOffset(direction:String):TPoint;
  function inBounds(position:TPoint):boolean;
  function sumBoxCoordinates:int64;
  procedure drawPuzzle;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayFifteen }
var
  map:TStringArray;
  instructions:string;
  robotPosition:TPoint;

procedure TDayFifteen.loadPuzzle;
var
  index:integer;
  onInstruction:boolean;
begin
  onInstruction:=false;
  map.clear;
  instructions:='';
  for index:=0 to pred(puzzleInputLines.size) do
    begin
    if (puzzleInputLines[index].Trim = '') then onInstruction:=true
    else
      begin
      if onInstruction then instructions:=instructions+puzzleInputLines[index]
      else map.push(puzzleInputLines[index]);
      end;
    end;
end;

procedure TDayFifteen.runPuzzle;
var
  index:integer;
  instruction:string;
  nearestSpace:TPoint;
begin
  robotPosition:=findRobotPos;
  for index:=0 to pred(instructions.Length) do
    begin
    instruction:=instructions.Substring(index,1);
    nearestSpace:= nearestSpaceToRobotInGivenDirection(robotPosition,instruction);
    if (nearestSpace.X > -1)and(nearestSpace.Y > -1)
      then moveRobotAndBoxes(robotPosition,nearestSpace,getRobotOffset(instruction));
    end;
end;

function TDayFifteen.findRobotPos: TPoint;
var
  row:integer;
begin
  for row:= 0 to pred(map.size) do
    if (map[row].IndexOf('@') > -1) then
      begin
      result:=TPoint.Create(map[row].indexOf('@'),row);
      exit
      end;
  result:=TPoint.Create(-1,-1);
end;

function TDayFifteen.nearestSpaceToRobotInGivenDirection(robotPos:TPoint;direction: String
  ): TPoint;
var
  robotOffset,currentPos:TPoint;
begin
  result:=TPoint.Create(-1,-1);
  robotOffset:=getRobotOffset(direction);
  currentPos:=robotPosition;
  while inBounds(currentPos) do
    begin
    if (map[currentPos.Y].Substring(currentPos.X,1) = '#') then exit;//can't walk through walls!
    if (map[currentPos.Y].Substring(currentPos.X,1) = '.') then
      begin
      result:=currentPos;
      exit;
      end;
    currentPos.X:=currentPos.X + robotOffset.X;
    currentPos.Y:=currentPos.Y + robotOffset.Y;
    end;
end;

procedure TDayFifteen.moveRobotAndBoxes(robotPos,spacePos,offset:TPoint);
var
  done:boolean;
  currentPos:TPoint;
  reverseOffset:TPoint;
  itemToMove,itemAtPoint:char;
begin
currentPos:=spacePos;
reverseOffset:=TPoint.Create(offset.X * -1, offset.Y * -1);
done:=false;
while not done do
  begin
  //Strings are 1 indexed!!!
  itemToMove:=map[currentPos.Y + reverseOffset.Y][currentPos.X + 1 + reverseOffset.X];
  itemAtPoint:=map[currentPos.Y][currentPos.X + 1];
  map[currentPos.Y][currentPos.X + 1]:=itemToMove;
  if (itemToMove = '@') then robotPosition:=currentPos;
  map[currentPos.Y + reverseOffset.Y][currentPos.X + 1 + reverseOffset.X]:='.';
  currentPos.X:=currentPos.X + reverseOffset.X;
  currentPos.Y:=currentPos.Y + reverseOffset.Y;
  done:= currentPos = robotPos;
  end;
end;

function TDayFifteen.getRobotOffset(direction: String): TPoint;
var
  x,y:integer;
begin
  x:=0;
  y:=0;
  case direction of
  '<': x:=-1;
  '>': x:=1;
  '^': y:=-1;
  'v': y:=1;
  end;
  result:=TPoint.Create(x,y);
end;

function TDayFifteen.inBounds(position: TPoint): boolean;
begin
  result:=(position.X > 0) and (position.X < pred(map[0].Length))
       and(position.Y > 0) and (position.Y < pred(map.size));
end;

function TDayFifteen.sumBoxCoordinates: int64;
var
  row,col:integer;
  rString:string;
begin
  result:=0;
  for row:=0 to pred(map.size) do
    begin
    rString:=map[row];
    for col:=0 to pred(rString.Length) do
      begin
      if (rString.Substring(col,1) = 'O') then result:=result+(row * 100)+col;
      end;
    end;
end;

procedure TDayFifteen.drawPuzzle;
var
  row:integer;
begin
  for row:=0 to pred(map.size) do
    results.add(map[row]);
end;

constructor TDayFifteen.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 15',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayFifteen.runPartOne;
var
  total:int64;
begin
  results.Clear;
  loadPuzzle;
  runPuzzle;
  total:=sumBoxCoordinates;
  results.add('Total is '+total.ToString);
end;

procedure TDayFifteen.runPartTwo;
begin
  results.Clear;
end;


end.

                
