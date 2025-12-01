unit day15;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayFifteen}
  TDayFifteen = class(TAocPuzzle)
  private
  procedure loadPuzzle(larger:boolean=false);
  procedure runPuzzle;
  procedure runLargePuzzle;
  function convertRow(row:string;larger:boolean=false):string;
  function findRobotPos:TPoint;
  function nearestSpaceToRobotInGivenDirection(robotPos:TPoint;direction:String):TPoint;
  procedure moveRobotAndBoxes(robotPos,spacePos,offset:TPoint);
  function getRobotOffset(direction:String):TPoint;
  function inBounds(position:TPoint):boolean;
  function inBounds(row,col:integer):boolean;
  function sumBoxCoordinates:int64;
  function findItemsToMove(robotPos,robotOffset:TPoint):TPointArray;
  function moveItems(itemsToMove:TPointArray;offset:TPoint):integer;
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

//methods common to both parts

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

function TDayFifteen.inBounds(row, col: integer): boolean;
begin
  result:=inBounds(TPoint.create(col,row));
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
      if ((rString.Substring(col,1) = 'O')or(rString.Substring(col,1) = '[')) then result:=result+(row * 100)+col;
      end;
    end;
end;

function TDayFifteen.findItemsToMove(robotPos, robotOffset: TPoint
  ): TPointArray;
var
  done:boolean;
  colLeft,colRight,col,row,index:integer;
  currentRowData:string;
begin
  //start at the robot.
  //for each item, if it has a partial box above we need to add the other half
  result:=TPointArray.create;
  done:=false;
  row:=robotPos.Y;
  col:=robotPos.X;
  colLeft:=robotPos.X;  //Only used for vertical
  colRight:=robotPos.X;
  row:=robotPos.Y;
  while not done do
    begin
    currentRowData:=map[row].Substring(colLeft,(colRight - colLeft)+1);
    if (robotOffset.X = 0) then
      begin
      if (currentRowData.Substring(0,1) = ']')then colLeft:=colLeft - 1;
      if (currentRowData.Substring(pred(currentRowData.Length),1) = '[') then colRight:=colRight+1;
      //we're only interested in blocks
      //adjust limits
      while (currentRowData.Length > 0)and(currentRowData.substring(0,1) = '.')do
        begin
        currentRowData:=currentRowData.Substring(1);
        colLeft:=colLeft+1;
        end;
      while (currentRowData.length > 0) and (currentRowData.Substring(pred(currentRowData.Length)-1,1) = '.')do
        begin
        currentRowData:=currentRowData.substring(0,pred(currentRowData.Length)-1);
        colRight:=colRight-1;
        end;
      end;
    //If the current row is clear we can move
    if (currentRowData.Replace('.','').Length = 0) then exit;
    if (currentRowData.IndexOf('#')> -1) then
      begin
      result:=TPointArray.create;
      exit;
      end else
      begin
      //Add the current item to the list and move in the required direction
      for index:=colLeft to colRight do
        result.push(TPoint.Create(index,row));
      row:=row+robotOffset.Y;
      col:=col+robotOffset.X;
      colLeft:=colLeft+robotOffset.X;
      colRight:=colRight+robotOffset.X;
      if not inBounds(row,col) then
        begin
        result:=TPointArray.create;
        exit;
        end;
      end;
    end;
end;

function TDayFifteen.moveItems(itemsToMove:TPointArray;offset:TPoint): integer;
var
  index:integer;
  itemToMove:Char;
begin
  //move each item on the map
  for index:= pred(itemsToMove.size) downto 0 do
    begin
    itemToMove:=map[itemsToMove[index].Y][itemsToMove[index].X + 1];
    map[itemsToMove[index].Y + offset.Y][itemsToMove[index].X +offset.X + 1]:=itemToMove;
    map[itemsToMove[index].Y][itemsToMove[index].X + 1]:='.';
    if (itemToMove = '@') then
       begin
       robotPosition:=TPoint.Create(itemsToMove[index].X +offset.X,itemsToMove[index].Y + offset.Y );
       end;
    end;
  result:=itemsToMove.size;
end;

procedure TDayFifteen.drawPuzzle;
var
  row:integer;
begin
  for row:=0 to pred(map.size) do
    results.add(map[row]);

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

procedure TDayFifteen.loadPuzzle(larger:boolean=false);
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
      else map.push(convertRow(puzzleInputLines[index],larger));
      end;
    end;
end;

//Part one methods
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

//Part two methods

procedure TDayFifteen.runLargePuzzle;
var
  index:integer;
  itemsToMove:TPointArray;
  robotOffset:TPoint;
  instruction:String;
begin
  robotPosition:=findRobotPos;
  //start at the robot and move in the direction indicated
  for index:=0 to pred(instructions.Length) do
    begin
    instruction:=instructions.substring(index,1);
    robotOffset:=getRobotOffset(instruction);
    itemsToMove:=findItemsToMove(robotPosition,robotOffset);
    moveItems(itemsToMove,robotOffset);
    results.Add((index+1).toString+' '+instruction);
    drawPuzzle;
    end;

end;

function TDayFifteen.convertRow(row: string; larger: boolean): string;
var
  element:integer;
begin
  result:='';
  if not larger then result:=row
  else for element:=0 to pred(row.Length) do
    begin
      case row.Substring(element,1) of
      '#': result:=result + '##';
      'O': result:=result + '[]';
      '.': result:=result + '..';
      '@': result:=result + '@.';
      end;
    end;
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
var
  total:int64;
begin
  results.Clear;
  loadPuzzle(true);
  runLargePuzzle;
  total:= sumBoxCoordinates;
  results.add('Total is '+total.ToString);
end;


end.

                
