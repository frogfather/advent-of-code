unit day4;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayFour}
  TDayFour = class(TAocPuzzle)
  private
  function getWordInDir(dir:TPoint;x,y:integer; range:integer=4):string;
  function canFormCross(x,y:integer):boolean;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayFour }
var
  grid:TStringArray;
  points:TPointArray;

function TDayFour.getWordInDir(dir:TPoint; x, y: integer; range: integer=4): string;
var
  xIndex,yIndex,count:integer;
begin
  result:='';
  count:=0;
  xIndex:=X;
  yIndex:=Y;
  repeat
  if (YIndex < 0)or(YIndex > pred(grid.size))
  or (XIndex < 0)or(XIndex > pred(grid[YIndex].Length)) then exit;
  result:=result + grid[YIndex].Substring(xIndex,1);
  YIndex:=YIndex + dir.Y;
  XIndex:=XIndex + dir.X;
  count:=count+1;
  until count = range;
end;


function TDayFour.canFormCross(x, y: integer): boolean;
var
  negWord,posWord:String;
begin
result:=false;
negWord:='';
posWord:='';
if (x < 1)or(y < 1) or (y > pred(pred(grid.size))) then exit;
if (x > pred(grid[y-1].Length)) or (x > pred(grid[y+1].Length)) then exit;
negWord:=grid[y-1].Substring(x-1,1)+grid[y].Substring(x,1)+grid[y+1].Substring(x+1,1);
posWord:=grid[y+1].Substring(x-1,1)+grid[y].Substring(x,1)+grid[y-1].Substring(x+1,1);
result:=((negWord = 'MAS')or(negWord = 'SAM'))and((posWord = 'MAS')or(posWord = 'SAM'));
end;

constructor TDayFour.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 4',paintbox_);
grid:=TStringArray.Create;
points:=TPointArray.create;
points.push(TPoint.Create(1,1)); //SE
points.push(TPoint.Create(-1,1));//SW
points.push(TPoint.Create(-1,-1));//NW
points.push(TPoint.Create(1,-1)); //NE
points.push(TPoint.Create(1,0)); //E
points.push(TPoint.Create(0,1)); //S
points.push(TPoint.Create(-1,0));//W
points.push(TPoint.Create(0,-1)); //N
//parent loads the file as a string and converts to string array;
end;

procedure TDayFour.runPartOne;
var
  rowNo,colNo,dirNo:integer;
  sWord:String;
  runningTotal:integer;
begin
  results.Clear;
  runningTotal:=0;
  //from a given point can we form the word XMAS in any direction?
  grid.Clear;
  for rowNo:=0 to pred(puzzleInputLines.size) do
    grid.push(puzzleInputLines[rowNo]);
  //print it out for checking
  for rowNo:=0 to pred(grid.size) do
    for colNo:=0 to pred(grid[rowNo].Length) do
      for dirNo:= 0 to pred(points.size) do
        begin
        sWord:=getWordInDir(points[dirNo],colNo,rowNo);
        if (sWord = 'XMAS') then runningTotal:=runningTotal + 1;
        end;
  results.add('Total '+runningTotal.toString);
end;

procedure TDayFour.runPartTwo;
var
  rowNo,colNo,dirNo:integer;
  sWord:String;
  runningTotal:integer;
begin
  results.Clear;
  runningTotal:=0;
  //from a given point can we form the word XMAS in any direction?
  grid.Clear;
  for rowNo:=0 to pred(puzzleInputLines.size) do
    grid.push(puzzleInputLines[rowNo]);
  //print it out for checking
  for rowNo:=0 to pred(grid.size) do
    for colNo:=0 to pred(grid[rowNo].Length) do
      begin
      if canFormCross(colNo,rowNo) then runningTotal:=runningTotal+1;
      end;
  results.add('Total '+runningTotal.toString);
end;


end.

                
