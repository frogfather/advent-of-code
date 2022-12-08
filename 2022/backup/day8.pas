unit day8;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, aocPuzzle, LazLogger, ExtCtrls, Graphics, arrayUtils;
type

{ TDayEight }

TDayEight = class(TAocPuzzle)
  private
    fTreeMap:T3DIntMap;
    procedure initializeMap;
    procedure checkIfThisTreeHasView(x,y:integer);
    procedure checkTrees(scores:boolean=false);
    function getAllTreesWithView:integer;
    procedure findThisTreeScore(x,y:integer);
    function getTreeWithHighestScore:integer;
  public
    constructor Create(filename: string; paintbox_: TPaintbox = nil);
    procedure runPartOne; override;
    procedure runPartTwo; override;
    property map:T3DIntMap read fTreeMap;
  end;
implementation

{ TDayEight }

constructor TDayEight.Create(filename: string; paintbox_: TPaintbox);
begin
  inherited Create(filename,'Day 8',paintbox_);
  fTreeMap:=T3DIntMap.Create;
end;


procedure TDayEight.initializeMap;
var
  x,y:integer;
begin
  setLength(fTreeMap,puzzleInputLines[0].Length,puzzleInputLines.size,2);
  for x:=0 to pred(puzzleInputLines[0].Length)  do
    for y:=0 to pred(puzzleInputLines.size) do
    begin
      map[x][y][0]:=puzzleInputLines[y].Substring(x,1).ToInteger;
      map[x][y][1]:=0; //Part 1: 0 = no view and 1 = view. Part 2: score
    end;
end;

procedure TDayEight.findThisTreeScore(x, y: integer);
var
  thisTreeHeight,thisTreeScore,xIndex,yIndex,dirScore:integer;
begin
 thisTreeHeight:=map[x][y][0];
 thisTreeScore:=1;
 //North
 dirScore:=0;
 if (y > 0) then
   for yIndex:=(y-1) downto 0 do
   begin
    dirScore:=dirScore + 1;
    if (map[x][yIndex][0] >= thisTreeHeight) then break;
    end;
 thisTreeScore:=thisTreeScore * dirScore;
 dirScore:=0;
 //South
 if (y < pred(length(map[x]))) then
   for yIndex:= (y+1) to pred(length(map[x])) do
   begin
    dirScore:=dirScore + 1;
    if (map[x][yIndex][0] >= thisTreeHeight) then break;
    end;
 thisTreeScore:=thisTreeScore * dirScore;
 dirScore:=0;
 //West
 if (x > 0) then
   for xIndex:=(x-1) downto 0 do
   begin
    dirScore:=dirScore + 1;
    if (map[xIndex][y][0] >= thisTreeHeight) then break;
    end;
 thisTreeScore:=thisTreeScore * dirScore;
 dirScore:=0;
 //East
 if (x < pred(length(map))) then
   for xIndex:= (x+1) to pred(length(map)) do
   begin
    dirScore:=dirScore + 1;
    if (map[xIndex][y][0] >= thisTreeHeight) then break;
    end;
 thisTreeScore:=thisTreeScore * dirScore;
 map[x][y][1]:=thisTreeScore;
end;

procedure TDayEight.checkIfThisTreeHasView(x,y:integer);
var
  thisTreeHeight,checkTreeHeight,xIndex,yIndex:integer;
begin
  thisTreeHeight:=map[x][y][0];
  if (x=0) or (y=0) then
    begin
      map[x][y][1]:=1;
      exit;
    end;
  If map.max(0,x-1,y,y,0,0) < thisTreeHeight then
    begin
      map[x][y][1]:=1;
      exit;
    end;
  if map.max(x+1,pred(length(map)),y,y,0,0) < thisTreeHeight then
    begin
      map[x][y][1]:=1;
      exit;
    end;
  if map.max(x,x,0,y-1,0,0) < thisTreeHeight then
    begin
      map[x][y][1]:=1;
      exit;
    end;
  if map.max(x,x,y+1,pred(Length(map[x])),0,0) < thisTreeHeight then
    begin
      map[x][y][1]:=1;
      exit;
    end;
end;

procedure TDayEight.checkTrees(scores:boolean);
var
  x,y:integer;
begin
  //go through all trees and check if there is a view
  for x:= 0 to pred(length(map)) do
    for y:= 0 to pred(length(map[x])) do
      begin
      if scores then findThisTreeScore(x,y)
        else checkIfThisTreeHasView(x,y);
      end;
end;

function TDayEight.getAllTreesWithView: integer;
var
  visibleTrees,x,y:integer;
begin
  //Go through the array and return all items where 3rd dimension element is 1
  visibleTrees:=0;
  for x:=0 to pred(length(map)) do
    for y:=0 to pred(length(map[x])) do
      if (map[x][y][1] = 1) then visibleTrees:=visibleTrees+1;
  result:=visibleTrees;
end;

function TDayEight.getTreeWithHighestScore: integer;
var
  x,y:integer;
begin
  result:=0;
  for x:=0 to pred(length(map)) do
    for y:=0 to pred(length(map[x])) do
      if (map[x][y][1] > result) then result:= map[x][y][1];
end;

procedure TDayEight.runPartOne;
begin
  //for each tree in the map we want to see if there is any path left/right/up/down
  //where all the trees in front of it are smaller
  initializeMap;
  checkTrees;
  results.add('visible trees '+getAllTreesWithView.toString);
end;

procedure TDayEight.runPartTwo;
begin
  initializeMap;
  checkTrees(true);
  results.Add('Tree with highest score '+getTreeWithHighestScore.toString);
end;

end.

