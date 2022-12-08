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
    procedure checkSingleTreeView(x,y:integer);
    procedure checkTreeViews;
    function getAllTreesWithView:integer;
  public
    constructor Create(filename: string; paintbox_: TPaintbox = nil);
    procedure runPartOne; override;
    procedure runPartTwo; override;
    property map:T3DIntMap read fTreeMap;
  end;
implementation

{ TDayEight }

procedure TDayEight.initializeMap;
var
  x,y:integer;
begin
  setLength(fTreeMap,puzzleInputLines[0].Length,puzzleInputLines.size,2);
  for x:=0 to pred(puzzleInputLines[0].Length)  do
    for y:=0 to pred(puzzleInputLines.size) do
    begin
      map[x][y][0]:=puzzleInputLines[y].Substring(x,1).ToInteger;
      map[x][y][1]:=0; //no view initially
    end;
end;

procedure TDayEight.checkSingleTreeView(x,y:integer);
var
  thisTreeHeight,checkTreeHeight,xIndex,yIndex:integer;
  viewInThisDirection:boolean;
begin
  thisTreeHeight:=map[x][y][0];
  //from this point is there any path (nsew) where all trees ahead are smaller
  if (x=0) or (y=0) then //if we're on an edge there's always a view
    begin
      results.Add('tree '+x.ToString+':'+y.ToString+' has view');
      map[x][y][1]:=1;
      exit;
    end;
  //get the distances from each edge
  //TODO arrange with shortest first
  //For the moment just do them in order
  viewInThisDirection:=true;
  for xIndex:=(x-1) downTo 0 do
    begin
      checkTreeHeight:= map[xIndex][y][0];
    if (checkTreeHeight >= thisTreeHeight) then viewInThisDirection:=false;
    end;
  if viewInThisDirection then
    begin
      map[x][y][1]:=1;
      exit;
    end;
  viewInThisDirection:=true;
  for xIndex:=(x+1) to pred(length(map)) do
    begin
    checkTreeHeight:= map[xIndex][y][0];
    if (checkTreeHeight >= thisTreeHeight) then viewInThisDirection:=false;
    end;
  if viewInThisDirection then
    begin
      map[x][y][1]:=1;
      exit;
    end;
  viewInThisDirection:=true;
  for yIndex:=(y-1) downto 0 do
    begin
    checkTreeHeight:= map[x][yIndex][0];
    if (checkTreeHeight >= thisTreeHeight) then viewInThisDirection:=false;
    end;
  if viewInThisDirection then
    begin
      map[x][y][1]:=1;
      exit;
    end;
  viewInThisDirection:=true;
  for yIndex:= (y+1) to pred(length(map[x])) do
    begin
    checkTreeHeight:= map[x][yIndex][0];
    if (checkTreeHeight >= thisTreeHeight) then viewInThisDirection:=false;
    end;
  if viewInThisDirection then
    begin
      map[x][y][1]:=1;
      exit;
    end;
end;

procedure TDayEight.checkTreeViews;
var
  x,y:integer;
begin
  //go through all trees and check if there is a view
  for x:= 0 to pred(length(map)) do
    for y:= 0 to pred(length(map[x])) do
      checkSingleTreeView(x,y);
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

constructor TDayEight.Create(filename: string; paintbox_: TPaintbox);
begin
  inherited Create(filename,'Day 8',paintbox_);
  fTreeMap:=T3DIntMap.Create;
end;

procedure TDayEight.runPartOne;
begin
  //for each tree in the map we want to see if there is any path left/right/up/down
  //where all the trees in front of it are smaller
  initializeMap;
  checkTreeViews;
  results.add('visible trees '+getAllTreesWithView.toString);
end;

procedure TDayEight.runPartTwo;
begin

end;

end.

