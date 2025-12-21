unit day9;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayNine}
  TDayNine = class(TAocPuzzle)
  private
  procedure loadTiles;
  function getLargestArea:int64;
  function getSortedCoords(arr:TPointArray;xs,ys:TIntArray;index:integer):TRect;
  function getSortedCoords(arr:T2DIntMap;xs,ys:TIntArray;x1,y1,x2,y2:integer):TRect;
  function getXs:TIntArray;
  function getYs:TIntArray;
  function createEmptyGrid(x,y:integer):T2DIntMap;
  function createCompressedGrid:T2DIntMap;
  function floodFillGrid(grid:T2DIntMap):T2DIntMap;
  function constructPsa(var grid:T2DIntMap):T2DIntMap;
  function validPoints(psa:T2DIntMap;x1,x2,y1,y2:integer):boolean;
  procedure showGrid(grid:T2DIntMap);
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

var
  redTiles:TPointArray;
{ TDayNine }

procedure TDayNine.loadTiles;
var
  index:integer;
begin
  for index:=0 to pred(puzzleInputlines.size)do
    redTiles.push(TPoint.Create(puzzleInputLines[index].Split(',')[0].ToInteger,puzzleInputLines[index].Split(',')[1].ToInteger));
end;

function TDayNine.getLargestArea: int64;
var
  index1,index2:integer;
  area:int64;
begin
  result:=0;
  for index1:=0 to pred(pred(redTiles.size))do
    for index2:=index1+1 to pred(redTiles.size)do
      begin
      area:=(abs(redTiles[index1].X - redTiles[index2].X)+1)*(abs(redTiles[index1].Y - redTiles[index2].Y)+1);
      if (area > result) then
        begin
        result := area;
        results.add('largest area is now '+result.toString);
        end;
      end;
end;

function TDayNine.getSortedCoords(arr: TPointArray;xs,ys:TIntArray; index: integer): TRect;
var
  startX,startY,endX,endY:integer;
begin
startX:=xs.indexOf(arr[index].X) * 2;
endX:= xs.indexOf(arr[index+1].X) * 2;
if StartX > EndX then
  begin
  result.Left:=EndX;
  result.Right:=StartX;
  end else
  begin
  result.Left:=StartX;
  result.Right:=EndX;
  end;

startY:=ys.indexOf(arr[index].Y) * 2;
endY:=ys.indexOf(arr[index+1].Y) * 2;
if StartY > EndY then
  begin
  result.Top:=EndY;
  result.Bottom:=StartY;
  end else
  begin
  result.Top:=StartY;
  result.Bottom:=EndY;
  end;
end;

function TDayNine.getSortedCoords(arr: T2DIntMap; xs, ys: TIntArray;x1,y1,x2,y2:integer): TRect;
var
  startX,startY,endX,endY:integer;
begin
  startX:=xs.indexOf(x1)*2;
  endX:= xs.indexOf(x2)*2;

  if StartX > EndX then
  begin
  result.Left:=EndX;
  result.Right:=StartX;
  end else
  begin
  result.Left:=StartX;
  result.Right:=EndX;
  end;

  startY:=ys.indexOf(y1)*2;
  endY:=ys.indexOf(y2)*2;

  if StartY > EndY then
  begin
  result.Top:=EndY;
  result.Bottom:=StartY;
  end else
  begin
  result.Top:=StartY;
  result.Bottom:=EndY;
  end;
end;

function TDayNine.getXs: TIntArray;
var
  index:integer;
begin
  //Very slow!
  result:=TIntArray.create;
  for index:= 0 to pred(redTiles.size) do
    if(result.indexOf(redTiles[index].X) = -1) then result.push(redTiles[index].X);
  sort(result,result.size);
end;

function TDayNine.getYs: TIntArray;
var
  index:integer;
begin
  result:=TIntArray.create;
  for index:= 0 to pred(redTiles.size) do
    if(result.indexOf(redTiles[index].Y) = -1) then result.push(redTiles[index].Y);
  sort(result,result.size);
end;

function TDayNine.createEmptyGrid(x, y: integer): T2DIntMap;
var
  xindex,yindex:integer;
begin
  result:=T2DIntMap.create;
  for yIndex:=0 to pred(y) do
    for xindex:=0 to pred(x) do
      result.push(yIndex,0);
end;

//Entirely based on Hyperneutrino's solution and keeping her naming
//although the sizes are arrays here
function TDayNine.createCompressedGrid: T2DIntMap;
var
  allPoints:TPointArray;
  xs,ys:TIntArray;
  coords:TRect;
  cx1,cx2,cy1,cy2:integer;
  xSizes,ySizes:TIntArray;
  index,indexX,indexY:integer;
begin
  allPoints:=redTiles;
  allPoints.push(redTiles[0]);
  xs:=getXs;
  ys:=getYs;
  xSizes:=TIntArray.create;
  ySizes:=TIntArray.create;
  //xs is a list of the x coordinates of the red tiles in ascending order
  //ys is a list of the y coordinates of the red tiles in ascending order
  result:=createEmptyGrid((xs.size * 2) -1,(ys.size * 2) - 1);
  for index:= 0 to pred(pred(xs.size)) do
    xSizes.push(xs[index+1]-xs[index]-1);
  for index:= 0 to pred(pred(ys.size)) do
    ySizes.push(ys[index+1]-ys[index]-1);
  //xSizes and ySizes are the numbers of green tiles between each red tile in each direction
  for index:=0 to pred(pred(allPoints.size)) do
    begin
    coords:=getSortedCoords(allPoints,xs,ys,index);
    cx1:=coords.Left;
    cx2:=coords.Right;
    cy1:=coords.Top;
    cy2:=coords.Bottom;
    //For each pair of coordinates
    for indexX:=cx1 to cx2 do
      for indexY:= cy1 to cy2 do
        begin
        if result.inRange(indexY,indexX) then
          result[indexX][indexY]:=1;
        end;
    end;
end;

function TDayNine.floodFillGrid(grid: T2DIntMap): T2DIntMap;
var
  queue:TPointArray;
  Outside:TIntPointMap;
  currentPoint,testPoint:TPoint;
  xIndex,yIndex:integer;
begin
  result:=grid;
  outside:=TIntPointMap.Create;
  outside.sorted:=true;
  queue:=TPointArray.create;
  outside.addItem(TPoint.create(-1,-1));
  queue.push(TPoint.create(-1,-1));
  while queue.size > 0 do
    begin
    currentPoint:=queue.popLeft;
    for xIndex:=currentPoint.X - 1 to currentPoint.X + 1 do
      for yIndex:=currentPoint.Y - 1 to currentPoint.Y +1 do
        begin
        testPoint.X:=xIndex;
        testPoint.Y:=yIndex;
        if (xIndex < -1)or(yIndex < -1)or(xIndex > result.rows)or(yIndex > result.size(yIndex))then continue;
        if (result.inRange(yIndex,xIndex)) and (result[yIndex][xIndex] = 1) then continue;
        if outside.included(testPoint) then continue;
        outside.addItem(testPoint);
        queue.push(testPoint);
        end;
    end;
  //Now fill everything that isn't in the queue with 1s
  for yIndex:=0 to pred(result.rows)do
    for xIndex:=0 to pred(result.size(yIndex)) do
      begin
      currentPoint.X:=xIndex;
      currentPoint.Y:=yIndex;
      if not outside.included(currentPoint) then
        grid[yIndex][xIndex]:=1;
      end;
end;

function TDayNine.constructPsa(var grid: T2DIntMap): T2DIntMap;
var
  xIndex,yIndex:integer;
  left,top,topLeft:integer;
begin
  result:=T2DIntMap.create;
  setLength(result,grid.rows,grid.size(0));
  //Set it to all zeros
  for yIndex:= 0 to pred(grid.rows) do
    for xIndex:=0 to pred(grid.size(yIndex)) do
      result[yIndex][xIndex]:=0;
  for yIndex:= 0 to pred(result.rows) do
    for xIndex:=0 to pred(result.size(yIndex)) do
      begin
      if (result.inRange(yIndex, xIndex-1)) then left:=result[yIndex][xIndex-1] else left:=0;
      if (result.inRange(yIndex-1,xIndex)) then top:=result[yIndex-1][xIndex] else top:=0;
      if (result.inRange(yIndex -1,xIndex -1)) then topLeft:=result[yIndex - 1][xIndex - 1] else topLeft:=0;
      result[yIndex][xIndex]:= (left + top + grid[yindex][xIndex]) - topLeft
      end;
end;

function TDayNine.validPoints(psa: T2DIntMap; x1, x2, y1, y2: integer):Boolean;
var
  xs,ys:TIntArray;
  coords:TRect;
  cx1,cx2,cy1,cy2,left,top,topLeft,count:integer;
  isValid:boolean;
begin
  xs:=getXs;
  ys:=getYs;
  coords:=getSortedCoords(psa,xs,ys,x1,y1,x2,y2);
  cx1:=coords.left;
  cx2:=coords.right;
  cy1:=coords.top;
  cy2:=coords.bottom;
  if (cx1 > 0) then left:= psa[cx1-1][cy2] else left:=0;
  if (cy1 > 0) then top:= psa[cx2][cy1-1] else top := 0;
  if (cx1 > 0)and(cy1 > 0) then topLeft:= psa[cx1-1][cy1-1] else topLeft:=0;
  count:= psa[cx2][cy2] - left - top + topLeft;
  isValid:=count = (cx2 - cx1 + 1) * (cy2 - cy1 +1);
  result:=isValid;
end;

procedure TDayNine.showGrid(grid: T2DIntMap);
var
  x,y:integer;
  sOutput:string;
begin
  for y:=0 to pred(grid.rows) do
    begin
    sOutput:='';
    for x:=0 to pred(grid.size(y)) do
      sOutput:=sOutput+grid[y][x].ToString+' ';
    results.Add(sOutput);
    end;
end;

constructor TDayNine.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 9',paintbox_);
//parent loads the file as a string and converts to string array;
redTiles:=TPointArray.create;
end;

procedure TDayNine.runPartOne;
var
  maxArea:Int64;
begin
  results.Clear;
  loadTiles;
  maxArea:=getLargestArea;
  results.add('Max area is '+maxArea.toString);
end;

procedure TDayNine.runPartTwo;
var
  compGrid,psa:T2DIntMap;
  x1,x2:integer;
  area,maxArea:int64;
begin
  results.Clear;
  loadTiles;
  maxArea := 0;
  compGrid:=createCompressedGrid;
  compGrid:=floodFillGrid(compGrid);
  psa:=constructPsa(compGrid);
  for x1:=0 to pred((redTiles.size)) do
    begin
    for x2:=0 to pred(x1) do
      begin
      if (validPoints(psa,redTiles[x1].X,redTiles[x2].X,redTiles[x1].Y,redTiles[x2].Y)) then
        begin
        area:=(abs(redTiles[x1].X - redTiles[x2].X) + 1) * (abs(redTiles[x1].Y - redTiles[x2].Y) + 1);
        if (area > maxArea) then maxArea:=area;
        end;
      end;
    end;
  results.Add('largest area  is '+maxArea.ToString);

end;


end.

                
