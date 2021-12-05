unit ventMap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fileUtilities;

type
  AVentMap = array of array of integer;

  RCoord = record
    start: TPoint;
    finish: TPoint;
  end;

  ACoordArray = array of RCoord;

  { TVentMap }

  TVentMap = class(TInterfacedObject)
    private
      fVentMap: AVentMap;
      fCoords: ACoordArray;
      function getStraightLines(input:ACoordArray):ACoordArray;
      function getStraightAndDiagonalLines(input:ACoordArray):ACoordArray;
      function convertInputToCoordList(input:TStringArray):ACoordArray;
      function coordToPoint(coord:String):TPoint;
      procedure addCoord(var input:ACoordArray;coord:RCoord);
      procedure resetMap;
      function getMaxValue(width:boolean=true):integer;
      function lineIs45Degrees(coord:RCoord):boolean;
      function calculateMove(coord:RCoord):TPoint;
    public
      constructor create(input:TStringArray);
      procedure calculateVents(noDiagonal:boolean=true);
      function getOverlapCount:Integer;
  end;

implementation

{ TVentMap }

function TVentMap.getStraightLines(input: ACoordArray): ACoordArray;
var
  output:ACoordArray;
  index:integer;
begin
  output:=ACoordArray.create;
  for index := 0 to pred(length(input)) do
    begin
    if (input[index].start.X = input[index].finish.X)
      or (input[index].start.Y = input[index].finish.Y)
      then addCoord(output,input[index]);
    end;
  result:=output;
end;

function TVentMap.getStraightAndDiagonalLines(input: ACoordArray): ACoordArray;
var
  output:ACoordArray;
  index:integer;
  currentCoord:RCoord;
begin
  //here we want to include straight lines and lines at 45 degrees
  //we can get the straight lines from the method above
  output:=getStraightLines(input);
  for index := 0 to pred(length(input)) do
    begin
    currentCoord:=input[index];
    if lineIs45Degrees(currentCoord)
      then addCoord(output,currentcoord);
    end;
  result:=output;
end;

function TVentMap.convertInputToCoordList(input: TStringArray): ACoordArray;
var
  lineNumber:Integer;
  coordPair:TStringArray;
  output:ACoordArray;
  coord:RCoord;
begin
  output:=ACoordArray.create;
  for lineNumber:= 0 to pred(length(input)) do
    begin
    coordPair:=fileUtilities.removeBlankLinesFromStringArray(input[lineNumber].Split('->'));
    if (length(coordPair)=2) then
      begin
       coord.start:=coordToPoint(coordPair[0]);
       coord.finish:=coordToPoint(coordPair[1]);
       addCoord(output,coord);
      end;
    end;
  result:=output;
end;

function TVentMap.coordToPoint(coord: String): TPoint;
var
  xy:TStringArray;
  x,y:integer;
begin
  x:=0;
  y:=0;
  xy:=fileUtilities.removeBlankLinesFromStringArray(coord.split(','));
  if (length(xy) = 2) then
    begin
     x:=xy[0].Trim(' ').ToInteger;
     y:=xy[1].Trim(' ').ToInteger;
    end;
  result:=TPoint.Create(x,y);
end;

procedure TVentMap.addCoord(var input: ACoordArray; coord:RCoord);
begin
  setLength(input, length(input)+1);
  input[pred(length(input))]:=coord;
end;

procedure TVentMap.resetMap;
var
  mapWidth,mapHeight,x,y:Integer;
begin
  mapHeight:=length(fVentMap);
  if mapHeight > 0 then mapWidth:=length(fVentMap[0]) else mapWidth:=0;
  for y:=0 to pred(mapHeight) do
    begin
    for x:=0 to pred(mapWidth) do
      begin
      fVentMap[x][y]:=0;
      end;
    end;
end;

function TVentMap.getMaxValue(width: boolean): integer;
var
  index:integer;
  coordValue,maxValue:integer;
  currentCoord: RCoord;
begin
  maxValue:=0;
  for index:=0 to pred(length(fCoords)) do
    begin
    currentCoord:=fCoords[index];
    if width then
      begin
      coordValue:=currentCoord.start.X;
      if currentCoord.finish.X > coordValue
        then coordValue:=currentCoord.finish.X;
      end else
      begin
      coordValue:=currentCoord.start.Y;
      if currentCoord.finish.Y > coordValue
        then coordValue:=currentCoord.finish.Y;
      end;
    if (coordValue > maxValue) then maxValue:=coordValue;
    end;
  result:=maxValue;
end;

function TVentMap.lineIs45Degrees(coord: RCoord): boolean;
begin
  //is the absolute distance between start and finish the same for x and y?
  result:= abs(coord.start.X - coord.finish.X)=abs(coord.start.Y - coord.finish.Y);
end;

function TVentMap.calculateMove(coord: RCoord): TPoint;
var
  intervalX,intervalY:integer;
begin
  result.X:=0;
  result.Y:=0;
  intervalX:=coord.finish.X - coord.start.X;
  intervalY:=coord.finish.Y - coord.start.Y;
  if (abs(intervalX) > 0) then result.X:=intervalX div abs(intervalX);
  if (abs(intervalY) > 0) then result.Y:=intervalY div abs(intervalY);
end;

constructor TVentMap.create(input: TStringArray);
begin
 fCoords:=convertInputToCoordList(input);
 fVentMap:=AVentMap.create;
 setLength(fVentMap,getMaxValue+1,getMaxValue(false)+1);
 resetMap;
end;

procedure TVentMap.calculateVents(noDiagonal: boolean);
var
  puzzleInput:ACoordArray;
  coordsLength,coord:integer;
  startPoint,currentPoint,finishPoint,move:TPoint;
  currentCoord:RCoord;
  rangeY,maxRange:integer;
  done:boolean;
begin
  if noDiagonal then puzzleInput:=getStraightLines(fCoords)
    else puzzleInput:=fCoords;
  coordsLength:=length(puzzleInput);
  //we have an array of coordinates
  //for each entry in the array we need to work out
  //all the points that are passed by a line from start to finish
  //and increment the map accordingly
  resetMap;
  for coord:=0 to pred(coordsLength) do
    begin
    currentCoord:=puzzleInput[coord];
    //distance travelled on x axis
    maxRange:=abs(currentCoord.start.X - currentCoord.finish.X);
    //distance travelled on y axis
    rangeY:=abs(currentCoord.start.Y - currentCoord.finish.Y);
    if rangeY > maxRange then maxRange:=rangeY;
    //Now we know how far we're travelling. Set the start point
    startPoint:=currentCoord.start;
    finishPoint:=currentCoord.finish;
    currentPoint:=startPoint;
    move:=calculateMove(currentCoord);
      repeat
      done:=(currentPoint.X = finishPoint.X)and(currentPoint.Y = finishPoint.Y);
      fVentMap[currentPoint.X][currentPoint.Y]
        :=fVentMap[currentPoint.X][currentPoint.Y]+1;
      //now adjust our point. For straight lines one coordinate won't change
      currentPoint.X:=currentPoint.X + move.X;
      currentPoint.Y:=currentPoint.Y + move.Y;
      until done;
    end;
end;

function TVentMap.getOverlapCount: Integer;
var
  output:integer;
  x,y:integer;
  mapHeight,mapWidth:integer;
begin
  output:=0;
  mapHeight:=length(fVentMap);
  if (mapHeight > 0) then mapWidth := length(fVentMap[0]) else mapWidth:=0;
  for x:=0 to pred(mapWidth) do
    begin
    for y:=0 to pred(mapHeight) do
      begin
      if (fVentMap[x][y] >= 2) then output:=output+1;
      end;
    end;
  result:=output;
end;

end.

