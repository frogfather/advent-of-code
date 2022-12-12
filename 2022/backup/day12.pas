unit day12;

{$mode ObjFPC}{$H+}

interface

uses
 Classes, SysUtils, aocPuzzle, LazLogger, ExtCtrls, Graphics, arrayUtils,routeFind;

  { TDayTwelve }
type
  TDayTwelve = class(TAocPuzzle)
  private
    fStartPoint,fEndPoint:TPoint;
    fRouteFinder:TRouteFinder;
    function convertMap:T3DIntMap;
  public
    constructor Create(filename: string; paintbox_: TPaintbox = nil);
    procedure runPartOne; override;
    procedure runPartTwo; override;
  end;

implementation

{ TDayTwelve }

function TDayTwelve.convertMap:T3DIntMap;
var
  lineNo,charNo:integer;
  currentLine:string;
  currentChar:char;
  charValue:integer;
begin
  result:=T3DIntMap.create;
  setLength(result,length(puzzleInputLines[0]),length(puzzleInputLines),2);
  //convert the letters a-z into numbers
  //chr - 96 for small letters
  //special case for S and E call them 0 and 27
  for lineNo := 0 to pred(puzzleInputLines.size) do
    begin
    currentLine:=puzzleInputLines[lineNo];
    for charNo:= 0 to pred(currentLine.Length) do
      begin
      currentChar:= currentLine[charNo+1]; //strings 1 indexed!!
      if currentChar = 'S' then
        begin
        charValue:= 1;
        fStartPoint:= TPoint.Create(charNo,LineNo);
        end
      else if currentChar = 'E' then
        begin
        charValue:= 26;
        fEndPoint:= TPoint.Create(charNo, LineNo);
        end else
      charValue:=ord(currentChar) - 96;
      result[charNo][lineNo][0]:= charValue;
      result[charNo][lineNo][1]:= 0;
      end;
    end;
end;

constructor TDayTwelve.Create(filename: string; paintbox_: TPaintbox);
begin
  inherited Create(filename, 'Day 12', paintbox_);
end;

procedure TDayTwelve.runPartOne;
begin
  fRouteFinder:= TRouteFinder.create(convertMap);
  fRouteFinder.findShortestPath(fStartPoint,fEndPoint);
  results.add('shortest path has '+fRouteFinder.shortestSteps.ToString+' steps');
end;

procedure TDayTwelve.runPartTwo;
var
  aArray:TPointArray;
  resultArray:TIntArray;
  map:T3DIntMap;
  x,y,index:integer;
  thisPoint:TPoint;
  pathLength:integer;
begin
  //create an array of points that are 'a' (including S)
  aArray:=TPointArray.Create;
  resultArray:= TIntArray.create;
  map:=convertMap;
  fRouteFinder:= TRouteFinder.create(map);

  for x:=0 to pred(length(map)) do
    for y:=0 to pred(Length(map[x])) do
      if (map[x][y][0] = 1)or(map[x][y][0] = 0) then
        aArray.push(TPoint.Create(x,y));
  results.add('Number of a in the grid '+aArray.size.toString);
  for index:= 0 to pred(aArray.size) do
    begin
    thisPoint:=aArray[index];
    fRouteFinder.findShortestPath(thisPoint,fEndPoint);
    if (fRouteFinder.pathFound) then
      begin
      pathLength:=fRouteFinder.shortestSteps;
      resultArray.push(pathLength);
      end else results.add('No path '+thispoint.X.toString+','+thisPoint.Y.ToString);
    end;
  sort(resultArray,resultArray.size);
  results.add('Shortest path from any point a '+resultArray[0].ToString);
end;

end.

