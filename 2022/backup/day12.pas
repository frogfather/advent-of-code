unit day12;

{$mode ObjFPC}{$H+}

interface

uses
 Classes, SysUtils, aocPuzzle, LazLogger, ExtCtrls, Graphics, arrayUtils;

  { TDayTwelve }

  TDayTwelve = class(TAocPuzzle)
  private
    fMap:T3DIntMap;
    fStartPoint,fEndPoint:TPoint;
    fShortestPath:integer;
    procedure createMap;
    procedure traverseMap;
    procedure doTraverseMap(startPoint:TPoint;pointsVisited,depth:integer);
  public
    constructor Create(filename: string; paintbox_: TPaintbox = nil);
    procedure runPartOne; override;
    procedure runPartTwo; override;
  end;

implementation

{ TDayTwelve }

procedure TDayTwelve.createMap;
var
  lineNo,charNo:integer;
  currentLine:string;
  currentChar:char;
  charValue:integer;
begin
  setLength(fmap,length(puzzleInputLines[0]),length(puzzleInputLines),2);
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
        charValue:= 0;
        fStartPoint:= TPoint.Create(charNo,LineNo);
        end
      else if currentChar = 'E' then
        begin
        charValue:= 27;
        fEndPoint:= TPoint.Create(charNo, LineNo);
        end else
      charValue:=ord(currentChar) - 96;
      fMap[charNo][lineNo][0]:= charValue;
      fMap[charNo][lineNo][1]:= 0;
      end;
    end;
end;

procedure TDayTwelve.traverseMap;
begin
  fMap[fStartPoint.X][fStartPoint.Y][1]:=1; //visited
  results.add('map width '+ length(fMap).toString);
  results.add('map height '+ length(fMap[0]).toString);
  doTraverseMap(fStartPoint,0,0);
end;

procedure TDayTwelve.doTraverseMap(startPoint:TPoint; pointsVisited,depth: integer);
var
  currentHeight:integer;
  nextPoint:TPoint;
  nextheight:integer;
  nextVisited:boolean;
begin
  currentHeight:= fMap[startPoint.X][startPoint.Y][0];

  fMap[startPoint.X][startPoint.Y][1]:=1; //mark this point as visited
  //If we are at 26 next to value 27 we are finished
  if (currentHeight = 26)and(((startPoint.X > 0) and (fMap[startPoint.X -1][startPoint.Y][0] = 27))
  or ((startPoint.X < pred(Length(fMap))) and (fMap[startPoint.X + 1][startPoint.Y][0] = 27))
  or ((startPoint.Y > 0) and (fMap[startPoint.X][startPoint.Y - 1][0] = 27))
  or ((startPoint.Y < pred(Length(fMap[startPoint.Y]))) and (fMap[startPoint.X][startPoint.Y + 1][0] = 27)))
  then
    begin
    Results.add('found endpoint with path length '+(pointsVisited).ToString);
    if (pointsVisited + 1 < fShortestPath) then fShortestPath:=pointsVisited + 1;
    fMap[startPoint.X][startPoint.Y][1]:=0;
    exit;
    end;

  //we can go in one of 4 directions if we've not been there before
  //and if it's one more than the current value
  //best choice: next item is 1 more than current
  //next

  //Go West
  if (startPoint.X > 0) then
    begin
    nextPoint.X:=startPoint.X -1;
    nextPoint.Y:=startPoint.Y;
    nextHeight:=fMap[nextPoint.X][nextPoint.Y][0];
    nextVisited:= (fMap[nextPoint.X][nextPoint.Y][1] = 1);
    if ((nextHeight = currentHeight)or(nextHeight = currentHeight + 1)) and (nextVisited = false)
      then
        doTraverseMap(nextPoint,pointsVisited + 1,depth+1);
    end;

  //Go North
  if (startPoint.Y > 0) then
    begin
    nextPoint.X:=startPoint.X;
    nextPoint.Y:=startPoint.Y - 1;
    nextHeight:=fMap[nextPoint.X][nextPoint.Y][0];
    nextVisited:= (fMap[nextPoint.X][nextPoint.Y][1] = 1);
    if ((nextHeight = currentHeight)or(nextHeight = currentHeight + 1)) and (nextVisited = false)
      then doTraverseMap(nextPoint,pointsVisited + 1,depth+1);
    end;

  //Go East
  if (startPoint.X < pred(Length(fMap))) then
    begin
    nextPoint.X:=startPoint.X + 1;
    nextPoint.Y:=startPoint.Y;
    nextHeight:=fMap[nextPoint.X][nextPoint.Y][0];
    nextVisited:= (fMap[nextPoint.X][nextPoint.Y][1] = 1);
    if ((nextHeight = currentHeight)or(nextHeight = currentHeight + 1)) and (nextVisited = false)
      then doTraverseMap(nextPoint,pointsVisited + 1,depth+1);
    end;

  //Go South
  if (startPoint.Y < pred(length(fMap[startPoint.X]))) then
    begin
    nextPoint.X:=startPoint.X;
    nextPoint.Y:=startPoint.Y + 1;
    nextHeight:=fMap[nextPoint.X][nextPoint.Y][0];
    nextVisited:= (fMap[nextPoint.X][nextPoint.Y][1] = 1);
    if ((nextHeight = currentHeight)or(nextHeight = currentHeight + 1)) and (nextVisited = false)
      then doTraverseMap(nextPoint,pointsVisited + 1,depth+1);
    end;
  fMap[startPoint.X][startPoint.Y][1]:=0;
  results.add('no path from '+startpoint.X.ToString+','+startpoint.Y.ToString+' at level '+depth.ToString);
end;

constructor TDayTwelve.Create(filename: string; paintbox_: TPaintbox);
begin
  inherited Create(filename, 'Day 12', paintbox_);
  fMap:=T3DIntMap.create;
end;

procedure TDayTwelve.runPartOne;
begin
  createMap;
  fShortestPath:=length(fMap)*length(fMap[0]);//set  to dimensions of map
  traverseMap;
end;

procedure TDayTwelve.runPartTwo;
begin

end;

end.

