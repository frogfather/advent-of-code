unit day14;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, aocPuzzle, LazLogger, ExtCtrls, Graphics, arrayUtils,routeFind,anysort;
type

  { TDayFourteen }

  TDayFourteen = class(TAocPuzzle)
  private
   fMap:T2DStringArray;
   fMapMin,fMapMax:TPoint;
   fXoffset:integer;
   procedure findMapDimensions;
   procedure buildMap;
   procedure drawScreen(Sender: TObject);
   procedure rockHorizontal(y,start,finish:integer);
   procedure rockVertical(x,start,finish:integer);
   function dropSand:boolean;
   function pointInRange(x,y:integer):boolean;
  public
    constructor Create(filename: string; paintbox_: TPaintbox = nil);
    procedure runPartOne; override;
    procedure runPartTwo; override;
  end;
implementation

{ TDayFourteen }

procedure TDayFourteen.findMapDimensions;
var
  index, segmentIndex:integer;
  lineSegments:TStringArray;
  coords:TStringArray;
begin
  fMapMax.X:=0;
  fMapMax.Y:=0;
  fMapMin.X:=10000;
  fMapMin.Y:=0;//the sand comes in at 500,0
  for index:= 0 to pred(puzzleInputLines.size) do
    begin
      //separate the line on ' -> '
      lineSegments:=puzzleInputLines[index].Split(' -> ');
      //that gives us an array of points
      //go through them and find the max
      for segmentindex:= 0 to pred(lineSegments.size) do
        begin
        //separate on .
        coords:=lineSegments[segmentindex].Split(',');
        if (coords.size = 2)then
          begin
          if (coords[0].ToInteger > fMapMax.X) then fMapMax.X:=coords[0].ToInteger;
          if (coords[0].ToInteger < fMapMin.X) then fMapMin.X:=coords[0].ToInteger;
          if (coords[1].ToInteger > fMapMax.Y) then fMapMax.Y:=coords[1].ToInteger;
          end;
        end;
    end;
  fXoffset:= fMapMin.X;
  results.add('map min '+fMapMin.X.ToString+':'+fMapMin.Y.ToString+' max '+fMapMax.X.ToString+':'+fMapMax.Y.ToString)
end;

procedure TDayFourteen.buildMap;
var
  index,x,y,segmentIndex:integer;
  lineSegments:TStringArray;
  coords:TStringArray;
  previousCoord,currentCoord:TPoint;
begin
  // create the map and fill in visited points
  // So each line is a set of lines in the grid
  fMap:=T2DStringArray.Create;
  setLength(fMap,(fMapMax.X - (fXoffset-1)),fMapMax.Y+1);
  for x:= 0 to pred(fMapMax.X - (fXoffset-1)) do
    for y:= 0 to pred(fMapMax.Y+1) do
      fMap[x][y]:='.';
  //now add the rocks
  //each line is a string of coordinates with ' -> ' between
  for index:=0 to pred(puzzleInputLines.size) do
    begin
    previousCoord.X:=-1;
    previousCoord.Y:=-1;
    lineSegments:=puzzleInputLines[index].Split(' -> ');
    for segmentIndex:= 0 to pred(lineSegments.size) do
      begin
      //separate on .
      coords:=lineSegments[segmentindex].Split(','); //single pair of numbers
      if (coords.size = 2)then
        begin
        if (previousCoord.X = -1) then //first entry
          begin
          previousCoord.X:=coords[0].toInteger - fXOffset;
          previousCoord.Y:=coords[1].toInteger;
          end else
          begin
          currentCoord.X:=coords[0].ToInteger - fXoffset;
          currentCoord.Y:=coords[1].ToInteger;

          if (currentCoord.X = previousCoord.X)
          then rockVertical(currentCoord.X,currentCoord.Y,previousCoord.Y)
          else rockHorizontal(currentCoord.Y,currentCoord.X,previousCoord.X);
          previousCoord:= currentCoord;
          end;
        end;
      end;
    end;
end;

procedure TDayFourteen.drawScreen(Sender: TObject);
var
  screenX, screenY: integer;
  pitchX,pitchY:integer;
begin
  if Sender is TPaintbox then with Sender as TPaintbox do
    begin
      pitchX:= (canvas.Width - 40) div length(fMap);
      pitchY:=(canvas.height - 40) div length(fMap[0]);
      canvas.font.Size := 8;
      canvas.Brush.color := clGray;
      canvas.pen.color:=clBlack;
      canvas.Rectangle(0, 0, canvas.Width, canvas.Height);
      for screenY := 0 to pred(Length(fMap[0])) do
        begin
        canvas.TextOut(0,20+(screenY*pitchY),screenY.ToString);
        for screenX := 0 to pred(length(fMap)) do
          begin
          if (screenY = 0) then canvas.TextOut(20+(screenX*pitchX),0, (screenX+fXoffset).ToString);
          if (fMap[screenX][screenY] = '#') then canvas.brush.Color:=clwhite else
          if (fMap[screenX][screenY] = 'o') then canvas.Brush.Color:=clYellow else
          if (screenY = 0) and ((screenX + fXoffset = 500)) then canvas.brush.color:=clRed else
          canvas.brush.color:=clBlack;
          canvas.Rectangle(20+(screenX*pitchX),20+(screenY*pitchY),20+(screenX*pitchX)+pitchX-1,20+(screenY*pitchY)+pitchY-1);
          canvas.Brush.color:=clGray;
          end;
        end;
    end;
end;

procedure TDayFourteen.rockHorizontal(y,start, finish: integer);
var
  x,xStart,xEnd:integer;
begin
  if start < finish then
    begin
    xStart:=start;
    xEnd:=finish;
    end else
    begin
    xEnd:=start;
    xStart:=finish;
    end;
  for x:= xStart to xEnd do
    fMap[x][y]:='#';
end;

procedure TDayFourteen.rockVertical(x,start, finish: integer);
var
  y,yStart,yEnd:integer;
begin
  if start < finish then
    begin
    yStart:=start;
    yEnd:=finish;
    end else
    begin
    yEnd:=start;
    yStart:=finish;
    end;
  for y:= yStart to yEnd do
    fMap[x][y]:='#';
end;

//drops a single piece of sand from 500,0 and return true if it finds a place to land
function TDayFourteen.dropSand: boolean;
var
  sandPos:TPoint;
  done:boolean;
begin
  sandPos.X:=500 - fXoffset;
  sandPos.Y:=0;
  done:=false;
  result:=true;
  while not done do
    begin
    if (pointInRange(sandPos.X,sandPos.Y + 1)) and (fMap[sandPos.X][sandPos.Y + 1] = '.') then
      begin
      sandPos.Y:= sandPos.Y + 1; //falls straight down
      end
    else if (pointInRange(sandPos.X - 1, sandPos.Y + 1))and(fMap[sandPos.X-1][sandPos.Y+1] = '.') then
      begin //falls to the left of the obstruction
      sandPos.Y:= sandPos.Y + 1;
      sandPos.X:= sandPos.X - 1;
      end
    else if (pointInRange(sandPos.X+1,sandPos.Y+1)) and (fMap[sandPos.X + 1][sandPos.Y+1] = '.') then
      begin //falls to the right of the obstruction
      sandPos.Y:= sandPos.Y + 1;
      sandPos.X:= sandPos.X + 1;
      end
    else if not pointInRange(sandPos.X - 1, sandPos.Y) or not pointInRange(sandPos.X, sandPosY+1)  then
      begin //falls off the left hand side into the abyss
      done:=true;
      result:=false;
      end
    else
      begin
      //nowhere to go so we're done
      fMap[sandPos.X][sandPos.Y]:='o';
      done:=true;
      end;
    end;
end;

function TDayFourteen.pointInRange(x,y:integer): boolean;
begin
  result:= (x >= 0) and (x < length(fMap)) and (y < length(fMap[x]))
end;

constructor TDayFourteen.Create(filename: string; paintbox_: TPaintbox);
begin
  inherited Create(filename, 'Day 14', paintbox_);
  if (paintbox <> nil) then Paintbox.OnPaint := @drawScreen;
end;

procedure TDayFourteen.runPartOne;
var
  sandResult:boolean;
  sandCount:integer;
begin
 findMapdimensions;
 buildMap;
 sandResult:=true;
 sandCount:=0;
 while sandResult = true do
   begin
   sandResult:= dropSand;
   if sandResult = True then sandCount:=sandCount + 1;
   end;
  results.add('units of sand dropped '+sandCount.ToString);
end;

procedure TDayFourteen.runPartTwo;
begin

end;

end.

