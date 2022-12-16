unit day14;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, aocPuzzle, LazLogger, ExtCtrls, Graphics, arrayUtils,routeFind,anysort;
type

  { TDayFourteen }

  TDayFourteen = class(TAocPuzzle)
  private
   fFilled:TPointArray; //better solution
   fMaxY:Integer;
   fMinX:integer;
   procedure generatePoints;
   procedure addRange(point1,point2:TStringArray);
   function runSand(part1:boolean=true):TPoint;
   function spaceFree(atPoint:TPoint;part1:boolean=true):boolean;
  public
    constructor Create(filename: string; paintbox_: TPaintbox = nil);
    procedure runPartOne; override;
    procedure runPartTwo; override;
  end;
implementation

{ TDayFourteen }

procedure TDayFourteen.generatePoints;
var
  index,elementIndex:integer;
  lineElements:TStringArray;
  coords,prevCoords:TStringArray;
  x,y:integer;
begin
  fMaxY:=0;
  fMinX:=500; //actual min will be less than this
  for index:= 0 to pred(puzzleInputLines.size) do
    begin
    lineElements:=puzzleInputLines[index].Split(' -> ');
    prevCoords:=lineElements[0].Split(',');
    for elementIndex:=0 to pred(lineElements.size) do
      begin
      coords:=lineElements[elementIndex].Split(',');
      if (coords.size = 2) then
        begin
        if (elementIndex > 0) then addRange(coords,prevCoords);
        prevCoords:=coords;
        end;
      end;
    end;
end;

procedure TDayFourteen.addRange(point1, point2: TStringArray);
var
  x,y:integer;
  xStart,yStart,xEnd,yEnd:integer;
  thisPoint:TPoint;
begin
  //fFilled is the point array
  //for each item in this range, generate a point and add it to point array
  if (point1[0].ToInteger < point2[0].toInteger) then
    begin
    xStart:=point1[0].toInteger;
    xEnd:=point2[0].toInteger;
    end else
    begin
    xStart:=point2[0].toInteger;
    xEnd:=point1[0].toInteger;
    end;
  if (point1[1].ToInteger < point2[1].toInteger) then
    begin
    yStart:=point1[1].toInteger;
    yEnd:=point2[1].toInteger;
    end else
    begin
    yStart:=point2[1].toInteger;
    yEnd:=point1[1].toInteger;
    end;
    for x:=xStart to xEnd do
      for y:=yStart to yEnd do
        begin
        thisPoint:=TPoint.Create(x,y);
        if (fFilled.indexOf(thisPoint) = -1) then
          fFilled.push(thisPoint);
        end;
    if (xStart < fMinX) then fMinX:= xStart;
    if (yEnd > fMaxY) then fMaxY:= yEnd;
end;

function TDayFourteen.runSand(part1: boolean): TPoint;
begin
  result:=TPoint.Create(500,0);
  while result.Y < fMaxY do
    begin
    if (spaceFree(TPoint.Create(result.X,result.Y+1)))
      then result.Y:=result.Y+1 else
    if (spaceFree(TPoint.Create(result.X-1,result.Y+1)))
      then
        begin
        result.X:=result.X-1;
        result.Y:=result.Y+1;
        end else
    if spaceFree(TPoint.Create(result.X+1,result.Y+1))
      then
        begin
        result.X:=result.X+1;
        result.Y:=result.Y+1;
        end else exit;
    end;
end;

function TDayFourteen.spaceFree(atPoint: TPoint;part1:boolean): boolean;
begin
  result:= fFilled.indexOf(atPoint) = -1;
end;

constructor TDayFourteen.Create(filename: string; paintbox_: TPaintbox);
begin
  inherited Create(filename, 'Day 14', paintbox_);
  fFilled:=TPointArray.create;
end;

procedure TDayFourteen.runPartOne;
var
  done:boolean;
  sandAt:TPoint;
  counter:integer;
begin
  generatePoints;
  done:=false;
  counter:=0;
  results.add('MaxY '+fMaxY.toString);
  while not done do
    begin
    sandAt:=runSand;
    done:= (sandAt.X < fMinX) or (sandAt.Y >= fMaxY);
    if not done then
      begin
      if (fFilled.indexOf(sandAt)= -1) then fFilled.push(sandAt);
      counter:=counter+1;
      end;
    end;
  results.add('sand units '+counter.toString);
end;

procedure TDayFourteen.runPartTwo;
var
  done:boolean;
  sandAt:TPoint;
  counter:integer;
begin
  generatePoints;
  done:=false;
  counter:=0;
  fMaxY:=fMaxY + 1; //floor is 1 lower
  while not done do
    begin
    sandAt:=runSand;
    counter:=counter+1;
    done:= (sandAt.X = 500)and(sandAt.Y = 0) or (counter = 30000);
    if not done then
      begin
      if (fFilled.indexOf(sandAt)= -1) then fFilled.push(sandAt);
      end;
    end;
  results.add('sand units '+counter.toString);
end;

end.

