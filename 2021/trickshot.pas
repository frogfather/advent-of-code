unit trickshot;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,aocUtils;
type
  { TRange }
  TRange = record
    xMin: integer;
    xMax: integer;
    yMin: integer;
    yMax: integer;
  end;

  { TTrickShot }

  TTrickShot = class(TInterfacedObject)
    private
    fTargetMin:TPoint;
    fTargetMax:TPoint;
    function minXVelocityToReachPos(pos:integer):integer;
    function calculateVelocityAtInterval(initialVelocity:TPoint;interval:integer):TPoint;
    function calculatePositionAtInterval(initialVelocity:TPoint;interval:integer):TPoint;
    function hitsTarget(initialVelocity:TPoint):boolean;
    property targetMin: TPoint read fTargetMin;
    property targetMax: TPoint read fTargetMax;
    public
    function calculateVelocityForMaxHeight:TPoint;
    constructor create(puzzleInput:string);
  end;

implementation

{ TTrickShot }

function TTrickShot.minXVelocityToReachPos(pos: integer): integer;
var
  ans1,ans2: double;
  roundedValue:integer;
begin
  //equation for max x
  //distance = (vx*vx) - triangular(vx-1)
  //which results in (1 +/- sqrt(1 - 24d)) div 6
  ans1:= (1 + sqrt(abs(1 - (24 * pos)))) / 6;
  ans2:= (1 - sqrt(abs(1 - (24 * pos)))) / 6;
  //if pos is negative use the negative one
  if (pos < 0) then
    begin
    roundedValue:=round(ans2);
    if roundedValue > ans2 then roundedValue:=roundedValue -1 ;
    end else
    begin
    roundedValue:=round(ans1);
    if roundedValue < ans1 then roundedValue:=roundedValue +1;
    end;
  result:=roundedValue;
end;

function TTrickShot.calculateVelocityAtInterval(initialVelocity: TPoint;
  interval: integer): TPoint;
  var
    xOffset:integer;
begin
  //X velocity decreases towards 0
  xOffset:= interval * (initialVelocity.X div abs(initialVelocity.X));
  //if the abs value of the offset is larger than the original velocity the result is zero
  if abs(xOffset) > abs(initialVelocity.X)
    then result.X:=0
  else result.X:=initialVelocity.X - xOffset;
  //Y velocity is easier
  result.Y:=initialVelocity.Y - interval;
end;

function TTrickShot.calculatePositionAtInterval(initialVelocity:TPoint;interval:integer):TPoint;
var
  vx,vy:integer;
begin
  vx:= initialVelocity.X;
  vy:= initialVelocity.Y;
  if (interval >= initialVelocity.X)
    then result.X:= (vx*vx) - triangular(vx-1)
  else result.X:= (interval * vx) - triangular(interval - 1);
  result.Y:= (interval * vy) - triangular(interval - 1);
end;

function TTrickShot.hitsTarget(initialVelocity: TPoint): boolean;
var
  maxHeight:integer;
  interval:integer;
  hitOrPast:boolean;
  positionAtInterval,velocityAtInterval:TPoint;
begin
  result:=false;
  maxHeight:= calculatePositionAtInterval(initialVelocity,initialVelocity.Y).Y;
  //if the target is above the max height we'll never hit it
  if maxHeight < targetMin.Y
    then exit
  else
    begin
    interval:=0;
    hitOrPast:=false;
      repeat
      positionAtInterval:=calculatePositionAtInterval(initialVelocity,interval);
      velocityAtInterval:=calculateVelocityAtInterval(initialVelocity,interval);
      //is our position at this point on the target
      if (positionAtInterval.X >= TargetMin.X)
        and(positionAtInterval.X <= TargetMax.X)
        and(positionAtInterval.Y >= TargetMin.Y)
        and(positionAtInterval.Y <= TargetMax.Y)
      then
        begin
        hitOrPast:=true;
        result:=true;
        break;
        end else
      //if we're not on target are we heading in the right direction?
      //ignore positive y because it will come down again
      if ((positionAtInterval.X > TargetMax.X) and (velocityAtInterval.X > 0))
      or ((positionAtInterval.X < TargetMin.X) and (velocityAtInterval.X < 0))
      or ((positionAtInterval.Y < TargetMin.Y) and (velocityAtInterval.Y < 0))
      then
        begin
        hitOrPast:=true;
        result:=false;
        break
        end;
      interval:=interval+1;
      until hitOrPast;
    end;
end;

function TTrickShot.calculateVelocityForMaxHeight:TPoint;
var
  minX,minY,maxX,maxY:integer;
  x,y:integer;
  initVelocities:TPoint;
  maxHeight:integer;
  largestMaxHeight:integer;
begin
  //find initial velocity to reach fTargetMin and fTargetMax
  minX:=minXVelocityToReachPos(targetMin.X);
  maxX:=minXVelocityToReachPos(targetMax.X);
  minY:=0;
  maxY:=100;
  largestMaxHeight:=0;
  for x:= minX to maxX do
    begin
    for y:=minY to maxY do
      begin
      initVelocities:=TPoint.Create(x,y);
      if hitsTarget(initVelocities) then
        begin
        maxHeight:=calculatePositionAtInterval(initVelocities,y).Y;
        if maxHeight > largestMaxHeight then largestMaxHeight:=maxHeight;
        end;
      end;
    end;
  //these should be the limits of X velocity
  //min Y velocity is 0
  //max Y velocity is where probe just hits target

end;

constructor TTrickShot.create(puzzleInput: string);
const saSeparators: array [0..3] of string = ('..','=',':',',');
var
  targetCoords: TStringArray;
begin
  targetCoords:= puzzleInput.Split(saSeparators);
  fTargetMin.X:=trim(targetCoords[2]).ToInteger;
  fTargetMax.X:=trim(targetCoords[3]).ToInteger;
  fTargetMin.Y:=trim(targetCoords[5]).ToInteger;
  ftargetMax.Y:=trim(targetCoords[6]).ToInteger;
end;

end.

