unit origami;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ArrayUtils;
type
  
  { TOrigami }

  TOrigami = class(TInterfacedObject)
    private
    fMap: T2DStringArray;
    fCoordinates:TStringArray;
    fDimensions:TPoint;
    fMapCentre:TPoint;
    function calculateDotCount: integer;
    function calculateMapDimensions(coordinates:TStringArray):TPoint;
    function calculateMapCentre:TPoint;
    function getMapDimensions:TPoint;
    procedure populateMap;
    procedure resizeMap(size:integer;columns:boolean=true);
    procedure moveMapData(offset:integer;columns:boolean=true);
    procedure mapFolded(foldPoint:integer;columns:boolean=true);
    property mapCentre:TPoint read fMapCentre;
    public
    constructor create(coordinates:TStringArray);
    procedure fold(input:string);
    property dotCount:integer read calculateDotCount;
  end;

implementation

{ TOrigami }

constructor TOrigami.create(coordinates:TStringArray);
var
  mapDimensions:TPoint;
begin
  fMap:=T2DStringArray.create;
  fCoordinates:=coordinates;
  mapDimensions:=calculateMapDimensions(fCoordinates);
  setLength(fmap,mapDimensions.X,mapDimensions.Y);
  getMapDimensions;
  fMapCentre:=calculateMapCentre;
  populateMap;
end;

procedure TOrigami.fold(input:string);
var
  instruction:TStringArray;
  centrePoint,originalDimension:integer;
  foldPosition,foldOffset,edgeOffset,foldedDimension:integer;
  foldHorizontally:boolean;
begin
  if (length((input.Split(' '))) <> 3) then exit;
  instruction:=input.Split(' ')[2].Split('=');
  if (length(instruction) <> 2) then exit;
  if (instruction[0] <> 'x')and(instruction[0] <> 'y') then exit;
  foldPosition:=instruction[1].ToInteger;
  foldHorizontally:=instruction[0]='y';
  //the calculations are the same regardless of the axis
  //if we're folding on x we're folding on the y axis
  if foldHorizontally then
    begin
    centrePoint:=mapCentre.Y;
    originalDimension:=fDimensions.Y;
    end else
    begin
    centrePoint:=mapCentre.X;
    originalDimension:=fDimensions.X;
    end;
  //How far the fold position is from the centre point
  foldOffset:=foldPosition - centrePoint;
  if (foldOffset < 0)
    then edgeOffset:=2*foldOffset
  else edgeOffset:=0;
  foldedDimension:=(originalDimension div 2)+ abs(foldOffset);
  if (edgeOffset > 0) then
    begin
    resizeMap(abs(edgeOffset),foldHorizontally);
    moveMapData(edgeOffset,foldHorizontally);
    centrePoint:=centrePoint + abs(edgeOffset);
    foldPosition:=foldPosition + abs(edgeOffset);
    end;
  //we can now map any items after the fold line
  mapFolded(foldPosition,foldHorizontally);
  //now resize the map again to the folded size
  resizeMap(foldedDimension,foldHorizontally);
end;

function TOrigami.calculateMapDimensions(coordinates: TStringArray): TPoint;
var
  index:integer;
  begin
  result.X:=0;
  result.Y:=0;
  for index:=0 to pred(length(coordinates)) do
    begin
    if (length(coordinates[index].Split(',')) = 2) then
      begin
      if (coordinates[index].Split(',')[0].ToInteger > result.X)
        then result.X:=coordinates[index].Split(',')[0].ToInteger;
      if (coordinates[index].Split(',')[1].ToInteger > result.Y)
        then result.Y:=coordinates[index].Split(',')[1].ToInteger;
      end;
    end;
  result.X:=result.X + 1;
  result.Y:=result.Y + 1;
  end;

function TOrigami.calculateMapCentre: TPoint;
begin
  result.X:=length(fMap) div 2;
  if result.X > 0 then result.Y:= length(fMap[0]) div 2 else result.Y:=0;
end;

function TOrigami.getMapDimensions: TPoint;
begin
  result.X:=length(fMap);
  if (length(fMap)>0)then result.Y:=length(fMap[0])else result.Y:=0;
  fDimensions:=result;
end;

function TOrigami.calculateDotCount: integer;
var
  x,y:integer;
  dotsCount:integer;
begin
dotsCount:=0;
  for x:=0 to pred(fDimensions.X) do
    for y:=0 to pred(fDimensions.Y) do
  if (fMap[x][y]='#') then dotsCount:=dotsCount+1;
  result:=dotsCount;
end;

procedure TOrigami.populateMap;
var
  index:integer;
  xCoord,yCoord:integer;
  begin
  for index:=0 to pred(length(fcoordinates)) do
    begin
    if length((fcoordinates[index].Split(','))) = 2 then
      begin
      xCoord:=fcoordinates[index].Split(',')[0].ToInteger;
      yCoord:=fcoordinates[index].Split(',')[1].ToInteger;
      fmap[xCoord][yCoord]:='#';
      end;
    end;
  end;

procedure TOrigami.resizeMap(size: integer; columns: boolean);
var
  column:integer;
begin
  if columns then
    begin
    for column:=0 to length(fMap)do
      setLength(fMap[column], size);
    end else
    begin
    setLength(fMap,size);
    end;
  getMapDimensions;
end;

procedure TOrigami.moveMapData(offset: integer; columns: boolean);
var
  xPosition,yPosition:integer;
begin
  if columns then
    begin
    for yPosition:= pred(fDimensions.Y) downto abs(offset) do
      for xPosition:=0 to pred(fDimensions.X) do
        fMap[xPosition][yPosition]:= fMap[xPosition][yPosition + offset];
    end else
    begin
    for xPosition:=pred(fDimensions.X) downto abs(offset) do
      for yPosition:=0 to pred(fDimensions.Y) do
        fMap[xPosition][yPosition]:= fMap[xPosition + offset][yPosition];
    end;
end;

procedure TOrigami.mapFolded(foldPoint: integer; columns: boolean);
var
  xpos,ypos,mirroredPosition:integer;
begin
if columns then
  begin
  for ypos:=foldPoint+1 to pred(fDimensions.Y)do
    for xpos:=0 to pred(fDimensions.X) do
      begin
      mirroredPosition:=foldPoint - abs(yPos - foldPoint);
      //if there's a # in either position then the result is #
      if (fMap[xPos][mirroredPosition] = '#') or (fMap[xpos][ypos] = '#')
        then fMap[xPos][mirroredPosition]:= '#';
      end;
  end else
  begin
  for xpos:=foldPoint+1 to pred(fDimensions.X)do
    for ypos:=0 to pred(fDimensions.Y) do
      begin
      mirroredPosition:=foldPoint - abs(xPos - foldPoint);
      //if there's a # in either position then the result is #
      if (fMap[mirroredPosition][yPos] = '#') or (fMap[xpos][ypos] = '#')
        then fMap[mirroredPosition][yPos]:= '#';
      end;
  end;
end;

end.

