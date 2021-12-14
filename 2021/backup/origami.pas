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
    function calculateDotCount: integer;
    function calculateMapDimensionsFromCoordinates(coordinates:TStringArray):TPoint;
    function calculateMapCentre:TPoint;
    function getMapDimensions:TPoint;
    procedure populateMap;
    procedure resizeMap(size:integer;columns:boolean=true);
    procedure mapFolded(foldPoint:integer;columns:boolean=true);
    public
    constructor create(coordinates:TStringArray);
    procedure fold(input:string);
    function getMapOutput:TStringList;
    property dotCount:integer read calculateDotCount;
    property dimensions:TPoint read fDimensions;
  end;

implementation

{ TOrigami }

constructor TOrigami.create(coordinates:TStringArray);
var
  mapDimensions:TPoint;
begin
  fMap:=T2DStringArray.create;
  fCoordinates:=coordinates;
  mapDimensions:=calculateMapDimensionsFromCoordinates(fCoordinates);
  setLength(fmap,mapDimensions.X,mapDimensions.Y);
  getMapDimensions;
  calculateMapCentre;
  populateMap;
end;

procedure TOrigami.fold(input:string);
var
  instruction:TStringArray;
  originalDimension:integer;
  foldPosition,foldedDimension:integer;
  foldHorizontally:boolean;
begin
  if (length((input.Split(' '))) <> 3) then exit;
  instruction:=input.Split(' ')[2].Split('=');
  if (length(instruction) <> 2) then exit;
  if (instruction[0] <> 'x')and(instruction[0] <> 'y') then exit;
  foldPosition:=instruction[1].ToInteger;
  foldHorizontally:=instruction[0]='y';
  if foldHorizontally
    then originalDimension:=fDimensions.Y
  else originalDimension:=fDimensions.X;
  foldedDimension:=(originalDimension div 2);
  mapFolded(foldPosition,foldHorizontally);
  resizeMap(foldedDimension,foldHorizontally);
end;

function TOrigami.getMapOutput: TStringList;
var
  x,y:integer;
  sline:string;
  output:TStringList;
begin
  output:=TStringlist.create;
  for y:= 0 to pred(fDimensions.Y) do
    begin
    sLine:='';
    for x:=0 to pred(fDimensions.X) do
      begin
      if (fMap[x][y])='#' then sLine:=sLine+'#' else sLine:=sLine+'  ';
      end;
    output.Add(sLine);
    end;
  result:=output;
end;

function TOrigami.calculateMapDimensionsFromCoordinates(coordinates: TStringArray): TPoint;
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
  if length(fMap) > 0
    then result.Y:=length(fMap[0])
  else result.Y:=0;
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
  columnLength:integer;
begin
  if columns
    then for column:=0 to pred(length(fMap)) do
      setLength(fMap[column], size) else
    begin
    if (size > 0) and (length(fMap)>0)
      then columnLength:=length(fMap[0]) else columnLength:=0;
    setLength(fMap,size,columnLength);
    end;
  getMapDimensions;
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
      if (mirroredPosition >= 0)
      and ((fMap[xPos][mirroredPosition] = '#') or (fMap[xpos][ypos] = '#'))
        then fMap[xPos][mirroredPosition]:= '#';
      end;
  end else
  begin
  for xpos:=foldPoint+1 to pred(fDimensions.X)do
    for ypos:=0 to pred(fDimensions.Y) do
      begin
      mirroredPosition:=foldPoint - abs(xPos - foldPoint);
      //if there's a # in either position then the result is #
      if (mirroredPosition >= 0)
      and ((fMap[mirroredPosition][yPos] = '#') or (fMap[xpos][ypos] = '#'))
        then fMap[mirroredPosition][yPos]:= '#';
      end;
  end;
end;

end.

