unit ventMap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fileUtilities;

type
  AVentMap = array of array of integer;

  { TVentMap }

  TVentMap = class(TInterfacedObject)
    private
    fVentMap: AVentMap;
    fInput: TStringArray;
    function removeDiagonal(input:TStringArray):TStringArray;
    function coordToPoint(coord:String):TPoint;
    public
    constructor create(input:TStringArray);
  end;

implementation

{ TVentMap }

function TVentMap.removeDiagonal(input: TStringArray): TStringArray;
var
  output:TStringArray;
  coord:TStringArray;
  start,finish:TPoint;
  lineNo:integer;
begin
//filter out any entries where both the x and y start and end point are different
output:=TStringArray.create;
for lineNo:=0 to pred(length(input)) do
  begin
    coord:=input[lineNo].split('->');
    start:=coordToPoint(coord[0]);
    finish:=coordToPoint(coord[1]);
    if (start.X = finish.X) or (start.Y = finish.Y)
      then fileutilities.addToArray(output,input[lineNo]);
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
     x:=xy[0].ToInteger;
     y:=xy[1].ToInteger;
    end;
  result:=TPoint.Create(x,y);
end;

constructor TVentMap.create(input: TStringArray);
var
  mapWidth,mapLength: integer;
begin
 mapLength:=length(input);
 if (mapLength > 0 ) then mapWidth:= length(input[0]) else mapWidth:=0;
 fVentMap:=AVentMap.create;
 setLength(fVentMap,mapWidth,mapLength);
 fInput:=input;
end;

end.

