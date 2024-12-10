unit pointVisitCount;

{$mode ObjFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$modeswitch TypeHelpers}
interface

uses
  Classes, SysUtils,arrayUtils;
Type

  TPointVisitCount = record
    point:TPoint;
    count:integer;
  end;

  TPointVisitCountArray = array of TPointVisitCount;

  { TPointVisitCountArrayHelper }
  TPointVisitCountArrayHelper = type helper for TPointVisitCountArray
  function size:integer;
  function push(pointVisitCount:TPointVisitCount):integer;
  function indexOf(inputPoint:TPoint):integer;
  function countOf(inputPoint:TPoint):integer;
  function addToCount(inputPoint:TPoint;inputCount:integer):integer;
  procedure clear;
  end;

implementation

{ TPointVisitCountArrayHelper }

function TPointVisitCountArrayHelper.size: integer;
begin
  result:=length(self);
end;

function TPointVisitCountArrayHelper.push(pointVisitCount: TPointVisitCount
  ): integer;
begin
  if (self.indexOf(pointVisitCount.point) = -1) then
    begin
    setLength(self,self.size+1);
    self[self.size - 1]:=pointVisitCount;
    end;
end;

function TPointVisitCountArrayHelper.indexOf(inputPoint: TPoint): integer;
begin
  result:=-1;
  if (self.size = 0) then exit;
  for result:=0 to pred(self.size) do
    if (self[result].point = inputPoint) then exit;
  result:=-1;
end;

function TPointVisitCountArrayHelper.countOf(inputPoint: TPoint): integer;
begin
  if (self.indexOf(inputPoint)= -1) then result:=0
  else result:=self[indexOf(inputPoint)].count;
end;

function TPointVisitCountArrayHelper.addToCount(inputPoint: TPoint; inputCount: integer
  ): integer;
var
  item:TPointVisitCount;
  itemIndex:integer;
begin
  itemIndex:=self.indexOf(inputPoint);
  if (itemIndex = -1) then
    begin
    item.point:=inputPoint;
    item.count:=inputCount;
    self.push(item);
    result:=inputCount;
    end else
    begin
    item:=self[itemIndex];
    item.count:=item.count+inputCount;
    self[itemIndex]:=item;
    result:=item.count;
    end;
end;

procedure TPointVisitCountArrayHelper.clear;
begin
  setLength(self,0);
end;


end.

