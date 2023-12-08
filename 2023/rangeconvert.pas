unit rangeConvert;

{$mode ObjFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils,anysort;
//For puzzle day 5 adjusting a source number over a specified range
type
  TRangeConverter = record
    start: int64;
    length: int64;
    dest: int64;
  end;

  TRange = record
    rangeStart: int64;
    rangeEnd: int64;
  end;

  //Used in day 5 puzzle
  TRangeConverterArray = array of TRangeConverter;
  TRangeArray = array of TRange;


  { TRangeConverterArrayHelper }

  TRangeConverterArrayHelper = type helper for TRangeConverterArray
  function size: integer;
  function push(element: TRangeConverter):integer;
  end;

  { TRangeArrayHelper }

  TRangeArrayHelper = type helper for TRangeArray
  function size: integer;
  function push(element: TRange):integer;
  end;

procedure sort(var arr: array of TRangeConverter; count: Integer; ascending: boolean=true);
procedure sortRangeArray(var arr: array of TRange; count:Integer; ascending: boolean=true);

implementation

{ TRangeArrayHelper }

function TRangeArrayHelper.size: integer;
begin
  result:=length(self);
end;

function TRangeArrayHelper.push(element: TRange): integer;
begin
  insert(element,self,length(self));
  result:=self.size;
end;

{ TRangeConverterArrayHelper }

function TRangeConverterArrayHelper.size: integer;
begin
  result:=length(self);
end;

function TRangeConverterArrayHelper.push(element: TRangeConverter
  ): integer;
begin
  insert(element,self,length(self));
  result:=self.size;
end;


function CompareRangeConverterAsc(const d1,d2):integer;
var
  s1: TRangeConverter absolute d1;
  s2: TRangeConverter absolute d2;
  comparison:int64;
begin
  comparison:= s1.start - s2.start;
  if comparison = 0 then result:=0
    else result:= comparison div abs(comparison);
end;

function CompareRangeConverterDesc(const d1,d2):integer;
var
  s1: TRangeConverter absolute d1;
  s2: TRangeConverter absolute d2;
  comparison:int64;
begin
  comparison:= s2.start - s1.start;
  if comparison = 0 then result:=0
    else result:= comparison div abs(comparison);
end;

function CompareRangeAsc(const d1,d2):integer;
var
  s1: TRange absolute d1;
  s2: TRange absolute d2;
  comparison:int64;
begin
  comparison:= s1.rangeStart - s2.rangeStart;
  if comparison = 0 then result:=0
    else result:= comparison div abs(comparison);
end;

function CompareRangeDesc(const d1,d2):integer;
var
  s1: TRange absolute d1;
  s2: TRange absolute d2;
  comparison:int64;
begin
  comparison:= s2.rangeStart - s1.rangeStart;
  if comparison = 0 then result:=0
    else result:= comparison div abs(comparison);
end;


procedure sort(var arr: array of TRangeConverter; count: Integer;
  ascending: boolean);
begin
  if ascending then
    anysort.AnySort(arr, Count, sizeof(TRangeConverter), @CompareRangeConverterAsc)
  else
    anysort.AnySort(arr, Count, sizeof(TRangeConverter), @CompareRangeConverterDesc)
end;

procedure sortRangeArray(var arr: array of TRange; count: Integer;
  ascending: boolean);
begin
  if ascending then
    anysort.AnySort(arr, Count, sizeof(TRange), @CompareRangeAsc)
  else
    anysort.AnySort(arr, Count, sizeof(TRange), @CompareRangeDesc)
end;

end.

