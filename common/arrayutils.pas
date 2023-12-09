unit arrayUtils;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$modeswitch TypeHelpers}
interface

uses
  Classes, SysUtils,anysort,graphics,fgl,cardData,rangeConvert;
type
  //Looks like the built in TintegerArray is a static array
  //so let's define our own dynamic integer array
  TIntArray = specialize Tarray<Integer>;
  TInt64Array = specialize Tarray<Int64>;
  TStringMap = specialize TFPGMap<String,String>;
  TStringIntMap = specialize TFPGMap<String,Integer>;
  TStringInt64Map = specialize TFPGMap<String,Int64>;
  T2DIntMap = array of array of integer;
  T2DInt64Map = array of array of int64;
  T3DIntMap = array of array of array of integer;
  T2DStringArray = array of array of string;
  TColours = array of TColor;
  TPointArray = array of TPoint;
  TIntPointMap = specialize TFPGMap<Integer,TPointArray>;
  TCardDataArray = array of TCardData;

  //Used in day 5 puzzle
  TRangeConverterArray = array of TRangeConverter;
  TRangeArray = array of TRange;

  { TIntArrayHelper }

  TIntArrayHelper = type helper for TIntArray
  function size: integer;
  function push(element:integer):integer;
  function indexOf(element:integer):integer;
  function shift:integer;
  function splice(index:integer; deleteCount:integer=0; newItems: TIntArray=nil):TIntArray;
  end;

  { TInt64ArrayHelper }

  TInt64ArrayHelper = type helper for TInt64Array
  function size: integer;
  function push(element:int64):integer;
  function indexOf(element:int64):integer;
  function shift:int64;
  function splice(index:integer; deleteCount:integer=0;newItems: TInt64Array=nil):TInt64Array;
  end;

  { TStringArrayHelper }
  TStringArrayHelper = type helper for TStringArray
  function size: integer;
  function push(element: string):integer;
  function indexOf(element:string):integer;
  function splice(index:integer; deleteCount: integer=0; newItems: TStringArray=nil):TStringArray;
  end;

  { TPointArrayHelper }
  TPointArrayHelper = type helper for TPointArray
  function size: integer;
  function push(element: TPoint):integer;
  function indexOf(element:TPoint):integer;
  function splice(index:integer; deleteCount: integer=0; newItems: TPointArray=nil):TPointArray;
  end;

  { T3DIntMapHelper }
  T3DIntMapHelper = type helper for T3DIntMap
  function max(xStart,xEnd,yStart,yEnd,zStart,zEnd:integer):integer;
  function min(xStart,xEnd,yStart,yEnd,zStart,zEnd:integer):integer;
  end;

  { T2DInt64MapHelper }
  T2DInt64MapHelper = type helper for T2DInt64Map
  function rows:integer;
  function size(row:integer):integer;
  function push(row:integer;value:int64):integer;
  function getLast(row:integer):int64;
  end;

  { TIntPointMap }
  
  { TIntPointMapHelper }

  TIntPointMapHelper = type helper for TIntPointMap
  function included(itemToFind:TPoint):boolean;
  procedure addItem(item:TPoint);
  end;

  { TCardDataArrayHelper }
  TCardDataArrayHelper = type helper for TCardDataArray
  function size: integer;
  function push(element: TCardData):integer;
  function indexOf(element:TCardData):integer;
  end;

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


function removeBlankEntriesFromArray(arrInput: TIntArray):TIntArray;
function toIntArray(arrInput: TStringArray):TIntArray;
function containsCharacters(toSearch,toFind:String):boolean;
function intArrayToCSV(input:TIntArray):string;
function CSVToIntArray(input:string):TIntArray;
function CSVToInt64Array(input:string):TInt64Array;
procedure sort(var arr: array of Integer; count: Integer; ascending:boolean=true);
procedure sort(var arr: array of int64; count: Integer; ascending:boolean=true);
procedure sort(var arr: array of string; count: Integer; ascending:boolean=true);
procedure sort(var str: string; count: Integer;ascending:boolean=true);
procedure sort(var arr: array of char; count: Integer; ascending:boolean=true);
procedure sort(var arr: array of TPoint;count: Integer;ascending:boolean=true);
procedure sort(var arr: array of TRangeConverter; count: Integer; ascending: boolean=true);
procedure sortRangeArray(var arr: array of TRange; count:Integer; ascending: boolean=true);
implementation

const strChars: string = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';

function removeBlankEntriesFromArray(arrInput: TStringArray): TStringArray;
var
  index: integer;
begin
  index:=length(arrInput);
  for index:= pred(length(arrInput)) downto 0 do
    begin
      if (length(arrInput[index]) = 0) then
        arrInput.splice(index,1);
    end;
  result:=arrInput;
end;

function removeBlankEntriesFromArray(arrInput: TIntArray): TIntArray;
var
  index: integer;
begin
  index:=length(arrInput);
  for index:= pred(length(arrInput)) downto 0 do
    try
     arrInput[index].ToString;
    except
     arrInput.splice(index,1);
    end;
  result:=arrInput;
end;

function toIntArray(arrInput: TStringArray): TIntArray;
//converts the elements of the array to integers if possible
//probably should throw
var
  index:integer;
  output:TIntArray;
begin
  output:=TIntArray.create;
  setLength(output,length(arrInput));
  if length(arrInput) = 0 then
    begin
    result:=output;
    exit;
    end;
  for index:=0 to pred(length(arrInput)) do
    begin
      try
      output[index]:=strToInt(arrInput[index]);
      except
      //do nothing atm
      end;
    end;
  result:=output;
end;

function containsCharacters(toSearch, toFind: String): boolean;
var
  index:integer;
  thisChar:string;
begin
  result:=true;
  for index:=1 to length(toFind)do
    begin
      thisChar:=tofind[index];
      if not toSearch.Contains(thisChar) then
        begin
        result:=false;
        exit;
        end;
    end;
end;

function CompareInt64Asc(const d1,d2): integer;
var
  i1 : int64 absolute d1;
  i2 : int64 absolute d2;
begin
  if i1=i2 then Result:=0
  else if i1<i2 then Result:=-1
  else Result:=1;
end;
function CompareInt64Desc(const d1,d2): integer;
var
  i1 : int64 absolute d1;
  i2 : int64 absolute d2;
begin
  if i1=i2 then Result:=0
  else if i1>i2 then Result:=-1
  else Result:=1;
end;

function CompareIntAsc(const d1,d2): integer;
var
  i1 : integer absolute d1;
  i2 : integer absolute d2;
begin
  if i1=i2 then Result:=0
  else if i1<i2 then Result:=-1
  else Result:=1;
end;

function CompareIntDesc(const d1,d2): integer;
var
  i1 : integer absolute d1;
  i2 : integer absolute d2;
begin
  if i1=i2 then Result:=0
  else if i1>i2 then Result:=-1
  else Result:=1;
end;

function CompareStrAsc(const d1,d2): integer;
var
  s1 : string absolute d1;
  s2 : string absolute d2;
  shortestStringLength,strIndex:integer;
  comparison:integer;
  done:boolean;
begin
  shortestStringLength:=length(s1);
  if (length(s2)<length(s1))then shortestStringLength:=length(s2);
  strIndex:=0;
    repeat
    comparison:= strChars.IndexOf(s2.Substring(strIndex,1))
    - strChars.indexOf(s1.Substring(strIndex,1));
    strIndex:=strIndex+1;
    done:=(strIndex > pred(shortestStringLength)) or (comparison <> 0);
    until done;
    if (comparison = 0)then result:=0
      else result:=comparison div abs(comparison);
end;

function CompareStrDesc(const d1,d2): integer;
var
  s1 : string absolute d1;
  s2 : string absolute d2;
  shortestStringLength,strIndex:integer;
  comparison:integer;
  done:boolean;
begin
  shortestStringLength:=length(s1);
  if (length(s2)<length(s1))then shortestStringLength:=length(s2);
  strIndex:=0;
    repeat
    comparison:= strChars.IndexOf(s1.Substring(strIndex,1))
    - strChars.indexOf(s2.Substring(strIndex,1));
    strIndex:=strIndex+1;
    done:=(strIndex > pred(shortestStringLength)) or (comparison <> 0);
    until done;
    if (comparison = 0)then result:=0
      else result:=comparison div abs(comparison);
end;

function CompareCharAsc(const d1,d2): integer;
var
  s1 : char absolute d1;
  s2 : char absolute d2;
  comparison:integer;
begin
  comparison:= strChars.IndexOf(s1) - strChars.indexOf(s2);
  if (comparison = 0)then result:=0
    else result:=comparison div abs(comparison);
end;

function CompareCharDesc(const d1,d2): integer;
var
  s1 : char absolute d1;
  s2 : char absolute d2;
  comparison:integer;
begin
  comparison:= strChars.IndexOf(s2) - strChars.indexOf(s1);
  if (comparison = 0)then result:=0
    else result:=comparison div abs(comparison);
end;

function ComparePointAsc(const d1,d2):integer;
var
  s1: TPoint absolute d1;
  s2: TPoint absolute d2;
  comparison:integer;
begin
  comparison:= s1.X - s2.X;
  if comparison = 0 then comparison:= s1.Y - s2.Y;
  if comparison = 0 then result:=0
    else result:= comparison div abs(comparison);
end;

function ComparePointDesc(const d1,d2):integer;
var
  s1: TPoint absolute d1;
  s2: TPoint absolute d2;
  comparison:integer;
begin
  comparison:= s2.X - s1.X;
  if comparison = 0 then comparison:= s2.Y - s1.Y;
  if comparison = 0 then result:=0
    else result:= comparison div abs(comparison);
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


function intArrayToCSV(input: TIntArray): string;
var
  index:integer;
  output:string;
begin
  output:='';
  for index:= 0 to pred(length(input)) do
    begin
    output:=output+input[index].ToString;
    if index < pred(length(input)) then
      output:=output + ',';
    end;
  result:=output;
end;

function CSVToIntArray(input: string): TIntArray;
var
  strArray:TStringArray;
  index:integer;
begin
  //should check these are integers
  result:=TIntArray.create;
  strArray:=input.Split(',');
  setLength(result,length(strArray));
  for index:=0 to pred(length(strArray)) do
    result[index]:= strArray[index].ToInteger;
end;

function CSVToInt64Array(input: string): TInt64Array;
var
  strArray:TStringArray;
  index:integer;
begin
  result:=TInt64Array.create;
  strArray:=input.Split(',');
  setLength(result,length(strArray));
  for index:=0 to pred(length(strArray)) do
    result[index]:= strArray[index].ToInt64;
end;

procedure sort(var arr: array of Integer; count: Integer;ascending:boolean=true);
begin
  if ascending then
    anysort.AnySort(arr, Count, sizeof(Integer), @CompareIntAsc)
  else
    anysort.AnySort(arr, Count, sizeof(Integer), @CompareIntDesc)
end;

procedure sort(var arr: array of int64; count: Integer; ascending: boolean);
begin
 if ascending then
    anysort.AnySort(arr, Count, sizeof(Int64), @CompareInt64Asc)
  else
    anysort.AnySort(arr, Count, sizeof(Int64), @CompareInt64Desc)
end;

procedure sort(var arr: array of string; count: Integer; ascending: boolean);
begin
 if ascending then
    anysort.AnySort(arr, Count, sizeof(string), @CompareStrAsc)
  else
    anysort.AnySort(arr, Count, sizeof(string), @CompareStrDesc)
end;

procedure sort(var arr: array of char; count: Integer; ascending: boolean);
begin
 if ascending then
    anysort.AnySort(arr, Count, sizeof(char), @CompareCharAsc)
  else
    anysort.AnySort(arr, Count, sizeof(char), @CompareCharDesc)
end;

procedure sort(var arr: array of TPoint; count: Integer; ascending: boolean);
begin
  if ascending then
    anysort.AnySort(arr, Count, sizeof(TPoint), @ComparePointAsc)
  else
    anysort.AnySort(arr, Count, sizeof(TPoint), @ComparePointDesc)
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

procedure sort(var str: string; count: Integer; ascending: boolean);
var
  charArray:TCharArray;
  index:integer;
  output:string;
begin
  charArray:=str.ToCharArray;
  sort(charArray,count,ascending);
  //convert it back to a string. If there's a method for this I can't find it.
  output:='';
  for index:=0 to pred(length(charArray)) do
    output:=output+charArray[index];
  str:=output;
end;
{ Generic functions for arrays }

generic function GetIndex<T>(aItem:T; aArr: specialize TArray<T>): SizeInt;
begin
  for Result := 0 to High(aArr) do
    if aArr[Result] = aItem then
      Exit;
  Result := -1;
end;

generic function splice<T>(var aArray: specialize TArray<T>; index, deleteCount: sizeInt; var newItems: specialize TArray<T>
  ): specialize TArray<T>;
var
  normalizedCount, normalizedIndex, adjustIndex:sizeInt;
begin
 result:= specialize TArray<T>.create;
 //if index is greater than or equal to the size of the array then adjust it
  if (index > high(aArray)) then normalizedIndex:= high(aArray)
    else normalizedIndex:= index;
  //TODO - if index is negative should start at end of array

  //if the delete normalizedCount would take us off the end of the array then adjust it
  if (deleteCount > length(aArray) - normalizedIndex) then
    normalizedCount:= length(aArray) - normalizedIndex
      else normalizedCount:= deleteCount;

   if(deleteCount > 0) then
     begin
     //add the items that are to be deleted to the result array
     for adjustIndex:=normalizedIndex to normalizedIndex + pred(normalizedCount) do
       insert(aArray[adjustIndex],result,length(result));
     for adjustIndex:= normalizedIndex to pred(length(aArray) - normalizedCount) do
       aArray[adjustIndex]:= aArray[adjustIndex + normalizedCount];
     setLength(aArray, length(aArray) - normalizedCount);
     end;

   if (newItems <> nil) then
     begin
     setLength(aArray, length(aArray) + length(newItems));

     for adjustIndex:= high(aArray) downTo normalizedIndex + 1 do
       aArray[adjustIndex]:= aArray[adjustIndex - length(newItems)];

     for adjustIndex:= 0 to high(newItems) do
       aArray[index+adjustIndex]:= newItems[adjustIndex];
     end;
end;

{ T2DInt64MapHelper }

function T2DInt64MapHelper.rows: integer;
begin
  result:=length(self);
end;

function T2DInt64MapHelper.size(row: integer): integer;
begin
  result:=0;
  if (self.rows <= row) then exit;
  result:=length(self[row]);
end;

function T2DInt64MapHelper.push(row: integer; value: int64): integer;
begin
  //if the number of rows does not match then create
  if (self.rows < row) then setLength(self,rows+1);
  setLength(self[row],self.size(row)+1);
  self[row][self.size(row -1)]:=value;
  result:=self.size(row);
end;

function T2DInt64MapHelper.getLast(row: integer): int64;
begin
  result:=0;
  if self.rows <= row then exit;
  result:=self[row][self.size(row) - 1];
end;

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

{ TCardDataArrayHelper }

function TCardDataArrayHelper.size: integer;
begin
  result:=length(self);
end;

function TCardDataArrayHelper.push(element: TCardData): integer;
begin
  insert(element,self,length(self));
  result:=self.size;
end;

function TCardDataArrayHelper.indexOf(element: TCardData): integer;
begin
  result:= specialize getIndex < TCardData>(element,self);
end;

{ TIntPointMapHelper }

function TIntPointMapHelper.included(itemToFind: TPoint): boolean;
var
  points:TPointArray;
begin
  if self.TryGetData(itemToFind.X,points) = false then
    begin
    result:=false;
    exit;
    end;
  result:=points.indexOf(itemToFind) > -1;
end;

procedure TIntPointMapHelper.addItem(item: TPoint);
var
  keyIndex:integer;
  points:TPointArray;
begin
  //if the x value does not exist as a key then add it
  if self.Find(item.X,keyIndex) then
  points:= self.Data[keyIndex]
  else points:=TPointArray.create;

  points.push(item);
  self.AddOrSetData(item.X,points);
end;


{ T3DIntMapHelper }

function T3DIntMapHelper.max(xStart, xEnd, yStart, yEnd, zStart, zEnd: integer
  ): integer;
var
  x,y,z:integer;
begin
  result:=self[xStart][yStart][zStart];
  for x:=xStart to xEnd do
    for y:=yStart to yEnd do
      for z:= zStart to zEnd do
        try
          if (self[x][y][z]) > result then result:= self[x][y][z];
        finally
          //do nothing - if the contents of the array isn't an integer ignore
        end;
end;

function T3DIntMapHelper.min(xStart, xEnd, yStart, yEnd, zStart, zEnd: integer
  ): integer;
var
  x,y,z:integer;
begin
  result:=self[xStart][yStart][zStart];
  for x:=xStart to xEnd do
    for y:=yStart to yEnd do
      for z:= zStart to zEnd do
        try
          if (self[x][y][z]) < result then result:= self[x][y][z];
        finally
          //do nothing - if the contents of the array isn't an integer ignore
        end;
end;

{ TPointArrayHelper }

function TPointArrayHelper.size: integer;
begin
  result:=length(self);
end;

function TPointArrayHelper.push(element: TPoint): integer;
begin
  insert(element,self,length(self));
  result:=self.size;
end;

function TPointArrayHelper.indexOf(element: TPoint): integer;
begin
  result:= specialize getIndex<TPoint>(element,self);
end;

function TPointArrayHelper.splice(index: integer; deleteCount: integer;
  newItems: TPointArray): TPointArray;
begin
  result:= specialize splice<TPoint>(self,index,deleteCount,newItems);
end;

{ TStringArrayHelper }

function TStringArrayHelper.size: integer;
begin
  result:= length(self);
end;

function TStringArrayHelper.push(element: string): integer;
begin
  insert(element,self,length(self));
  result:=self.size;
end;

function TStringArrayHelper.indexOf(element: string): integer;
begin
  result:= specialize getIndex<string>(element,self);
end;

function TStringArrayHelper.splice(index: integer; deleteCount: integer;
  newItems: TStringArray): TStringArray;
begin
  result:= specialize splice<string>(self,index,deleteCount, newItems);
end;



{ TInt64ArrayHelper }

function TInt64ArrayHelper.size: integer;
begin
  result:= length(self);
end;

function TInt64ArrayHelper.push(element: int64): integer;
begin
  insert(element,self,length(self));
  result:=self.size;
end;

function TInt64ArrayHelper.indexOf(element: int64): integer;
begin
  result:= specialize getIndex<int64>(element,self);
end;

function TInt64ArrayHelper.shift: int64;
var
  index:integer;
begin
  if (self.size > 0) then
    begin
    result:=self[0];
    if (self.size > 1) then for index:= 0 to pred(self.size) do
      self[index]:=self[index+1];
    setLength(self,pred(self.size));
    end;
end;

function TInt64ArrayHelper.splice(index: integer; deleteCount: integer;
  newItems: TInt64Array): TInt64Array;
begin
  result:= specialize splice<int64>(self,index,deleteCount,newItems);
end;

{ TIntArrayHelper }

function TIntArrayHelper.size: integer;
begin
  result:=length(self);
end;

function TIntArrayHelper.push(element: integer): integer;
begin
  insert(element,self,length(self));
  result:=self.size;
end;

function TIntArrayHelper.indexOf(element: integer): integer;
begin
  result:= specialize getIndex<integer>(element,self);
end;

function TIntArrayHelper.shift: integer;
var
  index:integer;
begin
  if (self.size > 0) then
    begin
    result:=self[0];
    if (self.size > 1) then for index:= 0 to pred(self.size) do
      self[index]:=self[index+1];
    setLength(self,pred(self.size));
    end;
end;

function TIntArrayHelper.splice(index: integer; deleteCount: integer;
  newItems: TIntArray): TIntArray;
begin
 result:= specialize splice<integer>(self,index,deleteCount,newItems);
end;

end.

