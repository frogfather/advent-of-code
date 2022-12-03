unit arrayUtils;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$modeswitch TypeHelpers}
interface

uses
  Classes, SysUtils,anysort,graphics,fgl;
type
  //Looks like the built in TintegerArray is a static array
  //so let's define our own dynamic integer array
  TCharArray = specialize Tarray<Char>;
  TIntArray = specialize Tarray<Integer>;
  TInt64Array = specialize Tarray<Int64>;
  TStringMap = specialize TFPGMap<String,String>;
  TStringIntMap = specialize TFPGMap<String,Integer>;
  TStringInt64Map = specialize TFPGMap<String,Int64>;
  T2DIntMap = array of array of integer;
  T3DIntMap = array of array of array of integer;
  T2DStringArray = array of array of string;
  TColours = array of TColor;
  TPointArray = array of TPoint;

  { TIntArrayHelper }

  TIntArrayHelper = type helper for TIntArray
  function size: integer;
  function push(element:integer):integer;
  function indexOf(element:integer):integer;
  function splice(index:integer; deleteCount:integer=0; newItems: TIntArray=nil):TIntArray;
  end;

  { TInt64ArrayHelper }

  TInt64ArrayHelper = type helper for TInt64Array
  function size: integer;
  function push(element:int64):integer;
  function indexOf(element:int64):integer;
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

function removeBlankEntriesFromArray(arrInput: TIntArray):TIntArray;
function toIntArray(arrInput: TStringArray):TIntArray;
function containsCharacters(toSearch,toFind:String):boolean;
function intArrayToCSV(input:TIntArray):string;
function CSVToIntArray(input:string):TIntArray;
procedure sort(var arr: array of Integer; count: Integer; ascending:boolean=true);
procedure sort(var arr: array of int64; count: Integer; ascending:boolean=true);
procedure sort(var arr: array of string; count: Integer; ascending:boolean=true);
procedure sort(var str: string; count: Integer;ascending:boolean=true);
procedure sort(var arr: array of char; count: Integer; ascending:boolean=true);
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
  output:TIntArray;
  index:integer;
begin
  //should check these are integers
  result:=nil;
  if pos(',',input) > 0 then
    begin
    strArray:=input.Split(',');
    output:=TIntArray.create;
    setLength(output,length(strArray));
    for index:=0 to pred(length(strArray)) do
      output[index]:= strArray[index].ToInteger;
    result:=output;
    end;
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
  if (deleteCount > high(aArray) - normalizedIndex) then
    normalizedCount:= high(aArray) - normalizedIndex
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

function TIntArrayHelper.splice(index, deleteCount: integer; newItems: TIntArray
  ): TIntArray;
begin
 result:= specialize splice<integer>(self,index,deleteCount,newItems);
end;

end.
