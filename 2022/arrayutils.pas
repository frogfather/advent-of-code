unit arrayUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,anysort,graphics,fgl;
type
  //Looks like the built in TintegerArray is a static array
  //so let's define our own dynamic integer array
  TIntArray = array of integer;
  TInt64Array = array of int64;
  TStringMap = specialize TFPGMap<String,String>;
  TStringIntMap = specialize TFPGMap<String,Integer>;
  TStringInt64Map = specialize TFPGMap<String,Int64>;
  T2DIntMap = array of array of integer;
  T3DIntMap = array of array of array of integer;
  T2DStringArray = array of array of string;
  TColours = array of TColor;

procedure addToArray(var arrInput:TStringArray; item:string;index:integer=-1);
procedure addToArray(var arrInput:TIntArray;item:integer;index:integer=-1);
procedure addToArray(var arrInput:TInt64Array;item:int64;index:integer=-1);
function deleteFromArray(var arrInput:TStringArray; index: integer):string;
function deleteFromArray(var arrInput:TIntArray; index: integer):integer;
function removeBlankEntriesFromArray(arrInput: TIntArray):TIntArray;
function toIntArray(arrInput: TStringArray):TIntArray;
function arrPos(arrInput:TIntArray; element:integer):integer;
function arrPos(arrInput:TStringArray; element:string):integer;
function containsCharacters(toSearch,toFind:String):boolean;
procedure sort(var arr: array of Integer; count: Integer; ascending:boolean=true);
procedure sort(var arr: array of int64; count: Integer; ascending:boolean=true);
procedure sort(var arr: array of string; count: Integer; ascending:boolean=true);
procedure sort(var str: string; count: Integer;ascending:boolean=true);
procedure sort(var arr: array of char; count: Integer; ascending:boolean=true);
implementation

const strChars: string = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';

procedure addToArray(var arrInput: TStringArray; item: string; index: integer);
var
  pos,lastItemIndex:integer;
begin
  //if index is -1 add at the end
  setLength(arrInput,length(arrInput)+1);
  lastItemIndex:= pred(length(arrInput));
  if (index = -1) then index:=lastItemIndex; //insert at end
  for pos:=lastItemIndex downto index do
    begin
    if (pos > 0) then arrInput[pos]:=arrInput[pos-1];
    end;
  arrInput[index]:=item;
end;

procedure addToArray(var arrInput: TIntArray; item: integer; index: integer
  );
var
  pos,lastItemIndex:integer;
begin
  //if index is -1 add at the end
  setLength(arrInput,length(arrInput)+1);
  lastItemIndex:= pred(length(arrInput));
  if (index = -1) then index:=lastItemIndex; //insert at end
  for pos:=lastItemIndex downto index do
    begin
    if (pos > 0) then arrInput[pos]:=arrInput[pos-1];
    end;
  arrInput[index]:=item;
end;

procedure addToArray(var arrInput: TInt64Array; item: int64; index: integer);
var
  pos,lastItemIndex:integer;
begin
  //if index is -1 add at the end
  setLength(arrInput,length(arrInput)+1);
  lastItemIndex:= pred(length(arrInput));
  if (index = -1) then index:=lastItemIndex; //insert at end
  for pos:=lastItemIndex downto index do
    begin
    if (pos > 0) then arrInput[pos]:=arrInput[pos-1];
    end;
  arrInput[lastItemIndex]:=item;
end;

function deleteFromArray(var arrInput: TStringArray; index: integer):string;
var
  position:integer;
begin
  result:='';
  if (index < 0) or (index >= length(arrInput)) then exit;
  result:=arrInput[index];
  for position:=index to length(arrInput) - 1 do
    begin
      if (position+1 < length(arrInput))
        then arrInput[position]:=arrInput[position + 1];
    end;
  setLength(arrInput, length(arrInput) -1);
end;

function deleteFromArray(var arrInput: TIntArray; index: integer):integer;
var
  position:integer;
begin
 if (index < 0) or (index >= length(arrInput)) then exit;
 result:=arrInput[index];
  for position:=index to length(arrInput) - 1 do
    begin
      if (position+1 < length(arrInput))
        then arrInput[position]:=arrInput[position + 1];
    end;
  setLength(arrInput, length(arrInput) -1);
end;

function removeBlankEntriesFromArray(arrInput: TStringArray): TStringArray;
var
  index: integer;
begin
  index:=length(arrInput);
  for index:= pred(length(arrInput)) downto 0 do
    begin
      if (length(arrInput[index]) = 0) then
        deleteFromArray(arrInput,index);
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
     deleteFromArray(arrInput,index);
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

function arrPos(arrInput: TIntArray; element: integer): integer;
var
  index:integer;
begin
  result:=-1;
  if length(arrInput) = 0 then exit;
  for index:=0 to pred(length(arrInput)) do
    begin
    if (arrInput[index] = element) then
      begin
      result:=index;
      exit;
      end;
    end;
end;

function arrPos(arrInput: TStringArray; element: string): integer;
var
  index:integer;
begin
  result:=-1;
  if length(arrInput) = 0 then exit;
  for index:=0 to pred(length(arrInput)) do
    begin
    if (arrInput[index] = element) then
      begin
      result:=index;
      exit;
      end;
    end;
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

end.

