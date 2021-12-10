unit arrayUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,anysort,graphics;
type
  //Looks like the built in TintegerArray is a static array
  //so let's define our own dynamic integer array
  TIntArray = array of integer;
  //used in day 9 part two
  T3DIntMap = array of array of array of integer;
  TColours = array of TColor;
procedure addToArray(var arrInput:TStringArray; item:string;index:integer=-1);
procedure addToArray(var arrInput:TIntArray;item:integer;index:integer=-1);
function deleteFromArray(var arrInput:TStringArray; index: integer):string;
function deleteFromArray(var arrInput:TIntArray; index: integer):integer;
function removeBlankEntriesFromArray(arrInput: TStringArray):TStringArray;
function removeBlankEntriesFromArray(arrInput: TIntArray):TIntArray;
function toIntArray(arrInput: TStringArray):TIntArray;
function arrPos(arrInput:TIntArray; element:integer):integer;
function arrPos(arrInput:TStringArray; element:string):integer;
function containsCharacters(toSearch,toFind:String):boolean;
procedure sort(var arr: array of Integer; count: Integer; ascending:boolean=true);
procedure sort(var str: string; count: Integer;ascending:boolean=true);
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

//used in day 8 part 2. String has a .contains method but we
//can't assume the characters in the substring will be in the
//same order in the string we're searching
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

//Comparator functions for integers

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

//Comparator functions for strings: need to test behaviour

function CompareStrAsc(const d1,d2): integer;
var
  s1 : string absolute d1;
  s2 : string absolute d2;
  i1,i2:integer;
begin
  i1:=strChars.IndexOf(s1);
  i2:=strChars.indexOf(s2);
  if i1=i2 then Result:=0
  else if i1<i2 then Result:=-1
  else Result:=1;
end;

function CompareStrDesc(const d1,d2): integer;
var
  s1 : string absolute d1;
  s2 : string absolute d2;
  i1,i2:integer;
begin
  i1:=strChars.IndexOf(s1);
  i2:=strChars.indexOf(s2);
  if i1=i2 then Result:=0
  else if i1>i2 then Result:=-1
  else Result:=1;
end;

procedure sort(var arr: array of Integer; count: Integer;ascending:boolean=true);
begin
  if ascending then
    anysort.AnySort(arr, Count, sizeof(Integer), @CompareIntAsc)
  else
    anysort.AnySort(arr, Count, sizeof(Integer), @CompareIntDesc)
end;

procedure sort(var str: string; count: Integer; ascending: boolean);
var
  index,swapIndex:integer;
  swap:char;
  i1,i2:integer;
  doSwap:boolean;
begin
  if (count <=1) then exit;
  //start at 1. for each following item, if it's less than that element then swap
  index:=1;
  repeat
  for swapIndex:=index+1 to length(str)do
    begin
    i1:=strChars.IndexOf(str[index]);
    i2:=strChars.IndexOf(str[swapIndex]);
    doSwap:= (ascending and(i2 < i1)) or (not ascending and (i2>i1));
    if doSwap then
      begin
      swap:=str[index];
      str[index]:=str[swapIndex];
      str[swapIndex]:=swap;
      end;
    end;
  index:=index + 1;
  until index > length(str)-1;
end;

end.

