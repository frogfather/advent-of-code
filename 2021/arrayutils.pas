unit arrayUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
type
  //Looks like the built in TintegerArray is a static array
  //so let's define our own dynamic integer array
  TIntArray = Array of integer;

procedure addToArray(var arrInput:TStringArray; item:string;index:integer=-1);
procedure addToArray(var arrInput:TIntArray;item:integer;index:integer=-1);
procedure deleteFromArray(var arrInput:TStringArray; index: integer);
procedure deleteFromArray(var arrInput:TIntArray; index: integer);
function removeBlankEntriesFromArray(arrInput: TStringArray):TStringArray;
function removeBlankEntriesFromArray(arrInput: TIntArray):TIntArray;
function toIntArray(arrInput: TStringArray):TIntArray;
function arrPos(arrInput:TIntArray; element:integer):integer;
implementation

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

procedure deleteFromArray(var arrInput: TStringArray; index: integer);
var
  position:integer;
begin
  if (index < 0) or (index >= length(arrInput)) then exit;
  for position:=index to length(arrInput) - 1 do
    begin
      if (position+1 < length(arrInput))
        then arrInput[position]:=arrInput[position + 1];
    end;
  setLength(arrInput, length(arrInput) -1);
end;

procedure deleteFromArray(var arrInput: TIntArray; index: integer);
var
  position:integer;
begin
 if (index < 0) or (index >= length(arrInput)) then exit;
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

end.

