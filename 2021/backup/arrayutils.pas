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
function removeBlankLinesFromArray(arrInput: TStringArray):TStringArray;
function removeBlankLinesFromArray(arrInput: TIntArray):TIntArray;
function toIntArray(arrInput: TStringArray):TIntArray;
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
end.

