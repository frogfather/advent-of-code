unit aocUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,fileUtilities,math;
function getPuzzleInputAsStringArray(fileName: String; removeBlankLines: boolean=true): TStringArray;
function getPuzzleInputAsIntArray(fileName: String; removeBlankLines: boolean=true): TIntArray;
function stringOfBinaryToInteger(input: String): integer;
function calculateCommonestValue(input: TStringArray; reverse:Boolean=false): TBits;
implementation
const dataDir: string = '/Users/cloudsoft/Code/advent-of-code/2021/input/';


//Where the puzzle input is lines separated by linefeed (#$0A)
function getPuzzleInputAsStringArray(fileName: String; removeBlankLines: boolean=true): TStringArray;
begin
  result:= openFileAsArray(datadir+filename,#$0A,removeBlankLines);
end;

//Where the puzzle input is a single line of comma separated numbers
function getPuzzleInputAsIntArray(fileName: String; removeBlankLines: boolean
  ): TIntArray;
var
 fileLines:TStringArray;
begin
 fileLines:= openFileAsArray(datadir+filename,#$0A,removeBlankLines);
 if (length(fileLines)=1) then
 result:= toIntArray(fileLines[0].Split(','));
end;

function stringOfBinaryToInteger(input: String): integer;
var
 index,powerOf,elementLength:Integer;
 output:double;
begin
  elementLength:=length(input);
  output:=0;
  for index:= 1 to elementLength do
      begin
      powerOf:=elementLength - index;
      if (input[index]='1') then output:=output + power(2,powerOf);
      end;
  result:=round(output);
end;

function calculateCommonestValue(input: TStringArray; reverse:Boolean=false): TBits;
//used in day 3
type
  TintArray = array of integer;
 var
   element, index, elementLength:integer;
   intArray: TintArray;
   bBits:TBits;
begin
  elementLength:=length(input[0]);
  //Initialize array with all zeros
  intArray:=TintArray.create;
  setLength(intArray,elementLength);
  bBits:=TBits.create(elementLength);
  for index:= 0 to pred(length(intArray)) do
    begin
    intArray[index]:=0;
    end;
  for element:=0 to pred(length(input)) do
    begin
    for index:=0 to pred(length(intArray)) do
      //NB strings are indexed from 1!
      begin
      if (input[element][index+1] = '1')
        then intArray[index]:=intArray[index]+1;
      end;
    end;
    for index:= 0 to pred(elementLength) do
      begin
      //are more than half the entries 1s?
      if ((intArray[index] * 2) >= length(input))
        then bBits[index]:=true else bBits[index]:=false;
      if reverse then bBits[index]:=not bBits[index];
      end;
    result:= bBits;
end;

end.

