unit aocUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,fileUtilities,math,arrayUtils;

function getPuzzleInputAsStringArray(fileName: String; removeBlankLines: boolean=true): TStringArray;
function getPuzzleInputAsIntArray(fileName: String; removeBlankLines: boolean=true): TIntArray;
function getPuzzleInputAsString(fileName:string; removeBlankLines: boolean=true):string;
function stringOfBinaryToInteger(input: String): integer;
function calculateCommonestValue(input: TStringArray; reverse:Boolean=false): TBits;
function getUniqueEntry(input: TStringArray;reverse:boolean=false):String;
function getMaxValue(input:TIntArray):integer;
function getDescription(fileName:String):String;
function hexStringToBinString(hexString:string):string;
function binStringToInteger(binString:string):integer;
implementation

const dataDir: string = '/Users/cloudsoft/Code/advent-of-code/2021/input/';
const descriptionDir: string = '/Users/cloudsoft/Code/advent-of-code/2021/puzzle_description/';


//For where the puzzle input is lines separated by linefeed (#$0A)
function getPuzzleInputAsStringArray(fileName: String; removeBlankLines: boolean=true): TStringArray;
begin
  result:= openFileAsArray(datadir+filename,#$0A,removeBlankLines);
end;

//For where the puzzle input is a single line of comma separated numbers
function getPuzzleInputAsIntArray(
  fileName:String;
  removeBlankLines: boolean): TIntArray;
var
 fileLines:TStringArray;
begin
 fileLines:= openFileAsArray(datadir+filename,#$0A,removeBlankLines);
 if (length(fileLines)=1) then
 result:= toIntArray(fileLines[0].Split(','));
end;

function getPuzzleInputAsString(fileName: string; removeBlankLines: boolean
  ): string;
begin
  result:=fileUtilities.readStream(datadir+fileName);
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

//used in day 3
function calculateCommonestValue(input: TStringArray; reverse:Boolean=false): TBits;
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

function getUniqueEntry(input: TStringArray;reverse:boolean=false):String;
//Deletes entries from the input that don't match the pattern of
//1s and 0s
var
  entry,entryLength,element:integer;
  mostOnesAt: TBits;
  keepValue: integer;
begin
if length(input) = 0 then exit;
entryLength:=length(input[0]);
for element:=0 to pred(entryLength) do
  begin
  //Get the TBits object which tells us if 1
  //is the most common value at each index for the current set
  mostOnesAt:=calculateCommonestValue(input,reverse);
  if (mostOnesAt[element] = true) then keepValue:=1 else keepValue:=0;
  for entry:=pred(length(input)) downto 0 do
    begin
    if (strToInt(input[entry][element+1]) <> keepValue)
    then deleteFromArray(input,entry);
    if (length(input)=1) then
      begin
      result:=input[0];
      exit;
      end;
    end;
  end;
end;

//Used in Day 7
function getMaxValue(input:TIntArray):integer;
var
   index:integer;
begin
result:=0;
for index:=0 to pred(length(input)) do
  begin
  if (input[index] > result) then result:=input[index];
  end;
end;

function getDescription(fileName: String): String;
begin
  try
    result:=readStream(descriptionDir+fileName);
  except
    result:='No description for this puzzle'
  end;
end;

function hexStringToBinString(hexString: string): string;
var
  inputPos:integer;
  output:string;
  element:Char;
  value:integer;
  binValue:string;
  valid:boolean;
begin
  output:='';
  binValue:='';
  for inputPos:=1 to length(hexString) do
    begin
    valid:=true;
    element:=hexString[inputPos];
    if element IN ['A'..'F','a'..'f']
      then value:=(ord(element)+9) and 15
    else if element IN ['0'..'9']
      then value:= (ord(element)) and 15
    else valid := false;
    //convert to binary
    if valid then
      begin
      binValue:=binValue+(value div 8).ToString;
      if value >= 8 then value:=value - 8;
      binValue:=binValue+(value div 4).ToString;
      if value >= 4 then value:=value - 4;
      binValue:=binValue+(value div 2).ToString;
      if value >= 2 then value:=value - 2;
      binValue:=binValue+(value).ToString;
      output:=output + binValue;
      binValue:='';
      end;
    end;
  result:=output;
end;

function binStringToInteger(binString: string): integer;
var
  index,pwr,output:integer;
  element:string;
begin
output:=0;
  for index:=0 to pred(length(binString)) do
    begin
    element:=binString.Substring(index,1);
    pwr:=length(binString)-(index+1);
    output:=output + (element.ToInteger * round(power(2,pwr)));
    end;
  result:=output;
end;

end.

