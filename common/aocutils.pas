unit aocUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,fileUtilities,math,arrayUtils,HlpIHashInfo, HlpConverters, HlpHashFactory,DCPsha256;
function getPuzzleInputAsStringArray(fileName: String; removeBlankLines: boolean=true): TStringArray;
function getPuzzleInputAsIntArray(fileName: String; removeBlankLines: boolean=true): TIntArray;
function getPuzzleInputAsString(fileName:string):string;
function stringOfBinaryToInteger(input: String): integer;
function stringOfBinaryToInt64(input:string):int64;
function bitwiseXOR(arg1,arg2:string):string;
function calculateCommonestValue(input: TStringArray; reverse:Boolean=false): TBits;
function getUniqueEntry(input: TStringArray;reverse:boolean=false):String;
function getMaxValue(input:TIntArray):integer;
function getDescription(fileName:String):String;
function triangular(input:integer):integer;
function hexStringToBinString(hexString:string):string;
function binStringToInt64(binString:string):int64;
function findCharPos(input,substring:string;startIndex:integer=0):integer;
function isNumberString(input:string):boolean;
function getDimensionsOfPuzzleInput(input:TStringArray):TPoint;
function isNumber(element:char):boolean;
function isNumber(element:string):boolean;
function gcd(num1,num2:int64):int64;
function lcm(num1,num2:int64):int64;
function sha256(input:string):string;
implementation

const numbers: array[0..9] of integer = (0,1,2,3,4,5,6,7,8,9);

//For where the puzzle input is lines separated by linefeed (#$0A)
function getPuzzleInputAsStringArray(fileName: String; removeBlankLines: boolean=true): TStringArray;
begin
  result:= openFileAsArray(filename,#$0A,removeBlankLines);
end;

//For where the puzzle input is a single line of comma separated numbers
function getPuzzleInputAsIntArray(
  fileName:String;
  removeBlankLines: boolean): TIntArray;
var
 fileLines:TStringArray;
begin
 fileLines:= openFileAsArray(filename,#$0A,removeBlankLines);
 if (length(fileLines)=1) then
 result:= toIntArray(fileLines[0].Split(','));
end;

function getPuzzleInputAsString(fileName: string): string;
begin
  result:=fileUtilities.readStream(fileName);
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

function stringOfBinaryToInt64(input: string): int64;
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

function bitwiseXOR(arg1, arg2: string): string;
var
 index,longest:integer;
 a,b:string;
begin
 if (arg1.Length > arg2.Length)
   then longest:=arg1.Length
     else longest:=arg2.Length;
 result:='';
 for index:= 0 to pred(longest) do
     begin
     if (index < arg1.Length) then a:=arg1.Substring(index,1) else arg1:='0';
     if (index < arg2.Length) then b:=arg2.Substring(index,1) else arg2:='0';
     if (a=b)then result:=result+'0'else result:=result+'1';
     end;

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
    then input.splice(entry,1);
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
    result:=readStream(fileName);
  except
    result:='No description for this puzzle'
  end;
end;

function triangular(input: integer): integer;
begin
  result:= (input * (input + 1)) div 2;
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

function binStringToInt64(binString: string): int64;
var
  index,pwr:integer;
  output:int64;
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

function findCharPos(input, substring: string; startIndex: integer
  ): integer;
var
  p:integer;
  done:boolean;
begin
  if (startIndex < 0)
    or (startIndex > pred(length(input)))
    or (length(substring) = 0)
    then
      begin
      result:=-1;
      exit;
      end;
  p:=startIndex;
  done:=false;
  repeat
    if (input.Substring(p,1) = substring) then
    begin
      result:= p;
      exit;
    end;
  if p < pred(length(input))
    then p:=p+1
  else done:=true;
  until done;
end;

function isNumberString(input: string): boolean;
var
  index:integer;
begin
  result:=true;
  if length(input) = 0 then result:= false else
  for index:=1 to length(input) do
    if (ord(input[index]) < 48) or (ord(input[index]) > 57) then result:=false;
end;

function getDimensionsOfPuzzleInput(input: TStringArray): TPoint;
begin
  result.X:=0;
  result.Y:=0;
  if length(input) = 0 then exit;
  with result do
    begin
    Y:= length(input);
    X:= length(input[0]);
    end;
end;

//Is it between ascii 48 and 57?
function isNumber(element: char): boolean;
begin
  result:=(ord(element) > 47) and (ord(element) < 58);
end;

function isNumber(element: string): boolean;
begin
  result:= (element.Length = 1)and isNumber(element[1]);
end;

function gcd(num1, num2: int64): int64;
begin
  result:= min(num1,num2);
  while result > 0 do
    begin
    if (num1 mod result = 0) and (num2 mod result = 0) then break;
    result:=result - 1;
    end;
end;

function lcm(num1, num2: int64): int64;
begin
  if (num1 = 0) then result:=num2
  else if (num2 = 0) then result:= num1
  else result:= (num1 div gcd(num1, num2)) * num2;
end;

function sha256(input: string): string;
type
  TDigest = array[0..31] of byte;
var
    DCP_sha256_1:TDCP_Sha256;
    Digest: TDigest;
    i: integer;
    str1: string;
begin
    result:='';
    if input <> '' then
    begin
      FillByte(Digest, SizeOf(Digest), 0);
      DCP_sha256_1:= TDCP_sha256.Create(nil);
      DCP_sha256_1.Init;
      DCP_sha256_1.UpdateStr(input);
      DCP_sha256_1.Final(Digest);
      str1:= '';
      for i:= 0 to 31 do
        str1:= str1 + IntToHex(Digest[i],2);
      result:=str1;
    end;
end;

end.

