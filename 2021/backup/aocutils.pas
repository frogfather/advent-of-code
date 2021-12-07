unit aocUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,fileUtilities,math;
function getPuzzleInputAsStringArray(fileName: String; removeBlankLines: boolean=true): TStringArray;
function getPuzzleInputAsIntArray(fileName: String; removeBlankLines: boolean=true): TStringArray;
function stringOfBinaryToInteger(input: String): integer;
implementation
const dataDir: string = '/Users/cloudsoft/Code/advent-of-code/2021/input/';


//Where the puzzle input is lines separated by linefeed (#$0A)
function getPuzzleInputAsStringArray(fileName: String; removeBlankLines: boolean=true): TStringArray;
begin
  result:= fileUtilities.openFileAsArray(datadir+filename,#$0A,removeBlankLines);
end;

//Where the puzzle input is a single line of comma separated numbers
function getPuzzleInputAsIntArray(fileName: String; removeBlankLines: boolean
  ): TIntArray;
var
 fileLines:TStringArray;
begin
 fileLines:=fileUtilities.openFileAsArray(datadir+filename,#$0A,removeBlankLines);
 if (length(fileLines)=1) then
 result:=fileUtilities.toIntArray(fileLines[0].Split(','));
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
end.

