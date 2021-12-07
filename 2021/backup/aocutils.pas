unit aocUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,fileUtilities;

implementation
const dataDir: string = '/Users/cloudsoft/Code/advent-of-code/2021/input/';

function getPuzzleInputAsStringArray(fileName: String; removeBlankLines: boolean=true): TStringArray;
begin
  result:= fileUtilities.openFileAsArray(datadir+filename,#$0A,removeBlankLines);
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

