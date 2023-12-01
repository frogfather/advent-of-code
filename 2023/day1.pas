unit day1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayOne }
  TDayOne = class(TAocPuzzle)
  private
  function getFirstAndLastNumbers(input:String):String;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayOne }

constructor TDayOne.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 1',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayOne.runPartOne;
var
  lineNo:integer;
  sFirstAndLastNumbers:string;
  total:integer;
begin
  results.Clear;
  { #todo : A forEach method on TStringArray would be nice }
  //Strings consisting of alphanumerics
  //We want just the numbers
  total:=0;
  for lineNo:= 0 to pred(puzzleInputLines.size) do
    begin
    sFirstAndLastNumbers:= getFirstAndLastNumbers(puzzleInputLines[lineNo]);
    //We're only interested in the first and last numeric character
    total:=total + sFirstAndLastNumbers.ToInteger;
    end;
  results.Add('Total is '+total.ToString);
end;

procedure TDayOne.runPartTwo;
begin
  results.Clear;
end;

function TDayOne.getFirstAndLastNumbers(input: String): String;
var
  index:integer;
  firstFound,lastFound:boolean;

    function isNumber(element:char):boolean;
      begin
      result:=(ord(element) > 47) and (ord(element) < 58);
      end;

begin
  //Find the first character that is between ascii 48 and 57
  index:=1; //*** Your regular reminder that strings are 1 indexed *** !
  firstFound:=false;
  result:='';
  while not firstFound do
    begin
    if (index > input.Length) then exit; //Shouldn't happen but, well y'know...
    if isNumber(input[index]) then
      begin
        result:=result+input[index];
        break;
      end else index:=index+1;
    end;
  lastFound:=false;
  index:=input.length;
  while not lastFound do
    begin
    if (index = 0) then exit;
    if isNumber(input[index]) then
      begin
        result:=result+input[index];
        break;
      end else index:=pred(index);
    end;
end;

end.


