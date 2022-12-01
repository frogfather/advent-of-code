unit day1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type
  
  { TDayOne }
  TDayOne = class(TAocPuzzle)
  private
  fName:string;
  function processAndSort:TIntArray;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayOne }

constructor TDayOne.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,paintbox_);
fName:= 'Day 1';
//parent loads the file as a string;
end;

function TDayOne.processAndSort:TIntArray;
var
  inputAsLines:TStringArray;
  index,currentElfFood:integer;
begin
  result:=TIntArray.Create;
  results.Clear;
  currentElfFood:=0;
  inputAsLines:=puzzleInput.Split(#$0A); //split on newline
  for index:= 0 to pred(length(inputAsLines)) do
    begin
    if (inputAsLines[index]<> '')
      then currentElfFood:= currentElfFood + inputAsLines[index].ToInteger
      else
        begin
          result.push(currentElfFood);
          currentElfFood:=0;
        end;
    end;
  sort(result,result.size,false);
end;

//We have a list of groups of numbers separated by blank lines
//We want to add each group together and then sort to find the largest
procedure TDayOne.runPartOne;

begin

  results.Add('Elf with most calories has '+processAndSort[0].ToString);
end;

//Astonishingly I've done part one in a way that makes part two easy
procedure TDayOne.runPartTwo;
begin

end;

end.

