unit day4;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils,math;
type

  { TDayFour}
  TDayFour = class(TAocPuzzle)
  private
  function findOurWinningNumbers(winning_,toMatch_:TStringArray):TStringArray;
  function scoreForMatchingNumbers(input_:TStringArray):integer;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayFour }

constructor TDayFour.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 4',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayFour.runPartOne;
var
  total,lineNo:integer;
  allNumbers,winningNumbers,ourNumbers,matchingNumbers:TStringArray;
begin
  results.Clear;
  total:=0;
  for lineNo:=0 to pred(puzzleInputLines.size) do
    begin
    allNumbers:=puzzleInputLines[lineNo].Split([':','|']);
    winningNumbers:=allNumbers[1].Split([' '],(TstringSplitOptions.ExcludeEmpty));
    ourNumbers:=allNumbers[2].Split([' '],(TstringSplitOptions.ExcludeEmpty));
    matchingNumbers:=findOurWinningNumbers(winningNumbers,ourNumbers);
    total:=total+scoreForMatchingNumbers(matchingNumbers);
    end;
  results.Add('Total is '+total.ToString);
end;

procedure TDayFour.runPartTwo;
begin
  results.Clear;
end;

//---- methods for part one ----
function TDayFour.findOurWinningNumbers(winning_, toMatch_: TStringArray
  ): TStringArray;
var
  index:integer;
begin
  //Look through toMatch and add any entries that are also in winning to the result
  result:=TStringArray.create;
  for index:= 0 to pred(toMatch_.size) do
    if (winning_.indexOf(toMatch_[index])> -1) then result.push(toMatch_[index]);
end;

function TDayFour.scoreForMatchingNumbers(input_: TStringArray): integer;
begin
  result:= round(power(2,(input_.size - 1)));
end;

end.


