unit day4;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics, arrayUtils;

type
{ TDay Four }
  
  { TDayFour }

  TDayFour = class(TAocPuzzle)
  private
  fName:string;
  function overlaps(first,second:TPoint):boolean;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayFour }

function TDayFour.overlaps(first, second: TPoint): boolean;
begin
  result:= ((first.X >= second.X) and (first.Y <= second.Y))
           or ((second.X >= first.X) and (second.Y <= first.Y))
end;

constructor TDayFour.create(filename: string; paintbox_: TPaintbox);
begin
inherited create(filename,paintbox_);
fName:= 'Day 4';
end;

procedure TDayFour.runPartOne;
var
  index:integer;
  elfDuties:TStringArray;
  elf1,elf2:TPoint;
  overlappingPairs:integer;
begin
  results.Clear;
  overlappingPairs:=0;
  for index:= 0 to Pred(puzzleInputLines.size) do
    begin
      //split on comma and dash, convert to int
      elfDuties:=puzzleInputLines[index].Split([',','-']);
      elf1.X:=elfDuties[0].ToInteger;
      elf1.Y:=elfDuties[1].ToInteger;
      elf2.X:=elfDuties[2].ToInteger;
      elf2.Y:=elfDuties[3].ToInteger;
      if overlaps(elf1,elf2) then overlappingPairs:=overlappingPairs + 1;
    end;
  results.add('Number of overlapping pairs found '+overlappingPairs.ToString);
end;

procedure TDayFour.runPartTwo;
begin

end;

end.

