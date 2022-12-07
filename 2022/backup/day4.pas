unit day4;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics, arrayUtils;

type

  { TDay Four }
  TDayFour = class(TAocPuzzle)
  private
  fName:string;
  public
  function encloses(first,second:TPoint):boolean;
  function overlaps(first,second:TPoint):boolean;
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayFour }

function TDayFour.encloses(first, second: TPoint): boolean;
begin
  result:= ((first.X >= second.X) and (first.Y <= second.Y))
           or ((second.X >= first.X) and (second.Y <= first.Y))
end;

function TDayFour.overlaps(first, second: TPoint): boolean;
//is there any overlap at all
begin
  //we can check if one set is completely enclosed as in part 1
  if encloses(first,second) then
    begin
      result:=true;
      exit;
    end;
  result:= ((first.X <= second.X) and (first.Y >= second.X))
     or ((second.X <= first.X) and (second.Y >= first.X))
end;

constructor TDayFour.create(filename: string; paintbox_: TPaintbox);
begin
inherited create(filename,'Day 4',paintbox_);
end;

procedure TDayFour.runPartOne;
var
  index:integer;
  elfDuties:TStringArray;
  elf1,elf2:TPoint;
  enclosingPairs:integer;
begin
  results.Clear;
  enclosingPairs:=0;
  for index:= 0 to Pred(puzzleInputLines.size) do
    begin
      //split on comma and dash, convert to int
      elfDuties:=puzzleInputLines[index].Split([',','-']);
      elf1.X:=elfDuties[0].ToInteger;
      elf1.Y:=elfDuties[1].ToInteger;
      elf2.X:=elfDuties[2].ToInteger;
      elf2.Y:=elfDuties[3].ToInteger;
      if encloses(elf1,elf2) then enclosingPairs:=enclosingPairs + 1;
    end;
  results.add('Number of enclosing pairs found '+enclosingPairs.ToString);
end;

procedure TDayFour.runPartTwo;
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

end.

