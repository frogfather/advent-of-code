unit day5;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics, arrayUtils;

type

  { TDay Five }
  
  { TDayFive }

  TDayFive = class(TAocPuzzle)
  private
  fName:string;
  fCrates:T2DStringArray;
  fMoveInstructions: TStringArray;
  procedure separateCratesFromInstructions;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  property crates:T2DStringArray read fCrates;
  property moveInstructions: TStringArray read fMoveInstructions;
  end;
implementation

{ TDayFive }

procedure TDayFive.separateCratesFromInstructions;
var
  index,crateLineIndex,crateIndex,columnIndex,rowIndex:integer;
  endOfCrates:boolean;
  crateData:TStringArray;
  crateLine,crate:string;
begin
  endOfCrates:=false;
  crateData:=TStringArray.Create;
  for index:=0 to pred(puzzleInputLines.size) do
    begin
    if ((endOfCrates = false)and(puzzleInputLines[index] <> '')) then
       crateData.push(puzzleInputLines[index])
         else fMoveInstructions.push(puzzleInputLines[index]);
    if puzzleInputLines[index]='' then endOfCrates:=true;
    end;
  //set the array to an initial size of 9 x 8 (9 columns and initially 8 crates)
  setLength(fCrates,9,8);
  rowIndex:=0; //index of 2nd level array
  for crateLineIndex:= pred(pred(crateData.size)) downTo 0 do
    begin
    crateLine:=crateData[crateLineIndex];
    //Spaces are important here - some columns may be empty
    //the letters are 4 spaces apart
    for columnIndex:= 0 to 8 do
      begin
      crateIndex:=1; //actual position in the string;
      crate:=crateLine.Substring(crateIndex,1);
      if crate <> '' then fCrates[columnIndex][rowIndex]:= crate;
      crateIndex:=crateIndex + 4;
      end;
    rowIndex:=rowIndex + 1;
    end;
end;

constructor TDayFive.create(filename: string; paintbox_: TPaintbox);
begin
  inherited create(filename,paintbox_);
  fName:= 'Day 4';
  separateCratesFromInstructions;
end;

//General idea
//Separate out the crate diagram from tbe move instructions

procedure TDayFive.runPartOne;
begin

end;

procedure TDayFive.runPartTwo;
begin

end;

end.

