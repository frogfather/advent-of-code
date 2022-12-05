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
  procedure extractCrates(crates:TStringArray);
  procedure moveCrates(quantity,source,destination:integer);
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
  crateData:TStringArray;
begin
  crateData:=Copy(puzzleInputLines,0,8);
  fMoveInstructions:=Copy(puzzleInputLines,10,pred(puzzleInputLines.size));
  extractCrates(crateData);
end;

procedure TDayFive.extractCrates(crates: TStringArray);
var
  crateLineIndex,crateIndex,columnIndex,rowIndex:integer;
  crateLine,crate:string;
begin
  rowIndex:=0; //index of 2nd level array
  for crateLineIndex:= pred(crates.size) downTo 0 do
    begin
    crateLine:=crates[crateLineIndex];
    crateIndex:=1; //actual position in the string;
    //Spaces are important here - some columns may be empty
    //the letters are 4 spaces apart
    for columnIndex:= 0 to 8 do
      begin
      crate:=crateLine.Substring(crateIndex,1);
      if (crate <> ' ') then
        begin
        if (length(fCrates[columnIndex]) < rowIndex + 1)
          then setLength(fCrates[columnIndex],length(fCrates[columnIndex])+1);
        fCrates[columnIndex][rowIndex]:= crate;
        end;
      crateIndex:=crateIndex + 4;
      end;
    rowIndex:=rowIndex + 1;
    end;
end;

//The column is the first index of the array
//The row is the second index of the array
procedure TDayFive.moveCrates(quantity, source, destination: integer);
var
  newColumnHeight:integer;
begin

end;

constructor TDayFive.create(filename: string; paintbox_: TPaintbox);
begin
  inherited create(filename,paintbox_);
  fName:= 'Day 4';
  //set the 2D array to an initial size of 9 columns
  //the height of each column will be set dynamically
  setLength(fCrates,9,0);
  separateCratesFromInstructions;
end;

procedure TDayFive.runPartOne;
begin

end;

procedure TDayFive.runPartTwo;
begin

end;

end.

