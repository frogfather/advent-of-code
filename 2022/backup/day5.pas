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
  fCrateData:TStringArray;
  fMoveInstructions: TStringArray;
  procedure extractCrates;
  procedure moveCrates(quantity,source,destination:integer);
  procedure moveSingleCrate(source,destination:integer);
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  property crates:T2DStringArray read fCrates;
  property moveInstructions: TStringArray read fMoveInstructions;
  end;
implementation

{ TDayFive }

procedure TDayFive.extractCrates;
var
  crateLineIndex,crateIndex,columnIndex,rowIndex:integer;
  crateLine,crate:string;
begin
  rowIndex:=0; //index of 2nd level array
  for crateLineIndex:= pred(fcrateData.size) downTo 0 do
    begin
    crateLine:=fcrateData[crateLineIndex];
    crateIndex:=1; //actual position in the string;
    //Spaces are important here - some columns may be empty
    //the letters are 4 spaces apart
    for columnIndex:= 0 to 8 do
      begin
      crate:=crateLine.Substring(crateIndex,1).Trim;
      if (crate <> '') then
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
//So column 1 row 4 is [0][3]
procedure TDayFive.moveCrates(quantity, source, destination: integer);
var
  index:integer;
begin
  for index:= 0 to pred(quantity) do
    moveSingleCrate(source,destination);
end;

procedure TDayFive.moveSingleCrate(source, destination: integer);
begin
  //move the top crate from the source to the destination
  //and adjust column heights
  //Does the top crate exist?
  if (length(crates[source]) = 0) then
    begin
    debugln('nothing to move');
    exit;
    end;
  setLength(crates[destination],length(crates[destination])+1);
  crates[destination][length(crates[destination])-1]:= crates[source][length(crates[source]) - 1];
  setLength(crates[source],length(crates[source])-1);
end;

constructor TDayFive.create(filename: string; paintbox_: TPaintbox);
begin
  inherited create(filename,paintbox_);
  fName:= 'Day 4';
  //set the 2D array to an initial size of 9 columns
  //the height of each column will be set dynamically
end;

procedure TDayFive.runPartOne;
var
  index: integer;
  instruction:TStringArray;
  topCrates:string;
begin
  setLength(fCrates,9,0);
  fcrateData:=Copy(puzzleInputLines,0,8);
  fMoveInstructions:=Copy(puzzleInputLines,10,pred(puzzleInputLines.size));
  extractCrates;
  for index:= 0 to pred(moveInstructions.size) do
    begin
      instruction:=moveInstructions[index].Split(' ');
      moveCrates(instruction[1].ToInteger,instruction[3].ToInteger-1,instruction[5].ToInteger-1);
    end;
  topCrates:='';
  //Then get the top crate from each column
  for index:= 0 to pred(length(fCrates)) do
  topCrates:=topCrates+crates[index][length(crates[index])-1];
end;

procedure TDayFive.runPartTwo;
begin

end;

end.

