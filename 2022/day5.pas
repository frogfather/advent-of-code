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
  procedure setup;
  procedure extractCrates;
  procedure moveCrates(quantity,source,destination:integer;moveSingly:boolean = true);
  procedure moveCrateGroup(source,destination:integer;groupSize:integer = 1);
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  property crates:T2DStringArray read fCrates;
  property moveInstructions: TStringArray read fMoveInstructions;
  end;
implementation

{ TDayFive }

constructor TDayFive.create(filename: string; paintbox_: TPaintbox);
begin
  inherited create(filename,paintbox_);
  fName:= 'Day 4';
end;

procedure TDayFive.setup;
begin
  //TODO - hard coding the number of columns and number of lines
  //to the move instructions means we can't use the test data
  //Should calculate this dynamically instead.
  setLength(fCrates,9,0);
  fcrateData:=Copy(puzzleInputLines,0,8);
  fMoveInstructions:=Copy(puzzleInputLines,10,pred(puzzleInputLines.size));
  extractCrates;
end;

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

//Modified to make part two work without affecting part one
procedure TDayFive.moveCrates(quantity, source, destination: integer;moveSingly:boolean);
var
  index:integer;
begin
  if moveSingly then
  for index:= 0 to pred(quantity) do
    moveCrateGroup(source,destination)
  else moveCrateGroup(source,destination,quantity);
end;

procedure TDayFive.moveCrateGroup(source,destination:integer;groupSize:integer);
var
  groupToMove:TStringArray;
  index:integer;
begin
  groupToMove:=Copy(crates[source],length(crates[source])-(groupSize),groupSize);
  setLength(crates[destination],length(crates[destination])+groupSize);

  for index:= 0 to pred(groupSize) do
  crates[destination][length(crates[destination])- groupSize + index]:= groupToMove[index];

  setLength(crates[source],length(crates[source])-groupSize);
end;

procedure TDayFive.runPartOne;
var
  index: integer;
  instruction:TStringArray;
  topCrates:string;
begin
  setup;
  for index:= 0 to pred(moveInstructions.size) do
    begin
      instruction:=moveInstructions[index].Split(' ');
      moveCrates(instruction[1].ToInteger,instruction[3].ToInteger-1,instruction[5].ToInteger-1);
    end;
  topCrates:='';
  //Then get the top crate from each column
  for index:= 0 to pred(length(fCrates)) do
    topCrates:=topCrates+crates[index][length(crates[index])-1];
  results.Add('Top crates after rearranging singly'+topCrates);
end;

procedure TDayFive.runPartTwo;
var
  index: integer;
  instruction:TStringArray;
  topCrates:string;
begin
  setup;
  for index:= 0 to pred(moveInstructions.size) do
    begin
      instruction:=moveInstructions[index].Split(' ');
      moveCrates(instruction[1].ToInteger,instruction[3].ToInteger-1,instruction[5].ToInteger-1,false);
    end;
  topCrates:='';
  //Then get the top crate from each column
  for index:= 0 to pred(length(fCrates)) do
    topCrates:=topCrates+crates[index][length(crates[index])-1];
  results.Add('Top crates after rearranging in groups'+topCrates);
end;

end.

