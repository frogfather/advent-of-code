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
var
  index,blankLinePosition:integer;
  columnLabels:TStringArray;
begin
  index:=0;
  blankLinePosition:=-1;
  while (blankLinePosition = -1) and (index < puzzleInputLines.size) do
    begin
    if (puzzleInputLines[index].Trim = '')
      then blankLinePosition:=index
      else index:=index+1;
    end;
  columnLabels:= puzzleInputLines[blankLinePosition - 1].trim.Split(' ',TStringSplitOptions.ExcludeEmpty);
  setLength(fCrates,columnLabels.size,0);
  fcrateData:=Copy(puzzleInputLines,0,blankLinePosition - 1);
  fMoveInstructions:=Copy(puzzleInputLines,blankLinePosition+1,pred(puzzleInputLines.size));
  extractCrates;
end;

//Take the portion of the input data that contains the crate data
//and add it to a 2D string array where array[C][R] represents column C row R
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

//If the moveSingly parameter is true it calls moveCrateGroup 'quantity' times
//with group size defaulting to 1 (puzzle part one)

//If moveSingly is false it calls moveCrateGroup once with the group size set
//to the quantity. (puzzle part two)
procedure TDayFive.moveCrates(quantity, source, destination: integer;moveSingly:boolean);
var
  index:integer;
begin
  if moveSingly then
  for index:= 0 to pred(quantity) do
    moveCrateGroup(source,destination)
  else moveCrateGroup(source,destination,quantity);
end;

//Called by move crates method above. The default group size is 1 but this
//can be overridden to move multiple crates
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
  for index:= 0 to pred(length(fCrates)) do
    topCrates:=topCrates+crates[index][length(crates[index])-1];
  results.Add('Top crates after rearranging singly '+topCrates);
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
  for index:= 0 to pred(length(fCrates)) do
    topCrates:=topCrates+crates[index][length(crates[index])-1];
  results.Add('Top crates after rearranging in groups '+topCrates);
end;

end.

