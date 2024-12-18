unit day17;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils,math,strUtils,aocUtils;
type

  { THandheld }

  THandheld = class(TInterfacedObject)
  private
  fRegA:integer;
  fRegB:integer;
  fRegC:integer;
  fInstructionPointer:integer;
  fInstructions:TIntArray;
  fOutput:TIntArray;
  function instructionInRange:boolean;
  function getComboOperand(operand:integer):integer;
  function bitwise(arg1,arg2:integer):integer;
  procedure setInstructionPointer(amount:integer);
  procedure advanceInstructionPointer(amount:integer=2);
  procedure executeStep;
  procedure addToOutput(value:integer);
  function getOutput:string;
  public
  constructor create(regA,regB,regC:integer;instructions:string);
  procedure run;
  property output: string read getOutput;
  end;

  { TDaySeventeen}
  TDaySeventeen = class(TAocPuzzle)
  private
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ THandheld }

function THandheld.instructionInRange: boolean;
begin
  result:=(fInstructionPointer > -1)and(fInstructionPointer < fInstructions.size);
end;

function THandheld.getComboOperand(operand: integer): integer;
begin
  case operand of
  1,2,3: result:=operand;
  4: result:=fRegA;
  5: result:=fRegB;
  6: result:=fRegC;
  end;
end;

function THandheld.bitwise(arg1, arg2: integer): integer;
var
  str1,str2,strResult:string;
begin
  str1:=intToBin(arg1,32);
  str2:=intToBin(arg2,32);
  strresult:=bitwiseXOR(str1,str2);
  result:=stringOfBinaryToInteger(strresult);
end;

procedure THandheld.setInstructionPointer(amount: integer);
begin
  fInstructionPointer:=amount;
end;

procedure THandheld.advanceInstructionPointer(amount: integer);
begin
  fInstructionPointer:=fInstructionPointer + amount;
end;

procedure THandheld.executeStep;
var
  instruction:integer;
  operand:integer;
begin
  instruction:=fInstructions[fInstructionPointer];
  operand:= fInstructions[fInstructionPointer + 1];
  case instruction of
  0:
    begin
    fRegA:=fRegA div round(intPower(2,getComboOperand(operand)));
    advanceInstructionPointer;
    end;
  1:
    begin
    //bitwise XOR Reg B and literal
    fRegB:=bitwise(fRegB,operand);
    advanceInstructionPointer;
    end;
  2:
    begin
    fRegB:=getComboOperand(operand) mod 8;
    advanceInstructionPointer;
    end;
  3:
    begin
    if (fRegA <> 0)
      then setInstructionPointer(operand)
      else advanceInstructionPointer;
    end;
  4:
    begin
    fRegB:=bitwise(fRegB,fRegC);
    advanceInstructionPointer;
    end;
  5:
    begin
    addToOutput(getComboOperand(operand)mod 8);
    advanceInstructionPointer;
    end;
  6:
    begin
    fRegB:=fRegA div round(intPower(2,getComboOperand(operand)));
    advanceInstructionPointer;
    end;
  7:
    begin
    fRegC:=fRegA div round(intPower(2,getComboOperand(operand)));
    advanceInstructionPointer;
    end;
  end;
end;

procedure THandheld.addToOutput(value: integer);
begin
  fOutput.push(value);
end;

function THandheld.getOutput: string;
begin
  result:=fOutput.toString(',');
end;

constructor THandheld.create(regA, regB, regC: integer; instructions: string);
begin
  fRegA:=regA;
  fRegB:=regB;
  fRegC:=regC;
  fInstructions:=instructions.Split(',').toIntArray;
  fInstructionPointer:=0;
  fOutput:=TIntArray.create;
end;

procedure THandheld.run;
begin
  while instructionInRange do executeStep;
end;

{ TDaySeventeen }

constructor TDaySeventeen.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 17',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDaySeventeen.runPartOne;
var
  handheld:THandheld;
  regA,regB,regC,instructions:string;
begin
  results.Clear;
  regA:=puzzleInputLines[0].split(':')[1].Trim;
  regB:=puzzleInputLines[1].split(':')[1].Trim;
  regC:=puzzleInputLines[2].split(':')[1].Trim;
  instructions:=puzzleInputLines[4].split(':')[1].Trim;
  handheld:=THandheld.create(regA.ToInteger,regB.ToInteger,regC.ToInteger, instructions);
  handheld.run;
  results.Add(handheld.output);
end;

procedure TDaySeventeen.runPartTwo;
begin
  results.Clear;
end;


end.

                
