unit snailfish;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,aocUtils,math;
type
  
  { TSnailfish }

  TSnailfish = class(TInterfacedObject)
    private
    fNumbers: TStringArray;
    fSum: int64;
    function findNumberWidth(input:string;position:integer):integer;
    function findNearestNumberIndex(input,snailFishNumber:string;out nearestNumber: integer;left:boolean=true):boolean;
    function getNumber(snailFishNumber:string;left:boolean=true):integer;
    function addNumbers(number1,number2:string):string;
    function explodeNumber(input,numberToExplode:string):string;
    function splitNumber(input:string;position:integer):string;
    function magnitude(input: string):int64;
    function replaceNumberWithZero(input:string; numberStart:integer):string;
    public
    constructor create(puzzleInput:TStringArray);
    procedure doHomework;
    property sum: int64 read fSum;
  end;

implementation

{ TSnailfish }
const separators: array [0..2] of string = ('[',',',']');

function TSnailfish.findNumberWidth(input: string; position: integer): integer;
var
  numberWidth,index: integer;
  done:boolean;
begin
  index:=position;
  numberWidth:=0;
  done:=false;
  repeat
  if isNumberString(input.Substring(index,1))
    then numberWidth:=numberWidth + 1
  else
    begin
    done:=true;
    break;
    end;
  if index < pred(length(input))
    then index:=index+1
  else done:=true;
  until done;
  result:=numberWidth;
end;

function TSnailfish.findNearestNumberIndex(input,snailFishNumber:string;out nearestNumber: integer;left:boolean=true):boolean;
var
  sfNumberPos,index:integer;
  output:integer;
  sectionIndex:integer;
begin
  result:=false;
  //find the position of the snailfish number in the string
  sfNumberPos:=pos(snailFishNumber,input) -1; //Pos 1 indexed!
  if sfNumberPos = -1 then exit
  else
    begin
    if left then
      begin
      //look at the section of the input before snailFishNumber
      for index:= pred(sfNumberPos) downto 0 do
        begin
        //check if it's a string that can be converted to a number
        if isNumberString(input.Substring(index,1)) then
          begin
          nearestNumber:=index;
          result:=true;
          exit;
          end;
        end
      end else
      begin
      //look at the section of the input after snailFishNumber
      for index:= sFNumberPos+length(snailFishNumber) to pred(length(input)) do
        begin
        if isNumberString(input.Substring(index,1)) then
          begin
          nearestNumber:=index;
          result:=true;
          exit;
          end;
        end;
      end;
    end;
end;

function TSnailfish.getNumber(snailFishNumber: string; left: boolean): integer;
  begin
  if left
    then result:= snailFishNumber
      .Split(separators,TStringSplitOptions.ExcludeEmpty)[0].ToInteger
  else result:= snailFishNumber
      .Split(separators,TStringSplitOptions.ExcludeEmpty)[1].ToInteger;
  end;

function TSnailfish.addNumbers(number1, number2: string): string;
begin
  result:='['+number1+','+number2+']';
end;

function TSnailfish.explodeNumber(input,numberToExplode:string):string;
var
  leftNo,rightNo: integer;
  nearestLeftPos,nearestRightPos:integer;
  adjustedValue:integer;
  output:string;
  sfNumberPos:integer;
begin
  output:=Copy(input,0);
  sfNumberPos :=pos(numberToExplode, output) -1;
  if sfNumberPos = -1 then exit;//should log error maybe
  leftNo:= getNumber(numberToExplode);
  rightNo:= getNumber(numberToExplode,false);
  if findNearestNumberIndex(output, numberToExplode, nearestLeftPos) then
    begin
    adjustedValue:=output.Substring(nearestLeftPos,1).ToInteger + leftNo;
    output:=output.Remove(nearestLeftPos,1);
    output.Insert(nearestLeftPos,adjustedValue.ToString);
    end;
  if findNearestNumberIndex(output, numberToExplode, nearestRightPos, false) then
    begin
    adjustedValue:=output.Substring(nearestRightPos,1).ToInteger + rightNo;
    output:=output.Remove(nearestRightPos,1);
    output.Insert(nearestRightPos,adjustedValue.ToString);
    end;
  output:= replaceNumberWithZero(output,sfNumberPos);
  result:=output;
end;

function TSnailfish.splitNumber(input: string; position:integer): string;
var
  output,replacement:string;
  numberWidth: integer;
  numberBeforeSplit,leftNumber,rightNumber:integer;
  currentRoundingMode:TFPURoundingMode;
begin
  //replace the number with a pair. lh rounded down, rh rounded up
  //number should be followed by either a comma or ]
  output:=Copy(input,0);
  numberWidth:=findNumberWidth(input,position);
  if numberWidth = 0 then exit;
  numberBeforeSplit:=input.Substring(position,numberWidth).ToInteger;
  currentRoundingMode:= setRoundMode(TFPURoundingmode.rmDown);
  leftNumber:= round(roundTo((numberBeforeSplit/2),1));
  currentRoundingMode:= setRoundMode(TFPURoundingmode.rmUp);
  rightNumber:= round(roundTo((numberBeforeSplit/2),1));
  setRoundMode(currentRoundingMode);
  replacement:='['+leftNumber.ToString+','+rightNumber.ToString+']';
  output:=output.Remove(position,numberwidth);
  output.Insert(position,replacement);
  result:=output;
end;

function TSnailfish.magnitude(input: string): int64;
begin

end;

function TSnailfish.replaceNumberWithZero(input: string; numberStart: integer
  ): string;
var
  numberEnd:integer;
begin
  if (numberStart > pred(length(input))) or (input[numberStart] <> '[')
    then exit;
  numberEnd:=findCharPos(input,']', numberStart);
  //change to remove and insert
  result:=input.Substring(0,numberStart)+'0'+input.Substring(succ(numberEnd));
end;

constructor TSnailfish.create(puzzleInput: TStringArray);
begin
  fNumbers:=puzzleInput;
end;

procedure TSnailfish.doHomework;
var
  input:string;
  splitresult:string;
  noTosplit:integer;
begin
  input:='[[[[0,7],4],[15,[0,13]]],[1,1]]';
  noToSplit:= 13;
  splitResult:=splitNumber(input, noToSplit);
end;

end.

