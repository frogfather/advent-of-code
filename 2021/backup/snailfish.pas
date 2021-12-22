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
    fLog:TStringList;
    function findNumberWidth(input:string;position:integer):integer;
    function findNearestNumberIndex(input:string;startPos:integer;out nearestNumber: integer;left:boolean=true):boolean;
    function getNumber(snailFishNumber:string;left:boolean=true):integer;
    function addNumbers(number1,number2:string):string;
    function explodeNumber(input:string;explodeStart:integer):string;
    function splitNumber(input:string;position:integer):string;
    function magnitude(input: string):int64;
    function replaceNumberWithValue(input:string; numberStart:integer;value:integer=0):string;
    function replaceWithCalculatedvalue(input:string;position:integer):string;
    function findFirstBracketedSingleNumber(input:string;out bracketedSingle:integer):integer;
    function findFirstExplodingNumber(input:string):integer;
    function findFirstSplittingNumber(input:string):integer;
    procedure addTolog(message:string);
    public
    constructor create(puzzleInput:TStringArray);
    procedure doHomework;
    property sum: int64 read fSum;
    property log: TStringlist read fLog;
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

function TSnailfish.findNearestNumberIndex(input:string;startPos:integer;out nearestNumber: integer;left:boolean=true):boolean;
var
  index:integer;
begin
  result:=false;
  if left then
    begin
    //look at the section of the input before snailFishNumber
    for index:= pred(startPos) downto 0 do
      begin
      //check if it's a string that can be converted to a number
      if isNumberString(input.Substring(index,1)) then
        begin
        nearestNumber:=index;
        result:=true;
        exit;
        end;
      end
    end
  else
    begin
    //look at the section of the input after snailFishNumber
    for index:= startPos to pred(length(input)) do
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

function TSnailfish.explodeNumber(input:string;explodeStart:integer):string;
var
  leftNo,rightNo: integer;
  nearestLeftPos,nearestRightPos:integer;
  adjustedValue:integer;
  numberToExplode,output:string;
  explodeEnd:integer;
begin
  //find the length of the number to explode
  output:=Copy(input,0);
  explodeEnd:=findCharPos(output,']',explodeStart);
  numberToExplode:=output.Substring(explodeStart, succ(explodeEnd-explodeStart));
  leftNo:= getNumber(numberToExplode);
  rightNo:= getNumber(numberToExplode,false);
  //need to make replacements in this order or the index will change
  //if we replace a single digit number with a two digit number
  if findNearestNumberIndex(output, explodeEnd, nearestRightPos, false) then
    begin
    adjustedValue:=output.Substring(nearestRightPos,1).ToInteger + rightNo;
    output:=output.Remove(nearestRightPos,1);
    output.Insert(nearestRightPos,adjustedValue.ToString);
    end;
  output:= replaceNumberWithValue(output,explodeStart);
  if findNearestNumberIndex(output, explodeStart, nearestLeftPos) then
    begin
    adjustedValue:=output.Substring(nearestLeftPos,1).ToInteger + leftNo;
    output:=output.Remove(nearestLeftPos,1);
    output.Insert(nearestLeftPos,adjustedValue.ToString);
    end;
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
  //if the number is even both values should be that number div 2
  output:=Copy(input,0);
  numberWidth:=findNumberWidth(input,position);
  if numberWidth = 0 then exit;
  numberBeforeSplit:=input.Substring(position,numberWidth).ToInteger;
  if numberBeforeSplit mod 2 = 0 then
    begin
    leftNumber:=numberBeforeSplit div 2;
    rightNumber:=numberBeforeSplit div 2;
    end else
    begin
    currentRoundingMode:= setRoundMode(TFPURoundingmode.rmDown);
    leftNumber:= round(roundTo((numberBeforeSplit/2),-1));
    currentRoundingMode:= setRoundMode(TFPURoundingmode.rmUp);
    rightNumber:= round(roundTo((numberBeforeSplit/2),-1));
    setRoundMode(currentRoundingMode);
    end;
  replacement:='['+leftNumber.ToString+','+rightNumber.ToString+']';
  output:=output.Remove(position,numberwidth);
  output.Insert(position,replacement);
  result:=output;
end;

function TSnailfish.magnitude(input: string): int64;
var
  sfNoStart,sfNoEnd:integer;
  output:string;
  done,finished:boolean;
  index:integer;
begin
  output:=copy(input,0);
  done:=false;
  while not done do
    begin
    sfNoEnd := pos(']',output) - 1;
    if sfNoEnd > -1 then
      begin
      index:=sfNoEnd - 1;
      finished:=false;
      repeat
        if (output.Substring(index,1) = '[')
          then
            begin
            sfNoStart:=index;
            finished:=true;
            end
        else index:=index -1;
        if index < 0 then finished:=true;
      until finished;
      if sfNoEnd > 0
        then output:= replaceWithCalculatedValue(output,sfNoStart)
      else done:=true;
      end else done:=true;
    end;
  result:=output.ToInteger;
end;

function TSnailfish.replaceNumberWithValue(input: string; numberStart: integer;
  value:integer=0): string;
var
  numberEnd:integer;
  output:string;
begin
  if (numberStart > pred(length(input))) or (input.Substring(numberStart,1) <> '[')
    then exit;
  output:=copy(input,0);
  numberEnd:=findCharPos(output,']', numberStart);
  //change to remove and insert
  output:=output.Remove(numberStart,(numberEnd-numberStart)+1);
  output.Insert(numberStart, value.ToString);
  result:=output;
end;

function TSnailfish.replaceWithCalculatedvalue(input: string; position: integer
  ): string;
var
  elements:TStringArray;
  endBracketPos:integer;
  lhNo,rhNo:integer;
  replacement:string;
begin
  endBracketPos:=findCharPos(input,']',position + 1);
  elements:=input.Substring(position, ((endBracketPos - position) + 1))
    .Split(separators,TStringSplitOptions.ExcludeEmpty);
  lhNo:=elements[0].ToInteger;
  rhNo:=elements[1].ToInteger;
  replacement:= ((3*lhNo)+(2*rhNo)).ToString;
  result:=input.Remove(position, (endBracketPos-position)+1);
  result.Insert(position,replacement);
end;

function TSnailfish.findFirstBracketedSingleNumber(input: string; out bracketedSingle:integer ): integer;
var
  index:integer;
  startBracket:integer;
  element:string;
  commaFound:boolean;
begin
  result:=-1;
  //want to find [x] or [xx]
  startBracket:=-1;
  for index:= 0 to pred(length(input)) do
    begin
    element:= input.Substring(index,1);
    if element = '['
      then
        begin
        startBracket:=index;
        commaFound:=false;
        end
    else if element = ','
      then commaFound:=true
    else if (element = ']') and not commaFound then
      begin
      result:=startBracket;
      bracketedSingle:=input.Substring(succ(startBracket),index - succ(startBracket)).ToInteger;
      exit;
      end;
    end;
end;

function TSnailfish.findFirstExplodingNumber(input: string): integer;
var
  bracketCount,index:integer;
begin
  bracketCount:=0;
  result:=-1;
  for index:=0 to pred(length(input)) do
    begin
    if input.Substring(index,1) = '['
      then bracketCount:=bracketCount+1
    else if input.Substring(index,1) = ']'
      then bracketCount:=bracketCount -1;
    //we want the index of the bracket just before a number where bracketCount >=5
    if isNumberString(input.Substring(index,1)) and (bracketCount >= 5) then
      begin
      result:=index - 1;
      exit;
      end;
    end;
end;

function TSnailfish.findFirstSplittingNumber(input: string): integer;
var
  sepNumbers:TStringArray;
  index:integer;
  element:string;
begin
  result:=-1;
  //split on separators leaving just numbers then see if any are over 9
  sepNumbers:=input.Split(separators,TStringSplitOptions.ExcludeEmpty);
  for index:= 0 to pred(length(sepNumbers)) do
    begin
    element:=sepNumbers[index];
    if (length(element) > 0) and (element.ToInteger > 9) then
      begin
      result:=pos(element,input) - 1;
      exit;
      end;
    end;
end;

procedure TSnailfish.addTolog(message: string);
begin
  fLog.Add(message);
end;

constructor TSnailfish.create(puzzleInput: TStringArray);
begin
  fNumbers:=puzzleInput;
end;

procedure TSnailfish.doHomework;
var
  sfLineNo,explodeIndex,splitIndex,bracketedSingleIndex:integer;
  bracketedSingleValue:integer;
  sfsum:string;
  moreExplodes,moreSplits,moreBracketedSingles:boolean;
begin
  fSum:=0;
  sfLineNo:=1;
  if length(fNumbers)=0 then exit;
  //add the first sfNumber to the next
  sfsum:=fNumbers[0];
  while sfLineNo < length(fNumbers) do
    begin
    sfsum:='['+sfsum+','+fNumbers[sfLineNo]+']';
    addTolog('line no '+sfLineNo.ToString+' sum: '+sfSum);
    moreExplodes:=true;
    moreSplits:=true;
    while moreExplodes or moreSplits do
      begin
      //deal with all the explodes, then all the splits
      //then any explodes caused by the splits
      //then any splits caused by the explodes etc
      while moreExplodes do
        begin
        explodeIndex:= findFirstExplodingNumber(sfSum);
        if explodeIndex > -1
          then sfSum:= explodeNumber(sfSum,explodeIndex);
        moreExplodes:= explodeIndex > -1;
        addTolog('line no '+sfLineNo.ToString+' explode '+explodeIndex.ToString+' sum: '+sfSum);
        if findFirstBracketedSingleNumber(sfSum,bracketedSingleValue) > -1 then
          begin
          writeLn('something');
          exit;
          end;

        end;
      moreSplits:= findFirstSplittingNumber(sfSum) > -1;
      while moreSplits do
        begin
        splitIndex:= findFirstSplittingNumber(sfSum);
          if splitIndex > -1
            then sfSum:= splitNumber(sfSum,splitIndex);
        moreSplits:= splitIndex > -1;
        addTolog('line no '+sfLineNo.ToString+' split '+splitIndex.ToString+' sum: '+sfSum);
        if findFirstBracketedSingleNumber(sfSum,bracketedSingleValue) > -1 then
          begin
          writeLn('something');
          exit;
          end;

        end;
      moreExplodes:= findFirstExplodingNumber(sfSum) > -1;
      end;
    sfLineNo:=sfLineNo + 1;
    end;
  fSum:=magnitude(sfSum);
end;

end.

