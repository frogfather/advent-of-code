unit snailfish;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,aocUtils,math,regexpr;
type
  
  { TSnailfish }

  TSnailfish = class(TInterfacedObject)
    private
    fNumbers: TStringArray;
    fSum: int64;
    function findNumberWidth(input:string;position:integer):integer;
    function findNearestNumberIndex(input:string;startPos:integer;out nearestNumberIndex: integer;left:boolean=true):boolean;
    function getNumber(snailFishNumber:string;left:boolean=true):integer;
    function addNumbers(number1,number2:string):string;
    function explodeNumber(input:string;explodeStart:integer):string;
    function splitNumber(input:string;position:integer):string;
    function magnitude(input: string):int64;
    function bracketDepthAtPosition(input:string;position:integer):integer;
    function replaceNumberWithValue(input:string; numberStart:integer;value:integer=0):string;
    function replaceWithCalculatedvalue(input:string;position:integer):string;
    function findFirstExplodingNumber(input:string):integer;
    function findFirstSplittingNumber(input:string):integer;
    function getNumberAtPosition(input:string;position:integer):integer;
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
  regex:TRegexpr;
  sNumber:string;
begin
  result:=0;
  regex:=TRegexpr.Create('[0-9]*');
  if (regex.Exec(input))
    then
      begin
      if regex.ExecPos(position+1) then
        begin
        sNumber:=regex.Match[0];
        result:=length(sNumber);
        end;
      end;
  regex.free;
end;

function TSnailfish.findNearestNumberIndex(input:string;startPos:integer;out nearestNumberIndex: integer;left:boolean=true):boolean;
var
  index:integer;
  done:boolean;
  element,output:string;
begin
  result:=false;
  output:='';
  index:=startPos;
  repeat
  element:= input.Substring(index,1);
  if isNumberString(element)
    then
      begin
      if left then output.Insert(0,element)
      else output:= output + element;
      end
  else if length(output) > 0
    then
      begin
      if left then nearestNumberIndex:=index +1
      else
        begin
        nearestNumberIndex:= index -length(output);
        end;
      result:=true;
      exit;
      end;
  if left then index:= index - 1 else index:= index + 1;
  done:= (index = 0) or (index = length(input));
  until done;
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
  adjustedValue,oldValueWidth:integer;
  numberToExplode,output:string;
  explodeEnd:integer;
  nearestLeftFound,nearestRightFound:boolean;
begin
  output:=Copy(input,0);
  explodeEnd:=findCharPos(output,']',explodeStart);
  numberToExplode:=output.Substring(explodeStart, succ(explodeEnd-explodeStart));
  leftNo:= getNumber(numberToExplode);
  rightNo:= getNumber(numberToExplode,false);
  nearestRightFound:= findNearestNumberIndex(output, explodeEnd, nearestRightPos, false);
  nearestLeftFound:=findNearestNumberIndex(output, explodeStart, nearestLeftPos);
  //need to make replacements in this order or the index will change
  //if we replace a single digit number with a two digit number
  if nearestRightFound then
    begin
    oldValueWidth:=findNumberWidth(input,nearestRightPos);
    adjustedValue:=getNumberAtPosition(input,nearestRightPos) + rightNo;
    output:=output.Remove(nearestRightPos,oldValueWidth);
    output.Insert(nearestRightPos,adjustedValue.ToString);
    end;
  output:= replaceNumberWithValue(output,explodeStart);
  if nearestLeftFound then
    begin
    oldValueWidth:=findNumberWidth(input,nearestLeftPos);
    adjustedValue:=getNumberAtPosition(input,nearestLeftPos) + leftNo;
    output:=output.Remove(nearestLeftPos,oldValueWidth);
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


function TSnailfish.bracketDepthAtPosition(input: string; position: integer
  ): integer;
var
  index,bracketCount:integer;
  element:string;
begin
  bracketCount:=0;
  result:=-1;
  if position > pred(length(input)) then exit;
  for index:=0 to position do
    begin
    element:=input.Substring(index,1);
      case element of
      '[': bracketCount:=bracketCount+1;
      ']': bracketCount:=bracketCount-1;
      end;
    end;
  result:=bracketCount;
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

function TSnailfish.findFirstExplodingNumber(input: string): integer;
var
  matchPos,actualStart:integer;
  expression:String;
  regex:TRegexpr;
  done,nextResult,resultValid:boolean;
begin
result:=-1;
expression:= '\[\[[0-9]+,[0-9]+\],|\[[0-9]+,\[[0-9]+,[0-9]+\]';
regex:=TRegexpr.Create(expression);
nextResult:= regex.Exec(input);
if nextResult then
  begin
    repeat
    matchPos:=regex.MatchPos[0] - 1;//matchPos 1 based. Why?!?!
    actualStart:=matchPos + 1;
    while input.Substring(actualStart,1) <> '[' do
      actualStart:=actualStart + 1;
    resultValid:= (bracketDepthAtPosition(input,actualStart) > 4)
      and nextResult;
    if resultValid then result:=actualStart else result:=-1;
    if not resultValid then nextResult:=regex.ExecNext;
    done:= resultValid or not nextResult;
    until done;
  end
else result:=-1;
end;

function TSnailfish.findFirstSplittingNumber(input: string): integer;
var
  regex:TRegexpr;
begin
  result:=-1;
  regex:=TRegexpr.Create('\d{2,}');
  if regex.Exec(input)
  then result:= regex.MatchPos[0] -1;
  regex.Free;
end;

function TSnailfish.getNumberAtPosition(input: string; position: integer
  ): integer;
var
  regex:TRegexpr;
begin
  result:=0;
  regex:=TRegexpr.Create('[0-9]*');
  if (regex.Exec(input)) and (regex.ExecPos(position + 1))
    then
      begin
      result:=regex.Match[0].ToInteger;
      end;
  regex.free;
end;

constructor TSnailfish.create(puzzleInput: TStringArray);
begin
  fNumbers:=puzzleInput;
end;

procedure TSnailfish.doHomework;
var
  sfLineNo,explodeIndex,splitIndex:integer;
  sfsum:string;
  moreExplodes,moreSplits:boolean;
  loopCount:integer;
begin
  loopCount:=0;
  fSum:=0;
  sfLineNo:=0;
  if length(fNumbers)=0 then exit;
  //add the first sfNumber to the next
  sfsum:=fNumbers[0];
  //doLog(sfLineNo.ToString+' initial sum : '+sfSum);
    repeat
    //doLog(sfLineNo.ToString+'line : '+sfSum);
    moreExplodes:=true;
    moreSplits:=true;
    while moreExplodes or moreSplits do
      begin
      loopCount:=loopCount+1;
      //doLog('loop '+loopCount.ToString);
      //do all explodes first
      while moreExplodes do
        begin
        explodeIndex:= findFirstExplodingNumber(sfSum);
        moreExplodes:=explodeIndex > -1;
        //if explodeIndex > 1 then
        //  doLog(sfLineNo.ToString+' going to explode at : '+explodeIndex.ToString);
        if explodeIndex > -1
          then sfSum:= explodeNumber(sfSum,explodeIndex);
        //if explodeIndex > 1 then
        //  doLog(sfLineNo.ToString+' result: '+sfSum);
        end;
      splitIndex:= findFirstSplittingNumber(sfSum);
      //if splitIndex > 1 then
      //  doLog(sfLineNo.ToString+' going to split at: '+splitIndex.ToString);
      if splitIndex > -1
        then sfSum:= splitNumber(sfSum,splitIndex);
      //if splitIndex > 1 then
      //  doLog(sfLineNo.ToString+' result: '+sfSum);
      moreExplodes:=findFirstExplodingNumber(sfSum) > -1;
      moreSplits:= findFirstSplittingNumber(sfSum) > -1;
      end;
    sfLineNo:=sfLineNo + 1;
    if sfLineNo < length(fNumbers) then sfsum:='['+sfsum+','+fNumbers[sfLineNo]+']';
    until sfLineNo > length(fNumbers);
  fSum:=magnitude(sfSum);
end;

end.

