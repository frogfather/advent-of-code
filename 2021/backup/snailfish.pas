unit snailfish;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,aocUtils,math,regexpr,fgl;
type
  
  { TSnailfish } //Original implementation - v slow!

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

  { TInt } //provides an integer we can set to nil
  TInt = class(TInterfacedObject)
    private
    fValue:integer;
    public
    constructor create(value:integer);
    property value: integer read fValue write fValue;
  end;

  { TNode }
  TNode = class(TInterfacedObject)
    private
    fVal:TInt;
    fLeft: TNode;
    fRight: TNode;
    fParent: TNode;
    function getValue:integer;
    procedure setValue(val:integer);
    public
    constructor create(val:TInt);
    property left: TNode read fLeft write fLeft;
    property right: TNode read fRight write fRight;
    property parent: TNode read fParent write fParent;
    property val: TInt read fVal write fVal;
  end;

  { TStackEntry }
  TStackEntry = record
    node: TNode;
    depth: integer;
  end;

  { TStackArray }
  TStackArray = array of TStackEntry;

  { TStack }
  TStack = class(TInterfacedObject)
    private
    fStackArray: TStackArray;
    function getStackArrayLength:integer;
    public
    constructor create;
    function pop:TStackEntry;
    procedure append(stackEntry:TStackEntry);
    procedure clear;
    property len:integer read getStackArrayLength;
  end;


  { THomework }
  THomework = class
    private
    fPuzzleInput:TStringArray;
    fTree: TNode;
    fLevel:integer;
    function splitSfNumber(sfNumber:string):TStringArray;
    function parse(fishNum: string): TNode;
    function add(t1,t2:TNode):TNode;
    function reduce(tree:TNode):TNode;
    function magnitude(tree:TNode):integer;
    public
    constructor create(puzzleInput:TStringArray);
    procedure doHomework;
  end;


implementation

{ TStack }

function TStack.getStackArrayLength: integer;
begin
  result:=length(fStackArray);
end;

constructor TStack.create;
begin
  fStackArray:= TStackArray.create;
end;

function TStack.pop: TStackEntry;
begin
  if length(fStackArray) > 0 then
    begin
    result:= fStackArray[pred(length(fStackArray))];
    setLength(fStackArray,pred(length(fStackArray)));
    end;
end;

procedure TStack.append(stackEntry: TStackEntry);
begin
  setLength(fStackArray,length(fStackArray)+1);
  fStackArray[pred(length(fStackArray))]:= stackEntry;
end;

procedure TStack.clear;
begin
  setLength(fStackArray,0);
end;

{ THomework }
constructor THomework.create(puzzleInput: TStringArray);
begin
  fPuzzleInput:=puzzleInput;
  if length(puzzleInput) = 0 then exit;
end;

procedure THomework.doHomework;
var
  index:integer;
begin
  fTree:=parse(fPuzzleInput[0]);
  for index:= 1 to pred(length(fPuzzleInput)) do
    fTree:= add(fTree, parse(fPuzzleInput[index]));
  fLevel:=0;
  testTree(fTree);
end;

function THomework.splitSfNumber(sfNumber: string): TStringArray;
var
  index:integer;
  leftPart,rightPart:string;
begin
  result:= TStringArray.create;
  if (pos(',',sfNumber) = 0)
    then
    begin
    setLength(result,1);
    result[0]:= sfNumber;
    end
  else
    begin
    setLength(result,2);
    for index:= 0 to pred(length(sfNumber)) do
      begin
      if sfNumber[index] = ',' then
        begin
        //do the brackets match on both parts?
        leftPart:=sfNumber.Substring(1, index - 2);
        rightPart:= sfNumber.Substring(index, length(sfNumber)-(index +1));
        if (leftPart.CountChar('[') = leftPart.CountChar(']'))
        and (rightPart.CountChar('[') = rightPart.CountChar(']'))
          then
            begin
            result[0]:= leftPart;
            result[1]:= rightPart;
            exit;
            end;
        end;
      end;
    end;
end;

function THomework.parse(fishNum: string): TNode;
var
  parts:TStringArray;
begin
  result:=TNode.create(nil);
  parts:=splitSfNumber(fishNum);
  if length(parts) = 1 then
    begin
    result.setValue(parts[0].ToInteger);
    end else
    begin
    result.left:=parse(parts[0]);
    result.right:=parse(parts[1]);
    result.left.parent:=result;
    result.right.parent:=result;
    end;
  reduce(result);
end;

function THomework.add(t1, t2: TNode): TNode;
begin
  result:=TNode.create(nil);
  result.left:=t1;
  result.right:=t2;
  result.left.parent:= result;
  result.right.parent:= result;
end;

function THomework.reduce(tree: TNode): TNode;
var
  stack: TStack;
  done, condition: Boolean;
  stackEntry:TStackEntry;
  node,prevNode,currNode:TNode;
  depth:integer;

  procedure addToStack(node:TNode; depth:integer);
    begin
    stackEntry.node:=node;
    stackEntry.depth:= depth;
    stack.append(stackEntry);
    end;
  function splitNode(node:TNode):TNode;
  var
    lowValue,highValue:integer;
    begin
    lowValue:=node.getValue div 2;
    highValue:=node.getValue - lowValue;
    node.left := TNode.create(TInt.create(lowValue));
    node.right := TNode.create(TInt.create(highValue));
    node.left.parent := node;
    node.right.parent := node;
    node.fval := nil;
    result:=node;
    end;

begin
  done:= true;
  stack:=TStack.Create;
  addToStack(tree,0);
  while stack.len > 0 do
    begin
    stackEntry:= stack.pop;
    node:=stackEntry.node;
    depth:=stackEntry.depth;
    if node <> nil then
      begin
      condition:= ((node.left = nil) and (node.right = nil))
        or ((node.left.val <> nil) and (node.right.val <> nil));
      if (depth >= 4) and (node.val = nil) and condition then
        begin
        //Go up the stack to find left node
        prevNode:=node.left;
        currNode:=node;
        while (currNode <> nil)
          and ((currNode.left = prevNode)
          or (currNode.left = nil)) do
            begin
            prevNode:=currNode;
            currNode:=currNode.parent;
            end;
        if currNode <> nil then
          begin
          currNode:= currNode.left;
          while currNode.fVal = nil do
            begin
            if currNode.right <> nil
              then currNode:=currNode.right
            else currNode:=currNode.left;
            end;
          currNode.setValue(currNode.getValue + node.left.getValue);
          end;

        //do the same with the right node
        prevNode := node.right;
        currNode := node;
        while (currNode <> nil)
          and ((currNode.right = prevNode)
          or (currNode.right = nil)) do
            begin
            prevNode := currNode;
            currNode := currNode.parent;
            end;
          //Right node must exist
        if currNode <> nil then
          begin
          //Now cur_idx has a right child; we go all the way down
          currNode := currNode.right;
          while currNode.fVal = nil do
            begin
            if currNode.left <> nil
              then currNode := currNode.left
            else currNode := currNode.right;
            //Update some values!
            currNode.setValue(currNode.getValue + node.right.getValue);
            end;
          end;
        //then update the exploding node
        node.setValue(0);
        node.left:=nil;
        node.right:=nil;

        done:= false;
        break;
        end; //depth >= 4
      addToStack(node.right, depth + 1);
      addToStack(node.left, depth + 1);
      end; //node not nil
    end; //stackLength > 0

  //look for splits later
  if not done
    then
      begin
      reduce(tree);
      exit;
      end;
  stack.clear;
  addToStack(tree,0);
  while stack.len > 0 do
    begin
    node:= stack.pop.node;
    if node <> nil then
      begin
      if (node.fVal <> nil) then
        begin
        //split
        if (node.left = nil) and (node.right = nil)
          and (node.fVal <> nil) and (node.getValue >= 10) then
          begin
          node:= splitNode(node);
          done:=false;
          end;
        end;
        addToStack(node.right,0);
        addToStack(node.left,0);
      end;
    end;
  if not done then reduce(tree);
  result:=tree;
end;

function THomework.magnitude(tree: TNode): integer;
begin
  if tree.val is TInt
    then result:=tree.val.value
  else result:= (3 * magnitude(tree.left)) + (2 * magnitude(tree.right))
end;

{ TInt }
constructor TInt.create(value: integer);
begin
  fValue:=value;
end;

function TNode.getValue: integer;
begin
  if fVal <> nil
    then result := fVal.value
  else result:=0; //not strictly correct but we have to return something
end;

procedure TNode.setValue(val: integer);
begin
  if fVal = nil
    then fVal:= TInt.create(val)
  else fVal.value:=val;
end;

{ TNode }
constructor TNode.create(val: TInt);
begin
fLeft:=nil;
fRight:=nil;
fParent:=nil;
fVal:=val;
end;


//Old method below
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
//should be able to do this recursively
//need a 'find snailfish number' method
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

