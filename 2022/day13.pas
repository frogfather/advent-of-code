unit day13;

{$mode ObjFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils, aocPuzzle, LazLogger, ExtCtrls, Graphics, arrayUtils,routeFind,anysort;
type
  TPacketData = record
    left: string;
    right: string;
  end;

  TPacketDataArray = array of TPacketData;

  { TDayThirteen }

  TDayThirteen = class(TAocPuzzle)
  private
    fPacketPairs: TPacketDataArray;
    fAllPackets: TStringArray;
    procedure getPairs;
    procedure getPackets;
    procedure runPuzzle;
    procedure sortPackets(var arr: array of string; count: Integer);
    function separateNumbers(entry:TPacketData):TPacketDataArray;
    function doSeparateNumber(entry:string):TStringArray;
    function testPacket(packetIndex:integer):integer;
    function doProcessEntries(entry:TPacketData):integer;
    function isNumber(input:string):boolean;
  public
    constructor Create(filename: string; paintbox_: TPaintbox = nil);
    procedure runPartOne; override;
    procedure runPartTwo; override;
  end;

  { TPacketDataArrayHelper }

  TPacketDataArrayHelper = type helper for TPacketDataArray
    function size: integer;
    function push(element: TPacketData): integer;
  end;

implementation

{ TDayThirteen }

procedure TDayThirteen.getPairs;
var
  index,pairIndex:integer;
  currentPair:TPacketData;
  currentLine:String;
begin
  currentPair.left:='';
  currentPair.right:='';
  for index:= 0 to pred(puzzleInputLines.size) do
    begin
    currentLine:=puzzleInputLines[index].Trim;
    pairIndex:= (index + 1) mod 3;

    if (pairIndex = 1) then currentPair.left:= currentLine
    else if (pairIndex = 2) then currentPair.right:= currentLine;
    if (pairIndex = 0)or(index = pred(puzzleInputLines.size)) then
      begin
      fPacketPairs.push(currentPair);
      currentPair.left:='';
      currentPair.right:='';
      end;
    end;
end;

procedure TDayThirteen.getPackets;
var
  index:integer;
begin
  fAllPackets:=TStringArray.create;
  for index:= 0 to pred(puzzleInputLines.size) do
    begin
    if (puzzleInputLines[index]<>'') then fAllPackets.push(puzzleInputLines[index]);
    end;
  fAllPackets.push('[[2]]');
  fAllPackets.push('[[6]]');
end;

procedure TDayThirteen.runPuzzle;
var
  pairIndex:integer;
  validIndices:integer;
begin
  validIndices:=0;
  for pairIndex:=0 to pred(fPacketPairs.size) do
    if (testPacket(pairIndex) = 1)
      then validIndices:=validIndices + pairIndex + 1;
  results.add('valid indices '+validIndices.toString);
end;

function CompareStr(const d1,d2): integer;
var
  s1 : string absolute d1;
  s2 : string absolute d2;
  currentS1,currentS2:char;
  snumber1,snumber2:string;
  numbers1,numbers2:TIntArray;
  index,longestString,mostNumbers:integer;
  openBrS1,openBrS2:integer;
begin
  result:=0;
  numbers1:=TIntArray.create;
  numbers2:=TIntArray.create;
  sNumber1:='';
  sNumber2:='';
  openBrS1:=0;
  openBrS2:=0;
  if s1.Length > s2.Length
    then longestString:=s1.Length
    else longestString:=s2.Length;
  for index:= 1 to longestString do //strings 1 indexed!!!
    begin
    if (index <= s1.Length) then
      begin
      currentS1:=s1[index];
      if ord(currentS1) = 91 then openBrS1:= openBrS1+1 else
      if (ord(currentS1)>47)and(ord(currentS1)<58) then sNumber1:=sNumber1+s1.Substring(index-1,1) else
      if ((ord(currentS1)=44)or(ord(currentS1) = 93))then
        begin
        if (sNumber1.Length > 0) then
        numbers1.push(sNumber1.ToInteger) else numbers1.push(-1);
        sNumber1:='';
        end;
      end;
    if (index <= s2.Length) then
      begin
      currentS2:=s2[index];
      if ord(currentS2) = 91 then openBrS2:= openBrS2+1 else //open square bracket
      if (ord(currentS2)>47)and(ord(currentS2)<58) then sNumber2:=sNumber2+s2.Substring(index-1,1) else //number

      if ((ord(currentS2)=44)or(ord(currentS2) = 93))then
        begin
        if (sNumber2.Length > 0) then
        numbers2.push(sNumber2.ToInteger) else numbers2.push(-1);
        sNumber2:='';
        end;
      end;

    end;
  //Now we have info about the two strings

  //if neither has numbers then the one with more brackets is bigger
  if (numbers1.size = 0) and (numbers2.size = 0) and (openBrS1 <> openBrS2)  then
    result:= (openBrS1 - openBrS2) div abs(openBrS1 - openBrS2) else

  //if one has numbers and the other has none then the one with numbers is bigger
  if (numbers1.size = 0)and(numbers2.size > 0) then result:= -1
  else if (numbers1.size > 0)and(numbers2.size = 0) then result:= 1;
  //if the result is still 0 (neither bigger) then compare the numbers
  if (result = 0) then
    begin
    if numbers1.size > numbers2.size then mostNumbers:= numbers1.size
    else mostNumbers:=numbers2.size;
    for index:=0 to pred(mostNumbers) do
      begin
      if (index = numbers1.size)then result:= -1 else //1 has no more numbers
      if (index = numbers2.size) then result:= 1 else //2 has no more numbers
      if result = 0 then
        begin
        if (numbers1[index] <> numbers2[index]) then
          result:= (numbers1[index] - numbers2[index]) div abs(numbers1[index] - numbers2[index]);
        if (result <> 0) then exit;

        end;
      end;
    end;
end;

procedure TDayThirteen.sortPackets(var arr: array of string; count: Integer);
  begin
    anysort.AnySort(arr, Count, sizeof(string), @CompareStr)
  end;

function TDayThirteen.testPacket(packetIndex: integer): integer;
var
  entry:TPacketData;
begin
  entry.left:=fPacketPairs[packetIndex].left;
  entry.right:=fPacketPairs[packetIndex].right;
  result:= doProcessEntries(entry);
end;

function TDayThirteen.doProcessEntries(entry: TPacketData):integer;
var
  subPackets:TPacketDataArray;
  index:integer;
  leftIsNumber,rightIsNumber:boolean;
  leftNoValue,rightNoValue:boolean;
begin
  result:=0;
  subPackets:=separateNumbers(entry);
  for index:=0 to pred(subPackets.size) do
    begin
    leftNoValue:= subPackets[index].left.Length = 0;
    rightNoValue:=subPackets[index].right.Length = 0;
    leftIsNumber:=isNumber(subPackets[index].left);
    rightIsNumber:=isNumber(subPackets[index].right);
    if leftNoValue and not rightNoValue then //if lhs runs out of numbers
      begin
      result:=1;
      exit;
      end else
    if rightNoValue and not leftNoValue then //rhs runs out of numbers
      begin
      result:=-1;
      exit;
      end else
    if leftIsNumber and rightIsNumber then //both numbers
       begin
       if (subPackets[index].left.ToInteger > subPackets[index].right.ToInteger)
         then result:=-1 else
       if (subPackets[index].left.ToInteger < subPackets[index].right.ToInteger)
         then result:=1;
       if (result <> 0) then exit;
       end
      else
    if leftIsNumber then //left is number and right isn't
      begin
      subPackets[index].left:='['+subPackets[index].left+']';
      result:= doProcessEntries(subPackets[index]);
      if (result <> 0)then exit;
      end else
    if rightIsNumber then //right is number and left isn't
      begin
      subPackets[index].right:='['+subPackets[index].right+']';
      result:= doProcessEntries(subPackets[index]);
      if (result <> 0) then exit;
      end else
      begin  //both are lists
      result:= doProcessEntries(subPackets[index]);
      if (result <> 0) then exit;
      end;
    end;
end;

//a not very accurate check - just ensures no square brackets or commas
//good enough for this purpose
function TDayThirteen.isNumber(input: string): boolean;
begin
  result:=(input.Length > 0)
  and(input.IndexOf('[') = -1)
  and(input.IndexOf(']') = -1)
  and (input.IndexOf(',') = -1)
end;

//Takes a TPacketData record and returns an array of TPacketData
function TDayThirteen.separateNumbers(entry: TPacketData): TPacketDataArray;
var
  leftElements,rightElements:TStringArray;
  largest,index:integer;
  packet:TPacketData;
begin
  result:=TPacketDataArray.create;
  leftElements:=doSeparateNumber(entry.left);
  rightElements:=doSeparateNumber(entry.right);
  if (leftElements.size > rightElements.size)
    then largest:=leftElements.size
    else largest:=rightElements.size;
  for index:=0 to pred(largest) do
    begin
    if (index < leftElements.size)
      then packet.left:=leftElements[index]
      else packet.left:='';
    if (index < rightElements.size)
      then packet.right:= rightElements[index]
      else packet.right:='';
    result.push(packet);
    end;
end;

//separates the supplied string
function TDayThirteen.doSeparateNumber(entry: string): TStringArray;
var
  Input,sepNum,character:string;
  index,bracketCount:Integer;
  isSeparator:boolean;
begin
  result:=TStringArray.create;
  if entry.Length = 0 then
    begin
    result.push('');
    exit;
    end;
  if (entry.Length > 2)
    and (entry.Substring(0,1)='[')
    and (entry.Substring(pred(entry.Length),1)=']')
  then input:=entry.Substring(1, entry.Length - 2);
  bracketCount:=0;
  sepNum:='';
  for index:=0 to pred(input.Length) do
    begin
    character:=input.Substring(index,1);
    isSeparator:= (bracketCount = 0) and (sepNum.Length > 0) and (character = ',');
    if not isSeparator then sepNum:=sepNum + character; //add to the output
    if (character = '[')
      then bracketCount:=bracketCount+1
    else if (character = ']')
      then bracketCount:=bracketCount-1;
    if (bracketCount = 0) and (isSeparator or (index = pred(input.Length))) then
      begin
      result.push(sepNum);
      sepNum:='';
      end;
    end;
end;



constructor TDayThirteen.Create(filename: string; paintbox_: TPaintbox);
begin
  inherited Create(filename, 'Day 13', paintbox_);
  fPacketPairs:=TPacketDataArray.create;
end;

procedure TDayThirteen.runPartOne;
begin
  getPairs;
  runPuzzle;
end;

procedure TDayThirteen.runPartTwo;
var
  divider1,divider2:integer;
  index:integer;
begin
  getPackets;
  sortPackets(fAllPackets,fAllPackets.size);
  divider1:=-1;
  divider2:=-1;
  for index:=0 to pred(fAllPackets.size) do
    begin
    if (fAllPackets[index]='[[2]]')then divider1:=index+1 else
    if (fAllPackets[index]='[[6]]')then divider2:=index+1;
    results.add(fAllPackets[index]);
    end;
  if (divider1 > 0)and(divider2 > 0) then
      begin
      results.add('Decoder key '+(divider1*divider2).ToString);
      exit;
      end;
end;

//Helper class for TPacketData

{ TPacketDataHelper }

function TPacketDataArrayHelper.size: integer;
begin
  Result := length(self);
end;

function TPacketDataArrayHelper.push(element: TPacketData): integer;
begin
  setLength(Self, self.size + 1);
  self[pred(self.size)] := element;
  Result := self.size;
end;

end.

