unit day13;

{$mode ObjFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils, aocPuzzle, LazLogger, ExtCtrls, Graphics, arrayUtils,routeFind;
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
    procedure getPairs;
    procedure runPuzzle;
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
  results.add(puzzleInputLines.size.toString+' lines');
  for index:= 0 to pred(puzzleInputLines.size) do
    begin
    currentLine:=puzzleInputLines[index].Trim;
    pairIndex:= (index + 1) mod 3;
    results.add('index '+index.tostring);

    if (pairIndex = 1) then currentPair.left:= currentLine
    else if (pairIndex = 2) then currentPair.right:= currentLine;
    if (pairIndex = 0)or(index = pred(puzzleInputLines.size)) then
      begin
      results.add('adding '+currentPair.left+'::'+currentPair.right);
      fPacketPairs.push(currentPair);
      currentPair.left:='';
      currentPair.right:='';
      end;
    end;
end;

procedure TDayThirteen.runPuzzle;
var
  pairIndex:integer;
  validIndices:integer;
begin
  validIndices:=0;
  for pairIndex:=0 to pred(fPacketPairs.size) do
    begin
    if (testPacket(pairIndex) = 1) then
      begin
      results.add('index '+(pairIndex+1).ToString+' valid');
      validIndices:=validIndices + pairIndex + 1;
      end;
    end;
  results.add('valid indices '+validIndices.toString);
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
    results.add('compare '+subPackets[index].left+' to '+subPackets[index].right);
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
begin

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

