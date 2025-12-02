unit day2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayTwo}
  TDayTwo = class(TAocPuzzle)
  private
  function getRanges:TStringArray;
  function isInvalid(input:Int64; partOne:boolean=true):Boolean;
  function inputIsRepeatedSequence(testString,sequenceString:String):boolean;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayTwo }

function TDayTwo.getRanges: TStringArray;
begin
  result:= puzzleInput.Split(',');
end;

function TDayTwo.isInvalid(input: Int64; partOne:boolean): Boolean;
var
  inputLength,seqStart,seqLength:integer;
  sInput,sequence:string;
begin
result:=false;
sInput:=input.ToString;
inputLength:=sInput.Length;
if (inputLength < 2) then exit;
if partOne then
  begin
  if ((inputlength mod 2) > 0) then exit;
  seqStart:=inputlength div 2;
  end
else seqStart:=1;

for seqLength:=seqStart to inputLength div 2 do
  begin
  sequence:=sInput.Substring(0,seqLength);
  if inputIsRepeatedSequence(sInput,sequence) then
    begin
    result:=true;
    exit;
    end;
  end;
end;

function TDayTwo.inputIsRepeatedSequence(testString, sequenceString:string): boolean;
var
  repeats,repeatItem:integer;
  sequenceGroup:string;
begin
//Is the sequenceString a multiple of testString?
result:=false;
if (testString.Length mod sequenceString.Length) > 0 then exit;
repeats:=testString.Length div sequenceString.Length;
sequenceGroup:='';
for repeatItem:=0 to pred(repeats) do
  sequenceGroup:=sequenceGroup + sequenceString;
if (sequenceGroup = testString) then result:=true;
end;

constructor TDayTwo.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 2',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayTwo.runPartOne;
var
  ranges:TStringArray;
  index,invalidEntries:integer;
  start,finish,counter,InvalidTotal:int64;
  range:String;
begin
  results.Clear;
  ranges:=getranges;
  invalidEntries:=0;
  invalidTotal:=0;
  for index:=0 to pred(ranges.size) do
    begin
    range:=ranges[index].trim;
    start:=range.Split('-')[0].ToInt64;
    finish:=range.Split('-')[1].ToInt64;
    for counter:=start to finish do
      begin
      if isInvalid(counter)then
        begin
        invalidEntries:=invalidEntries+1;
        invalidTotal:=invalidTotal+counter;
        end;
      end;
    end;
  results.add('invalid entries '+invalidEntries.ToString);
  results.add('invalid total '+invalidTotal.ToString);
end;

procedure TDayTwo.runPartTwo;
var
  ranges:TStringArray;
  index,invalidEntries:integer;
  start,finish,counter,InvalidTotal:int64;
  range:String;
  test:int64;
begin
  results.Clear;
  ranges:=getranges;
  invalidEntries:=0;
  invalidTotal:=0;
  for index:=0 to pred(ranges.size) do
    begin
    range:=ranges[index].trim;
    start:=range.Split('-')[0].ToInt64;
    finish:=range.Split('-')[1].ToInt64;
    for counter:=start to finish do
      begin
      if isInvalid(counter,false)then
        begin
        invalidEntries:=invalidEntries+1;
        invalidTotal:=invalidTotal+counter;
        end;
      end;
    end;
  results.add('invalid entries '+invalidEntries.ToString);
  results.add('invalid total '+invalidTotal.ToString);
end;


end.

                
