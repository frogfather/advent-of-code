unit day13;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils;
type

  { TDayThirteen}
  TDayThirteen = class(TAocPuzzle)
  private
  function getMirrorValues(maxErrors:integer=0):integer;
  function getMirrorValue(mirror_:TStringArray;mismatches:integer):integer;
  function getMismatches(string1,string2:string):integer;
  function getLine(mirror_:TStringArray;lineId: integer):string;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayThirteen }

constructor TDayThirteen.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 13',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayThirteen.runPartOne;
begin
  results.Clear;
  results.add('Total is '+getMirrorValues(0).ToString);
end;

//Almost identical to part one but now we want positions where there is exactly one mismatch.
procedure TDayThirteen.runPartTwo;
begin
  results.Clear;
  results.add('Total is '+getMirrorValues(1).ToString);
end;

function TDayThirteen.getMirrorValues(maxErrors: integer): integer;
var
  currentMirror:TStringArray;
  lineNo,total:integer;
  currentLine:string;
begin
  currentMirror:=TStringArray.create;
  result:=0;
  currentMirror.clear;
  for lineNo:=0 to pred(puzzleInputLines.size) do
    begin
    currentLine:=puzzleInputLines[lineNo].Trim;
    if (currentLine.Length > 0) then
      currentMirror.push(currentLine);
    if (currentLine.Length = 0)or(lineNo = pred(puzzleInputLines.size)) then
      begin
      result:=result+getMirrorValue(currentMirror,maxErrors);
      currentMirror.clear;
      end;
    end;
end;

function TDayThirteen.getMirrorValue(mirror_: TStringArray; mismatches: integer
  ): integer;
var
  hreflection,vreflection:integer;
  compareStart,compareEnd:integer;
  mismatchCount,limit:integer;
  done:boolean;
  compLine1,compLine2:string;

  function outOfRange:boolean;
  begin
  result:= (compareStart < 0) or (compareEnd > limit)
  end;
//Probably could be simplified - quite a lot of repetition in this method!
begin
  limit:=pred(mirror_.size);
  for hReflection:=1 to limit do
    begin
    compareStart:=hReflection - 1;
    compareEnd:= hReflection;
    done:=false;
    mismatchCount:=0;
    repeat
    compLine1:=mirror_[compareStart];
    compLine2:=mirror_[compareEnd];
    mismatchCount:=mismatchCount + getMismatches(compLine1,compLine2);
    compareStart:=compareStart-1;
    compareEnd:=compareEnd+1;
    done:= outOfRange or (mismatchCount > mismatches);
    until done;
    if (mismatchCount = mismatches) then
      begin
      result:=100 * (hReflection);
      exit;
      end;
    end;
  limit:=pred(mirror_[0].Length);
  for vReflection:=1 to limit do
    begin
    compareStart:=vReflection -1;
    compareEnd:=vReflection;
    done:=false;
    mismatchCount:=0;
    repeat
    compLine1:=getLine(mirror_,compareStart);
    compLine2:=getLine(mirror_,compareEnd);
    mismatchCount:=mismatchCount + getMismatches(compLine1,compline2);
    compareStart:=compareStart-1;
    compareEnd:=compareEnd+1;
    done:= outOfRange or (mismatchCount > mismatches);
    until done;
    if (mismatchCount = mismatches) then
      begin
      result:=vReflection;
      exit;
      end;
    end;
results.add('*** No reflection found - your code is probably wrong!');
result:=-1; //shouldn't get here
end;

function TDayThirteen.getMismatches(string1, string2: string): integer;
var
  elementId:integer;
begin
  //Compare each element along the string and see how many elements match
  result:=0;
  for elementId:=0 to pred(string1.Length) do
    if (string1.Substring(elementId,1) <> string2.Substring(elementId,1))
      then result:=result+1;
end;

function TDayThirteen.getLine(mirror_: TStringArray; lineId: integer
  ): string;
var
  lineNo:integer;
begin
  result:='';
  for lineNo:=0 to pred(mirror_.size)do
    result:=result+mirror_[lineNo].Substring(lineId,1);

end;

end.


