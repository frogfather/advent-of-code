unit day3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils,aocUtils;
type

  { TDayThree}
  TDayThree = class(TAocPuzzle)
  private
  function findNextNumber(currentLine:string; linePos:integer; out numberStart:integer; out numberEnd: integer):string;
  function isValidPartNumber(prev_,curr_, next_:string; start_,end_: integer):boolean;
  function findPositionOfNextAsterisk(currentLine:string; linePos:integer):integer;
  function findAdjacentNumbers(prev_,current_,next_: string; pos_:integer):TStringList;
  function extractNumber(input_:string; pos_: integer; backwards:boolean = false):string;
  function findStartOfNumber(input_:string;pos_:integer):integer;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayThree }

constructor TDayThree.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 3',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayThree.runPartOne;
var
  lineNo,linePos:integer;
  prevLine,currentLine,nextLine:string;
  sNumberToCheck:string;
  numberStart,numberEnd:integer;
  foundAllNumbers:boolean;
  total:integer;
begin
  results.Clear;
  total:=0;
  for lineNo:=0 to pred(puzzleInputLines.size) do
    begin
      if (lineNo > 0) then prevLine:=puzzleInputLines[lineNo - 1]
        else prevLine:='';
      currentLine:=puzzleInputLines[lineNo];
      if (lineNo < pred(puzzleInputLines.size)) then nextLine:= puzzleInputLines[lineNo + 1]
        else nextLine:='';

      foundAllNumbers:=false;
      linePos:=0;
      while not foundAllNumbers do
        begin
        sNumberToCheck:= findNextNumber(currentLine, linePos, numberStart, numberEnd);
        if isValidPartNumber(prevLine, currentLine, nextLine, numberStart,numberEnd)
          then total:=total + sNumberToCheck.ToInteger;

        linePos:=numberEnd + 1;
        foundAllNumbers:= (numberStart = -1);
        end;
    end;
  results.add('Total is '+total.ToString);
end;

procedure TDayThree.runPartTwo;
var
  lineNo,linePos:integer;
  prevLine,currentLine,nextLine:string;
  asteriskPos:integer;
  foundAllAsterisksOnLine:boolean;
  total:integer;
  adjacentNumbers:TStringList;

  function adjacentNumbersProduct:integer;
  begin
    result:=adjacentNumbers[0].ToInteger * adjacentNumbers[1].ToInteger;
  end;

begin
  results.Clear;
  total:=0;
  for lineNo:=0 to pred(puzzleInputLines.size) do
    begin
      if (lineNo > 0) then prevLine:=puzzleInputLines[lineNo - 1]
        else prevLine:='';
      currentLine:=puzzleInputLines[lineNo];
      if (lineNo < pred(puzzleInputLines.size)) then nextLine:= puzzleInputLines[lineNo + 1]
        else nextLine:='';

      foundAllAsterisksOnLine:=false;
      linePos:=0;
      while not foundAllAsterisksOnLine do
        begin
        asteriskPos:=findPositionOfNextAsterisk(currentLine,linePos);
        if (asteriskPos > -1)
          then
            begin
            adjacentNumbers:=findAdjacentNumbers(prevLine,currentLine,nextLine,asteriskPos);
            if (adjacentNumbers.Count = 2)
              then total:=total+ adjacentNumbersProduct;
            end;

        linePos:=asteriskPos + 1;
        foundAllAsterisksOnLine:= (asteriskPos = -1);
        end;
    end;
  results.add('Total is '+total.ToString);
end;

//-----------Methods for part one --------------

//Returns the next number found on the string. The two out parameters hold the start and end
function TDayThree.findNextNumber(currentLine: string; linePos: integer; out
  numberStart: integer; out numberEnd: integer): string;
var
  element:integer;
  finished:boolean;

  //Wee helper function to make the code a little more readable
  function nextElementNotNumber:boolean;
    begin
    result:= (element = pred(currentLine.Length))
      or ((element < pred(currentLine.Length)) and not isNumber(currentLine.Substring(element+1,1)))
    end;

begin
  result:='';
  numberStart:=-1;
  numberEnd:=-1;
  if (linePos >= pred(currentLine.Length)) then exit;
  element:=linePos;
  result:= '';
  repeat

  if (isNumber(currentLine.Substring(element,1)) and (numberStart = -1))
    then numberStart:= element;

  if nextElementNotNumber and (numberEnd = -1) and (numberStart > -1)
    then numberEnd:= element;
  if (numberStart > -1) then result:=result + currentLine.Substring(element,1);
  finished:= (numberStart > -1) and (numberEnd > -1);
  //Finished either if we've found a number or hit the end of the row
  element:=element+1;
  finished:= ((numberStart > -1) and (numberEnd > -1)) or (element = currentLine.Length)
  until finished;
end;

function TDayThree.isValidPartNumber(prev_, curr_, next_: string; start_,
  end_: integer): boolean;
var
  startLimit,endLimit:integer;
  stringToSearch,element:string;
  index:integer;
begin
  result:=false;
  if (start_ = -1) or (end_ = -1) then exit;
  startLimit:=start_;
  if (startLimit > 0) then startLimit:=startLimit - 1;
  endLimit:= end_;
  if (endLimit < pred(curr_.Length)) then endLimit:=endLimit + 1;
  stringToSearch:= prev_.Substring(startLimit, 1 + endLimit - startLimit);
  stringToSearch:=stringToSearch + curr_.Substring(startLimit, 1 + endLimit - startLimit);
  stringToSearch:=stringToSearch + next_.Substring(startLimit, 1 + endLimit - startLimit);
  for index:= 0 to pred(stringToSearch.Length) do
    begin
    element:= stringToSearch.Substring(index,1);
    if not isNumber(element) and (element <> '.') then
      begin
        result:=true;
        exit;
      end;
    end;
end;

//------Methods for part two --------------------
function TDayThree.findPositionOfNextAsterisk(currentLine:string; linePos:integer):integer;
var
  index:integer;
  done:boolean;
begin
  result:=-1;
  index:=linePos;
  done:=false;
  while not done do
    begin
    if (currentLine.Substring(index,1) = '*') then
      begin
        result:=index;
        exit;
      end;
    done := (index = pred(currentLine.Length));
    if not done then index:=index + 1;
    end;
end;

function TDayThree.findAdjacentNumbers(prev_, current_, next_: string;
  pos_: integer): TStringList;
begin
  result:=TStringlist.Create;
  //1) are there adjacent numbers in the current line?
  if (isNumber(current_.Substring(pos_,1)))
    then result.Add(extractNumber(current_,findStartOfNumber(current_,pos_))) else
    begin
    if (pos_ > 0) and (isNumber(current_.Substring(pos_-1,1)))
      then result.Add(extractNumber(current_,pos_,true));
    if ((pos_ < pred(current_.Length)) and (isNumber(current_.Substring(pos_+1,1))))
      then result.Add(extractNumber(current_,pos_));
    end;
  //2) are there adjacent numbers in the line above?
  if (isNumber(prev_.Substring(pos_,1)))
    then result.Add(extractNumber(prev_,findStartOfNumber(prev_,pos_))) else
    begin
    if (pos_ > 0) and (isNumber(prev_.Substring(pos_-1,1)))
      then result.Add(extractNumber(prev_,pos_,true));
    if ((pos_ < pred(prev_.Length)) and (isNumber(prev_.Substring(pos_+1,1))))
      then result.Add(extractNumber(prev_,pos_));
    end;
  //3) are there adjacent numbers in the next line?
  if (isNumber(next_.Substring(pos_,1)))
    then result.Add(extractNumber(next_,findStartOfNumber(next_,pos_))) else
    begin
    if (pos_ > 0) and (isNumber(next_.Substring(pos_-1,1)))
      then result.Add(extractNumber(next_,pos_,true));
    if ((pos_ < pred(next_.Length)) and (isNumber(next_.Substring(pos_+1,1))))
      then result.Add(extractNumber(next_,pos_));
    end;
end;

function TDayThree.extractNumber(input_: string; pos_: integer;
  backwards: boolean): string;
var
  index,offset:integer;
  done:boolean;
  element:string;
begin
  //From the specified position return characters that are numbers up to the next non numerical character
  result:= '';
  if backwards then offset:=-1 else offset:= 1;
  done:=false;
  index:=pos_;
  while not done do
    begin
    element:=input_.Substring(index,1);
    if isNumber(element) then
      begin
        if backwards then result:= element+result
        else result:=result+element;
      end else if (result <> '') then exit;
    index:=index+offset;
    done:= (backwards and (index < 0)) or (not backwards and (index = input_.Length));
    end;
end;

function TDayThree.findStartOfNumber(input_: string; pos_: integer): integer;
var
  index,offset:integer;
  onNumber,done:boolean;
begin
  //If we're not currently on a number move forward til we find one
  //otherwise move backwards
  result:=-1;
  onNumber:=isNumber(input_.substring(pos_,1));
  if not onNumber then offset:=1 else offset:=-1;
  done:=false;
  index:=pos_;
  while not done do
    begin
    if (not isNumber(input_.Substring(index,1))) or (index = 0) or (index = pred(input_.Length)) then
      begin
      if onNumber then
        begin
        result:=index - offset;
        exit;
        end;
      end else onNumber:=true;
    index:=index+offset;
    done:=(index < 0) or (index = input_.Length);
    end;
end;

end.


