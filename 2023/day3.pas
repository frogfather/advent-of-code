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

//For each line there may be numbers, dots or other symbols
//Include a line if:
//- There is a symbol next to it on the same line
//- There is a symbol on the previous or next line that occupies
//- the same position or the position before or the position after
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
  prevLine:='';
  for lineNo:=0 to pred(puzzleInputLines.size) do
    begin
      //Set the lines
      if (lineNo > 0) then prevLine:=puzzleInputLines[lineNo - 1]
        else prevLine:='';
      currentLine:=puzzleInputLines[lineNo];
      if (lineNo < pred(puzzleInputLines.size)) then nextLine:= puzzleInputLines[lineNo + 1]
        else nextLine:='';

      //Look for numbers on the current line
      foundAllNumbers:=false;
      linePos:=0;
      while not foundAllNumbers do
        begin
        sNumberToCheck:= findNextNumber(currentLine, linePos, numberStart, numberEnd);
        //Now check if this number has a symbol which is not a number and not a dot:
        //between numberStart - 1 and numberEnd + 1 on
        //the previous line, the current line or the next line
        if isValidPartNumber(prevLine, currentLine, nextLine, numberStart,numberEnd) then total:=total + sNumberToCheck.ToInteger;

        linePos:=numberEnd + 1;
        foundAllNumbers:= (numberStart = -1);
        end;
    end;
  results.add('Total is '+total.ToString);
end;

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
  //for each of the three inputs look for a character which is not a number
  //and is not a dot
  result:=false;
  if (start_ = -1) or (end_ = -1) then exit;
  //adjust start and end if possible
  startLimit:=start_;
  if (startLimit > 0) then startLimit:=startLimit - 1;
  endLimit:= end_;
  //Assuming all lines the same length - they look as if they are
  if (endLimit < pred(curr_.Length)) then endLimit:=endLimit + 1;
  stringToSearch:= prev_.Substring(startLimit, 1 + endLimit - startLimit);
  stringToSearch:=stringToSearch + curr_.Substring(startLimit, 1 + endLimit - startLimit);
  stringToSearch:=stringToSearch + next_.Substring(startLimit, 1 + endLimit - startLimit);
  //Now look for character that is neither number or dot
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


procedure TDayThree.runPartTwo;
begin
  results.Clear;
end;

end.


