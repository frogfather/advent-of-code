unit advent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics,
  Dialogs, StdCtrls, fileUtilities, math,bingoCard,
  ventMap,fgl,DateUtils,aocUtils;

type

  { TmainForm }
  TmainForm = class(TForm)
    bExecute: TButton;
    cbSelect: TComboBox;
    lbResults: TListBox;
    OpenDialog1: TOpenDialog;
    procedure bExecuteClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure day1part1;
    procedure day1part2;
    procedure day2part1;
    procedure day2part2;
    procedure day3part1;
    procedure day3part2;
    procedure day4part1;
    procedure day4part2;
    procedure day5part1;
    procedure day5part2;
    procedure day6part1;
    procedure day6part2;
    procedure day7part1;
    procedure day7part2;
    procedure day8part1;
    procedure day8part2;

  public

  end;

type
  TbingoCards = array of TbingoCard;
var
  mainForm: TmainForm;

implementation

{$R *.lfm}

{ TmainForm }

procedure TmainForm.bExecuteClick(Sender: TObject);
var
  startTime,endTime:TDateTime;
begin
  lbresults.Clear;
  startTime:=now;
  lbResults.items.add('start '+formatDateTime('hh:mm:ss:zz',startTime));
  case cbselect.ItemIndex of
   0: day1part1;
   1: day1part2;
   2: day2part1;
   3: day2part2;
   4: day3part1;
   5: day3part2;
   6: day4part1;
   7: day4part2;
   8: day5part1;
   9: day5part2;
   10: day6part1;
   11: day6part2;
   12: day7part1;
   13: day7part2;
   14: day8part1;
   15: day8part2;
  end;
 endTime:=now;
 lbResults.items.add('end '+formatDateTime('hh:mm:ss:zz',endTime));
 lbResults.Items.Add('Time: '+inttostr(millisecondsBetween(endTime,startTime))+' ms');
end;

procedure TmainForm.FormShow(Sender: TObject);
var
  i:integer;
begin
  cbSelect.Clear;
  for i:=1 to 31 do
    begin
      cbSelect.Items.Add('Advent of code day '+inttostr(i)+' part 1');
      cbSelect.Items.Add('Advent of code day '+inttostr(i)+' part 2');
    end;
end;


{ day 1 }
procedure TmainForm.day1part1;
  //https://adventofcode.com/2021/day/1
var
  puzzleInput:TStringArray;
  index, increasingCount:integer;
begin
  puzzleInput:= getPuzzleInputAsStringArray('day_1_1.txt');
  increasingCount:=0;
  for index := 1 to length(puzzleInput) - 1 do
    begin
    if (strToInt(puzzleInput[index]) > strToInt(puzzleInput[index - 1]))
      then increasingCount := increasingCount +1;
    end;
  lbResults.Items.add(inttostr(increasingCount)+' entries are larger than the previous');
end;

procedure TmainForm.day1part2;
//Similar to part 1 but compare the average of three samples
var
  puzzleInput: TStringArray;
  index, firstSetIndex,secondSetIndex, increasingCount,avg1,avg2:integer;
begin
  puzzleInput:= getPuzzleInputAsStringArray('day_1_1.txt');
  increasingCount:=0;
  //we start at 3 because we need to compare the first three entries to
  //the second three entries (i.e. comparing 0,1,2 to 1,2,3)
  for index:=3 to pred(length(puzzleInput)) do
    begin
    avg1:=0;
    avg2:=0;
     for firstSetIndex:=index-1 downto index-3 do
       avg1:=avg1+strToInt(puzzleInput[firstSetIndex]);
     for secondSetIndex:=index downto index - 2 do
       avg2:=avg2+strToInt(puzzleInput[secondSetIndex]);
     if (avg2 > avg1) then increasingCount:=increasingCount + 1;
    end;
  lbResults.Items.add(inttostr(increasingCount)+' entries are larger than the previous');
end;

{ day 2 }
procedure TmainForm.day2part1;
//https://adventofcode.com/2021/day/2
var
  puzzleInput: TStringArray;
  elements:TStringArray;
  command: string;
  index,value: integer;
  horPos,depth:integer;
begin
  puzzleInput:=getPuzzleInputAsStringArray('day_2_1.txt');
  horPos:=0;
  depth:=0;
  for index:=0 to pred(length(puzzleinput)) do
    begin
    elements:=puzzleInput[index].Split(' ');
    command:=elements[0];
    value:=strToInt(elements[1]);
    case command of
     'forward': horPos:=horPos+value;
     'down': depth:= depth + value;
     'up': depth:= depth - value;
     end;
    end;
  lbresults.Items.add('depth * distance = '+inttostr(horPos * depth));
end;

procedure TmainForm.day2part2;
//similar to part 1 but with additional parameter and more calculations
 var
  puzzleInput: TStringArray;
  elements:TStringArray;
  command: string;
  index,value: integer;
  horPos,depth,aim:integer;
begin
  puzzleInput:=getPuzzleInputAsStringArray('day_2_1.txt');
  horPos:=0;
  depth:=0;
  aim:=0;
  for index:=0 to pred(length(puzzleinput)) do
    begin
    elements:=puzzleInput[index].Split(' ');
    command:=elements[0];
    value:=strToInt(elements[1]);
    case command of
     'forward':
       begin
       horPos:=horPos+value;
       depth:=depth+(aim * value);
       end;
     'down':
       begin
       aim:=aim + value;
       end;
     'up':
       begin
       aim:=aim - value;
       end;
     end;
    end;
  lbresults.Items.add('depth * distance = '+inttostr(horPos * depth));
end;

{ day 3 }
procedure TmainForm.day3part1;
 var
   puzzleInput: TStringArray;
   index, powerOf:integer;
   gamma,epsilon:double;
   bBits:TBits;
begin
  puzzleInput:=getPuzzleInputAsStringArray('day_3_1.txt');
  bBits:= calculateCommonestValue(puzzleInput);
  gamma:=0;
  epsilon:=0;

  for index:= 0 to pred(bBits.Size) do
    begin
    powerOf:=bBits.Size - (index + 1);
    if bBits[index]=true then gamma:=gamma + power(2,powerOf)
      else epsilon:=epsilon + power(2,powerOf);
    end;
    lbResults.Items.add('gamma and epsilon: '+formatFloat('0',gamma)+' and '+formatFloat('0',epsilon));
    lbResults.Items.add('their product is '+formatFloat('0',gamma*epsilon));
end;

procedure TmainForm.day3part2;
begin
 lbresults.items.add('not done yet');
end;

{ day 4 }
procedure TmainForm.day4part1;
begin
 lbresults.items.add('not done yet');
end;

procedure TmainForm.day4part2;
begin
  lbresults.items.add('not done yet');
end;

{ day 5 }
procedure TmainForm.day5part1;
begin
 lbresults.items.add('not done yet');
end;

procedure TmainForm.day5part2;
begin
 lbresults.items.add('not done yet');
end;

{ day 6 }
procedure TmainForm.day6part1;
begin
  lbresults.items.add('not done yet');
end;

procedure TmainForm.day6part2;
begin
 lbresults.items.add('not done yet');
end;

{ day 7 }

procedure TmainForm.day7part1;
begin
  lbresults.items.add('not done yet');
end;

procedure TmainForm.day7part2;
begin
  lbresults.items.add('not done yet');
end;

procedure TmainForm.day8part1;
begin
  lbresults.items.add('not done yet');
end;

procedure TmainForm.day8part2;
begin
  lbresults.items.add('not done yet');
end;





end.

