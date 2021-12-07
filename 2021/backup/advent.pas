unit advent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics,
  Dialogs, StdCtrls, fileUtilities, math,bingoCard,ventMap,fgl,DateUtils;

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
  puzzleInput:= aocUtils.getPuzzleInputAsStringArray('day_1_1.txt');
  increasingCount:=0;
  for index := 1 to length(puzzleInput) - 1 do
    begin
    if (strToInt(puzzleInput[index]) > strToInt(puzzleInput[index - 1]))
      then increasingCount := increasingCount +1;
    end;
  lbResults.Items.add(inttostr(increasingCount)+' entries are larger than the previous');
end;

procedure TmainForm.day1part2;
begin
  lbresults.items.add('not done yet');
end;

{ day 2 }
procedure TmainForm.day2part1;
begin
 lbresults.items.add('not done yet');
end;

procedure TmainForm.day2part2;
begin
 lbresults.items.add('not done yet');
end;

{ day 3 }
procedure TmainForm.day3part1;
begin
  lbresults.items.add('not done yet');
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

