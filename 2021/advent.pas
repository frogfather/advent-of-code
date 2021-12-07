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
    procedure CardNotifyWinHandler(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function getPuzzleInputAsStringArray(fileName: String; removeBlankLines:boolean=true):TStringArray;
    function stringOfBinaryToInteger(input:String):integer;
    function calculateCommonestValue(input: TStringArray; reverse:Boolean=false):TBits;
    function calculateLimitsForFuelCalc(input: TIntArray):TPoint;
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

const dataDir: string = '/Users/cloudsoft/Code/advent-of-code/2021/input/';
type
  TbingoCards = array of TbingoCard;
var
  mainForm: TmainForm;
  //Used in day 4 part 2. Because a card signals that it has won
  //by firing the event handler, and because the main method (day4part1)
  //doesn't know anything about which cards have won, we need to
  //keep a global list of these.
  winningCards: TBingoCards;

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

function TmainForm.getPuzzleInputAsStringArray(fileName: String; removeBlankLines: boolean=true): TStringArray;
begin
  result:= fileUtilities.openFileAsArray(datadir+filename,#$0A,removeBlankLines);
end;

function TmainForm.stringOfBinaryToInteger(input: String): integer;
var
 index,powerOf,elementLength:Integer;
 output:double;
begin
  elementLength:=length(input);
  output:=0;
  for index:= 1 to elementLength do
      begin
      powerOf:=elementLength - index;
      if (input[index]='1') then output:=output + power(2,powerOf);
      end;
  result:=round(output);
end;

{ day 1 }
procedure TmainForm.day1part1;
begin
  lbresults.items.add('not done yet');
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
function TmainForm.calculateCommonestValue(input: TStringArray; reverse:Boolean=false): TBits;
begin
  lbresults.items.add('not done yet');
end;

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

function TMainForm.calculateLimitsForFuelCalc(input: TIntArray):TPoint;
var
maxValue,totalValue,averageValue,index:integer;

  function getMaxValue(input:TIntArray):integer;
   var
   index:integer;
     begin
     result:=0;
     for index:=0 to pred(length(input)) do
       begin
       if (input[index] > result) then result:=input[index];
       end;
     end;

  begin
  maxValue:=getMaxValue(input);
  totalValue:=0;
  for index := 0 to pred(length(input)) do
    begin
    totalValue:=totalValue + input[index];
    end;
  averageValue:=totalValue div maxValue;
  //try values between average - 20% and average + 20%
  result.X:=averageValue - (length(input) div 5);
  result.Y:=averageValue + (length(input) div 5);
end;

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

