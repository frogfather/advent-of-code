unit aoc24;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics,
  Dialogs, StdCtrls, Math, clipbrd, ExtCtrls, EpikTimer, DateUtils, fpJSON,
  aocUtils, arrayUtils,iAoc,visualise,
  day1,day2,day3,day4,day5, day6,day7, day8,day9,day10,day11,day12,day13,day14,day15,
  day16,day17,day18,day19,day20,day21,day22,day23,day24,day25;

type

  { TMainForm }

  TMainForm = class(TForm)
    bExecute: TButton;
    bVisualise: TButton;
    cbSelect: TComboBox;
    ckTest: TCheckBox;
    cbTestFile: TComboBox;
    ET: TEpikTimer;
    lbResults: TListBox;
    Memo1: TMemo;
    clipboard: TClipboard;
    procedure bExecuteClick(Sender: TObject);
    procedure bVisualiseClick(Sender: TObject);
    procedure cbSelectSelect(Sender: TObject);
    procedure ckTestChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbResultsSelectionChange(Sender: TObject; User: boolean);
  private
    fPuzzleFile: string;
    fDescriptionFile: string;
    fPuzzle: iAdvent;
    procedure loadText(fileName: string);
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

const aocDirectory = '/Users/johncampbell/Code/advent-of-code/2024/';
const puzzleDataDirectory = aocDirectory+'input/';
const puzzleDescriptionDirectory = aocDirectory+'puzzle_description/';

procedure TMainForm.FormCreate(Sender: TObject);
begin
  clipboard := TClipboard.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  clipboard.Free;
  ET.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  i: integer;
begin
  cbSelect.Clear;
  for i := 1 to 25 do
  begin
    cbSelect.Items.Add('Advent of code day ' + IntToStr(i) + ' part 1');
    cbSelect.Items.Add('Advent of code day ' + IntToStr(i) + ' part 2');
  end;
  ckTest.Enabled:=false;
end;

procedure TMainForm.cbSelectSelect(Sender: TObject);
var
  day, part: integer;
begin
  divMod(cbSelect.ItemIndex, 2, day, part);
  part:=succ(part);
  day:=succ(day);
  if ckTest.Checked then
    begin
    //TODO: If there's only one test file then use that, otherwise let the user select

    fPuzzleFile:= puzzleDataDirectory+'puzzle_' + day.ToString+'_'+part.ToString+ '_test.txt';
    end else fpuzzleFile:= puzzleDataDirectory+'puzzle_' + day.ToString+ '.txt';
    if not fileexists(fpuzzleFile) then
      begin
      lbresults.Items.add('Puzzle file '+fpuzzleFile+' does not exist');
      end;
  case day of
   1: fpuzzle:= TDayOne.Create(fpuzzleFile);
   2: fpuzzle:= TDayTwo.Create(fPuzzleFile);
   3: fpuzzle:= TDayThree.Create(fPuzzleFile);
   4: fpuzzle:= TDayFour.Create(fPuzzleFile);
   5: fpuzzle:= TDayFive.Create(fPuzzleFile);
   6: fpuzzle:= TDaySix.Create(fPuzzleFile);
   7: fpuzzle:= TDaySeven.Create(fPuzzleFile);
   8: fpuzzle:= TDayEight.Create(fPuzzleFile);
   9: fpuzzle:= TDayNine.Create(fPuzzlefile);
   10:fpuzzle:= TDayTen.Create(fPuzzleFile);
   11:fpuzzle:= TDayEleven.Create(fPuzzlefile);
   12:fpuzzle:= TDayTwelve.Create(fPuzzlefile);
   13:fpuzzle:= TDayThirteen.Create(fPuzzlefile);
   14:fpuzzle:= TDayFourteen.Create(fPuzzlefile,fVisualise.PaintBox1);
   15:fpuzzle:= TDayFifteen.Create(fPuzzlefile);
   16:fpuzzle:= TDaySixteen.Create(fPuzzlefile);
   17:fpuzzle:= TDaySeventeen.Create(fPuzzlefile);
   18:fpuzzle:= TDayEighteen.Create(fPuzzlefile);
   19:fpuzzle:= TDayNineteen.Create(fPuzzlefile);
   20:fpuzzle:= TDayTwenty.Create(fPuzzlefile);
   21:fpuzzle:= TDayTwentyOne.Create(fPuzzlefile);
   22:fpuzzle:= TDayTwentyTwo.Create(fPuzzlefile);
   23:fpuzzle:= TDayTwentyThree.Create(fPuzzlefile);
   24:fpuzzle:= TDayTwentyFour.Create(fPuzzlefile);
   25:fpuzzle:= TDayTwentyFive.Create(fPuzzlefile);

  end;
  bVisualise.Visible:=fVisualise.PaintBox1.OnPaint <> nil;
  bExecute.Enabled:=fPuzzle <> nil;
  ckTest.Enabled:=fPuzzle <> nil;
  fdescriptionFile := puzzleDescriptionDirectory+'puzzle_' + day.ToString + '_' + part.ToString + '.txt';
  loadText(fdescriptionFile);
end;

procedure TMainForm.ckTestChange(Sender: TObject);
begin
  //re-select the item in the combo box to reload the file
  cbSelectSelect(self);
end;

procedure TMainForm.bExecuteClick(Sender: TObject);
begin
  lbresults.Clear;
  ET.Clear;
  ET.Start;
  fpuzzle.run(cbSelect.ItemIndex mod 2 = 0);
  ET.Stop;
  lbResults.Items:=fPuzzle.getResults;
  lbResults.Items.Add('Time: '+(formatFloat('0.000',ET.Elapsed*1000000))+' microseconds');
end;

procedure TMainForm.bVisualiseClick(Sender: TObject);
begin
  fVisualise.Show;
end;

procedure TMainForm.lbResultsSelectionChange(Sender: TObject; User: boolean);
var
  lineNo: integer;
  copiedLines: string;
begin
  if (lbResults.Items.Count = 0) or (lbResults.SelCount = 0) then
    exit;
  clipboard.Clear;
  copiedLines := '';
  for lineNo := 0 to pred(lbResults.Items.Count) do
  begin
    if (lbResults.Selected[lineNo]) then
      copiedLines := copiedLines + lbResults.Items[lineNo] + #$0A;
  end;
  clipboard.AsText := copiedLines;
end;

procedure TMainForm.loadText(fileName: string);
begin
  memo1.Text := getDescription(fileName);
end;

end.

