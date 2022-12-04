unit aoc_22;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics,
  Dialogs, StdCtrls, Math, clipbrd, ExtCtrls, DateUtils, fpJSON,
  aocUtils, arrayUtils,iAoc,visualise,
  day1,day2,day3;

type

  { TMainForm }

  TMainForm = class(TForm)
    bExecute: TButton;
    bVisualise: TButton;
    cbSelect: TComboBox;
    ckTest: TCheckBox;
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

const aocDirectory = '/Users/cloudsoft/Code/advent-of-code/2022/';
const puzzleDataDirectory = aocDirectory+'input/';
const puzzleDescriptionDirectory = aocDirectory+'puzzle_description/';

procedure TMainForm.FormCreate(Sender: TObject);
begin
  clipboard := TClipboard.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  clipboard.Free;
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
end;

procedure TMainForm.cbSelectSelect(Sender: TObject);
var
  day, part: integer;
begin
  divMod(cbSelect.ItemIndex, 2, day, part);
  part:=succ(part);
  day:=succ(day);
  if ckTest.Checked then
     fPuzzleFile:= puzzleDataDirectory+'puzzle_' + day.ToString+ '_test.txt'
     else fpuzzleFile:= puzzleDataDirectory+'puzzle_' + day.ToString+ '.txt';
  case day of
   1: fpuzzle:= TDayOne.Create(fpuzzleFile);
   2: fpuzzle:= TDayTwo.Create(fPuzzleFile);
   3: fpuzzle:= TDayThree.Create(fPuzzleFile);
   4: fpuzzle:= TDayFour.Create(fPuzzleFile);
  end;
  bVisualise.Visible:=fVisualise.PaintBox1.OnPaint <> nil;
  bExecute.Enabled:=fPuzzle <> nil;
  fdescriptionFile := puzzleDescriptionDirectory+'puzzle_' + day.ToString + '_' + part.ToString + '.txt';
  loadText(fdescriptionFile);
end;

procedure TMainForm.ckTestChange(Sender: TObject);
var
  currentIndex:integer;
begin
  currentIndex:=cbSelect.ItemIndex;
  cbSelectSelect(self);
  //re-select the item in the combo box to reload the file

end;

procedure TMainForm.bExecuteClick(Sender: TObject);
var
  startTime, endTime: TDateTime;
begin
  lbresults.Clear;
  startTime := now;

  fpuzzle.run(cbSelect.ItemIndex mod 2 = 0);
  endTime:=now;
  lbResults.Items:=fPuzzle.getResults;
  lbResults.items.Insert(0,'Start '+ formatDateTime('hh:mm:ss:zz', startTime));
  lbResults.items.add('End '+formatDateTime('hh:mm:ss:zz',endTime));
  lbResults.Items.Add('Time: '+inttostr(millisecondsBetween(endTime,startTime))+' ms');
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
