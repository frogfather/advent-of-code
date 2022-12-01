unit aoc_22;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics,
  Dialogs, StdCtrls, Math, clipbrd, ExtCtrls, DateUtils, fpJSON,
  aocUtils, arrayUtils,iAoc,visualise,
  day1;

type

  { TMainForm }

  TMainForm = class(TForm)
    bExecute: TButton;
    bVisualise: TButton;
    cbSelect: TComboBox;
    lbResults: TListBox;
    Memo1: TMemo;
    clipboard: TClipboard;
    procedure bExecuteClick(Sender: TObject);
    procedure bVisualiseClick(Sender: TObject);
    procedure cbSelectSelect(Sender: TObject);
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

procedure TMainForm.bExecuteClick(Sender: TObject);
var
  startTime, endTime: TDateTime;
begin
  lbresults.Clear;
  startTime := now;
  lbResults.items.add('Run puzzle '+ formatDateTime('hh:mm:ss:zz', startTime));
  fpuzzle.run(cbSelect.ItemIndex mod 2 = 0);
  endTime:=now;
  lbResults.items.add('end '+formatDateTime('hh:mm:ss:zz',endTime));
  lbResults.Items.Add('Time: '+inttostr(millisecondsBetween(endTime,startTime))+' ms');

end;

procedure TMainForm.bVisualiseClick(Sender: TObject);
begin
  fVisualise.Show;
end;

procedure TMainForm.cbSelectSelect(Sender: TObject);
var
  day, part: integer;
begin
  divMod(cbSelect.ItemIndex, 2, day, part);
  fpuzzleFile:= 'puzzle_' + day.ToString+ '.txt';
  part:=succ(part);
  day:=succ(day);
  case day of
   1: fpuzzle:= TDayOne.Create(fpuzzleFile,fVisualise.paintbox1);
  end;
  bVisualise.Visible:=fVisualise.PaintBox1.OnPaint <> nil;
  fdescriptionFile := 'puzzle_' + day.ToString + '_' + part.ToString + '.txt';
  loadText(fdescriptionFile);

end;


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
  for i := 1 to 31 do
  begin
    cbSelect.Items.Add('Advent of code day ' + IntToStr(i) + ' part 1');
    cbSelect.Items.Add('Advent of code day ' + IntToStr(i) + ' part 2');
  end;
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
