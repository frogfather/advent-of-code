unit advent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, strutils;

type
  TNumbers = Array of string;
  { TForm1 }
  TForm1 = class(TForm)
    bExecute: TButton;
    cbSelect: TComboBox;
    lbResults: TListBox;
    OpenDialog1: TOpenDialog;
    procedure bExecuteClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function openFileAsArray(fnam: string; separator: char): TNumbers;
    function readstream (fnam: string): string;
    procedure writestream (fnam: string; txt: string);
    function foundInArray(inputArray: TNumbers; required, startat: integer): boolean;
    procedure day1part1;
    procedure day1part2;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.bExecuteClick(Sender: TObject);
var
  puzzlename: string;
begin
  lbresults.Clear;
  case cbselect.ItemIndex of
   0: day1part1;
   1: day1part2;
  end;

end;

procedure TForm1.FormShow(Sender: TObject);
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

//The puzzles
procedure TForm1.day1part1;
var
  numbers: TNumbers;
  i: integer;
  currentNumber, requiredNumber : integer;
begin
if OpenDialog1.Execute then
  begin
  //the entries in the file have a newline character at the end which
  //anywhere else would be \n but here in Pascal land is linefeed #$0A
  numbers:= openFileAsArray(opendialog1.FileName,#$0A);
  for i:= 0 to length(numbers)-1 do
    begin
    currentNumber:= strToInt(numbers[i]);
    requiredNumber:= 2020 - currentNumber;
    if foundinarray(numbers, requiredNumber, i + 1) then
      begin
      lbResults.Items.add('first at position '+inttostr(i)+': '+inttostr(currentNumber));
      lbResults.Items.add('second: '+inttostr(requiredNumber));
      lbResults.Items.add('product: '+inttostr(currentNumber * requiredNumber));
      end;
    end;
  end;
end;

//https://adventofcode.com/2020/day/1#part2
procedure TForm1.day1part2;
var
  numbers: TNumbers;
  i,j: integer;
  firstNumber, secondNumber, requiredNumber : integer;
begin
if OpenDialog1.Execute then
  begin
  numbers:= openfileasArray(opendialog1.FileName, #$0A);
  //starting at the beginning of the array, find the first value
  //and add it to the following values in turn
  //if the sum is less than 2020 (most won't be) then use the foundInArray method
  for i:= 0 to length(numbers)-1 do
    begin
    firstNumber:= strToInt(numbers[i]);
    for j:= i+1 to length(numbers) -1 do
      begin
      if j > length(numbers) - 1 then exit;
      secondNumber:= strToInt(numbers[j]);
      requiredNumber:= 2020 - (firstnumber + secondNumber);
      if requiredNumber > 0 then
        begin
        if foundinarray(numbers, requiredNumber, j + 1) then
          begin
          lbresults.Items.Add('first at position '+inttostr(i)+': '+inttostr(firstNumber));
          lbresults.Items.Add('second at position '+inttostr(j)+': '+inttostr(secondNumber));
          lbresults.Items.Add('third: '+inttostr(requiredNumber));
          lbresults.Items.Add('product: '+inttostr(firstNumber * secondNumber * requiredNumber));
          end;
        end;
      end;
    end;
  end;
end;


//Some useful methods that should be in a separate unit

function TForm1.readstream(fnam: string): string;
var
  strm: TFileStream;
  n: longint;
  txt: string;
begin
  txt := '';
  strm := TFileStream.Create(fnam, fmOpenRead);
  try
    n := strm.Size;
    SetLength(txt, n);
    strm.Read(txt[1], n);
  finally
    strm.Free;
  end;
  result := txt;
end;

procedure TForm1.writestream(fnam: string; txt: string);
var
  strm: TFileStream;
  n: longint;
begin
  strm := TFileStream.Create(fnam, fmCreate);
  n := Length(txt);
  try
    strm.Position := 0;
    strm.Write(txt[1], n);
  finally
    strm.Free;
  end;
end;

function TForm1.foundInArray(inputArray: TNumbers; required, startat: integer): boolean;
var
  i:integer;
begin
  if (startat > (length(inputArray) -1)) then exit;
  for i:=startat to length(inputArray)-1 do
    begin
    if (strtoint(inputArray[i]) = required) then
      begin
        result:=true;
        exit;
      end;
    end;
end;

function TForm1.openFileAsArray(fnam: string; separator: char): TNumbers;
var
  contents: string;
  numbers: TNumbers;
begin
if FileExists(OpenDialog1.FileName) then
  begin
  contents := readStream(OpenDialog1.FileName);
  numbers := contents.Split(separator);
  result:=numbers;
  end;
end;


end.

