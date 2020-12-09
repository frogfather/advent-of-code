unit advent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, strutils;

type
  TStringArray = Array of string;
  TIntArray = Array of integer;
  { TForm1 }
  TForm1 = class(TForm)
    bExecute: TButton;
    cbSelect: TComboBox;
    lbResults: TListBox;
    OpenDialog1: TOpenDialog;
    procedure bExecuteClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function openFileAsArray(fnam: string; separator: char): TStringArray;
    function readstream (fnam: string): string;
    procedure writestream (fnam: string; txt: string);
    function foundInArray(inputArray: TStringArray; required, startat: integer): boolean;
    procedure day1part1;
    procedure day1part2;
    procedure day8part1;
    procedure day8part2;

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
   14: day8part1;
   15: day8part2;
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
  numbers: TStringArray;
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
  numbers: TStringArray;
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

procedure TForm1.day8part1;
var
  instructions: TStringArray;
  visited: TIntArray;
  command, instruction:String;
  splitInstruction: TStringArray;
  pos, acc, i, moveBy, commandAmount: integer;
  done: boolean;
begin
if OpenDialog1.Execute then
  begin
  instructions:= openfileasArray(opendialog1.FileName, #$0A);
  setlength(visited, length(instructions));
  lbresults.items.add('lines '+inttostr(length(visited)));
  for i:= 0 to length(visited)-1 do visited[i]:=0;
  //need a counter and some method of recording what line we've visited
  done:=false;
  acc:=0;
  pos:=0;
  moveBy:=1;
    while not done do
      begin
      //check value of visited[pos] If it is already 1 quit and log acc
      //look at the instruction. Inc value of visited[pos] by 1
      //If nop do nothing
      //If acc increase acc by specified amount
      //If jmp we need to find how much by and set moveBy to that amount
      //Adjust pos by moveBy
      done:=(visited[pos]= 1) or (pos > length(visited) -1);
      if done then lbresults.items.add('Previously been at pos '+intToStr(pos)+' '+inttostr(acc));
      instruction:=instructions[pos];
      visited[pos]:=visited[pos]+1;
      splitInstruction:=instruction.Split(' ');
      command:=splitInstruction[0];
      commandAmount:=StrToInt(splitInstruction[1]);
      case command of
       'jmp':
         moveBy:=commandAmount;
       'acc':
         begin
         moveBy:=1;
         acc:=acc + commandAmount;
         end;
       'nop':
         moveBy:=1;
      end;
      lbresults.Items.add('pos: '+inttostr(pos)+' acc: '+inttostr(acc)+' cmd: '+command+ ' cmd amount: '+inttostr(commandAmount));
      pos:=pos + moveBy;
    end;
  end;
end;

procedure TForm1.day8part2;
var
  instructions: TStringArray;
  visited: TIntArray;
  command, instruction:String;
  splitInstruction: TStringArray;
  pos, acc, i, moveBy, commandAmount: integer;
  done: boolean;
begin
if OpenDialog1.Execute then
  begin
  instructions:= openfileasArray(opendialog1.FileName, #$0A);
  setlength(visited, length(instructions));
  lbresults.items.add('lines '+inttostr(length(visited)));
  for i:= 0 to length(visited)-1 do visited[i]:=0;
  //need a counter and some method of recording what line we've visited
  done:=false;
  acc:=0;
  pos:=0;
  moveBy:=1;
    while not done do
      begin
      //check value of visited[pos] If it is already 1 quit and log acc
      //look at the instruction. Inc value of visited[pos] by 1
      //If nop do nothing
      //If acc increase acc by specified amount
      //If jmp we need to find how much by and set moveBy to that amount
      //Adjust pos by moveBy
      done:=visited[pos]= 1;
      if done then lbresults.items.add('Previously been here '+inttostr(acc));
      instruction:=instructions[pos];
      visited[pos]:=visited[pos]+1;
      splitInstruction:=instruction.Split(' ');
      command:=splitInstruction[0];
      commandAmount:=StrToInt(splitInstruction[1]);
      case command of
       'jmp':
         moveBy:=commandAmount;
       'acc':
         begin
         moveBy:=1;
         acc:=acc + commandAmount;
         end;
       'nop':
         moveBy:=1;
      end;
      lbresults.Items.add('pos: '+inttostr(pos)+' acc: '+inttostr(acc)+' cmd: '+command+ ' cmd amount: '+inttostr(commandAmount));
      pos:=pos + moveBy;
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

function TForm1.foundInArray(inputArray: TStringArray; required, startat: integer): boolean;
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

function TForm1.openFileAsArray(fnam: string; separator: char): TStringArray;
var
  contents: string;
  lines: TStringArray;
begin
if FileExists(OpenDialog1.FileName) then
  begin
  contents := readStream(OpenDialog1.FileName);
  lines := contents.Split(separator);
  result:=lines;
  end;
end;


end.

