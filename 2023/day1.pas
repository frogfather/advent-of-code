unit day1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils,aocUtils;
type

  { TDayOne }
  TDayOne = class(TAocPuzzle)
  private
  function getFirstAndLastNumbers(input:String):String;
  function getFirstAndLastNumbersIncludingWords(input:String):String;
  function firstNumberWord(input:string; startAtEnd:boolean=false):string;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

const numberWords: array[0..17] of string = ('one','two','three','four','five','six','seven','eight','nine','1','2','3','4','5','6','7','8','9');

{ TDayOne }

constructor TDayOne.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 1',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayOne.runPartOne;
var
  lineNo:integer;
  sFirstAndLastNumbers:string;
  total:integer;
begin
  results.Clear;
  { #todo : A forEach method on TStringArray would be nice }
  //Strings consisting of alphanumerics
  //We want just the numbers
  total:=0;
  for lineNo:= 0 to pred(puzzleInputLines.size) do
    begin
    sFirstAndLastNumbers:= getFirstAndLastNumbers(puzzleInputLines[lineNo]);
    //We're only interested in the first and last numeric character
    total:=total + sFirstAndLastNumbers.ToInteger;
    end;
  results.Add('Total is '+total.ToString);
end;

procedure TDayOne.runPartTwo;
var
  lineNo:integer;
  sFirstAndLastNumbers:string;
  total:integer;
begin
  results.Clear;
  //Much the same but we're looking for three letter words for numbers as well
  //e.g. 'one', 'two'
  total:=0;
  for lineNo:= 0 to pred(puzzleInputLines.size) do
    begin
    sFirstAndLastNumbers:= getFirstAndLastNumbersIncludingWords(puzzleInputLines[lineNo]);
    total:=total+sFirstAndLastNumbers.ToInteger;
    end;
  results.Add('Total is '+total.ToString);
end;

function TDayOne.getFirstAndLastNumbers(input: String): String;
var
  index:integer;
  firstFound,lastFound:boolean;

begin
  result:='';
  firstFound:=false;

  index:=1; //*** Your regular reminder that strings are 1 indexed *** !
  while not firstFound do
    begin
    if (index > input.Length) then exit;
    firstFound:= isNumber(input[index]);
    if firstFound then result:=result+input[index] else index:=index+1;
    end;

  lastFound:=false;
  index:=input.length;
  while not lastFound do
    begin
    if (index = 0) then exit;
    lastFound:= isNumber(input[index]);
    if lastFound then result:=result+input[index] else index:=pred(index);
    end;
end;

function TDayOne.getFirstAndLastNumbersIncludingWords(input: String): String;
var
  firstNumber,lastNumber:string; //may be '5' or 'five'
begin
  result:='';
  firstNumber:=firstNumberWord(input);
  lastNumber:=firstNumberWord(input,true);
  result:=firstNumber+lastNumber;
end;

function TDayOne.firstNumberWord(input:string; startAtEnd:boolean=false):string;
var
  index:integer;
  wordPosition:integer; //current word position
  outermostWord:integer; //keep track of word nearest start or end
begin
  if startAtEnd then outermostWord := -1 else outermostWord:=input.Length;
  result:='';
  for index:=0 to pred(length(numberWords)) do
    begin
    if startAtEnd then wordPosition:= input.LastIndexOf(numberWords[index])
    else wordPosition:=input.IndexOf(numberWords[index]);
    if (wordPosition = -1) then continue;

    if ((startAtEnd)and(wordPosition > outermostWord))
      or ((not startAtEnd)and(wordPosition < outermostWord)) then
      begin
        outermostWord:=wordPosition;
        //If the index is < 9 then we've found a three letter string.
        //For convenience convert to single char string here.
        if (index < 9) then result:= (index + 1).ToString
        else result:=numberWords[index];
      end;

    end;
end;

end.


