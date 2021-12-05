unit advent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics,
  Dialogs, StdCtrls, fileUtilities, math,bingoCard;

type
  TStringArray = Array of string;
  TIntArray = Array of integer;
  { TForm1 }
  TForm1 = class(TForm)
    bExecute: TButton;
    Button1: TButton;
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
  Form1: TForm1;
  //Used in day 4 part 2. Because a card signals that it has won
  //by firing the event handler, and because the main method (day4part1)
  //doesn't know anything about which cards have won, we need to
  //keep a global list of these.
  winningCards: TBingoCards;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.bExecuteClick(Sender: TObject);

begin
  lbresults.Clear;
  case cbselect.ItemIndex of
   0: day1part1;
   1: day1part2;
   2: day2part1;
   3: day2part2;
   4: day3part1;
   5: day3part2;
   6: day4part1;
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

function TForm1.getPuzzleInputAsStringArray(fileName: String; removeBlankLines: boolean=true): TStringArray;
begin
  result:= fileUtilities.openFileAsArray(datadir+filename,#$0A,removeBlankLines);
end;

function TForm1.stringOfBinaryToInteger(input: String): integer;
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

//https://adventofcode.com/2021/day/1
procedure TForm1.day1part1;
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

//Similar to part 1 but compare the average of three samples
procedure TForm1.day1part2;
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
//https://adventofcode.com/2021/day/2
procedure TForm1.day2part1;
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
//similar to part 1 but with additional parameter and more calculations
procedure TForm1.day2part2;
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

{ Day three }

function TForm1.calculateCommonestValue(input: TStringArray; reverse:Boolean=false): TBits;
type
  TintArray = array of integer;
 var
   element, index, elementLength:integer;
   intArray: TintArray;
   bBits:TBits;
begin
  elementLength:=length(input[0]);
  //Initialize array with all zeros
  intArray:=TintArray.create;
  setLength(intArray,elementLength);
  bBits:=TBits.create(elementLength);
  for index:= 0 to pred(length(intArray)) do
    begin
    intArray[index]:=0;
    end;
  for element:=0 to pred(length(input)) do
    begin
    for index:=0 to pred(length(intArray)) do
      //NB strings are indexed from 1!
      begin
      if (input[element][index+1] = '1')
        then intArray[index]:=intArray[index]+1;
      end;
    end;
    for index:= 0 to pred(elementLength) do
      begin
      //are more than half the entries 1s?
      if ((intArray[index] * 2) >= length(input))
        then bBits[index]:=true else bBits[index]:=false;
      if reverse then bBits[index]:=not bBits[index];
      end;
    result:= bBits;
end;
procedure TForm1.day3part1;
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
procedure TForm1.day3part2;
var
  oxygen,co2: TStringArray;
  sOxygen,sCo2: string;

  { Nested function to find a unique entry that matches the requirements}
  function getUniqueEntry(input: TStringArray;reverse:boolean=false):String;
  var
   entry,entryLength,element:integer;
   mostOnesAt: TBits;
   keepValue: integer;
  begin
    if length(input) = 0 then exit;
    entryLength:=length(input[0]);
    for element:=0 to pred(entryLength) do
      begin
      //Get the TBits object which tells us if 1
      //is the most common value at each index for the current set
      mostOnesAt:=calculateCommonestValue(input,reverse);
      if (mostOnesAt[element] = true) then keepValue:=1 else keepValue:=0;
      for entry:=pred(length(input)) downto 0 do
        begin
        if (strToInt(input[entry][element+1]) <> keepValue)
          then fileUtilities.deleteFromArray(input,entry);
        if (length(input)=1) then
          begin
            result:=input[0];
            exit;
          end;
        end;
      end;
    end;

begin
  oxygen:=getPuzzleInputAsStringArray('day_3_1.txt');
  co2:=copy(oxygen,0);
  sOxygen:=getUniqueEntry(oxygen);
  sCo2:=getUniqueEntry(co2,true);
  lbResults.items.add('oxygen '+sOxygen);
  lbResults.items.add('co2 '+sCo2);
  lbResults.items.add('life support rating '+inttostr(stringOfBinaryToInteger(sOxygen)*stringOfBinaryToInteger(sCo2)));

end;

//This is the method that any winning bingo card will call
procedure TForm1.CardNotifyWinHandler(Sender: TObject);
var
 winningCardIndex:integer;
 found:boolean;
begin
  with Sender as TBingoCard do
    begin
    //add the winning card to the winningCards list if it isn't there

    found:=false;
    for winningCardIndex:=0 to pred(length(winningCards)) do
      begin
      if (winningCards[winningCardIndex] = sender as TBingoCard) then found:=true;
      end;

    if not found then
      begin
      lbResults.items.add('Card '+id.ToString+' added to winning cards list');
      setLength(winningCards, length(winningCards)+1);
      winningCards[pred(length(winningCards))]:=sender as TBingoCard;
      end;
    end;
end;

procedure TForm1.day4part1;
var
 puzzleInput,numbersToCall:TStringArray;
 currentLine:string;
 currentCardData:TStringArray; //The block of numbers to passed to the card constructor
 bingoCards: TBingoCards;
 cardNumber:integer;
 lineNumber,numberOfLines,callNumber:integer;
 firstWinning,lastWinning:TBingoCard;
begin
 //Get the puzzle input without removing blank lines as we need these
 puzzleInput:= getPuzzleInputAsStringArray('day_4_1.txt',false);
 numberOfLines:=length(puzzleInput);
 //The first line is the numbers that will be called.
 numbersToCall:=puzzleInput[0].Split(',');
 //For the rest of the input we need to create a bingo card for each block
 //The blocks are separated by a blank line.
 bingoCards:=TBingoCards.create;
 winningCards:=TBingoCards.create;
 currentCardData:=TStringArray.Create;
 setLength(currentCardData,0); //clear the array
 cardNumber:=0;
 for lineNumber:= 1 to pred(numberOfLines) do
   begin
   currentLine:=puzzleInput[lineNumber];
   if length(currentLine)> 0
     then fileUtilities.addToArray(currentCardData,currentLine) else
       begin
         //The current line is blank
         //if the currentCardData is not empty then create a bingo card from it
         if (length(currentCardData)> 0) then
           begin
           setLength(bingoCards,length(bingoCards)+1);
           bingoCards[pred(length(bingoCards))]:=TBingoCard.create(currentCardData,cardNumber,@CardNotifyWinHandler);
           setLength(currentCardData,0);
           cardNumber:=cardnumber+1;
           end;
       end;
   end;
 //now we have our cards set up. We can feed numbers into them
 //If a card has won it will fire the event handler above
 for callNumber := 0 to pred(length(numbersToCall)) do
   begin
   //Pass each number into each card. A winning card will fire the event handler
   //Unfortunately it won't stop this loop. However, we can look at the first result
   //This also is useful in part 2
   for cardNumber := 0 to pred(length(bingoCards)) do
     begin
     bingoCards[cardNumber].call(strToInt(numbersToCall[callNumber]));
     end;
   end;
 //now examine the winning cards array to see what the first and last objects are
 lbresults.items.add('First winning card: '+winningCards[0].id.ToString+' '+(winningCards[0].uncalled * winningCards[0].lastCalled).ToString);
 lbresults.items.add('Last answer '+(winningCards[pred(length(winningCards))].uncalled * winningCards[pred(length(winningCards))].lastCalled).ToString);

end;

procedure TForm1.day4part2;
begin
  lbresults.items.add('not done yet');
end;

procedure TForm1.day5part1;
begin
  lbresults.items.add('not done yet');
end;

procedure TForm1.day5part2;
begin

end;

procedure TForm1.day6part1;
begin
  lbresults.items.add('not done yet');
end;

procedure TForm1.day6part2;
begin
  lbresults.items.add('not done yet');
end;

procedure TForm1.day7part1;
begin
  lbresults.items.add('not done yet');
end;

procedure TForm1.day7part2;
begin
  lbresults.items.add('not done yet');
end;

procedure TForm1.day8part1;
begin
  lbresults.items.add('not done yet');
end;

procedure TForm1.day8part2;
begin
  lbresults.items.add('not done yet');
end;





end.

