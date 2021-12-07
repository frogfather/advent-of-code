unit advent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics,
  Dialogs, StdCtrls, fileUtilities, math, bingoCard,
  ventMap,fgl,DateUtils,aocUtils;

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
  bBits:= aocUtils.calculateCommonestValue(puzzleInput);
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
var
  oxygen,co2: TStringArray;
  sOxygen,sCo2: string;

  function getUniqueEntry(input: TStringArray;reverse:boolean=false):String;
  //This nested method is only visible from the procedure day3part2
  //It deletes any entries from the input that don't match the pattern of
  //1s and 0s
  //The method calculateCommonestValues could also be nested as it's only
  //used by day3part2
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

{ day 4 }
procedure TmainForm.CardNotifyWinHandler(Sender: TObject);
//The constructor for the bingo cards gets passed a pointer to this method
//as their fNotifyCardWin property. If a card calculates that
//it has won it calls the event handler(this method) passing itself
//as the sender parameter
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

procedure TmainForm.day4part1;
var
 puzzleInput,numbersToCall:TStringArray;
 currentLine:string;
 currentCardData:TStringArray; //The block of numbers to passed to the card constructor
 bingoCards: TBingoCards;
 cardNumber:integer;
 lineNumber,numberOfLines,callNumber:integer;
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

procedure TmainForm.day4part2;
begin
  lbresults.items.add('included in day 4 part 1');
end;

{ day 5 }
procedure TmainForm.day5part1;
var
 ventMap:TVentMap;
begin
 ventMap:=TVentMap.create(getPuzzleInputAsStringArray('day_5_1.txt'));
 ventMap.calculateVents;
 lbResults.items.add(ventMap.getOverlapCount.ToString+' overlaps');
end;

procedure TmainForm.day5part2;
var
 ventMap:TVentMap;
begin
 ventMap:=TVentMap.create(getPuzzleInputAsStringArray('day_5_1.txt'));
 ventMap.calculateVents(false);
 lbResults.items.add(ventMap.getOverlapCount.ToString+' overlaps');
end;

procedure TmainForm.day6part1;
var
 fishInput,fishValues:TStringArray;
 fishes,newFishes:TIntArray;
 fishNo,newFishNo,dayNo:integer;
begin
 //Retrieve the lines from the file. In this case there's only one
 fishInput:= getPuzzleInputAsStringArray('day_6_1.txt');
 if (length(fishInput) = 1) then
   begin
   //split on comma to get an array of the values
   fishValues:=fileUtilities.removeBlankLinesFromStringArray(fishInput[0].Split(','));
   //easier to work with integers
   fishes:=fileUtilities.toIntArray(fishValues);
   newFishes:=TIntArray.create;
   for dayNo:=0 to 79 do
     begin
      setLength(newFishes,0);
      for fishNo:=0 to pred(length(fishes)) do
        begin
        fishes[fishNo]:=fishes[fishNo]-1;
        if (fishes[fishNo] < 0) then
          begin
          //create a new one
          fishes[fishNo]:=6;
          setLength(newFishes,length(newFishes)+1);
          newFishes[pred(length(newFishes))]:=8;
          end;
        end;
       //now add the new fishes to the existing ones
     for newFishNo := 0 to pred(length(newFishes)) do
       begin
       fileutilities.addToArray(fishes,newFishes[newFishNo]);
       end;
     end;
   lbResults.items.add('number of fish '+length(fishes).ToString);
   end;

end;

//The approach above won't work for the second part because it'll take
//far too long. The solution below was based on a mixture of this explanation
//https://zonito.medium.com/lantern-fish-day-6-advent-of-code-2021-python-solution-4444387a8380
//and this video https://www.youtube.com/watch?v=yJjpXJm7x0o
//I used a generic list so that the entries could be int64
procedure TmainForm.day6part2;
type
  TInt64List = specialize TFPGList<int64>;
var
 fishInput,fishValues:TStringArray;
 daysList: TInt64List;
 fishNo,i,index:integer;
 total,spawningFish:int64;
begin
 fishInput:= getPuzzleInputAsStringArray('day_6_1.txt');
 if (length(fishInput) = 1) then
   begin
   //split on comma to get an array of the values
   fishValues:=fileUtilities.removeBlankLinesFromStringArray(fishInput[0].Split(','));
   //create the map and add entries with keys 0-8 and values 0
   daysList:=TInt64List.Create;
   for i:=0 to 8 do daysList.Add(0);
   //Set the initial values
   for fishNo:=0 to pred(length(fishValues)) do
     begin
     daysList.Items[fishValues[fishNo].ToInteger]
       :=daysList.Items[fishValues[fishNo].ToInteger] +1;
     end;
   //our array now holds the distribution of days to spawn
   //e.g. position 3 holds the number of fish with 3 days to spawn
   //Now for each day we want to move the values in the array down one
   //So the value in position 5 moves to position 4.
   //For values in position 0 we want to add that number to position 6 (7 days to spawn)
   //and add the same number to position 8 (the babies take longer to spawn)
   for i:=0 to 255 do
     begin
     //fish that are in position 0 are ready to spawn
     spawningFish:=daysList.Items[0];
     //move all the other entries down one
     for index:=0 to daysList.Count - 2 do
       begin
       daysList.Items[index]:=daysList.items[index+1];
       end;
     //now add the spawningFish to both 6 and 8
     daysList.Items[6]:=daysList[6]+spawningFish;
     daysList.Items[8]:=spawningFish;
     end;
   total:=0;
   for index:=0 to pred(daysList.Count) do
   total:=total + daysList.Items[index];
   lbResults.Items.add('Total fish '+total.ToString);
   end;
end;


{Day 7}


procedure TmainForm.day7part1;
var
 puzzleInput:TStringArray;
 fishPositions:TIntArray;
 maxValue,totalValue,averageValue,index:integer;
 startPoint,endPoint,fuelAtThisPoint,leastFuel:integer;

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

 function calculateFuel(input:TIntArray;position:integer):integer;
 var
 index:integer;
 output:integer;
 begin
 //sum the difference between each fish and the desired position
 output:=0;
 for index:=0 to pred(length(input)) do
   begin
   output:=output + (abs(input[index] - position));
   end;
 result:=output;
 end;

begin
 puzzleInput:=getPuzzleInputAsStringArray('day_7_part_1.txt');
 if length(puzzleInput)= 1 then
   begin
   fishPositions:=fileUtilities.toIntArray(puzzleInput[0].Split(','));
   //we need to find out the minimum number of moves
   //that will get all the fish to the same position
   //Let's work out a distribution of where the fish are
   maxValue:=getMaxValue(fishPositions);
   totalValue:=0;
   for index := 0 to pred(length(fishPositions)) do
     begin
     totalValue:=totalValue + fishPositions[index];
     end;
   averageValue:=totalValue div maxValue;
   //Does this help?
   //try values between (say) average - 20% and average + 20%
   startPoint:=averageValue - (length(fishPositions) div 5);
   endPoint:=averageValue + (length(fishPositions) div 5);
   leastFuel:=calculateFuel(fishPositions,startPoint);//set initial value
   for index:=startPoint to endPoint do
     begin
     fuelAtThisPoint:=calculateFuel(fishPositions,index);
     if fuelAtThisPoint < leastFuel then leastFuel:=fuelAtThisPoint;
     end;
   lbResults.items.add('Min fuel in this range: '+leastFuel.ToString);
   end;
end;

procedure TmainForm.day7part2;
var
 puzzleInput:TStringArray;
 fishPositions:TIntArray;
 totalValue,averageValue,index:integer;
 startPoint,endPoint,fuelAtThisPoint,leastFuel:integer;

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

 function calculateFuel(input:TIntArray;position:integer):integer;
 var
 index,diff:integer;
 output:integer;
 begin
 //This time, we need to calculate the fuel differently
 //moving 1 costs 1 fuel
 //moving 2 costs 2 + 1 = 3
 //moving 3 costs 3 + 2 + 1 = 6
 //moving n costs n + n-1 + n-2 ... 1
 output:=0;
 for index:=0 to pred(length(input)) do
   begin
   diff:=abs(input[index] - position);
   while diff > 0 do
     begin
     output:=output + diff;
     diff:=pred(diff);
     end;
   end;
 result:=output;
 end;

begin
 puzzleInput:=getPuzzleInputAsStringArray('day_7_part_1.txt');
 if length(puzzleInput)= 1 then
   begin
   fishPositions:=fileUtilities.toIntArray(puzzleInput[0].Split(','));
   //we need to find out the minimum number of moves
   //that will get all the fish to the same position
   //Let's work out a distribution of where the fish are
   totalValue:=0;
   for index := 0 to pred(length(fishPositions)) do
     begin
     totalValue:=totalValue + fishPositions[index];
     end;
   averageValue:=totalValue div length(fishPositions);
   //Does this help?
   //try values between (say) average - 20% and average + 20%
   startPoint:=averageValue - (length(fishPositions) div 5);
   endPoint:=averageValue + (length(fishPositions) div 5);
   leastFuel:=calculateFuel(fishPositions,startPoint);//set initial value
   for index:=startPoint to endPoint do
     begin
     fuelAtThisPoint:=calculateFuel(fishPositions,index);
     if fuelAtThisPoint < leastFuel then leastFuel:=fuelAtThisPoint;
     end;
   lbResults.items.add('Min fuel in this range: '+leastFuel.ToString);
   end;
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

