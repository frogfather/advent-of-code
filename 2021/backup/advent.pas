unit advent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics,
  Dialogs, StdCtrls, math,clipbrd,fgl,DateUtils,fpJSON,
  aocUtils,arrayUtils,
  bingo,ventMap,paintbox,octopus,origami,cavePassage,
  polymer,chiton,packet,trickshot,snailfish;

type
  TStringInt64Map = specialize TFPGMap<String,Int64>;

  { TmainForm }
  TmainForm = class(TForm)
    bExecute: TButton;
    cbSelect: TComboBox;
    lbResults: TListBox;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    clipboard:TClipboard;
    procedure bExecuteClick(Sender: TObject);
    procedure cbSelectSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbResultsSelectionChange(Sender: TObject; User: boolean);
  private
    procedure paintMap(map:T3DIntMap;basinSizes:TIntArray);
    procedure loadText(fileName:String);
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
    procedure day9part1;
    procedure day9part2;
    procedure day10part1;
    procedure day10part2;
    procedure day11part1;
    procedure day11part2;
    procedure day12part1;
    procedure day12part2;
    procedure day13part1;
    procedure day13part2;
    procedure day14part1;
    procedure day14part2;
    procedure day15part1;
    procedure day15part2;
    procedure day16part1;
    procedure day16part2;
    procedure day17part1;
    procedure day17part2;
    procedure day18part1;
    procedure day18part2;
    function identifySegmentValues(input:TStringArray):TStringMap;
    public

  end;

const dataDir: string = '/Users/cloudsoft/Code/advent-of-code/2021/input/';
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
   16: day9part1;
   17: day9part2;
   18: day10part1;
   19: day10part2;
   20: day11part1;
   21: day11part2;
   22: day12part1;
   23: day12part2;
   24: day13part1;
   25: day13part2;
   26: day14part1;
   27: day14part2;
   28: day15part1;
   29: day15part2;
   30: day16part1;
   31: day16part2;
   32: day17part1;
   33: day17part2;
   34: day18part1;
   35: day18part2;
  end;
 endTime:=now;
 lbResults.items.add('end '+formatDateTime('hh:mm:ss:zz',endTime));
 lbResults.Items.Add('Time: '+inttostr(millisecondsBetween(endTime,startTime))+' ms');
end;

procedure TmainForm.cbSelectSelect(Sender: TObject);
var
  descriptionFile:String;
  day,part:integer;
begin
  divMod(cbSelect.ItemIndex,2,day,part);
  descriptionFile:='puzzle_'+(1+day).ToString+'_'+(1+part).ToString+'.txt';
  loadText(descriptionFile);
end;

procedure TmainForm.FormCreate(Sender: TObject);
begin
  clipboard:=TClipboard.Create;
end;

procedure TmainForm.FormDestroy(Sender: TObject);
begin
  clipboard.Free;
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

procedure TmainForm.lbResultsSelectionChange(Sender: TObject; User: boolean);
var
  lineNo:integer;
  copiedLines:String;
begin
  if (lbResults.Items.Count = 0) or (lbResults.SelCount = 0) then exit;
  clipboard.Clear;
  copiedLines:='';
  for lineNo:=0 to pred(lbResults.Items.Count) do
    begin
    if (lbResults.Selected[lineNo])
       then copiedLines:= copiedLines + lbResults.Items[lineNo] + #$0A;
    end;
  clipboard.AsText:=copiedLines;
end;

procedure TmainForm.loadText(fileName: String);
begin
  memo1.Text:=getDescription(fileName);
end;

{ day 1 }
//https://adventofcode.com/2021/day/1
procedure TmainForm.day1part1;
var
  puzzleInput:TStringArray;
  puzzleDimensions:TPoint;
  index, increasingCount:integer;
begin
  puzzleInput:= getPuzzleInputAsStringArray('day_1_1.txt');
  puzzleDimensions:=getDimensionsOfPuzzleInput(puzzleInput);
  increasingCount:=0;
  for index := 1 to pred(puzzleDimensions.Y) do
    begin
    if (strToInt(puzzleInput[index]) > strToInt(puzzleInput[index - 1]))
      then increasingCount := increasingCount +1;
    end;
  lbResults.Items.add(inttostr(increasingCount)+' entries are larger than the previous');
end;
procedure TmainForm.day1part2;
var
  puzzleInput: TStringArray;
  puzzleDimensions:TPoint;
  index, firstSetIndex,secondSetIndex, increasingCount,avg1,avg2:integer;
begin
  puzzleInput:= getPuzzleInputAsStringArray('day_1_1.txt');
  puzzleDimensions:=getDimensionsOfPuzzleInput(puzzleInput);
  increasingCount:=0;
  //we start at 3 because we need to compare the first three entries to
  //the second three entries (i.e. comparing 0,1,2 to 1,2,3)
  for index:=3 to pred(puzzleDimensions.Y) do
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
//https://adventofcode.com/2021/day/2
procedure TmainForm.day2part1;
var
  puzzleInput: TStringArray;
  puzzleDimensions:TPoint;
  elements:TStringArray;
  command: string;
  index,value: integer;
  horPos,depth:integer;
begin
  puzzleInput:=getPuzzleInputAsStringArray('day_2_1.txt');
  puzzleDimensions:=getDimensionsOfPuzzleInput(puzzleInput);
  horPos:=0;
  depth:=0;
  for index:=0 to pred(puzzleDimensions.Y) do
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
 var
  puzzleInput: TStringArray;
  puzzleDimensions:TPoint;
  elements:TStringArray;
  command: string;
  index,value: integer;
  horPos,depth,aim:integer;
begin
  puzzleInput:=getPuzzleInputAsStringArray('day_2_1.txt');
  puzzleDimensions:=getDimensionsOfPuzzleInput(puzzleInput);
  horPos:=0;
  depth:=0;
  aim:=0;
  for index:=0 to pred(puzzleDimensions.Y) do
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
procedure TmainForm.day3part2;
var
  oxygen,co2: TStringArray;
  sOxygen,sCo2: string;
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

procedure TmainForm.day4part1;
var
 bingoGame: TBingoGame;
 winningCards:TBingoCards;
begin
 bingoGame:=TBingoGame.create(getPuzzleInputAsStringArray('day_4_1.txt',false));
 bingoGame.playGame;
 winningCards:= bingoGame.winningCards;
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

{ day 6 }
procedure TmainForm.day6part1;
var
 fishInput,fishValues:TStringArray;
 fishes,newFishes:TIntArray;
 fishNo,newFishNo,dayNo:integer;
begin
 //Retrieve the lines from the file. In this case there's only one
 //TODO use int array instead
 fishInput:= getPuzzleInputAsStringArray('day_6_1.txt');
 if (length(fishInput) = 1) then
   begin
   //split on comma to get an array of the values
   fishValues:=fishInput[0].Split(',',TStringSplitOptions.ExcludeEmpty);
   //easier to work with integers
   fishes:=toIntArray(fishValues);
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
       addToArray(fishes,newFishes[newFishNo]);
       end;
     end;
   lbResults.items.add('number of fish '+length(fishes).ToString);
   end;

end;
procedure TmainForm.day6part2;
type
  TInt64List = specialize TFPGList<int64>;
var
 crabInput,fishValues:TStringArray;
 daysList: TInt64List;
 fishNo,i,index:integer;
 total,spawningFish:int64;
begin
 crabInput:= getPuzzleInputAsStringArray('day_6_1.txt');
 if (length(crabInput) = 1) then
   begin
   //split on comma to get an array of the values
   fishValues:=crabInput[0].Split(',',TStringSplitOptions.ExcludeEmpty);
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

{ day 7}
procedure TmainForm.day7part1;
var
 puzzleInput:TStringArray;
 puzzleDimensions:TPoint;
 crabPositions:TIntArray;
 maxValue,totalValue,averageValue,index:integer;
 startPoint,endPoint,fuelAtThisPoint,leastFuel:integer;

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
  puzzleDimensions:=getDimensionsOfPuzzleInput(puzzleInput);
  if puzzleDimensions.Y = 1 then
    begin
    crabPositions:=toIntArray(puzzleInput[0].Split(','));
    maxValue:=getMaxValue(crabPositions);
    totalValue:=0;
    for index := 0 to pred(length(crabPositions)) do
      begin
      totalValue:=totalValue + crabPositions[index];
      end;
    averageValue:=totalValue div maxValue;

    startPoint:=averageValue - (length(crabPositions) div 5);
    endPoint:=averageValue + (length(crabPositions) div 5);
    leastFuel:=calculateFuel(crabPositions,startPoint);//set initial value
    for index:=startPoint to endPoint do
      begin
      fuelAtThisPoint:=calculateFuel(crabPositions,index);
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
  fishPositions:=toIntArray(puzzleInput[0].Split(','));
  //we need to find out the minimum number of moves
  //that will get all the fish to the same position
  //Let's work out a distribution of where the fish are
  totalValue:=0;
  for index := 0 to pred(length(fishPositions)) do
    begin
    totalValue:=totalValue + fishPositions[index];
    end;
  averageValue:=totalValue div length(fishPositions);
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

{ day 8 }
procedure TmainForm.day8part1;
var
requiredOutputs:TIntArray;
puzzleDimensions:TPoint;
puzzleInput,outputSeq:TStringArray;
lineNo,elementNo:Integer;
matchingOutputs:integer;
begin
 requiredOutputs:=TIntArray.create(2,3,4,7);
 puzzleInput:=getPuzzleInputAsStringArray('day_8_1.txt');
 puzzleDimensions:=getDimensionsOfPuzzleInput(puzzleInput);
 matchingOutputs:=0;
 for lineNo:=0 to pred(puzzleDimensions.Y) do
   begin
   //split on pipe
   outputSeq:=puzzleInput[lineNo].Split('|')[1].Split(' ');
   //now find out how many entries have length 2,3,4,7
   for elementNo:=0 to pred(length(outputSeq)) do
     begin
     if (arrPos(requiredOutputs,length(outputSeq[elementNo]))> -1)
       then matchingOutputs := matchingOutputs+1;
     end;
   end;
 lbResults.items.add('Number of entries that are 2,3,4 or 7: '+matchingOutputs.ToString);
end;

//A method to determine what number each sequence of letters corresponds to
function TmainForm.identifySegmentValues(input: TStringArray): TStringMap;
var
  output:TStringMap;
  index:integer;
  code1,code3,code4,code6:string;
  inputItem:string;
begin
  //We can find 1,7,4 and 8 by their length
  //3 is the only one of the 5 segment values that uses the segments in 1
  //6 doesn't have the segments in 1
  //0 is the other 6 segment value
  //all the segments in 5 are also in 6
  output:=TStringMap.create;
  //Start by sorting the entries and putting them into the map
  for index:=0 to pred(length(input)) do
    begin
    inputItem:=input[index];
    sort(inputItem,length(inputItem));
    output.Add(inputItem,'?');
    end;
  //First pass: identify items 1,7,4,8
  for index:=0 to pred(output.Count)do
    begin
    case length(output.Keys[index]) of
     2:
       begin
       code1:=output.Keys[index];
       output.AddOrSetData(code1,'1');
       end;
     3: output.AddOrSetData(output.Keys[index],'7');
     4:
       begin
       code4:=output.Keys[index];
       output.AddOrSetData(code4,'4');
       end;
     7: output.AddOrSetData(output.Keys[index],'8');
    end;
  end;

  //Second pass: identify 3
  for index:=0 to pred(output.Count)do
    begin
    if (length(output.Keys[index])=5)
      and (containsCharacters(output.Keys[index],code1))
    then
      begin
      code3:=output.Keys[index];
      output.AddOrSetData(code3,'3');
      break;
      end;
    end;

  //identify the six segment entries now
  for index:=0 to pred(output.Count)do
    begin
    if (length(output.Keys[index])=6) then
      begin
      //9 contains all the segments in 3 and all the segments in 4
      if containsCharacters(output.Keys[index],code3)
      and containsCharacters(output.Keys[index],code4)
        then
        output.AddOrSetData(output.Keys[index],'9')
      else
      //6 doesn't contain the segments in 1
      if not containsCharacters(output.Keys[index],code1) then
        begin
        code6:=output.Keys[index];
        output.AddOrSetData(code6,'6');
        end else output.AddOrSetData(output.Keys[index],'0');
      end;
    end;
  //finally identify 5 and 2. All the characters in 5 are found in 6
  for index:=0 to pred(output.Count)do
    begin
    if (length(output.Keys[index])=5) then
      begin
      if containsCharacters(code6,output.Keys[index])
      then output.AddOrSetData(output.Keys[index],'5')
      else if (output.Keys[index] <> code3)
        then output.AddOrSetData(output.Keys[index],'2')
      end;
    end;
  result:=output;
end;

procedure TmainForm.day8part2;
var
  segmentMap: TStringMap;
  puzzleInput,inputSeq,outputSeq:TStringArray;
  puzzleDimensions:TPoint;
  lineNo,outputIndex:integer;
  sOutput,sLineValue: string;
  lineSum,totalSum:integer;
begin
  puzzleInput:=getPuzzleInputAsStringArray('day_8_1.txt');
  puzzleDimensions:=getDimensionsOfPuzzleInput(puzzleInput);
  totalSum:=0;
  for lineNo:=0 to pred(puzzleDimensions.Y) do
   begin
   sLineValue:='';
   inputSeq:=puzzleInput[lineNo].Split('|',TStringSplitOptions.ExcludeEmpty)[0]
     .Split(' ',TStringSplitOptions.ExcludeEmpty);
   outputSeq:=puzzleInput[lineNo].Split('|',TStringSplitOptions.ExcludeEmpty)[1]
     .Split(' ',TStringSplitOptions.ExcludeEmpty);
   //TODO Sort both input and output
   segmentMap:=identifySegmentValues(inputSeq);
   for outputIndex:=0 to pred(length(outputSeq)) do
     begin
     sOutput:=outputSeq[outputIndex];
     sort(sOutput,length(sOutput));
     sLineValue:=sLineValue+segmentMap.KeyData[sOutput];
     end;
   lineSum:=sLineValue.ToInteger;
   totalSum:=totalSum+lineSum;
   end;
 lbResults.items.add('Sum of all entries '+totalSum.ToString);
end;

{ day 9 }
procedure TmainForm.day9part1;
var
  puzzleInput:TStringArray;
  puzzleDimensions:TPoint;
  lineNo,index:integer;
  currentLine,previousLine,nextLine:String;
  centre,north,south,east,west:integer;
  totalRiskLevel:integer;
begin
  puzzleInput:=getPuzzleInputAsStringArray('day_9_1.txt');
  puzzleDimensions:=getDimensionsOfPuzzleInput(puzzleInput);
  //find points that are lower than all those around them
  //start at the second line and stop at the second last line
  totalRiskLevel:=0;
  for lineNo:=0 to pred(puzzleDimensions.Y) do
    begin
    currentLine:=puzzleInput[lineNo];
    if (lineNo > 0) then previousLine:=puzzleInput[lineNo-1];
    if (lineNo < pred(length(puzzleInput))) then nextLine:=puzzleInput[lineNo+1];
    for index:=0 to pred(length(currentLine)) do
      begin
      centre:=currentLine.Substring(index,1).ToInteger;
      if (lineNo > 0)
         then North:=previousLine.Substring(index,1).ToInteger
         else North:=10; //makes logic easier
      if (lineNo < pred(length(puzzleInput)))
         then South:=nextLine.Substring(index,1).ToInteger
         else South:=10;
      if (index > 0)
         then West:=currentLine.Substring(index-1,1).ToInteger
         else West:=10;
      if (index < pred(length(currentLine)))
         then East:=currentLine.Substring(index+1,1).ToInteger
         else East:=10;
      //is centre lower than all the other values?
      if (centre < North)and(centre < South)and(centre < West)and(centre < East)then
        begin
        totalRiskLevel:=totalRiskLevel + centre + 1;
        end;
      end;
    end;
  lbResults.items.add('Total risk level '+totalRiskLevel.ToString);
end;

procedure TmainForm.day9part2;
var
  puzzleInput:TStringArray;
  puzzleDimensions:TPoint;
  basinMap:T3DIntMap;
  mapMaxX,mapMaxY:integer;
  currentLine:string;
  y,elementNo,basinNo,currentElement,currentBasin,itemsMarked:integer;
  basinSizes:TIntArray;
  //keep traversing the map until we hit 9s
  procedure basinCrawl(var map: T3DIntMap;currentLine, currentLinePosition, currentBasin:integer);
  var
    mapWidth,mapHeight:integer;
    canGoDown,canGoUp,canGoLeft,canGoRight:boolean;
    basinId,pointHeight:integer;
    begin
    mapWidth:=length(map);
    mapHeight:=length(map[0]);
    basinId:=map[currentLinePosition][currentLine][1];
    pointHeight:=map[currentLinePosition][currentLine][0];
    if pointHeight = 9 then exit;
    if (basinId = -1) then
       begin
       map[currentLinePosition][currentLine][1]:=currentBasin;
       itemsMarked:=itemsMarked+1;
       end;
    canGoDown:=(currentLine < pred(mapHeight))
       and (map[currentLinePosition][currentLine+1][0]<> 9)
       and (map[currentLinePosition][currentLine+1][1]= -1);
    canGoLeft:=(currentLinePosition > 0)
       and (map[currentLinePosition - 1][currentLine][0] <> 9)
       and (map[currentLinePosition - 1][currentLine][1] = -1);
    canGoRight:=(currentLinePosition < pred(mapWidth))
       and (map[currentLinePosition + 1][currentLine][0] <> 9)
       and (map[currentLinePosition + 1][currentLine][1] = -1);
    canGoUp:=(currentLine > 0)
       and (map[currentLinePosition][currentLine-1][0]<> 9)
       and (map[currentLinePosition][currentLine-1][1]= -1);
    if canGoDown then basinCrawl(map, currentLine + 1,currentLinePosition, currentBasin);
    if canGoLeft then basinCrawl(map, currentLine, currentLinePosition - 1, currentBasin);
    if canGoRight then basinCrawl(map, currentLine, currentLinePosition + 1, currentBasin);
    if canGoUp then basinCrawl(map, currentLine - 1,currentLinePosition, currentBasin);
    end;

begin
  //load the puzzle input into the array
  basinNo:=0;
  puzzleInput:=getPuzzleInputAsStringArray('day_9_1.txt');
  puzzleDimensions:=getDimensionsOfPuzzleInput(puzzleInput);
  if (puzzleDimensions.Y = 0) then exit;
  basinMap:=T3DIntMap.create;
  mapMaxX:=length(puzzleInput[0]);
  mapMaxY:=length(puzzleInput);
  setLength(basinMap,mapMaxX,mapMaxY,2);
  //populate the map
  for y:=0 to pred(puzzleDimensions.Y) do
    begin
    currentLine:=puzzleInput[y];
    for elementNo:=0 to pred(currentline.length) do
      begin
      currentElement:=currentLine.Substring(elementNo,1).ToInteger;
      basinMap[elementNo][y][0]:=currentElement;
      basinMap[elementNo][y][1]:=-1;
      end;
    end;

  //now crawl about it
  itemsMarked:=0;
  for y:=0 to pred(mapMaxY) do
    begin
    for elementNo:=0 to pred(mapMaxX) do
      begin
      basinCrawl(basinMap, y,elementNo,basinNo);
      if itemsMarked > 0 then basinNo:=basinNo+1;
      itemsMarked:=0;
      end;
    end;

  //Find the basin sizes
  basinSizes:=TIntArray.create;
  setLength(basinSizes,basinNo);
  for y:=0 to pred(mapMaxY) do
    begin
    for elementNo:=0 to pred(mapMaxX) do
      begin
      currentBasin:=basinMap[elementNo][y][1];
      if (currentBasin > -1)then
        begin
        basinSizes[currentBasin]:=basinSizes[currentBasin]+1;
        end;
      end;
    end;
  //sort the array
  paintMap(basinMap,basinSizes);
  sort(basinSizes,length(basinSizes),false);
  //and get the first three entries
  lbResults.items.add('risk is '+(basinSizes[0]*basinSizes[1]*basinSizes[2]).ToString);
end;

procedure TmainForm.paintMap(map: T3DIntMap; basinSizes: TIntArray);
var
basinColors: TColours;
basinindex,basinSize:integer;
basinColor:TColor;
red,green,blue:Byte;

  function generateNormalisedRandomValue(normalizeTo,offset:integer):byte;
    begin
    result:= random(normalizeTo)+offset mod normalizeTo;
    end;

begin
basinColors:=TColours.create;
setLength(basinColors,length(basinsizes));
for basinIndex:=0 to pred(length(basinsizes))do
  begin
  basinSize:=basinSizes[basinindex];
  red:=generateNormalisedRandomValue(255,basinSize);
  green:=generateNormalisedRandomValue(255,basinSize);
  blue:=generateNormalisedRandomValue(255,basinSize);
  //create some random colours
  basinColor:=rgbToColor(red,green,blue);
  basinColors[basinIndex]:=basinColor;
  end;
paintboxForm:=TPaintboxForm.Create(nil);
paintboxForm.colours:=basinColors;
paintboxForm.map:=map;
paintboxForm.showModal;
freeAndNil(paintboxForm);
end;

{ day 10 }
procedure TmainForm.day10part1;
const
 openingTags: array [0..3] of string = ('(','[','{','<');
 closingTags: array [0..3] of string = (')',']','}','>');
 tagScore: array[0..3] of integer = (3,57,1197,25137);
 autoCompleteScore: array[0..3] of integer = (1,2,3,4);
var
  puzzleInput:TStringArray;
  puzzleDimensions:TPoint;
  lineNo:integer;
  incomplete:boolean;
  tagResult:string;
  illegalIndex,totalScore:integer;
  completionScores:TInt64Array;
  tagValue:integer;
  completionScore,completionResult :int64;
  allScoresCount, midpointIndex:integer;


  function calculateCompletionScore(tags:string):int64;
  var
    completionTag:string;
    tagScore,index,completionTagIndex: integer;
    runningTotal:int64;
    begin
    runningTotal:=0;
    for index:=0 to pred(length(tags)) do
      begin
      completionTag:=tags.Substring(index,1);
      completionTagIndex:=arrPos(closingTags,completionTag);
      tagScore:=autocompleteScore[completionTagIndex];
      runningTotal:=(runningTotal*5)+tagScore;
      end;
    result:=runningTotal;
    end;

  function missingOrIllegalTags(input:string; out incomplete: boolean):string;
  var
    expectedClosingTags: TStringArray;
    index:integer;
    currentTag,expectedClosingTag:string;
    openingTagIndex:integer;
    remainingTagIndex:integer;
    begin
    expectedClosingTags:=TStringArray.create;
    for index := 0 to pred(puzzleDimensions.Y) do
      begin
      currentTag:=input.Substring(index,1);
      openingTagIndex := arrPos(openingTags,currentTag);
      //if an opening tag, add the matching closing tag to the array
      if (openingTagIndex > -1)
        then addToArray(expectedClosingTags,closingTags[openingTagIndex])
      else
        begin
        if (length(expectedClosingTags)=0)
           then expectedClosingTag:=''
        else
          begin
          expectedClosingTag:=deleteFromArray(expectedClosingTags,pred(length(expectedClosingTags)));
          if expectedClosingTag <> currentTag then
            begin
            result:=currentTag;
            incomplete:=false;
            exit;
            end;
          end;
        end;
      end;

    incomplete:= length(expectedClosingTags) > 0;
    if incomplete then
      begin
      result:='';
      for remainingTagIndex:= pred(length(expectedClosingTags)) downto 0 do
        result:=result+expectedClosingTags[remainingTagIndex];
      end else result:='';
    end;

begin
  totalScore:=0;
  completionScores:=TInt64Array.create;
  puzzleInput:=getPuzzleInputAsStringArray('day_10_1.txt');
  puzzleDimensions:=getDimensionsOfPuzzleInput(puzzleInput);
  for lineNo:= 0 to pred(puzzleDimensions.Y) do
    begin
    tagResult:=missingOrIllegalTags(puzzleInput[lineNo],incomplete);
    if incomplete then
      begin
      completionScore:=calculateCompletionScore(tagResult);
      addToArray(completionScores,completionScore);
      end
    else if (tagResult <> '') then
      begin
      illegalIndex:=arrPos(closingTags,tagResult);
      tagValue:=tagScore[illegalIndex];
      totalScore:=totalScore + tagValue;
      end;
    end;

  //sort the completion results and choose the middle one
  sort(completionScores,length(completionScores));

  for lineNo:=0 to pred(length(completionScores)) do
    begin
    lbresults.items.add(lineNo.ToString+' '+completionScores[lineNo].ToString);
    end;


  allScoresCount:= length(completionScores);
  midpointIndex:=(allScoresCount div 2);//array zero indexed!
  completionResult:=completionScores[midpointIndex];
  lbResults.Items.Add('Total score of illegal tags '+totalScore.ToString);
  lbResults.items.add('value of middle completion score '+completionResult.ToString);
end;

procedure TmainForm.day10part2;
begin
 day10part1;
end;

{ day 11 }
procedure TmainForm.day11part1;
const maxSteps = 100;
var
  octopusHandler:TOctopusHandler;
  steps:integer;
begin
  octopusHandler:=TOctopusHandler
    .create(getPuzzleInputAsStringArray('day_11_1.txt'));
  for steps:=0 to maxSteps -1 do
    begin
    octopusHandler.runOctopuses;
    octopusHandler.resetOctopuses;
    end;
  lbResults.items.add('total flash count: '+octopusHandler.flashCount.ToString);
end;

procedure TmainForm.day11part2;
var
  octopusHandler:TOctopusHandler;
  steps:integer;
  done:boolean;
begin
  octopusHandler:= TOctopusHandler
    .create(getPuzzleInputAsStringArray('day_11_1.txt'));
  //we want to run the octopuses until we reach a point where they have
  //all flashed. We can find this by checking the hasFlashed property
  steps:=0;
  repeat
  octopusHandler.runOctopuses;
  steps:=steps+1;
  done:=octopusHandler.allFlashed;
  octopusHandler.resetOctopuses;
  until done;
lbresults.items.add('All octopuses flash on step '+steps.ToString);
end;

{ day 12 }
procedure TmainForm.day12part1;
var
  puzzleInput:TStringArray;
  caveNavigator:TCaveNavigator;
begin
  puzzleInput:=getPuzzleInputAsStringArray('day_12_1.txt');
  caveNavigator:= TCaveNavigator.create(puzzleInput);
  caveNavigator.explore('start');
  lbResults.items.add('Paths: '+caveNavigator.paths.ToString);
end;

procedure TmainForm.day12part2;
var
  puzzleInput:TStringArray;
  caveNavigator:TCaveNavigator;

begin
  puzzleInput:=getPuzzleInputAsStringArray('day_12_1.txt');
  caveNavigator:= TCaveNavigator.create(puzzleInput);
  caveNavigator.explore('start',2,1);
  lbResults.items.add('Paths: '+caveNavigator.paths.ToString);
end;

{ day 13 }
procedure TmainForm.day13part1;
var
  origami:TOrigami;
  coordinates:TStringArray;
  instructions:TStringList;
  index:integer;
begin
  coordinates:=getPuzzleInputAsStringArray('day_13_1.txt',false); //leave blank lines
  //remove last blank line
  setLength(coordinates,length(coordinates)-1);
  instructions:=TStringlist.Create;
  //move the instructions to the stringlist and delete from array;
  index:=pred(length(coordinates));
  while length(coordinates[index]) > 0 do
    begin
    instructions.Insert(0,coordinates[index]);
    setLength(coordinates,length(coordinates)-1);
    index:=index-1;
    end;
  //remove blank line after coordinates
  setLength(coordinates,length(coordinates)-1);
  origami:=TOrigami.create(coordinates);
  //constructor sets up the map and populates it.
  //Now we need to call the fold method and find the number of dots
  origami.fold(instructions[0]);
  lbResults.items.add('Dots after fold '+origami.dotCount.ToString);
end;

procedure TmainForm.day13part2;
var
  origami:TOrigami;
  coordinates:TStringArray;
  instructions,mapOutput:TStringList;
  index,instruction:integer;
begin
  coordinates:=getPuzzleInputAsStringArray('day_13_1.txt',false); //leave blank lines
  //remove last blank line
  setLength(coordinates,length(coordinates)-1);
  instructions:=TStringlist.Create;
  //move the instructions to the stringlist and delete from array;
  index:=pred(length(coordinates));
  while length(coordinates[index]) > 0 do
    begin
    instructions.Insert(0,coordinates[index]);
    setLength(coordinates,length(coordinates)-1);
    index:=index-1;
    end;
  //remove blank line after coordinates
  setLength(coordinates,length(coordinates)-1);
  origami:=TOrigami.create(coordinates);
  //constructor sets up the map and populates it.
  for instruction:=0 to pred(instructions.Count) do
    begin
    origami.fold(instructions[instruction]);
    end;
  //this time we need to print out the dots
  mapOutput:=origami.getMapOutput;
  lbResults.Items:=mapOutput;
end;

{ day 14 }
procedure TmainForm.day14part1;
//From solution by William Feng https://www.youtube.com/watch?v=2IVmEcZb4Mw
const maxSteps: integer = 10;
var
  puzzleInput:TStringArray;
  polymer:TPolymer;
begin
  puzzleInput:=getPuzzleInputAsStringArray('day_14_1.txt');
  polymer:=TPolymer.Create(puzzleInput);
  polymer.run(maxSteps);
  lbResults.items.add('Answer is: '+polymer.answer.ToString);
end;

procedure TmainForm.day14part2;
//From solution by William Feng https://www.youtube.com/watch?v=2IVmEcZb4Mw
const maxSteps: integer = 40;
var
  puzzleInput:TStringArray;
  polymer:TPolymer;
begin
  puzzleInput:=getPuzzleInputAsStringArray('day_14_1.txt');
  polymer:=TPolymer.Create(puzzleInput);
  polymer.run(maxSteps);
  lbResults.items.add('Answer is: '+polymer.answer.ToString)
end;

procedure TmainForm.day15part1;
var
  puzzleInput:TStringArray;
  routeFinder:TRouteFinder;
  startPoint,endPoint:TPoint;
begin
  puzzleInput:=getPuzzleInputAsStringArray('day_15_1.txt');
  routeFinder:=TRouteFinder.create(puzzleInput);
  startPoint.X:=0;
  startPoint.Y:=0;
  endPoint.X:= pred(routeFinder.mapDimensions.X);
  endPoint.Y:= pred(routeFinder.mapDimensions.Y);
  routeFinder.findShortestPath(startPoint, endPoint);
  lbResults.Items.add('least risky route has risk '+routeFinder.shortest.ToString)
end;

procedure TmainForm.day15part2;
var
  puzzleInput:TStringArray;
  routeFinder:TRouteFinder;
  startPoint,endPoint:TPoint;
  lineNo,copyCount:integer;
  originalLine,newLine:String;
  originalPuzzleLength:integer;

  function copyAndIncrement(input:String; copyNo:integer):string;
  var
    index:integer;
    copyLine:string;
    element:integer;
    begin
    copyLine:='';
    for index:=0 to pred(length(input)) do
      begin
      element:=input.Substring(index,1).ToInteger;
      element:=element+copyNo;
      if element > 9 then element:=element - 9;
      copyLine:=copyLine+element.ToString;
      end;
    result:=copyLine;
    end;

begin
  puzzleInput:=getPuzzleInputAsStringArray('day_15_1.txt');
  //we need to clone this map 4 times in each direction
  //first extend each string incrementing each time
  for lineNo:=0 to pred(length(puzzleInput)) do
    begin
    originalLine:=puzzleInput[lineNo];
    newLine:=originalLine;
    for copyCount:=1 to 4 do
    newLine:=newLine+copyAndIncrement(originalLine,copyCount);
    puzzleInput[lineNo]:=newLine;
    end;

  //now for lines 0 to pred(originalPuzzleLength) we need to do the same
  originalPuzzleLength:=length(puzzleInput);
  for copyCount:= 1 to 4 do
    begin
    for lineNo:=0 to pred(originalPuzzleLength) do
      begin
      newLine:=copyAndIncrement(puzzleInput[lineNo],copyCount);
      addToArray(puzzleInput,newLine);
      end;
    end;

  routeFinder:=TRouteFinder.create(puzzleInput);
  startPoint.X:=0;
  startPoint.Y:=0;
  endPoint.X:= pred(routeFinder.mapDimensions.X);
  endPoint.Y:= pred(routeFinder.mapDimensions.Y);

  routeFinder.findShortestPath(startPoint, endPoint);
  lbResults.Items.add('least risky route has risk '+routeFinder.shortest.ToString);
end;

{ day 16 }
procedure TmainForm.day16part1;
 var
   puzzleInput:String;
   packetFactory:TPacketFactory;
begin
   puzzleInput:=getPuzzleInputAsString('day_16_1.txt');
   packetFactory:=TPacketFactory.create(puzzleInput);
   lbResults.items.add('version total '+packetFactory.getVersionTotal.ToString);

end;

procedure TmainForm.day16part2;
 var
    puzzleInput:String;
    packetFactory:TPacketFactory;
begin
    puzzleInput:=getPuzzleInputAsString('day_16_1.txt');
    packetFactory:=TPacketFactory.create(puzzleInput);
    lbResults.items.add('Packet sum :'+ packetFactory.packetSum.ToString);
end;

{ day 17 }
procedure TmainForm.day17part1;
var
   puzzleInput: String;
   trickshot:Ttrickshot;
   maxHeightVelocity:TPoint;
begin
   puzzleInput:= getPuzzleInputAsString('day_17_1.txt');
   //puzzleInput:='target area: x=20..30, y=-10..-5';
   trickshot:=TTrickshot.create(puzzleInput);
   maxHeightVelocity:=trickshot.calculateVelocityForMaxHeight;
   lbResults.Items.Add('velocity for max height: X: '+maxHeightVelocity.X.ToString+' Y: '+maxHeightVelocity.Y.ToString);
   lbResults.items.add('max height is '+trickShot.maxHeight.ToString);
   lbResults.items.add('number of hits '+trickShot.allHits.ToString);

end;

procedure TmainForm.day17part2;
begin
  day17part1;
end;

{ day 18 }
procedure TmainForm.day18part1;
var
  puzzleInput:TStringArray;
  snailFish:TSnailfish;
begin
  puzzleInput:=getPuzzleInputAsStringArray('day_18_1.txt');
  snailFish:= TSnailfish.create(puzzleInput);
  snailFish.doHomework;
  lbResults.Items.add('Sum '+snailfish.sum.ToString);
end;

procedure TmainForm.day18part2;
var
  puzzleInput:TStringArray;
  homework:THomework;
begin
  //tree version of part 1
  puzzleInput:=getPuzzleInputAsStringArray('day_18_test_1.txt');
  homework:=THomework.create(puzzleInput);
  homework.doHomework;
  lbResults.Items:= homework.log;
end;


end.

