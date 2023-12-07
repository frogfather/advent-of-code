unit day7;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils,hand,TypInfo;
type

  { TDaySeven}
  TDaySeven = class(TAocPuzzle)
  private
  function getHandType(hand_:string;jokers:boolean=false):THandType;
  function applyJokers(grouping:TStringIntMap):TStringIntMap;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation
var
  handArray: THandArray;
{ TDaySeven }

constructor TDaySeven.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 7',paintbox_);
//parent loads the file as a string and converts to string array;
handArray:=THandArray.create;
end;

procedure TDaySeven.runPartOne;
const cardRank: TStringArray = ('2','3','4','5','6','7','8','9','T','J','Q','K','A');
var
  lineNo:integer;
  parts:TStringArray;
  cards:String;
  bid:integer;
  newHand:THand;
  sum:integer;
begin
  results.Clear;
  setLength(handArray,0);
  //Add each hand along with its type and bid
  //each line is hand and bid
  for lineNo:=0 to pred(puzzleInputLines.size) do
  begin
  parts:=puzzleInputLines[lineNo].Split([' '],(TstringSplitOptions.ExcludeEmpty));
  cards:=parts[0].Trim;
  bid:=parts[1].Trim.ToInteger;
  newHand.hand:=cards;
  newHand.handType:=getHandType(cards);
  newHand.bid:=bid;
  handArray.push(newHand);
  end;
  sortHandArray(handArray,handArray.size,cardRank);

  sum:=0;
  for lineNo:= 0 to pred(handArray.size) do
    sum:=sum + ((lineNo + 1) * handArray[lineNo].bid);
  results.add('The sum is '+sum.ToString);
end;

procedure TDaySeven.runPartTwo;
const cardRank: TStringArray = ('J','2','3','4','5','6','7','8','9','T','Q','K','A');
var
  lineNo:integer;
  parts:TStringArray;
  cards:String;
  bid:integer;
  newHand:THand;
  sum:integer;
begin
  results.Clear;
  setLength(handArray,0);
  for lineNo:=0 to pred(puzzleInputLines.size) do
  begin
  //Generate an array of hands which contain the cards, type and bid
  parts:=puzzleInputLines[lineNo].Split([' '],(TstringSplitOptions.ExcludeEmpty));
  cards:=parts[0].Trim;
  bid:=parts[1].Trim.ToInteger;
  newHand.hand:=cards;
  newHand.handType:=getHandType(cards,true);
  newHand.bid:=bid;
  handArray.push(newHand);
  end;
  //This sort method, in unit 'hand' does most of the work in this puzzle
  sortHandArray(handArray,handArray.size, cardRank);

  sum:=0;
  for lineNo:= 0 to pred(handArray.size) do
    sum:=sum + ((lineNo + 1) * handArray[lineNo].bid);
  results.add('The sum is '+sum.ToString);
end;

function TDaySeven.getHandType(hand_: string;jokers:boolean): THandType;
var
  cardGroupings:TStringIntMap;
  cardId,cardCount:integer;
  card:string;
begin
  cardGroupings:=TStringIntMap.Create;
  for cardId:=0 to pred(hand_.Length) do
    begin
    card:=hand_.Substring(cardId,1);
    if not cardGroupings.TryGetData(card,cardCount) then cardCount:=0;
    cardCount:=cardCount+1;
    cardGroupings.AddOrSetData(card,cardCount);
    end;
  //In part 2 we modify this grouping to improve the strength of the hand
  if jokers then cardGroupings:= applyJokers(cardGroupings);

  //If we have five entries then it's high card
  case cardGroupings.Count of
    5:result:=THandType.HIGH_CARD;
    4:result:=THandType.ONE_PAIR;
    3:
      begin
      //May be one group of 3 and two different cards
      //or two groups of 2
      if (cardGroupings.IndexOfData(3) > -1) then result:=THandType.THREE_OF_KIND
      else result:=THandType.TWO_PAIR;
      end;
    2:
      begin
      if (cardGroupings.IndexOfData(4) > -1) then result:= THandType.FOUR_OF_KIND
      else result:=THandType.FULL_HOUSE;
      end;
    1: result:=THandType.FIVE_OF_KIND;
  end;
end;

function TDaySeven.applyJokers(grouping:TStringIntMap):TStringIntMap;
var
  jokerCount:integer;
  mostCards:string;
  index,mostCardsCount:integer;
begin
  result:=grouping;
  //We have the groupings. Let's see how many jokers (J) we have
  if result.TryGetData('J',jokerCount) then
    begin
    //now find the largest remaining group
    mostCards:='';
    mostCardsCount:=0;
    for index:=0 to pred(result.Count) do
      if result.Data[index] > mostCardsCount then
        begin
        mostCardsCount:=result.Data[index];
        mostCards:=result.Keys[index];
        end;
    //and add the number of jokers to that
    mostCardsCount:=mostCardsCount + jokerCount;
    result.AddOrSetData(mostCards,mostCardsCount);
    end;
end;

end.


