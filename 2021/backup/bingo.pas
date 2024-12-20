unit bingo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,aocUtils,arrayUtils;
type
  BingoGrid = array of array of array of integer;

  { TBingoCard }
  TBingoCard = class(TInterfacedObject)
    private
    fId: Integer;
    fCard: BingoGrid;
    fLastCalled: integer;
    fUncalledSum:integer;
    fColCount:integer;
    fRowCount:integer;
    fWinning: boolean;
    fNotifyCardWin: TNotifyEvent;//an event handler that we can fire when a card wins
    function checkWin:boolean;
    function getRowMatches(row:integer):TBits;
    function getColMatches(col:integer):TBits;
    procedure calculateUncalled;
    public
    procedure call(number:integer);
    constructor create(input:TStringArray; cardId:integer; eventHandler:TNotifyEvent);
    property uncalled: integer read fUncalledSum;
    property lastCalled: integer read fLastCalled;
    property id: integer read fId;
  end;

  TbingoCards = array of TbingoCard;

  { TBingoGame }
  TBingoGame = class(TInterfacedObject)
  private
  fCards:TBingoCards;
  fWinningCards:TBingoCards;
  fNumbersToCall:TStringArray;
  procedure CardNotifyWinHandler(Sender: TObject);
  procedure createCards(puzzleInput:TStringArray);
  public
  constructor create(puzzleInput:TStringArray);
  procedure playGame;
  property winningCards:TBingoCards read fWinningCards;
  end;

implementation

{ TBingoGame }

procedure TBingoGame.CardNotifyWinHandler(Sender: TObject);
var
 winningCardIndex:integer;
 found:boolean;
begin
if Sender is TBingoCard then
  with Sender as TBingoCard do
  begin
  //add the winning card to the winningCards list if it isn't there
  found:=false;
  for winningCardIndex:=0 to pred(length(fWinningCards)) do
    begin
    if (fWinningCards[winningCardIndex] = sender as TBingoCard) then found:=true;
    end;
  if not found then
    begin
    setLength(fWinningCards, length(fWinningCards)+1);
    fWinningCards[pred(length(fWinningCards))]:=sender as TBingoCard;
    end;
  end;
end;

procedure TBingoGame.createCards(puzzleInput: TStringArray);
var
 puzzleDimensions:TPoint;
 currentCardData:TStringArray;
 cardNumber, lineNumber: Integer;
 currentLine: String;
begin
 currentCardData:=TStringArray.Create;
 setLength(currentCardData,0); //clear the array
 puzzleDimensions:=getDimensionsOfPuzzleInput(puzzleInput);
 cardNumber:=0;
 for lineNumber:= 1 to pred(puzzleDimensions.Y) do
   begin
   currentLine:=puzzleInput[lineNumber];
   if length(currentLine)> 0
     then addToArray(currentCardData,currentLine) else
       begin
         //The current line is blank
         //if the currentCardData is not empty then create a bingo card from it
         if (length(currentCardData)> 0) then
           begin
           setLength(fCards,length(fCards)+1);
           fCards[pred(length(fCards))]:=TBingoCard.create(currentCardData,cardNumber,@CardNotifyWinHandler);
           setLength(currentCardData,0);
           cardNumber:=cardnumber+1;
           end;
       end;
   end;
end;

constructor TBingoGame.create(puzzleInput: TStringArray);
begin
  fNumbersToCall:=puzzleInput[0].Split(',');
  fCards:=TBingoCards.create;
  fWinningCards:=TBingoCards.create;
  createCards(puzzleInput);
end;

procedure TBingoGame.playGame;
var
  callNumber, cardNumber: Integer;
begin
  for callNumber := 0 to pred(length(fNumbersToCall)) do
   begin
   //Pass each number into each card. A winning card will fire the event handler
   //but won't stop this loop. However, we can look at the first result
   //This also is useful in part 2
   for cardNumber := 0 to pred(length(fCards)) do
     begin
     fCards[cardNumber].call(strToInt(fNumbersToCall[callNumber]));
     end;
   end;
end;

{ TBingoCard }

function TBingoCard.checkWin:boolean;
var
  row,col:integer;
  matches:TBits;
  rowMatch,colMatch:boolean;
  rowIndex,colIndex:integer;
begin
//are there any complete rows or columns where the match flag is set for each element?
  for row:= 0 to pred(fRowCount) do
    begin
    matches:=getRowMatches(row);
    //this returns a row of bits which are true or false if the number is matched
    rowMatch:=true;
    for rowIndex:=0 to pred(matches.Size) do
    rowMatch:=rowMatch and matches[rowIndex];
    if rowMatch then //If a single row matches the card wins
      begin
        result:=true;
        exit;
      end;
    end;

  for col:= 0 to pred(fColCount) do
    begin
    matches:=getColMatches(col);
    //this returns a row of bits which are true or false if the number is matched
    colMatch:=true;
    for colIndex:=0 to pred(matches.Size) do
    colMatch:=colMatch and matches[colIndex];
    if colMatch then //If a single row matches the card wins
      begin
      result:=true;
      exit;
      end;
    end;
  result:=false;
end;

function TBingoCard.getRowMatches(row: integer): TBits;
var
  bits: TBits;
  col:integer;
begin
  bits:=TBits.Create(fColCount);
  for col := 0 to pred(fColCount) do
  bits[col]:= fCard[row][col][1] = 1;
  result:=bits;
end;

function TBingoCard.getColMatches(col: integer): TBits;
var
  bits: TBits;
  row:integer;
begin
  bits:=TBits.Create(fRowCount);
  for row := 0 to pred(fRowCount) do
  bits[row]:= fCard[row][col][1] = 1;
  result:=bits;
end;

procedure TBingoCard.calculateUncalled;
var
  row,col,uncalledCount:integer;
  uncalledValue:integer;
begin
  //In theory we could set fUncalledSum to the number of elements at the start
  //and decrement with each call, but that doesn't handle the same number being
  //called twice.
  uncalledCount:=0;
  for row:=0 to pred(fRowCount) do
    begin
    for col:=0 to pred(fColCount) do
      begin
      if fCard[row][col][1] = 0
        then
          begin
          uncalledValue:=fCard[row][col][0];
          uncalledCount:=uncalledCount + fCard[row][col][0]
          end;

      end;
    end;
  fUncalledSum:=uncalledCount;
end;

procedure TBingoCard.call(number: integer);
var
  col,row:integer;
begin
  //if the supplied number matches one of the numbers on our card
  //set the second element of the third dimension(?) to '1'
  //and call checkWin
  for row:=0 to pred(fRowCount) do
    begin
    for col:=0 to pred(fColCount) do
      if (fCard[row][col][0] = number) then fCard[row][col][1]:= 1;
    end;
  if not fWinning and checkWin then
    begin
    fLastCalled:=number;
    fWinning:=true;
    calculateUnCalled;
    fNotifyCardWin(self);
    end;
end;

constructor TBingoCard.create(input:TStringArray; cardId:integer; eventHandler: TNotifyEvent);
var
  currElements:TStringArray;
  currLine,currCol:integer;

begin
fRowCount:=length(input);
if (fRowCount > 0) then currElements:= input[0].Split(' ',TStringSplitOptions.ExcludeEmpty) else currElements:=TStringArray.Create;
fcolCount:=length(currElements);
fWinning:=false;
fCard:=BingoGrid.create;
fNotifyCardWin:=eventHandler;
fId:=cardId;//we need this to identify which card has won
//set the three dimensions of the array
setLength(fCard,fRowCount,fColCount,2);
for currLine:=0 to pred(fRowCount) do
  begin
  currElements:=input[currLine].Split(' ',TStringSplitOptions.ExcludeEmpty);
  for currCol:=0 to pred(fColCount) do
    begin
    fCard[currLine][currCol][0]:= currElements[currCol].ToInteger;
    fCard[currLine][currCol][1]:= 0;
    end;
  end;
end;

end.

