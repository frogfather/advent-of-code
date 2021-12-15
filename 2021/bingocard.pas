unit bingoCard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,fileUtilities,arrayUtils;
type
  ABingoCard = array of array of array of string;
  { TBingoCard }

  TBingoCard = class(TInterfacedObject)
    private
    fId: Integer;
    fCard: ABingoCard;
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

implementation

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
  bits[col]:= fCard[row][col][1] = '1';
  result:=bits;
end;

function TBingoCard.getColMatches(col: integer): TBits;
var
  bits: TBits;
  row:integer;
begin
  bits:=TBits.Create(fRowCount);
  for row := 0 to pred(fRowCount) do
  bits[row]:= fCard[row][col][1] = '1';
  result:=bits;
end;

procedure TBingoCard.calculateUncalled;
var
  row,col,uncalledCount:integer;
  uncalledValue:string;
begin
  //In theory we could set fUncalledSum to the number of elements at the start
  //and decrement with each call, but that doesn't handle the same number being
  //called twice.
  uncalledCount:=0;
  for row:=0 to pred(fRowCount) do
    begin
    for col:=0 to pred(fColCount) do
      begin
      if fCard[row][col][1]='0'
        then
          begin
          uncalledValue:=fCard[row][col][0];
          uncalledCount:=uncalledCount+strToInt(fCard[row][col][0])
          end;

      end;
    end;
  fUncalledSum:=uncalledCount;
end;

procedure TBingoCard.call(number: integer);
var
  sNumber:string;
  col,row:integer;
begin
  //if the supplied number matches one of the numbers on our card
  //set the second element of the third dimension(?) to '1'
  //and call checkWin
  sNumber:=number.ToString;
  for row:=0 to pred(fRowCount) do
    begin
    for col:=0 to pred(fColCount) do
      if (fCard[row][col][0] = sNumber) then fCard[row][col][1]:='1';
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
fCard:=ABingoCard.create;
fNotifyCardWin:=eventHandler;
fId:=cardId;//we need this to identify which card has won
//set the three dimensions of the array
setLength(fCard,fRowCount,fColCount,2);
for currLine:=0 to pred(fRowCount) do
  begin
  currElements:=input[currLine].Split(' ',TStringSplitOptions.ExcludeEmpty);
  for currCol:=0 to pred(fColCount) do
    begin
    fCard[currLine][currCol][0]:=currElements[currCol];
    fCard[currLine][currCol][1]:='0';
    end;
  end;
end;

end.

