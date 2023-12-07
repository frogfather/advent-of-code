unit hand;

{$mode ObjFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$modeswitch TypeHelpers}
interface

uses
  Classes, SysUtils,anysort,arrayUtils;
type

  THandType = (HIGH_CARD, ONE_PAIR, TWO_PAIR, THREE_OF_KIND, FULL_HOUSE, FOUR_OF_KIND, FIVE_OF_KIND);

  THand = Record
    hand: string;
    handType:THandType;
    bid: integer;
  end;

  THandArray = array of THand;

  { THandArrayHelper }
  THandArrayHelper = type helper for THandArray
  function size: integer;
  function push(element: THand):integer;
  end;
  procedure sortHandArray(var arr: array of THand; count:Integer; rank:TStringArray);
implementation

var
  cardRank:TStringArray;
{ THandArrayHelper }

function THandArrayHelper.size: integer;
begin
  result:=length(self);
end;

function THandArrayHelper.push(element: THand): integer;
begin
  insert(element,self,length(self));
  result:=self.size;
end;



  function CompareHandAsc(const d1,d2):integer;
var
  s1: THand absolute d1;
  s2: THand absolute d2;
  cardId:integer;
  comparison:integer;
begin
  comparison:= ord(s1.handType) - ord(s2.handType);
  if comparison = 0 then
    begin
    //Compare the individual cards
    for cardId:=0 to pred(s1.hand.Length) do
      begin
      comparison:=cardRank.indexOf(s1.hand.Substring(cardId,1)) - cardRank.indexOf(s2.hand.Substring(cardId,1));
      if comparison <> 0 then break;
      end;
    end;
    if comparison =0 then result:=comparison
    else result:= comparison div abs(comparison);
end;

procedure sortHandArray(var arr: array of THand; count: Integer; rank:TStringArray);
begin
    cardRank:=rank;
    anysort.AnySort(arr, Count, sizeof(THand), @CompareHandAsc)
end;
end.

