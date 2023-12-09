unit cardData;

{$mode ObjFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$modeswitch TypeHelpers}
interface

uses
  Classes, SysUtils;
type

  TCardData = Record
    cardId: integer;
    cardCount: integer;
    matchCount:integer;
    winning: TStringArray;
    candidates: TStringArray;
  end;

  TCardDataArray = array of TCardData;

  { TCardDataArrayHelper }
  TCardDataArrayHelper = type helper for TCardDataArray
  function size: integer;
  function push(element: TCardData):integer;
  end;

operator = (a, b: TCardData): Boolean;

implementation

//not really needed but interested to see how to overload operators
operator=(a, b: TCardData): Boolean;
begin
  result:= a.cardId = b.cardId;
end;

{ TCardDataArrayHelper }

function TCardDataArrayHelper.size: integer;
begin
  result:=length(self);
end;

function TCardDataArrayHelper.push(element: TCardData): integer;
begin
  insert(element,self,length(self));
  result:=self.size;
end;

end.

