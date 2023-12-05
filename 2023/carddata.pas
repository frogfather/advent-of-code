unit cardData;

{$mode ObjFPC}{$H+}

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
  operator = (a, b: TCardData): Boolean;

implementation

//not really needed but interested to see how to overload operators
operator=(a, b: TCardData): Boolean;
begin
  result:= a.cardId = b.cardId;
end;

end.

