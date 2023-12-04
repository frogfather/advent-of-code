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

implementation

end.

