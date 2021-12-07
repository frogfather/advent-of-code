unit lanternfish;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
type
  
  { TLanternFish }

  TLanternFish = class(TInterfacedObject)
    private
    fdaysUntilNewFish: integer;
    fCreateNewFish: TNotifyEvent;
    public
    constructor create(onCreateFishHandler: TNotifyEvent; initValue: integer=8);
    procedure addDay;
  end;

implementation

{ TLanternFish }

constructor TLanternFish.create(onCreateFishHandler: TNotifyEvent; initValue: integer);
begin
  fdaysUntilNewFish:=initValue;
  fCreateNewFish:=onCreateFishHandler;
end;

procedure TLanternFish.addDay;
begin
  fdaysUntilNewFish:=fdaysUntilNewFish - 1;
  if (fDaysUntilNewFish = 0) then
    begin
      fDaysUntilNewFish:=6;
      fCreateNewFish(self);
    end;
end;

end.

