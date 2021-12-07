unit lanternfish;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
type
  
  { TLanternFish }

  TLanternFish = class(TInterfacedObject)
    private
    fdays: integer;
    fCreateNewFish: TNotifyEvent;
    public
    constructor create(onCreateFishHandler: TNotifyEvent; initValue: integer=8);
    procedure addDay;
  end;

implementation

{ TLanternFish }

constructor TLanternFish.create(onCreateFishHandler: TNotifyEvent; initValue: integer);
begin
  fdays:=initValue;
  fCreateNewFish:=onCreateFishHandler;
end;

procedure TLanternFish.addDay;
begin
  fdays:=fdays - 1;
end;

end.

