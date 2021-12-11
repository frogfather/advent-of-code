unit octopus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  
  { TOctopus }

  TOctopus = class(TInterfacedObject)
    private
    fPosition:TPoint;
    fEnergy: integer;
    fNotifyFlash:TNotifyEvent;//Fired when the octopus flashes
    fHasFlashed:boolean;
    public
    constructor create(initialEnergy:integer;position:TPoint; eventHandler:TNotifyEvent);
    procedure addEnergy(units:integer);
    procedure resetFlash;
    property energy: integer read fEnergy write fEnergy;
    property position:TPoint read fPosition write fPosition;
    property hasFlashed:boolean read fHasFlashed;
  end;

implementation

{ TOctopus }

constructor TOctopus.create(initialEnergy: integer; position: TPoint; eventHandler: TNotifyEvent);
begin
  fEnergy:=initialEnergy;
  fPosition:=position;
  fNotifyFlash:=eventHandler;
end;

procedure TOctopus.addEnergy(units: integer);
begin
  if not hasFlashed then energy:=energy+units;
  if energy > 9 then
    begin
    energy:=energy - 10;
    fNotifyFlash(self);
    fhasFlashed:=true;
    end;
end;

procedure TOctopus.resetFlash;
begin
  fhasFlashed:=false;
end;

end.

