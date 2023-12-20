unit commModule;

{$mode ObjFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$modeswitch TypeHelpers}
interface

uses
  Classes, SysUtils;
type
  TNotifyEvents = array of TNotifyEvent;

  { TNotifyEventsHelper }

  TNotifyEventsHelper = type helper for TNotifyEvents
  function size: integer;
  function push(element:TNotifyEvent):integer;
  function shift:TNotifyEvent;
  end;

  { TCommModule }

  TCommModule = class(TInterfacedObject)
    private
    fNotifySignals:TNotifyEvents;
    fLow:Boolean;
    fName:string;
    public
    constructor create(name_:string);
    procedure action(sender:TObject);virtual abstract;
    procedure subscribe(eventHandler:TNotifyEvent);
    property isLow:boolean read fLow write fLow;
    property name: string read fName;
  end;

  { TFlipFlopCommsModule }

  TFlipFlopCommsModule = class(TCommModule)
  private
  public
  procedure action(sender:TObject);override;
  end;

  { TConjunctionCommsModule }

  TConjunctionCommsModule = class(TCommModule)
  private
  public
  procedure action(sender:TObject);override;
  end;

implementation

{ TConjunctionCommsModule }

procedure TConjunctionCommsModule.action(sender: TObject);
begin
  if sender is TCommModule then with sender as TCommModule do
  writeLn('Message received at '+self.name+' from '+name);
end;

{ TFlipFlopCommsModule }

procedure TFlipFlopCommsModule.action(sender: TObject);
begin
  if sender is TCommModule then with sender as TCommModule do
  writeLn('Message received at '+self.name+' from '+name);
end;

{ TCommModule }

constructor TCommModule.create(name_: string);
begin
  fName:=name_;
  fNotifySignals:=TNotifyEvents.create;
end;

procedure TCommModule.subscribe(eventHandler: TNotifyEvent);
begin
  fNotifySignals.push(eventHandler);
end;

{ TNotifyEventsHelper }

function TNotifyEventsHelper.size: integer;
begin
  result:=length(self);
end;

function TNotifyEventsHelper.push(element: TNotifyEvent): integer;
begin
  insert(element,self,length(self));
  result:=self.size;
end;

function TNotifyEventsHelper.shift: TNotifyEvent;
var
  index:integer;
begin
  if (self.size > 0) then
    begin
    result:=self[0];
    if (self.size > 1) then for index:= 0 to pred(self.size) do
      self[index]:=self[index+1];
    setLength(self,pred(self.size));
    end;
end;

end.

