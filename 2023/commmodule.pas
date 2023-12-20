unit commModule;

{$mode ObjFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$modeswitch TypeHelpers}
interface

uses
  Classes, SysUtils,arrayUtils;
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
    //need to keep record of inputs and where they came from
    fInputs:TStringBoolMap;
    fOutput:boolean;
    fName:string;
    fHold:boolean;
    fLows:integer;
    fHighs: integer;
    procedure addSubscription(eventHandler:TNotifyEvent);
    public
    constructor create(name_:string);
    procedure subscribeTo(to_:TCommModule);
    procedure action(sender:TObject);virtual abstract;
    property name: string read fName;
    property output: boolean read fOutput;
    property hold: boolean read fHold write fHold;
    property highs: integer read fHighs;
    property lows: integer read fLows;
  end;

  { TBroadcastCommsModule }

  TBroadcastCommsModule = class(TCommModule)
  private
  public
  constructor create(name_:string);
  procedure pressButton;
  procedure action(sender:TObject);override;
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

  { TOutputModule }

  TOutputModule = class(TCommModule)
  private
  public
  procedure action(sender:TObject);override;
  end;

  TCommModules = array of TCommModule;

  { TCommModulesHelper }

  TCommModulesHelper = type helper for TCommModules
  function size: integer;
  function push(element:TCommModule):integer;
  end;

implementation

{ TOutputModule }

procedure TOutputModule.action(sender: TObject);
begin
  //This needs to signal the calling process that the round has ended

end;

{ TCommModulesHelper }

function TCommModulesHelper.size: integer;
begin
  result:= length(self);
end;

function TCommModulesHelper.push(element: TCommModule): integer;
begin
  insert(element,self,length(self));
  result:=self.size;
end;

{ TBroadcastCommsModule }

constructor TBroadcastCommsModule.create(name_:string);
begin
  inherited create(name_);
  fInputs.AddOrSetData('btn',false);
end;

procedure TBroadcastCommsModule.pressButton;
begin
  fOutput:=false;
  action(self);
end;

procedure TBroadcastCommsModule.action(sender: TObject);
var
  subNo:integer;
  currentState:boolean;
begin
  if sender is TCommModule then with sender as TCommModule do
    begin
    self.hold:=true;
    for subNo:=0 to pred(self.fNotifySignals.size) do
      self.fNotifySignals[subNo](self);
    self.hold:=false;
    for subNo:=0 to pred(self.fNotifySignals.size) do
      self.fNotifySignals[subNo](self);
    if self.output then self.fHighs:=self.fHighs+self.fNotifySignals.size
      else self.fLows:=self.fLows+self.fNotifySignals.size;
    end;
end;

{ TConjunctionCommsModule }
procedure TConjunctionCommsModule.action(sender: TObject);
var
  subNo:integer;
  currentState:boolean;
  sendingModule:TCommModule;
  inputValue:boolean;
  inputId:integer;
  inputResult:boolean;
begin
  if sender is TCommModule then with sender as TCommModule do
    begin
    sendingModule:=sender as TCommModule;
    if sendingModule.hold then
      begin
      if self.fInputs.TryGetData(sendingModule.name,inputValue) then
        begin
         if (inputValue <> sendingModule.output) then
           begin
           self.fInputs.AddOrSetData(sendingModule.name,sendingModule.output);
           writeLn('Input '+name+' changed to '+sendingModule.output.ToString);
           end;
         inputResult:=true;
         if (self.fInputs.Count > 0) then
           begin
           for inputId:=0 to pred(self.fInputs.Count) do
             if (self.fInputs.Data[inputId] = false) then inputResult:=false;
           if inputResult then writeln('All inputs of '+self.name+' true');
           self.fOutput:=not InputResult;
           writeLn('output of '+self.name+' now '+self.output.toString);
           end else writeln(self.name+' has no inputs');
        end;
      end else
      begin
      writeLn(self.name+ ' update subscribers with value '+self.output.toString);
      self.hold:=true;

      if (self.fNotifySignals.size > 0) then
        for subNo:=0 to pred(self.fNotifySignals.size) do
          self.fNotifySignals[subNo](self);

      writeLn(self.name+ ' Subscribers updated notify subscribers without hold');
      self.hold:=false;

      if (self.fNotifySignals.size > 0) then
        for subNo:=0 to pred(self.fNotifySignals.size) do
          self.fNotifySignals[subNo](self);

      //update the counters
      if self.output then self.fHighs:=self.fHighs+1 else self.fLows:=self.fLows+1;
      end;
    end;
end;

{ TFlipFlopCommsModule }
procedure TFlipFlopCommsModule.action(sender: TObject);
var
  subNo,inputId:integer;
  inputValue:boolean;
  sendingModule:TCommModule;
begin
  if sender is TCommModule then with sender as TCommModule do
    begin
    sendingModule:=sender as TCommModule;
    if sendingModule.hold then
      begin
      if self.fInputs.TryGetData(sendingModule.name,inputValue) then
        begin
        if (inputValue <> sendingModule.output) then
          begin
          self.fInputs.AddOrSetData(sendingModule.name,sendingModule.output);
          writeLn('Input '+name+' changed to '+sendingModule.output.ToString);
          end;
         if sendingModule.output = false then
           begin
           self.fOutput:=not self.fOutput;
           writeLn('output of '+self.name+' changed to '+self.output.toString);
           end;
        end;
      end else
    if (sendingModule.output = false) then
      begin
      writeLn(self.name+ ' update subscribers with value '+self.output.toString);
      self.hold:=true;

      if (self.fNotifySignals.size > 0) then
        for subNo:=0 to pred(self.fNotifySignals.size) do
          self.fNotifySignals[subNo](self);
      writeLn(self.name+ ' Subscribers updated notify subscribers without hold');
      self.hold:=false;

      if (self.fNotifySignals.size > 0) then
        for subNo:=0 to pred(self.fNotifySignals.size) do
          self.fNotifySignals[subNo](self);
      if self.output then self.fHighs:=self.fHighs+1 else self.fLows:=self.fLows+1;
      end;
    end;
end;

{ TCommModule }

constructor TCommModule.create(name_: string);
begin
  fName:=name_;
  fNotifySignals:=TNotifyEvents.create;
  fInputs:=TStringBoolMap.Create;
  fLows:=0;
  fHighs:=0;
end;

procedure TCommModule.subscribeTo(to_: TCommModule);
begin
  to_.addSubscription(@self.action);
  fInputs.AddOrSetData(to_.name,false);
end;

procedure TCommModule.addSubscription(eventHandler: TNotifyEvent);
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

