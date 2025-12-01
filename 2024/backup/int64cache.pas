unit int64Cache;

{$mode ObjFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
{$modeswitch TypeHelpers}
interface

uses
  Classes, SysUtils,arrayUtils;
Type

  TInt64CacheRecord = record
    input1:int64;
    input2:int64;
    output:int64;
  end;

  TInt64Cache = array of TInt64CacheRecord;

  { TInt64CacheHelper }
  TInt64CacheHelper = type helper for TInt64Cache
  function size:integer;
  function push(rec:TInt64CacheRecord):int64;
  function indexOf(in1,in2:int64):int64;
  function outputOf(in1,in2:int64):int64;
  procedure clear;
  end;

implementation

{ TInt64CacheHelper }

function TInt64CacheHelper.size: integer;
begin
  result:=length(self);
end;

function TInt64CacheHelper.push(rec: TInt64CacheRecord): int64;
begin
  if (self.indexOf(rec.input1,rec.input2) = -1) then
    begin
    setLength(self,self.size+1);
    self[self.size - 1]:=rec;
    end;
end;

function TInt64CacheHelper.indexOf(in1,in2:int64): int64;
begin
  result:=-1;
  if (self.size = 0) then exit;
  for result:=0 to pred(self.size) do
    if (self[result].input1 = in1)and(self[result].input2 = in2) then exit;
  result:=-1;
end;

function TInt64CacheHelper.outputOf(in1,in2:int64): int64;
begin
  if (self.indexOf(in1,in2) = -1) then result:=0
  else result:=self[indexOf(in1,in2)].output;
end;

procedure TInt64CacheHelper.clear;
begin
  setLength(self,0);
end;


end.

