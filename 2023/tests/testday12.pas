unit testday12;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,arrayUtils,day12;

type

  { TTestSubject }

  TTestSubject = class(TDayTwelve)
    public
    function updateGroupOffsetsMethod(var offsets:TIntArray; groups: TIntArray;dataLength:integer):boolean;
    function sequenceIsTooLongMethod(offsets,groups:TIntArray;offsetId,dataLength:integer):boolean;
  end;

  { TTestDay12 }

  TTestDay12 = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure sequenceIsTooLongTest;
    procedure UpdateLastGroupTest;
    procedure UpdateSecondLastGroupTest;
    procedure UpdateNotPossibleTest;
  end;

implementation
var
  day12T:TTestSubject;

{ TTestSubject }

function TTestSubject.updateGroupOffsetsMethod(var offsets: TIntArray;
  groups: TIntArray; dataLength: integer): boolean;
begin
  result:=day12T.updateGroupOffsets(offsets,groups,dataLength);
end;

function TTestSubject.sequenceIsTooLongMethod(offsets, groups: TIntArray; offsetId,
  dataLength: integer): boolean;
begin
  result:=day12T.sequenceIsTooLong(offsets, groups, offsetId,dataLength);
end;

{ TDay12T }

procedure TTestDay12.sequenceIsTooLongTest;
var
  offsets,groups:TIntArray;
  offsetId,datalength:integer;
  isOutOfRange:Boolean;
begin
  offsets:= TIntArray.create;
  groups:=TIntArray.create;
  groups.push(1);
  groups.push(6);
  groups.push(5);
  offsets.push(0);
  offsets.push(4);
  offsets.push(1);
  datalength:=19;
  offsetId:=2;
  isOutOfRange:= day12T.sequenceIsTooLongMethod(offsets,groups,offsetId,datalength);
  assertTrue(isOutOfRange);
  //Change third offset to 0
  offsets[2]:=0;
  isOutOfRange:= day12T.sequenceIsTooLongMethod(offsets,groups,offsetId,datalength);
  assertFalse(isOutOfRange);
end;

procedure TTestDay12.UpdateLastGroupTest;
var
  offsets,groups:TIntArray;
  updatePossible:boolean;
begin
  //Updates the first offset it can
  //????.######..#####. 1,6,5
  offsets:= TIntArray.create;
  groups:=TIntArray.create;
  groups.push(1);
  groups.push(6);
  groups.push(5);
  offsets.push(0);
  offsets.push(0);
  offsets.push(0);
  updatePossible:= day12T.updateGroupOffsetsMethod(offsets,groups,19);
  assertTrue(updatePossible);
  assertEquals(offsets[2],1); //The last group gets updated
end;

procedure TTestDay12.UpdateSecondLastGroupTest;
var
  offsets,groups:TIntArray;
  updatePossible:boolean;
begin
  //Updates the first offset it can
  //????.######..#####. 1,6,5
  offsets:= TIntArray.create;
  groups:=TIntArray.create;
  groups.push(1);
  groups.push(6);
  groups.push(5);
  offsets.push(0);
  offsets.push(0);
  offsets.push(5);
  updatePossible:= day12T.updateGroupOffsetsMethod(offsets,groups,19);
  assertTrue(updatePossible);
  assertEquals(offsets[1],1); //The second last group gets updated
  assertEquals(offsets[2],0);
end;

procedure TTestDay12.UpdateNotPossibleTest;
var
  offsets,groups:TIntArray;
  updatePossible:boolean;
begin
  //Updates the first offset it can
  //????.######..#####. 1,6,5
  offsets:= TIntArray.create;
  groups:=TIntArray.create;
  groups.push(1);
  groups.push(6);
  groups.push(5);
  offsets.push(5);
  offsets.push(1);
  offsets.push(1);
  updatePossible:= day12T.updateGroupOffsetsMethod(offsets,groups,19);
  assertFalse(updatePossible);
  assertEquals(offsets[1],0);
  assertEquals(offsets[2],0);
end;

procedure TTestDay12.SetUp;
begin
  day12T:=TTestSubject.create('/Users/john/Developer/advent-of-code/2023/input/puzzle_12_1_test.txt');
end;

procedure TTestDay12.TearDown;
begin

end;

initialization

  RegisterTest(TTestDay12);
end.

