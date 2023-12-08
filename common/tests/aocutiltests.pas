unit aocUtilTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,aocUtils;

type

  { TCommonUtilTests }

  TCommonUtilTests= class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure gcdLargestFirst;
    procedure gcdSmallestFirst;
    procedure gcdLargeNumbers;
    procedure lowestCommonMultiple;
    procedure lcmLargeNumbers;
  end;

implementation

procedure TCommonUtilTests.gcdLargestFirst;
var
  greatestCommon:Int64;
begin
  greatestCommon:=gcd(20,28);
  assertEquals(4,greatestCommon);
end;

procedure TCommonUtilTests.gcdSmallestFirst;
var
  greatestCommon:Int64;
begin
  greatestCommon:=gcd(28,20);
  assertEquals(4,greatestCommon);
end;

procedure TCommonUtilTests.gcdLargeNumbers;
var
  greatestCommon:Int64;
begin
  greatestCommon:=gcd(14935034899483,17621);
  assertEquals(17621,greatestCommon);
end;

procedure TCommonUtilTests.lowestCommonMultiple;
var
  lowestCommon:Int64;
begin
  lowestCommon:=lcm(15,20);
  assertEquals(60,lowestCommon);
end;

procedure TCommonUtilTests.lcmLargeNumbers;
var
  lowestCommon:Int64;
begin
  lowestCommon:=lcm(13939,17621);
  assertEquals(933913,lowestCommon);
end;

procedure TCommonUtilTests.SetUp;
begin

end;

procedure TCommonUtilTests.TearDown;
begin

end;

initialization

  RegisterTest(TCommonUtilTests);
end.

