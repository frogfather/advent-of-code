unit day1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  TAocTest= class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHookUp;
  end;

implementation

procedure TAocTest.TestHookUp;
begin
  Fail('Write your own test');
end;

procedure TAocTest.SetUp;
begin

end;

procedure TAocTest.TearDown;
begin

end;

initialization

  RegisterTest(TAocTest);
end.

