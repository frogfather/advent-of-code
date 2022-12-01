unit aocPuzzle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,iAoc,fileUtilities, lazLogger,graphics,ExtCtrls;

type
  
  { TAocPuzzle }

  TAocPuzzle = class(TInterfacedObject, iAdvent)
  private
    fPaintBox:TPaintbox;
    fResults: TStringList;
    fData: string;
    function getResults: TStringList;
  public
    constructor Create(filename: string;paintbox:TPaintBox = nil);
    procedure runPartOne; virtual abstract;
    procedure runPartTwo; virtual abstract;
    procedure run(partOne: boolean = True);
    property results: TStringList read getResults;
    property paintbox: TPaintbox read fPaintbox;
  end;

implementation

{ TAocPuzzle }

constructor TAocPuzzle.Create(filename: string; paintbox:TPaintbox);
begin
  fPaintbox:=paintbox;
  try
  fData:=readStream(filename);
  except
  on E: Exception do
  DebugLn('Error reading puzzle file '+E.Message);
  end;
end;

procedure TAocPuzzle.run(partOne: boolean);
begin
  if partOne then runPartOne else runPartTwo;
end;

function TAocPuzzle.getResults: TStringList;
begin
  result:=fResults;
end;

end.
