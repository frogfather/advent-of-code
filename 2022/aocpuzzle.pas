unit aocPuzzle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,iAoc,fileUtilities, lazLogger;

type
  
  { TAocPuzzle }

  TAocPuzzle = class(TInterfacedObject, iAdvent)
  private
    fResults: TStringList;
    fData: string;
    function getResults: TStringList;
  public
    constructor Create(filename: string);
    procedure runPartOne; virtual abstract;
    procedure runPartTwo; virtual abstract;
    procedure run(partOne: boolean = True);
    property results: TStringList read getResults;
  end;

implementation

{ TAocPuzzle }

function TAocPuzzle.getResults: TStringList;
begin
  result:=fResults;
end;

constructor TAocPuzzle.Create(filename: string);
begin
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

end.
