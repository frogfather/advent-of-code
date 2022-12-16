unit day15;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, aocPuzzle, LazLogger, ExtCtrls, Graphics, arrayUtils;
type

  { TDayFifteen }

  TDayFifteen = class(TAocPuzzle)
  private
  public
    constructor Create(filename: string; paintbox_: TPaintbox = nil);
    procedure runPartOne; override;
    procedure runPartTwo; override;
  end;

implementation

end.

