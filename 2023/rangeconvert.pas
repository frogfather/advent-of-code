unit rangeConvert;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;
//For puzzle day 5 adjusting a source number over a specified range
type
  TRangeConverter = record
    start: int64;
    length: int64;
    dest: int64;
  end;

implementation

end.

