unit iAoc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  IAdvent = interface
  ['{0343dbf2-7b1c-4cda-9a55-a8975b50b9bb}']
  procedure run(partOne:boolean=true);
  function getResults:TStringlist;
  procedure setResults(results_:TStringlist);
  property results:TStringlist read getResults;
  end;

implementation

end.

