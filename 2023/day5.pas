unit day5;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  aocPuzzle,LazLogger,ExtCtrls,Graphics,arrayUtils,rangeConvert;
type

  { TDayFive}
  TDayFive = class(TAocPuzzle)
  private
  procedure addToSection(var arr_:TRangeConverterArray;input_:string);
  function convert(var arr_:TRangeConverterArray; input: int64):int64;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayFive }

constructor TDayFive.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 5',paintbox_);
//parent loads the file as a string and converts to string array;
end;

procedure TDayFive.runPartOne;
var
  lineNo:integer;
  seedList:TStringArray;
  seedToSoil,soilToFertilizer,fertilizerToWater,waterToLight,lightToTemp,
    tempToHumidity,humidityToLocation:TRangeConverterArray;
  currentLine:string;
  section,item:integer;
  convertedItem:int64;
  lowestValue:int64;
  lowestValueFound:boolean;
begin
  results.Clear;
  seedToSoil:=TRangeConverterArray.create;
  soilToFertilizer:=TRangeConverterArray.create;
  fertilizerToWater:=TRangeConverterArray.create;
  waterTolight:=TRangeConverterArray.create;
  lightToTemp:=TRangeConverterArray.create;
  tempToHumidity:=TRangeConverterArray.create;
  humidityToLocation:=TRangeConverterArray.create;
  //Start with seeds
  //record start, range and offset
  section:=0;
  for lineNo:=0 to pred(puzzleInputLines.size) do
    begin
    currentLine:=puzzleInputLines[lineNo].Trim;
    //Some lines have description, some have data, some are blank
    if currentLine.Length = 0 then //blank line separates sections
      begin
      section:=section+1;
      continue;
      end;
    if (currentLine.indexOf(':') > -1)
      then currentLine:=currentLine.Split([':'],(TstringSplitOptions.ExcludeEmpty))[1].Trim;
    if currentLine.Length = 0 then continue;
    //Seeds
      case section of
      0: seedList:=currentLine.Split([':',' '],(TstringSplitOptions.ExcludeEmpty));
      1: addToSection(seedToSoil, currentLine);
      2: addToSection(soilToFertilizer,currentLine);
      3: addToSection(fertilizerToWater,currentLine);
      4: addToSection(waterTolight,currentLine);
      5: addToSection(lightToTemp,currentLine);
      6: addToSection(tempToHumidity,currentLine);
      7: addToSection(humidityToLocation,currentLine);
      end;
    end;
  //Now take our seeds and pass them into these converters
  lowestValue:=0;
  lowestValueFound:=false;
  for item:=0 to pred(seedList.size) do
    begin
    convertedItem :=convert(seedToSoil,seedList[item].ToInt64);
    convertedItem :=convert(soilToFertilizer,convertedItem);
    convertedItem :=convert(fertilizerToWater,convertedItem);
    convertedItem :=convert(waterTolight,convertedItem);
    convertedItem :=convert(lightToTemp,convertedItem);
    convertedItem :=convert(tempToHumidity,convertedItem);
    convertedItem :=convert(humidityToLocation,convertedItem);
    if (convertedItem < lowestValue) or not lowestValueFound then
      begin
      lowestValueFound:=true;
      lowestValue:=convertedItem;
      end;
    end;
  results.add('Lowest value is '+lowestValue.toString);
end;

procedure TDayFive.runPartTwo;
begin
  results.Clear;
end;

//----- methods for part one

//line structure is three parts: dest, source and range
procedure TDayFive.addToSection(var arr_: TRangeConverterArray; input_: string);
var
  parts:TStringArray;
  converter:TRangeConverter;
begin
  //start is second number
  //length is third number
  //offset is first minus second
  parts:=input_.Split(' ');
  converter.start:=parts[1].Trim.ToInt64;
  converter.length:=parts[2].Trim.ToInt64;
  converter.dest:=parts[0].Trim.ToInt64;
  arr_.push(converter);
end;

function TDayFive.convert(var arr_: TRangeConverterArray; input: int64
  ): int64;
var
  index:integer;
  s,l,d:int64;
begin
  result:=input;
  //Go through the entries in the converter looking for one
  //where the number is >= start and < start+length
  for index:=0 to pred(arr_.size) do
    begin
    s:=arr_[index].start;
    l:=arr_[index].length;
    d:=arr_[index].dest;
    if (input >= s) and (input < (s + l)) then
      begin
      result:=(input - s) + d;
      exit;
      end;
    end;
end;

end.


