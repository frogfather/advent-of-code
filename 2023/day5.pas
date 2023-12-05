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
  procedure addToRange(var arr_:TRangeArray; input_: string);
  procedure setUpConverters;
  procedure resetArrays;
  procedure convert(var input_:TRangeArray;var output_:TRangeArray;var converter:TRangeConverterArray);
  function convert(var arr_:TRangeConverterArray; input: int64):int64;
  public
  constructor create(filename:string; paintbox_:TPaintbox = nil);
  procedure runPartOne; override;
  procedure runPartTwo; override;
  end;

implementation

{ TDayFive }

var
  seedList:TStringArray;

  seedRange,soilRange,fertilizerRange,waterRange,lightRange,
  tempRange,humidityRange,locationRange:TRangeArray;

  seedToSoil,soilToFertilizer,fertilizerToWater,waterToLight,lightToTemp,
  tempToHumidity,humidityToLocation:TRangeConverterArray;

constructor TDayFive.create(filename:string;paintbox_:TPaintbox);
begin
inherited create(filename,'Day 5',paintbox_);
//parent loads the file as a string and converts to string array;
  seedRange:= TRangeArray.create;
  soilRange:= TRangeArray.create;
  fertilizerRange:= TRangeArray.create;
  waterRange:= TRangeArray.create;
  lightRange:= TRangeArray.create;
  tempRange:= TRangeArray.create;
  humidityRange:= TRangeArray.create;
  locationRange:= TRangeArray.create;
  seedToSoil:=TRangeConverterArray.create;
  soilToFertilizer:=TRangeConverterArray.create;
  fertilizerToWater:=TRangeConverterArray.create;
  waterTolight:=TRangeConverterArray.create;
  lightToTemp:=TRangeConverterArray.create;
  tempToHumidity:=TRangeConverterArray.create;
  humidityToLocation:=TRangeConverterArray.create;
end;

procedure TDayFive.runPartOne;
var
  item:integer;
  convertedItem:int64;
  lowestValue:int64;
  lowestValueFound:boolean;
begin
  results.Clear;
  resetArrays;
  setupConverters;
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
var
  index:integer;
begin
  results.Clear;
  resetArrays;
  setupConverters;

  sortRangeArray(seedRange,seedRange.size);
  convert(seedRange,soilRange,seedToSoil);
  sortRangeArray(soilRange,soilRange.size);
  convert(soilRange,fertilizerRange,soilToFertilizer);
  sortRangeArray(fertilizerRange,fertilizerRange.size);
  convert(fertilizerRange,waterRange,fertilizerToWater);
  sortRangeArray(waterRange,waterRange.size);
  convert(waterRange,lightRange,waterToLight);
  sortRangeArray(lightRange,lightRange.size);
  convert(lightRange,tempRange,lightToTemp);
  sortRangeArray(tempRange,tempRange.size);
  convert(tempRange,humidityRange,tempToHumidity);
  sortRangeArray(humidityRange,humidityRange.size);
  convert(humidityRange,locationRange,humidityToLocation);

  sortRangeArray(locationRange,locationRange.size);
  for index:=0 to pred(locationRange.size) do
    results.Add(locationRange[index].rangeStart.ToString+' -> '+locationRange[index].rangeEnd.ToString);

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

procedure TDayFive.addToRange(var arr_: TRangeArray; input_: string);
var
  parts:TStringArray;
  range:TRange;
  index:integer;
begin
  parts:=input_.Split(' ',(TstringSplitOptions.ExcludeEmpty));
  for index:= 0 to pred(parts.size) do
    begin
    if (index mod 2 = 0) then range.rangeStart:=parts[index].ToInt64
    else begin
    range.rangeEnd:=(range.rangeStart + parts[index].ToInt64 - 1);
    arr_.push(range);
    end;

    end;
end;

procedure TDayFive.setUpConverters;
var
  lineNo:integer;
  currentLine:string;
  section:integer;
begin
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
      0: //Set up seedlist for part one and seedRange for part two
        begin
        seedList:=currentLine.Split([':',' '],(TstringSplitOptions.ExcludeEmpty));
        addToRange(seedRange,currentLine);
        end;
      1: addToSection(seedToSoil, currentLine);
      2: addToSection(soilToFertilizer,currentLine);
      3: addToSection(fertilizerToWater,currentLine);
      4: addToSection(waterTolight,currentLine);
      5: addToSection(lightToTemp,currentLine);
      6: addToSection(tempToHumidity,currentLine);
      7: addToSection(humidityToLocation,currentLine);
      end;
    end;
  sort(seedToSoil,seedToSoil.size);
  sort(soilToFertilizer,soilToFertilizer.size);
  sort(fertilizerToWater,fertilizerToWater.size);
  sort(waterTolight,waterTolight.size);
  sort(lightToTemp,lightToTemp.size);
  sort(tempToHumidity,tempToHumidity.size);
  sort(HumidityToLocation,humidityToLocation.size);

end;

procedure TDayFive.resetArrays;
begin
  setLength(seedRange,0);
  setLength(soilRange,0);
  setLength(fertilizerRange,0);
  setLength(waterRange,0);
  setLength(lightRange,0);
  setLength(tempRange,0);
  setLength(humidityRange,0);
  setLength(locationRange,0);
  setLength(seedToSoil,0);
  setLength(soilToFertilizer,0);
  setLength(fertilizerToWater,0);
  setLength(waterTolight,0);
  setLength(lightToTemp,0);
  setLength(tempToHumidity,0);
  setLength(humidityToLocation,0);
end;

procedure TDayFive.convert(var input_: TRangeArray; var output_: TRangeArray;
  var converter: TRangeConverterArray);
var
  rangeIndex,converterIndex:integer;
  rangeStart,rangeEnd:int64;
  newRange:TRange;
  converterStart,converterEnd,offset:int64;
  rangeProcessed: boolean;
begin
  //Iterate through the entries in input_
  //check overlaps with each line of the converter
  //add the transformed entries to output
  for rangeIndex:=0 to pred(input_.size) do
    begin
    rangeStart:=input_[rangeIndex].rangeStart;
    rangeEnd:=input_[rangeIndex].rangeEnd;
    converterIndex:= 0;
    rangeProcessed:=false;
    while not rangeProcessed do
      begin
      converterStart:=converter[converterIndex].start;
      converterEnd:= (converter[converterIndex].start + converter[converterIndex].length - 1);
      offset:=converter[converterIndex].dest - converter[converterIndex].start;

      //1) is the input start less than the converter start?
      if (rangeStart < converterStart) then
        begin
        //add unconverted range to output
        newRange.rangeStart:=rangeStart;
        if (rangeEnd < converterStart) then
          begin
          newRange.rangeEnd:=rangeEnd;
          rangeProcessed:=true;
          end
        else
          begin
          newRange.rangeEnd:=converterStart - 1;
          rangeStart:=converterStart; //Stay on this converter
          end;
        output_.push(newRange);
        end else
      //2) is the rangeStart in range of this converter
      if (rangeStart >= converterStart) and (rangeStart <= converterEnd) then
        begin
        //add converted range from rangeStart to either rangeEnd or converterEnd
        newRange.rangeStart:=rangeStart + offset;
        if (rangeEnd > converterEnd) then
          begin
          newRange.rangeEnd:=converterEnd + offset;
          rangeStart:=converterEnd+1;
          //move to the next converter
          if (converterIndex < pred(converter.size)) then converterIndex:=converterIndex+1;
          end
        else
          begin
          newRange.rangeEnd:=rangeEnd + offset;
          rangeProcessed:=true;
          end;
        output_.push(newRange);
        end else
        //3) range start is after the end of the converter. If there are other converters increment and don't add anything
        begin
        if (converterIndex = pred(converter.size)) then
          begin
          newRange.rangeStart:=rangeStart;
          newRange.rangeEnd:=rangeEnd;
          output_.push(newRange);
          rangeProcessed:=true;
          end else converterIndex:=converterIndex+1;
        end;
      end;
    end;
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


