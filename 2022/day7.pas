unit day7;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, aocPuzzle, LazLogger, ExtCtrls, Graphics, arrayUtils;

type

  { TDaySeven }

  TDaySeven = class(TAocPuzzle)
  private
    fDirectorySizes: TStringIntMap;
    fPathsToUpdate: TStringArray;
    fCurrentPath: string;
    procedure addToDirectoryPath(directory: string);
    procedure removeLastFromDirectoryPath;
    procedure addToDirectorySize(file_: string);
    procedure processInstruction(instruction: string);
    procedure calculateDirectorySizes;
  public
    constructor Create(filename: string; paintbox_: TPaintbox = nil);
    procedure runPartOne; override;
    procedure runPartTwo; override;
    property directorySizes: TStringIntMap read fDirectorySizes write fDirectorySizes;
    property pathsToUpdate: TStringArray read fPathsToUpdate write fPathsToUpdate;
    property currentPath: string read fCurrentPath write fCurrentPath;
  end;

implementation

{ TDaySeven }

procedure TDaySeven.addToDirectoryPath(directory: string);
begin
  //update the current path and then add the whole path to the array
  if currentPath.Length > 1 then currentPath := currentPath + '/';
  currentPath := currentPath + directory;
  pathsToUpdate.push(currentPath);
end;

procedure TDaySeven.removeLastFromDirectoryPath;
begin
  //update current path and remove the last item from the array
  currentPath := currentPath.Substring(0, currentPath.LastIndexOf('/'));
  if (currentPath = '') then currentPath := '/';
  pathsToUpdate.splice(pred(pathsToUpdate.size), 1);
end;

procedure TDaySeven.addToDirectorySize(file_: string);
var
  pathToUpdate: string;
  fileSize, currentSize, newSize: integer;
  index: integer;
begin
  //Add the full path as key
  if (file_.Substring(0, 3) <> 'dir') then
  begin
    fileSize := file_.Split(' ')[0].ToInteger;
    //update every directory that is in pathsToUpdate
    for index := 0 to pred(pathsToUpdate.size) do
    begin
      pathToUpdate := pathsToUpdate[index];
      directorySizes.TryGetData(pathToUpdate, currentSize);
      newSize := currentSize + fileSize;
      directorySizes.AddOrSetData(pathToUpdate, newSize);
    end;
  end;
end;

procedure TDaySeven.processInstruction(instruction: string);
var
  command, argument: string;
begin
  command := instruction.Substring(2, 2);
  if command = 'cd' then
  begin
    argument := instruction.Substring(5);
    if argument = '..' then removeLastFromDirectoryPath
    else
      addToDirectoryPath(argument);
  end;
end;

procedure TDaySeven.calculateDirectorySizes;
var
  index: integer;
  curr: string;
begin
  currentPath := '';
  for index := 0 to pred(puzzleInputLines.size) do
  begin
    curr := puzzleInputLines[index];
    if (curr.Substring(0, 1) = '$') then processInstruction(puzzleInputLines[index])
    else
      addToDirectorySize(puzzleInputLines[index]);
  end;
end;

constructor TDaySeven.Create(filename: string; paintbox_: TPaintbox);
begin
  inherited Create(filename,'Day 7',paintbox_);
  fDirectorySizes := TStringIntMap.Create;
  fDirectorySizes.Sorted := True;
  fPathsToUpdate := TStringArray.Create;
end;

procedure TDaySeven.runPartOne;
var
  index: integer;
  dirSize, totalDirSize: integer;
begin
  calculateDirectorySizes;
  //for now print the contents of the directorySizes
  totalDirSize := 0;
  for index := 0 to pred(directorySizes.Count) do
  begin
    dirSize := directorySizes.Data[index];
    if dirSize <= 100000 then
    begin
      totalDirSize := totalDirSize + dirSize;
      results.add('dir ' + directorySizes.Keys[index] + ' ' +
        directorySizes.Data[index].ToString);
    end
    else
      results.add('**Oversize dir ' + directorySizes.Keys[index] + ' ' +
        directorySizes.Data[index].ToString);
  end;
  results.Add('Total size of all directories under 100000 is ' + totalDirSize.ToString);
end;

procedure TDaySeven.runPartTwo;
var
  spaceToFree: integer;
  index, difference, closest, closestIndex: integer;
begin
  calculateDirectorySizes;
  spaceToFree := 30000000 - (70000000 - directorySizes.Data[0]);
  closest := 70000000; //needs to be larger than the largest value we'll find
  for index := 0 to pred(directorySizes.Count) do
  begin
    difference := spaceToFree - directorySizes.Data[index];
    if (difference < 0) and (abs(difference) < closest) then
    begin
      closest := abs(difference);
      closestIndex := index;
    end;
  end;
  results.add('Item to delete is ' + directorySizes.Keys[closestIndex] + ' with size ' +
    directorySizes.Data[closestIndex].ToString);
end;

end.
