unit fileUtilities;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson;
type
  //Looks like the built in TintegerArray is a static array
  //so let's define our own dynamic integer array
  TIntArray = Array of integer;

function readStream(fnam: string): string;
procedure writeStream(fnam: string; txt: string);
function foundInArray(inputArray: TStringArray; required, startat: integer): boolean;
function openFileAsArray(fnam: string; separator: char;removeBlankLines:Boolean=true): TStringArray;
procedure addToArray(var arrInput:TStringArray; item:string;index:integer=-1);
procedure addToArray(var arrInput:TIntArray;item:integer;index:integer=-1);
procedure deleteFromArray(var arrInput:TStringArray; index: integer);
procedure deleteFromArray(var arrInput:TIntArray; index: integer);
function removeBlankLinesFromStringArray(arrInput: TStringArray):TStringArray;
function removeBlankLinesFromArray(arrInput: TIntArray):TIntArray;
function toIntArray(arrInput: TStringArray):TIntArray;
function getUserDir: string;
function findDirectories(path:string):TStringlist;
implementation

procedure addToArray(var arrInput: TStringArray; item: string; index: integer);
var
  pos,lastItemIndex:integer;
begin
  //if index is -1 add at the end
  setLength(arrInput,length(arrInput)+1);
  lastItemIndex:= pred(length(arrInput));
  if (index = -1) then index:=lastItemIndex; //insert at end
  for pos:=lastItemIndex downto index do
    begin
    if (pos > 0) then arrInput[pos]:=arrInput[pos-1];
    end;
  arrInput[index]:=item;
end;

procedure addToArray(var arrInput: TIntArray; item: integer; index: integer
  );
var
  pos,lastItemIndex:integer;
begin
  //if index is -1 add at the end
  setLength(arrInput,length(arrInput)+1);
  lastItemIndex:= pred(length(arrInput));
  if (index = -1) then index:=lastItemIndex; //insert at end
  for pos:=lastItemIndex downto index do
    begin
    if (pos > 0) then arrInput[pos]:=arrInput[pos-1];
    end;
  arrInput[index]:=item;
end;

procedure deleteFromArray(var arrInput: TStringArray; index: integer);
var
  position:integer;
begin
  if (index < 0) or (index >= length(arrInput)) then exit;
  for position:=index to length(arrInput) - 1 do
    begin
      if (position+1 < length(arrInput))
        then arrInput[position]:=arrInput[position + 1];
    end;
  setLength(arrInput, length(arrInput) -1);
end;

procedure deleteFromArray(var arrInput: TIntArray; index: integer);
var
  position:integer;
begin
 if (index < 0) or (index >= length(arrInput)) then exit;
  for position:=index to length(arrInput) - 1 do
    begin
      if (position+1 < length(arrInput))
        then arrInput[position]:=arrInput[position + 1];
    end;
  setLength(arrInput, length(arrInput) -1);
end;

function removeBlankLinesFromStringArray(arrInput: TStringArray): TStringArray;
var
  index: integer;
begin
  index:=length(arrInput);
  for index:= pred(length(arrInput)) downto 0 do
    begin
      if (length(arrInput[index]) = 0) then
        deleteFromArray(arrInput,index);
    end;
  result:=arrInput;
end;

function removeBlankLinesFromArray(arrInput: TIntArray): TIntArray;
var
  index: integer;
begin
  index:=length(arrInput);
  for index:= pred(length(arrInput)) downto 0 do
    try
     arrInput[index].ToString;
    except
     deleteFromArray(arrInput,index);
    end;
  result:=arrInput;
end;

function toIntArray(arrInput: TStringArray): TIntArray;
//converts the elements of the array to integers if possible
//probably should throw
var
  index:integer;
  output:TIntArray;
begin
  output:=TIntArray.create;
  setLength(output,length(arrInput));
  if length(arrInput) = 0 then
    begin
    result:=output;
    exit;
    end;
  for index:=0 to pred(length(arrInput)) do
    begin
      try
      output[index]:=strToInt(arrInput[index]);
      except
      //do nothing atm
      end;
    end;
end;

//This complete mess is required because getUserDir on MacOS Catalina returns '/'
function getUserDir: string;
var
  userDir:string;
  directoryList:TStringlist;
  index:integer;
begin
  directoryList:=TStringlist.create;
  userDir:=getCurrentDir;
  if (userDir <> '/') then directoryList.Add(userDir) else
    begin
    directoryList:= fileUtilities.findDirectories('/');
    if (directoryList.IndexOf('Users') > -1) then
      begin
      chdir('Users');
      directoryList:=findDirectories('Users/');
      if (directoryList.IndexOf('Shared') > -1) then directoryList.Delete(directorylist.IndexOf('Shared'));
      for index := 0 to directoryList.Count - 1 do
        begin
        directoryList[index]:='/Users/'+directoryList[index];
        end;
      end;
    end;
  if (directoryList.count > 0) then result:=directoryList[0] else result:='';
end;

function findDirectories(path:string):TStringlist;
Var Info : TSearchRec;
    Count : Longint;
begin
result:=TStringlist.Create;
Count:=0;
  If FindFirst ('*',faAnyFile and faDirectory,Info)=0 then
    begin
    Repeat
      Inc(Count);
      With Info do
        begin
        If ((Attr and faDirectory) = faDirectory) and (Name <> '.') and (Name <> '..')  then
          result.Add(Name);
        end;
    Until FindNext(info)<>0;
    end;
  FindClose(Info);
end;

function foundInArray(inputArray: TStringArray; required, startat: integer): boolean;
var
  i:integer;
begin
  if (startat > (length(inputArray) -1)) then exit;
  for i:=startat to length(inputArray)-1 do
    begin
    if (length(inputArray[i]) > 0) and (strtoint(inputArray[i]) = required) then
      begin
        result:=true;
        exit;
      end;
    end;
end;

function openFileAsArray(fnam: string; separator: char;removeBlankLines:boolean=true): TStringArray;
begin
if FileExists(fNam) then
  begin
  if removeBlankLines then
  result := removeBlankLinesFromStringArray(readStream(fNam).Split(separator))
  else result := readStream(fNam).Split(separator);
  end;
end;


//File I/O methods
function readStream(fnam: string): string;
var
  strm: TFileStream;
  n: longint;
  txt: string;
  begin
    txt := '';
    strm := TFileStream.Create(fnam, fmOpenRead);
    try
      n := strm.Size;
      SetLength(txt, n);
      strm.Read(txt[1], n);
    finally
      strm.Free;
    end;
    result := txt;
  end;

procedure writeStream(fnam: string; txt: string);
var
  strm: TFileStream;
  n: longint;
begin
  try
    strm := TFileStream.Create(fnam, fmCreate);
    n := Length(txt);
    strm.Position := 0;
    strm.Write(txt[1], n);
  finally
    strm.Free;
  end;
end;

end.

