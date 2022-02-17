unit fileUtilities;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson,arrayUtils;

function readStream(fnam: string): string;
procedure writeStream(fnam: string; txt: string);
function foundInArray(inputArray: TStringArray; required, startat: integer): boolean;
function openFileAsArray(fnam: string; separator: char;removeBlankLines:Boolean=true): TStringArray;
function getUserDir: string;
function findDirectories(path:string):TStringlist;
implementation

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
var
  option:TStringSplitOptions;
  fileContents:String;
begin
if FileExists(fNam) then
  begin
  if removeBlankLines
    then option:=TStringSplitOptions.ExcludeEmpty
  else option:=TStringSplitOptions.None;
  fileContents:=readStream(fNam);
  //remove separator as last character
  if fileContents[length(fileContents)] = separator
    then fileContents:= fileContents.Remove(pred(length(fileContents)),1);
  result := fileContents.Split(separator,option);
  end;
end;


//File I/O methods
function readStream(fnam: string): string;
var
  strm: TFileStream;
  n: longint;
  txt: string;
  begin
    if not fileExists(fnam) then exit;
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

