unit aocgen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, MaskEdit, fileUtilities;

type

  { TAocgenerator }

  TAocgenerator = class(TForm)
    bGenerate: TButton;
    ckPuzzleFiles: TCheckBox;
    eTarget: TEdit;
    Label1: TLabel;
    lActions: TLabel;
    lTarget: TLabel;
    ListBox1: TListBox;
    meYear: TMaskEdit;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure bGenerateClick(Sender: TObject);
    procedure eTargetDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function numberToName(number:integer):string;
    function unitNumberToName(number: integer):TstringArray;
    function tensNumberToName(number: integer):TstringArray;
  public

  end;

var
  Aocgenerator: TAocgenerator;

implementation

{$R *.lfm}

{ TAocgenerator }

procedure TAocgenerator.FormShow(Sender: TObject);
begin
meYear.Text:=formatDateTime('YYYY', now);
end;

function TAocgenerator.numberToName(number: integer): string;
var
  tens,units:TStringArray;
begin
  tens:=tensNumberToName(number div 10);
  units:=unitNumberToName(number mod 10);
  if (number < 11) or (number > 19)
     then result:=tens[0]+units[1]
  else if (number > 10) and (number < 13)
     then result:= units[0]
  else if (number < 20) then result:=units[0]+tens[1];

end;

function TAocgenerator.unitNumberToName(number: integer): TstringArray;
begin
  case number of
  1: result:=['Eleven','One'];
  2: result:=['Twelve','Two'];
  3: result:=['Thir','Three'];
  4: result:=['Four','Four'];
  5: result:=['Fif','Five'];
  6: result:=['Six','Six'];
  7: result:=['Seven','Seven'];
  8: result:=['Eigh','Eight'];
  9: result:=['Nine','Nine'];
  0: result:=['',''];
  end;
end;

function TAocgenerator.tensNumberToName(number: integer): TstringArray;
begin
  case number of
  0: result:=[''];
  1: result:=['Ten','teen'];
  2: result:=['Twenty','Twenty'];
  end;
end;

procedure TAocgenerator.bGenerateClick(Sender: TObject);
var
  input,output,dayname,filename: string;
  dayno:integer;
  dirName:string;
begin
if (ckPuzzleFiles.Checked = true) then
  begin
  input:=readStream('/Users/johncampbell/Code/advent-of-code/aoc-generator/template.txt');
  dirname:= selectDirectoryDialog1.FileName;
  for dayno:= 1 to 25 do
    begin
    dayname:=numberToName(dayno);
    filename:= 'day'+dayno.toString+'.pas';
    //does it exist?
    if fileExists(dirName+'/'+filename)
      then listbox1.Items.add('file '+filename+' already exists. Skipping')
      else
        begin
        output:=input.replace('$%', dayno.toString).Replace('$$',dayname);
        listbox1.items.add('generating '+filename);
        writeStream(dirName+'/'+filename,output);
        listbox1.items[listbox1.Count-1]:='generating '+filename+'...success!'
        end;
    end;
  end;
end;

procedure TAocgenerator.eTargetDblClick(Sender: TObject);
begin
  if SelectDirectoryDialog1.Execute then
    eTarget.Text:=SelectDirectoryDialog1.FileName;
end;

end.

