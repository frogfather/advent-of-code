unit packet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,aocUtils,arrayUtils;
type

  PacketType = (literalType,operatorType);

  { TPacket }

  TPacket = class(TInterfacedObject)
     private
     fVersion:integer;
     fPacketType: PacketType;
     property version:integer read fVersion;
     public
     constructor create(versionNo:integer; packetType:PacketType);
  end;

  { TPacketArray }
  TPacketArray = array of TPacket;

  { TliteralPacket }

  TliteralPacket = class(TPacket)
    private
    fData:integer;
    public
    constructor create(versionNo: integer; packetData:integer);
  end;

  { TOperatorPacket }

  TOperatorPacket = class(TPacket)
    private
    fData:string;
    fSubPackets: array of TPacket;
    procedure addSubPacket(subPacket: TPacket);
    public
    constructor create(versionNo,subPacketCount:integer;data:string);
  end;


  { TpacketFactory }

  TpacketFactory = class(TInterfacedObject)
    private
    fData:string;
    fPackets: array of TPacket;
    fRDepth: integer;
    fLog: TStringList;
    function findPacketVersion(packetStart:integer):integer;
    function findPacketType(packetStart: integer): PacketType;
    function findPacketLengthType(packetStart:integer):integer;
    function decodeLiteralData(data:string;start,finish: integer):integer;
    function findCurrentPacketEnd(start: integer;pcktType:PacketType):integer;
    function getSubPacketCount(packetStart,packetEnd:integer):integer;
    function getPacketCount:integer;
    function getVersionTotal:integer;
    procedure addPacket(pckt:TPacket);
    function parse(packetStart:integer):integer;
    procedure log(message: string);
    property data:string read fData;
    property rDepth: integer read fRdepth write fRdepth;
    public
    constructor create(input:string);
    destructor destroy;
    property packetCount: integer read getPacketCount;
    property versionTotal: integer read getVersionTotal;
    property pLog: TStringlist read fLog;
  end;

implementation

{ TpacketFactory }

constructor TpacketFactory.create(input: string);
begin
  fData:=hexStringToBinString(input);
  fLog:=TStringList.Create;
  parse(0);
  fRDepth:=0;
end;

destructor TpacketFactory.destroy;
begin
  fLog.Free;
end;

function TpacketFactory.findPacketVersion(packetStart: integer): integer;
begin
  result:=binStringToInteger(fData.Substring(packetStart,3));
end;

function TpacketFactory.findPacketType(packetStart: integer): PacketType;
var
  typeId:integer;
begin
  typeId:=binStringToInteger(fData.Substring(packetStart+3,3));
  if typeId = 4 then result:= PacketType.literalType
  else result:=PacketType.operatorType;
end;

function TpacketFactory.findPacketLengthType(packetStart:integer): integer;
begin
  result:= fData.Substring(packetStart+6,1).ToInteger;
end;

function TpacketFactory.decodeLiteralData(data:string;start,finish:integer): integer;
var
  input:string;
  leadingBitPos:integer;
begin
  input:=data.Substring(start+6, succ(finish) - (start+6) );
  leadingBitPos:=0;
  while (leadingBitPos + 4) < length(input) do
    begin
      input:= input.Remove(leadingBitPos,1);
      leadingBitPos:=leadingBitPos + 4;
    end;
  result:=binStringToInteger(input);
end;

function TpacketFactory.findCurrentPacketEnd(start:integer;pcktType:PacketType): integer;
var
  index,subPacketLength,subPacketCount,lengthType:integer;
  done:boolean;
begin
  index:=start + 6; //first bit after header. First bit of data if literal or length type id
  if pcktType = PacketType.literalType then
    begin
      repeat
      done:= (fData.Substring(index,1) = '0') or (index + 5 >= length(fData));
      if not done and (index + 5 < length(fData)) then index:= index + 5;
      until done;
    index:=index+4;
    result:= index;
    end
  else if pcktType = PacketType.operatorType then
    begin
    lengthType:=findPacketLengthType(start);
    if lengthType = 0 then
      begin
      index:=index+1; //this is the start of subpacket length block
      subPacketLength:=binStringToInteger(fData.Substring(index,15));
      index:=index+14+subPacketLength; //takes us to the end of the packet
      end else
      begin
      index:=index+1; //this is the start of number of subpackets block
      subPacketCount:=binStringToInteger(fData.Substring(index,11));
      index:=index+11; //start of first subpacket
      while subPacketCount > 0 do
        begin
        index:=findCurrentPacketEnd(index,findPacketType(index));
        subPacketCount:=subPacketCount - 1;
        if subPacketCount > 0 then index:=index+1; //move to start of next packet
        end;
      end;
    end;
  result:=index;
end;

function TpacketFactory.getSubPacketCount(packetStart, packetEnd: integer
  ): integer;
var
  isPacketCount:boolean;
  subPacketType:PacketType;
  subPacketLength,subPacketCount,subPacketStart,subPacketEnd:integer;
begin
  //how we do this depends on the mode
  isPacketCount:=(fData.Substring(packetStart+6,1).ToInteger = 1);
  if isPacketCount
    then result:= binStringToInteger(fData.Substring(packetStart+7,11))
  else
    begin
    subPacketCount:=0;
    subPacketLength:= binStringToInteger(fData.Substring(packetStart+7,15));
    subPacketStart:=packetStart + 22;
    while subPacketLength > 0 do
      begin
      subPacketType:=findPacketType(subPacketStart);
      subPacketEnd:=findCurrentPacketEnd(subPacketStart, subPacketType);
      subPacketLength:=subPacketLength - (1+ subPacketEnd - subPacketStart);
      subPacketCount:=subPacketCount+1;
      end;
    result:=subPacketCount;
    end;

end;

function TpacketFactory.getPacketCount: integer;
begin
  result:=length(fPackets);
end;

function TpacketFactory.getVersionTotal: integer;
var
  packetNo,versionSum:integer;
begin
  versionSum:=0;
  for packetNo:=0 to pred(packetCount) do
    versionSum:=versionSum + fPackets[packetNo].version;
  result:=versionSum;
end;

procedure TpacketFactory.addPacket(pckt: TPacket);
begin
  setLength(fPackets,length(fPackets)+1);
  fPackets[length(fPackets)-1]:=pckt;
end;

function TpacketFactory.parse(packetStart:integer):integer;
var
  version,packetEnd: integer;
  subPacketCount,subPacketStart, subPacketEnd, packetLengthType:integer;
  pcktType:PacketType;
  literalData,i:integer;
  operatorData:string;
  sPacketType,sPad:string;
begin
 spad:='';
 version:=findPacketVersion(packetStart);
 pcktType:=findPacketType(packetStart);
 if pcktType = PacketType.literalType then sPacketType:='literal' else sPacketType := 'operator';
 packetEnd:=findCurrentPacketEnd(packetStart,pcktType);
 for i:= 0 to rDepth do sPad:=sPad+'  ';
 log(sPad+rDepth.ToString+' Start '+packetStart.ToString+' End '+packetEnd.ToString+' Version '+version.ToString+' Type: '+spacketType);

 if pcktType = PacketType.literalType then
   begin
     literalData:=decodeLiteralData(fData, packetStart,packetEnd);
     log(sPad+rDepth.ToString+' Add literal packet');
     addPacket(TLiteralPacket.create(version,literalData));
   end
 else if pcktType = PacketType.operatorType then
   begin
     packetLengthType:=findPacketLengthType(packetStart);
     subPacketCount:=getSubPacketCount(packetStart,packetEnd);
     log(sPad+rDepth.ToString+' Sub packet count: '+subPacketCount.ToString);
       if packetLengthType = 1
       then subPacketStart:= packetStart + 18
     else subPacketStart:= packetStart + 22;
     operatorData:=fData.Substring(subPacketStart, (1+ packetEnd - subPacketStart));
     log(sPad+rDepth.ToString+' Add operator packet');
     addPacket(TOperatorPacket.create(version,subPacketCount,operatorData));
     //while this packet has subpackets call this method again
     while subPacketCount > 0 do
       begin
       rDepth:=rDepth + 1;
       subPacketEnd:= parse(subPacketStart);
       rDepth:=rDepth -1;
       subPacketCount:=subPacketCount -1;
       if subPacketCount > 0 then subPacketStart:=subPacketEnd + 1;
       end;
   end;
 result:=packetEnd;
end;

procedure TpacketFactory.log(message: string);
begin
  fLog.Add(message);
end;

{ TPacket }

procedure TOperatorPacket.addSubPacket(subPacket: TPacket);
begin
 setLength(fSubPackets, length(fSubPackets) + 1);
 fSubPackets[pred(length(fSubPackets))]:=subPacket;
end;

constructor TPacket.create(versionNo: integer; packetType: PacketType);
begin
  fVersion:=versionNo;
  fPacketType:=packetType;
end;

{ TOperatorPacket }

constructor TOperatorPacket.create(versionNo,subPacketCount:integer;data:string);
begin
  inherited create(versionNo,PacketType.operatorType);
  fSubPackets:=TPacketArray.create;
end;

{ TliteralPacket }

constructor TliteralPacket.create(versionNo:integer;packetData:integer);
begin
  inherited create(versionNo,PacketType.literalType);
  fData:=packetData;
end;

end.

