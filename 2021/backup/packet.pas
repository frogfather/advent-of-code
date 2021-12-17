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
     public
     constructor create(version:integer; packetType:PacketType);
  end;

  { TliteralPacket }

  TliteralPacket = class(TPacket)
    private
    fData:integer;
    public
    constructor create(version: integer; packetData:integer);

  end;

  { TOperatorPacket }

  TOperatorPacket = class(TPacket)
    private
    fData:string;
    fSubPackets:integer;
    property subPackets: integer read fSubPackets write fSubPackets;
    public
    constructor create(version,subPacketCount:integer;data:string);
  end;

  TPacketArray = array of TPacket;

  { TpacketFactory }

  TpacketFactory = class(TInterfacedObject)
    private
    fData:string;
    fPackets: TPacketArray;
    function findPacketVersion(packetStart:integer):integer;
    function findPacketType(packetStart: integer): PacketType;
    function decodeLiteralData(data:string;start,finish: integer):integer;
    function findCurrentPacketEnd(start: integer;pcktType:PacketType;isSub:boolean=false):integer;
    function getSubPacketCount(packetStart,packetEnd:integer):integer;
    procedure addPacket(pckt:TPacket);
    procedure findNextPacket;
    procedure identifyPackets(packetStart:integer);
    property data:string read fData;
    public
    constructor create(input:string);
  end;

implementation

{ TpacketFactory }

constructor TpacketFactory.create(input: string);
begin
  fData:=hexStringToBinString(input);
  //process the string and create packets from it
  identifyPackets(0);
end;

function TpacketFactory.findPacketVersion(packetStart: integer): integer;
begin
  //assuming we have correctly found the start of the packet
  //fCurrentPacketStart should point to it
  result:=binStringToInteger(fData.Substring(packetStart,3));
end;

function TpacketFactory.findPacketType(packetStart: integer): PacketType;
var
  typeId:integer;
begin
  //assuming we have correctly found the start of the packet
  //fCurrentPacketStart should point to it
  typeId:=binStringToInteger(fData.Substring(packetStart+3,3));
  if typeId = 4 then result:= PacketType.literalType
  else result:=PacketType.operatorType;
end;

function TpacketFactory.decodeLiteralData(data:string;start,finish:integer): integer;
var
  input:string;
  leadingBitPos:integer;
begin
  //remove 6 id bits and padding zeros
  input:=data.Substring(6, length(data) - (5 + (length(data) mod 5)));
  //remove leading bit from each 5 bit block
  leadingBitPos:=0;
  while (leadingBitPos + 4) < length(input) do
    begin
      input:= input.Remove(leadingBitPos,1);
      leadingBitPos:=leadingBitPos + 4;
    end;
  result:=binStringToInteger(input);
end;

function TpacketFactory.findCurrentPacketEnd(start:integer;pcktType:PacketType;isSub:boolean): integer;
var
  index,subPacketLength,subPacketCount,lengthType:integer;
  done:boolean;
begin
  index:=start + 6; //first bit after header
  if pcktType = PacketType.literalType then
    begin
      repeat
      done:= (fData.Substring(index,1) = '0') or (index + 5 >= length(fData));
      if not done and (index + 5 < length(fData)) then index:= index + 5;
      until done;
    index:=index+4;// move to the end of the group
    result:= index;
    end
  else if pcktType = PacketType.operatorType then
    begin
    lengthType:=fData.Substring(index,1).ToInteger;
    index:=index+1;
    if lengthType = 0 then
      begin
      subPacketLength:=binStringToInteger(fData.Substring(index,15));
      index:=index+15+subPacketLength;
      end else
      begin
      subPacketCount:=binStringToInteger(fData.Substring(index,11));
      //we can call this method again to find the end of the subpackets
      index:=index+11;
      while subPacketCount > 0 do
        begin
        index:=findCurrentPacketEnd(index,findPacketType(index),true);
        subPacketCount:=subPacketCount - 1;
        if subPacketCount > 0 then index:=index+1; //move to start of next packet
        end;
      end;
    end;
  if not isSub and (((index - start) + 1) mod 4 <> 0) then index:= index + (4 - ((index+1) mod 4));
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
    end;

end;

procedure TpacketFactory.addPacket(pckt: TPacket);
begin
  setLength(fPackets,length(fPackets)+1);
  fPackets[length(fPackets)-1]:=pckt;
end;

procedure TpacketFactory.findNextPacket;
begin
  //based on the type of packet we're on, it should be possible to
  //find where it ends and where the next one begins if there is one.

end;

procedure TpacketFactory.identifyPackets(packetStart:integer);
var
  packetVersion,packetEnd,subPacketCount:integer;
  pcktType:PacketType;
  literalData:integer;
  operatorData:string;
begin
 //should get the packet version and type, create the relevant packet type
 //and then move on to the next packet
 packetVersion:=findPacketVersion(packetStart);
 pcktType:=findPacketType(packetStart);
 packetEnd:=findCurrentPacketEnd(packetStart,pcktType);
 if pcktType = PacketType.literalType then
   begin
     literalData:=decodeLiteralData(fData,packetStart,packetEnd);
     addPacket(TLiteralPacket.create(packetVersion,literalData));
     //identify the data, decode it and
     //create a packet of that type
   end
 else if pcktType = PacketType.operatorType then
   begin
     //we need to add this packet as well as any sub packets
     //get the subpacket count for this
     subPacketCount:=getSubPacketCount(packetStart,packetEnd);
     operatorData:=fData.Substring(packetStart+7,packetEnd - (packetStart+7));
     addPacket(TOperatorPacket.create(packetVersion,subPacketCount,operatorData));
     //while this packet has subpackets call this method again

   end;
end;

{ TPacket }

constructor TPacket.create(version: integer; packetType: PacketType);
begin
  fVersion:=version;
  fPacketType:=packetType;
end;

{ TOperatorPacket }

constructor TOperatorPacket.create(version,sub:integer);
begin
  inherited create(version,PacketType.operatorType);
end;

{ TliteralPacket }

constructor TliteralPacket.create(version:integer;packetData:integer);
begin
  inherited create(version,PacketType.literalType);
  fData:=packetData;
end;

end.

