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
    fData:string;
    public
    constructor create(version: integer; packetData:string);

  end;

  { TOperatorPacket }

  TOperatorPacket = class(TPacket)
    private

    public
    constructor create(version:integer);

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
    function findCurrentPacketEnd(start: integer;pcktType:PacketType):integer;
    procedure findNextPacket;
    procedure identifyPackets;
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
  identifyPackets;
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
      input.Remove(leadingBitPos,1);
      leadingBitPos:=leadingBitPos + 4;
    end;
  result:=binStringToInteger(input);
end;

function TpacketFactory.findCurrentPacketEnd(start:integer;pcktType:PacketType): integer;
var
  index:integer;
  done:boolean;
begin
  if pcktType = PacketType.literalType then
    begin
      index:=start + 6;
      repeat

      if (index + 5 < length(fData)) then index:= index + 5;

      done:= (fData.Substring(index,1) = '0')
        or (index + 5 >= length(fData));

      until done;
    index:=index+4;
    if (index + 1) mod 4 <> 0 then index:= index + (4 - ((index+1) mod 4));
    result:= index;
    end
  else if pcktType = PacketType.operatorType then
    begin

    end;
end;

procedure TpacketFactory.findNextPacket;
begin
  //based on the type of packet we're on, it should be possible to
  //find where it ends and where the next one begins if there is one.

end;

procedure TpacketFactory.identifyPackets;
var
  packetVersion:integer;
  pcktType:PacketType;
  morePackets:boolean;
  packetEnd:integer;
  packetStart:integer;
  literalData:integer;
begin
 //should get the packet version and type, create the relevant packet type
 //and then move on to the next packet
 morePackets:=true;
 packetStart:=0;
 packetVersion:=findPacketVersion(packetStart);
 pcktType:=findPacketType(packetStart);
 if pcktType = PacketType.literalType then
   begin
     packetEnd:=findCurrentPacketEnd(packetStart,pcktType);
     literalData:=decodeLiteralData(fData,packetStart,packetEnd);
     //identify the data, decode it and
     //create a packet of that type
   end
 else if pcktType = PacketType.operatorType then
   begin
     //create a packet of that type
   end;
end;

{ TPacket }

constructor TPacket.create(version: integer; packetType: PacketType);
begin
  fVersion:=version;
  fPacketType:=packetType;
end;

{ TOperatorPacket }

constructor TOperatorPacket.create(version:integer);
begin
  inherited create(version,PacketType.operatorType);
end;

{ TliteralPacket }

constructor TliteralPacket.create(version:integer;packetData:string);
begin
  inherited create(version,PacketType.literalType);
  fData:=packetData;
end;

end.

