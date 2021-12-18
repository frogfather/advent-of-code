unit packet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,aocUtils,arrayUtils;
type

  PacketType = (literalType, operatorType);
  PacketOperator = (poSum, poProduct, poMin, poMax, poGreater, poLess, poEqual,poNone);

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
    fSubPackets: TPacketArray;
    fOperator: PacketOperator;
    procedure addSubPacket(subPacket: TPacket);
    function getSubPacketCount:integer;
    public
    constructor create(versionNo:integer; pktOperator: PacketOperator);
    property packetOp: PacketOperator read fOperator;
    property subPackets: TPacketArray read fSubPackets;
    property subPacketCount:integer read getSubPacketCount;
  end;


  { TpacketFactory }

  TpacketFactory = class(TInterfacedObject)
    private
    fData:string;
    fPackets: TPacketArray;
    function findPacketVersion(packetStart:integer):integer;
    function findPacketType(packetStart: integer): PacketType;
    function findPacketOperator(packetStart: integer): PacketOperator;
    function findPacketLengthType(packetStart:integer):integer;
    function decodeLiteralData(data:string;start,finish: integer):integer;
    function findCurrentPacketEnd(start: integer;pcktType:PacketType):integer;
    function getSubPacketCount(packetStart:integer):integer;
    function getPacketCount:integer;
    procedure addPacket(pckt:TPacket);
    function parse(parentPacket:TPacket;packetStart:integer):integer;
    property data:string read fData;
    property packets: TPacketArray read fPackets;
    public
    constructor create(input:string);
    function getVersionTotal(packet:TPacket=nil):integer;
    function calculateValue(packet:TPacket=nil):integer;
    property packetCount: integer read getPacketCount;
  end;

implementation

{ TpacketFactory }

constructor TpacketFactory.create(input: string);
begin
  fData:=hexStringToBinString(input);
  parse(nil,0);
end;

function TpacketFactory.findPacketVersion(packetStart: integer): integer;
begin
  result:=binStringToInteger(fData.Substring(packetStart,3));
end;

//TODO - change this to take a packet instead of an integer
function TpacketFactory.findPacketType(packetStart: integer): PacketType;
var
  typeId:integer;
begin
  typeId:=binStringToInteger(data.Substring(packetStart+3,3));
  if typeId = 4 then result:= PacketType.literalType
  else result:=PacketType.operatorType;
end;

function TpacketFactory.findPacketOperator(packetStart: integer): PacketOperator;
var
  typeId:integer;
begin
  typeId:=binStringToInteger(fData.Substring(packetStart+3,3));
  case typeId of
    0: result:= PacketOperator.poSum;
    1: result:= PacketOperator.poProduct;
    2: result:= PacketOperator.poMin;
    3: result:= PacketOperator.poMax;
    5: result:= PacketOperator.poGreater;
    6: result:= PacketOperator.poLess;
    7: result:= PacketOperator.poEqual;
  else
    result:=PacketOperator.poNone;
  end;
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

function TpacketFactory.getSubPacketCount(packetStart: integer
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
      subPacketStart:=succ(subPacketEnd);
      subPacketCount:=subPacketCount+1;
      end;
    result:=subPacketCount;
    end;

end;

function TpacketFactory.getPacketCount: integer;
begin
  result:=length(fPackets);
end;

function TpacketFactory.getVersionTotal(packet:TPacket=nil): integer;
var
  pNo,subPacketCount:integer;
  packetArray:TPacketArray;
  versionTotal:integer;
begin
  versionTotal:=0;
  if packet is TLiteralPacket then
    begin
    versionTotal:=versionTotal + packet.version;
    subPacketCount:=0;
    end else
  if packet is TOperatorPacket then
    begin
    versionTotal:=versionTotal + packet.version;
    packetArray:= (packet as TOperatorPacket).subPackets;
    subPacketCount:= (packet as TOperatorPacket).subPacketCount;
    end else
  if packet = nil then
    begin
    packetArray:= packets;
    subPacketCount:= packetCount;
    end;
  //for each element in the array, call this again
  for pNo:= 0 to pred(subPacketCount) do
    begin
    versionTotal:= versionTotal + getVersionTotal(packetArray[pNo])
    end;
  result:=versionTotal;
end;

procedure TpacketFactory.addPacket(pckt: TPacket);
begin
  setLength(fPackets,length(fPackets)+1);
  fPackets[length(fPackets)-1]:=pckt;
end;

function TpacketFactory.parse(parentPacket:TPacket;packetStart:integer):integer;
var
  thisPacket:TPacket;
  version,packetEnd: integer;
  subPacketCount,subPacketStart, subPacketEnd, packetLengthType:integer;
  pcktType:PacketType;
  pcktOp: PacketOperator;
  literalData:integer;

begin
 version:=findPacketVersion(packetStart);
 pcktType:=findPacketType(packetStart);
 packetEnd:=findCurrentPacketEnd(packetStart,pcktType);
 if pcktType = PacketType.literalType then
   begin
     literalData:=decodeLiteralData(fData, packetStart,packetEnd);
     thisPacket:=TLiteralPacket.create(version,literalData);
     if parentPacket = nil
       then addPacket(thisPacket) //edge case where there is only one packet
     else with parentPacket as TOperatorPacket do addSubPacket(thisPacket);
   end
 else if pcktType = PacketType.operatorType then
   begin
     packetLengthType:=findPacketLengthType(packetStart);
     pcktOp:= findPacketOperator(packetStart);
     subPacketCount:=getSubPacketCount(packetStart);
       if packetLengthType = 1
       then subPacketStart:= packetStart + 18
     else subPacketStart:= packetStart + 22;
     thisPacket:=TOperatorPacket.create(version,pcktOp);
     if parentPacket = nil
       then addPacket(thisPacket)
     else with parentPacket as TOperatorPacket do addSubPacket(thisPacket);
     //while this packet has subpackets call this method again
     while subPacketCount > 0 do
       begin
       subPacketEnd:= parse(thisPacket,subPacketStart);
       subPacketCount:=subPacketCount -1;
       if subPacketCount > 0 then subPacketStart:=subPacketEnd + 1;
       end;
   end;
 result:=packetEnd;
end;

function TpacketFactory.calculateValue(packet: TPacket): integer;
var
  pNo,subPacketCount:integer;
  packetArray:TPacketArray;
  firstPacket,secondPacket:TLiteralPacket;
  calculatedTotal:integer;
  allLiteral:boolean;
  pktOp: PacketOperator;
begin
  calculatedTotal:=0;
  if packet is TOperatorPacket then
    begin
    packetArray:= (packet as TOperatorPacket).subPackets;
    subPacketCount:= (packet as TOperatorPacket).subPacketCount;
    end else
  if packet = nil then
    begin
    packetArray:= packets;
    subPacketCount:= packetCount;
    end;

  if subPacketCount = 0 then
    begin
    result:=0;
    exit;
    end;

  allLiteral:=true;
  for pNo:=0 to pred(subPacketCount) do
    begin
    if packetArray[pNo] is TOperatorPacket then
      begin
      allLiteral:=false;
      break;
      end;
    end;
  if allLiteral = false then
    for pNo:=0 to pred(subPacketCount) do
      begin
      calculatedTotal:= calculateValue(packetArray[pNo]);
      end else
      begin
      firstPacket:=(packetArray[0] as TLiteralPacket);
      if subPacketCount > 1 then secondPacket:=(packetArray[1] as TLiteralPacket);
      pktOp:= (packet as TOperatorPacket).packetOp;
        case pktOp of
          poSum:
            begin
            for pNo:= 0 to pred(subPacketCount) do;
              begin
              calculatedTotal:=calculatedTotal + (packetArray[pNo] as TLiteralPacket).fData;
              end;
            end;
          poProduct:
            begin
            calculatedTotal:=1;
            for pNo:= 0 to pred(subPacketCount) do;
              begin
              calculatedTotal:=calculatedTotal * (packetArray[pNo] as TLiteralPacket).fData;
              end;
            end;
          poMin:
            begin
            calculatedTotal:= (packetArray[0] as TLiteralPacket).fData;
            for pNo:= 0 to pred(subPacketCount) do;
              begin
              if (packetArray[pNo] as TLiteralPacket).fData < calculatedTotal
                then calculatedTotal:= (packetArray[pNo] as TLiteralPacket).fData;
              end;
            //minimum of the values
            end;
          poMax:
            begin
            calculatedTotal:=0;
            for pNo:= 0 to pred(subPacketCount) do;
              begin
              if (packetArray[pNo] as TLiteralPacket).fData > calculatedTotal
                then calculatedTotal:= (packetArray[pNo] as TLiteralPacket).fData;
              end;
            //max of the values
            end;
          poGreater:
            begin
            if firstPacket.fData > secondPacket.fData
              then calculatedTotal:=1 else calculatedTotal:=0;
            end;
          poLess:
            begin
            if firstPacket.fData < secondPacket.fData
              then calculatedTotal:=1 else calculatedTotal:=0;
            end;
          poEqual:
            begin
            if firstPacket.fData = secondPacket.fData
              then calculatedTotal:=1 else calculatedTotal:=0;
            end;
        end;
      end;
  result:=calculatedTotal;
end;

{ TPacket }

procedure TOperatorPacket.addSubPacket(subPacket: TPacket);
begin
 setLength(fSubPackets, length(fSubPackets) + 1);
 fSubPackets[pred(length(fSubPackets))]:=subPacket;
end;

function TOperatorPacket.getSubPacketCount: integer;
begin
  result:=length(fSubPackets);
end;

constructor TPacket.create(versionNo: integer; packetType: PacketType);
begin
  fVersion:=versionNo;
  fPacketType:=packetType;
end;

{ TOperatorPacket }

constructor TOperatorPacket.create(versionNo:integer;pktOperator:PacketOperator);
begin
  inherited create(versionNo,PacketType.operatorType);
  fSubPackets:=TPacketArray.create;
  fOperator:=pktOperator;
end;

{ TliteralPacket }

constructor TliteralPacket.create(versionNo:integer;packetData:integer);
begin
  inherited create(versionNo,PacketType.literalType);
  fData:=packetData;
end;

end.

