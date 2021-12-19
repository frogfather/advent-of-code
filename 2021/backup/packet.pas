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
    fData:int64;
    public
    constructor create(versionNo: integer; packetData:int64);
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
    fLog:TStringList;
    fPacketSum:integer;
    fLevel:integer;
    function findPacketVersion(packetStart:integer):integer;
    function findPacketType(packetStart: integer): PacketType;
    function findPacketOperator(packetStart: integer): PacketOperator;
    function findPacketLengthType(packetStart:integer):integer;
    function decodeLiteralData(data:string;start,finish: integer):int64;
    function findCurrentPacketEnd(start: integer;pcktType:PacketType):integer;
    function getSubPacketCount(packetStart:integer):integer;
    function getPacketCount:integer;
    procedure addPacket(pckt:TPacket);
    procedure log(message:string);
    function getLog:TStringList;
    function parse(parentPacket:TPacket;packetStart:integer):integer;
    function calculateValue(packet:TPacket=nil):TPacket;
    property data:string read fData;
    property packets: TPacketArray read fPackets;
    property level:integer read fLevel write fLevel;
    public
    constructor create(input:string);
    function getVersionTotal(packet:TPacket=nil):integer;
    property packetSum: integer read fPacketSum;
    property packetCount: integer read getPacketCount;
    property logItems: TStringlist read fLog;

  end;

implementation

{ TpacketFactory }

constructor TpacketFactory.create(input: string);
begin
  fData:=hexStringToBinString(input);
  fLog:=TStringList.Create;
  fLevel:=0;
  parse(nil,0);
  fPacketSum:= (calculateValue as TLiteralPacket).fData;
end;

function TpacketFactory.findPacketVersion(packetStart: integer): integer;
begin
  result:=binStringToInt64(fData.Substring(packetStart,3));
end;

//TODO - change this to take a packet instead of an integer
function TpacketFactory.findPacketType(packetStart: integer): PacketType;
var
  typeId:integer;
begin
  typeId:=binStringToInt64(data.Substring(packetStart+3,3));
  if typeId = 4 then result:= PacketType.literalType
  else result:=PacketType.operatorType;
end;

function TpacketFactory.findPacketOperator(packetStart: integer): PacketOperator;
var
  typeId:integer;
begin
  typeId:=binStringToInt64(fData.Substring(packetStart+3,3));
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

function TpacketFactory.decodeLiteralData(data:string;start,finish:integer): int64;
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
  result:=binStringToInt64(input);
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
      subPacketLength:=binStringToInt64(fData.Substring(index,15));
      index:=index+14+subPacketLength; //takes us to the end of the packet
      end else
      begin
      index:=index+1; //this is the start of number of subpackets block
      subPacketCount:=binStringToInt64(fData.Substring(index,11));
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
    then result:= binStringToInt64(fData.Substring(packetStart+7,11))
  else
    begin
    subPacketCount:=0;
    subPacketLength:= binStringToInt64(fData.Substring(packetStart+7,15));
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

procedure TpacketFactory.log(message: string);
begin
  fLog.add(message);
end;

function TpacketFactory.getLog: TStringList;
begin
  result:=fLog;
end;

function TpacketFactory.parse(parentPacket:TPacket;packetStart:integer):integer;
var
  thisPacket:TPacket;
  version,packetEnd: integer;
  subPacketCount,subPacketStart, subPacketEnd, packetLengthType:integer;
  pcktType:PacketType;
  pcktOp: PacketOperator;
  literalData:int64;

begin
 version:=findPacketVersion(packetStart);
 pcktType:=findPacketType(packetStart);
 packetEnd:=findCurrentPacketEnd(packetStart,pcktType);
 if pcktType = PacketType.literalType then
   begin
     literalData:=decodeLiteralData(fData, packetStart,packetEnd);
     log('literal data '+data.Substring(packetStart+6, succ(packetEnd) - (packetStart+6) )+' -> '+literalData.ToString);
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

function TpacketFactory.calculateValue(packet: TPacket): TPacket;
var
  pNo,subPNo,subPacketCount:integer;
  packetArray:TPacketArray;
  firstPacket,secondPacket:TLiteralPacket;
  calculatedTotal,pkValue:int64;
  allLiteral:boolean;
  pktOp: PacketOperator;
begin
  calculatedTotal:=0;
  if packet is TLiteralPacket then
    begin
    //return it unchanged and quit - nothing more to do
    calculatedTotal:= (packet as TLiteralPacket).fData;
    subPacketCount:=0;
    packetArray:=nil;
    result:=packet;
    exit;
    log('L '+level.ToString+' Packet is literal with value '+calculatedTotal.ToString);
    end;
  if packet is TOperatorPacket then
    begin
    //traverse the subpackets and return a Literal packet with the value of it
    packetArray:= (packet as TOperatorPacket).subPackets;
    subPacketCount:= (packet as TOperatorPacket).subPacketCount;
    log('L '+level.ToString+' Operator packet has '+subPacketCount.ToString+' subpackets');
    end else
  if packet = nil then
    begin
    //top level packet - traverse subpackets
    packetArray:= packets;
    subPacketCount:= packetCount;
    log('L '+level.ToString+' top level packet. SubPacketCount: '+subPacketCount.ToString);
    end;

  if (packetArray = nil) or (subPacketCount = 0) then
    begin
    //return a packet with zero value
    result:= TLiteralPacket.create(0,0);
    exit;
    end;

  //Find out if all the subpackets are literal - if not further processing required
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
      //this should return a LiteralPacket for each element in the array;
      fLevel:=fLevel+1;
      result:= calculateValue(packetArray[pNo]);
      log('L '+level.ToString+' Sum of subpackets '+(result as TLiteralPacket).fData.ToString);
      fLevel:=fLevel-1;
      end else
      begin
      //apply the specified operator to them and return a literal packet with that value
      firstPacket:=(packetArray[0] as TLiteralPacket);
      if subPacketCount > 1 then secondPacket:=(packetArray[1] as TLiteralPacket);
      pktOp:= (packet as TOperatorPacket).packetOp;
        case pktOp of
          poSum:
            begin
            for subPNo:= 0 to pred(subPacketCount) do
              begin
              pkValue:= (packetArray[subPNo] as TLiteralPacket).fData;
              log('L '+level.ToString+' Literal packet value '+(pkValue.ToString));
              calculatedTotal:=calculatedTotal + (packetArray[subPNo] as TLiteralPacket).fData;
              end;
              log('L '+level.ToString+' Total from add operation '+ calculatedTotal.ToString);
            end;
          poProduct:
            begin
            calculatedTotal:=1;
            for subPNo:= 0 to pred(subPacketCount) do
              begin
              pkValue:= (packetArray[subPNo] as TLiteralPacket).fData;
              log('L '+level.ToString+' Literal packet value '+(pkValue.ToString));
              calculatedTotal:=calculatedTotal * pkValue;
              log('L '+level.ToString+' Total from multiply operation '+ calculatedTotal.ToString);
              end;
            end;
          poMin:
            begin
            calculatedTotal:= (packetArray[0] as TLiteralPacket).fData;
            for subPNo:= 0 to pred(subPacketCount) do
              begin
              pkValue:= (packetArray[subPNo] as TLiteralPacket).fData;
              log('L '+level.ToString+' Literal packet value '+(pkValue.ToString));
              if (packetArray[subPNo] as TLiteralPacket).fData < calculatedTotal
                then calculatedTotal:= (packetArray[subPNo] as TLiteralPacket).fData;
              end;
            log('L '+level.ToString+' Lowest value '+ calculatedTotal.ToString);
            //minimum of the values
            end;
          poMax:
            begin
            calculatedTotal:=0;
            for subPNo:= 0 to pred(subPacketCount) do
              begin
              pkValue:= (packetArray[subPNo] as TLiteralPacket).fData;
              log('L '+level.ToString+' Literal packet value '+(pkValue.ToString));
              if (packetArray[subPNo] as TLiteralPacket).fData > calculatedTotal
                then calculatedTotal:= (packetArray[subPNo] as TLiteralPacket).fData;
              end;
            //max of the values
            log('L '+level.ToString+' Highest value '+ calculatedTotal.ToString);
            end;
          poGreater:
            begin
            log('L '+level.ToString+' poGreater '+ firstPacket.fData.ToString+ ' '+ secondPacket.fData.ToString);
            if firstPacket.fData > secondPacket.fData
              then calculatedTotal:=1 else calculatedTotal:=0;
            log('L '+level.ToString+'result '+calculatedTotal.ToString);
            end;
          poLess:
            begin
            log('L '+level.ToString+' poLess '+ firstPacket.fData.ToString+ ' '+ secondPacket.fData.ToString);
            if firstPacket.fData < secondPacket.fData
              then calculatedTotal:=1 else calculatedTotal:=0;
            log('L '+level.ToString+'result '+calculatedTotal.ToString);
            end;
          poEqual:
            begin
            log('L '+level.ToString+' poEqual '+ firstPacket.fData.ToString+ ' '+ secondPacket.fData.ToString);
            if firstPacket.fData = secondPacket.fData
              then calculatedTotal:=1 else calculatedTotal:=0;
            log('L '+level.ToString+'result '+calculatedTotal.ToString);
            end;
        end;
      end;
  result:= TLiteralPacket.create(0,calculatedTotal);
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

