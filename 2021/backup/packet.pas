unit packet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,aocUtils;
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


  { TpacketFactory }

  TpacketFactory = class(TInterfacedObject)
    private
    fData:string;
    fIndex:integer;
    function findPacketVersion:string;
    function findPacketType: PacketType;
    public
    constructor create(input:string);
  end;

implementation

{ TOperatorPacket }

constructor TOperatorPacket.create(version:integer);
begin
  inherited create(version,PacketType.operatorType);
end;

{ TliteralPacket }

constructor TliteralPacket.create(version,packetData:string);
begin
  inherited create(version:integer;PacketType.literalType);
  fData:=packetData;
end;

{ TpacketFactory }

function TpacketFactory.findPacketVersion: string;
begin
  //assuming we have correctly found the start of the packet
  //fIndex should point to it

end;

function TpacketFactory.findPacketType: PacketType;
begin
  //assuming we have correctly found the start of the packet
  //fIndex should point to it
end;

constructor TpacketFactory.create(input: string);
begin
  fData:=input;
  fIndex:=0;
end;

{ TPacket }

constructor TPacket.create(version: integer; packetType: PacketType);
begin
  fVersion:=version;
  fPacketType:=packetType;
end;



end.

