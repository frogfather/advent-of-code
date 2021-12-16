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
     fVersion:string;
     fPacketType: PacketType;
     public
     constructor create(version:string);
  end;

  TliteralPacket = class(TPacket)
    private

    public
    constructor create();

  end;

  TOperatorPacket = class(TPacket)
    private

    public
    constructor create();

  end;


  { TpacketFactory }

  TpacketFactory = class(TInterfacedObject)
    private
    fData:string;
    function findPacketVersion:string;
    function findPacketType: PacketType;
    public
    constructor create(input:string);
  end;

implementation

{ TpacketFactory }

function TpacketFactory.findPacketVersion: string;
begin

end;

function TpacketFactory.findPacketType: PacketType;
begin

end;

constructor TpacketFactory.create(input: string);
begin

end;

{ TPacket }

constructor TPacket.create(version: string);
begin

end;

{ TpacketDecoder }

function TpacketDecoder.findPacketVersion: string;
begin

end;

function TpacketDecoder.findPacketType: PacketType;
begin

end;

constructor TpacketDecoder.create(input: string);
begin
  fData:=hexStringToBinString(input);
end;

end.

