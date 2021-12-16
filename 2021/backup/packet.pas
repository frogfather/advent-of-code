unit packet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,aocUtils;
type

  PacketType = (literalType,operatorType);
  
  TliteralPacket = record
    version:string;

  end;

  TOperatorPacket = record
    version:string;

  end;


  { TpacketDecoder }

  TpacketDecoder = class(TInterfacedObject)
    private
    fData:string;
    function findPacketVersion:string;
    function findPacketType: PacketType;
    public
    constructor create(input:string);
  end;

implementation

{ TpacketDecoder }

constructor TpacketDecoder.create(input: string);
begin
  fData:=hexStringToBinString(input);
end;

end.

