{===============================================================================
  公共类型单元

===============================================================================}
unit xTypes;

interface

type
  TSendRevPack = procedure ( aPacks: TArray<Byte>; bSend : Boolean) of object;

type
  TIPEvent = procedure ( sIP : string; sPort : Integer) of object;

type
  TIPSendRevPack = procedure ( sIP : string; nPort: Integer; aPacks: TArray<Byte>; bSend : Boolean) of object;

type
  TSendPack = function(APacks: TArray<Byte>; sParam1: string = ''; sParam2 : string=''): Boolean of object;

type
  TOrderObject = function( nOrderType: Integer):TObject of object;

type
  TEnventPack = procedure (aPacks: TArray<Byte>; sParam1: string = ''; sParam2 : string='') of object;

implementation

end.
