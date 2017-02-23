unit xProtocolPacksDL645_97;

interface

uses System.Types, xProtocolPacks, System.Classes, system.SysUtils, xDL645Type,
  xFunction, xMeterDataRect, xProtocolPacksDl645;

type
  TProtocolPacksDl645_97 = class(TProtocolPacksDl645)
  private
  protected
    function CreatePackReadNextData: TBytes;override;   //  读后续数据
    function CreatePackReadAddr: TBytes;override;       //  读通信地址
    function CreatePackWriteData: TBytes;override;      //  写数据
    function CreatePackChangePWD: TBytes;override;      //  改密码
    function CreatePackClearMaxDemand: TBytes;override; //  最大需量清零
    function CreatePackSetWOutType: TBytes;override;    //  设置多功能口
    function CreatePackIdentity: TBytes;override;       //  身份认证
    function CreatePackOnOffControl: TBytes;override;   //  费控 拉合闸

    /// <summary>
    /// 解析接收到的数据包
    /// </summary>
    procedure ParseRevPack( APack : TBytes ); override;
    procedure ParsePackReadData(APack : TBytes); // 解析读数据
    procedure ParsePackNextData(APack : TBytes); // 解析读后续数据
    procedure ParsePackReadAddr(APack : TBytes); // 解析读地址

    function GetBaudRateCode( sBaudRate : string ) : Integer; override;

    /// <summary>
    /// 获取标识
    /// </summary>
    function GetSignCode( nDataSign : Int64) : TBytes; override;

    /// <summary>
    /// 获取控制码
    /// </summary>
    function GetControlCode(ADL645Data : TDL645_DATA) : Integer; override;

    /// <summary>
    /// 生成数据包
    /// </summary>
    procedure CreatePacks(var aDatas: TBytes; nCmdType: Integer; ADevice: TObject);override;
  public
    constructor Create; override;
    destructor Destroy; override;

  public


  end;

implementation

{ TProtocolPacksDl645_97 }

constructor TProtocolPacksDl645_97.Create;
begin
  inherited;

end;

function TProtocolPacksDl645_97.CreatePackChangePWD: TBytes;
var
  ACodes : TBytes;
  i: Integer;
begin
  SetLength( Result, GetSendCodeLen );
  ACodes := GetSignCode(FDevice.DataSign);

  //  标识
  for i := 0 to Length(ACodes) - 1 do
    Result[10 + i] := ACodes[i] + $33;

  //  密码
  Result[14] := PWDLevel;
  for i := 0 to 2 do
    Result[15+i] := (ShortPWD shr ((2-i)*8) + $33) and $FF;

  //  数据
  for i := 0 to Length(FDevice.BytesDataValue) - 1 do
    Result[18 + i] := (UserCode shr ((Length(FDevice.BytesDataValue)-i-1)*8) +
      $33) and $FF;
end;

function TProtocolPacksDl645_97.CreatePackClearMaxDemand: TBytes;
begin
  SetLength( Result, GetSendCodeLen );
end;

function TProtocolPacksDl645_97.CreatePackIdentity: TBytes;
begin

end;

function TProtocolPacksDl645_97.CreatePackOnOffControl: TBytes;
begin

end;

function TProtocolPacksDl645_97.CreatePackReadAddr: TBytes;
var
  ACodes : TBytes;
  i: Integer;
begin
  SetLength( Result, GetSendCodeLen );
  ACodes := GetSignCode($9010);

  for i := 0 to Length(ACodes) - 1 do
    Result[10 + i] := ACodes[i] + $33;
end;

function TProtocolPacksDl645_97.CreatePackReadNextData: TBytes;
var
  ACodes : TBytes;
  i: Integer;
begin
  SetLength( Result, GetSendCodeLen );
  ACodes := GetSignCode(FDevice.DataSign);

  for i := 0 to Length(ACodes) - 1 do
    Result[10 + i] := ACodes[i] + $33;
end;

procedure TProtocolPacksDl645_97.CreatePacks(var aDatas: TBytes; nCmdType: Integer;
  ADevice: TObject);
begin
  inherited;

end;

function TProtocolPacksDl645_97.CreatePackSetWOutType: TBytes;
begin

end;

function TProtocolPacksDl645_97.CreatePackWriteData: TBytes;
var
  ACodes : TBytes;
  i: Integer;
begin
  SetLength( Result, GetSendCodeLen );
  ACodes := GetSignCode(FDevice.DataSign);

  //  标识
  for i := 0 to Length(ACodes) - 1 do
    Result[10 + i] := ACodes[i] + $33;

  //  数据
  for i := 0 to Length(FDevice.BytesDataValue) - 1 do
    Result[14 + i] := (UserCode shr ((Length(FDevice.BytesDataValue)-i-1)*8)
      + $33) and $FF;
end;

destructor TProtocolPacksDl645_97.Destroy;
begin

  inherited;
end;

function TProtocolPacksDl645_97.GetBaudRateCode(sBaudRate: string): Integer;
var
  nTemp : Integer;
begin
  TryStrToInt( sBaudRate, nTemp );
  case nTemp of
    600 : Result := 2;
    1200 : Result := 4;
    4800 : Result := 16;
    9600 : Result := 32;
    19200 : Result := 64
  else
    Result := 8;
  end;
end;

function TProtocolPacksDl645_97.GetControlCode(ADL645Data: TDL645_DATA): Integer;
begin
  case FDevice.OrderType of
    C_645_RESET_TIME:      Result := $08;
    C_645_READ_DATA:       Result := $01;
    C_645_READ_NEXTDATA:   Result := $02;
    C_645_READ_ADDR:       Result := $01;
    C_645_WRITE_DATA:      Result := $04;
    C_645_WRITE_ADDR:      Result := $0A;
    C_645_CHANGE_BAUD_RATE: Result := $0C;
    C_645_CHANGE_PWD:      Result := $0F;
    C_645_CLEAR_MAX_DEMAND: Result := $10;
  else
    Result := 0;
  end;
end;

function TProtocolPacksDl645_97.GetSignCode(nDataSign: Int64): TBytes;
begin
  SetLength( Result, 2 );
  Result[1] := nDataSign shr 8;
  Result[0] := nDataSign and $FF;
end;

procedure TProtocolPacksDl645_97.ParseRevPack(APack: TBytes);
  function IsError : Boolean;
  var
    A645Data : TDL645_DATA;
  begin
    Result := False;

    if (APack[8] and $40) = $40 then
    begin
      A645Data := TDL645_DATA.Create;
      A645Data.Assign(FDevice);
      Result := True;

      if APack[10] and $01 = $01 then
        A645Data.RePlyError := de645_07OtherError
      else if APack[10] and $02 = $02 then
        A645Data.RePlyError := de645_07NoneData
      else if APack[10] and $04 = $04 then
        A645Data.RePlyError := de645_07PwdError
      else if APack[10] and $08 = $08 then
        A645Data.RePlyError := de645_07BaudNotChange
      else if APack[10] and $10 = $10 then
        A645Data.RePlyError := de645_07OverYearTme
      else if APack[10] and $20 = $20 then
        A645Data.RePlyError := de645_07OverDayTime
      else if APack[10] and $40 = $40 then
        A645Data.RePlyError := de645_07OverRate;

      FRevList.AddObject('',A645Data);
    end
  end;
begin
  inherited;
  // 是否异常
  if not IsError then
  begin
    case FDevice.OrderType of
      C_645_READ_DATA: ParsePackReadData(APack);
      C_645_READ_NEXTDATA: ParsePackNextData(APack);
      C_645_READ_ADDR: ParsePackReadAddr(APack);
    end;
  end;

  if Assigned(OnRev645Data) then
    OnRev645Data(FRevList);

  ClearStringList(FRevList);
end;

procedure TProtocolPacksDl645_97.ParsePackNextData(APack: TBytes);
begin

end;

procedure TProtocolPacksDl645_97.ParsePackReadAddr(APack: TBytes);
var
  i: Integer;
  A645Data : TDL645_DATA;
begin
  A645Data := TDL645_DATA.Create;
  A645Data.Assign( FDevice );
  A645Data.DataReply := '';

  for i := 0 to 5 do
    A645Data.DataReply := IntToHex(APack[i+1],2) + A645Data.DataReply;

  FRevList.AddObject( '',A645Data );
end;

procedure TProtocolPacksDl645_97.ParsePackReadData(APack: TBytes);
var
  ARev : TBytes;
  nIndex : Integer;
  i: Integer;
  A645Data : TDL645_DATA;
begin
  // 68 49 00 30 07 21 20 68 91 08 33 33 34 33 33 33 33 33 C3 16
  SetLength(ARev, FDevice.DataLen);
  nIndex := 12;
  while nIndex + FDevice.DataLen < Length(APack) do
  begin
    for i := 0 to Length(ARev) - 1 do
      ARev[i] := APack[nIndex + i] - $33;

    FDevice.BytesDataValue := ARev;
    GetRvdPackData;
    A645Data := TDL645_DATA.Create;
    A645Data.Assign( FDevice );

    FRevList.AddObject( '',A645Data );
    nIndex := nIndex + FDevice.DataLen;

    //  数据块用AA分割
    if APack[nIndex] = $AA then
      Inc(nIndex);
  end;
end;

end.



