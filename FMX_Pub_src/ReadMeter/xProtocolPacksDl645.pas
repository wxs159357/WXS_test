unit xProtocolPacksDl645;

interface

uses System.Types, xProtocolPacks, System.Classes, system.SysUtils, xDL645Type,
  xFunction, xMeterDataRect;

type
  /// <summary>
  /// 数据包发送事件 TDL645_DATA
  /// </summary>
  TGet645Data = procedure( A645data : TStringList ) of object;


type
  /// <summary>
  /// DL645命令包
  /// </summary>
  TDL645_DATA  = class
  private
    FOrderType: Integer;
    FDataSign: Int64;
    FDataFormat: string;
    FDataReply: string;
    FDataSend: string;
    FDataLen: Integer;
    FAddress: string;
    FDateTimeValue: TDateTime;
    FBlockNum: Integer;
    FDataPackSN: Integer;
    FRePlyError: TDL645_07_ERR;
    FFreezeType: TDL645_07_FREEZE_TYPE;
    FClearEventTYPE: TDL645_07_CLEAREVENT_TYPE;
    FMeterDataGroupName: string;
    FBytesDataValue: TBytes;
    FDataNote: string;
    FMeterProtocolType : TDL645_PROTOCOL_TYPE;
    FReadLoadRecord: Integer;
//    function GetBytesDataValue: TBytes;
    procedure SetBytesDataValue(const Value: TBytes);
    procedure SetMeterDataGroupName(const Value: string);
    procedure SetMeterProtocolType(const Value: TDL645_PROTOCOL_TYPE);
  public
    constructor Create;
    procedure Assign(Source: TObject);
    procedure AssignMeterItem(Source: TObject);

    /// <summary>
    /// 命令类型
    /// </summary>
    property OrderType : Integer read FOrderType write FOrderType;

    /// <summary>
    /// 通信地址
    /// </summary>
    property Address : string read FAddress write FAddress;

    /// <summary>
    /// 数据标识
    /// </summary>
    property DataSign : Int64 read FDataSign write FDataSign;

    /// <summary>
    /// 块数
    /// </summary>
    property BlockNum : Integer read FBlockNum write FBlockNum;

    /// <summary>
    /// 时间（冻结时间，负荷记录块时间）
    /// </summary>
    property DateTimeValue : TDateTime read FDateTimeValue write FDateTimeValue;

    /// <summary>
    /// 读取负荷记录 （0 非读取负荷记录，1读取负荷记录块数，2读取给定时间的负荷记录块）
    /// </summary>
    property ReadLoadRecord: Integer read FReadLoadRecord write FReadLoadRecord;

    /// <summary>
    /// 冻结类型
    /// </summary>
    property FreezeType : TDL645_07_FREEZE_TYPE read FFreezeType write FFreezeType;

    /// <summary>
    /// 后续帧序号
    /// </summary>
    property DataPackSN : Integer read FDataPackSN write FDataPackSN;

    /// <summary>
    /// 数据格式
    /// </summary>
    property DataFormat : string read FDataFormat write FDataFormat;

    /// <summary>
    /// 数据长度
    /// </summary>
    property DataLen : Integer read FDataLen write FDataLen;

    /// <summary>
    /// 数据描述
    /// </summary>
    property DataNote : string read FDataNote write FDataNote;

    /// <summary>
    /// 发送的数据（通讯地址、通讯速率，密码权限,多功能口输出类型）
    /// </summary>
    property DataSend : string read FDataSend write FDataSend;

    /// <summary>
    /// 数据包格式的值
    /// </summary>
    property BytesDataValue : TBytes read FBytesDataValue write SetBytesDataValue;

    /// <summary>
    /// 接收到的数据（通讯地址、各种表格）
    /// </summary>
    property DataReply : string read FDataReply write FDataReply;

    /// <summary>
    /// 清除事件类型
    /// </summary>
    property ClearEventTYPE : TDL645_07_CLEAREVENT_TYPE read FClearEventTYPE
      write FClearEventTYPE;

    /// <summary>
    /// 返回错误
    /// </summary>
    property RePlyError : TDL645_07_ERR read FRePlyError write FRePlyError;

    /// <summary>
    /// 表组名称
    /// </summary>
    property MeterDataGroupName:string read FMeterDataGroupName write SetMeterDataGroupName;

    /// <summary>
    /// 协议类型
    /// </summary>
    property MeterProtocolType : TDL645_PROTOCOL_TYPE read FMeterProtocolType write SetMeterProtocolType;
  end;

type
  TProtocolPacksDl645 = class(TProtocolPacks)
  private
    FAddrWildcard: Byte;
    FUserCode: Integer;
    FIsUseWildcard: Boolean;
    FUnifyAddSign: Byte;
    FLongPWD: Integer;
    FAddrFillSign: Byte;
    FOnRev645Data: TGet645Data;
    function GetPWDLevel: Byte;
    function GetShortPWD: Integer;
    procedure SetPWDLevel(const Value: Byte);
    procedure SetShortPWD(const Value: Integer);
  protected
    FDevice  : TDL645_DATA;         // 电表数据项
    FRevList : TStringList;
    FRev645Data : TBytes;  //  接收临时数据包
    FIsStartRev : Boolean; //  是否开始接收
    nLength : Integer;   // 数据包的数据长度

    /// <summary>
    /// 获取波特率标识
    /// </summary>
    function GetBaudRateCode( sBaudRate : string ) : Integer; virtual; abstract;

    /// <summary>
    /// 获取发送命令包长度
    /// </summary>
    function GetSendCodeLen : Integer; virtual;
    /// <summary>
    /// 获取地址
    /// </summary>
    function GetAddrCode(ADL645Data : TDL645_DATA) : TBytes; virtual;

    /// <summary>
    /// 获取控制码
    /// </summary>
    function GetControlCode(ADL645Data : TDL645_DATA) : Integer; virtual;abstract;

    /// <summary>
    /// 获取标识
    /// </summary>
    function GetSignCode( nDataSign : Int64) : TBytes; virtual;abstract;

    /// <summary>
    /// 提取电表返回数据包中的数据
    /// </summary>
    procedure GetRvdPackData;

    /// <summary>
    ///  处理接收数据包
    /// </summary>
    procedure ProcRvdData(RvdPack : TBytes); virtual;

    /// <summary>
    /// 解析接收到的数据包
    /// </summary>
    procedure ParseRevPack( APack : TBytes ); virtual;

    /// <summary>
    /// 生成数据包
    /// </summary>
    procedure CreatePacks(var aDatas: TBytes; nCmdType: Integer; ADevice: TObject);override;
  public
    constructor Create; override;
    destructor Destroy; override;

    /// <summary>
    /// 解析接收的数据包
    /// </summary>
    function RevPacks(nCmdType: Integer; ADevice: TObject; APack: TBytes): Integer;override;

    /// <summary>
    /// 接收数据事件
    /// </summary>
    property OnRev645Data : TGet645Data read FOnRev645Data write FOnRev645Data;
  public
    /// <summary>
    /// 地址填充字符（$00,$99，$AA）
    /// </summary>
    property AddrFillSign : Byte read FAddrFillSign write FAddrFillSign;

    /// <summary>
    /// 通配符($AA)
    /// </summary>
    property AddrWildcard : Byte read FAddrWildcard write FAddrWildcard;

    /// <summary>
    /// 广播地址标识 $99，$AA）
    /// </summary>
    property UnifyAddSign : Byte read FUnifyAddSign write FUnifyAddSign;

    /// <summary>
    /// 长密码(包括权限和密码)
    /// </summary>
    property LongPWD : Integer read FLongPWD write FLongPWD;

    /// <summary>
    /// 短密码
    /// </summary>
    property ShortPWD : Integer read GetShortPWD write SetShortPWD;

    /// <summary>
    /// 权限
    /// </summary>
    property PWDLevel : Byte read GetPWDLevel write SetPWDLevel;

    /// <summary>
    /// 操作者代码
    /// </summary>
    property UserCode : Integer read FUserCode write FUserCode;

    /// <summary>
    /// 是否用通配符 true 用通配符 地址为AA或99
    /// </summary>
    property IsUseWildcard : Boolean read FIsUseWildcard write FIsUseWildcard;

  end;

implementation

{ TProtocolPacksDl645 }

constructor TProtocolPacksDl645.Create;
begin
  inherited;
  FRevList := TStringList.Create;
  FIsUseWildcard := False;
  FIsStartRev := False;

  FAddrFillSign := $00;
  FAddrWildcard := $AA;
  FUnifyAddSign := $99;
  FLongPWD := $02000000;
  FUserCode := $12345678;
end;

procedure TProtocolPacksDl645.CreatePacks(var aDatas: TBytes; nCmdType: Integer;
  ADevice: TObject);
var
  nLen : Integer;
  ABytes : TBytes;
  i: Integer;
  ADL645Data : TDL645_DATA;
begin
  if not Assigned(ADevice) then
    Exit;

  SetLength(FRev645Data, 0);

  if ADevice is TDL645_DATA then
  begin
    FDevice := TDL645_DATA(ADevice);
    ADL645Data := TDL645_DATA(ADevice);

    case ADL645Data.OrderType of
      C_645_RESET_TIME:      aDatas := CreatePackRsetTime;
      C_645_READ_DATA:       aDatas := CreatePackReadData;
      C_645_READ_NEXTDATA:   aDatas := CreatePackReadNextData;
      C_645_READ_ADDR:       aDatas := CreatePackReadAddr;
      C_645_WRITE_DATA:      aDatas := CreatePackWriteData;
      C_645_WRITE_ADDR:      aDatas := CreatePackWriteAddr;
      C_645_FREEZE:         aDatas := CreatePackFreeze;
      C_645_CHANGE_BAUD_RATE: aDatas := CreatePackChangeBaudRate;
      C_645_CHANGE_PWD:      aDatas := CreatePackChangePWD;
      C_645_CLEAR_MAX_DEMAND: aDatas := CreatePackClearMaxDemand;
      C_645_CLEAR_RDATA:      aDatas := CreatePackClearData;
      C_645_CLEAR_REVENT:     aDatas := CreatePackClearEvent;
      C_SET_WOUT_TYPE:       aDatas := CreatePackSetWOutType;
      C_645_IDENTITY:       aDatas := CreatePackIdentity;
      C_645_ONOFF_CONTROL:   aDatas := CreatePackOnOffControl;

    else
      aDatas := nil;
    end;

    if aDatas <> nil then
    begin
      nLen := High( aDatas );
      // 包头$68
      aDatas[0] := $68;
      // 地址
      ABytes := GetAddrCode(ADL645Data);
      for i := 0 to Length(ABytes) - 1 do
        aDatas[i + 1] := ABytes[i];
      // 包头$68
      aDatas[7] := $68;
      // 控制码
      aDatas[8] := GetControlCode(ADL645Data);
      //  长度
      aDatas[9] := Length(aDatas) - 12;
      // 校验码
      aDatas[ nLen - 1 ] := CalCS( aDatas, 0, nLen - 2 );
      // 包尾 $16
      aDatas[ nLen ]     := $16;

      SetLength(aDatas, Length(aDatas) + 4);

      for i := Length(aDatas) - 1 downto 4   do
      begin
        aDatas[i] := aDatas[i-4];
      end;
      aDatas[0] := $FE;
      aDatas[1] := $FE;
      aDatas[2] := $FE;
      aDatas[3] := $FE;
    end;
  end;
end;

destructor TProtocolPacksDl645.Destroy;
begin
  ClearStringList(FRevList);
  FRevList.Free;
  inherited;
end;

function TProtocolPacksDl645.GetAddrCode(ADL645Data : TDL645_DATA): TBytes;
var
  i: Integer;
begin
  SetLength(Result, 6);

  case ADL645Data.OrderType of
    C_645_RESET_TIME:
    begin
      for i := 0 to Length(Result) - 1 do
        Result[i] := UnifyAddSign;
    end;
    C_645_READ_ADDR, C_645_WRITE_ADDR:
    begin
      if ADL645Data.FMeterProtocolType = dl645pt2007 then
      begin
        for i := 0 to Length(Result) - 1 do
          Result[i] := AddrWildcard;
      end
      else
      begin
        for i := 0 to Length(Result) - 1 do
          Result[i] := UnifyAddSign;
      end;
    end;
  else
    begin
      if FIsUseWildcard then
      begin
        if ADL645Data.MeterProtocolType = dl645pt2007 then
        begin
          for i := 0 to Length(Result) - 1 do
            Result[i] := AddrWildcard;
        end
        else
        begin
          for i := 0 to Length(Result) - 1 do
            Result[i] := UnifyAddSign;
        end;
      end
      else
      begin
        for i := 0 to Length(Result) - 1 do
        begin
          if Length(ADL645Data.Address) >= i * 2 + 2 then
          begin
            Result[5-i] := StrToInt('$' + Copy(ADL645Data.Address,i*2+1, 2));
          end
          else
          begin
            Result[5-i] := $00;
          end;
        end;
      end;
    end;
  end;
end;

function TProtocolPacksDl645.GetPWDLevel: Byte;
begin
  Result := FLongPWD shr 24;
end;

procedure TProtocolPacksDl645.GetRvdPackData;
  procedure ChangeStrSize( var s : string; nLen : Integer );
  var
    i : Integer;
  begin
    if Length( s ) < nLen  then
    begin
      for i := 1 to nLen - Length( s ) do
        s :=  s + '0';
    end
    else if Length( s ) > nLen then
    begin
      // 多个时段信息 时 下面错误
      s := Copy( s, 1, nLen );
    end;
  end;
var
  i: Integer;
  sDataReplay : string;
  sDataReplayFinal : string;
  nFormatLen , nFormatCount, nRvdDataLen: Integer;
  j: Integer;
  bFu : Boolean;
begin
  sDataReplay := '';
  sDataReplayFinal := '';
  bFu := False;

  with FDevice do
  begin
    if (FOrderType = C_645_READ_DATA) or (FOrderType = C_645_READ_NEXTDATA) then
    begin
      if Length(FBytesDataValue) > 0 then
      begin
        nRvdDataLen := Length(FBytesDataValue);

        if Length( FDataFormat ) = 0 then
          nFormatLen := FDataLen
        else
          nFormatLen := Round(Length( FDataFormat )/2);

        nFormatCount := Round(nRvdDataLen/ nFormatLen );

        for i := 0 to nRvdDataLen - 1 do
        begin
          if (i = nRvdDataLen -1)and (BytesDataValue[i] and $80 = $80) then
          begin
            bFu := True;
            sDataReplay := IntToHex(BytesDataValue[i]-$80, 2) + sDataReplay;
          end
          else
          begin
            sDataReplay := IntToHex(BytesDataValue[i], 2) + sDataReplay;
          end;
        end;

        if nFormatLen <> 0 then
        begin
          for j := 0 to nFormatCount -1 do
          begin
            sDataReplayFinal := Copy(sDataReplay, j * nFormatLen * 2 +1,
              nFormatLen * 2) + sDataReplayFinal
          end;
        end;

        ChangeStrSize(sDataReplayFinal,DataLen * 2);

        FDataReply := VerifiedStr(sDataReplayFinal, FDataFormat,DataLen);

        if bFu then
          FDataReply := '-'+FDataReply;
      end;
    end;
  end;
end;

function TProtocolPacksDl645.GetSendCodeLen: Integer;
begin
  case FDevice.OrderType of
    C_645_RESET_TIME:      Result := 18;
    C_645_READ_NEXTDATA:   Result := 14;
    C_645_READ_ADDR:       Result := 14;
    C_645_WRITE_DATA:      Result := 14 + FDevice.DataLen;
    C_645_WRITE_ADDR:      Result := 18;
    C_645_CHANGE_BAUD_RATE: Result := 13;
    C_645_CHANGE_PWD:      Result := 20;
    C_645_CLEAR_MAX_DEMAND: Result := 12;
  else
    Result := 14;
  end;
end;

function TProtocolPacksDl645.GetShortPWD: Integer;
begin
  Result := FLongPWD and $00FFFFFF;
end;

procedure TProtocolPacksDl645.ParseRevPack(APack: TBytes);
begin

end;

procedure TProtocolPacksDl645.ProcRvdData(RvdPack: TBytes);
begin
  // 检查停止位
  if RvdPack[ High( RvdPack ) ] <> $16 then
  begin
//    FReplySate := C_REPLY_PACK_ERROR;
    Exit;
  end;

  // 检查校验码
  if CalCS( RvdPack, 0, High( RvdPack ) - 2 ) <> RvdPack[ High( RvdPack ) - 1 ] then
  begin
//    FReplySate := C_REPLY_PACK_ERROR;
    Exit;
  end;

  //  解析数据包
  ParseRevPack(RvdPack);

//  FReplySate := C_REPLY_CORRECT;
end;

function TProtocolPacksDl645.RevPacks(nCmdType: Integer; ADevice: TObject;
  APack: TBytes): Integer;
  procedure AddRevData( nByte : Byte );
  begin
    if Length(FRev645Data) = 9 then
      nLength := nByte;

    SetLength(FRev645Data, Length(FRev645Data) + 1);
    FRev645Data[Length(FRev645Data)-1] := nByte;
  end;
var
  i: Integer;
  nValue : Byte;

begin
  Result := 0;
//  68 01 00 00 00 00 00 68 81 06 43 C3 A8 99 33 33 05 16
  for i := 0 to Length(APack) - 1 do
  begin
    nValue := APack[i];

    if nValue = $68 then
    begin
      if (Length(FRev645Data)>6)and(FRev645Data[Length(FRev645Data)-7]=$68) and
        (nLength = 0)then
      begin
        nLength := 0;
        AddRevData(nValue);
      end
      else
      begin
        if not FIsStartRev then
        begin
          AddRevData(nValue);
        end
        else
        begin
          SetLength(FRev645Data, 0);
          AddRevData(nValue);
          FIsStartRev := false;
        end;
      end;
    end
    else if (nValue = $16)  then
    begin
      if (Length(FRev645Data)=nLength+11) then
      begin
        AddRevData(nValue);
        ProcRvdData(FRev645Data);
        FIsStartRev := true;
        SetLength(FRev645Data, 0);
      end
      else
      begin
        AddRevData(nValue);
      end;
    end
    else
    begin
      if (nValue = $FE) and (Length(FRev645Data) = 0) then
      begin
        Continue;
      end
      else
      begin
        AddRevData(nValue);
      end;
    end;
  end;
end;

procedure TProtocolPacksDl645.SetPWDLevel(const Value: Byte);
begin
  FLongPWD := (FLongPWD and $00FFFFFF) + (Value shl 24);
end;

procedure TProtocolPacksDl645.SetShortPWD(const Value: Integer);
var
  nValue : Integer;
begin
  nValue := FLongPWD and $FF000000;
  FLongPWD := nValue + (Value and $00FFFFFF);
end;

procedure TDL645_DATA.Assign(Source: TObject);
begin
  Assert( Source is TDL645_DATA );
  FOrderType    := TDL645_DATA( Source ).OrderType;
  FDataSign     := TDL645_DATA( Source ).DataSign;
  FDataFormat   := TDL645_DATA( Source ).DataFormat;
  FDataReply    := TDL645_DATA( Source ).DataReply;
  FDataSend     := TDL645_DATA( Source ).DataSend;
  FDataLen      := TDL645_DATA( Source ).DataLen;
  FAddress      := TDL645_DATA( Source ).Address;
  FRePlyError   := TDL645_DATA( Source ).RePlyError;
  FDateTimeValue:= TDL645_DATA( Source ).DateTimeValue;
  FDataPackSN:= TDL645_DATA( Source ).DataPackSN;
  FBlockNum     := TDL645_DATA( Source ).BlockNum;
  FMeterDataGroupName:= TDL645_DATA(Source).MeterDataGroupName;
  FBytesDataValue := TDL645_DATA( Source ).BytesDataValue;
  FMeterProtocolType := TDL645_DATA( Source ).MeterProtocolType;
  FClearEventTYPE    := TDL645_DATA( Source ).ClearEventTYPE;
  FFreezeType:= TDL645_DATA(Source).FreezeType;
  FReadLoadRecord:= TDL645_DATA(Source).ReadLoadRecord;
end;

procedure TDL645_DATA.AssignMeterItem(Source: TObject);
begin
  Assert( Source is TMeterDataItem );
//  FOrderType    := TMeterDataItem( Source ).OrderType;
  FDataSign     := TMeterDataItem( Source ).Sign;
  FDataFormat   := TMeterDataItem( Source ).Format;
//  FDataReply    := TMeterDataItem( Source ).DataReply;
  FDataSend     := TMeterDataItem( Source ).Value;
  FDataLen      := TMeterDataItem( Source ).Length;
//  FAddress      := TMeterDataItem( Source ).Address;
//  FDateTimeValue:= TMeterDataItem( Source ).DateTimeValue;
//  FBlockNum     := TMeterDataItem( Source ).BlockNum;
end;

constructor TDL645_DATA.Create;
begin
  inherited;
  FOrderType    := 0;
  FDataSign     := 0;
  FDataFormat   := '';
  FDataReply    := '';
  FDataSend     := '';
  FDataLen      := 0;
  FAddress      := '';
  FDateTimeValue:= 0;
  FBlockNum     := 0;
  FMeterDataGroupName:= '';
  FReadLoadRecord := 0;
  FMeterProtocolType := dl645pt2007;
end;

procedure TDL645_DATA.SetBytesDataValue(const Value: TBytes);
begin
  //  数据
  FBytesDataValue := Value;
//  FDataReply := FixDataForShow(Value,FDataFormat, FDataLen);
end;

procedure TDL645_DATA.SetMeterDataGroupName(const Value: string);

begin
  FMeterDataGroupName := Value;
end;

procedure TDL645_DATA.SetMeterProtocolType(const Value: TDL645_PROTOCOL_TYPE);
begin
  FMeterProtocolType := Value;
end;


end.


















