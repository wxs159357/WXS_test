unit xProtocolBase;

interface

uses System.Types, xTypes, xConsts, System.Classes, xFunction, xVCL_FMX,
  system.SysUtils;

type
  /// <summary>
  /// 设备通信协议
  /// </summary>
  TProtocolBase = class
  private
    FOnRevData: TEnventPack;
    FOnSendData: TSendPack;
    FOnLog: TSendRevPack;
    FOrderTimeOut: Cardinal;
    FOnError: TGetStrProc;
    FIsReplay: Boolean;
    FOnGetOrderObject: TOrderObject;
  protected
    FIsStop : Boolean;  // 是否停止运行
    FOrderType : Integer;  // 命令类型
    FDev       : TObject;  // 命令对象
    FRevDataLen    : Integer;  // 接收数据长度
    FRvdPacks  : TBytes;   // 接收数据包
    FReplySate : Integer;  // 数据接收状态

    /// <summary>
    /// 发送接收
    /// </summary>
    procedure CommSenRev( aPacks : TBytes; bSend : Boolean );

    /// <summary>
    /// 发送之前函数
    /// </summary>
    procedure BeforeSend; virtual;

    /// <summary>
    /// 发送之后函数
    /// </summary>
    procedure AfterSend; virtual;

    /// <summary>
    /// 接收之前函数
    /// </summary>
    procedure BeforeRev; virtual;

    /// <summary>
    /// 接收之后函数
    /// </summary>
    procedure AfterRev; virtual;

    /// <summary>
    /// 检查是否有反馈
    /// </summary>
    /// <returns></returns>
    function IsReplied : Boolean;

    /// <summary>
    /// 是否需要返回数据
    /// </summary>
    property IsReplay : Boolean read FIsReplay write FIsReplay;

    /// <summary>
    /// 生成数据包
    /// </summary>
    function CreatePacks: TBytes; virtual;

    /// <summary>
    /// 解析接收到的数据包
    /// </summary>
    procedure ParseData(RvdPack : TBytes); virtual;

    /// <summary>
    /// 检查接收的数据包是否合法
    /// </summary>
    function CheckData(RvdPack : TBytes): Boolean; virtual;

    /// <summary>
    /// 通讯错误
    /// </summary>
    procedure ProtocolError( sError : string );

    /// <summary>
    /// 命令超时
    /// </summary>
    procedure OrderOutTime; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    /// <summary>
    /// 接收数据包
    /// </summary>
    procedure ReceivedData(aPacks: TBytes; sParam1, sParam2 : string); virtual;

    /// <summary>
    /// 发送数据包
    /// </summary>
    function SendData(nOrderType: Integer; ADev: TObject) : Boolean; virtual;

    /// <summary>
    /// 退出
    /// </summary>
    procedure SetExit;

  public
    /// <summary>
    /// 命令超时时间
    /// </summary>
    property OrderTimeOut : Cardinal read FOrderTimeOut write FOrderTimeOut;

    /// <summary>
    /// 发送数据包事件
    /// </summary>
    property OnSendData: TSendPack read FOnSendData write FOnSendData;

    /// <summary>
    /// 接收数据包事件
    /// </summary>
    property OnRevData: TEnventPack read FOnRevData write FOnRevData;

    /// <summary>
    /// 通讯记录
    /// </summary>
    property OnLog : TSendRevPack read FOnLog write FOnLog;

    /// <summary>
    /// 错误事件
    /// </summary>
    property OnError: TGetStrProc read FOnError write FOnError;

    /// <summary>
    /// 根据命令获取对象(设备发送软件应答的命令 必须赋值这个事件)
    /// </summary>
    property OnGetOrderObject : TOrderObject read FOnGetOrderObject write FOnGetOrderObject;
  end;

implementation



procedure TProtocolBase.AfterRev;
begin
  // nothing
//  WaitForSeconds(20);
end;

procedure TProtocolBase.AfterSend;
begin
  // nothing
end;

procedure TProtocolBase.BeforeRev;
begin
  // nothing
end;

procedure TProtocolBase.BeforeSend;
begin
  FRevDataLen    := 0;
  FReplySate := C_REPLY_NORESPONSE;
  SetLength(FRvdPacks, 0);
end;

function TProtocolBase.CheckData(RvdPack: TBytes): Boolean;
begin
  Result := False;
  // nothing
end;

procedure TProtocolBase.CommSenRev(aPacks: TBytes; bSend: Boolean);
begin
  if Length(aPacks) > 0 then
  begin
    if Assigned(FOnLog) then
      FOnLog( aPacks, bSend);

    if bSend then
    begin
      if Assigned(FOnSendData) then
        FOnSendData(aPacks);
    end
    else
    begin
      if Assigned(FOnRevData) then
        FOnRevData(aPacks);
    end;
  end;
end;

constructor TProtocolBase.Create;
begin
  FOrderTimeOut := 1500;
  FIsReplay := True;

end;

function TProtocolBase.CreatePacks: TBytes;
begin
  // nothing
end;

destructor TProtocolBase.Destroy;
begin

  inherited;
end;

function TProtocolBase.IsReplied: Boolean;
var
  nTick : Cardinal;
begin
  nTick := TThread.GetTickCount;

  repeat
    Sleep(3);
    MyProcessMessages;
  until ( FReplySate > C_REPLY_NORESPONSE ) or
        ( TThread.GetTickCount - nTick  > FOrderTimeOut );

  if FReplySate > C_REPLY_NORESPONSE then
    Result := True
  else
    Result := False;

  if not Result then
    OrderOutTime;
end;

procedure TProtocolBase.OrderOutTime;
begin

end;

procedure TProtocolBase.ParseData(RvdPack: TBytes);
begin
  // nothing
end;

procedure TProtocolBase.ProtocolError(sError: string);
begin
  if Assigned(FOnError) then
    FOnError(sError);
end;

procedure TProtocolBase.ReceivedData(aPacks: TBytes; sParam1, sParam2 : string);
begin
  BeforeRev;
  ParseData(aPacks);
  CommSenRev(aPacks, False);

  AfterRev;
end;

function TProtocolBase.SendData(nOrderType: Integer; ADev: TObject): Boolean;
begin
  FOrderType := nOrderType;
  FDev := ADev;

  BeforeSend;
  CommSenRev(CreatePacks, True);
  AfterSend;

  if FIsReplay then
    Result := IsReplied
  else
    Result := True;
end;

procedure TProtocolBase.SetExit;
begin
  FIsStop := True;
end;

end.

