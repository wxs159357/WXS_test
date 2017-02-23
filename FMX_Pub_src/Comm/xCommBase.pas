{===============================================================================
  通讯基类

===============================================================================}
unit xCommBase;

interface

uses System.Types, xTypes, System.Classes, xFunction;

type
  TCommBase = class
  private
    FOnSendRevPack: TSendRevPack;
    FOnError: TGetStrProc;
    FCommBusy: Boolean;
    FActive: Boolean;
    FOnRevPacks: TEnventPack;
    FOnLog: TGetStrProc;
  protected
    /// <summary>
    /// 通讯错误
    /// </summary>
    procedure CommError( sError : string ); virtual;

    /// <summary>
    /// 连接之前函数
    /// </summary>
    procedure BeforeConn; virtual;

    /// <summary>
    /// 连接之后函数
    /// </summary>
    procedure AfterConn; virtual;

    /// <summary>
    /// 断开连接之前函数
    /// </summary>
    procedure BeforeDisConn; virtual;

    /// <summary>
    /// 断开连接之后函数
    /// </summary>
    procedure AfterDisConn; virtual;

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
    /// 接收数据
    /// </summary>
    procedure RevPacksData(aPacks: TArray<Byte>); overload; virtual;
    procedure RevStrData(sStr: string); overload; virtual;

    /// <summary>
    ///真实发送 串口或以太网发送
    /// </summary>
    function RealSend(APacks: TArray<Byte>): Boolean; virtual;

    /// <summary>
    /// 真实连接
    /// </summary>
    function RealConnect : Boolean; virtual;

    /// <summary>
    /// 真实断开连接
    /// </summary>
    procedure RealDisconnect; virtual;

    procedure Log(s : string);

  public
    constructor Create; virtual;
    destructor Destroy; override;

    /// <summary>
    /// 发送数据
    /// </summary>
    function SendPacksData(APacks: TArray<Byte>): Boolean; overload; virtual;
    function SendPacksData(sStr: string): Boolean; overload; virtual;

    /// <summary>
    /// 连接
    /// </summary>
    procedure Connect;

    /// <summary>
    /// 断开连接
    /// </summary>
    procedure Disconnect;
  public
    /// <summary>
    /// 是否忙
    /// </summary>
    property CommBusy : Boolean read FCommBusy write FCommBusy;

    /// <summary>
    /// 串口是否打开 TCP是否连接上
    /// </summary>
    property Active : Boolean read FActive;

    /// <summary>
    /// 接收数据包事件
    /// </summary>
    property OnRevPacks : TEnventPack read FOnRevPacks write FOnRevPacks;

    /// <summary>
    /// 收发数据包事件
    /// </summary>
    property OnSendRevPack : TSendRevPack read FOnSendRevPack write FOnSendRevPack;

    /// <summary>
    /// 错误事件
    /// </summary>
    property OnError: TGetStrProc read FOnError write FOnError;

    /// <summary>
    /// 事件记录
    /// </summary>
    property OnLog : TGetStrProc read FOnLog write FOnLog;

  end;

implementation

{ TCommBase }

procedure TCommBase.AfterConn;
begin

end;

procedure TCommBase.AfterDisConn;
begin

end;

procedure TCommBase.AfterRev;
begin

end;

procedure TCommBase.AfterSend;
begin
  FCommBusy := False;
end;

procedure TCommBase.BeforeConn;
begin

end;

procedure TCommBase.BeforeDisConn;
begin

end;

procedure TCommBase.BeforeRev;
begin

end;

procedure TCommBase.BeforeSend;
begin
  FCommBusy := True;
end;

procedure TCommBase.CommError(sError: string);
begin
  if Assigned(FOnError) then
    FOnError(sError);
end;

procedure TCommBase.Connect;
begin
  BeforeConn;

  try
    FActive := RealConnect;
  except

  end;

  AfterConn;
end;

constructor TCommBase.Create;
begin
  FCommBusy := False;
  FActive := False;
end;

destructor TCommBase.Destroy;
begin

  inherited;
end;

procedure TCommBase.Disconnect;
begin
  BeforeDisConn;

  RealDisconnect;

  AfterDisConn;
end;

procedure TCommBase.Log(s: string);
begin
  if Assigned(FOnLog) then
    FOnLog(s);
end;

function TCommBase.RealConnect : Boolean;
begin
  Result := True;
end;

procedure TCommBase.RealDisconnect;
begin
  FActive := False;
end;

function TCommBase.RealSend(APacks: TArray<Byte>): Boolean;
begin
  Result := True;
end;

procedure TCommBase.RevPacksData(aPacks: TArray<Byte>);
begin
  if Assigned(FOnSendRevPack) then
    FOnSendRevPack(APacks, False);

  if Assigned(FOnRevPacks) then
    FOnRevPacks(aPacks);
end;

procedure TCommBase.RevStrData(sStr: string);
begin

end;

function TCommBase.SendPacksData(sStr: string): Boolean;
begin
  Result := SendPacksData(StrToPacks(sStr));
end;

function TCommBase.SendPacksData(APacks: TArray<Byte>): Boolean;
begin
  BeforeSend;

  Result := RealSend(APacks);

  if Result and Assigned(FOnSendRevPack) then
    FOnSendRevPack(APacks, True);

  AfterSend;
end;

end.
