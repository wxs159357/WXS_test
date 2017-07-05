{===============================================================================
  通讯线程基类

===============================================================================}
unit xThreadBase;

interface

uses System.SysUtils, System.Classes, xProtocolBase,xFunction, xVCL_FMX,
  xCommBase, xSerialBase, xTCPClientBase, xTCPServerBase, xUDPServerBase;

type
  TOrderObject = class
  private
    FOrderType: Integer;
    FOrderObject: TObject;
    FOrderIP: string;
    FOrderPort: Integer;

  public
    /// <summary>
    /// 命令类型
    /// </summary>
    property OrderType : Integer read FOrderType write FOrderType;
    /// <summary>
    /// 命令对象
    /// </summary>
    property OrderObject : TObject read FOrderObject write FOrderObject;

    /// <summary>
    /// 命令IP
    /// </summary>
    property OrderIP : string read FOrderIP write FOrderIP;

    /// <summary>
    /// 命令端口
    /// </summary>
    property OrderPort : Integer read FOrderPort write FOrderPort;

  end;

type
  /// <summary>
  /// 线路类型
  /// </summary>
  TCommType = (ctNot,
               ctSerial,    // 串口
               ctTCPClient, // TCP客户端
               ctTCPServer, // TCP服务器
               ctUDPServer  // UDP服务器
               );

type
  /// <summary>
  /// 通讯线程基类
  /// </summary>
  TThreadBase = class(TThread)
  private
    FTOWaitForSeconds: Cardinal;
    FOrderReplayed: Boolean;
    FProType: TCommType;
    FTimerOrderEnable: Boolean;
    FTimeOrderList: TStringList;
    FTempIndex : Integer;
    FMSeconds : Cardinal;      // 设置定时命令有效后计时用

    FOrderList : TStringList;  // 命令列表

    procedure SetProType(const Value: TCommType);
    procedure SetTimerOrderEnable(const Value: Boolean);
    procedure SetCommBase(const Value: TCommBase);
  protected
    FIsStop : Boolean;  // 是否停止运行
    FProtocol : TProtocolBase;
    FCommBase : TCommBase;     // 通讯线路


    procedure Execute; override;

    /// <summary>
    /// 执行实时命令
    /// </summary>
    procedure ExecuteOrder(ACmdType: Integer; ADevice: TObject; sIP : string; nPort : Integer); virtual;

    /// <summary>
    /// 执行定时命令 （如果定时命令要执行列表需要判断 FIsStop 是佛停止运行，如果挺尸运行需要跳出循环）
    /// </summary>
    procedure ExecuteTimerOrder; virtual;

    /// <summary>
    /// 退出
    /// </summary>
    procedure SetExit;
  public
    constructor Create(CreateSuspended: Boolean); virtual;
    destructor Destroy; override;

    /// <summary>
    /// 线程名称
    /// </summary>
    function TheardName : string; virtual;

    /// <summary>
    /// 通讯线路类型
    /// </summary>
    property ProType : TCommType read FProType write SetProType;

    /// <summary>
    /// 通讯线路
    /// </summary>
    property CommBase : TCommBase read FCommBase write SetCommBase;

    /// <summary>
    /// 执行命令
    /// </summary>
    /// <param name="ACmdType">命令类型</param>
    /// <param name="ADevice">命令对象</param>
    /// <param name="bWairtExecute">是否等待执行完命令才推出函数</param>
    procedure AddOrder( ACmdType: Integer; ADevice: TObject;
      bWairtExecute : Boolean = True; sIP : string = ''; nPort : Integer = 0); virtual;

    /// <summary>
    /// 命令是否执行完
    /// </summary>
    function OrdersFinished : Boolean;

    /// <summary>
    /// 等待命名执行完
    /// </summary>
    function IsWaitReplied( nCmdTimeOut : Cardinal = 1000 ): Boolean;

    /// <summary>
    /// 定时命令是否有效
    /// </summary>
    property TimerOrderEnable: Boolean read FTimerOrderEnable write SetTimerOrderEnable;

    /// <summary>
    /// 定时命令有效之后等待时间 毫秒
    /// </summary>
    property TOWaitForSeconds : Cardinal read FTOWaitForSeconds write FTOWaitForSeconds;

    /// <summary>
    /// 定时命令列表
    /// </summary>
    property TimeOrderList : TStringList read FTimeOrderList write FTimeOrderList;

    /// <summary>
    /// 命令是否通讯正常
    /// </summary>
    property OrderReplayed : Boolean read FOrderReplayed write FOrderReplayed;

    /// <summary>
    /// 是否打开串口 连接到服务器，侦听端口
    /// </summary>
    function IsConned : Boolean;
  end;

implementation

{ TThreadBase }

procedure TThreadBase.AddOrder(ACmdType: Integer; ADevice: TObject;
  bWairtExecute: Boolean;sIP : string; nPort : Integer);
var
  nTimeOut : Integer;
  AOderObject : TOrderObject;
begin
  if IsConned then
  begin
    AOderObject := TOrderObject.Create;
    AOderObject.OrderType   := ACmdType;
    AOderObject.OrderObject := ADevice;
    AOderObject.OrderIP     := sIP;
    AOderObject.OrderPort   := nPort;


    FOrderList.AddObject( IntToStr( Integer(ACmdType) ), AOderObject);

    if bWairtExecute then
    begin
      if Assigned(FProtocol) then
      begin
        nTimeOut := FProtocol.OrderTimeOut;
        IsWaitReplied(nTimeOut);
      end;
    end;
  end;
end;

constructor TThreadBase.Create(CreateSuspended: Boolean);
begin
  inherited;
  FOrderList := TStringList.Create;
  FTimeOrderList := TStringList.Create;
  FTimerOrderEnable := False;
  FTOWaitForSeconds := 3000;
  FIsStop := False;

end;

destructor TThreadBase.Destroy;
begin
  SetExit;
  Terminate;
  FOrderList.Free;
  ClearStringList(FTimeOrderList);
  FTimeOrderList.Free;

  if Assigned(FCommBase) then
    FCommBase.Free;

  inherited;
end;

procedure TThreadBase.Execute;
var
  AOrderObject : TOrderObject;
begin
  inherited;
  while not Terminated do
  begin
    if FMSeconds > 0 then
    begin
      if GetTickCount - FMSeconds > FTOWaitForSeconds then
      begin
        FTimerOrderEnable := True;
        FMSeconds := 0;
      end;
    end;

    Sleep(1);
    MyProcessMessages;

    //  处理所有实时命令
    while FOrderList.Count > 0 do
    begin
      if FIsStop then
        Break;

      if Assigned( FOrderList.Objects[ 0 ] ) then
      begin
        AOrderObject := TOrderObject(FOrderList.Objects[ 0 ]);

        with AOrderObject do
        begin
          ExecuteOrder(OrderType, OrderObject, OrderIP, OrderPort);
        end;

        if FOrderList.Count > 0 then
        begin
          AOrderObject.Free;
          FOrderList.Delete( 0 );
        end;
      end;
    end;

    //  处理定时命令
    if FTimerOrderEnable and not FIsStop then
      ExecuteTimerOrder;

//    Sleep(1);
//    MyProcessMessages;
  end;
end;

procedure TThreadBase.ExecuteOrder(ACmdType: Integer; ADevice: TObject;
  sIP : string; nPort : Integer);
begin
  if Assigned(FProtocol) then
    FOrderReplayed := FProtocol.SendData(ACmdType, ADevice, sIP, IntToStr(nPort))
  else
    FOrderReplayed := False;
end;

procedure TThreadBase.ExecuteTimerOrder;
begin
  if TimeOrderList.Count > FTempIndex then
  begin
    with TOrderObject(TimeOrderList.Objects[FTempIndex]) do
    begin
      if Assigned(FProtocol) and not FIsStop then
        FProtocol.SendData(OrderType, OrderObject, OrderIP, IntToStr(OrderPort));
    end;
  end;

  Inc(FTempIndex);

  if FTempIndex >= TimeOrderList.Count then
  begin
    FTempIndex := 0;
  end;
end;

function TThreadBase.IsConned: Boolean;
begin
  if Assigned(FCommBase) then
    Result := FCommBase.Active
  else
    Result := False;
end;

function TThreadBase.IsWaitReplied(nCmdTimeOut: Cardinal): Boolean;
var
  nTick : Cardinal;
begin
  nTick := GetTickCount;

  repeat
    MyProcessMessages;
    Result := OrdersFinished;
    Sleep(1);
  until Result or ( GetTickCount - nTick  > nCmdTimeOut );
end;

function TThreadBase.OrdersFinished: Boolean;
begin
  Result := FOrderList.Count = 0;
end;

procedure TThreadBase.SetCommBase(const Value: TCommBase);
begin
  FCommBase := Value;
end;

procedure TThreadBase.SetExit;
begin
  FIsStop := True;
  if Assigned(FProtocol) then
    FProtocol.SetExit;
end;

procedure TThreadBase.SetProType(const Value: TCommType);
begin
  FProType := Value;
  case FProType of
    ctSerial :
    begin
      if Assigned(FCommBase) then
      begin
        if FCommBase.ClassName <> TSerialBase.ClassName then
        begin
          FCommBase.OnRevPacks := nil;
          FCommBase.Free;
          FCommBase := TSerialBase.Create;
        end;
      end
      else
      begin
        FCommBase := TSerialBase.Create;
      end;

      // FProtocol 必须先创建
      FProtocol.OnSendData := FCommBase.SendPacksDataBase;
      FCommBase.OnRevPacks := FProtocol.ReceivedData;

    end;
    ctTCPClient:
    begin
      if Assigned(FCommBase) then
      begin
        if FCommBase.ClassName <> TTCPClientBase.ClassName then
        begin
          FCommBase.OnRevPacks := nil;
          FCommBase.Free;
          FCommBase := TTCPClientBase.Create;
        end;
      end
      else
      begin
        FCommBase := TTCPClientBase.Create;
      end;

      FProtocol.OnSendData := FCommBase.SendPacksDataBase;
      FCommBase.OnRevPacks := FProtocol.ReceivedData;





    end;
    ctTCPServer:
    begin
      if Assigned(FCommBase) then
      begin
        if FCommBase.ClassName <> TTCPServerBase.ClassName then
        begin
          FCommBase.OnRevPacks := nil;
          FCommBase.Free;
          FCommBase := TTCPServerBase.Create;
        end;
      end
      else
      begin
        FCommBase := TTCPServerBase.Create;
      end;

      FProtocol.OnSendData := FCommBase.SendPacksDataBase;
      FCommBase.OnRevPacks := FProtocol.ReceivedData;


    end;
    ctUDPServer:
    begin
      if Assigned(FCommBase) then
      begin
        if FCommBase.ClassName <> TUDPServerBase.ClassName then
        begin
          FCommBase.OnRevPacks := nil;
          FCommBase.Free;
          FCommBase := TUDPServerBase.Create;
        end;
      end
      else
      begin
        FCommBase := TUDPServerBase.Create;
      end;

      FProtocol.OnSendData := FCommBase.SendPacksDataBase;
      FCommBase.OnRevPacks := FProtocol.ReceivedData;
    end;
  end;
end;

procedure TThreadBase.SetTimerOrderEnable(const Value: Boolean);
begin
  if Value then
  begin
    FMSeconds := GetTickCount;
  end
  else
  begin
    FMSeconds :=0;
    FTimerOrderEnable := Value;
  end;
end;

function TThreadBase.TheardName: string;
begin
  Result := '匿名线程';
end;

end.
