{===============================================================================
  TCP客户端通讯基类

===============================================================================}
unit xTCPClientBase;

interface

uses xCommBase, System.Types, xTypes, System.Classes, xFunction,
  system.SysUtils, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdGlobal;

type
  TCheckRevDataProc = reference to procedure(Sender: TObject);

type
  TRevDataThread = class(TThread)
  private
    FCheckRevDataProce : TCheckRevDataProc;
  protected
    procedure Execute; override;
    procedure FormDo;
  public
    constructor Create(CreateSuspended: Boolean; ACheckRevDataProce : TCheckRevDataProc); virtual;
    destructor Destroy; override;
  end;

type
  TTCPClientBase = class(TCommBase)
  private
    FTCPClient: TIdTCPClient;
    FServerPort: Integer;
    FServerIP: string;
    FRevThread : TRevDataThread;
    FOnConnected: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FConnected: Boolean;
    procedure RevData(Sender: TObject);

    procedure TCPConnect(Sender: TObject);
    procedure TCPDisconnect(Sender: TObject);
    procedure SetConnected(const Value: Boolean);

  protected

    /// <summary>
    ///真实发送 串口或以太网发送
    /// </summary>
    function RealSend(APacks: TArray<Byte>; sParam1: string = ''; sParam2 : string=''): Boolean; override;

    /// <summary>
    /// 真实连接
    /// </summary>
    function RealConnect : Boolean; override;

    /// <summary>
    /// 真实断开连接
    /// </summary>
    procedure RealDisconnect; override;

  public
    constructor Create; override;
    destructor Destroy; override;

    /// <summary>
    /// 服务器IP
    /// </summary>
    property ServerIP : string read FServerIP write FServerIP;

    /// <summary>
    /// 服务器端口
    /// </summary>
    property ServerPort : Integer read FServerPort write FServerPort;

    /// <summary>
    ///
    /// </summary>
    property Connected : Boolean read FConnected write SetConnected;
  public
    /// <summary>
    /// 连接事件
    /// </summary>
    property OnConnected : TNotifyEvent read FOnConnected write FOnConnected;

    /// <summary>
    /// 连接事件
    /// </summary>
    property OnDisconnect : TNotifyEvent read FOnDisconnect write FOnDisconnect;
  end;


implementation

{ TTCPClientBase }

constructor TTCPClientBase.Create;
begin
  inherited;
  FRevThread := TRevDataThread.Create(False, RevData);
  FTCPClient:= TIdTCPClient.Create;

//  FTCPClient.OnConnected := TCPConnect;
//  FTCPClient.OnDisconnected := TCPDisconnect;

  FServerIP := '127.0.0.1';
  FServerPort := 10000;
end;

destructor TTCPClientBase.Destroy;
begin
  FTCPClient.Free;
  FRevThread.Free;


  inherited;
end;

function TTCPClientBase.RealConnect: Boolean;
var
  s : string;
begin
  FTCPClient.Host := FServerIP;
  FTCPClient.Port := FServerPort;
  try
    FTCPClient.Connect;
  finally
    Result := FTCPClient.Connected;

    if Result then
      s := '成功'
    else
      s := '失败';
    Log(FormatDateTime('hh:mm:ss:zzz', Now) + ' 连接服务器'+FServerIP + ':' +
      IntToStr(FServerPort)+s);
  end;


end;

procedure TTCPClientBase.RealDisconnect;
begin
  inherited;
  FTCPClient.Disconnect;

  Log(FormatDateTime('hh:mm:ss:zzz', Now) + ' 断开连接'+FServerIP + ':' +
    IntToStr(FServerPort));
end;

function TTCPClientBase.RealSend(APacks: TArray<Byte>; sParam1,sParam2 : string): Boolean;
begin
  Result := False;
  if FTCPClient.Connected then
  begin
    try
      FTCPClient.IOHandler.Write(PacksToStr(APacks));
      Result := True;
    except

    end;
  end;
end;

procedure TTCPClientBase.RevData(Sender: TObject);
var
  nLen : Integer;
  aBuf : TIdBytes;
  i : Integer;
  s : string;
begin
  try
    Connected := FTCPClient.Connected;
  except
    Disconnect;
    Connected := False;
  end;


  try

    if Assigned(FTCPClient) and FTCPClient.Connected then
    begin
      FTCPClient.IOHandler.CheckForDataOnSource;
      nLen := FTCPClient.IOHandler.InputBuffer.Size;

      if nLen > 0 then
      begin
      try
        FTCPClient.Socket.ReadBytes(aBuf, nLen, False);
      except
        Connected := FTCPClient.Connected;
      end;

        s := '';
        for i := 0 to  Length(aBuf) - 1 do
          s := s + Char(aBuf[i]);

        if s <> '' then
        begin
          RevStrData(s);
          RevPacksData(StrToPacks(s))
        end;
      end;

    end;
  except

  end;
end;

procedure TTCPClientBase.SetConnected(const Value: Boolean);
begin
  if Value <>  FConnected then
  begin
    FConnected := Value;

    if Value then
    begin
      TCPConnect(Self);
    end
    else
    begin
      TCPDisconnect(Self);
      Disconnect;
    end;
  end;

end;

procedure TTCPClientBase.TCPConnect(Sender: TObject);
begin
  if Assigned(FOnConnected) then
  begin
    FOnConnected(Self);
  end;
end;

procedure TTCPClientBase.TCPDisconnect(Sender: TObject);
begin
  if Assigned(FOnDisconnect) then
  begin
    FOnDisconnect(Self);
  end;
end;

{ TRevDataThread }

constructor TRevDataThread.Create(CreateSuspended: Boolean;
  ACheckRevDataProce: TCheckRevDataProc);
begin
  inherited Create(CreateSuspended);

  FCheckRevDataProce := ACheckRevDataProce;
end;

destructor TRevDataThread.Destroy;
begin

  inherited;
end;

procedure TRevDataThread.Execute;
begin
  inherited;
  while not Terminated do
  begin
    Synchronize( FormDo);
    Sleep(5);
  end;
end;

procedure TRevDataThread.FormDo;
begin
  if Assigned(FCheckRevDataProce) then
    FCheckRevDataProce(Self);
end;

end.

