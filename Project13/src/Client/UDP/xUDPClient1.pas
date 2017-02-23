unit xUDPClient1;

interface

uses System.Types,System.Classes, xFunction, system.SysUtils, xUDPServerBase,
  xConsts, System.IniFiles, xVCL_FMX, FrmSelectTCPServer,
  {$IFDEF MSWINDOWS}
  Winapi.Windows, ShlObj, CommCtrl, Messages,
  {$ENDIF}
  xClientType;

type
  TUDPClient = class(TUDPServerBase)

  private
    FInfoServerPort: Integer;
    FInfoServerIP: string;
    FList : TStringlist;
    bGetInfo : Boolean;
    FOnConnServer: TNotifyEvent;

    procedure ReadINI;
    procedure WriteINI;

  protected
    /// <summary>
    /// 接收数据包
    /// </summary>
    procedure RevStrData(sIP: string; nPort :Integer; sStr: string); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    /// <summary>
    /// 获取TCP服务器IP端口
    /// </summary>
    function GetTCPServerIPPort(var sIP: string; var nPort : Integer; nTimeOut : Cardinal = 1000): Boolean;

    /// <summary>
    /// 连接服务器事件
    /// </summary>
    property OnConnServer : TNotifyEvent read FOnConnServer write FOnConnServer;
  end;
var
  UDPClient : TUDPClient;

implementation
constructor TUDPClient.Create;
begin
  inherited;
  FList := TStringlist.Create;
  ListenPort := 16001;
  bGetInfo := False;
end;

destructor TUDPClient.Destroy;
begin
  FList.free;
  inherited;
end;

function TUDPClient.GetTCPServerIPPort(var sIP: string; var nPort: Integer;
  nTimeOut: Cardinal): Boolean;
var
  nTick : Cardinal;
begin
  nTick := GetTickCount;
  sIP := '';
  nPort := 0;
  FList.Clear;
  bGetInfo := True;
  SendPacksData('255.255.255.255', 16000, 'GetServerInfo');
  repeat
    MyProcessMessages;
    Sleep(1);
  until GetTickCount - nTick  > nTimeOut;



  if FList.Count = 1 then
  begin
    GetTCPIPPort(FList[0], sIP, nPort);

  end
  else if FList.Count = 0 then
  begin

  end
  else
  begin
    with TfSelectTCPServer.Create(nil) do
    begin
      SelectTCPServer(FList, sIP, nPort);
    end;
  end;
  bGetInfo := True;
  Result := sIP <> '';
end;

procedure TUDPClient.ReadINI;
begin

end;

procedure TUDPClient.RevStrData(sIP: string; nPort: Integer; sStr: string);
var
  nIndex : Integer;
begin
  inherited;
  if bGetInfo then
  begin
    nIndex := Pos(',', sStr);

    if nIndex > 0 then
    begin
      FList.Add(sStr);
    end;
  end;

  if sStr = 'ConnectServer' then
  begin
    if Assigned(FOnConnServer) then
    begin
      FOnConnServer(Self);
    end;
  end;
end;

procedure TUDPClient.WriteINI;
begin

end;
end.
