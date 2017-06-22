unit xUDPServer;

interface

uses System.Types,System.Classes, xFunction, system.SysUtils, xUDPServerBase,
  xConsts, System.IniFiles, xClientType, Winapi.WinInet, winsock;

type
  TUDPServer = class(TUDPServerBase)

  private
    FInfoServerPort: Integer;
    FInfoServerIP: string;

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
    /// 服务器IP
    /// </summary>
    property InfoServerIP : string read FInfoServerIP write FInfoServerIP;

    /// <summary>
    /// 服务器端口
    /// </summary>
    property InfoServerPort : Integer read FInfoServerPort write FInfoServerPort;

    /// <summary>
    /// 连接服务器命令
    /// </summary>
    procedure SendConnServer;
  end;
var
  UDPServer : TUDPServer;

implementation

{ TTCPServer }

constructor TUDPServer.Create;
begin
  inherited;
  ReadINI;
end;

destructor TUDPServer.Destroy;
begin
  WriteINI;
  inherited;
end;

procedure TUDPServer.ReadINI;
  function GetLocalIP(var LocalIp: string): Boolean;

  var
    HostEnt: PHostEnt;
    IP: String;
    Addr: PAnsiChar;
    Buffer: array [0..63] of Char;
    WSData: TWSADATA;
  begin
    Result := False;
    try
      WSAStartUp(2, WSData);
      GetHostName(@Buffer, SizeOf(Buffer));
      // Buffer:='ZhiDa16';
      HostEnt := GetHostByName(@Buffer);
      if HostEnt = nil then
        exit;
      Addr := HostEnt^.h_addr_list^;
      IP := Format('%d.%d.%d.%d', [Byte(Addr[0]), Byte(Addr[1]), Byte(Addr[2]),
        Byte(Addr[3])]);
      LocalIp := IP;
      Result := True;
    finally
      WSACleanup;
    end;
  end;

begin
  with TIniFile.Create(sPubIniFileName) do
  begin
    FInfoServerIP := ReadString('UPDOption', 'ServerIP', '');

    if FInfoServerIP = '' then
      GetLocalIP(FInfoServerIP);

    FInfoServerPort := ReadInteger('UPDOption', 'ServerPort', 15000);

    ListenPort := ReadInteger('UPDOption', 'ListenPort', 16000);
    Free;
  end;
end;

procedure TUDPServer.RevStrData(sIP: string; nPort: Integer; sStr: string);
begin
  inherited;
  if sStr = 'GetServerInfo' then
  begin
    SendPacksDataUDP(sIP, ListenPort+1, FInfoServerIP + ',' + IntToStr(FInfoServerPort));
  end;
end;

procedure TUDPServer.SendConnServer;
begin
  SendPacksDataUDP('255.255.255.255', ListenPort+1, 'ConnectServer');
end;

procedure TUDPServer.WriteINI;
begin
  with TIniFile.Create(sPubIniFileName) do
  begin
    WriteString('UPDOption', 'ServerIP', FInfoServerIP);
    WriteInteger('UPDOption', 'ServerPort', FInfoServerPort);

    Free;
  end;
end;

end.
