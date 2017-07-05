unit xThreadUDPSendScreen;

interface

uses System.Types,System.Classes, xFunction, system.SysUtils, xUDPServerBase,
  xConsts, System.IniFiles,  winsock, Graphics,
  xThreadBase, xProtocolSendScreen, Vcl.Imaging.jpeg;

type
  TThreadUDPSendScreen = class(TThreadBase)

  private
    FStream1 : TMemoryStream;
    FCanClose : Boolean;
    FClose : Boolean;
    FOnLog: TGetStrProc;
    FSendPort : Integer; // 发送端口
    FOnGetScreen: TNotifyEvent;


    procedure ReadINI;
    procedure WriteINI;
    procedure GetScreen;

  protected

    /// <summary>
    /// 执行定时命令 （如果定时命令要执行列表需要判断 FIsStop 是佛停止运行，如果挺尸运行需要跳出循环）
    /// </summary>
    procedure ExecuteTimerOrder; override;

  public
    constructor Create(CreateSuspended: Boolean); override;
    destructor Destroy; override;

    procedure Connect;
    procedure DisConnect;

    /// <summary>
    /// 记录时间
    /// </summary>
    property OnLog : TGetStrProc read FOnLog write FOnLog;

    /// <summary>
    /// 获取图像事件
    /// </summary>
    property OnGetScreen : TNotifyEvent read FOnGetScreen write FOnGetScreen;
  end;
var
  UDPSendScreen : TThreadUDPSendScreen;

implementation

{ TTCPServer }

procedure TThreadUDPSendScreen.Connect;
begin
  TUDPServerBase(FCommBase).Connect;
end;

constructor TThreadUDPSendScreen.Create(CreateSuspended: Boolean);
begin
  inherited;
  FStream1 := TMemoryStream.Create;
  FCanClose := True;

  FProtocol := TProtocolSendScreen.Create;
  ProType := ctUDPServer;
  TimerOrderEnable := True;
  FClose := False;
  ReadINI;
end;

destructor TThreadUDPSendScreen.Destroy;
begin
  FClose := True;
  WaitForSeconds(2000);

  repeat
    WaitForSeconds(1);
  until (FCanClose);

  FStream1.Free;
  FProtocol.Free;
  WriteINI;
  inherited;
end;

procedure TThreadUDPSendScreen.DisConnect;
begin
  TUDPServerBase(FCommBase).DisConnect;
end;

procedure TThreadUDPSendScreen.ExecuteTimerOrder;
begin
  if FClose then
    Exit;

  FCanClose := False;

  Synchronize(GetScreen);

  FStream1.Position := 0;

  if FStream1.Size > 0 then
  begin
    ExecuteOrder(C_SEND_SCREEN, FStream1, '255.255.255.255', FSendPort);
  end;

  WaitForSeconds(10);
  FCanClose := True;
end;

procedure TThreadUDPSendScreen.GetScreen;
begin
  if Assigned(FOnGetScreen) then
    FOnGetScreen(FStream1);
end;

procedure TThreadUDPSendScreen.ReadINI;
begin
  with TIniFile.Create(sPubIniFileName) do
  begin
    TUDPServerBase(FCommBase).ListenPort := ReadInteger('UPDSendScreen', 'ListenPort', 16100);
    FSendPort := ReadInteger('UPDSendScreen', 'SendPort', 16101);
    Free;
  end;
end;

procedure TThreadUDPSendScreen.WriteINI;
begin
  with TIniFile.Create(sPubIniFileName) do
  begin
    WriteInteger('UPDSendScreen', 'ListenPort', TUDPServerBase(FCommBase).ListenPort);
    WriteInteger('UPDSendScreen', 'SendPort', FSendPort);
    Free;
  end;
end;

end.

