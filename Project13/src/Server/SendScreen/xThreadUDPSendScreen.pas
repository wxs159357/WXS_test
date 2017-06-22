unit xThreadUDPSendScreen;

interface

uses System.Types,System.Classes, xFunction, system.SysUtils, xUDPServerBase,
  xConsts, System.IniFiles, xClientType, Winapi.WinInet, winsock, Graphics,
  xThreadBase, xProtocolSendScreen, Vcl.Imaging.jpeg;

type
  TThreadUDPSendScreen = class(TThreadBase)

  private
    FStream1 : TMemoryStream;
    FStream2 : TMemoryStream;
    FCanClose : Boolean;
    FClose : Boolean;
//    FJpeg : TJPEGImage;
    FIsStream1 : Boolean;
    FOnLog: TGetStrProc;
    FOnGetScreen: TNotifyEvent;

    /// <summary>
    /// 获取要发送的数据流
    /// </summary>
    function GetStream : TMemoryStream;

    procedure ReadINI;
    procedure WriteINI;
    procedure SetSendSecreen(const Value: TJPEGImage);

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
    /// 要发送的屏幕图片
    /// </summary>
    property SendSecreen : TJPEGImage write SetSendSecreen;

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
  FStream2 := TMemoryStream.Create;
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
  FStream2.Free;
  FProtocol.Free;
  WriteINI;
  inherited;
end;

procedure TThreadUDPSendScreen.DisConnect;
begin
  TUDPServerBase(FCommBase).DisConnect;
end;

procedure TThreadUDPSendScreen.ExecuteTimerOrder;
var
  AStream : TMemoryStream;
begin
  if FClose then
    Exit;

//  inherited;
  FCanClose := False;
//  AStream := GetStream;


  Synchronize(GetScreen);


  FStream1.Position := 0;

  if FStream1.Size > 0 then
  begin

    ExecuteOrder(C_SEND_SCREEN, FStream1, '255.255.255.255', 16101);
  end;

  WaitForSeconds(200);
  FCanClose := True;
end;

procedure TThreadUDPSendScreen.GetScreen;
begin
  if Assigned(FOnGetScreen) then
    FOnGetScreen(FStream1);
end;

function TThreadUDPSendScreen.GetStream: TMemoryStream;
var
  s : string;
begin
  if FIsStream1 then
  begin
    Result := FStream1;
    s := s + '取值FStream1';
  end
  else
  begin
    Result := FStream2;
    s := s + '取值FStream2';
  end;

  if Assigned(FOnLog) then
    FOnLog(s);
end;

procedure TThreadUDPSendScreen.ReadINI;
begin
  with TIniFile.Create(sPubIniFileName) do
  begin

    TUDPServerBase(FCommBase).ListenPort := ReadInteger('UPDSendScreen', 'ListenPort', 16100);
    Free;
  end;
end;

procedure TThreadUDPSendScreen.SetSendSecreen(const Value: TJPEGImage);
var
  s : string;
begin
  if FIsStream1 then
  begin
    FStream2.Clear;
    s := s + '赋值FStream2';
    Value.SaveToStream(FStream2);
    FIsStream1 := False;
  end
  else
  begin
    s := s + '赋值FStream1';
    FStream1.Clear;
    Value.SaveToStream(FStream1);
    FIsStream1 := True;
  end;
  if Assigned(FOnLog) then
    FOnLog('==============================');
  if Assigned(FOnLog) then
    FOnLog(s);
end;

procedure TThreadUDPSendScreen.WriteINI;
begin
  with TIniFile.Create(sPubIniFileName) do
  begin
    WriteInteger('UPDSendScreen', 'ListenPort', TUDPServerBase(FCommBase).ListenPort);

    Free;
  end;
end;

end.

