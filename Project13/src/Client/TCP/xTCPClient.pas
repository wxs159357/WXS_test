unit xTCPClient;

interface

uses System.Types,System.Classes, xFunction, system.SysUtils, xTCPClientBase,
  xConsts, System.IniFiles, xClientType, xVCL_FMX;

type
  TStuReadyEvent = procedure( nTotalCount : Integer) of object;
type
  TStuProgressEvent = procedure( nReadyCount, nTotalCount : Integer) of object;

type
  TTCPClient = class(TTCPClientBase)

  private
    FOnStuReady: TStuReadyEvent;
    FOnStuLogin: TNotifyEvent;
    FOnStopExam: TNotifyEvent;
    FOnStartExam: TNotifyEvent;
    FOnStuProgress: TStuProgressEvent;
    FLoginSeccess : Boolean; // 登录是否成功
    FCanRevData : Boolean; // 是否可以接收数据
    FRevData : TBytes;     // 接收的数据包

    procedure ReadINI;
    procedure WriteINI;

    /// <summary>
    /// 发送命令
    /// </summary>
    procedure SendOrder(aData : TBytes);
    /// <summary>
    /// 解析数据
    /// </summary>
    procedure AnalysisData;
  protected
    /// <summary>
    /// 接收数据包
    /// </summary>
    procedure RevPacksData(aPacks: TArray<Byte>);  override;

  public
    constructor Create; override;
    destructor Destroy; override;

    /// <summary>
    /// 考生登录
    /// </summary>
    function StuLogin(nStuID : Integer) : Boolean;

    /// <summary>
    /// 发送考生状态
    /// </summary>
    procedure SendStuState(AState : TClientState);

    /// <summary>
    /// 学员登录事件
    /// </summary>
    property OnStuLogin : TNotifyEvent read FOnStuLogin write FOnStuLogin;


    /// <summary>
    /// 学员准备事件
    /// </summary>
    property OnStuReady : TStuReadyEvent read FOnStuReady write FOnStuReady;


    /// <summary>
    /// 学员准备事件
    /// </summary>
    property OnStuProgress : TStuProgressEvent read FOnStuProgress write FOnStuProgress;

    /// <summary>
    /// 开始考试事件
    /// </summary>
    property OnStartExam : TNotifyEvent read FOnStartExam write FOnStartExam;

    /// <summary>
    /// 停止考试事件
    /// </summary>
    property OnStopExam : TNotifyEvent read FOnStopExam write FOnStopExam;
//
//    /// <summary>
//    /// 开始考试
//    /// </summary>
//    procedure StartExam(sIP : string = ''; nPort : Integer = 0);
//
//    /// <summary>
//    /// 停止考试
//    /// </summary>
//    procedure StopExam(sIP : string = ''; nPort : Integer = 0);
//
//    /// <summary>
//    /// 返回登录结果
//    /// </summary>
//    procedure LoginResult(sIP : string; nPort : Integer; bResult : Boolean);
  public
//    /// <summary>
//    /// 接收到学员机数据包
//    /// </summary>
//    property OnRevStuData : TRevStuData read FOnRevStuData write FOnRevStuData;
  end;
var
  TCPClient : TTCPClient;

implementation

{ TTCPServer }

procedure TTCPClient.AnalysisData;
var
  aBuf : TBytes;
begin
  aBuf := AnalysisRevData(FRevData);

  if Assigned(aBuf) then
  begin
    // 解析数据
    if Length(aBuf) = 4 then
    begin
      case aBuf[1] of
        1 :
        begin
          if Assigned(FOnStuLogin) then
            FOnStuLogin(Self);
        end;
        4 :
        begin
          if Assigned(FOnStartExam) then
            FOnStartExam(Self);
        end;
        5 :
        begin
          if Assigned(FOnStopExam) then
            FOnStopExam(Self);
        end;
      end;
    end
    else if Length(aBuf) = 5 then
    begin
      if aBuf[1] = 2 then
      begin
        if Assigned(FOnStuReady) then
          FOnStuReady(aBuf[2]);
      end
      else if aBuf[1] = 6 then
      begin
        FLoginSeccess := aBuf[2] = 1;
      end;
    end
    else if Length(aBuf) = 6 then
    begin
      if aBuf[1] = 3 then
      begin
        if Assigned(FOnStuProgress) then
          FOnStuProgress(aBuf[2], aBuf[3]);
      end;
    end;
  end;
end;

constructor TTCPClient.Create;
begin
  inherited;
  ReadINI;
end;

destructor TTCPClient.Destroy;
begin
  WriteINI;
  inherited;
end;

procedure TTCPClient.ReadINI;
begin
  with TIniFile.Create(sPubIniFileName) do
  begin
    ServerIP := ReadString('Option', 'SeverIP', '');
    ServerPort := ReadInteger('Option', 'ServerPort', 15000);
    Free;
  end;
end;

procedure TTCPClient.RevPacksData(aPacks: TArray<Byte>);
  procedure AddData(nData : Byte);
  var
    nLen : Integer;
  begin
    nLen := Length(FRevData);

    SetLength(FRevData, nLen + 1);
    FRevData[nLen] := nData;
  end;
var
  i : Integer;
  nByte : Byte;
begin
  inherited;
  for i := 0 to Length(aPacks) - 1 do
  begin
    nByte := aPacks[i];
    if nByte = $7E then
    begin
      if Length(FRevData) = 0 then
      begin
        FCanRevData := True;
        AddData(nByte);
      end
      else
      begin
        if FRevData[Length(FRevData)-1] = $7E then
        begin
          SetLength(FRevData, 0);

          FCanRevData := True;
          AddData(nByte);
        end
        else
        begin
          AddData(nByte);
          // 转译字符处理
          AnalysisData;
          SetLength(FRevData, 0);
          FCanRevData := False;
        end;

      end;
    end
    else
    begin
      if FCanRevData then
      begin
        AddData(nByte);
      end;
    end;
  end;
end;

procedure TTCPClient.SendOrder(aData: TBytes);
var
  aBuf : TBytes;
begin
  if Active then
  begin
    aBuf := BuildData(aData);

    SendPacksData(aBuf);
  end;
end;

procedure TTCPClient.SendStuState(AState: TClientState);
var
  aBuf : Tbytes;
begin
  SetLength(aBuf, 2);
  aBuf[0] := 7;
  aBuf[1] := Integer(AState);

  SendOrder(aBuf);
end;

function TTCPClient.StuLogin(nStuID: Integer): Boolean;
  function WaitResult( nMSeconds : Cardinal ) : Boolean;
  var
    nTick : Cardinal;
  begin
    nTick := TThread.GetTickCount;

    repeat
      MyProcessMessages;
      Sleep(1);
      Result := FLoginSeccess;
    until (TThread.GetTickCount - nTick  > nMSeconds) or Result;
  end;

var
  aBuf : Tbytes;
begin
  SetLength(aBuf, 4);
  aBuf[0] := 6;
  aBuf[1] := nStuID shr 16 and $FF;
  aBuf[2] := nStuID shr 8 and $FF;
  aBuf[3] := nStuID and $FF;

  FLoginSeccess := False;
  SendOrder(aBuf);
  Result := True;
//  Result := WaitResult(2000);
end;

procedure TTCPClient.WriteINI;
begin
  with TIniFile.Create(sPubIniFileName) do
  begin
    writestring('Option', 'SeverIP', ServerIP);
    WriteInteger('Option', 'ServerPort', ServerPort);
    Free;
  end;
end;

end.
