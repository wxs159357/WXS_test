unit xClientControl;

interface

uses System.Classes, System.SysUtils, xClientType, xStudentInfo, xTCPClient,
  xFunction, xDataDictionary;

type
  TClientControl = class
  private
    FStudentInfo: TStudentInfo;
    FStartTime: TDateTime;
    FClientState: TClientState;
    FEndTime: TDateTime;
    FSubList: TStringList;
    FOnStateChange: TNotifyEvent;
    FConnState: TClientConnState;
    FLoginState: TLoginState;
    FWorkState: TClientWorkState;
    FOnStopExam: TNotifyEvent;
    FOnStuReady: TStuReadyEvent;
    FOnStartExam: TNotifyEvent;
    FOnStuProgress: TStuProgressEvent;
    FOnStuLogin: TNotifyEvent;
    FOnConnected: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FTCPClient : TTCPClient;
    FOnLog: TGetStrProc;
    FExamName: string;
    FExamTimes: Integer;

    procedure ReadINI;
    procedure WriteINI;

    procedure StateChange;
    procedure SetClientState(const Value: TClientState);
    procedure SetConnState(const Value: TClientConnState);
    procedure SetLoginState(const Value: TLoginState);
    procedure SetWorkState(const Value: TClientWorkState);

    /// <summary>
    /// 客户端基本状态改变
    /// </summary>
    procedure ClientStateChange;

    procedure TCPConnect(Sender: TObject);
    procedure TCPDisconnect(Sender: TObject);
    procedure StuLogin(Sender: TObject);
    procedure StuReady( nTotalCount : Integer);
    procedure StuProgress( nReadyCount, nTotalCount : Integer);
    procedure StartExam(Sender: TObject);
    procedure StopExam(Sender: TObject);
    procedure TCPLog(const S: string);
    procedure TCPPacksLog(  aPacks: TBytes; bSend : Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// 客户端连接状态
    /// </summary>
    property ConnState : TClientConnState read FConnState write SetConnState;

    /// <summary>
    /// 登录状态
    /// </summary>
    property LoginState : TLoginState read FLoginState write SetLoginState;

    /// <summary>
    /// 工作状态
    /// </summary>
    property WorkState : TClientWorkState read FWorkState write SetWorkState;



    /// <summary>
    /// 客户端状态
    /// </summary>
    property ClientState : TClientState read FClientState write SetClientState;

    /// <summary>
    /// 考生信息
    /// </summary>
    property StudentInfo : TStudentInfo read FStudentInfo write FStudentInfo;

    /// <summary>
    /// 考生的考题列表
    /// </summary>
    property SubList : TStringList read FSubList write FSubList;

    /// <summary>
    /// 开始考试时间
    /// </summary>
    property StartTime : TDateTime read FStartTime write FStartTime;

    /// <summary>
    /// 结束考试时间
    /// </summary>
    property EndTime : TDateTime read FEndTime write FEndTime;

    /// <summary>
    /// 考试名称
    /// </summary>
    property ExamName : string read FExamName write FExamName;

    /// <summary>
    /// 考试时间 分钟
    /// </summary>
    property ExamTimes : Integer read FExamTimes write FExamTimes;

  public
    property TCPClient : TTCPClient read FTCPClient;
    /// <summary>
    /// 考生登录
    /// </summary>
    function SendStuLogin(nStuID : Integer) : Boolean;

    /// <summary>
    /// 发送考生状态
    /// </summary>
    procedure SendStuState(AState : TClientState);

    /// <summary>
    /// 学员机准备考试
    /// </summary>
    procedure StuReadyExam;

  public
    /// <summary>
    /// 状态改变
    /// </summary>
    property OnStateChange : TNotifyEvent read FOnStateChange write FOnStateChange;

    /// <summary>
    /// 连接事件
    /// </summary>
    property OnConnected : TNotifyEvent read FOnConnected write FOnConnected;

    /// <summary>
    /// 断开连接事件
    /// </summary>
    property OnDisconnect : TNotifyEvent read FOnDisconnect write FOnDisconnect;

    /// <summary>
    /// 学员登录事件
    /// </summary>
    property OnStuLogin : TNotifyEvent read FOnStuLogin write FOnStuLogin;

    /// <summary>
    /// 学员准备事件
    /// </summary>
    property OnStuReady : TStuReadyEvent read FOnStuReady write FOnStuReady;

    /// <summary>
    /// 学员准备进度事件
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
    /// <summary>
    /// 通讯记录
    /// </summary>
    property OnLog : TGetStrProc read FOnLog write FOnLog;
  end;
var
  ClientControl : TClientControl;

implementation

{ TClientControl }


procedure TClientControl.ClientStateChange;
begin
  if FConnState = ccsDisConn then
  begin
    ClientState := esDisconn;
    if FLoginState = lsLogin then
    begin
      ClientState := esLogin;
    end
    else
    begin
      ClientState := esConned;
    end;
  end
  else
  begin
    case FWorkState of
      cwsNot :
      begin
        if FLoginState = lsLogin then
        begin
          ClientState := esLogin;
        end
        else
        begin
          ClientState := esConned;
        end;
      end;
      cwsTrain :
      begin
        ClientState := esTrain;
      end;
      cwsPractise:
      begin
        ClientState := esPractise;
      end;
      cwsExamReady:
      begin
        ClientState := esWorkReady;
      end;
      cwsExamDoing:
      begin
        ClientState := esWorking;
      end;
      cwsExamFinished:
      begin
        ClientState := esWorkFinished;
      end;
    end;
  end;
end;

constructor TClientControl.Create;
begin
  FExamName:= '默认考试';
  FExamTimes:= 30;
  FSubList:= TStringList.Create;
  FClientState := esDisconn;
  FStudentInfo:= TStudentInfo.Create;

  FConnState:= ccsDisConn;
  FLoginState:= lsLogOut;
  FWorkState:= cwsNot;

  FTCPClient := TTCPClient.Create;
  FTCPClient.OnConnected := TCPConnect;
  FTCPClient.OnDisconnect := TCPDisconnect;
  FTCPClient.OnLog := TCPLog;
  FTCPClient.OnSendRevPack := TCPPacksLog;
  FTCPClient.OnStuLogin := StuLogin;
  FTCPClient.OnStuReady := StuReady;
  FTCPClient.OnStuProgress := StuProgress;
  FTCPClient.OnStartExam := StartExam;
  FTCPClient.OnStopExam := StopExam;

  ReadINI;
end;

destructor TClientControl.Destroy;
begin
  WriteINI;

  FStudentInfo.Free;
  FTCPClient.Free;
  FSubList.Free;
  inherited;
end;

procedure TClientControl.ReadINI;
begin

end;

function TClientControl.SendStuLogin(nStuID: Integer): Boolean;
begin
  Result := FTCPClient.StuLogin(nStuID);
end;

procedure TClientControl.SendStuState(AState: TClientState);
begin
  FTCPClient.SendStuState(AState);
end;

procedure TClientControl.SetClientState(const Value: TClientState);
begin
  if FClientState <> Value then
  begin
    FClientState := Value;
    StateChange;
    FTCPClient.SendStuState(FClientState);
  end;
end;

procedure TClientControl.SetConnState(const Value: TClientConnState);
begin
  if FConnState <> Value then
  begin
    FConnState := Value;
    ClientStateChange;
  end;
end;

procedure TClientControl.SetLoginState(const Value: TLoginState);
begin
  if FLoginState <> Value then
  begin
    FLoginState := Value;
    ClientStateChange;
    FTCPClient.StuLogin(FStudentInfo.stuNumber);
  end;
end;

procedure TClientControl.SetWorkState(const Value: TClientWorkState);
begin
  if FWorkState <> Value then
  begin
    FWorkState := Value;
    ClientStateChange;
  end;
end;

procedure TClientControl.StartExam(Sender: TObject);
begin
  if Assigned(FOnStartExam) then
  begin
    FStartTime := Now;
    FOnStartExam(Self);
  end;
end;

procedure TClientControl.StateChange;
begin
  if Assigned(FOnStateChange) then
    FOnStateChange(Self);
end;

procedure TClientControl.StopExam(Sender: TObject);
begin
  if Assigned(FOnStopExam) then
  begin
    EndTime := Now;
    FOnStopExam(Self);
  end;
end;

procedure TClientControl.StuLogin(Sender: TObject);
begin
  if Assigned(FOnStuLogin) then
  begin
    FOnStuLogin(Self);
  end;
end;

procedure TClientControl.StuProgress(nReadyCount, nTotalCount: Integer);
begin
  if Assigned(FOnStuProgress) then
  begin
    FOnStuProgress(nReadyCount, nTotalCount);
  end;
end;

procedure TClientControl.StuReady(nTotalCount: Integer);
begin
  if Assigned(FOnStuReady) then
  begin
    FOnStuReady(nTotalCount);

    FSubList.Text := DataDict.Dictionary['考题列表'].Text;
    FExamName := DataDict.Dictionary['考试名称'].Text;
    trystrtoint(DataDict.Dictionary['考试时间'].Text, fexamtimes);

  end;
end;

procedure TClientControl.StuReadyExam;
begin
  FTCPClient.StuReadyExam;
end;

procedure TClientControl.TCPConnect(Sender: TObject);
begin
  if Assigned(FOnConnected) then
  begin
    FOnConnected(Sender);
  end;
end;

procedure TClientControl.TCPDisconnect(Sender: TObject);
begin
  if Assigned(FOnDisconnect) then
  begin
    FOnDisconnect(Sender);
  end;
end;

procedure TClientControl.TCPLog(const S: string);
begin
  if Assigned(FOnLog) then
  begin
    FOnLog(s);
  end;
end;

procedure TClientControl.TCPPacksLog(aPacks: TBytes; bSend: Boolean);
var
  s : string;
begin
  if bsend then
    s := '发送'
  else
    s := '接收';

  TCPLog(FormatDateTime('hh:mm:ss:zzz', Now) + s  + ' ' + BCDPacksToStr(aPacks) );
end;

procedure TClientControl.WriteINI;
begin

end;

end.
