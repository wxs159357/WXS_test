unit xExamControl;

interface

uses xClientInfo, System.SysUtils, System.Classes, xFunction, System.IniFiles,
  xTCPServer, xStudentInfo, xConsts, xClientType, xStudentControl, Vcl.Dialogs,
  Vcl.Forms, Windows;

type
  /// <summary>
  /// 控制类
  /// </summary>
  TExamControl = class
  private
    FClientList: TStringList;
    FColCount: Integer;
    FOnClinetChanged: TNotifyEvent;
//    FTCPServer : TTCPServer;
    FExamStuList: TStringList;
    FOnTCPLog: TGetStrProc;
    FExamStartTime: TDateTime;
    FIsExamStart: Boolean;

    function GetClientInfo(nIndex: Integer): TClientInfo;
    procedure SetClinetCount(const Value: Integer);
    function GetClinetCount: Integer;

    procedure ReadINI;
    procedure WriteINI;

    procedure ClinetChanged(Sender: TObject);
    procedure LoginEvent(Sender: TObject; nStuID : Integer);
    procedure ExamReady(Sender: TObject);
    procedure RevPacksData(sIP: string; nPort :Integer;aPacks: TArray<Byte>);
    procedure ClientChange( AIP: string; nPort: Integer; AConnected: Boolean );
    function GetTrainClientCount: Integer;


//    procedure TCPPacksLog( sIP : string; nPort: Integer; aPacks: TArray<Byte>; bSend : Boolean);
//    procedure TCPLog(const S: string);
  public
    constructor Create;
    destructor Destroy; override;

//    /// <summary>
//    /// TCP通讯对象
//    /// </summary>
//    property TCPServer : TTCPServer read FTCPServer write FTCPServer;

    /// <summary>
    /// 客户端数量
    /// </summary>
    property ClinetCount : Integer read GetClinetCount write SetClinetCount;

    /// <summary>
    /// 培训状态的客户端列表
    /// </summary>
    property TrainClientCount : Integer read GetTrainClientCount;

    /// <summary>
    /// 列表
    /// </summary>
    property ClientList : TStringList read FClientList write FClientList;
    property ClientInfo[nIndex:Integer] : TClientInfo read GetClientInfo;

    /// <summary>
    /// 获取客户端
    /// </summary>
    function GetClient(sIP : string; nPort : Integer) : TClientInfo;

    /// <summary>
    /// 显示的列数
    /// </summary>
    property ColCount : Integer read FColCount write FColCount;

    /// <summary>
    /// 参加考试的考生列表
    /// </summary>
    property ExamStuList : TStringList read FExamStuList write FExamStuList;

    /// <summary>
    /// 添加考生
    /// </summary>
    procedure AddStu(AStu : TStudentInfo);

    /// <summary>
    /// 考试开始时间
    /// </summary>
    property ExamStartTime : TDateTime read FExamStartTime write FExamStartTime;

    /// <summary>
    /// 考试是否开始
    /// </summary>
    property IsExamStart : Boolean read FIsExamStart write FIsExamStart;

    /// <summary>
    /// 考试开始
    /// </summary>
    procedure ExamStart;

    /// <summary>
    /// 考试停止
    /// </summary>
    procedure ExamStop;

  public
    /// <summary>
    /// 改变事件
    /// </summary>
    property OnClinetChanged : TNotifyEvent read FOnClinetChanged write FOnClinetChanged;

    /// <summary>
    /// TCP通讯数据记录
    /// </summary>
    property OnTCPLog : TGetStrProc read FOnTCPLog write FOnTCPLog;

  end;
var
  ExamControl : TExamControl;
implementation

{ TExamControl }

procedure TExamControl.AddStu(AStu: TStudentInfo);
begin
  if Assigned(AStu) then
  begin
    FExamStuList.AddObject(IntToStr(AStu.stuNumber), AStu);
  end;
end;

procedure TExamControl.ClientChange(AIP: string; nPort: Integer;
  AConnected: Boolean);
var
  AClientInfo : TClientInfo;
begin
  AClientInfo := GetClient(AIP, nPort);

  if Assigned(AClientInfo) then
  begin
    if AConnected then
      AClientInfo.ClientState := esConned
    else
      AClientInfo.ClientState := esDisconn
  end
  else
  begin
    if AConnected then
    begin
      Application.MessageBox(PWideChar(AIP + '连接服务器，在客户机列表中没有找到对应记录！'), '提示', MB_OK +
        MB_ICONINFORMATION);
    end;
  end;
end;

procedure TExamControl.ClinetChanged(Sender: TObject);
begin
  if Assigned(FOnClinetChanged) then
  begin
    FOnClinetChanged(Sender);
  end;
end;

constructor TExamControl.Create;
begin
  FClientList := TStringList.Create;
  FExamStuList:= TStringList.Create;
  TCPServer.OnRevStuData := RevPacksData;
  TCPServer.OnClientChange := ClientChange;
//  FTCPServer := TTCPServer.Create;
//  FTCPServer.OnIPSendRev := TCPPacksLog;
//  FTCPServer.OnLog := TCPLog;
//  FTCPServer.Connect;

  FIsExamStart := False;

  ReadINI;
end;

destructor TExamControl.Destroy;
begin
  WriteINI;
//  FTCPServer.Free;

  ClearStringList(FClientList);
  FClientList.Free;
  FExamStuList.Free;
  inherited;
end;

procedure TExamControl.ExamReady(Sender: TObject);
var
  i : Integer;
  nTotal, nReadyCount : Integer;
  AInfo : TClientInfo;
begin
  with TClientInfo(Sender) do
  begin
    ClientState := esWorkReady;



  end;
  nTotal := 0;
  nReadyCount := 0;
  for i := 0 to FClientList.Count - 1 do
  begin
    AInfo := ClientInfo[i];

    if AInfo.ClientState in [esLogin, esWorkReady] then
      Inc(nTotal);

    if AInfo.ClientState = esWorkReady then
    begin
      Inc(nReadyCount);
    end;
  end;

  TCPServer.SendProgress(nReadyCount, nTotal);
end;

procedure TExamControl.ExamStart;
begin
  FIsExamStart := True;
  FExamStartTime := Now;
end;

procedure TExamControl.ExamStop;
begin
  FIsExamStart := False
end;

function TExamControl.GetClient(sIP: string; nPort: Integer): TClientInfo;
var
  i : Integer;
begin
  Result := nil;
  for i := 0 to FClientList.Count - 1 do
  begin
    with TClientInfo(FClientList.Objects[i]) do
    begin
      if (ClientIP = sIP) then
      begin
        Result := TClientInfo(FClientList.Objects[i]);
        Break;
      end;
    end;
  end;

end;

function TExamControl.GetClientInfo(nIndex: Integer): TClientInfo;
begin
  if (nIndex >= 0) and (nIndex < FClientList.Count) then
  begin
    Result := TClientInfo(FClientList.Objects[nIndex]);
  end
  else
  begin
    Result := nil;
  end;
end;

function TExamControl.GetClinetCount: Integer;
begin
  Result := FClientList.Count;
end;

function TExamControl.GetTrainClientCount: Integer;
var
  i : Integer;
begin
  Result := 0;
  for i := 0 to FClientList.Count - 1 do
  begin
    if TClientInfo(FClientList.Objects[i]).ClientState = esTrain then
    begin
      Inc(Result);
    end;
  end;
end;

procedure TExamControl.LoginEvent(Sender: TObject; nStuID: Integer);
//var
//  nIndex : Integer;
begin
  with TClientInfo(Sender) do
  begin
//    nIndex := FExamStuList.IndexOf(IntToStr(nStuID));
//    TCPServer.LoginResult(ClientIP, ClientPort, nIndex <> -1);
//
//    if nIndex <> -1 then
//    begin
//      StudentInfo.Assign(TStudentInfo(FExamStuList.Objects[nIndex]));
//    end;
    TCPServer.LoginResult(ClientIP, ClientPort, True);
    StudentInfo.Assign(StudentControl.SearchStu(nStuID));
    ClientState := esLogin;
  end;
end;

procedure TExamControl.ReadINI;
var
  s : string;
  i : Integer;
  AStuInfo : TStudentInfo;
  nSN : Integer;
begin
  with TIniFile.Create( spubFilePath + 'OptionClient.ini') do
  begin
    FColCount := ReadInteger('Option', 'ClientColCount', 6);
    ClinetCount := ReadInteger('Option', 'ClientCount', 30);

    s := ReadString('Exam', 'ExamStuList', '');

    GetPartValue(FExamStuList,s, ',');

    for i := FExamStuList.Count - 1 downto 0 do
    begin
      TryStrToInt(FExamStuList[i], nSN);
      AStuInfo := StudentControl.SearchStu(nSN);
      if Assigned(AStuInfo) then
      begin
        FExamStuList.Objects[i] := AStuInfo;
      end
      else
      begin
        FExamStuList.Delete(i);
      end;
    end;



    Free;
  end;
end;

procedure TExamControl.RevPacksData(sIP: string; nPort: Integer;
  aPacks: TArray<Byte>);
var
  AClientInfo : TClientInfo;
begin
  AClientInfo := GetClient(sIP, nPort);

  if Assigned(AClientInfo) then
  begin
    AClientInfo.RevPacksData(sIP, nPort, aPacks);
  end;
end;

procedure TExamControl.SetClinetCount(const Value: Integer);
var
  i: Integer;
  AClientInfo : TClientInfo;
begin
  if Value > FClientList.Count then
  begin
    // 添加
    for i := FClientList.Count to Value - 1 do
    begin
      AClientInfo := TClientInfo.Create;
      AClientInfo.ClientSN := i+1;
      AClientInfo.OnChanged := ClinetChanged;
      AClientInfo.OnStuLogin := LoginEvent;
      AClientInfo.OnStuReady := ExamReady;

      FClientList.AddObject('', AClientInfo);
    end;
  end
  else if Value < FClientList.Count then
  begin
    for i := FClientList.Count - 1 downto Value do
    begin
      FClientList.Objects[i].Free;
      FClientList.Delete(i);
    end;
  end;
end;


//procedure TExamControl.TCPLog(const S: string);
//begin
//  if Assigned(FOnTCPLog) then
//    FOnTCPLog(s);
//end;
//
//procedure TExamControl.TCPPacksLog(sIP: string; nPort: Integer; aPacks: TArray<Byte>;
//  bSend: Boolean);
//var
//  s : string;
//begin
//  if bsend then
//    s := '发送'
//  else
//    s := '接收';
//
//  if Assigned(FOnTCPLog) then
//    FOnTCPLog(FormatDateTime('hh:mm:ss:zzz', Now) + s + sIP +':' + IntToStr(nPort) + ' ' + BCDPacksToStr(aPacks) );
//end;

procedure TExamControl.WriteINI;
var
  s : string;
  i : Integer;
begin
  with TIniFile.Create(spubFilePath + 'OptionClient.ini') do
  begin
    WriteInteger('Option', 'ClientColCount', FColCount);
    WriteInteger('Option', 'ClientCount', ClinetCount);

    s := '';
    for i := 0 to FExamStuList.Count - 1 do
      s := s + FExamStuList[i] + ',';

    WriteString('Exam', 'ExamStuList', s);
    Free;
  end;
end;

end.


