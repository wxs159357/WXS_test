unit FrmClientMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, System.ImageList,
  Vcl.ImgList, Vcl.Menus, System.Actions, Vcl.ActnList, Vcl.XPStyleActnCtrls,
  Vcl.ActnMan, Vcl.ToolWin, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Buttons, xConfig,
  xDBConn, xClientControl, xUDPClient1, xClientType, IdBaseComponent,
  IdComponent, IdTCPConnection, IdTCPClient, xFunction, xTCPClient, FrmLogin,
  uStudentInfo, xExerciseControl, FrmExercise, System.IniFiles, xConsts,FrmExamReady,
  xUDPRevScreen, Vcl.Imaging.jpeg, FrmRevScreenMain, FrmExamAnswer, Winapi.WinInet,
  winsock, xDataDictionary, xSortControl ;
const
  WM_FORM_LOADING = WM_USER + 1;
type
  TfClientMain = class(TForm)
    stsbrMain: TStatusBar;
    actnmngrMain: TActionManager;
    actExit: TAction;
    actAbout: TAction;
    actOption: TAction;
    actDevLog: TAction;
    actTraining: TAction;
    actExam: TAction;
    actQuestion: TAction;
    actPaper: TAction;
    actExaminee: TAction;
    actHelp: TAction;
    actMore: TAction;
    actExercise: TAction;
    actRanking: TAction;
    actCommitBug: TAction;
    actExerciseSetting: TAction;
    tmr1: TTimer;
    mnmnmm1: TMainMenu;
    mntmN1: TMenuItem;
    mntmExit: TMenuItem;
    mntmN8: TMenuItem;
    mntmTraining: TMenuItem;
    mntmExercise: TMenuItem;
    mntmRanking2: TMenuItem;
    mntmN2: TMenuItem;
    mntmDevLog: TMenuItem;
    mntmN11: TMenuItem;
    mntmN16: TMenuItem;
    mntmN17: TMenuItem;
    mntmN4: TMenuItem;
    mntmHelpOnAbout: TMenuItem;
    mntmHelp: TMenuItem;
    imglstil3: TImageList;
    imglstil1: TImageList;
    pnl1: TPanel;
    tlbr1: TToolBar;
    pnl2: TPanel;
    imgStu: TImage;
    btnStu: TSpeedButton;
    btnExit: TToolButton;
    tlbtnSavePaper: TToolButton;
    btnHelp: TToolButton;
    btn7: TToolButton;
    idtcpclnt1: TIdTCPClient;
    spltr2: TSplitter;
    pnl3: TPanel;
    spltr1: TSplitter;
    mmo1: TMemo;
    mmo2: TMemo;
    btnExercise: TToolButton;
    pmn1: TPopupMenu;
    mntmN5: TMenuItem;
    img1: TImage;
    tlbtnTraining: TToolButton;
    actSavePaper: TAction;
    procedure actExitExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure actOptionExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tmr1Timer(Sender: TObject);
    procedure btnStuClick(Sender: TObject);
    procedure actExerciseExecute(Sender: TObject);
    procedure mntmN5Click(Sender: TObject);
    procedure pmn1Popup(Sender: TObject);
    procedure actTrainingExecute(Sender: TObject);
  private
    { Private declarations }
    FExamReadyForm : TfExamReady;
    FExamAnswerForm : TfExamAnswer;
    /// <summary>
    /// 系统启动处理
    /// </summary>
    procedure ProcLoadingMsg( var Msg : TMessage ); message WM_FORM_LOADING;
    /// <summary>
    /// 设置公共信息
    /// </summary>
    procedure SetPubInfo;

    /// <summary>
    /// 初始化数据库连接
    /// </summary>
    procedure IniDBConn;

    /// <summary>
    /// 加载各个模块
    /// </summary>
    procedure Load;

    /// <summary>
    /// 卸载各个模块
    /// </summary>
    procedure Unload;

    /// <summary>
    /// 状态改变
    /// </summary>
    procedure StateChange(Sender: TObject);

    procedure TCPConnect(Sender: TObject);
    procedure TCPDisconnect(Sender: TObject);



    procedure TCPLog(const S: string);
    procedure UDPLog(const S: string);
    procedure UDPPacksLog( sIP : string; nPort: Integer; aPacks: TBytes; bSend : Boolean);
    /// <summary>
    /// TCP连接
    /// </summary>
    procedure TCPConn;
    procedure ConnServer(Sender: TObject);

    procedure Login(Sender: TObject);
    procedure StuReady( nTotalCount : Integer);
    procedure StuProgress( nReadyCount, nTotalCount : Integer);
  public
    { Public declarations }
  end;

var
  fClientMain: TfClientMain;

implementation

{$R *.dfm}

procedure TfClientMain.actAboutExecute(Sender: TObject);
begin
  //
end;

procedure TfClientMain.actExerciseExecute(Sender: TObject);
begin
  with TfExercise.Create(nil) do
  begin
    IsClientExercise := True;

    ClientControl.WorkState := cwsPractise;

    ShowModal;
    ClientControl.WorkState := cwsNot;

    Free;
  end;
end;

procedure TfClientMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfClientMain.actOptionExecute(Sender: TObject);
begin
 //
end;

procedure TfClientMain.actTrainingExecute(Sender: TObject);
begin
  with TfRevScreenMain.Create(nil) do
  begin
    ClientControl.WorkState := cwsTrain;

    ShowModal;
    ClientControl.WorkState := cwsNot;
    Free;
  end;
end;

procedure TfClientMain.btnStuClick(Sender: TObject);
begin
  if btnStu.Caption  = '未登录' then
  begin
    Login(nil);
  end
  else
  begin
    with TfStudentInfo.Create(nil) do
    begin
      ShowStu(ClientControl.StudentInfo);
      ShowModal;
      Free;
    end;
  end;
end;

procedure TfClientMain.ConnServer(Sender: TObject);
begin
  TCPConn;
end;

procedure TfClientMain.FormCreate(Sender: TObject);
begin
  stsbrMain.Panels[1].Text := FormatDateTime('YYYY-MM-DD hh:mm:ss', Now);
  SetPubInfo;


  PostMessage( Handle, WM_FORM_LOADING, 0, 0 );
end;

procedure TfClientMain.FormDestroy(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
//    WritesysINI;

    Unload;
    if Assigned(ADBConn) then
      ADBConn.Free;

//    FError.Free;


  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfClientMain.IniDBConn;
begin
  ADBConn := TDBConn.Create;
  if ClientControl.TCPClient.serverIP <> '' then
  begin
    ADBConn.ConnStr := 'DriverID=MSSQL;Database=test;Password=ckm2008byx;'+
      'Server='+ClientControl.TCPClient.serverIP+';User_Name=sa';
  end
  else
  begin
    ADBConn.ConnStr := 'DriverID=MSSQL;Database=test;Password=ckm2008byx;'+
      'Server=127.0.0.1;User_Name=sa';
  end;
end;

procedure TfClientMain.Load;
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
var
  sIP : string;

begin
  try
    if not GetLocalIP(sIP) then
      sIP := '127.0.0.1';

    stsbrMain.Panels[5].Text := '本机：'+sIP;


    UDPClient := TUDPClient.Create;
    UDPClient.OnLog := UDPLog;
    UDPClient.OnIPSendRev := UDPPacksLog;
    UDPClient.Connect;



    ClientControl := TClientControl.Create;
    ClientControl.OnStateChange := StateChange;
    ClientControl.OnConnected := TCPConnect;
    ClientControl.OnDisconnect := TCPDisconnect;
    ClientControl.OnLog := TCPLog;
    ClientControl.OnStuLogin := Login;
    ClientControl.OnStuReady := StuReady;
    ClientControl.OnStuProgress := StuProgress;

    UDPClient.OnConnServer := ConnServer;

    TCPConn;

    if ClientControl.TCPClient.Active then
      ClientControl.ConnState := ccsConned
    else
      ClientControl.ConnState := ccsDisConn;

    IniDBConn;

    SortControl := TSortControl.Create;

    DataDict := TDataDictionary.Create;

    ExerciseControl := TExerciseControl.Create;
    FExamReadyForm := TfExamReady.Create(nil);
    FExamAnswerForm := TfExamAnswer.Create(nil);
    ClientControl.OnStartExam := FExamAnswerForm.StartExam;
    ClientControl.OnStopExam := FExamAnswerForm.StopExam;

    with TIniFile.Create(sPubIniFileName) do
    begin
      if ReadBool('Option', 'InShowLoagin', True) then
      begin
        Login(nil);
      end;
      Free;
    end;

  finally

  end;
end;

procedure TfClientMain.Login(Sender: TObject);
begin
  if ClientControl.LoginState = lsLogOut then
  begin
    with TfStudentLogin.Create(nil) do
    begin
      if ShowModal = mrOk then
      begin

      end;

      Free;
    end;
  end;

end;

procedure TfClientMain.mntmN5Click(Sender: TObject);
begin
  if ClientControl.LoginState = lsLogin then
  begin
    ClientControl.LoginState := lsLogOut;
    ClientControl.StudentInfo.Clear;
  end;
end;

procedure TfClientMain.pmn1Popup(Sender: TObject);
begin
  if ClientControl.LoginState = lsLogin then
  begin
    mntmN5.Visible := True;
  end
  else
  begin

    mntmN5.Visible := False;
  end;
end;

procedure TfClientMain.ProcLoadingMsg(var Msg: TMessage);
begin
  Application.ProcessMessages;

  try
    Enabled := False;
    Load;    // 加载模块
  finally

    Enabled := True;
  end;
end;

procedure TfClientMain.SetPubInfo;
begin

end;

procedure TfClientMain.StateChange(Sender: TObject);
begin
  stsbrMain.Panels[2].Text := ClientConnStateStr(ClientControl.ConnState);
  stsbrMain.Panels[3].Text := LoginStateStr(ClientControl.LoginState);
  stsbrMain.Panels[4].Text := ClientWorkStateStr(ClientControl.WorkState);


  if ClientControl.LoginState = lsLogin then
  begin
    btnStu.Caption := ClientControl.StudentInfo.stuName;
    imgStu.Picture.LoadFromFile('OnLine.bmp');
  end
  else
  begin
    btnStu.Caption := '未登录';
    imgStu.Picture.LoadFromFile('OutLine.bmp');
  end;
end;


procedure TfClientMain.StuProgress(nReadyCount, nTotalCount: Integer);
begin
  if nReadyCount >= nTotalCount then
  begin
    FExamReadyForm.ShowReadyProgress(nReadyCount, nTotalCount);
    FExamReadyForm.Close;

    if nReadyCount > 0 then
    begin
      FExamAnswerForm.Show;
    end;
  end
  else
  begin
    FExamReadyForm.Show;
    FExamReadyForm.ShowReadyProgress(nReadyCount, nTotalCount);
  end;
end;

procedure TfClientMain.StuReady(nTotalCount: Integer);
begin
  FExamReadyForm.ShowReady(nTotalCount);
  FExamReadyForm.Show;
end;

procedure TfClientMain.TCPConn;
  procedure Conn;
  var
    sIP : string;
    nPort : Integer;
  begin
    UDPClient.GetTCPServerIPPort(sIP, nPort, 2000);

    if sIP <> '' then
    begin
      ClientControl.TCPClient.ServerIP := sIP;
      ClientControl.TCPClient.ServerPort := nPort;
      try
        ClientControl.TCPClient.Connect;
      finally

      end;
    end;
  end;
begin
  if not ClientControl.TCPClient.Active then
  begin
    if ClientControl.TCPClient.ServerIP = '' then
    begin
      Conn;
    end
    else
    begin
      try
        ClientControl.TCPClient.Connect;
      finally
        if not ClientControl.TCPClient.Active then
          Conn;
      end;
    end;
  end;
end;

procedure TfClientMain.TCPConnect(Sender: TObject);
begin
  if Assigned(ClientControl) then
  begin
    ClientControl.ConnState := ccsConned;

    if ClientControl.LoginState = lsLogin then
      ClientControl.SendStuLogin(ClientControl.StudentInfo.stuNumber);
  end;
end;

procedure TfClientMain.TCPDisconnect(Sender: TObject);
begin
  if Assigned(ClientControl) then
  begin
    ClientControl.ConnState := ccsDisConn;
  end;
end;

procedure TfClientMain.TCPLog(const S: string);
begin
  if mmo1.Visible then
  begin
    if mmo1.Lines.Count > 1000 then
    begin
      mmo1.Lines.Clear;
      mmo1.Lines.Add('删除前1000条数据记录');
    end;
    mmo1.Lines.Add(s);
  end;
end;

procedure TfClientMain.tmr1Timer(Sender: TObject);
begin
  stsbrMain.Panels[1].Text := FormatDateTime('YYYY-MM-DD hh:mm:ss', Now);
end;

procedure TfClientMain.UDPLog(const S: string);
begin
  if mmo2.Visible then
  begin
    if mmo2.Lines.Count > 1000 then
    begin
      mmo2.Lines.Clear;
      mmo2.Lines.Add('删除前1000条数据记录');
    end;
    mmo2.Lines.Add(s);
  end;
end;

procedure TfClientMain.UDPPacksLog(sIP: string; nPort: Integer; aPacks: TBytes;
  bSend: Boolean);
var
  s : string;
begin
  if bsend then
    s := '发送'
  else
    s := '接收';

  UDPLog(FormatDateTime('hh:mm:ss:zzz', Now) + s + sIP +':' + IntToStr(nPort) + ' ' + PacksToStr(aPacks) );
end;

procedure TfClientMain.Unload;
begin
  ClientControl.Free;
  UDPClient.Free;
  ExerciseControl.Free;
  FExamReadyForm.Free;
  FExamAnswerForm.Free;
  DataDict.Free;
  SortControl.Free;
end;

end.
