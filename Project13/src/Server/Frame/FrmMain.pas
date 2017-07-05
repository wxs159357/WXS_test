unit FrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ImgList, ActnMan, ActnMenus, ToolWin, StdActns,
  ActnList, XPStyleActnCtrls, ExtCtrls, Buttons,xConsts, IniFiles, Menus,
  jpeg, FrmOption, FrmPaper,
   ActnCtrls,  StdCtrls,
   System.Actions, FrmTrain, FrmExercise, FrmRanking, FrmLog,
  System.ImageList, FrmExam, FrmErrorSelect, xWiringError,
  U_WE_PHASE_MAP, xStudentControl, uStudentList,
  xExerciseControl, FrmPosState, xExamControl, xUDPServer, xFunction, xTCPServer,
  IdContext, IdBaseComponent, IdComponent, IdCustomTCPServer, IdTCPServer,
  FrmExamineeList, xDataDictionary, xUDPServerBase;

const
  WM_FORM_LOADING = WM_USER + 1;

type
  TfMain = class(TForm)
    actmgrMain: TActionManager;
    actHelpOnAbout: TAction;
    actOption: TAction;
    tmr1: TTimer;
    mm1: TMainMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    N4: TMenuItem;
    N11: TMenuItem;
    N16: TMenuItem;
    N17: TMenuItem;
    actDevLog: TAction;
    N18: TMenuItem;
    N24: TMenuItem;
    statMain: TStatusBar;
    actTraining: TAction;
    actExam: TAction;
    actQuestion: TAction;
    actPaper: TAction;
    N15: TMenuItem;
    il3: TImageList;
    actExaminee: TAction;
    actExit: TAction;
    actHelp: TAction;
    il1: TImageList;
    clbr1: TCoolBar;
    mntmN3: TMenuItem;
    mntmExam: TMenuItem;
    actRanking: TAction;
    actCommitBug: TAction;
    mntmRanking: TMenuItem;
    mntmOption: TMenuItem;
    actExerciseSetting: TAction;
    mntmExerciseSetting: TMenuItem;
    mntmHelp: TMenuItem;
    mntmN7: TMenuItem;
    mntmExit: TMenuItem;
    btnSetExaminee: TButton;
    mntmN6: TMenuItem;
    mntmExaminee1: TMenuItem;
    mntmPaper1: TMenuItem;
    mntmRanking2: TMenuItem;
    mntmN8: TMenuItem;
    pnl1: TPanel;
    mmo1: TMemo;
    spltr1: TSplitter;
    mmo2: TMemo;
    spltr2: TSplitter;
    idtcpsrvr1: TIdTCPServer;
    actSetExaminee: TAction;
    btn1: TButton;
    img1: TImage;
    procedure FormDestroy(Sender: TObject);
    procedure actHelpOnAboutExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tmr1Timer(Sender: TObject);
    procedure N16Click(Sender: TObject);
    procedure N17Click(Sender: TObject);
    procedure actTrainingExecute(Sender: TObject);
    procedure actPaperExecute(Sender: TObject);
    procedure actDictExecute(Sender: TObject);
    procedure actDevLogExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actDevSetExecute(Sender: TObject);
    procedure actQuestionExecute(Sender: TObject);
    procedure mntmt1Click(Sender: TObject);
    procedure actExerciseSettingExecute(Sender: TObject);
    procedure actRankingExecute(Sender: TObject);
    procedure actOptionExecute(Sender: TObject);
    procedure actExamExecute(Sender: TObject);
    procedure actExamineeExecute(Sender: TObject);
    procedure actSetExamineeExecute(Sender: TObject);
  private
    { Private declarations }

//    Fullscreen:Tbitmap;
//    AJpeg : TJPEGImage;
//    FUDPServer : TUDPServerBase;

    procedure ReadsysINI;
    procedure WritesysINI;

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

    procedure IniMapColor;

    procedure TCPLog(const S: string);
    procedure UDPLog(const S: string);
    procedure TCPPacksLog( sIP : string; nPort: Integer; aPacks: TBytes; bSend : Boolean);
    procedure UDPPacksLog( sIP : string; nPort: Integer; aPacks: TBytes; bSend : Boolean);
  public
    { Public declarations }
  end;

var
  fMain: TfMain;

implementation

uses  FrmAbout, FrmLoadForm, xDBConn, xSortControl, FrmQuestionListC;

{$R *.dfm}

procedure TfMain.actDevLogExecute(Sender: TObject);
begin
  with TfLog.Create(nil) do
  begin
    ShowModal;
    Free;
  end;
end;

procedure TfMain.actDevSetExecute(Sender: TObject);
begin
  with TfOption.Create(nil) do
  begin
    ShowModal;

    Free;
  end;
end;

procedure TfMain.actDictExecute(Sender: TObject);
begin
//
end;

procedure TfMain.actExamExecute(Sender: TObject);
begin
  with TfExam.Create(nil) do
  begin
    ShowModal;
    Free;
  end;
end;

procedure TfMain.actExamineeExecute(Sender: TObject);
begin
  with TfStudentList.Create(nil) do
  begin
    ShowModal;
    Free;
  end;
end;

procedure TfMain.actExerciseSettingExecute(Sender: TObject);
begin
  with TfExercise.Create(nil) do
  begin
    ShowModal;
    Free;
  end;
end;

procedure TfMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfMain.actHelpOnAboutExecute(Sender: TObject);
begin
  with TfAbout.Create( nil ) do
  begin
    SetVersion('');
    SetCompanyName(C_SYS_COMPANY);
    SetCopyRight('');
    SetWebSite(C_SYS_WEB);
    ShowCode2(spubFilePath + 'MoreInfo.png');

    ShowAboutInfo(C_SYS_OBJECT_MODEL, C_SYS_OBJECT_NAME);
    Free;
  end;
end;

procedure TfMain.actOptionExecute(Sender: TObject);
begin
  with TfOption.Create(nil) do
  begin
    ShowModal;
    Free;
  end;
end;

procedure TfMain.actPaperExecute(Sender: TObject);
begin
  with TfPaper.Create(nil) do
  begin
    ShowModal;
    Free;
  end;
end;

procedure TfMain.actQuestionExecute(Sender: TObject);
begin
  with TfQuestionListC.Create(nil) do
  begin
    ShowModal;
    Free;
  end;
end;

procedure TfMain.actRankingExecute(Sender: TObject);
begin
  with TfRanking.Create(nil) do
  begin
    ShowModal;
    Free;
  end;
end;

procedure TfMain.actSetExamineeExecute(Sender: TObject);
begin
  with TfExamineeList.Create(nil) do
  begin
    ShowModal;
    Free;
  end;
end;

procedure TfMain.actTrainingExecute(Sender: TObject);
begin
  with TfTrain.Create(nil) do
  begin
    ShowModal;
    Free;
  end;
end;

procedure TfMain.FormCreate(Sender: TObject);
begin
  SetPubInfo;
  ReadsysINI;

  pnl1.Visible := bPubIsAdmin;

  IniDBConn;
  Caption := C_SYS_OBJECT_MODEL + '  ' + C_SYS_OBJECT_NAME;
  fLoadform := TfLoadform.Create(Self);

  PostMessage( Handle, WM_FORM_LOADING, 0, 0 );
end;

procedure TfMain.IniMapColor;
begin
  with TIniFile.Create( 'MapColor.ini' ) do
  begin
    with PhaseMapColor do
    begin
      Background         := ReadInteger( 'PhaseMapColor', 'Background', clWhite );
      DotLine            := ReadInteger( 'PhaseMapColor', 'DotLine', $00D1D1D1 );
      PhaseLine          := ReadInteger( 'PhaseMapColor', 'PhaseLine', $005E5E5E );
      PhaseAB            := ReadInteger( 'PhaseMapColor', 'PhaseAB', $003D9DFE );
      PhaseCB            := ReadInteger( 'PhaseMapColor', 'PhaseCB', clRed     );
      PhaseA             := ReadInteger( 'PhaseMapColor', 'PhaseA', $003D9DFE );
      PhaseB             := ReadInteger( 'PhaseMapColor', 'PhaseB', $00009700 );
      PhaseC             := ReadInteger( 'PhaseMapColor', 'PhaseC', clRed     );
      EquationBackground := ReadInteger( 'PhaseMapColor', 'EquationBackground', clWhite   );
      EquationFont       := ReadInteger( 'PhaseMapColor', 'EquationFont', clBlack   );
    end;

    Free;
  end;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
//    Fullscreen.Free;
//    AJpeg.Free;

    WritesysINI;

    Unload;
    ADBConn.Free;

  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfMain.IniDBConn;
begin
  ADBConn := TDBConn.Create;
  ADBConn.ConnStr := 'DriverID=MSSQL;Database=test;Password=ckm2008byx;'+
    'Server=127.0.0.1;User_Name=sa';



end;

procedure TfMain.Load;
begin
  try
    {**********加载模块加在下面************}
    SortControl := TSortControl.Create;
    StudentControl := TStudentControl.Create;
    ExerciseControl := TExerciseControl.Create;

    TCPServer := TTCPServer.Create;
    TCPServer.OnLog := TCPLog;
    TCPServer.OnIPSendRev := TCPPacksLog;
    TCPServer.Connect;

    ExamControl := TExamControl.Create;

    UDPServer := TUDPServer.Create;
    UDPServer.OnLog := UDPLog;
    UDPServer.OnIPSendRev := UdpPacksLog;
    UDPServer.Connect;
    UDPServer.SendConnServer;

    DataDict := TDataDictionary.Create;

    IniMapColor;

    fLoadform.Close;
  finally

  end;
end;

procedure TfMain.mntmt1Click(Sender: TObject);
begin
  Close;
end;

procedure TfMain.N16Click(Sender: TObject);
begin
  clbr1.Visible := N16.Checked;
end;

procedure TfMain.N17Click(Sender: TObject);
begin
  statMain.Visible := N17.Checked;
end;

procedure TfMain.ProcLoadingMsg(var Msg: TMessage);
begin
  Application.ProcessMessages;

  try
    Enabled := False;

    Load;    // 加载模块
  finally

    Enabled := True;
  end;
end;

procedure TfMain.ReadsysINI;
begin

end;

procedure TfMain.SetPubInfo;
begin
  statMain.Panels.Items[1].Text := '系统时间：' +
    formatdatetime('YYYY年MM月DD日 hh:mm:ss',now);
end;

procedure TfMain.TCPLog(const S: string);
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

procedure TfMain.TCPPacksLog(sIP: string; nPort: Integer; aPacks: TBytes;
  bSend: Boolean);
var
  s : string;
begin
  if bsend then
    s := '发送'
  else
    s := '接收';

  TCPLog(FormatDateTime('hh:mm:ss:zzz', Now) + s + sIP +':' + IntToStr(nPort) + ' ' + BCDPacksToStr(aPacks) );
end;

procedure TfMain.tmr1Timer(Sender: TObject);
begin
  statMain.Panels.Items[1].Text := '系统时间：' +
    formatdatetime('YYYY年MM月DD日 hh:mm:ss',now);

end;

procedure TfMain.UDPLog(const S: string);
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

procedure TfMain.UDPPacksLog(sIP: string; nPort: Integer; aPacks: TBytes;
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

procedure TfMain.Unload;
begin
  try

    StudentControl.Free;
    ExerciseControl.Free;
    SortControl.Free;
    ExamControl.Free;
    UDPServer.free;
    TCPServer.Free;
    DataDict.Free;
//    FUDPServer.Free;
  finally

  end;
end;

procedure TfMain.WritesysINI;
begin

end;

end.






