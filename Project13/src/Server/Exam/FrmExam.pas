unit FrmExam;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.ExtCtrls, Vcl.ImgList,
  Vcl.ComCtrls, Vcl.StdCtrls, System.ImageList, System.Actions, Vcl.ActnList,
  Vcl.PlatformDefaultStyleActnCtrls, Vcl.ActnMan, FrmPosState, xExamControl,
  xTCPServer, xDataDictionary, xQuestionInfo, xSortControl, uQuestionList, uExamInfo,
  Vcl.Buttons, FrmQInfoC;

type
  TfExam = class(TForm)
    spltrspl1: TSplitter;
    pnl2: TPanel;
    pnlLastTime: TPanel;
    pnlExamName: TPanel;
    lblExamTime: TLabel;
    imglstil1: TImageList;
    tmr1: TTimer;
    pmnpm1: TPopupMenu;
    mntmN1: TMenuItem;
    mntmN2: TMenuItem;
    mntmN3: TMenuItem;
    imglstil2: TImageList;
    pnlBottom: TPanel;
    btnExamStop: TButton;
    lvSubList: TListView;
    stsbr1: TStatusBar;
    pmn1: TPopupMenu;
    actnmngr1: TActionManager;
    actAddStu: TAction;
    actDelStu: TAction;
    mntmAddStu: TMenuItem;
    mntmDelStu: TMenuItem;
    pmn2: TPopupMenu;
    actAddQuestion: TAction;
    actDelQuestion: TAction;
    mntmDelQuestion: TMenuItem;
    mntmDelQuestion1: TMenuItem;
    pnl4: TPanel;
    actLogin: TAction;
    btnLogin: TButton;
    actReadyExam: TAction;
    btnReadyExam: TButton;
    actExamStart: TAction;
    actExamStop: TAction;
    btnExamStart: TButton;
    pnl3: TPanel;
    btnAddQuestion: TButton;
    btnDelQuestion: TButton;
    actEditExam: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure actLoginExecute(Sender: TObject);
    procedure actReadyExamExecute(Sender: TObject);
    procedure actExamStartExecute(Sender: TObject);
    procedure actExamStopExecute(Sender: TObject);
    procedure actAddQuestionExecute(Sender: TObject);
    procedure actDelQuestionExecute(Sender: TObject);
    procedure actEditExamExecute(Sender: TObject);
    procedure pnlExamNameDblClick(Sender: TObject);
    procedure tmr1Timer(Sender: TObject);
    procedure lvSubListDblClick(Sender: TObject);
  private
    { Private declarations }
    FromPosState : TfPosState;

    procedure RefurshQuestionList;
  public
    { Public declarations }
  end;

var
  fExam: TfExam;

implementation

{$R *.dfm}

procedure TfExam.actAddQuestionExecute(Sender: TObject);
var
  AQInfo : TQuestionInfo;
  slList : TStringList;
begin
  with TfQuestionList.Create(nil) do
  begin
    AQInfo := SelOneQuestion;

    if Assigned(AQInfo) then
    begin
      slList := DataDict.Dictionary['考题列表'];

      if slList.IndexOf(IntToStr(AQInfo.QID)) = -1 then
      begin
        slList.Add(IntToStr(AQInfo.QID));

        RefurshQuestionList;
      end;
    end;
    Free;
  end;
end;

procedure TfExam.actDelQuestionExecute(Sender: TObject);
var
  slList : TStringList;
  nQID : Integer;
begin
  if Assigned(lvSubList.Selected) then
  begin
    if Application.MessageBox('确定要删除记录？', '', MB_OKCANCEL + MB_ICONQUESTION) =
      IDOK then
    begin
      TryStrToInt(lvSubList.Selected.Caption, nQID);
      slList := DataDict.Dictionary['考题列表'];
      if slList.IndexOf(IntToStr(nQID)) <> -1 then
      begin
        slList.Delete(lvSubList.Selected.Index);
        RefurshQuestionList;
      end;
    end;
  end;
end;

procedure TfExam.actEditExamExecute(Sender: TObject);
var
  sExamName: string;
  nExamTime: Integer;
begin
  sExamName := DataDict.Dictionary['考试名称'].Text;
  trystrtoint(DataDict.Dictionary['考试时间'].Text, nExamTime);


  with TfExamInfo.Create(nil) do
  begin
    EditInfo(sExamName, nExamTime);
    DataDict.Dictionary['考试名称'].Text := sExamName;
    DataDict.Dictionary['考试时间'].Text := inttostr(nExamTime);
    pnlExamName.Caption := sExamName;
    lblExamTime.Caption := inttostr(nExamTime);
  end;
end;

procedure TfExam.actExamStartExecute(Sender: TObject);
begin
  ExamControl.ExamStart;

  TCPServer.StartExam;
  tmr1Timer(nil);
end;

procedure TfExam.actExamStopExecute(Sender: TObject);
begin
  TCPServer.StopExam;
  ExamControl.ExamStop;
end;

procedure TfExam.actLoginExecute(Sender: TObject);
begin
  if Assigned(TCPServer) then
    TCPServer.StuLogin;
end;

procedure TfExam.actReadyExamExecute(Sender: TObject);
begin
  TCPServer.StuReady(ExamControl.ExamStuList.Count);
end;

procedure TfExam.FormCreate(Sender: TObject);
begin
  FromPosState := TfPosState.Create(nil);
  FromPosState.Align := alClient;
  FromPosState.Parent := pnl4;
  FromPosState.BorderStyle := bsNone;
  pnlExamName.Caption := DataDict.Dictionary['考试名称'].Text;
  lblExamTime.Caption := DataDict.Dictionary['考试时间'].text;
  RefurshQuestionList;

end;

procedure TfExam.FormDestroy(Sender: TObject);
begin
  DataDict.SaveToDB;
  FromPosState.Free;
end;

procedure TfExam.FormShow(Sender: TObject);
begin
  FromPosState.Show;
end;

procedure TfExam.lvSubListDblClick(Sender: TObject);
begin
  if lvSubList.ItemIndex <> -1 then
  begin
    with TfQInfo.Create(nil) do
    begin
      ShowInfoReadOnly(TQuestionInfo(lvSubList.Items[lvSubList.ItemIndex].Data));
      ShowModal;
      Free;
    end;
  end;

end;

procedure TfExam.pnlExamNameDblClick(Sender: TObject);
begin
  actEditExamExecute(nil);
end;

procedure TfExam.RefurshQuestionList;
var
  slList : TStringList;
  i : Integer;
  AQInfo : TQuestionInfo;
  nID : Integer;
begin
  slList := DataDict.Dictionary['考题列表'];

  lvSubList.Clear;

  for i := 0 to slList.Count - 1 do
  begin
    TryStrToInt(slList[i], nID);
    AQInfo := SortControl.GetQInfo(nID);

    if Assigned(AQInfo) then
    begin
      with lvSubList.Items.Add do
      begin
        Caption := IntToStr(AQInfo.QID);
        SubItems.Add(AQInfo.QName);
        Data := AQInfo;
      end;
    end;
  end;
end;

procedure TfExam.tmr1Timer(Sender: TObject);
var
  nTime : Integer;
  dtTime : TDateTime;
begin
  TryStrToInt(DataDict.Dictionary['考试时间'].text, nTime);

  if ExamControl.IsExamStart then
  begin
    dtTime := ExamControl.ExamStartTime + nTime/MinsPerDay - now;
  end
  else
  begin
    dtTime := nTime/MinsPerDay;
  end;
  pnlLastTime.Caption := FormatDateTime('hh:mm:ss', dtTime);
end;

end.
