unit uQuestionList;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uSortList, xSortInfo, xSortControl,
  xQuestionInfo, uQuestionInfo, xFunction, System.ImageList, Vcl.ImgList,
  System.Actions, Vcl.ActnList, Vcl.XPStyleActnCtrls, Vcl.ActnMan, Vcl.ComCtrls,
  Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfQuestionList = class(TForm)
    pnlTop: TPanel;
    lbl10: TLabel;
    btnSortManage: TButton;
    cbbSortList: TComboBox;
    lvSubject: TListView;
    pnl3: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    imglstil1: TImageList;
    actnmngractmgr1: TActionManager;
    actAdd: TAction;
    actEdit: TAction;
    actDelete: TAction;
    actClear: TAction;
    pnl2: TPanel;
    btnAdd: TButton;
    btnDelete: TButton;
    btnEdit: TButton;
    btnClear: TButton;
    actLoadSort: TAction;
    procedure actAddExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
    procedure actEditExecute(Sender: TObject);
    procedure actLoadSortExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvSubjectClick(Sender: TObject);
    procedure lvSubjectDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSortManageClick(Sender: TObject);
    procedure cbbSortListChange(Sender: TObject);
  private
    { Private declarations }

  protected
    FSortInfo : TSortInfo;
    FFormInfo : TfQuestionInfo;

    /// <summary>
    /// 创建考题信息界面
    /// </summary>
    function CreateQuestionFrom : TfQuestionInfo; virtual;

    /// <summary>
    /// 加载题库考题
    /// </summary>
    procedure LoadSortQuestions(ASortInfo : TSortInfo);

    /// <summary>
    /// 刷新
    /// </summary>
    procedure RefurshAct;

    /// <summary>
    /// 添加记录
    /// </summary>
    procedure AddRecord(AInfo : TQuestionInfo);

    /// <summary>
    /// 刷新StringGrid纪录
    /// </summary>
    procedure RefurshGrd(nIndex : Integer);

  public
    { Public declarations }


    /// <summary>
    /// 选择一个考题
    /// </summary>
    function SelOneQuestion : TQuestionInfo;
  end;

var
  fQuestionList: TfQuestionList;

implementation

{$R *.dfm}


procedure TfQuestionList.actAddExecute(Sender: TObject);
var
  AInfo : TQuestionInfo;
begin
  if Assigned(FFormInfo) then
  begin
    AInfo := TQuestionInfo.Create;

    FFormInfo.ShowInfo(AInfo);
    if FFormInfo.ShowModal = mrOk then
    begin
      FFormInfo.SaveInfo;

      if Assigned(FSortInfo) then
      begin
        FSortInfo.AddQuestion(AInfo);
        AddRecord(AInfo);
      end;
      lvSubject.ItemIndex := lvSubject.Items.Count - 1;
    end
    else
    begin
      AInfo.Free;
    end;
  end;
  RefurshAct;
end;

procedure TfQuestionList.actLoadSortExecute(Sender: TObject);
var
  i : Integer;
  ASortInfo : TSortInfo;
begin
  cbbSortList.Items.Clear;

  for i := 0 to SortControl.SortList.Count - 1 do
  begin
    ASortInfo := SortControl.SortInfo[i];
    cbbSortList.Items.AddObject(ASortInfo.SortName, ASortInfo);
  end;

  if SortControl.SortList.Count > 0 then
  begin
    cbbSortList.ItemIndex := -1;
    cbbSortList.ItemIndex := 0;
  end;

  cbbSortListChange(nil);
end;

procedure TfQuestionList.actClearExecute(Sender: TObject);
begin
  if HintMessage(1, '您确定清空记录吗?', '提示', hbtYesNo) = hrtYes then
  begin
    lvSubject.Items.Clear;
    FSortInfo.ClearQuestion;
  end;
  RefurshAct;
end;

procedure TfQuestionList.actDeleteExecute(Sender: TObject);
var
  AInfo : TQuestionInfo;
begin
  if lvSubject.ItemIndex = -1 then
    HintMessage(1, '请选择要删除的记录', '提示', hbtOk)
  else
  begin
    if HintMessage(1, '你确定删除吗?', '提示', hbtYesNo) = hrtYes then
    begin
      if lvSubject.ItemIndex <> -1 then
      begin
        AInfo := FSortInfo.QuestionInfo[lvSubject.ItemIndex];
        FSortInfo.DelQuestion(AInfo.QID);
        LoadSortQuestions(FSortInfo);
      end;
    end;
  end;
  RefurshAct;
end;

procedure TfQuestionList.actEditExecute(Sender: TObject);
var
  AInfo : TQuestionInfo;
begin
  if lvSubject.ItemIndex = -1 then
    HintMessage(1, '请选择要修改的记录!', '提示', hbtOk)
  else
  begin
    AInfo := FSortInfo.QuestionInfo[lvSubject.ItemIndex];
    if Assigned(AInfo) then
    begin
      FFormInfo.ShowInfo(AInfo);
      if FFormInfo.ShowModal = mrOk then
      begin
        FFormInfo.SaveInfo;

        FSortInfo.EditQuestion(AInfo);

        RefurshGrd(lvSubject.ItemIndex);
      end;
    end
    else
    begin
      HintMessage(1, '请选择要修改的记录!', '提示', hbtOk)
    end;
  end;
end;

procedure TfQuestionList.AddRecord(AInfo: TQuestionInfo);
begin
  if Assigned(AInfo) and Assigned(FSortInfo) then
  begin
    with lvSubject.Items.Add, AInfo do
    begin
      Caption := IntToStr(AInfo.QID);
      SubItems.Add(AInfo.QName);
      SubItems.Add(AInfo.QDescribe);
      SubItems.Add(AInfo.QRemark1);
    end;
  end;
end;

procedure TfQuestionList.btnSortManageClick(Sender: TObject);
begin
  with TfSortList.Create(Self) do
  begin
    ShowModal;
    Free;
  end;

  actLoadSortExecute(nil);
end;

procedure TfQuestionList.cbbSortListChange(Sender: TObject);
begin
  if cbbSortList.ItemIndex <> -1 then
  begin
    FSortInfo := SortControl.SortInfo[cbbSortList.ItemIndex];
    FSortInfo.LoadQuestion;
    LoadSortQuestions(FSortInfo);
  end;
  RefurshAct;
end;

function TfQuestionList.CreateQuestionFrom: TfQuestionInfo;
begin
  Result := TfQuestionInfo.Create(Self);
end;

procedure TfQuestionList.FormCreate(Sender: TObject);
begin
  FFormInfo := CreateQuestionFrom;
end;

procedure TfQuestionList.FormDestroy(Sender: TObject);
begin
  FFormInfo.Free;
end;

procedure TfQuestionList.FormShow(Sender: TObject);
begin
  actLoadSortExecute(nil);
end;

procedure TfQuestionList.LoadSortQuestions(ASortInfo: TSortInfo);
var
  i : Integer;
begin
  lvSubject.Items.Clear;
  if Assigned(ASortInfo) then
  begin
    for i := 0 to ASortInfo.QuestionList.Count - 1 do
      AddRecord(ASortInfo.QuestionInfo[i]);
  end;
end;

procedure TfQuestionList.lvSubjectClick(Sender: TObject);
begin
  RefurshAct;
end;

procedure TfQuestionList.lvSubjectDblClick(Sender: TObject);
begin
  actEditExecute(nil);
end;

procedure TfQuestionList.RefurshAct;
begin
  actDelete.Enabled := lvSubject.ItemIndex <> -1;
  actEdit.Enabled := lvSubject.ItemIndex <> -1;
  actClear.Enabled := lvSubject.Items.Count > 0;
end;

procedure TfQuestionList.RefurshGrd(nIndex: Integer);
var
  AInfo : TQuestionInfo;
begin
  if (nIndex <> -1) and Assigned(FSortInfo) then
  begin
    AInfo := FSortInfo.QuestionInfo[nIndex];

    if Assigned(AInfo) then
    begin
      with lvSubject.Items[nIndex], AInfo do
      begin
        Caption := IntToStr(AInfo.SortID);
        SubItems[0] := AInfo.QName;
        SubItems[1] := AInfo.QDescribe;
        SubItems[2] := AInfo.QRemark1;
      end;
      lvSubject.SetFocus;
    end;
  end;
end;

function TfQuestionList.SelOneQuestion: TQuestionInfo;
begin
  pnl2.Visible := False;
  pnl3.Visible := True;

  Result := nil;

  if ShowModal = mrOk then
  begin
    if Assigned(FSortInfo) then
    begin
      if lvSubject.Items.Count > 0 then
      begin
        if lvSubject.ItemIndex <> -1 then
        begin
          Result := FSortInfo.QuestionInfo[lvSubject.ItemIndex];
        end;
      end;
    end;
  end;

  pnl2.Visible := True;
  pnl3.Visible := False;
end;


end.
