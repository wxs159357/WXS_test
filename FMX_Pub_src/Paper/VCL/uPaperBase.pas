unit uPaperBase;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls,DB, ADODB,
  ActnList, XPStyleActnCtrls, ActnMan, Menus, ActnPopup, Buttons, ImgList,
  uPaperInfoBase, frxClass, System.Actions, Vcl.PlatformDefaultStyleActnCtrls, xDBConn,
  xDBActionBase, Vcl.Grids, Vcl.DBGrids, xPaperAction;

type
  TfPaperBase = class(TForm)
    pnlSearch: TPanel;
    ds: TDataSource;
    pctnbr1: TPopupActionBar;
    actmgr1: TActionManager;
    actDetaileInfo: TAction;
    actDelete: TAction;
    actRefresh: TAction;
    mniDetaileInfo: TMenuItem;
    mniDelete: TMenuItem;
    mniRefresh: TMenuItem;
    il1: TImageList;
    actSearch: TAction;
    actClose: TAction;
    spl1: TSplitter;
    actPrint: TAction;
    actPrintview: TAction;
    frxsrdtst1: TfrxUserDataSet;
    frxrprtExam: TfrxReport;
    grpbx1: TGroupBox;
    grpbx2: TGroupBox;
    dtmpckrEnd: TDateTimePicker;
    dtmpckrStart: TDateTimePicker;
    chkDate: TCheckBox;
    lbl3: TLabel;
    lbl2: TLabel;
    lbl1: TLabel;
    grpbx3: TGroupBox;
    btnSearch: TSpeedButton;
    btnDetaileInfo1: TSpeedButton;
    btnDelete1: TSpeedButton;
    btnPrint2: TSpeedButton;
    btnPrintview: TSpeedButton;
    cbbExamnieeName: TComboBox;
    cbbExamName: TComboBox;
    dbgrd1: TDBGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actDetaileInfoExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure lbledtLoginNameKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtNameKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure pctnbr1Popup(Sender: TObject);
    procedure actSearchExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actPrintExecute(Sender: TObject);
    procedure frxrprtExamGetValue(const VarName: string; var Value: Variant);
    procedure actRefreshExecute(Sender: TObject);
  private
  { Private declarations }
    adoquery : TDBActionBase;
    FPaperAction : TPaperAction;

    /// <summary>
    /// 执行查询
    /// </summary>
    procedure DoSearch;

    /// <summary>
    /// 设置显示格式
    /// </summary>
    procedure FormatDisplay;

  protected
    FFormPaperInfo : TfPaperInfoBase;

  public
    { Public declarations }
  end;

var
  fPaperBase: TfPaperBase;

implementation

{$R *.dfm}

procedure TfPaperBase.actCloseExecute(Sender: TObject);
begin
  close;
end;

procedure TfPaperBase.actDeleteExecute(Sender: TObject);
var
  nPID, nPersonID : Integer;
  i : Integer;
begin
  if not adoquery.Query.Active then
    Exit;

  if adoquery.Query.RecordCount =0 then
    Exit;

  if Application.MessageBox('确定要删除记录？', '', MB_OKCANCEL + MB_ICONQUESTION) =
    IDOK then
  begin
    dbgrd1.SelectedRows.CurrentRowSelected := True;

    with dbgrd1.DataSource.DataSet do
    begin
      for i := 0 to dbgrd1.SelectedRows.Count - 1 do
      begin
        GotoBookmark(Pointer(dbgrd1.SelectedRows.Items[i]));

        nPID := dbgrd1.DataSource.DataSet.FieldByName( '考卷编号' ).AsInteger;
        nPersonID := dbgrd1.DataSource.DataSet.FieldByName( 'STUNumber' ).AsInteger;
        APaperAction.DelPaper(nPID, nPersonID);
      end;
    end;

    DoSearch;
  end;
end;

procedure TfPaperBase.actDetaileInfoExecute(Sender: TObject);
begin
  if not Assigned(FFormPaperInfo) then
    FFormPaperInfo := TfPaperInfoBase.Create(nil);

  with adoquery.Query do
  begin
    FFormPaperInfo.ShowPaperInfo(FieldByName('考卷编号').AsInteger,
      FieldByName('STUNumber').AsInteger,
      FieldByName('得分').AsFloat );
  end;

  DoSearch;
end;

procedure TfPaperBase.actPrintExecute(Sender: TObject);
var
  LineCount: Integer;
begin
  LineCount := adoquery.Query.RecordCount;

  if LineCount = 0 then
    Exit;

  frxsrdtst1.RangeEnd := reCount;
  frxsrdtst1.RangeEndCount := LineCount;

  frxrprtExam.ShowReport(True);
end;

procedure TfPaperBase.actRefreshExecute(Sender: TObject);
begin
  DoSearch;
end;

procedure TfPaperBase.actSearchExecute(Sender: TObject);
begin
  DoSearch;
  actDetaileInfo.Enabled := adoquery.Query.RecordCount > 0;
  actDelete.Enabled := adoquery.Query.RecordCount > 0;
end;

procedure TfPaperBase.DoSearch;
const
  C_SEL = 'select b.PaperID as 考卷编号, b.PExamName as 考试名称, '+
          'a.STUName as 考生姓名, a.StuScore as 得分,a.EStartTime as 开始时间, '+
          'a.EStopTime as 结束时间, a.PaperID, b.PaperID, a.STUNumber '+
          'from PSTUInfo a,PExamInfo b where a.PaperID = b.PaperID ';

  procedure ConnnetSQL( var ASQL : string; ACondition : string );
  begin
    if ASQL = '' then
      ASQL :=  ACondition
    else
      ASQL := ASQL + ' and ' + ACondition;
  end;
var
  sCondition : string;
  sExamName : string;
  sExamineeName : string;
  sStartDate, dtEndDate : string;
  sSQL : string;
begin
  sSQL := '';
  sExamineeName := Trim( cbbExamnieeName.Text );
  sExamName := Trim( cbbExamName.Text );

  if chkDate.Checked then
  begin
    sStartDate := FormatDateTime('YYYY-MM-DD', dtmpckrStart.DateTime);
    dtEndDate   := FormatDateTime('YYYY-MM-DD', dtmpckrEnd.DateTime + 1 - 1/SecsPerDay );
    sCondition := ' EStartTime  between :EStartTime and :EStopTime ';

    ConnnetSQL( sSQL, sCondition );
  end;

  if sExamName <> EmptyStr then
  begin
    sCondition := ' PExamName like ' + '''' + '%' + sExamName + '%' + '''' ;
    ConnnetSQL( sSQL, sCondition );
  end;

  if sExamineeName <> EmptyStr then
  begin
    sCondition := ' STUName like ' + '''' + '%' + sExamineeName + '%' + '''';
    ConnnetSQL( sSQL, sCondition );
  end;

  // 考卷编号，考试名称，考生姓名，考生得分，开始时间，结束时间

  if sSQL = '' then
    adoquery.Query.SQL.Text := C_SEL
  else
    adoquery.Query.SQL.Text := C_SEL + ' and ' + sSQL;

  with adoquery.Query.Params do
  begin
    if chkDate.Checked then
    begin
      ParamByName( 'EStartTime' ).Value := sStartDate;
      ParamByName( 'EStopTime'  ).Value := dtEndDate;
    end;
  end;

  adoquery.Query.Open;
  FormatDisplay;
end;

procedure TfPaperBase.edtNameKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 13 then
    DoSearch;
end;

procedure TfPaperBase.FormatDisplay;
begin
  with dbgrd1 do
  begin
    Columns[0].Width := 60;
    Columns[1].Width := 120;
    Columns[2].Width := 100;
    Columns[3].Width := 80;
    Columns[4].Width := 115;
    Columns[5].Width := 115;
    Columns[6].Visible := False;
    Columns[7].Visible := False;
    Columns[8].Visible := False;
  end;
end;

procedure TfPaperBase.FormCreate(Sender: TObject);
begin
  FPaperAction := TPaperAction.Create;
  adoquery := TDBActionBase.Create;
  ds.DataSet := adoquery.Query;
  DoSearch;
end;

procedure TfPaperBase.FormDestroy(Sender: TObject);
begin
  adoquery.Free;
  FPaperAction.Free;
  if Assigned(FFormPaperInfo) then
    FFormPaperInfo.Free;
end;

procedure TfPaperBase.FormShow(Sender: TObject);
begin
  dtmpckrStart.DateTime := Now;
  dtmpckrEnd.DateTime := Now;
  DoSearch;
end;

procedure TfPaperBase.frxrprtExamGetValue(const VarName: string;
  var Value: Variant);
var
  LineNo: Integer;
begin
  LineNo := -1;
  adoquery.Query.First;
  while not adoquery.Query.Eof do
  begin
    Inc(LineNo);
    if LineNo = frxsrdtst1.RecNo then
    begin
      if VarName = 'ExamID' then
        Value := adoquery.Query.FieldByName('考卷编号').AsString
      else if VarName = 'ExamName' then
        Value := adoquery.Query.FieldByName('考试名称').AsString
      else if VarName = 'Name' then
        Value := adoquery.Query.FieldByName('考生姓名').AsString
      else if VarName = 'Mark' then
        Value := adoquery.Query.FieldByName('得分').AsString
      else if VarName = 'StartTime' then
        Value := adoquery.Query.FieldByName('开始时间').AsString
      else if VarName = 'StopTime' then
        Value := adoquery.Query.FieldByName('结束时间').AsString;

    end;

//  C_SEL = 'select b.PaperID as 考卷编号, b.PExamName as 考试名称, '+
//          'a.STUName as 考生姓名, a.EStartTime as 开始时间, '+
//          'a.EStopTime as 结束时间, a.StuScore as 得分,a.PaperID, b.PaperID '+
//          'from PSTUInfo a,PExamInfo b where a.PaperID = b.PaperID ';


    adoquery.Query.Next;
  end;
end;

procedure TfPaperBase.lbledtLoginNameKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 13 then
    DoSearch;
end;

procedure TfPaperBase.pctnbr1Popup(Sender: TObject);
begin
  actDetaileInfo.Enabled := adoquery.Query.RecordCount > 0;
  actDelete.Enabled := adoquery.Query.RecordCount > 0;
end;

end.







