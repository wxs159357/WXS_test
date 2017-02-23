unit uStudentList;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  xStudentControl, FMX.Controls.Presentation, FMX.Edit, FMX.ListView.Types,
  FMX.ListView, System.Rtti, FMX.Layouts, FMX.Grid, xStudentInfo,
  System.Actions, FMX.ActnList, uStudentInfo, FMX.Menus, FMX.Grid.Style,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ScrollBox,
  System.ImageList, FMX.ImgList;

type
  TfStudentList = class(TForm)
    pnl1: TPanel;
    edtContent: TEdit;
    btn1: TButton;
    pnl2: TPanel;
    btnAdd: TButton;
    btnDel: TButton;
    btnEdit: TButton;
    strGrdStu: TStringGrid;
    strngclmn1: TStringColumn;
    strngclmn2: TStringColumn;
    strngclmn3: TStringColumn;
    strngclmn4: TStringColumn;
    strngclmn5: TStringColumn;
    strngclmn6: TStringColumn;
    strngclmn7: TStringColumn;
    strngclmn8: TStringColumn;
    strngclmn9: TStringColumn;
    actList: TActionList;
    actAdd: TAction;
    actDel: TAction;
    actUpdate: TAction;
    actSearch: TAction;
    actSearchAll: TAction;
    actClearAll: TAction;
    btn3: TButton;
    pm1: TPopupMenu;
    MenuItem1: TMenuItem;
    MeuDel: TMenuItem;
    MeuUpdate: TMenuItem;
    pnl3: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    actStuImput: TAction;
    actStuOutPut: TAction;
    btn4: TButton;
    btn5: TButton;
    il1: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure actAddExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actDelExecute(Sender: TObject);
    procedure actUpdateExecute(Sender: TObject);
    procedure actSearchExecute(Sender: TObject);
    procedure actSearchAllExecute(Sender: TObject);
    procedure actClearAllExecute(Sender: TObject);
    procedure actStuImputExecute(Sender: TObject);
    procedure actStuOutPutExecute(Sender: TObject);
    procedure pm1Popup(Sender: TObject);
  private
    { Private declarations }
    FFrmStudentInio : TFrmStudentInfo;

    /// <summary>
    /// 刷新
    /// </summary>
    procedure RefurshAct;

    /// <summary>
    /// StringGrid 双击   单机事件
    /// </summary>
    procedure GridClick(Sender: TObject);
    procedure GridDblClick(Sender: TObject);
  public
    { Public declarations }
    /// <summary>
    /// 显示考生列表
    /// </summary>
    procedure ShowStus(slList : TStringList);

    /// <summary>
    /// 添加学员信息
    /// </summary>
    procedure AddRecord(AStrInfo : TStudentInfo);

    /// <summary>
    /// 刷新StringGrid纪录
    /// </summary>
    procedure RefurshGrd(nIndex : Integer);

    /// <summary>
    /// 选择一个考生
    /// </summary>
    function SelOneStu : TStudentInfo;
  end;

var
  fStudentList: TfStudentList;

implementation

uses
  xStudentTabelOutOrIn, xFunction;

{$R *.fmx}

procedure TfStudentList.actAddExecute(Sender: TObject);
var
  AStuInfo : TStudentInfo;
begin
  if Assigned(FFrmStudentInio) then
  begin
    AStuInfo := TStudentInfo.Create;

    FFrmStudentInio.ShowStu(AStuInfo);
    if FFrmStudentInio.ShowModal = mrOk then
    begin
      FFrmStudentInio.SaveStu;

      if Assigned(StudentControl) then
      begin
        StudentControl.AddStu(AStuInfo);
        AddRecord(AStuInfo);
      end;
    end
    else
    begin
      AStuInfo.Free;
    end;
  end;
  RefurshAct;
end;

procedure TfStudentList.actClearAllExecute(Sender: TObject);
begin
  if HintMessage(1, '您确定清空吗?', '提示', hbtYesNo) = hrtYes then
  begin
    StudentControl.ClearStus;
    ShowStus(StudentControl.StuList);
  end;

  RefurshAct;
end;

procedure TfStudentList.actDelExecute(Sender: TObject);
var
  nValue, nStuNumber : integer;
begin
  nValue := strGrdStu.Selected;
  if nValue = -1 then
    HintMessage(1, '请选择要删除的记录', '提示', hbtOk)
  else
  begin
    if HintMessage(1, '你确定删除吗?', '提示', hbtYesNo) = hrtYes then
    begin
      if (strGrdStu.RowCount > 0) and (strGrdStu.Cells[0, nValue] <> '') then
      begin
        nStuNumber := StrToInt(strGrdStu.Cells[0, nValue]);
        if StudentControl.DelStu(nStuNumber) then
        begin
          ShowStus(StudentControl.StuList);
        end;
      end
      else
      begin
        HintMessage(1, '请选择要删除的记录', '提示', hbtOk);
      end;
    end;
  end;

  RefurshAct;
end;

procedure TfStudentList.actSearchAllExecute(Sender: TObject);
begin
  ShowStus(StudentControl.StuList);
  RefurshAct;
end;

procedure TfStudentList.actSearchExecute(Sender: TObject);
var
  sContent : string;
  slList : TStringList;
begin
  sContent := Trim(edtContent.Text);

  if sContent = '' then
  begin
    ShowStus(StudentControl.StuList);
  end
  else
  begin
    slList := TStringList.Create;
    StudentControl.SearchStus(sContent, slList);
    ShowStus(slList);
    slList.Free;
  end;

  RefurshAct;
end;

procedure TfStudentList.actStuImputExecute(Sender: TObject);
var
  AStuOutOrIn : TStudentTableOption;
  slList : TStringList;
  nResult, i : Integer;
begin
  Cursor := crHourGlass;
  AStuOutOrIn := TStudentTableOption.Create;
  slList := TStringList.Create;
  nResult := AStuOutOrIn.ImportStu(slList);

  if nresult = 0 then   /// * 导入失败会怎么样
    HintMessage(1, '记录录入成功!', '提示', hbtOk)
  else if nresult = 1 then
    HintMessage(1, '记录录入失败!', '提示', hbtOK)
  else
    Exit;

  for i := 0 to slList.Count - 1 do
  begin
    StudentControl.AddStu(TStudentInfo(slList.Objects[i]));
    AddRecord(TStudentInfo(slList.Objects[i]));
  end;

  slList.Free;
  AStuOutOrIn.Free;
  RefurshAct;
  Cursor:=crDefault;
end;

procedure TfStudentList.actStuOutPutExecute(Sender: TObject);
var
  i: Integer;
  slList : TStringList;
  AStuExcel: TStudentTableOption;
  nResult: Integer;
  nRows : Integer;
begin
  nRows := strGrdStu.RowCount;
  if nRows > 0 then
  begin
    Cursor := crHourGlass;

    AStuExcel :=  TStudentTableOption.Create;
    slList := TStringList.Create;

    // 获取选择的学员列表
    for i := 0 to nRows - 1 do
    begin
      slList.AddObject('',StudentControl.SearchStu(strGrdStu.Cells[0, i].ToInteger));
    end;

    // 把列表到处到文件
    nResult := AStuExcel.ExportStu(slList);
    if nResult = 0 then
      HintMessage(1, '所有记录导出成功!', '提示', hbtOk)
    else if nResult = 1 then
      HintMessage(1, '所有记录导出失败!', '提示', hbtOk)
    else
      Exit;

    slList.Free;
    AStuExcel.free;
    RefurshAct;
    Cursor := crDefault;
  end;
end;

procedure TfStudentList.actUpdateExecute(Sender: TObject);
var
  nValue, nStuNumber : integer;
  AStuInfo : TStudentInfo;
begin
  nValue := strGrdStu.Selected;
  if nValue = -1 then
    HintMessage(1, '请选择要修改的记录!', '提示', hbtOk)
  else
  begin
    TryStrToInt(strGrdStu.Cells[0, nValue], nStuNumber);
    AStuInfo := StudentControl.SearchStu(nStuNumber);
    if Assigned(AStuInfo) then
    begin
      FFrmStudentInio.ShowStu(AStuInfo);
      if FFrmStudentInio.ShowModal = mrOk then
      begin
        FFrmStudentInio.SaveStu;

        StudentControl.EditStu(AStuInfo);

        RefurshGrd(nValue);
      end;
    end
    else
    begin
      HintMessage(1, '请选择要修改的记录!', '提示', hbtOk)
    end;
  end;

  RefurshAct;
end;

procedure TfStudentList.AddRecord(AStrInfo: TStudentInfo);
var
  nGrdRowNum : Integer;
begin
  if Assigned(AStrInfo) then
  begin
    strGrdStu.RowCount := strGrdStu.RowCount + 1;
    nGrdRowNum := strGrdStu.RowCount - 1;

    with strGrdStu, AStrInfo do
    begin
      Cells[0, nGrdRowNum] := IntToStr(stuNumber);
      Cells[1, nGrdRowNum] := stuName;
      Cells[2, nGrdRowNum] := stuSex;
      Cells[3, nGrdRowNum] := stuIdCard;
      Cells[4, nGrdRowNum] := stuLogin;
      Cells[5, nGrdRowNum] := stuPwd;
      Cells[6, nGrdRowNum] := stuArea;
      Cells[7, nGrdRowNum] := stuTel;
      Cells[8, nGrdRowNum] := stuNote1;
    end;
  end;
end;

procedure TfStudentList.FormCreate(Sender: TObject);
begin
  FFrmStudentInio := TFrmStudentInfo.Create(nil);

  ShowStus(StudentControl.StuList);
  RefurshAct;

  pnl2.Visible := True;
  pnl3.Visible := False;

  strGrdStu.OnDblClick := GridDblClick;
  strGrdStu.OnClick := GridClick;
end;

procedure TfStudentList.FormDestroy(Sender: TObject);
begin
  FFrmStudentInio.Free;
end;

procedure TfStudentList.GridClick(Sender: TObject);
begin
  RefurshAct;
end;

procedure TfStudentList.GridDblClick(Sender: TObject);
begin
  actUpdateExecute(nil);
end;

procedure TfStudentList.pm1Popup(Sender: TObject);
begin
  RefurshAct;
end;

procedure TfStudentList.RefurshAct;
begin
  actAdd.Enabled := strGrdStu.RowCount >= 0;

  if (strGrdStu.Selected = -1) or (strGrdStu.RowCount = 0)  then
  begin
    actDel.Enabled := False;
    actUpdate.Enabled := False;
  end
  else
  begin
    if strGrdStu.Cells[0, strGrdStu.Selected] = '' then
    begin
      actDel.Enabled := False;
      actUpdate.Enabled := False;
    end
    else
    begin
      actDel.Enabled := True;
      actUpdate.Enabled := True;
    end;
  end;

  actClearAll.Enabled := strGrdStu.RowCount > 0;

  actStuOutPut.Enabled := strGrdStu.RowCount > 0;
end;

procedure TfStudentList.RefurshGrd(nIndex: Integer);
var
  AStrInfo : TStudentInfo;
begin
  if nIndex <> -1 then
  begin
    AStrInfo := StudentControl.SearchStu(StrToInt(strGrdStu.Cells[0, nIndex]));

    if Assigned(AStrInfo) then
    begin
      with strGrdStu, AStrInfo do
      begin
        Cells[0, nIndex] := IntToStr(stuNumber);
        Cells[1, nIndex] := stuName;
        Cells[2, nIndex] := stuSex;
        Cells[3, nIndex] := stuIdCard;
        Cells[4, nIndex] := stuLogin;
        Cells[5, nIndex] := stuPwd;
        Cells[6, nIndex] := stuArea;
        Cells[7, nIndex] := stuTel;
        Cells[8, nIndex] := stuNote1;
      end;
      strGrdStu.SetFocus;
    end;
  end;
end;

function TfStudentList.SelOneStu: TStudentInfo;
var
  nIndex : Integer;
  nStuNum : Integer;
begin
  pnl2.Visible := False;
  pnl3.Visible := True;

  Result := nil;

  if ShowModal = mrOk then
  begin
    if strGrdStu.RowCount > 0 then
    begin
      nIndex := strGrdStu.Selected;

      if nIndex <> -1 then
      begin
        nStuNum := strGrdStu.Cells[0, nIndex].ToInteger;
        Result := StudentControl.SearchStu(nStuNum);
      end;
    end;
  end;
end;

procedure TfStudentList.ShowStus(slList: TStringList);
var
  i : Integer;
begin
  if Assigned(slList) then
  begin
    strGrdStu.RowCount := 0;
    for i := 0 to slList.Count - 1 do
      AddRecord(TStudentInfo(slList.Objects[i]));
  end;
end;

end.

