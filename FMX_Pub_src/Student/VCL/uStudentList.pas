unit uStudentList;

interface

uses
  Windows,xStudentInfo, Messages, SysUtils, Variants, Classes, Graphics, Forms,
  Dialogs,uStudentInfo,xStudentControl, StdCtrls,Grids, Buttons, ComCtrls,
  Controls,ComObj, ActnList, XPStyleActnCtrls, ActnMan, ExtCtrls, ImgList, ExtActns,
  xStudentTabelOutOrIn, StdActns, Menus, ActnPopup,
  Vcl.PlatformDefaultStyleActnCtrls, System.ImageList, System.Actions, xFunction;

type
  TfStudentList = class(TForm)
    dlgOpenExcel: TOpenDialog;
    dlgSaveExcel: TSaveDialog;
    actmgr1: TActionManager;
    actClearList: TAction;
    pnl1: TPanel;
    btnExport: TBitBtn;
    btnModify: TBitBtn;
    btnDelete: TBitBtn;
    btnAdd: TBitBtn;
    pnl2: TPanel;
    btnSelect: TBitBtn;
    btnSelectCancel: TBitBtn;
    ilImage: TImageList;
    ActionAdd: TAction;
    actDelete: TAction;
    actRevise: TAction;
    actImportStu: TAction;
    btnClearList: TBitBtn;
    actSearch: TAction;
    actExport: TAction;
    btnImportStu: TBitBtn;
    pctnbrMousepctnbr1: TPopupActionBar;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    actSelectAll: TAction;
    actEnable: TAction;
    actShowAll: TAction;
    pnl3: TPanel;
    btbtnShowAll: TBitBtn;
    btbtnSearch: TBitBtn;
    edtCondition: TEdit;
    lbl1: TLabel;
    lvStuListV: TListView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actClearListExecute(Sender: TObject);
    procedure ActionAddExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actReviseExecute(Sender: TObject);
    procedure actSearchExecute(Sender: TObject);
    procedure actImportStuExecute(Sender: TObject);
    procedure actExportExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actEnableExecute(Sender: TObject);
    procedure actShowAllExecute(Sender: TObject);
  private
    { Private declarations }
    FFormfStudentList: TfStudentInfo;

    /// <summary>
    /// 刷新
    /// </summary>
    procedure RefurshAct;

    /// <summary>
    /// 显示考生列表
    /// </summary>
    procedure ShowStus(slList : TStringList);

   /// <summary>
   /// listview添加记录
   /// </summary>
    function AddRecord(stu: TStudentInfo):Boolean;

    /// <summary>
    /// 刷新记录
    /// </summary>
    procedure RefurshLV(nIndex : Integer);

  public
    { Public declarations }
    /// <summary>
    /// 选择listView 中的学员，放到 TStringList
    /// </summary>
    procedure SelectStu(slList: TStringList);


    /// <summary>
    /// 选择一个考生
    /// </summary>
    function SelOneStu:TStudentInfo;

     /// <summary>
    /// 学员管理
    /// </summary>
    procedure ManageStu;

  end;

var
  fStudentList: TfStudentList;

implementation

{$R *.dfm}

function TfStudentList.SelOneStu: TStudentInfo;
var
  nIndex : Integer;
begin
  pnl1.Visible := False;
  pnl2.Visible := True;
  lvStuListV.MultiSelect := False;
  Result := nil;

  if ShowModal = mrOk then
  begin
    nIndex := lvStuListV.ItemIndex;

    if nIndex <> -1 then
    begin
      Result := TStudentInfo(lvStuListV.Items[nIndex].Data);
    end;
  end;
end;

procedure TfStudentList.SelectStu(slList: TStringList);
var
  i: Integer;
  AStu : TStudentInfo;
begin
  pnl1.Visible := False;
  pnl2.Visible := True;
  lvStuListV.MultiSelect := True;

  if ShowModal = mrOk then
  begin
    for I := 0 to lvStuListV.Items.Count - 1 do
    begin
      if lvStuListV.Items[i].Selected then
      begin
        AStu := TStudentInfo(lvStuListV.Items[i].Data);
        slList.AddObject(IntToStr(AStu.stuNumber), AStu);
      end;
    end;
  end;
end;

procedure TfStudentList.ShowStus(slList: TStringList);
var
  i: Integer;
begin
  if Assigned(slList) then
  begin
    lvStuListV.Items.Clear;

    for i := 0 to slList.Count - 1 do
      AddRecord(TStudentInfo(slList.Objects[i]))
  end;
end;

procedure TfStudentList.RefurshAct;
begin
  actEnableExecute(nil);
end;

procedure TfStudentList.RefurshLV(nIndex: Integer);
var
  stu : TStudentInfo;
begin
  if nIndex <> -1 then
  begin
    with lvStuListV.items[nIndex] do
    begin
      stu := TStudentInfo(Data);
      caption:=IntToStr(stu.stuNumber);
      SubItems[0] := stu.stuName;
      subitems[1] := stu.stuSex;
      SubItems[2] := stu.stuIDcard;
      SubItems[3] := stu.stuLogin;
      SubItems[4] := stu.stupwd;
      SubItems[5] := stu.stuArea;
      SubItems[6] := stu.stuTel;
      SubItems[7] := stu.stuNote1;
    end;
  end;
end;

procedure TfStudentList.actClearListExecute(Sender: TObject);
begin
   if Application.MessageBox('确定要清空数据库中的记录吗？', '警告',
     MB_OKCANCEL + MB_ICONQUESTION) = IDOK then
   begin
     StudentControl.ClearStus;
     lvStuListV.Clear;
     actClearList.Enabled := False;
     actSelectAll.Enabled := False;
   end;
end;

procedure TfStudentList.actDeleteExecute(Sender: TObject);
var
  i: Integer;
  Astu: TStudentInfo;
begin
  if lvStuListV.ItemIndex = -1 then
  begin
    Application.MessageBox('请选择要删除的记录！','警告', MB_OK +MB_ICONINFORMATION);
    Exit;

  end;

  if Application.MessageBox('确定要删除所选记录吗？', '警告', MB_OKCANCEL +
    MB_ICONQUESTION) = IDOK then
  begin
    for I := lvStuListV.Items.Count  - 1 downto 0 do
    begin
      if lvStuListV.Items[I].Selected then
      begin
        Astu := TStudentInfo(lvStuListV.Items[I].Data);
        StudentControl.DelStu(Astu);
        lvStuListV.Items[i].Delete;
      end;
    end;
    actClearList.Enabled := not(lvStuListV.Items.Count = 0);
    actSelectAll.Enabled := not(lvStuListV.Items.Count = 0);
  end;
end;

procedure TfStudentList.actEnableExecute(Sender: TObject);
begin
  btnModify.Enabled := lvStuListV.ItemIndex <> -1;
  btnDelete.Enabled := lvStuListV.ItemIndex <> -1;
  btnExport.Enabled := lvStuListV.ItemIndex <> -1;
  actDelete.Enabled := lvStuListV.ItemIndex <> -1;
  actRevise.Enabled := lvStuListV.ItemIndex <> -1;

  btnModify.Enabled := not (lvStuListV.ItemIndex = -1);
  btnDelete.Enabled := not (lvStuListV.ItemIndex = -1);
  btnExport.Enabled := not (lvStuListV.ItemIndex = -1);
  actDelete.Enabled := not (lvStuListV.ItemIndex = -1);
  actRevise.Enabled := not (lvStuListV.ItemIndex = -1);
end;

procedure TfStudentList.actExportExecute(Sender: TObject);
var
  i: Integer;
  slList : TStringList;
  AStuExcel: TStudentTableOption;
  nResult: Integer;
begin
  if lvStuListV.items.count > 0 then
  begin
    Cursor := crHourGlass;

    AStuExcel :=  TStudentTableOption.Create;
    slList := TStringList.Create;

    // 获取选择的学员列表
    for i := 0 to lvStuListV.items.count - 1 do
    begin
      slList.AddObject('', TStudentInfo(lvStuListV.items[i].data));
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

procedure TfStudentList.actImportStuExecute(Sender: TObject);
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

procedure TfStudentList.ActionAddExecute(Sender: TObject);
var
  AStu: TStudentInfo;
begin
  AStu := TStudentInfo.Create;
  FFormfStudentList.ShowStu(AStu);

  if FFormfStudentList.ShowModal = mrOk then
  begin
    FFormfStudentList.SaveStu;
    StudentControl.AddStu(AStu);
    AddRecord(AStu);
    actClearList.Enabled := lvStuListV.Items.Count <> 0;
    actSelectAll.Enabled := lvStuListV.Items.Count <> 0;
  end
  else
  begin
    AStu.Free;
  end;

end;

procedure TfStudentList.actReviseExecute(Sender: TObject);
var
  nIndex : Integer;
  AStu : TStudentInfo;
begin
  nIndex := lvStuListV.ItemIndex;

  if nIndex = -1 then
  begin
    Application.MessageBox('请选择需要修改的记录!','警告', MB_OK +MB_ICONINFORMATION);
    Exit;
  end;

  AStu := TStudentInfo(lvStuListV.Items[nIndex].Data);
  FFormfStudentList.ShowStu(AStu);

  if FFormfStudentList.ShowModal = mrOk then
  begin
    FFormfStudentList.SaveStu;
    StudentControl.EditStu(AStu);

    RefurshLV(nIndex);
  end;

end;

procedure TfStudentList.actSearchExecute(Sender: TObject);
var
  slList : TStringList;
  sCondition : string;
begin
  sCondition := Trim(edtCondition.Text);
  lvStuListV.Clear;
   // 当 edtCondition控件文本内容为空点击查询，显示所有学员信息
  if (edtCondition.Text = '') then
  begin
    ShowStus(StudentControl.StuList);
  end
  else
  begin
    slList := TStringList.Create;
    StudentControl.SearchStus(sCondition,slList);
    ShowStus(slList);
    slList.Free;
  end;

end;

procedure TfStudentList.actSelectAllExecute(Sender: TObject);
begin
  lvStuListV.SelectAll;
end;

procedure TfStudentList.actShowAllExecute(Sender: TObject);
begin
  ShowStus(StudentControl.StuList);
end;

function TfStudentList.AddRecord(stu: TStudentInfo):Boolean;
begin
  Result := False;

  if not Assigned(stu) then
    Exit;

  with lvStuListV.items.add do
  begin
    caption:=IntToStr(stu.stuNumber);
    subitems.add(stu.stuName);
    subitems.add(stu.stuSex);
    SubItems.Add(stu.stuIDcard);
    SubItems.Add(stu.stuLogin);
    SubItems.Add(stu.stuPwd);
    SubItems.Add(stu.stuArea);
    SubItems.Add(stu.stuTel);
    SubItems.Add(stu.stuNote1);
    data := stu;
    Result := True;
  end;

end;

procedure TfStudentList.FormCreate(Sender: TObject);
begin
  FFormfStudentList := TfStudentInfo.Create(nil);

  if StudentControl.StuList.Count = 0 then
  begin
    btnModify.Enabled    := False;
    btnDelete.Enabled    := False;
    btnExport.Enabled    := False;
    actDelete.Enabled    := False;
    actRevise.Enabled    := False;
    actSelectAll.Enabled := False;
    actClearList.Enabled := False;
  end;

  with lvStuListV do
  begin
    ViewStyle:=vsreport;
    GridLines:=true;
    Columns.Items[0].Width := 40;
    Columns.Items[1].Width := 80;
    Columns.Items[2].Width := 50;
    Columns.Items[3].Width := 130;
    Columns.Items[4].Width := 80;
    Columns.Items[5].Width := 80;
    Columns.Items[6].Width := 50;
    Columns.Items[7].Width := 80;
    Columns.Items[8].Width := 158;
  end;

  ShowStus(StudentControl.StuList);
end;

procedure TfStudentList.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FFormfStudentList);
  inherited;
end;

procedure TfStudentList.ManageStu;
begin
  pnl2.Visible := False;
  pnl1.Visible := true;
  ShowModal;
end;

end.




