unit uSortList;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
  FMX.Grid.Style, FMX.StdCtrls, System.Actions, FMX.ActnList, FMX.Grid,
  FMX.Controls.Presentation, FMX.ScrollBox, uSortInfo, xSortControl, xSortInfo,
  xFunction;

type
  TfSortList = class(TForm)
    strngrdSortList: TStringGrid;
    strngclmn1: TStringColumn;
    strngclmn2: TStringColumn;
    strngclmn3: TStringColumn;
    actnlstList: TActionList;
    actAdd: TAction;
    actDel: TAction;
    actUpdate: TAction;
    pnl2: TPanel;
    btnAdd: TButton;
    btnDel: TButton;
    btnEdit: TButton;
    actLoadList: TAction;
    actClear: TAction;
    btn1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actLoadListExecute(Sender: TObject);
    procedure actAddExecute(Sender: TObject);
    procedure actDelExecute(Sender: TObject);
    procedure actUpdateExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
  private
    { Private declarations }
    FFormSortInfo : TfSortInfo;

    /// <summary>
    /// 刷新
    /// </summary>
    procedure RefurshAct;

    /// <summary>
    /// 添加记录
    /// </summary>
    procedure AddRecord(AInfo : TSortInfo);

    /// <summary>
    /// 刷新StringGrid纪录
    /// </summary>
    procedure RefurshGrd(nIndex : Integer);

    /// <summary>
    /// StringGrid 双击   单机事件
    /// </summary>
    procedure GridClick(Sender: TObject);
    procedure GridDblClick(Sender: TObject);
  public
    { Public declarations }
  end;

var
  fSortList: TfSortList;

implementation

{$R *.fmx}

procedure TfSortList.actAddExecute(Sender: TObject);
var
  AInfo : TSortInfo;
begin
  if Assigned(FFormSortInfo) then
  begin
    AInfo := TSortInfo.Create;

    FFormSortInfo.ShowInfo(AInfo);
    if FFormSortInfo.ShowModal = mrOk then
    begin
      FFormSortInfo.SaveInfo;

      if Assigned(SortControl) then
      begin
        SortControl.AddSort(AInfo);
        AddRecord(AInfo);
      end;
      strngrdSortList.SelectRow(strngrdSortList.RowCount - 1);
    end
    else
    begin
      AInfo.Free;
    end;
  end;
  RefurshAct;
end;

procedure TfSortList.actClearExecute(Sender: TObject);
begin
  if HintMessage(1, '您确定清空记录吗?', '提示', hbtYesNo) = hrtYes then
  begin
    strngrdSortList.RowCount := 0;
    SortControl.ClearSrot;
  end;
  RefurshAct;
end;

procedure TfSortList.actDelExecute(Sender: TObject);
var
  nValue : integer;
  AInfo : TSortInfo;
begin
  nValue := strngrdSortList.Selected;
  if nValue = -1 then
    HintMessage(1, '请选择要删除的记录', '提示', hbtOk)
  else
  begin
    if HintMessage(1, '你确定删除吗?', '提示', hbtYesNo) = hrtYes then
    begin
      if (strngrdSortList.RowCount > 0) then
      begin
        if strngrdSortList.Selected <> -1 then
        begin
          AInfo := SortControl.SortInfo[strngrdSortList.Selected];
          SortControl.DelSort(AInfo.SortID);
          actLoadListExecute(nil);
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

procedure TfSortList.actLoadListExecute(Sender: TObject);
var
  i : Integer;
begin
  strngrdSortList.RowCount := 0;
  if Assigned(SortControl) then
  begin
    for i := 0 to SortControl.SortList.Count - 1 do
      AddRecord(SortControl.SortInfo[i]);
  end;
end;

procedure TfSortList.actUpdateExecute(Sender: TObject);
var
  ASortInfo : TSortInfo;
begin
  if strngrdSortList.Selected = -1 then
    HintMessage(1, '请选择要修改的记录!', '提示', hbtOk)
  else
  begin
    ASortInfo := SortControl.SortInfo[strngrdSortList.Selected];
    if Assigned(ASortInfo) then
    begin
      FFormSortInfo.ShowInfo(ASortInfo);
      if FFormSortInfo.ShowModal = mrOk then
      begin
        FFormSortInfo.SaveInfo;

        SortControl.EditSort(ASortInfo);

        RefurshGrd(strngrdSortList.Selected);
      end;
    end
    else
    begin
      HintMessage(1, '请选择要修改的记录!', '提示', hbtOk)
    end;
  end;

end;

procedure TfSortList.AddRecord(AInfo: TSortInfo);
var
  nGrdRowNum : Integer;
begin
  if Assigned(AInfo) then
  begin
    strngrdSortList.RowCount := strngrdSortList.RowCount + 1;
    nGrdRowNum := strngrdSortList.RowCount - 1;

    with strngrdSortList, AInfo do
    begin
      Cells[0, nGrdRowNum] := IntToStr(AInfo.SortID);
      Cells[1, nGrdRowNum] := AInfo.SortName;
      Cells[2, nGrdRowNum] := AInfo.SortRemark;
    end;
  end;
end;

procedure TfSortList.FormCreate(Sender: TObject);
begin
  FFormSortInfo := TfSortInfo.Create(nil);

  strngrdSortList.OnDblClick := GridDblClick;
  strngrdSortList.OnClick := GridClick;
end;

procedure TfSortList.FormDestroy(Sender: TObject);
begin
  FFormSortInfo.Free;
end;

procedure TfSortList.FormShow(Sender: TObject);
begin
  actLoadListExecute(nil);
end;

procedure TfSortList.GridClick(Sender: TObject);
begin
  RefurshAct;
end;

procedure TfSortList.GridDblClick(Sender: TObject);
begin
  actUpdateExecute(nil);
end;

procedure TfSortList.RefurshAct;
begin
  actDel.Enabled := strngrdSortList.Selected <> -1;
  actUpdate.Enabled := strngrdSortList.Selected <> -1;

  actClear.Enabled := strngrdSortList.RowCount > 0;
end;

procedure TfSortList.RefurshGrd(nIndex: Integer);
var
  ASortInfo : TSortInfo;
begin
  if nIndex <> -1 then
  begin
    ASortInfo := SortControl.SortInfo[nIndex];

    if Assigned(ASortInfo) then
    begin
      with strngrdSortList, ASortInfo do
      begin
        Cells[0, nIndex] := IntToStr(ASortInfo.SortID);
        Cells[1, nIndex] := ASortInfo.SortName;
        Cells[2, nIndex] := ASortInfo.SortRemark;
      end;
      strngrdSortList.SetFocus;
    end;
  end;
end;

end.
