unit uSimpleInfoList;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
  FMX.Grid.Style, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Grid,
  FMX.StdCtrls, FMX.Objects, FMX.Layouts, System.Actions, FMX.ActnList,
  System.ImageList, FMX.ImgList, FMX.Menus, xSimpleInfoControl, uSimpleInfo;

type
  TfSimpleInfoList = class(TForm)
    strngrdSimpleInfoList: TStringGrid;
    lyt1: TLayout;
    ln1: TLine;
    btn1: TButton;
    btn2: TButton;
    strngclmn1: TStringColumn;
    strngclmn2: TStringColumn;
    strngclmn3: TStringColumn;
    actnlst1: TActionList;
    actAdd: TAction;
    actDel: TAction;
    actEdit: TAction;
    imglst1: TImageList;
    btn3: TButton;
    btn4: TButton;
    btn5: TButton;
    pmn1: TPopupMenu;
    mntm1: TMenuItem;
    mntm2: TMenuItem;
    mntm3: TMenuItem;
    procedure actAddExecute(Sender: TObject);
    procedure actEditExecute(Sender: TObject);
    procedure actDelExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FSimpleInfoName: string;
    FSimpleInfoDBName: string;
    FSimpleInfoControl : TSimpleInfoControl;
    { Private declarations }

    /// <summary>
    /// 刷新项
    /// </summary>
    procedure RefurshLV(nIndex : Integer);

    /// <summary>
    /// 加载列表
    /// </summary>
    procedure LoadList;

    /// <summary>
    /// StringGrid 单双击事件
    /// </summary>
    procedure GridClick(Sender: TObject);
    procedure GridDblClick(Sender: TObject);
  public
    { Public declarations }
    /// <summary>
    /// 选择信息
    /// </summary>
    function SelectInfo : TSimpleInfo;

    /// <summary>
    /// 简单信息名称  show 之前赋值
    /// </summary>
    property SimpleInfoName : string read FSimpleInfoName write FSimpleInfoName;

    /// <summary>
    /// 简单信息数据库表名称 show 之前赋值
    /// </summary>
    property SimpleInfoDBName : string read FSimpleInfoDBName write FSimpleInfoDBName;
  end;

implementation

{$R *.fmx}

{ TfSimpleInfoList }

procedure TfSimpleInfoList.actAddExecute(Sender: TObject);
var
  AInfo : TSimpleInfo;
begin
  AInfo := TSimpleInfo.Create;

  with TfSimpleInfo.Create(Self) do
  begin
    Caption := FSimpleInfoControl.SimpleInfoName;
    ShowInfo(AInfo);
    if ShowModal = mrOk then
    begin
      SaveInfo;

      if Assigned(FSimpleInfoControl.GetInfoByName(AInfo.SIName)) then
      begin
        MessageDlg('名称重复，请重新用其他名称！',  TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);

        AInfo.Free;
      end
      else
      begin
        strngrdSimpleInfoList.RowCount := strngrdSimpleInfoList.RowCount + 1;

        FSimpleInfoControl.AddInfo(AInfo);
        RefurshLV(strngrdSimpleInfoList.RowCount-1);
      end;
    end
    else
    begin
      AInfo.Free;
    end;

    Free;
  end;
end;

procedure TfSimpleInfoList.actDelExecute(Sender: TObject);
var
  nIndex : Integer;
  nID : Integer;
begin
  nIndex := strngrdSimpleInfoList.Row;
  if (nIndex = -1) or (strngrdSimpleInfoList.RowCount = 0) then
    Exit;

  if MessageDlg('确定删除选择记录?',  TMsgDlgType.mtConfirmation,
    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
  begin
    TryStrToInt(strngrdSimpleInfoList.Cells[0, nIndex], nID);
    FSimpleInfoControl.DelInfo(nID);

    LoadList;

    if nIndex > 0 then
      strngrdSimpleInfoList.Row := nIndex - 1;
  end;
end;

procedure TfSimpleInfoList.actEditExecute(Sender: TObject);
var
  AInfo : TSimpleInfo;
  nID : Integer;
begin
  if strngrdSimpleInfoList.Row = -1 then
    Exit;
  TryStrToInt(strngrdSimpleInfoList.Cells[0, strngrdSimpleInfoList.Row], nID);
  AInfo := FSimpleInfoControl.GetInfoByID(nID);

  with TfSimpleInfo.Create(Self) do
  begin
    Caption := FSimpleInfoControl.SimpleInfoName;
    ShowInfo(AInfo);
    if ShowModal = mrOk then
    begin

      if Assigned(FSimpleInfoControl.GetInfoByName(edtSIName.Text, AInfo)) then
      begin
        MessageDlg('修改失败！名称已存在，请重新用其他名称！',
          TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
      end
      else
      begin
        SaveInfo;

        FSimpleInfoControl.EditInfo(AInfo);
        RefurshLV(strngrdSimpleInfoList.Row);
      end;
    end;
    Free
  end;
end;

procedure TfSimpleInfoList.FormCreate(Sender: TObject);
begin
  FSimpleInfoControl := TSimpleInfoControl.Create;

  strngrdSimpleInfoList.OnDblClick := GridDblClick;
  strngrdSimpleInfoList.OnClick := GridClick;
end;

procedure TfSimpleInfoList.FormDestroy(Sender: TObject);
begin
  FSimpleInfoControl.Free;
end;

procedure TfSimpleInfoList.FormShow(Sender: TObject);
begin
  FSimpleInfoControl.SimpleInfoName := FSimpleInfoName;

  if FSimpleInfoDBName = '' then
  begin
    ShowMessage('显示界面先要赋值数据库表名称！');
    Exit;
  end;

  FSimpleInfoControl.LoadList(FSimpleInfoDBName);

  Caption := FSimpleInfoControl.SimpleInfoName + '列表';
  LoadList;
end;

procedure TfSimpleInfoList.GridClick(Sender: TObject);
begin

end;

procedure TfSimpleInfoList.GridDblClick(Sender: TObject);
begin
  actEditExecute(nil);
end;

procedure TfSimpleInfoList.LoadList;
var
  i : Integer;
begin
  strngrdSimpleInfoList.RowCount := 0;

  if not Assigned(FSimpleInfoControl) then
    Exit;

  for i := 0 to FSimpleInfoControl.SimpleInfoList.Count - 1 do
  begin
    strngrdSimpleInfoList.RowCount := strngrdSimpleInfoList.RowCount + 1;
    RefurshLV(i);
  end;
end;

procedure TfSimpleInfoList.RefurshLV(nIndex: Integer);
var
  AInfo : TSimpleInfo;
begin
  if nIndex >= 0 then
  begin
    AInfo := FSimpleInfoControl.SimpleInfo[nIndex];

    if Assigned(AInfo) then
    begin
      strngrdSimpleInfoList.Cells[0, nIndex] := IntToStr(AInfo.SIID);
      strngrdSimpleInfoList.Cells[1, nIndex] := AInfo.SIName;
      strngrdSimpleInfoList.Cells[2, nIndex] := AInfo.SIRemark1;
    end;
  end;
end;

function TfSimpleInfoList.SelectInfo: TSimpleInfo;
var
  nID : Integer;
begin
  Result := nil;

  if ShowModal = mrOk then
  begin
    if strngrdSimpleInfoList.Row <> -1 then
    begin
      TryStrToInt(strngrdSimpleInfoList.Cells[0, strngrdSimpleInfoList.Row], nID);
      Result := FSimpleInfoControl.GetInfoByID(nID);
    end;
  end;
end;

end.
