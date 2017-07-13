unit uSimpleInfoList;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.ImageList, Vcl.ImgList,
  Vcl.Menus, Vcl.PlatformDefaultStyleActnCtrls, Vcl.ActnPopup, System.Actions,
  Vcl.ActnList, Vcl.XPStyleActnCtrls, Vcl.ActnMan, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.ExtCtrls, xSimpleInfoControl, uSimpleInfo;

type
  TfSimpleInfoList = class(TForm)
    pnlBottom: TPanel;
    btnClose: TButton;
    btnSelect: TButton;
    lvSimpleInfoList: TListView;
    actnmngractmgr1: TActionManager;
    actAdd: TAction;
    actDel: TAction;
    actEdit: TAction;
    actSearch: TAction;
    actImport: TAction;
    pctnbr1: TPopupActionBar;
    mntmAddPerson: TMenuItem;
    mntmEditInfo: TMenuItem;
    mntmDelPerson: TMenuItem;
    dlgOpen1: TOpenDialog;
    imglstil1: TImageList;
    btnAddPerson: TButton;
    btnEditInfo: TButton;
    btnDelPerson: TButton;
    procedure actAddExecute(Sender: TObject);
    procedure actEditExecute(Sender: TObject);
    procedure actDelExecute(Sender: TObject);
    procedure lvSimpleInfoListDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
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

var
  fSimpleInfoList: TfSimpleInfoList;

implementation

{$R *.dfm}

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
        Application.MessageBox('名称重复，请重新用其他名称！', '警告', MB_OK +
          MB_ICONINFORMATION);
        AInfo.Free;
      end
      else
      begin
        with lvSimpleInfoList.Items.Add do
        begin
          Caption := '';
          SubItems.Add('');
          SubItems.Add('');
          Data := AInfo;
        end;
        FSimpleInfoControl.AddInfo(AInfo);
        RefurshLV(lvSimpleInfoList.Items.Count-1);
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
  AInfo : TSimpleInfo;
  nIndex : Integer;
  i : Integer;
begin
  if lvSimpleInfoList.ItemIndex = -1 then
    Exit;

  nIndex := -1;

  if MessageBox(Handle, '确定删除选择记录？', '', MB_YESNO + MB_ICONQUESTION)
    = IDYES then
  begin
    for i := lvSimpleInfoList.Items.Count - 1 downto 0 do
    begin
      if lvSimpleInfoList.Items[i].Selected then
      begin
        AInfo := TSimpleInfo(lvSimpleInfoList.Items[i].Data);
        lvSimpleInfoList.Items.Delete(i);
        FSimpleInfoControl.DelInfo(AInfo.SIID);
        nIndex := i;
      end;
    end;

    if nIndex > 0 then
      lvSimpleInfoList.ItemIndex := nIndex - 1;
  end;
end;

procedure TfSimpleInfoList.actEditExecute(Sender: TObject);
var
  AInfo : TSimpleInfo;
begin
  if lvSimpleInfoList.ItemIndex = -1 then
    Exit;

  AInfo := TSimpleInfo(lvSimpleInfoList.Items[lvSimpleInfoList.ItemIndex].Data);

  with TfSimpleInfo.Create(Self) do
  begin
    Caption := FSimpleInfoControl.SimpleInfoName;
    ShowInfo(AInfo);
    if ShowModal = mrOk then
    begin

      if Assigned(FSimpleInfoControl.GetInfoByName(edtSIName.Text, AInfo)) then
      begin
        Application.MessageBox('修改失败！名称已存在，请重新用其他名称！', '警告', MB_OK +
          MB_ICONINFORMATION);
      end
      else
      begin
        SaveInfo;

        FSimpleInfoControl.EditInfo(AInfo);
        RefurshLV(lvSimpleInfoList.ItemIndex);
      end;
    end;
    Free
  end;
end;

procedure TfSimpleInfoList.FormCreate(Sender: TObject);
begin
  FSimpleInfoControl := TSimpleInfoControl.Create;
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

procedure TfSimpleInfoList.LoadList;
var
  i : Integer;
begin
  lvSimpleInfoList.Items.Clear;

  if not Assigned(FSimpleInfoControl) then
    Exit;

  for i := 0 to FSimpleInfoControl.SimpleInfoList.Count - 1 do
  begin
    with lvSimpleInfoList.Items.Add do
    begin
      Caption := '';
      SubItems.Add('');
      SubItems.Add('');
      Data := FSimpleInfoControl.SimpleInfoList.Objects[i];
    end;
    RefurshLV(i);
  end;
end;

procedure TfSimpleInfoList.lvSimpleInfoListDblClick(Sender: TObject);
begin
  actEditExecute(nil);
end;

procedure TfSimpleInfoList.RefurshLV(nIndex: Integer);
var
  AInfo : TSimpleInfo;
begin
  if lvSimpleInfoList.Items.Count > nIndex then
  begin
    AInfo := TSimpleInfo(lvSimpleInfoList.Items[nIndex].Data);
    lvSimpleInfoList.Items[nIndex].Caption := IntToStr(AInfo.SIID);
    lvSimpleInfoList.Items[nIndex].SubItems[0] := AInfo.SIName;
    lvSimpleInfoList.Items[nIndex].SubItems[1] := AInfo.SIRemark1;
  end;
end;

function TfSimpleInfoList.SelectInfo: TSimpleInfo;
begin
  Result := nil;

  lvSimpleInfoList.MultiSelect := False;

  if ShowModal = mrOk then
  begin
    if lvSimpleInfoList.ItemIndex <> -1 then
    begin
      Result := TSimpleInfo(lvSimpleInfoList.Items[lvSimpleInfoList.ItemIndex].Data);
    end;
  end;
end;

end.

