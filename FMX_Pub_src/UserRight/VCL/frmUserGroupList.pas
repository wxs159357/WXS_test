unit frmUserGroupList;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList,
  Vcl.XPStyleActnCtrls, Vcl.ActnMan, Vcl.Menus, Vcl.ComCtrls,uUserInfo;

type
  TfUserGroupList = class(TForm)
    lv1: TListView;
    pm1: TPopupMenu;
    NewGroup1: TMenuItem;
    DeleteGroup2: TMenuItem;
    EditGroup1: TMenuItem;
    DeleteGroup3: TMenuItem;
    N3: TMenuItem;
    Refresh2: TMenuItem;
    actmgr1: TActionManager;
    actNewGroup: TAction;
    actRefresh: TAction;
    actDelGroup: TAction;
    actEditGroup: TAction;
    procedure FormShow(Sender: TObject);
    procedure actNewGroupExecute(Sender: TObject);
    procedure actEditGroupExecute(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actDelGroupExecute(Sender: TObject);
  private
    { Private declarations }
    procedure AddGroupList(AGroup : TUserGroup);
    procedure LoadGroupData;
  public
    { Public declarations }
  end;

var
  fUserGroupList: TfUserGroupList;

implementation

uses
  uUserControl, frmUserGroupInfo;

{$R *.dfm}

{ TfUserGroupOption }

procedure TfUserGroupList.actDelGroupExecute(Sender: TObject);
var
  AUserGroup : TUserGroup;
begin
  if lv1.Selected <> nil then
  begin
    if MessageBox(0, '确认要删除选择的记录吗？', '提示',
                  MB_YESNO + MB_ICONQUESTION) = IDYES then
    begin
      AUserGroup := TUserGroup(lv1.Selected.Data);
      if Assigned(UserControl) then
      begin
        UserControl.DelUserGroup(AUserGroup);
        lv1.DeleteSelected;
        ShowMessage('删除组成功!');
      end
      else
        ShowMessage('删除组失败');
    end;
  end;
end;

procedure TfUserGroupList.actEditGroupExecute(Sender: TObject);
var
  AUserGroup : TUserGroup;
  lvSel : TListItem;
begin
  if lv1.ItemIndex <> -1 then
  begin
    lvSel := lv1.Selected;
    AUserGroup := TUserGroup(lvSel.Data);
    with TfUserGroupInfo.Create(nil) do
    begin
      ShowInfo(AUserGroup);
      if ShowModal = mrOk then
      begin
        SaveInfo;
        if Assigned(UserControl) then
        begin
          UserControl.SaveUserGroup(AUserGroup);
          with lvSel do
          begin
            Caption := AUserGroup.GroupName;
            SubItems[0] := AUserGroup.Description;
            Data := AUserGroup;
          end;
          MessageBox(0, '编辑成功!', '提示', MB_OK);
        end;
      end;
      Free;
    end;
  end
  else
  begin
    MessageBox(0, '请选择需要编辑的组!', '提示', MB_OK);
  end;
end;

procedure TfUserGroupList.actNewGroupExecute(Sender: TObject);
var
  AUserGroup : TUserGroup;
begin
  with TfUserGroupInfo.Create(nil) do
  begin
    AUserGroup := TUserGroup.Create;
    ShowInfo(AUserGroup);
    if ShowModal = mrOk then
    begin
      SaveInfo;
      if Assigned(UserControl) then
      begin
        UserControl.SaveUserGroup(AUserGroup);
        AddGroupList(AUserGroup);
        MessageBox(0, '保存成功!', '提示', MB_OK);
      end;
    end;
    Free;
  end;
end;

procedure TfUserGroupList.actRefreshExecute(Sender: TObject);
begin
  LoadGroupData;
end;

procedure TfUserGroupList.AddGroupList(AGroup: TUserGroup);
begin
  if Assigned(AGroup) then
  begin
    with lv1.Items.Add, TUserGroup( AGroup ) do
    begin
      Caption := GroupName;
      SubItems.Add( Description );
      Data := AGroup;
    end;
  end;
end;

procedure TfUserGroupList.FormShow(Sender: TObject);
begin
  LoadGroupData;
end;

procedure TfUserGroupList.LoadGroupData;
var
  i : integer;
begin
  lv1.Items.Clear;

  if Assigned(UserControl) then
  begin
    with UserControl.UserGroupList do
    begin
      for i := 0 to Count - 1 do
        AddGroupList(TUserGroup(Objects[i]));
    end;
  end;
  if lv1.Items.Count > 0 then
    lv1.ItemIndex := 0;
end;

end.
