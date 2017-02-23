unit frmUserList;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList,
  Vcl.XPStyleActnCtrls, Vcl.ActnMan, Vcl.Menus, Vcl.ComCtrls, uUserInfo;

type
  TfUserList = class(TForm)
    lv1: TListView;
    pm1: TPopupMenu;
    NewUser2: TMenuItem;
    N3: TMenuItem;
    ChangePassword2: TMenuItem;
    EditUser1: TMenuItem;
    DeleteUser2: TMenuItem;
    N4: TMenuItem;
    Refresh2: TMenuItem;
    actmgr1: TActionManager;
    actNewUser: TAction;
    actRefresh: TAction;
    actChangePassword: TAction;
    actDeleteUser: TAction;
    actEditUser: TAction;
    procedure actNewUserExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actEditUserExecute(Sender: TObject);
    procedure actDeleteUserExecute(Sender: TObject);
    procedure actChangePasswordExecute(Sender: TObject);
  private
    { Private declarations }
    procedure AddUserList(AUser : TUser);
    procedure LoadData;
  public
    { Public declarations }
  end;

var
  fUserList: TfUserList;

implementation

uses
  frmUserNew, uUserControl, uUpUsPass;

{$R *.dfm}

procedure TfUserList.actChangePasswordExecute(Sender: TObject);
var
  lvItem : TListItem;
begin
  lvItem := lv1.Selected;
  if lvItem <> nil then
  begin
    if Assigned(UserControl) then
    begin
      UserControl.UserUpass(TUser(lvItem.Data));
    end;
  end;
end;

procedure TfUserList.actDeleteUserExecute(Sender: TObject);
var
  AUser : TUser;
begin
  if lv1.Selected <> nil then
  begin
    if MessageBox(0, '确认要删除选择的记录吗？', '提示',
                  MB_YESNO + MB_ICONQUESTION) = IDYES then
    begin
      AUser := TUser(lv1.Selected.Data);
      if Assigned(UserControl) then
      begin
        UserControl.DelUser(AUser);
        lv1.DeleteSelected;
        ShowMessage('删除用户成功!');
      end
      else
        ShowMessage('删除用户失败');
    end;
  end;
end;

procedure TfUserList.actEditUserExecute(Sender: TObject);
var
  AUser : TUser;
  lvSel : TListItem;
begin
  if lv1.ItemIndex <> -1 then
  begin
    lvSel := lv1.Selected;
    AUser := TUser(lvSel.Data);
    with TfUserNew.Create(nil) do
    begin
      ShowInfo(AUser);
      if ShowModal = mrOk then
      begin
        SaveInfo;
        if Assigned(UserControl) then
        begin
          UserControl.SaveUser(AUser);
          with lvSel do
          begin
            Caption := AUser.LoginName;
            SubItems[0] := AUser.FullName;
            SubItems[1] := AUser.Description;
            Data := AUser;
          end;
          MessageBox(0, '编辑成功!', '提示', MB_OK);
        end;
      end;
      Free;
    end;
  end
  else
  begin
    MessageBox(0, '请选择需要编辑的用户!', '提示', MB_OK);
  end;
end;

procedure TfUserList.actNewUserExecute(Sender: TObject);
var
  AUser : TUser;
begin
  with TfUserNew.Create(nil) do
  begin
    AUser := TUser.Create;
    ShowInfo(AUser);
    if ShowModal = mrOk then
    begin
      SaveInfo;
      if Assigned(UserControl) then
      begin
        UserControl.SaveUser(AUser);
        AddUserList(AUser);
        MessageBox(0, '保存成功!', '提示', MB_OK);
      end;
    end;
    Free;
  end;
end;

procedure TfUserList.actRefreshExecute(Sender: TObject);
begin
  LoadData;
end;

procedure TfUserList.AddUserList(AUser: TUser);
begin
  if Assigned(AUser) then
  begin
    with lv1.Items.Add, AUser do
    begin
      Caption := AUser.LoginName;
      SubItems.Add(AUser.FullName);
      SubItems.Add(AUser.Description);
      Data := AUser;
    end;
  end;
end;

procedure TfUserList.FormShow(Sender: TObject);
begin
  LoadData;
end;

procedure TfUserList.LoadData;
var
  i : integer;
begin
  lv1.Items.Clear;

  if Assigned(UserControl) then
  begin
    with UserControl.UserList do
    begin
      for i := 0 to Count - 1 do
        AddUserList(TUser(Objects[i]));
    end;
  end;
  if lv1.Items.Count > 0 then
    lv1.ItemIndex := 0;
end;

end.
