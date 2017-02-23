unit uFramUserList;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, System.Rtti, FMX.Grid.Style, FMX.Grid,
  FMX.ScrollBox, FMX.Menus, System.Actions, FMX.ActnList, uUserInfo,
  FMX.DialogService;

type
  TfamUserList = class(TFrame)
    strGrdUser: TStringGrid;
    strngclmn1: TStringColumn;
    strngclmn2: TStringColumn;
    strngclmn3: TStringColumn;
    PopupMenu1: TPopupMenu;
    ActionList1: TActionList;
    actAddUser: TAction;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    actRefresh: TAction;
    actEditUser: TAction;
    actDel: TAction;
    actUserUpPass: TAction;
    procedure actAddUserExecute(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actEditUserExecute(Sender: TObject);
    procedure actDelExecute(Sender: TObject);
    procedure actUserUpPassExecute(Sender: TObject);
  private
    { Private declarations }
    procedure AddUserList(AUser : TUser);
    procedure OnDoubleClick(Sender: TObject);
  public
    { Public declarations }
    procedure LoadData;
  end;

implementation

uses
  uUserControl, uNewUser, uUpUsPass;

{$R *.fmx}

{ TfamUserList }

procedure TfamUserList.actAddUserExecute(Sender: TObject);
var
  AUser : TUser;
begin
  with TfNewUser.Create(nil) do
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
        TDialogService.MessageDialog('新增成功!',TMsgDlgType.mtInformation,
                                    [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0, nil);
      end;
    end;
    Free;
  end;
end;

procedure TfamUserList.actDelExecute(Sender: TObject);
var
  AUser : TUser;
begin
  if strGrdUser.Selected <> -1 then
  begin
    TDialogService.MessageDialog('确认要删除选择的记录吗?',TMsgDlgType.mtInformation,
                                      [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], TMsgDlgBtn.mbOK,
                                       0,
                                       procedure(const AResult: TModalResult)
                                       begin
                                        if AResult = mrYes then
                                        begin
                                          if Assigned(UserControl) then
                                          begin
                                            AUser := UserControl.GetUserInfo(strGrdUser.Cells[0, strGrdUser.Selected]);
                                            if Assigned(AUser) then
                                            begin
                                              UserControl.DelUser(AUser);
                                              LoadData;
                                              TDialogService.MessageDialog('删除用户成功!',TMsgDlgType.mtInformation,
                                                                            [], TMsgDlgBtn.mbOK, 0, nil);
                                            end;
                                          end;
                                        end;
                                       end);
  end;
end;

procedure TfamUserList.actEditUserExecute(Sender: TObject);
var
  AUser : TUser;
  nIndex : Integer;
  sUserName : string;
begin
  nIndex := strGrdUser.Selected;
  if nIndex <> -1 then
  begin
    sUserName := strGrdUser.Cells[0, nIndex];
    if Assigned(UserControl) then
    begin
      AUser := UserControl.GetUserInfo(sUserName);
      if Assigned(AUser) then
      begin
        with TfNewUser.Create(nil) do
        begin
          ShowInfo(AUser);
          if ShowModal = mrOk then
          begin
            SaveInfo;
            UserControl.SaveUser(AUser);
            TDialogService.MessageDialog('编辑成功!',TMsgDlgType.mtInformation,
                                      [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0, nil);
          end;
          Free;
        end;
      end;
    end;
  end
  else
  begin
    TDialogService.MessageDialog('请选择需要编辑的用户!',TMsgDlgType.mtInformation,
                                    [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0, nil);
  end;
end;

procedure TfamUserList.actRefreshExecute(Sender: TObject);
begin
  LoadData;
end;

procedure TfamUserList.actUserUpPassExecute(Sender: TObject);
var
  AUser : TUser;
begin
  if strGrdUser.Selected <> -1 then
  begin
    if Assigned(UserControl) then
    begin
      AUser := UserControl.GetUserInfo(strGrdUser.Cells[0, strGrdUser.Selected]);
      UserControl.UserUpass(AUser);
    end;
  end;
end;

procedure TfamUserList.AddUserList(AUser: TUser);
var
  nGrdRowNum : Integer;
begin
  strGrdUser.RowCount := strGrdUser.RowCount + 1;
  nGrdRowNum := strGrdUser.RowCount - 1;

  with strGrdUser, AUser do
  begin
    Cells[0, nGrdRowNum] := LoginName;
    Cells[1, nGrdRowNum] := FullName;
    Cells[2, nGrdRowNum] := Description;
  end;
end;

procedure TfamUserList.LoadData;
var
  i : integer;
begin
  strGrdUser.RowCount := 0;
  strGrdUser.OnDblClick := OnDoubleClick;
  if Assigned(UserControl) then
  begin
    with UserControl.UserList do
    begin
      for i := 0 to Count - 1 do
        AddUserList(TUser(Objects[i]));
    end;
  end;
end;

procedure TfamUserList.OnDoubleClick(Sender: TObject);
begin
  actEditUserExecute(nil);
end;

end.
