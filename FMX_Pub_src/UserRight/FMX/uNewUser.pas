unit uNewUser;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit, FMX.ListBox, System.Rtti,
  FMX.Grid.Style, FMX.Grid, FMX.ScrollBox, uUserInfo, FMX.DialogService;

type
  TfNewUser = class(TForm)
    pgc1: TTabControl;
    ts1: TTabItem;
    ts2: TTabItem;
    Label1: TLabel;
    Label2: TLabel;
    lbl6: TLabel;
    Label4: TLabel;
    lbl7: TLabel;
    edtLognName: TEdit;
    edtName: TEdit;
    edtDesc: TEdit;
    edtPass: TEdit;
    edtYesPass: TEdit;
    lblNotice: TLabel;
    Button1: TButton;
    Button2: TButton;
    Label6: TLabel;
    Label7: TLabel;
    cbbGroup: TComboBox;
    lvGroup: TStringGrid;
    strngclmn1: TStringColumn;
    strngclmn2: TStringColumn;
    procedure cbbGroupChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    FUserInfo : TUser;
    procedure ShowRights;
  public
    { Public declarations }
    procedure ShowInfo(AUserInfo : TUser);
    procedure SaveInfo;
  end;

var
  fNewUser: TfNewUser;

implementation

uses
  uUserControl;

{$R *.fmx}

{ TfNewUser }

procedure TfNewUser.Button1Click(Sender: TObject);
  procedure CheckUser;
  begin
    if UserControl.CheckUNameExists(Trim(edtLognName.Text)) then
    begin
      TDialogService.MessageDialog('该名称已存在，请使用其他名称!',TMsgDlgType.mtInformation,
                                    [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0, nil);
      pgc1.TabIndex := 0;
      edtLognName.SetFocus;
    end
    else
    begin
      ModalResult := mrOk;
    end;
  end;
begin
  if Trim(edtLognName.Text) = '' then
  begin
    TDialogService.MessageDialog('名称不能为空!',TMsgDlgType.mtInformation,
                                 [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0, nil);
    pgc1.TabIndex := 0;
    edtLognName.SetFocus;
  end
  else if edtPass.Text <> edtYesPass.Text then
  begin
    TDialogService.MessageDialog('两次输入的密码不一致，请重新输入!',TMsgDlgType.mtInformation,
                                   [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0, nil);
    pgc1.TabIndex := 0;
    edtPass.SetFocus;
  end
  else
  begin
    if Assigned(FUserInfo) then
    begin
      if Assigned(UserControl) then
      begin
        if FUserInfo.ID = -1 then
        begin
          CheckUser;
        end
        else
        begin
          if FUserInfo.Embedded then
            ModalResult := mrCancel
          else
          begin
            if FUserInfo.LoginName <> Trim(edtLognName.Text) then
              CheckUser
            else
              ModalResult := mrOk;
          end;
        end;
      end;
    end;
  end;
end;

procedure TfNewUser.Button2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfNewUser.cbbGroupChange(Sender: TObject);
begin
  ShowRights;
end;

procedure TfNewUser.SaveInfo;
begin
  if Assigned(FUserInfo) then
  begin
    FUserInfo.LoginName   := Trim( edtLognName.Text );
    FUserInfo.FullName    := edtName.Text;
    FUserInfo.Description := edtDesc.Text;

    if edtPass.Visible then
      FUserInfo.Password  := Trim( edtPass.Text ) ;

    with cbbGroup do
      FUserInfo.GroupID := TUserGroup( Items.Objects[ ItemIndex ] ).ID;
  end;
end;

procedure TfNewUser.ShowInfo(AUserInfo: TUser);
var
  AGroupList : TStringList;
  i : integer;
begin
  FUserInfo := AUserInfo;
  if Assigned(FUserInfo) then
  begin
    if Assigned(UserControl) then
    begin
      AGroupList := UserControl.UserGroupList;
      cbbGroup.Clear;
      cbbGroup.Items.Clear;
      for i := 0 to AGroupList.Count - 1 do
      begin
        cbbGroup.Items.AddObject(TUserGroup(AGroupList.Objects[i]).GroupName,
                                  AGroupList.Objects[i]);
      end;
      if cbbGroup.Items.Count > 0 then
        cbbGroup.ItemIndex := 0;

      edtLognName.Text := FUserInfo.LoginName;
      edtName.Text := FUserInfo.FullName;
      edtDesc.Text := FUserInfo.Description;

      if FUserInfo.ID <> -1 then
      begin

        for i := 0 to AGroupList.Count - 1 do
        begin
          with TUserGroup(AGroupList.Objects[i]) do
          begin
            if FUserInfo.GroupID = ID then
            begin
              cbbGroup.ItemIndex := i;
              Break;
            end;
          end;
        end;

        // 隐藏密码修改
        lbl6.Visible := False;
        lbl7.Visible := False;
        edtPass.Visible := False;
        edtYesPass.Visible := False;
      end;

      ShowRights;

      if FUserInfo.Embedded then
      begin
        ts1.Enabled := False;
        ts2.Enabled := False;
        lblNotice.Text := '系统内置，不能编辑修改';
        lblNotice.Visible := True;
      end;
    end;
  end;
end;

procedure TfNewUser.ShowRights;
var
  oGroup : TUserGroup;
  i : Integer;
begin
  // 读取当前组的权限
  if cbbGroup.ItemIndex = -1 then
    Exit;

  with cbbGroup do
    oGroup := TUserGroup( Items.Objects[ ItemIndex ] );

  // 显示权限
  lvGroup.RowCount := 0;

  for i := 0 to oGroup.Rights.Count - 1 do
    with lvGroup, TUserRight( oGroup.Rights.Objects[ i ] ) do
    begin
      lvGroup.RowCount := lvGroup.RowCount + 1;
      Cells[0, i] := RightName;
      Cells[1, i] := Description;
    end;
end;

end.
