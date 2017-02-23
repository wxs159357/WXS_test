unit frmUserNew;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, uUserInfo;

type
  TfUserNew = class(TForm)
    pgc1: TPageControl;
    ts1: TTabSheet;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    lbl6: TLabel;
    lbl7: TLabel;
    edtLognName: TEdit;
    edtName: TEdit;
    edtDesc: TEdit;
    edtPass: TEdit;
    edtYesPass: TEdit;
    ts2: TTabSheet;
    lbl4: TLabel;
    lbl5: TLabel;
    cbbGroup: TComboBox;
    lvGroup: TListView;
    btnOK: TButton;
    btnCancel: TButton;
    lblNotice: TLabel;
    procedure btnCancelClick(Sender: TObject);
    procedure cbbGroupChange(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
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
  fUserNew: TfUserNew;

implementation

uses
  uUserControl, xFunction;

{$R *.dfm}

{ TfUserNew }

procedure TfUserNew.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfUserNew.btnOKClick(Sender: TObject);
  procedure CheckUser;
  begin
    if UserControl.CheckUNameExists(Trim(edtLognName.Text)) then
    begin
      MessageBox(0, '该名称已存在，请使用其他名称!', '错误', mrok);
      pgc1.ActivePageIndex := 0;
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
    MessageBox(0, '名称不能为空!', '错误', mrok);
    pgc1.ActivePageIndex := 0;
    edtLognName.SetFocus;
  end
  else if edtPass.Text <> edtYesPass.Text then
  begin
    MessageBox(0, '两次输入的密码不一致，请重新输入!', '错误', mrok);
    pgc1.ActivePageIndex := 0;
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

procedure TfUserNew.cbbGroupChange(Sender: TObject);
begin
  ShowRights;
end;

procedure TfUserNew.SaveInfo;
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

procedure TfUserNew.ShowInfo(AUserInfo: TUser);
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
        lblNotice.Caption := '系统内置，不能编辑修改';
        lblNotice.Visible := True;
      end;
    end;
  end;
end;

procedure TfUserNew.ShowRights;
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
  lvGroup.Clear;

  for i := 0 to oGroup.Rights.Count - 1 do
    with lvGroup.Items.Add, TUserRight( oGroup.Rights.Objects[ i ] ) do
    begin
      Caption := RightName;
      SubItems.Add( Description );
    end;
end;

end.
