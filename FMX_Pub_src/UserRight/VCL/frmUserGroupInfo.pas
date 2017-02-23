unit frmUserGroupInfo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  uUserInfo;

type
  TfUserGroupInfo = class(TForm)
    pgc1: TPageControl;
    ts1: TTabSheet;
    lbl1: TLabel;
    lbl3: TLabel;
    edtGroupName: TEdit;
    edtGroupDesc: TEdit;
    ts2: TTabSheet;
    lbl4: TLabel;
    lbl5: TLabel;
    lstAllRight: TListBox;
    lstCurrRight: TListBox;
    btnAdd: TButton;
    btnDel: TButton;
    btnAddAll: TButton;
    btnDelAll: TButton;
    btnCancel: TButton;
    btnOK: TButton;
    lblNotice: TLabel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDelClick(Sender: TObject);
    procedure btnAddAllClick(Sender: TObject);
    procedure btnDelAllClick(Sender: TObject);
  private
    { Private declarations }
    FUserGroup : TUserGroup;
    procedure ShowAllRights;
    procedure ShowGroupRights;
    procedure MoveToGroupList(AFromList, AToList: TListBox);
    procedure MoveAll(AFromList, AToList: TListBox);
  public
    { Public declarations }
    procedure ShowInfo(AUserGroup : TUserGroup);
    procedure SaveInfo;
  end;

var
  fUserGroupInfo: TfUserGroupInfo;

implementation

uses
  uUserControl, xFunction;

{$R *.dfm}

procedure TfUserGroupInfo.MoveAll(AFromList, AToList: TListBox);
var
  I: Integer;
begin
  for I := AFromList.Count - 1 downto 0 do
  begin
    AToList.Items.AddObject(AFromList.Items.Strings[I],
      AFromList.Items.Objects[I]);
    AFromList.Items.Delete(I);
  end;
end;

procedure TfUserGroupInfo.btnOKClick(Sender: TObject);
  procedure CheckUserGroup;
  begin
    if UserControl.CheckUGNameExists(edtGroupName.Text) then
    begin
      MessageBox(0, '该组名已存在，请使用其他名称!', '错误', mrok);
      pgc1.ActivePageIndex := 0;
      edtGroupName.SetFocus;
    end
    else
      ModalResult := mrOk;
  end;
begin
  if edtGroupName.Text = '' then
  begin
    MessageBox(0, '组名不能为空!', '错误', mrok);
    pgc1.ActivePageIndex := 0;
    edtGroupName.SetFocus;
  end
  else
  begin
    if Assigned(FUserGroup) then
    begin
      if Assigned(UserControl) then
      begin
        if FUserGroup.ID = -1 then
        begin
          CheckUserGroup;
        end
        else
        begin
          if FUserGroup.Embedded then
            ModalResult := mrCancel
          else
          begin
            if FUserGroup.GroupName <> Trim(edtGroupName.Text) then
              CheckUserGroup
            else
              ModalResult := mrOk;
          end;
        end;
      end;
    end;
  end;
end;

procedure TfUserGroupInfo.MoveToGroupList(AFromList, AToList: TListBox);
var
  I: Integer;
begin
  for I := 0 to AFromList.Count - 1 do
  if AFromList.Selected[I] then
  begin
    AToList.Items.AddObject(AFromList.Items.Strings[I],
      AFromList.Items.Objects[I]);
    AFromList.Items.Delete(I);
    Break;
  end;
end;

procedure TfUserGroupInfo.btnAddAllClick(Sender: TObject);
begin
  MoveAll(lstAllRight, lstCurrRight);
end;

procedure TfUserGroupInfo.btnAddClick(Sender: TObject);
begin
  MoveToGroupList(lstAllRight, lstCurrRight);
end;

procedure TfUserGroupInfo.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfUserGroupInfo.btnDelAllClick(Sender: TObject);
begin
  MoveAll(lstCurrRight, lstAllRight);
end;

procedure TfUserGroupInfo.btnDelClick(Sender: TObject);
begin
  MoveToGroupList(lstCurrRight, lstAllRight);
end;

procedure TfUserGroupInfo.SaveInfo;
var
  i : integer;
  ARight : TUserRight;
begin
  if Assigned(FUserGroup) then
  begin
    FUserGroup.GroupName := Trim(edtGroupName.Text);
    FUserGroup.Description := Trim(edtGroupDesc.Text);

    ClearStringList(FUserGroup.Rights);
    for i := 0 to lstCurrRight.Items.Count - 1 do
    begin
      ARight := TUserRight.Create;
      ARight.Assign(TUserRight(lstCurrRight.Items.Objects[i]));
      FUserGroup.Rights.AddObject(ARight.RightName, ARight);
    end;
  end;
end;

procedure TfUserGroupInfo.ShowAllRights;
var
  AUserRightList : TStringList;
  i : integer;
begin
  AUserRightList := UserControl.UserRightList;
  lstAllRight.Clear;
  lstCurrRight.Clear;
  for I := 0 to AUserRightList.Count - 1 do
  begin
    with lstAllRight.Items do
    begin
      AddObject(TUserRight(AUserRightList.Objects[i]).RightName, AUserRightList.Objects[i]);
    end;
  end;
end;

procedure TfUserGroupInfo.ShowGroupRights;
var
  i: Integer;
  nIndex : Integer;
begin
  for i := 0 to FUserGroup.Rights.Count - 1 do
  begin
    nIndex := lstAllRight.Items.IndexOf( FUserGroup.Rights[ i ] );

    if nIndex <> -1 then
      lstAllRight.Selected[ nIndex ] := True;

    MoveToGroupList( lstAllRight, lstCurrRight );
  end;
end;

procedure TfUserGroupInfo.ShowInfo(AUserGroup: TUserGroup);
begin
  FUserGroup := AUserGroup;
  if Assigned(FUserGroup) then
  begin
    if Assigned(UserControl) then
    begin
      edtGroupName.Text := FUserGroup.GroupName;
      edtGroupDesc.Text := FUserGroup.Description;
      ShowAllRights;

      if FUserGroup.ID <> -1 then
      begin
        ShowGroupRights;

        if FUserGroup.Embedded then
        begin
          ts1.Enabled := False;
          ts2.Enabled := False;
          lblNotice.Caption := '系统内置，不能编辑修改';
          lblNotice.Visible := True;
        end;
      end;
    end;
  end;
end;

end.
