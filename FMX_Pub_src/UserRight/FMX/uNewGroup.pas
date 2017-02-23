unit uNewGroup;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
  FMX.Grid.Style, FMX.StdCtrls, FMX.Grid, FMX.ScrollBox, FMX.ListBox, FMX.Edit,
  FMX.Controls.Presentation, FMX.TabControl, FMX.Layouts, uUserInfo;

type
  TfNewGroup = class(TForm)
    pgc1: TTabControl;
    ts1: TTabItem;
    Label1: TLabel;
    Label2: TLabel;
    edtGroupName: TEdit;
    edtGroupDesc: TEdit;
    ts2: TTabItem;
    Label6: TLabel;
    Label7: TLabel;
    lblNotice: TLabel;
    Button1: TButton;
    Button2: TButton;
    lstAllRight: TListBox;
    lstCurrRight: TListBox;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
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
  fNewGroup: TfNewGroup;

implementation

uses
  uUserControl, xFunction, FMX.DialogService;

{$R *.fmx}

{ TfNewGroup }

procedure TfNewGroup.Button1Click(Sender: TObject);
  procedure CheckUserGroup;
  begin
    if UserControl.CheckUGNameExists(edtGroupName.Text) then
    begin
      TDialogService.MessageDialog('该组名已存在!',TMsgDlgType.mtInformation,
                                    [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0, nil);
      pgc1.TabIndex := 0;
      edtGroupName.SetFocus;
    end
    else
      ModalResult := mrOk;
  end;
begin
  if edtGroupName.Text = '' then
  begin
    TDialogService.MessageDialog('组名不能为空!',TMsgDlgType.mtInformation,
                                    [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0, nil);
    pgc1.TabIndex := 0;
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

procedure TfNewGroup.Button3Click(Sender: TObject);
begin
  MoveToGroupList(lstAllRight, lstCurrRight);
end;

procedure TfNewGroup.Button4Click(Sender: TObject);
begin
  MoveToGroupList(lstCurrRight, lstAllRight);
end;

procedure TfNewGroup.Button5Click(Sender: TObject);
begin
  MoveAll(lstAllRight, lstCurrRight);
end;

procedure TfNewGroup.Button6Click(Sender: TObject);
begin
  MoveAll(lstCurrRight, lstAllRight);
end;

procedure TfNewGroup.MoveAll(AFromList, AToList: TListBox);
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

procedure TfNewGroup.MoveToGroupList(AFromList, AToList: TListBox);
var
  I: Integer;
begin
  for I := 0 to AFromList.Count - 1 do
  if AFromList.ItemIndex = i then
  begin
    AToList.Items.AddObject(AFromList.Items.Strings[I],
      AFromList.Items.Objects[I]);
    AFromList.Items.Delete(I);
    Break;
  end;
end;

procedure TfNewGroup.SaveInfo;
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

procedure TfNewGroup.ShowAllRights;
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

procedure TfNewGroup.ShowGroupRights;
var
  i: Integer;
  nIndex : Integer;
begin
  for i := 0 to FUserGroup.Rights.Count - 1 do
  begin
    nIndex := lstAllRight.Items.IndexOf( FUserGroup.Rights[ i ] );

    if nIndex <> -1 then
      lstAllRight.ItemIndex := nIndex;

    MoveToGroupList( lstAllRight, lstCurrRight );
  end;
end;

procedure TfNewGroup.ShowInfo(AUserGroup: TUserGroup);
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
          lblNotice.Text := '系统内置，不能编辑修改';
          lblNotice.Visible := True;
        end;
      end;
    end;
  end;
end;

end.
