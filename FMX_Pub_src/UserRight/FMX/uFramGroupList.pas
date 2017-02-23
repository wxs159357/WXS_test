unit uFramGroupList;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Rtti, FMX.Grid.Style, FMX.Grid, FMX.Controls.Presentation,
  FMX.ScrollBox, uUserInfo, System.Actions, FMX.ActnList, FMX.Menus;

type
  TfamGroupList = class(TFrame)
    strGrdGroup: TStringGrid;
    strngclmn2: TStringColumn;
    strngclmn3: TStringColumn;
    PopupMenu1: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    ActionList1: TActionList;
    actAddGroup: TAction;
    actRefresh: TAction;
    actEditGroup: TAction;
    actDel: TAction;
    procedure actAddGroupExecute(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actEditGroupExecute(Sender: TObject);
    procedure actDelExecute(Sender: TObject);
  private
    { Private declarations }
    procedure AddGroupList(AGroup : TUserGroup);
    procedure OnDoubleClick(Sender: TObject);
  public
    { Public declarations }
    procedure LoadGroupData;
  end;

implementation

uses
  uUserControl, FMX.DialogService, uNewGroup;

{$R *.fmx}

{ TfamGroupList }

procedure TfamGroupList.actAddGroupExecute(Sender: TObject);
var
  AGroup : TUserGroup;
begin
  with TfNewGroup.Create(nil) do
  begin
    AGroup := TUserGroup.Create;
    ShowInfo(AGroup);
    if ShowModal = mrOk then
    begin
      SaveInfo;
      if Assigned(UserControl) then
      begin
        UserControl.SaveUserGroup(AGroup);
        AddGroupList(AGroup);
        TDialogService.MessageDialog('新增成功!',TMsgDlgType.mtInformation,
                                    [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0, nil);
      end;
    end;
    Free;
  end;
end;

procedure TfamGroupList.actDelExecute(Sender: TObject);
var
  AUserGroup : TUserGroup;
begin
  if strGrdGroup.Selected <> -1 then
  begin
    TDialogService.MessageDialog('确认要删除选择的记录吗?', TMsgDlgType.mtInformation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo],
                                  TMsgDlgBtn.mbOK, 0,
                                  procedure(const AResult: TModalResult)
                                  begin
                                    case AResult of
                                      mrYes :
                                        begin
                                          if Assigned(UserControl) then
                                          begin
                                            AUserGroup := UserControl.GetGroupInfo(strGrdGroup.Cells[0, strGrdGroup.Selected]);
                                            if Assigned(AUserGroup) then
                                            begin
                                              UserControl.DelUserGroup(AUserGroup);
                                              LoadGroupData;
                                              TDialogService.MessageDialog('删除组成功!',TMsgDlgType.mtInformation,
                                                                            [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0, nil);
                                            end;
                                          end;
                                        end;
                                    end;
                                  end
                                  );
  end;
end;

procedure TfamGroupList.actEditGroupExecute(Sender: TObject);
var
  AUserGroup : TUserGroup;
begin
  if strGrdGroup.Selected <> -1 then
  begin
    if Assigned(UserControl) then
    begin
      AUserGroup := UserControl.GetGroupInfo(strGrdGroup.Cells[0, strGrdGroup.Selected]);
      if Assigned(AUserGroup) then
      begin
        with TfNewGroup.Create(nil) do
        begin
          ShowInfo(AUserGroup);
          if ShowModal = mrOk then
          begin
            SaveInfo;
            UserControl.SaveUserGroup(AUserGroup);
            strGrdGroup.Cells[0, strGrdGroup.Selected] := AUserGroup.GroupName;
            strGrdGroup.Cells[1, strGrdGroup.Selected] := AUserGroup.Description;
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
    TDialogService.MessageDialog('请选择需要编辑的组!',TMsgDlgType.mtInformation,
                                  [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0, nil);
  end;
end;

procedure TfamGroupList.actRefreshExecute(Sender: TObject);
begin
  LoadGroupData;
end;

procedure TfamGroupList.AddGroupList(AGroup: TUserGroup);
var
  nGrdRowNum : Integer;
begin
  strGrdGroup.RowCount := strGrdGroup.RowCount + 1;
  nGrdRowNum := strGrdGroup.RowCount - 1;

  with strGrdGroup, AGroup do
  begin
    Cells[0, nGrdRowNum] := GroupName;
    Cells[1, nGrdRowNum] := Description;
  end;
end;

procedure TfamGroupList.LoadGroupData;
var
  i : integer;
begin
  strGrdGroup.RowCount := 0;
  strGrdGroup.OnDblClick := OnDoubleClick;
  if Assigned(UserControl) then
  begin
    with UserControl.UserGroupList do
    begin
      for i := 0 to Count - 1 do
        AddGroupList(TUserGroup(Objects[i]));
    end;
  end;
end;

procedure TfamGroupList.OnDoubleClick(Sender: TObject);
begin
  actEditGroupExecute(nil);
end;

end.
