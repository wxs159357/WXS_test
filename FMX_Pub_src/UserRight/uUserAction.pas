unit uUserAction;

interface

uses
  System.Classes, system.SysUtils, xDBActionBase, uUserInfo,
  FireDAC.Stan.Param;

type
  TUserAction = class(TDBActionBase)
  public
    /// <summary>
    /// 读取用户列表
    /// </summary>
    procedure GetAllUsers( Users : TStringList );

    /// <summary>
    /// 读取用户权限列表
    /// </summary>
    procedure GetAllUserRights( UserRights : TStringList);

    /// <summary>
    /// 读取用户组列表
    /// </summary>
    procedure GetAllUserGroups( UserGroups : TStringList);

    /// <summary>
    /// 读取用户的权限
    /// </summary>
    /// <param name="nGroupID">用户组Id</param>
    /// <param name="Rights">权限列表</param>
    procedure GetUserRights( nGroupID : Integer; Rights : TStringList );

    /// <summary>
    /// 保存用户
    /// </summary>
    procedure SaveUser(AUser : TUser);

    /// <summary>
    /// 修改密码
    /// </summary>
    procedure UpUserPass(AUser: TUser);

    /// <summary>
    /// 获取用户MAX(ID)
    /// </summary>
    function GetMaxUserId : Integer;

    /// <summary>
    /// 删除用户
    /// </summary>
    procedure DelUser(nUname : string);

    /// <summary>
    /// 用户名是否存在
    /// </summary>
    function CheckUNameExists(sUname : string) : Boolean;

    /// <summary>
    /// 根据Name获取用户信息
    /// </summary>
    function GetUserInfo(sUname : string;AUser : TUser) : Boolean;

    /// <summary>
    /// 保存用户权限
    /// </summary>
    procedure SaveUserRight(AUserRight : TUserRight);

    /// <summary>
    /// 获取用户权限MAX(ID)
    /// </summary>
    function GetMaxUserRightId : Integer;

    /// <summary>
    /// 删除用户权限
    /// </summary>
    procedure DelUserRight(sUserRightName: string);

    /// <summary>
    /// 保存用户组(新增/编辑)
    /// </summary>
    procedure SaveUserGroup(AUserGroup : TUserGroup);

    /// <summary>
    /// 删除用户组
    /// </summary>
    procedure DelUserGroup(sUserGroupName : string);

    /// <summary>
    /// 获取用户组最大ID
    /// </summary>
    function GetMaxUserGroupId : Integer;

    /// <summary>
    /// 检查组是否存在
    /// </summary>
    function CheckUGNameExists(sUGname: string): Boolean;
  end;

implementation

uses
  xFunction;

{ TUserAction }

function TUserAction.CheckUGNameExists(sUGname: string): Boolean;
const
  C_SQLSEL = 'select count(*) from UA_GROUP where G_Name = ''%s''';
begin
  Result := False;
  if Assigned(FQuery) then
  begin
    with FQuery do
    begin
      Open(Format(C_SQLSEL, [sUGname]));
      Result := Fields[0].AsInteger > 0;
      Close;
    end;
  end;
end;

function TUserAction.CheckUNameExists(sUname: string) : Boolean;
const
  C_SQLSEL = 'select count(*) from UA_USER where U_NAME = ''%s''';
begin
  Result := False;
  if Assigned(FQuery) then
  begin
    with FQuery do
    begin
      Open(Format(C_SQLSEL, [sUname]));
      Result := Fields[0].AsInteger > 0;
      Close;
    end;
  end;
end;

procedure TUserAction.DelUser(nUname: string);
const
  C_SQL_USER = 'delete from UA_USER where U_NAME= ''%s''';
begin
  if Assigned(FQuery) then
  begin
    with FQuery do
    begin
      ExecSQL(Format(C_SQL_USER, [nUname]));
    end;
  end;
end;

procedure TUserAction.DelUserGroup(sUserGroupName: string);
const
  C_SQL_RIGHTS = 'delete from UA_GROUP_RIGHT where G_ID in' +
                  '(select G_ID from UA_GROUP where G_NAME = ''%s'')';
  C_SQL_GROUP = 'delete from UA_GROUP where G_NAME=''%s''';
begin
  if Assigned(FQuery) then
  begin
    with FQuery do
    begin
      ExecSQL(Format(C_SQL_RIGHTS, [sUserGroupName]));
      ExecSQL(Format(C_SQL_GROUP, [sUserGroupName]));
    end;
  end;
end;

procedure TUserAction.DelUserRight(sUserRightName: string);
const
  C_SQL_USER = 'delete from UA_RIGHT where R_NAME = ''%s''';
begin
  if Assigned(FQuery) then
  begin
    with FQuery do
    begin
      ExecSQL(Format(C_SQL_USER, [sUserRightName]));
    end;
  end;
end;

procedure TUserAction.GetAllUserGroups(UserGroups: TStringList);
const
  C_SEL_USR = 'select * from UA_GROUP order by G_ID';
var
  AUserGroup : TUserGroup;
  i : Integer;
begin
  if Assigned(FQuery) then
  begin
    if Assigned(UserGroups) then
    begin
      ClearStringList(UserGroups);
      with FQuery do
      begin
        Open(C_SEL_USR);
        First;
        while not Eof do
        begin
          AUserGroup := TUserGroup.Create;
          with AUserGroup do
          begin
            Id          := FieldByName( 'G_ID'          ).AsInteger;
            GroupName   := FieldByName( 'G_NAME'        ).AsString;
            Description := FieldByName( 'G_DESCRIPTION' ).AsString;
            Embedded    := FieldByName( 'G_EMBEDDED' ).AsBoolean;
          end;
          UserGroups.AddObject(AUserGroup.GroupName, AUserGroup);
          Next;
        end;
        Close;
      end;
      for i := 0 to UserGroups.Count - 1 do
      begin
        AUserGroup := TUserGroup(UserGroups.Objects[i]);
        GetUserRights(AUserGroup.ID, AUserGroup.Rights);
      end;
    end;
  end;
end;

procedure TUserAction.GetAllUserRights(UserRights: TStringList);
const
  C_SEL_USR = 'select * from UA_RIGHT order by R_ID';
var
  AUserRight : TUserRight;
begin
  if Assigned(FQuery) then
  begin
    if Assigned(UserRights) then
    begin
      ClearStringList(UserRights);
      with FQuery do
      begin
        Open(C_SEL_USR);
        First;
        while not Eof do
        begin
          AUserRight := TUserRight.Create;
          with AUserRight do
          begin
            Id          := FieldByName( 'R_ID'          ).AsInteger;
            RightName   := FieldByName( 'R_NAME'        ).AsString;
            Description := FieldByName( 'R_DESCRIPTION' ).AsString;
          end;
          UserRights.AddObject(AUserRight.RightName, AUserRight);
          Next;
        end;
        Close;
      end;
    end;
  end;
end;

procedure TUserAction.GetAllUsers(Users: TStringList);
const
  C_SEL_USR = 'select * from UA_USER order by U_ID';
var
  AUser : TUser;
begin
  if Assigned(FQuery) then
  begin
    if Assigned(Users) then
    begin
      ClearStringList(Users);
      with FQuery do
      begin
        Open(C_SEL_USR);
        First;
        while not Eof do
        begin
          AUser := TUser.Create;
          with AUser do
          begin
            ID           := FieldByName( 'U_ID'          ).AsInteger;
            GroupID      := FieldByName( 'G_ID'          ).AsInteger;
            LoginName    := FieldByName( 'U_NAME'        ).AsString;
            Password     := FieldByName( 'U_PASSWORD'    ).AsString;
            FullName     := FieldByName( 'U_FULL_NAME'   ).AsString;
            Description  := FieldByName( 'U_DESCRIPTION' ).AsString;
            ChangePwd    := FieldByName( 'U_CHANGE_PWD'  ).AsBoolean;
            Disabled     := FieldByName( 'U_DISABLEED'   ).AsBoolean;
            Embedded     := FieldByName( 'U_EMBEDDED'    ).AsBoolean;
          end;
          Users.AddObject(AUser.LoginName, AUser);
          Next;
        end;
        Close;
      end;
    end;
  end;
end;

function TUserAction.GetMaxUserGroupId: Integer;
const
  C_SEL = 'select max(G_ID) + 1 from UA_GROUP';
  C_SEL1='select  count(*) from UA_GROUP';
begin
  Result := -1;
  if Assigned(FQuery) then
  begin
    with FQuery do
    begin
      Open(C_SEL1);
      if Fields[0].AsInteger = 0 then
      begin
        Result := 1;
        Close;
      end
      else
      begin
        Close;
        Open(C_SEL);
        Result := Fields[0].AsInteger;
        Close;
      end;
    end;
  end;
end;

function TUserAction.GetMaxUserId: Integer;
const
  C_SEL_COU = 'select  max(u_id) + 1 from UA_USER';
  C_SEL_COU1='select  count(*) from UA_USER';
begin
  result := -1;
  if Assigned(FQuery) then
  begin
    with FQuery do
    begin
      Open(C_SEL_COU1);
      if Fields[0].AsInteger = 0 then
      begin
        Result := 1;
        Close;
      end
      else
      begin
        if Active then
          Close;
        Open(C_SEL_COU);
        Result := Fields[0].AsInteger;
        Close;
      end;
    end;
  end;
end;

function TUserAction.GetMaxUserRightId: Integer;
const
  C_SEL_COU = 'select  max(R_ID) + 1 from UA_RIGHT';
  C_SEL_COU1='select  count(*) from UA_RIGHT';
begin
  result := -1;
  if Assigned(FQuery) then
  begin
    with FQuery do
    begin
      Open(C_SEL_COU1);
      if Fields[0].AsInteger = 0 then
      begin
        Close;
        Result := 1;
      end
      else
      begin
        Close;
        Open(C_SEL_COU);
        Result := Fields[0].AsInteger;
        Close;
      end;
    end;
  end;
end;

function TUserAction.GetUserInfo(sUname: string; AUser: TUser) : Boolean;
const
  C_SQLSEL = 'select * from UA_USER where U_NAME = ''%s''';
var
  sSqlText : string;
begin
  Result := False;
  if Assigned(FQuery) then
  begin
    if Assigned(AUser) then
    begin
      sSqlText := Format(C_SQLSEL, [sUname]);
      with FQuery do
      begin
        Open(sSqlText);
        First;
        if RecordCount > 0 then
        begin
          AUser.ID           := FieldByName('U_ID').AsInteger;
          AUser.GroupID      := FieldByName('G_ID').AsInteger;
          AUser.LoginName    := FieldByName('U_NAME').AsString;
          AUser.Password     := FieldByName('U_PASSWORD').AsString;
          AUser.FullName     := FieldByName('U_FULL_NAME').AsString;
          AUser.Description  := FieldByName('U_DESCRIPTION').AsString;
          AUser.ChangePwd    := FieldByName('U_CHANGE_PWD').AsBoolean;
          AUser.Disabled     := FieldByName('U_DISABLEED').AsBoolean;
          AUser.Embedded     := FieldByName('U_EMBEDDED').AsBoolean;
          Result := True;
        end;
        Close;
      end;
    end;
  end;
end;

procedure TUserAction.GetUserRights(nGroupID: Integer; Rights: TStringList);
const
  C_SEL_RIGHT = 'select * from ua_right where r_id in ' +
    '( select r_id from ua_group_right where g_id = %d )';
var
  ARight : TUserRight;
begin
  if Assigned(FQuery) then
  begin
    if Assigned(Rights) then
    begin
      ClearStringList(Rights);
      with FQuery do
      begin
        Open(Format(C_SEL_RIGHT, [nGroupID]));
        First;
        while not Eof do
        begin
          ARight := TUserRight.Create;
          with ARight do
          begin
            ID          := FieldByName('R_ID').AsInteger;
            RightName   := FieldByName('R_NAME').AsString;
            Description := FieldByName('R_DESCRIPTION').AsString;
          end;
          Rights.AddObject(ARight.RightName, ARight);
          Next;
        end;
        Close;
      end;
    end;
  end;
end;

procedure TUserAction.SaveUser(AUser: TUser);
const
  C_UPT_USR = 'update UA_USER set G_ID = %d, U_NAME = :U_NAME, ' +
    'U_FULL_NAME = :U_FULL_NAME, U_DESCRIPTION = :U_DESCRIPTION, ' +
    'U_PASSWORD = :U_PASSWORD, U_CHANGE_PWD = :U_CHANGE_PWD, ' +
    'UPDATE_TIME = :UPDATE_TIME where U_ID = %d';

  C_SAVE_USR = 'insert into UA_USER(U_ID, G_ID, U_NAME, U_FULL_NAME, U_DESCRIPTION, '
           + 'U_PASSWORD, U_CHANGE_PWD, UPDATE_TIME ) values( '
           + '%d, %d, :U_NAME, :U_FULL_NAME, :U_DESCRIPTION, :U_PASSWORD, '
           + ':U_CHANGE_PWD, :UPDATE_TIME) ';
var
  sSQL : string;
begin
  if Assigned(FQuery) then
  begin
    if Assigned(AUser) then
    begin
      if AUser.ID = -1 then
        AUser.ID := GetMaxUserId;

      with FQuery do
      begin
        sSQL := Format(C_UPT_USR, [ AUser.GroupID, AUser.ID]);
        SQL.Text := sSQL;
        ParamByName('U_NAME').Value := AUser.LoginName;
        ParamByName('U_FULL_NAME').Value := AUser.FullName;
        ParamByName('U_DESCRIPTION').Value := AUser.Description;
        ParamByName('U_PASSWORD').Value := UpperCase(AUser.Password);
        ParamByName('U_CHANGE_PWD').Value := AUser.ChangePwd;
        ParamByName('UPDATE_TIME').Value := Now;
        if ExecSQL(sSQL) = 0 then
        begin
          AUser.Password := UpperCase(GetMD5(AUser.Password));
          SQL.Text := Format(C_SAVE_USR, [AUser.ID, AUser.GroupID]);
          ParamByName('U_NAME').Value := AUser.LoginName;
          ParamByName('U_FULL_NAME').Value := AUser.FullName;
          ParamByName('U_DESCRIPTION').Value := AUser.Description;
          ParamByName('U_PASSWORD').Value := AUser.Password;
          ParamByName('U_CHANGE_PWD').Value := AUser.ChangePwd;
          ParamByName('UPDATE_TIME').Value := Now;
          ExecSQL;
        end;
      end;
    end;
  end;
end;

procedure TUserAction.SaveUserGroup(AUserGroup: TUserGroup);
const
  C_UPTGRP = 'update UA_GROUP set G_NAME = :G_NAME, '
           + 'G_DESCRIPTION = :G_DESCRIPTION, UPDATE_TIME = :UPDATE_TIME where '
           + 'G_ID = %d';
  C_INSGRP_RGT = 'insert into UA_GROUP_RIGHT ( G_ID, R_ID, UPDATE_TIME '
               + ') values (  %d, %d, :UPDATE_TIME )';

  C_DEL_RIGHT = 'delete from UA_GROUP_RIGHT where G_ID = %d';
  C_INSGRP = 'insert into UA_GROUP ( G_ID, G_NAME, G_DESCRIPTION, '
           + 'UPDATE_TIME ) values (  %d, :G_NAME, :G_DESCRIPTION, '
           + ':UPDATE_TIME )';
var
  SqlTest : string;
  i : Integer;
begin
  if Assigned(FQuery) then
  begin
    if Assigned(AUserGroup) then
    begin
      if AUserGroup.ID = -1 then
        AUserGroup.ID := GetMaxUserGroupId;

      with FQuery do
      begin
        SqlTest := Format( C_UPTGRP, [ AUserGroup.ID ] );
        SQL.Text := SqlTest;
        ParamByName( 'G_NAME'        ).Value := AUserGroup.GroupName  ;
        ParamByName( 'G_DESCRIPTION' ).Value := AUserGroup.Description;
        ParamByName( 'UPDATE_TIME'   ).Value := Now ;
        if ExecSQL(SqlTest) = 0 then
        begin
          SQL.Text := Format( C_INSGRP, [ AUserGroup.ID ] );
          ParamByName( 'G_NAME'        ).Value := AUserGroup.GroupName  ;
          ParamByName( 'G_DESCRIPTION' ).Value := AUserGroup.Description;
          ParamByName( 'UPDATE_TIME'   ).Value := Now ;
          ExecSQL;
        end;

        SQL.Text := Format( C_DEL_RIGHT, [ AUserGroup.ID ] );
        ExecSQL;

        for i := 0 to AUserGroup.Rights.Count - 1 do
        begin
          with TUserRight( AUserGroup.Rights.Objects[ i ] ) do
          begin
            SQL.Text := Format( C_INSGRP_RGT, [ AUserGroup.ID, ID ] );
            ParamByName( 'UPDATE_TIME'   ).Value := Now ;
            ExecSQL;
          end;
        end;
      end;
    end;
  end;
end;

procedure TUserAction.SaveUserRight(AUserRight: TUserRight);
const
  C_EDIT = 'update UA_RIGHT set R_NAME = :R_NAME,'+
          ' R_DESCRIPTION = :R_DESCRIPTION, UPDATE_TIME = :UPDATE_TIME'+
          ' where R_ID = :R_ID';

  C_INS = 'insert into UA_RIGHT(R_ID, R_NAME, R_DESCRIPTION, UPDATE_TIME)'+
           ' values(:R_ID, :R_NAME, :R_DESCRIPTION, :UPDATE_TIME)';
var
  sSQL : string;
begin
  if Assigned(FQuery) then
  begin
    if Assigned(AUserRight) then
    begin
      if AUserRight.ID = -1 then
        AUserRight.ID := GetMaxUserRightId;

      with FQuery do
      begin
        sSQL := C_EDIT;
        SQL.Text := sSQL;
        ParamByName('R_ID').Value          :=  AUserRight.ID;
        ParamByName('R_NAME').Value        :=  AUserRight.RightName;
        ParamByName('R_DESCRIPTION').Value :=  AUserRight.Description;
        ParamByName('UPDATE_TIME').Value   :=  Now;
        if ExecSQL(sSQL) = 0 then
        begin
          SQL.Text := C_INS;
          ParamByName('R_ID').Value          :=  AUserRight.ID;
          ParamByName('R_NAME').Value        :=  AUserRight.RightName;
          ParamByName('R_DESCRIPTION').Value :=  AUserRight.Description;
          ParamByName('UPDATE_TIME').Value   :=  Now;
          ExecSQL;
        end;
      end;
    end;
  end;
end;

procedure TUserAction.UpUserPass(AUser: TUser);
const
  C_UPT_USR = 'update UA_USER set U_PASSWORD = ''%s'' where U_ID = %d';
begin
  if Assigned(AUser) then
  begin
    if Assigned(FQuery) then
    begin
      with FQuery do
      begin
        ExecSQL(Format(C_UPT_USR, [AUser.Password, AUser.ID]));
      end;
    end;
  end;
end;

end.
