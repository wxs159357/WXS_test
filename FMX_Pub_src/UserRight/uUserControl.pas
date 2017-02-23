unit uUserControl;

interface

uses
  System.Classes, system.SysUtils, uUserInfo, uUserAction;

type
  /// <summary>
  /// 登录结果
  /// </summary>
  TUA_LOGIN_STATUS = ( uaUserNotExist, uaWrongPassword, uaSucceed );

type
  TUserControl = class
  private
    FUserGroupList: TStringList;
    FUserList: TStringList;
    FUserRightList: TStringList;
    /// <summary>
    /// 用户登录成功后的列表
    /// </summary>
    FUserLognList : TStringList;
    FUserAction : TUserAction;
    FUserInfo: TUser;
    function GetUserList: TStringList;
    function GetUserRightList: TStringList;
    function GetUserGroupList: TStringList;
  public
    constructor Create;
    destructor Destroy;override;

    /// <summary>
    /// 有户列表
    /// </summary>
    property UserList : TStringList read GetUserList;

    /// <summary>
    /// 加载用户列表
    /// </summary>
    procedure LoadUserList;

    /// <summary>
    /// 保存用户(新增/编辑)
    /// </summary>
    procedure SaveUser(AUser : TUser);

    /// <summary>
    /// 删除用户
    /// </summary>
    procedure DelUser(nUname : string);overload;
    procedure DelUser(AUser : TUser);overload;

    /// <summary>
    /// 权限列表
    /// </summary>
    property UserRightList : TStringList read GetUserRightList;

    /// <summary>
    /// 加载有户权限列表
    /// </summary>
    procedure LoadUserRightList;

    /// <summary>
    /// 保存用户权限(新增/编辑)
    /// </summary>
    procedure SaveUserRight(AUserRight : TUserRight);

    /// <summary>
    /// 删除用户权限
    /// </summary>
    procedure DelUserRight(sUserRightName: string);overload;
    procedure DelUserRight(AUserRight : TUserRight);overload;

    /// <summary>
    /// 有户组列表
    /// </summary>
    property UserGroupList : TStringList read GetUserGroupList;

    /// <summary>
    /// 保存用户组(新增/编辑)
    /// </summary>
    procedure SaveUserGroup(AUserGroup : TUserGroup);

    /// <summary>
    /// 删除用户组
    /// </summary>
    procedure DelUserGroup(sUserGroupName : string);overload;
    procedure DelUserGroup(AUserGroup : TUserGroup);overload;

    /// <summary>
    /// 通过UserName获取用户信息
    /// </summary>
    function GetUserInfo(sUserName : string) : TUser;

    /// <summary>
    /// 加载有户组列表
    /// </summary>
    procedure LoadUserGroupList;

    /// <summary>
    /// 通过GroupName获取组信息
    /// </summary>
    function GetGroupInfo(sGroupName : string) : TUserGroup;

    /// <summary>
    /// 加载所有列表
    /// </summary>
    procedure LoadUserAllList;

    /// <summary>
    /// 用户信息
    /// </summary>
    property UserInfo : TUser read FUserInfo write FUserInfo;

    /// <summary>
    /// 登录
    /// </summary>
    function Login( const sLName, sLPassword : string ) : TUA_LOGIN_STATUS;

    /// <summary>
    /// 注销
    /// </summary>
    procedure LogOut;

    /// <summary>
    /// 判断权限是否存在
    /// </summary>
    function RightExist( sRightName : string ) : Boolean;

    /// <summary>
    /// 用户名是否存在
    /// </summary>
    function CheckUNameExists(sUname : string) : Boolean;

    /// <summary>
    /// 用户名是否存在
    /// </summary>
    function CheckUGNameExists(sUGname : string) : Boolean;

    /// <summary>
    /// 修改密码
    /// </summary>
    procedure UserUpass;overload; //修改登录用户密码
    procedure UserUpass(AUser : TUser);overload; //管理用户修改密码

  end;

var
  UserControl : TUserControl;

implementation

uses
  xFunction, uUpUsPass, System.UITypes;

{ TUserControl }

function TUserControl.CheckUGNameExists(sUGname: string): Boolean;
begin
  Result := FUserAction.CheckUGNameExists(sUGname);
end;

function TUserControl.CheckUNameExists(sUname: string): Boolean;
begin
  Result := FUserAction.CheckUNameExists(sUname);
end;

constructor TUserControl.Create;
begin
  FUserList := TStringList.Create;
  FUserRightList := TStringList.Create;
  FUserGroupList := TStringList.Create;
  FUserAction := TUserAction.Create;
  FUserLognList := TStringList.Create;
  LoadUserAllList;
end;

procedure TUserControl.DelUser(nUname: string);
var
  nIndex : Integer;
begin
  FUserAction.DelUser(nUname);
  nIndex := FUserList.IndexOf(nUname);
  if nIndex <> -1 then
  begin
    FUserList.Objects[nIndex].Free;
    FUserList.Delete(nIndex);
  end;
end;

procedure TUserControl.DelUser(AUser: TUser);
begin
  if Assigned(AUser) then
    DelUser(AUser.LoginName);
end;

procedure TUserControl.DelUserGroup(sUserGroupName: string);
var
  nIndex : Integer;
begin
  FUserAction.DelUserGroup(sUserGroupName);
  nIndex := FUserGroupList.IndexOf(sUserGroupName);
  if nIndex <> -1 then
  begin
    FUserGroupList.Objects[nIndex].Free;
    FUserGroupList.Delete(nIndex);
  end;
end;

procedure TUserControl.DelUserGroup(AUserGroup: TUserGroup);
begin
  if Assigned(AUserGroup) then
      DelUserGroup(AUserGroup.GroupName);
end;

procedure TUserControl.DelUserRight(sUserRightName: string);
var
  nIndex : integer;
begin
  FUserAction.DelUserRight(sUserRightName);
  nIndex := FUserRightList.IndexOf(sUserRightName);
  if nIndex <> -1 then
  begin
    FUserRightList.Objects[nIndex].Free;
    FUserRightList.Delete(nIndex);
  end;
end;

procedure TUserControl.DelUserRight(AUserRight: TUserRight);
begin
  if Assigned(AUserRight) then
    DelUserRight(AUserRight.RightName);
end;

destructor TUserControl.Destroy;
begin
  ClearStringList(FUserList);
  ClearStringList(FUserRightList);
  ClearStringList(FUserGroupList);
  ClearStringList(FUserLognList);
  FUserList.Free;
  FUserRightList.Free;
  FUserGroupList.Free;
  FUserAction.Free;
  FUserLognList.Free;
  inherited;
end;

function TUserControl.GetGroupInfo(sGroupName: string): TUserGroup;
var
  nIndex : integer;
begin
  Result := nil;
  nIndex := UserGroupList.IndexOf(sGroupName);
  if nIndex <> -1 then
  begin
    Result := TUserGroup(UserGroupList.Objects[nIndex]);
  end;
end;

function TUserControl.GetUserGroupList: TStringList;
begin
  LoadUserGroupList;
  Result := FUserGroupList;
end;

function TUserControl.GetUserInfo(sUserName : string) : TUser;
var
  nIndex : integer;
begin
  Result := nil;
  nIndex := UserList.IndexOf(sUserName);
  if nIndex <> -1 then
  begin
    Result := TUser(UserList.Objects[nIndex]);
  end;
end;

function TUserControl.GetUserList: TStringList;
begin
  LoadUserList;
  Result := FUserList;
end;

function TUserControl.GetUserRightList: TStringList;
begin
  LoadUserRightList;
  Result := FUserRightList;
end;

procedure TUserControl.LoadUserAllList;
begin
  LoadUserList;
  LoadUserRightList;
  LoadUserGroupList;
end;

procedure TUserControl.LoadUserGroupList;
begin
  FUserAction.GetAllUserGroups(FUserGroupList);
end;

procedure TUserControl.LoadUserList;
begin
  FUserAction.GetAllUsers(FUserList);
end;

procedure TUserControl.LoadUserRightList;
begin
  FUserAction.GetAllUserRights(FUserRightList);
end;

function TUserControl.Login(const sLName, sLPassword: string): TUA_LOGIN_STATUS;
begin
  FUserInfo := TUser.Create;

  // 登录
  if not FUserAction.GetUserInfo( sLName, FUserInfo ) then
    Result := uaUserNotExist
  else
  begin
    if UpperCase(GetMD5( sLPassword )) = FUserInfo.Password then
      Result := uaSucceed
    else
      Result := uaWrongPassword;
  end;

  // 登录成功后读取权限
  if Result = uaSucceed then
    FUserAction.GetUserRights( FUserInfo.GroupID, FUserLognList )
  else
    FreeAndNil( FUserInfo );
end;

procedure TUserControl.LogOut;
begin
  if Assigned( FUserInfo ) then
    FreeAndNil( FUserInfo );

  ClearStringList( FUserLognList );
end;

function TUserControl.RightExist(sRightName: string): Boolean;
begin
  result := False;
  if FUserLognList.IndexOf( sRightName ) <> -1 then
    Result := True;
end;

procedure TUserControl.SaveUser(AUser : TUser);
var
  nIndex : Integer;
  FUser :TUser;
begin
  FUserAction.SaveUser(AUser);

  if Assigned(AUser) then
  begin
    nIndex := FUserGroupList.IndexOf(AUser.LoginName);
    if nIndex <> -1 then
    begin
      FUserList.Delete(nIndex);
      FUser := TUser.Create;
      FUser.Assign(AUser);
      FUserGroupList.AddObject(FUser.LoginName, FUser);
    end
    else
    begin
      FUser := TUser.Create;
      FUser.Assign(AUser);
      FUserGroupList.AddObject(FUser.LoginName, FUser);
    end;
  end;
end;

procedure TUserControl.SaveUserGroup(AUserGroup: TUserGroup);
var
  nIndex : Integer;
  AGroupUser :TUserGroup;
begin
  FUserAction.SaveUserGroup(AUserGroup);
  if Assigned(AUserGroup) then
  begin
    nIndex := FUserGroupList.IndexOf(AUserGroup.GroupName);
    if nIndex <> -1 then
    begin
      FUserGroupList.Delete(nIndex);
      AGroupUser := TUserGroup.Create;
      AGroupUser.Assign(AUserGroup);
      FUserGroupList.AddObject(AGroupUser.GroupName, AGroupUser);
    end
    else
    begin
      AGroupUser := TUserGroup.Create;
      AGroupUser.Assign(AUserGroup);
      FUserGroupList.AddObject(AGroupUser.GroupName, AGroupUser);
    end;
  end;
end;

procedure TUserControl.SaveUserRight(AUserRight: TUserRight);
var
  nIndex : integer;
  ARightUser : TUserRight;
begin
  FUserAction.SaveUserRight(AUserRight);

  if Assigned(AUserRight) then
  begin
    nIndex := FUserGroupList.IndexOf(AUserRight.RightName);
    if nIndex <> -1 then
    begin
      TUserGroup(FUserRightList.Objects[nIndex]).Assign(AUserRight);
    end
    else
    begin
      ARightUser := TUserRight.Create;
      ARightUser.Assign(AUserRight);
      FUserGroupList.AddObject(ARightUser.RightName, ARightUser);
    end;
  end;
end;

procedure TUserControl.UserUpass(AUser: TUser);
begin
  if Assigned(AUser) then
  begin
    with TfUpUsPass.Create(nil) do
    begin
      ShowInfo(AUser);
      if ShowModal = mrOk then
      begin
        SaveInfo;
        FUserAction.UpUserPass(AUser);
        HintMessage(0, '修改成功!', '提示');
      end;
      Free;
    end;
  end;
end;

procedure TUserControl.UserUpass;
begin
  UserUpass(UserInfo);
end;

end.
