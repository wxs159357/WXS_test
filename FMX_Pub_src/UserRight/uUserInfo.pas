unit uUserInfo;

interface

uses
  System.Classes, system.SysUtils, xFunction;

type
  /// <summary>
  /// 用户信息类
  /// </summary>
  TUser = class
  private
    FGroupID: Integer;
    FID: Integer;
    FChangePwd: Boolean;
    FDescription: String;
    FFullName: String;
    FPassword: string;
    FDisabled: Boolean;
    FLoginName: String;
    FEmbedded: Boolean;
  public
    constructor Create;
    property ID          : Integer  read FID          write FID;
    property GroupID     : Integer  read FGroupID     write FGroupID ;
    property LoginName   : String   read FLoginName   write FLoginName ;
    property Password    : string   read FPassword    write FPassword ;
    property FullName    : String   read FFullName    write FFullName ;
    property Description : String   read FDescription write FDescription ;
    property ChangePwd   : Boolean  read FChangePwd   write FChangePwd ;
    property Disabled    : Boolean  read FDisabled    write FDisabled ;
    property Embedded    : Boolean  read FEmbedded    write FEmbedded ;

    /// <summary>
    /// 克隆对象
    /// </summary>
    procedure Assign(Source : TObject);
  end;

type
  /// <summary>
  /// 用户权限
  /// </summary>
  TUserRight = class
  private
    FRightName: String;
    FID: Integer;
    FDescription: String;
  public
    constructor Create;
    property ID           : Integer read FID          write FID;
    property RightName    : String  read FRightName   write FRightName;
    property Description  : String  read FDescription write FDescription;

    /// <summary>
    /// 克隆对象
    /// </summary>
    procedure Assign(Source : TObject);
  end;

type
  /// <summary>
  /// 用户组
  /// </summary>
  TUserGroup = class
  private
    FID: Integer;
    FDescription: String;
    FRights: TStringList;
    FEmbedded: Boolean;
    FGroupName: String;
  public
    property ID           : Integer     read FID          write FID;
    property GroupName    : String      read FGroupName   write FGroupName;
    property Description  : String      read FDescription write FDescription;
    property Embedded     : Boolean     read FEmbedded    write FEmbedded;
    property Rights       : TStringList read FRights      write FRights;

    constructor Create;
    destructor Destroy;override;

    /// <summary>
    /// 克隆对象
    /// </summary>
    procedure Assign(Source : TObject);
  end;
implementation

{ TUserGroup }

constructor TUserGroup.Create;
begin
  FRights     := TStringList.Create;
  FID         := -1;
  FDescription:= EmptyStr;
  FEmbedded   := False;
  FGroupName  := EmptyStr;
end;

destructor TUserGroup.Destroy;
begin
  ClearStringList(FRights);
  FRights.Free;
  inherited;
end;

procedure TUserGroup.Assign(Source: TObject);
var
  i : integer;
  AUserRight : TUserRight;
begin
  Assert(Source is TUserGroup);

  FID           := TUserGroup(Source).ID          ;
  FGroupName    := TUserGroup(Source).GroupName   ;
  FDescription  := TUserGroup(Source).Description ;
  FEmbedded     := TUserGroup(Source).Embedded    ;

  ClearStringList(FRights);

  for i := 0 to TUserGroup(Source).Rights.Count - 1 do
  begin
    AUserRight := TUserRight.Create;
    AUserRight.Assign(TUserRight(TUserGroup(Source).Rights.Objects[i]));
    FRights.AddObject('', AUserRight);
  end;
end;

{ TUser }

procedure TUser.Assign(Source: TObject);
begin
  Assert(Source is TUser);

  FID          := TUser(Source).ID          ;
  FGroupID     := TUser(Source).GroupID     ;
  FLoginName   := TUser(Source).LoginName   ;
  FPassword    := TUser(Source).Password    ;
  FFullName    := TUser(Source).FullName    ;
  FDescription := TUser(Source).Description ;
  FChangePwd   := TUser(Source).ChangePwd   ;
  FDisabled    := TUser(Source).Disabled    ;
  FEmbedded    := TUser(Source).Embedded    ;
end;

constructor TUser.Create;
begin
  FGroupID    := -1;
  FID         := -1;
  FChangePwd  := False;
  FDescription:= EmptyStr;
  FFullName   := EmptyStr;
  FPassword   := EmptyStr;
  FDisabled   := False;
  FLoginName  := EmptyStr;
  FEmbedded   := False;
end;

{ TUserRight }

procedure TUserRight.Assign(Source: TObject);
begin
  Assert(Source is TUserRight);
  FID           := TUserRight(Source).ID          ;
  FRightName    := TUserRight(Source).RightName   ;
  FDescription  := TUserRight(Source).Description ;
end;

constructor TUserRight.Create;
begin
  FRightName  := EmptyStr;
  FID         := -1;
  FDescription:= EmptyStr;
end;

end.
