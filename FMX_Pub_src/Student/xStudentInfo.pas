{===============================================================================
  Copyright(c) 2014, 北京北研兴电力仪表有限责任公司
  All rights reserved.

  学员信息单元

  + TStudentInfo  学员信息类

===============================================================================}
unit xStudentInfo;

interface

uses
  System.Classes, System.SysUtils;

type
  /// <summary>
  /// 学员信息类
  /// </summary>
  TStudentInfo = class(TPersistent)
  private
    FstuTel: string;
    FstuNumber: LongInt;
    FstuSex: string;
    FstuArea: string;
    FstuLogin: string;
    FstuName: string;
    FstuPwd: string;
    FstuIdCard: string;
    FstuNote2: string;
    FstuNote1: string;
  public
    /// <summary>
    /// 学员编号
    /// </summary>
    property stuNumber : LongInt read FstuNumber write FstuNumber;

    /// <summary>
    /// 学员姓名
    /// </summary>
    property stuName : string read FstuName write FstuName;

    /// <summary>
    /// 性别
    /// </summary>
    property stuSex : string read FstuSex write FstuSex;

    /// <summary>
    /// 身份证号
    /// </summary>
    property stuIdCard : string read FstuIdCard write FstuIdcard;

    /// <summary>
    /// 登录名
    /// </summary>
    property stuLogin : string read FstuLogin write FstuLogin;

    /// <summary>
    /// 登录密码
    /// </summary>
    property stuPwd : string read FstuPwd write FstuPwd;

    /// <summary>
    /// 所在区域
    /// </summary>
    property stuArea : string read FstuArea write FstuArea;

    /// <summary>
    /// 联系电话
    /// </summary>
    property stuTel : string read FstuTel write FstuTel;

    /// <summary>
    /// 备注1
    /// </summary>
    property stuNote1 : string read FstuNote1 write FstuNote1;

    /// <summary>
    /// 备注2
    /// </summary>
    property stuNote2 : string read FstuNote2 write FstuNote2;

    constructor Create;

    /// <summary>
    /// 复制对象
    /// </summary>
    procedure Assign(Source : TPersistent); override;

    /// <summary>
    /// 初始化数据
    /// </summary>
    procedure stuDataInio;
  end;
implementation

{ TStuDentInfo }

procedure TStuDentInfo.Assign(Source: TPersistent);
begin
  Assert(Source is TStuDentInfo);

  FstuTel    := TStuDentInfo(Source).stuTel;
  FstuNumber := TStuDentInfo(Source).stuNumber;
  FstuSex    := TStuDentInfo(Source).stuSex;
  FstuArea   := TStuDentInfo(Source).stuArea;
  FstuLogin  := TStuDentInfo(Source).stuLogin;
  FstuName   := TStuDentInfo(Source).stuName;
  FstuPwd    := TStuDentInfo(Source).stuPwd;
  FstuIdCard := TStuDentInfo(Source).stuIdCard;
  FstuNote2  := TStuDentInfo(Source).stuNote2;
  FstuNote1  := TStuDentInfo(Source).stuNote1;
end;

constructor TStuDentInfo.Create;
begin
  stuDataInio;
end;

procedure TStuDentInfo.stuDataInio;
begin
  FstuTel    := '';
  FstuNumber := 0;
  FstuSex    := '';
  FstuArea   := '';
  FstuLogin  := '';
  FstuName   := '';
  FstuPwd    := '';
  FstuIdCard := '';
  FstuNote2  := '';
  FstuNote1  := '';
end;

end.
