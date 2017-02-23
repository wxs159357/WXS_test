{===============================================================================
  Copyright(c) 2014, 北京北研兴电力仪表有限责任公司
  All rights reserved.

  学员信息单元

  + TStudentControl  学员数信息控制类

===============================================================================}
unit xStudentControl;

interface

uses
  System.Classes, System.SysUtils, xStudentInfo, xStudentAction;

type
  /// <summary>
  /// 学员数信息控制类
  /// </summary>
  TStudentControl = class
  private
    FStuList : TStringList;
    FStuAction : TStudentAction;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// 学员列表
    /// </summary>
    property StuList : TStringList read FStuList write FStuList;

    /// <summary>
    /// 添加学员
    /// </summary>
    procedure AddStu(AStuInfo : TStudentInfo);

    /// <summary>
    /// 修改学员
    /// </summary>
    procedure EditStu(AStuInfo : TStudentInfo);

    /// <summary>
    /// 删除学员
    /// </summary>
    function DelStu(AStuInfo : TStudentInfo) : Boolean; overload;
    function DelStu(nStuNum : Integer) : Boolean; overload;

    /// <summary>
    /// 查询考生（模糊查找）
    /// </summary>
    /// <param name="sKey">输入查询的关键字，登录名/姓名/身份证号</param>
    procedure SearchStus(sKey: string; slStus: TStringList);

    /// <summary>
    /// 根据编号查询学员信息
    /// </summary>
    function SearchStu(sStuNum : Integer) : TStudentInfo;

    /// <summary>
    /// 清空学员表
    /// </summary>
    procedure ClearStus;

    /// <summary>
    /// 是否存在登录的考生 （登录时用，只检查登录名，用户名，或者卡号）
    /// </summary>
    function StuExist(AStuInfo : TStudentInfo) : Boolean;

    /// <summary>
    /// 考生是否存在
    /// </summary>
    /// <param name="nNum">学员编号</param>
    procedure IsExisStu(nStuNum : Integer; slstu: TStringList); overload;
    /// <param name="sStuName">学员姓名</param>
    procedure IsExisStu(sStuName : string; slstu: TStringList); overload;
    /// <param name="sStuLoginName">学员登录名</param>
    ///  <param name="sStuPwd">学员登录名密码</param>
    procedure IsExisStu(sStuLoginName, sStuPwd : string; slstu: TStringList); overload;
  end;

var
  StudentControl : TStudentControl;

implementation

uses
  xFunction;

{ TStudentControl }

procedure TStudentControl.AddStu(AStuInfo: TStudentInfo);
begin
  if Assigned(AStuInfo) then
  begin
    if Assigned(FStuAction) then
    begin
      AStuInfo.stuNumber := FStuAction.GetStuMaxNumber;
      FStuAction.SaveStuData(AStuInfo);
      FStuList.AddObject(IntToStr(AStuInfo.stuNumber), AStuInfo);
    end;
  end;
end;

constructor TStudentControl.Create;
begin
  FStuList := TStringList.Create;
  FStuAction := TStudentAction.Create;
  FStuAction.SelStusAll(FStuList);
end;

function TStudentControl.DelStu(AStuInfo: TStudentInfo) : Boolean;
begin
  result := False;
  if Assigned(AStuInfo) then
    result := DelStu(AStuInfo.stuNumber);
end;

function TStudentControl.DelStu(nStuNum: Integer) : Boolean;
var
  nIndex : Integer;
begin
  Result := False;
  // 释放学员对象
  nIndex := FStuList.IndexOf(IntToStr(nStuNum));
  if nIndex <> -1 then
  begin
     // 删除数据库中记录
    FStuAction.DelStu(nStuNum);
    {$IFDEF MSWINDOWS}
    FStuList.Objects[nIndex].Free;
    {$ENDIF}

   // 删除list
    FStuList.Delete(nIndex);

    Result := True;
  end;
end;

procedure TStudentControl.ClearStus;
begin
  if Assigned(FStuAction) then
  begin
    FStuAction.ClearStus;
    ClearStringList(FStuList);
  end;
end;

destructor TStudentControl.Destroy;
begin
  ClearStringList(FStuList);
  FStuList.Free;

  FStuAction.Free;
  inherited;
end;

procedure TStudentControl.EditStu(AStuInfo: TStudentInfo);
begin
  if Assigned(AStuInfo) then
  begin
    if Assigned(FStuAction) then
    begin
      FStuAction.SaveStuData(AStuInfo);
    end;
  end;
end;

procedure TStudentControl.IsExisStu(nStuNum : Integer; slstu: TStringList);
var
  i: Integer;
begin
  for i := 0 to FStuList.Count - 1 do
  begin
    if TStudentInfo(FStuList.Objects[i]).stuNumber = nStuNum then
    begin
      slstu.AddObject('',FStuList.Objects[i]);
      Break;
    end;
  end;
end;

procedure TStudentControl.IsExisStu(sStuName : string; slstu: TStringList);
var
  i: Integer;
begin
  if Assigned(slstu) then
  begin
    for i := 0 to FStuList.Count - 1 do
    begin
      if TStudentInfo(FStuList.Objects[i]).stuName = sStuName then
      begin
        slstu.AddObject('',FStuList.Objects[i]);
      end;
    end;
  end;
end;

procedure TStudentControl.IsExisStu(sStuLoginName, sStuPwd: string;
  slstu: TStringList);
var
  i: Integer;
begin
  for i := 0 to FStuList.Count - 1 do
  begin
    if (TStudentInfo(FStuList.Objects[i]).stuLogin = sStuLoginName) and
    (TStudentInfo(FStuList.Objects[i]).stuPwd = sStuPwd) then
    begin
      slstu.AddObject('',FStuList.Objects[i]);
    end
  end;
end;

function TStudentControl.SearchStu(sStuNum: Integer): TStudentInfo;
var
  nIndex : integer;
begin
  Result := nil;

  nIndex := FStuList.IndexOf(IntToStr(sStuNum));
  if nIndex <> -1 then
  begin
    Result := TStudentInfo(FStuList.Objects[nIndex]);
  end;
end;

procedure TStudentControl.SearchStus(sKey: string; slStus: TStringList);
var
  i: Integer;
begin
  if Assigned(slStus) then
  begin
    ClearStringList(slStus);
    for i := 0 to FStuList.Count - 1 do
    with TStudentInfo(FStuList.Objects[i]) do
    begin
      //利用函数 Pos 在TStringlist中模糊查找
      if (Pos(sKey,stuLogin)>0) or (Pos(sKey,stuName) >0) or (Pos(sKey,stuIdCard) >0) then
        slStus.AddObject(IntToStr(stuNumber), FStuList.Objects[i]);
    end;
  end;
end;

function TStudentControl.StuExist(AStuInfo: TStudentInfo) : Boolean;
var
  i : integer;
begin
  Result := False;
  if Assigned(AStuInfo) then
  begin
    for i := 0 to FStuList.Count - 1 do
    begin
      with TStudentInfo(FStuList.Objects[i]) do
      begin
        if (stuLogin = AStuInfo.stuLogin ) or (stuName = AStuInfo.stuName) or
          (stuIdCard = AStuInfo.stuIDcard) then
        begin
          Result := True;
          AStuInfo.Assign(TStudentInfo(FStuList.Objects[i]));
          Break;
        end;
      end;
    end;
  end;
end;

end.
