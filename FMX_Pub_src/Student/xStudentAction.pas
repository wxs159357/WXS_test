{===============================================================================
  Copyright(c) 2014, 北京北研兴电力仪表有限责任公司
  All rights reserved.

  学员信息单元

  + TStudentAction  学员数据库操作类

===============================================================================}
unit xStudentAction;

interface

uses
  System.Classes, System.SysUtils, xDBActionBase, system.Variants, xStudentInfo,
  FireDAC.Stan.Param;

type
  /// <summary>
  /// 学员数据库操作类
  /// </summary>
  TStudentAction = class(TDBActionBase)
  public

    /// <summary>
    /// 保存学员信息
    /// </summary>
    procedure SaveStuData(AStuInfo : TStudentInfo);

    /// <summary>
    /// 获取最大学员编号
    /// </summary>
    function GetStuMaxNumber : Integer;

    /// <summary>
    /// 删除学员
    /// </summary>
    procedure DelStu(nStuNumber : Integer);

    /// <summary>
    /// 清空学员表
    /// </summary>
    procedure ClearStus;

    /// <summary>
    /// 查询所有学员信息
    /// </summary>
    procedure SelStusAll(sList : TStringList);

    /// <summary>
    /// 获取考生
    /// </summary>
    function GetStuInfo( sLogName, sPWD : string; AStuInfo : TStudentInfo) : Boolean;
  end;

implementation

{ TStudentAction }

procedure TStudentAction.DelStu(nStuNumber: Integer);
const
  C_SQL_DELETE = 'delete from SUDENT_INFO where STUNumber = %d';
begin
  if Assigned(FQuery) then
  begin
    FQuery.ExecSQL(Format(C_SQL_DELETE, [nStuNumber]));
  end;
end;

procedure TStudentAction.ClearStus;
const
  C_SQL = 'delete from SUDENT_INFO';
begin
  if Assigned(FQuery) then
    FQuery.ExecSQL(C_SQL);
end;

function TStudentAction.GetStuInfo(sLogName, sPWD: string;
  AStuInfo: TStudentInfo): Boolean;
const
  C_SQL = 'select * from SUDENT_INFO where STULogin = :STULogin and STUPassword = :STUPassword';
begin
//  if sPWD = '' then
//  begin
//    FQuery.SQL.Text := Format(C_SQL, [sLogName, '''']);
//  end
//  else
//  begin
//    FQuery.SQL.Text := Format(C_SQL, [sLogName, sPWD]);
//  end;
//  FQuery.SQL.Text := 'select * from SUDENT_INFO where STULogin = 5 and STUPassword = null';

  FQuery.SQL.Text := C_SQL;
  FQuery.Params.ParamByName('STULogin').Value := sLogName;
  FQuery.Params.ParamByName('STUPassword').Value := sPWD;

  FQuery.Open;

  Result := FQuery.RecordCount > 0;
  if Result then
  begin
    with AStuInfo, FQuery do
    begin
      stuNumber := FieldByName('STUNumber').AsInteger;
      stuName   := FieldByName('STUName').AsString;
      stuSex    := FieldByName('STUSex').AsString;
      stuIdCard := FieldByName('STUIDcard').AsString;
      stuLogin  := FieldByName('STULogin').AsString;
      stuPwd    := FieldByName('STUPassword').AsString;
      stuArea   := FieldByName('STUArea').AsString;
      stuTel    := FieldByName('STUTel').AsString;
      stuNote1  := FieldByName('STURemark1').AsString;
      stuNote2  := FieldByName('STURemark2').AsString;
    end;
  end;
end;

function TStudentAction.GetStuMaxNumber: Integer;
const
  C_SQL_MAX = 'select max(STUNumber) from SUDENT_INFO';
begin
  Result := 1;

  if Assigned(FQuery) then
  begin
    with FQuery do
    begin
      Open(C_SQL_MAX);

      if Fields[0].Value <> null then
      begin
        Result := Fields[0].AsInteger + 1;
      end;
      Close;
    end;
  end;
end;

procedure TStudentAction.SaveStuData(AStuInfo: TStudentInfo);
const
  C_SQL_INSERT  = 'Insert Into SUDENT_INFO(STUNumber,STUName,STUSex,STUIDcard,' +
                  'STULogin,STUPassword,STUArea,STUTel,STURemark1,STURemark2) ' +
                  'values(%d, ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ' +
                  '''%s'', ''%s'', ''%s'')';

  C_SQL_UPDATE  = 'update SUDENT_INFO set STUNumber = %d,STUName = ''%s'',' +
                  'STUSex = ''%s'',STUIDcard = ''%s'',STULogin = ''%s'',STUPassword = ''%s'',' +
                  'STUArea = ''%s'',STUTel = ''%s'',STURemark1 = ''%s'',STURemark2 = ''%s''' +
                  ' where STUNumber = %d';
var
  sSQL : string;
begin
  if Assigned(AStuInfo) then
  begin
    if Assigned(FQuery) then
    begin
      with AStuInfo do
      begin
        sSQL := Format(C_SQL_UPDATE, [stuNumber, stuName, stuSex,
                                                stuIdCard, stuLogin, stuPwd
                                                , stuArea, stuTel, stuNote1
                                                , stuNote2, stuNumber]);
        try
          if FQuery.ExecSQL(sSQL) = 0 then
          begin
            sSQL := Format(C_SQL_INSERT, [stuNumber, stuName, stuSex,
                                                  stuIdCard, stuLogin, stuPwd
                                                  , stuArea, stuTel, stuNote1
                                                  , stuNote2]);
            FQuery.ExecSQL(sSQL);
          end;
        except
          raise Exception.Create('更新失败!');
        end;
      end;
    end;
  end;
end;

procedure TStudentAction.SelStusAll(sList: TStringList);
var
  AStuInio : TStudentInfo;
begin
  if Assigned(sList) then
  begin
    if Assigned(FQuery) then
    begin
      FQuery.Open('select * from SUDENT_INFO');
      if FQuery.RecordCount > 0 then
      begin
        FQuery.First;
        while not FQuery.Eof do
        begin
          AStuInio := TStudentInfo.Create;
          with FQuery, AStuInio do
          begin
            stuNumber := FieldByName('STUNumber').AsInteger;
            stuName   := FieldByName('STUName').AsString;
            stuSex    := FieldByName('STUSex').AsString;
            stuIdCard := FieldByName('STUIDcard').AsString;
            stuLogin  := FieldByName('STULogin').AsString;
            stuPwd    := FieldByName('STUPassword').AsString;
            stuArea   := FieldByName('STUArea').AsString;
            stuTel    := FieldByName('STUTel').AsString;
            stuNote1  := FieldByName('STURemark1').AsString;
            stuNote2  := FieldByName('STURemark2').AsString;
          end;
          sList.AddObject(IntToStr(AStuInio.stuNumber), AStuInio);
          FQuery.Next;
        end;
        FQuery.Close;
      end;
    end;
  end;
end;

end.
