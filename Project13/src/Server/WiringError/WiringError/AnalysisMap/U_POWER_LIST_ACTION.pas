unit U_POWER_LIST_ACTION;

interface

uses Classes, SysUtils, ADODB, U_PUB_DB_CONN, Forms, U_POWER_LIST_INFO;

type
  TPowerListAction = class

  private
    FAdoquery : TADOQuery;
    FMDB : TMDB_CONNECT;

  public
    constructor Create;
    destructor Destroy; override;

    {三相四线}
    /// <summary>
    /// 添加三相四线功率列表
    /// </summary>
    procedure AddFourPower(AFourInfo : TFourPower);

    /// <summary>
    /// 清空三相四线功率列表
    /// </summary>
    procedure ClearFoutPower;

    {三相三线}
    /// <summary>
    /// 添加三相三线功率列表
    /// </summary>
    procedure AddThreePower(AThreeInfo : TThreePower);

    /// <summary>
    /// 清空三相三线功率列表
    /// </summary>
    procedure ClearThreePower;

    /// <summary>
    /// 获取错误列表
    /// </summary>
    procedure GetErrorList(AStdPower : TFourPower; slError : TStringList;
      dAngle : Double); overload;
    procedure GetErrorList(AStdPower : TThreePower; slError : TStringList;
      dAngle : Double); overload;

  end;

implementation

{ TPowerListAction }

procedure TPowerListAction.AddFourPower(AFourInfo: TFourPower);
const
  C_SQL = 'insert into FourPower ( ErrorCode, ErrorCount, U1, U2, U3,' +
           'I1, I2, I3, U1I1, U2I2, U3I3, U1U2, U1U3, U2U3 ) values (' +
           ':ErrorCode, %d, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f, %f,' +
           '%f )';
begin
  if not Assigned(AFourInfo) then
    Exit;

  try
    with AFourInfo, FadoQuery do
    begin
      SQL.Text := Format( C_SQL, [ Errorcount, U1, U2, U3, I1, I2, I3,U1i1, U2i2,
        U3i3, U1u2, U1u3, U2u3 ] );

      Parameters.ParamByName( 'ErrorCode'  ).Value := Errorcode;
    end;

    FadoQuery.ExecSQL;
  finally

  end;
end;

procedure TPowerListAction.AddThreePower(AThreeInfo: TThreePower);
const
  C_SQL = 'insert into ThreePower ( ErrorCode, ErrorCount, U12, U32,' + #13#10 +
 'I1, I3, U12I1, U32I3, U12U32 ) values (  :ErrorCode, %d,' + #13#10 +
 '%f, %f, %f, %f, %f, %f, %f )';
begin
  if not Assigned(AThreeInfo) then
    Exit;

  try
    with AThreeInfo, FadoQuery do
    begin
      SQL.Text := Format( C_SQL, [ Errorcount, U12, U32, I1, I3, U12i1, U32i3, U12u32 ] );

      Parameters.ParamByName( 'ErrorCode'  ).Value := Errorcode;
    end;

    FadoQuery.ExecSQL;
  finally

  end;
end;

procedure TPowerListAction.ClearFoutPower;
begin
  FadoQuery.SQL.Text := 'delete from FourPower';
  FadoQuery.ExecSQL;
end;

procedure TPowerListAction.ClearThreePower;
begin
  FadoQuery.SQL.Text := 'delete from ThreePower';
  FadoQuery.ExecSQL;
end;

constructor TPowerListAction.Create;
begin
  FMDB := TMDB_CONNECT.Create(nil);
  FMDB.FileName := ExtractFilePath( Application.ExeName ) + 'CKPowerList.mdb';

  FAdoquery := FMDB.AdoQuery;
end;

destructor TPowerListAction.Destroy;
begin
  FMDB.Free;
  FAdoquery.Free;

  inherited;
end;

procedure TPowerListAction.GetErrorList(AStdPower: TFourPower;
  slError: TStringList; dAngle : Double);
const
  C_SQL = 'select * from FourPower where U1=%f and U2 = %f and U3 = %f and '+
          'I1=%f and I2 = %f and I3 = %f and U1I1 = %f and U2I2 = %f and '+
          'U3I3 = %f and U1U2 = %f and U1U3 = %f and U2U3 = %f order by ErrorCount';
var
  APower : TFourPower;
begin
  if not (Assigned(AStdPower) and Assigned(slError)) then
    Exit;

  with FAdoquery, AStdPower do
  begin
    SQL.Text := Format(C_SQL, [U1, U2, U3, I1, I2, I3, U1I1, U2I2, U3I3, U1U2,
      U1U3, U2U3]);

    FAdoquery.Open;

    while not FAdoquery.Eof do
    begin
      APower := TFourPower.Create;
      APower.Errorcode  := FAdoquery.FieldByName('ErrorCode').AsString;
      APower.Errorcount := FAdoquery.FieldByName('Errorcount').AsInteger;
      APower.U1   := FAdoquery.FieldByName('U1').AsFloat;
      APower.U2   := FAdoquery.FieldByName('U2').AsFloat;
      APower.U3   := FAdoquery.FieldByName('U3').AsFloat;
      APower.I1   := FAdoquery.FieldByName('I1').AsFloat;
      APower.I2   := FAdoquery.FieldByName('I2').AsFloat;
      APower.I3   := FAdoquery.FieldByName('I3').AsFloat;
      APower.U1i1 := FAdoquery.FieldByName('U1i1').AsFloat;
      APower.U2i2 := FAdoquery.FieldByName('U2i2').AsFloat;
      APower.U3i3 := FAdoquery.FieldByName('U3i3').AsFloat;
      APower.U1u2 := FAdoquery.FieldByName('U1u2').AsFloat;
      APower.U1u3 := FAdoquery.FieldByName('U1u3').AsFloat;
      APower.U2u3 := FAdoquery.FieldByName('U2u3').AsFloat;

      APower.Angle      := dAngle;

      slError.AddObject(FieldByName('ErrorCode').AsString, APower);

      Next;
    end;
    FAdoquery.Close;
  end;
end;

procedure TPowerListAction.GetErrorList(AStdPower: TThreePower;
  slError: TStringList; dAngle : Double);
const
  C_SQL = 'select * from ThreePower where U12=%f and U32 = %f and I1 = %f and '+
          'I3=%f and U12i1 = %f and U32i3 = %f and U12u32 = %f order by ErrorCount';

var
  APower : TThreePower;
begin
  if not (Assigned(AStdPower) and Assigned(slError)) then
    Exit;

  with FAdoquery, AStdPower do
  begin
    SQL.Text := Format(C_SQL, [U12, U32, I1, I3, U12i1, U32i3, U12u32]);

    FAdoquery.Open;

    while not FAdoquery.Eof do
    begin
      APower := TThreePower.Create;
      APower.Errorcode  := FAdoquery.FieldByName('ErrorCode').AsString;
      APower.Errorcount := FAdoquery.FieldByName('Errorcount').AsInteger;
      APower.U12        := FAdoquery.FieldByName('U12').AsFloat;
      APower.U32        := FAdoquery.FieldByName('U32').AsFloat;
      APower.I1         := FAdoquery.FieldByName('I1').AsFloat;
      APower.I3         := FAdoquery.FieldByName('I3').AsFloat;
      APower.U12i1      := FAdoquery.FieldByName('U12i1').AsFloat;
      APower.U32i3      := FAdoquery.FieldByName('U32i3').AsFloat;
      APower.U12u32     := FAdoquery.FieldByName('U12u32').AsFloat;
      APower.Angle      := dAngle;

      slError.AddObject(FieldByName('ErrorCode').AsString, APower);

      Next;
    end;
    FAdoquery.Close;
  end;
end;

end.
