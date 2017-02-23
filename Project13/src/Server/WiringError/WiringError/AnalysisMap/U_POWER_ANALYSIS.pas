unit U_POWER_ANALYSIS;

interface

uses Classes, SysUtils, U_POWER_LIST_INFO, U_POWER_LIST_ACTION, Dialogs;

type
  TIntArray = array of Integer;
type
  TFloatArray = array of Double;

type
  /// <summary>
  /// 功率解析类
  /// </summary>
  TPowerAnalysis = class
  private
    FPowerAction : TPowerListAction;
    /// <summary>
    /// 三相四线所有Φ角度值列表
    /// </summary>
    FFourUIAngle : TIntArray;
    FFourUUAngle : TIntArray;


    /// <summary>
    /// 三线三线所有Φ角度值列表
    /// </summary>
    FThreeUIAngle : TIntArray;
    FThreeUUAngle : TIntArray;

    /// <summary>
    /// 获取最接近的标准角度
    /// </summary>
    function NearStdAngle(arAngle : TIntArray; dAngle: Double):Double;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// 解析角度
    /// </summary>
    procedure AnalysisAngle(APower: TFourPower; dAngle: Double; var arAangle : TFloatArray);overload;
    procedure AnalysisAngle(APower: TThreePower; dAngle: Double; var arAangle : TFloatArray);overload;

    /// <summary>
    /// 解析功率
    /// </summary>
    procedure AnalysisPower(APower: TFourPower; slErrors: TStringList;
      dAngle: Double); overload;
    procedure AnalysisPower(APower: TThreePower; slErrors: TStringList;
      dAngle: Double); overload;

  end;

implementation

{ TPowerAnalysis }


procedure TPowerAnalysis.AnalysisPower(APower: TFourPower;
  slErrors: TStringList; dAngle: Double);
  procedure SetStdPower(AStdP : TFourPower);
    function SetUValue(dValue : Double) : Double;
    begin
      if dValue > 55 then
        Result := 1
      else
        Result := -1;
    end;
    function SetIValue(dValue : Double) : Double;
    begin
      if dValue > 0.5 then
        Result := 1
      else
        Result := -1;
    end;
  begin
    AStdP.Assign(APower);

    with AStdP do
    begin
      U1 := SetUValue(U1);
      U2 := SetUValue(U2);
      U3 := SetUValue(U3);

      I1 := SetIValue(I1);
      I2 := SetIValue(I2);
      I3 := SetIValue(I3);

      if (U1 <> -1) and (I1 <> -1) then
        U1i1 := NearStdAngle(FFourUIAngle, U1i1-dAngle)
      else
        U1i1 := -1;

      if (U2 <> -1) and (I2 <> -1) then
        U2i2 := NearStdAngle(FFourUIAngle, U2i2-dAngle)
      else
        U2i2 := -1;
      if (U3 <> -1) and (I3 <> -1) then
        U3i3 := NearStdAngle(FFourUIAngle, U3i3-dAngle)
      else
        U3i3 := -1;

      if (U1 <> -1) and (U2 <> -1) then
        U1u2 := NearStdAngle(FFourUUAngle, U1u2)
      else
        U1u2 := -1;

      if (U1 <> -1) and (U3 <> -1) then
        U1u3 := NearStdAngle(FFourUUAngle, U1u3)
      else
        U1u3 := -1;

      if (U2 <> -1) and (U3 <> -1) then
        U2u3 := NearStdAngle(FFourUUAngle, U2u3)
      else
        U2u3 := -1;
    end;
  end;

var
  AStdPower : TFourPower;
  arAangle : TFloatArray;
  i : Integer;
begin
  if not Assigned(slErrors) then
    Exit;

  if not Assigned(APower) then
    Exit;

  slErrors.clear;

  // 计算Φ角
  if dAngle = -1 then
    AnalysisAngle(APower, dAngle, arAangle)
  else
  begin
    SetLength(arAangle, 1);
    arAangle[0] := dAngle;
  end;

  AStdPower := TFourPower.Create;
  for i := 0 to Length(arAangle) - 1 do
  begin
    dAngle := arAangle[i];

    // 计算标准化功率
    SetStdPower(AStdPower);

    // 从数据库中查询错误列表
    FPowerAction.GetErrorList(AStdPower, slErrors, dAngle);
  end;

  AStdPower.Free;
end;

procedure TPowerAnalysis.AnalysisAngle(APower: TFourPower; dAngle: Double;
  var arAangle : TFloatArray);
  function AngleExist(nValue : Double) : Boolean;
  var
    j : Integer;
  begin
    Result := False;
    for j := 0 to Length(arAangle) - 1 do
    begin
      if arAangle[j] = nValue then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
var
  i: Integer;
  dValue1, dValue2, dValue3 : Double;
  dValue : Double;
  nCount : Integer;
begin
  SetLength(arAangle, 0);
  for i := 0 to 360 do
  begin
    nCount := 0;
    if (APower.U1 > 30) and (APower.I1 > 0.3) then
      dValue1 := NearStdAngle(FFourUIAngle, APower.U1i1 - i)
    else
      dValue1 := -2;

    if (APower.U2 > 30) and (APower.I2 > 0.3) then
      dValue2 := NearStdAngle(FFourUIAngle, APower.U2i2 - i)
    else
      dValue2 := -2;

    if (APower.U3 > 30) and (APower.I3 > 0.3) then
      dValue3 := NearStdAngle(FFourUIAngle, APower.U3i3 - i)
    else
      dValue3 := -2;

    if (dValue1 <> -1) and (dValue2 <> -1) and (dValue3 <> -1) then
    begin
      dValue := 0;
      if dValue1 <> -2 then
      begin
        dValue := dValue + APower.U1i1 - dValue1;
        Inc(nCount);
      end;

      if dValue2 <> -2 then
      begin
        dValue := dValue + APower.U2i2 - dValue2;
        Inc(nCount);
      end;

      if dValue3 <> -2 then
      begin
        dValue := dValue + APower.U3i3 - dValue3;
        Inc(nCount);
      end;

      if nCount <> 0 then
      begin
        dValue := dValue / nCount;

        if not AngleExist(dValue) then
        begin
          if (dValue >= -60) and (dValue <= 60) then
          begin
            SetLength(arAangle, Length(arAangle) + 1);
            arAangle[Length(arAangle) - 1] := dValue;
          end;
        end;
      end;
    end;
  end;

//var
//  i: Integer;
//  dMax, dMax1, dMax2, dMax3 : Double;
//  nCount : Integer;
//begin
//  dMax1  := 888;
//  dMax2  := 888;
//  dMax3  := 888;
//  nCount := 0;
//  dMax   := 0;
//
//  if (APower.U1 > 55) and (APower.I1 > 0.3) then
//    for i := 0 to Length(FFourUIAngle) - 1 do
//      if Abs(APower.U1i1 - FFourUIAngle[i]) < Abs(dMax1) then
//        dMax1 := APower.U1i1 - FFourUIAngle[i];
//
//  if (APower.U2 > 55) and (APower.I2 > 0.3) then
//    for i := 0 to Length(FFourUIAngle) - 1 do
//      if Abs(APower.U2i2 - FFourUIAngle[i]) < Abs(dMax2) then
//        dMax2 := APower.U2i2 - FFourUIAngle[i];
//
//  if (APower.U3 > 55) and (APower.I3 > 0.3) then
//    for i := 0 to Length(FFourUIAngle) - 1 do
//      if Abs(APower.U3i3 - FFourUIAngle[i]) < Abs(dMax3) then
//        dMax3 := APower.U3i3 - FFourUIAngle[i];
//
//  if dMax1 <> 888 then
//  begin
//    Inc(nCount);
//    dMax := dMax + dMax1;
//  end;
//
//  if dMax2 <> 888 then
//  begin
//    Inc(nCount);
//    dMax := dMax + dMax2;
//  end;
//
//  if dMax3 <> 888 then
//  begin
//    Inc(nCount);
//    dMax := dMax + dMax3;
//  end;
//
//  if ncount <> 0 then
//    dAngle := dMax/ncount
//  else
//    dAngle := 20;
end;

procedure TPowerAnalysis.AnalysisAngle(APower: TThreePower; dAngle: Double;
  var arAangle : TFloatArray);
  function AngleExist(nValue : Double) : Boolean;
  var
    j : Integer;
  begin
    Result := False;
    for j := 0 to Length(arAangle) - 1 do
    begin
      if arAangle[j] = nValue then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
var
  i: Integer;
  dValue1, dValue2 : Double;
  dValue : Double;
  nCount : Integer;
begin
  SetLength(arAangle, 0);
  
  for i := 0 to 360 do
  begin
    nCount := 0;
    if (APower.U12 > 30) and (APower.I1 > 0.3) then
      dValue1 := NearStdAngle(FThreeUIAngle, APower.U12i1 - i)
    else
      dValue1 := -2;

    if (APower.U32 > 30) and (APower.I3 > 0.3) then
      dValue2 := NearStdAngle(FThreeUIAngle, APower.U32i3 - i)
    else
      dValue2 := -2;

    if (dValue1 <> -1) and (dValue2 <> -1) then
    begin
      dValue := 0;
      if dValue1 <> -2 then
      begin
        dValue := dValue + APower.U12i1 - dValue1;
        Inc(nCount);
      end;

      if dValue2 <> -2 then
      begin
        dValue := dValue + APower.U32i3 - dValue2;
        Inc(nCount);
      end;

      if nCount <> 0 then
      begin
        dValue := dValue / nCount;

        if not AngleExist(dValue) then
        begin
          SetLength(arAangle, Length(arAangle) + 1);
          arAangle[Length(arAangle) - 1] := dValue;
        end;
      end;
    end;
  end;

//var
//  i: Integer;
//  dMax, dMax1, dMax2 : Double;
//  nCount : Integer;
//begin
//  dMax1  := 888;
//  dMax2  := 888;
//  nCount := 0;
//  dMax   := 0;
//
//  if (APower.U12 > 30) and (APower.I1 > 0.3) then
//    for i := 0 to Length(FThreeUIAngle) - 1 do
//      if Abs(APower.U12i1 - FThreeUIAngle[i]) < Abs(dMax1) then
//        dMax1 := APower.U12i1 - FThreeUIAngle[i];
//
//  if (APower.U32 > 30) and (APower.I3 > 0.3) then
//    for i := 0 to Length(FThreeUIAngle) - 1 do
//      if Abs(APower.U32i3 - FThreeUIAngle[i]) < Abs(dMax2) then
//        dMax2 := APower.U32i3 - FThreeUIAngle[i];
//
//  if dMax1 <> 888 then
//  begin
//    Inc(nCount);
//    dMax := dMax + dMax1;
//  end;
//
//  if dMax2 <> 888 then
//  begin
//    Inc(nCount);
//    dMax := dMax + dMax2;
//  end;
//
//  if ncount <> 0 then
//    dAngle := dMax/ncount
//  else
//    dAngle := 20;
end;

procedure TPowerAnalysis.AnalysisPower(APower: TThreePower;
  slErrors: TStringList; dAngle: Double);
  procedure SetStdPower(AStdP : TThreePower);
    function SetUValue(dValue : Double) : Double;
    begin
      if dValue > 30 then
        Result := 1
      else
        Result := -1;
    end;
    function SetIValue(dValue : Double) : Double;
    begin
      if dValue > 0.5 then
        Result := 1
      else
        Result := -1;
    end;
  begin
    AStdP.Assign(APower);

    with AStdP do
    begin
      U12 := SetUValue(U12);
      U32 := SetUValue(U32);
      I1 := SetIValue(I1);
      I3 := SetIValue(I3);

      if (U12 <> -1) and (I1 <> -1) then
        U12i1 := NearStdAngle(FThreeUIAngle, U12i1-dAngle)
      else
        U12i1 := -1;

      if (U32 <> -1) and (I3 <> -1) then
        U32i3 := NearStdAngle(FThreeUIAngle, U32i3-dAngle)
      else
        U32i3 := -1;


      if (U12 <> -1) and (U32 <> -1) then
        U12u32 := NearStdAngle(FThreeUUAngle, U12u32)
      else
        U12u32 := -1;
    end;
  end;

var
  AStdPower : TThreePower;
  arAangle : TFloatArray;
  i: Integer;
begin
  if not Assigned(slErrors) then
    Exit;

  if not Assigned(APower) then
    Exit;

  slErrors.clear;

  // 计算Φ角
  if dAngle = -1 then
    AnalysisAngle(APower, dAngle, arAangle)
  else
  begin
    SetLength(arAangle, 1);
    arAangle[0] := dAngle;
  end;

  AStdPower := TThreePower.Create;
  for i := 0 to Length(arAangle) - 1 do
  begin
    dAngle := arAangle[i];

    if (dAngle >= -30) and (dAngle <= 60) then
    begin
      // 计算标准化功率
      SetStdPower(AStdPower);

      // 从数据库中查询错误列表
      FPowerAction.GetErrorList(AStdPower, slErrors, dAngle);
    end;
  end;

  AStdPower.Free;
end;

constructor TPowerAnalysis.Create;
begin
  FPowerAction := TPowerListAction.Create;

  SetLength(FFourUIAngle, 6);
  FFourUIAngle[0] := 300;
  FFourUIAngle[1] := 240;
  FFourUIAngle[2] := 180;
  FFourUIAngle[3] := 120;
  FFourUIAngle[4] := 60;
  FFourUIAngle[5] := 0;

  SetLength(FFourUUAngle, 2);
  FFourUUAngle[0] := 240;
  FFourUUAngle[1] := 120;

  SetLength(FThreeUIAngle, 10);
  FThreeUIAngle[0] := 330;
  FThreeUIAngle[1] := 300;
  FThreeUIAngle[2] := 270;
  FThreeUIAngle[3] := 240;
  FThreeUIAngle[4] := 210;
  FThreeUIAngle[5] := 150;
  FThreeUIAngle[6] := 120;
  FThreeUIAngle[7] := 90;
  FThreeUIAngle[8] := 60;
  FThreeUIAngle[9] := 30;

  SetLength(FThreeUUAngle, 8);
  FThreeUUAngle[0] := 330;
  FThreeUUAngle[1] := 300;
  FThreeUUAngle[2] := 240;
  FThreeUUAngle[3] := 180;
  FThreeUUAngle[4] := 120;
  FThreeUUAngle[5] := 60;
  FThreeUUAngle[6] := 30;
  FThreeUUAngle[7] := 0;
end;

destructor TPowerAnalysis.Destroy;
begin
  FPowerAction.Free;

  inherited;
end;

function TPowerAnalysis.NearStdAngle(arAngle : TIntArray; dAngle: Double): Double;
  function GetValue(dV1, dV2 : Double) : Double;
  begin
    Result := dV1 - dV2;

    if Result > 180 then
      Result := 360- Result;

    if Result < -180 then
      Result := 360 + Result;
  end;
var
  i : Integer;
  dTemp : Double;
  nIndex : Integer;
begin
  dTemp := 888;
  nIndex := -1;

  if dAngle < 0 then
    dAngle := dAngle + 360
  else if dAngle > 360 then
    dAngle := dAngle - 360;

  for i := 0 to Length(arAngle) - 1 do
  begin
    if Abs(GetValue(dAngle, arAngle[i])) < Abs(dTemp) then
    begin
      nIndex := i;
      dTemp := GetValue(dAngle, arAngle[i]);
    end;
  end;

  if Abs(dTemp) < 5 then
    Result := arAngle[nIndex]
  else
    Result := -1;
end;

end.


