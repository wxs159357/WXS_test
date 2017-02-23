unit U_POWER_ANALYSIS_FUN;

interface

uses U_POWER_LIST_INFO, U_CKM_DEVICE;

/// <summary>
/// 功率状态转成成功率信息
/// </summary>
procedure PowerStatusToInfo(APstatus : TPOWER_STATUS; APInfo : TObject);

implementation

procedure PowerStatusToInfo(APstatus : TPOWER_STATUS; APInfo : TObject);
  function GetAngle(dValue : Double) : Double;
  begin
    if dValue >= 360 then
      dValue := dValue - 360;

    if dValue <= -360 then
      dValue := dValue + 360;

    if dValue >= 360 then
      dValue := dValue - 360;

    if dValue <= -360 then
      dValue := dValue + 360;

    Result := dValue;
  end;
begin
  if not (Assigned(APstatus) and Assigned(APInfo)) then
    Exit;

  if APstatus.PowerType = ptThree then
  begin
    if APInfo is TThreePower then
    begin
      TThreePower(APInfo).U12        := APstatus.U1*100;
      TThreePower(APInfo).U32        := APstatus.U3*100;
      TThreePower(APInfo).I1         := APstatus.I1*5;
      TThreePower(APInfo).I3         := APstatus.I3*5;
      TThreePower(APInfo).U12i1      := GetAngle(APstatus.O1);
      TThreePower(APInfo).U32i3      := GetAngle(APstatus.O3);
      TThreePower(APInfo).U12u32     := GetAngle(APstatus.OU1U3);
    end;
  end
  else
  begin
    if APInfo is TFourPower then
    begin
      TFourPower(APInfo).U1      := APstatus.U1*220;
      TFourPower(APInfo).U2      := APstatus.U2*220;
      TFourPower(APInfo).U3      := APstatus.U3*220;
      TFourPower(APInfo).I1      := APstatus.I1*5;
      TFourPower(APInfo).I2      := APstatus.I2*5;
      TFourPower(APInfo).I3      := APstatus.I3*5;
      TFourPower(APInfo).U1i1    := GetAngle(APstatus.O1);
      TFourPower(APInfo).U2i2    := GetAngle(APstatus.O2);
      TFourPower(APInfo).U3i3    := GetAngle(APstatus.O3);
      TFourPower(APInfo).U1u2    := GetAngle(APstatus.OU1U2);
      TFourPower(APInfo).U1u3    := GetAngle(APstatus.OU1U3);
      TFourPower(APInfo).U2u3    := GetAngle(APstatus.OU1U3- APstatus.OU1U2);
    end;
  end;
end;

end.
