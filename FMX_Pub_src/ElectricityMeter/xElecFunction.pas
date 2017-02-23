unit xElecFunction;

interface

uses xElecPoint, Math;

/// <summary>
/// 已知 A，B向量，求两个向量的和向量C
/// </summary>
procedure GetCValue(APoint, BPoint, CPoint : TElecPoint);

/// <summary>
/// 已知两边和两边夹角求第三边
/// </summary>
procedure GetOtherValue(APoint, BPoint, CPoint : TElecPoint);

/// <summary>
/// 调整角度 （把角度调整到0-360度范围内）
/// </summary>
function AdjustAngle(dAngle : Double) : Double;

/// <summary>
/// 获取角度所在象限
/// </summary>
function GetQuadrantSN(dAngle : Double) : Integer;

implementation

function GetQuadrantSN(dAngle : Double) : Integer;
var
  dValue : Double;
begin
  dValue := AdjustAngle(dAngle);

  if (dValue >= 0) and (dValue < 90) then
    Result := 1
  else if (dValue >= 90) and (dValue < 180) then
    Result := 2
  else if (dValue >= 180) and (dValue < 270) then
    Result := 3
  else if (dValue >= 270) and (dValue < 360) then
    Result := 4
  else
    Result := 1;
end;

function AdjustAngle(dAngle : Double) : Double;
var
  dTemp : Integer;
begin
  dTemp := Round(dAngle*1000);

  dTemp := dTemp mod 360000;

  Result := dTemp / 1000;

  if Result < 0 then
    Result := 360 + Result;

end;

procedure GetOtherValue(APoint, BPoint, CPoint : TElecPoint);
var
  dABAngle : Double; // ab夹角
  dAngle : Double;
begin
  // 用余弦定理,c^2=a^2+b^2-2abcosC
  // 正弦定理  a/sinA=c/sinC=b/sinB
  if Assigned(APoint) and Assigned(BPoint) and Assigned(CPoint) then
  begin
    dABAngle := BPoint.Angle-APoint.Angle;

    // 值
    CPoint.Value := Sqrt(Sqr(APoint.Value) + Sqr(BPoint.Value) -
      2*APoint.Value*BPoint.Value * Cos(DegToRad(dABAngle)));

    // 角度
    if CPoint.Value* sin(DegToRad(dABAngle)) <> 0 then
    begin
      dAngle := RadToDeg(ArcSin(BPoint.Value/CPoint.Value*sin(DegToRad(dABAngle))));

      CPoint.Angle := APoint.Angle - dAngle;

//      if dAngle > APoint.Angle then
//      begin
//        CPoint.Angle := APoint.Angle + dAngle;
//      end
//      else
//      begin
//        CPoint.Angle := APoint.Angle - dAngle;
//      end;
    end;
  end;
end;

procedure GetCValue(APoint, BPoint, CPoint : TElecPoint);
var
  dABAngle : Double; // ab夹角
  dAngle : Double;
begin
  // 用余弦定理,c^2=a^2+b^2-2abcosC
  // 正弦定理  a/sinA=c/sinC=b/sinB
  if Assigned(APoint) and Assigned(BPoint) and Assigned(CPoint) then
  begin
    dABAngle := abs(180- abs(BPoint.Angle-APoint.Angle));

    // 值
    CPoint.Value := Sqrt(Sqr(APoint.Value) + Sqr(BPoint.Value) -
      2*APoint.Value*BPoint.Value * Cos(DegToRad(dABAngle)));

    // 角度
    if CPoint.Value* sin(DegToRad(dABAngle)) <> 0 then
    begin
      dAngle := RadToDeg(ArcSin(BPoint.Value/CPoint.Value*sin(DegToRad(dABAngle))));



      if BPoint.Angle > APoint.Angle then
      begin
        if abs(BPoint.Angle-APoint.Angle) < 180 then
        begin
          CPoint.Angle := AdjustAngle(APoint.Angle + dAngle);
        end
        else
        begin
          CPoint.Angle := AdjustAngle(APoint.Angle - dAngle);
        end;
      end
      else
      begin
        if abs(BPoint.Angle-APoint.Angle) < 180 then
        begin
          CPoint.Angle := AdjustAngle(APoint.Angle - dAngle);
        end
        else
        begin
          CPoint.Angle := AdjustAngle(APoint.Angle + dAngle);
        end;
      end;

    end;
  end;
end;

end.


