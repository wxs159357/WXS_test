unit U_VECTOR_LIEN_INFO;

interface

uses Classes, SysUtils, Graphics, Windows, GDIPAPI, GDIPOBJ, Math, Dialogs, System.Types;

const
  /// <summary>
  /// 选择线的颜色
  /// </summary>
  C_COLOR_SELECT = $00FF0080;

  C_COLOR_A = $0000E3E3;
  C_COLOR_B = $00009B00;
  C_COLOR_C = $000000B7;


type
  /// <summary>
  /// 向量类型
  /// </summary>
  TVECTOR_TYPE = ( vtVol,       // 电压
                  vtCurrent     // 电流
                  );

/// <summary>
/// 将自定义Vtype转换成字符串
/// </summary>
function GetVTStr(AVT : TVECTOR_TYPE) : string;

function SetVTType(sStr : string):TVECTOR_TYPE;

function GetVTAllStr : string;

/// <summary>
/// 判断点是否在两点之间
/// </summary>
function IsInArea(APoint: TPoint; APoint1,APoint2 : TGPPointF): Boolean;


type
  /// <summary>
  /// 向量信息
  /// </summary>
  TVECTOR_LIEN_INFO = class

  private
    FVAngle: Double;
    FVID: Integer;
    FVValue: Double;
    FVType: TVECTOR_TYPE;
    FIsSelected: Boolean;
    FVColor: TColor;
    FOnChange: TNotifyEvent;
    FCenterPoint: TPoint;
    FScale: Double;
    FVName: string;
    FCanvas: TCanvas;
    FIsDrawPoint: Boolean;
    FIsMainSelect: Boolean;
    FIsOver: Boolean;
    procedure SetIsSelected(const Value: Boolean);

    /// <summary>
    /// 获取终点
    /// </summary>
    function GetLastPoint : TGPPointF;

    function GetTextPoint : TGPPointF;
    function GetVTypeStr: string;
    procedure SetVTypeStr(const Value: string);
    procedure SetVAngle(const Value: Double);

  public
    constructor Create;
    procedure Assign(Source: TVECTOR_LIEN_INFO);

    /// <summary>
    /// 向量ID
    /// </summary>
    property VID : Integer read FVID write FVID;

    /// <summary>
    /// 向量名称
    /// </summary>
    property VName : string read FVName write FVName;

    /// <summary>
    /// 向量类型
    /// </summary>
    property VType : TVECTOR_TYPE read FVType write FVType;

    /// <summary>
    /// 向量类型 字符串形式
    /// </summary>
    property VTypeStr : string read GetVTypeStr write SetVTypeStr;

    /// <summary>
    /// 向量颜色
    /// </summary>
    property VColor : TColor read FVColor write FVColor;

    /// <summary>
    /// 向量值 填写电压电流值
    /// </summary>
    property VValue : Double read FVValue write FVValue;

    /// <summary>
    /// 基本定压电流值
    /// </summary>
    function GetBaseValue : Integer;

    /// <summary>
    /// 角度 原点水平向右为零度，上移为正角度
    /// </summary>
    property VAngle : Double read FVAngle write SetVAngle;

    /// <summary>
    /// 是否被选中
    /// </summary>
    property IsSelected : Boolean read FIsSelected write SetIsSelected;

    /// <summary>
    /// 是否选择的是主向量
    /// </summary>
    property IsMainSelect : Boolean read FIsMainSelect write FIsMainSelect;

    /// <summary>
    /// 鼠标是否在上面
    /// </summary>
    property IsOver : Boolean read FIsOver write FIsOver;

    /// <summary>
    /// 改变事件
    /// </summary>
    property OnChange : TNotifyEvent read FOnChange write FOnChange;

    /// <summary>
    /// 原点
    /// </summary>
    property CenterPoint : TPoint read FCenterPoint write FCenterPoint;

    /// <summary>
    /// 比例  默认为1 100%
    /// </summary>
    property Scale : Double read FScale write FScale;

    /// <summary>
    /// 画布
    /// </summary>
    property Canvas : TCanvas read FCanvas write FCanvas;

    /// <summary>
    /// 是否画点
    /// </summary>
    property IsDrawPoint : Boolean read FIsDrawPoint write FIsDrawPoint;



    /// <summary>
    /// 画图
    /// </summary>
    procedure Draw;

    /// <summary>
    /// 点是否在线上
    /// </summary>
    function IsInLine(APoint: TPoint):Boolean;

  end;

implementation

function IsInArea(APoint: TPoint; APoint1,APoint2 : TGPPointF): Boolean;
var
  iMaxX, iMinX: Single;
  iMaxY, iMinY: Single;
  k, b: Real;
begin
  iMaxX := Max(APoint1.X, APoint2.X);
  iMinX := Min(APoint1.X, APoint2.X);
  iMaxY := Max(APoint1.Y, APoint2.Y);
  iMinY := Min(APoint1.Y, APoint2.Y);

  { 如果是垂直线 }
  if iMaxX - iMinX <= 5 then
  begin
    Result := (Abs(APoint.X - iMaxX) <= 5) and (APoint.Y >= iMinY)
    and (APoint.Y <= iMaxY);
  end
  { 如果是水平线 }
  else if iMaxY - iMinY <=5 then
  begin
    Result := (Abs(APoint.Y - iMaxY) <=5 ) and (APoint.X >= iMinX)
    and (APoint.X <= iMaxX);
  end
  { 根据公式判断 }
  else
  begin
    { 判断公式为：y = k * x + b  斜率：k = (y2 - y1) / (x2 - x1) y轴截距：b = y1 - k * x1 }
    k := (APoint2.Y - APoint1.Y) / (APoint2.X - APoint1.X);
    b := APoint1.Y - k * APoint1.X;
    Result := (APoint.Y <= round(k * APoint.X + b) + 5) and
              (APoint.Y >= round(k * APoint.X + b) - 5);

    if Result then
      Result := (APoint.X >= iMinX) and (APoint.X <= iMaxX) and
                (APoint.Y >= iMinY) and (APoint.Y <= iMaxY);
  end;
end;

/// <summary>
/// 将自定义Vtype转换成字符串
/// </summary>
function GetVTStr(AVT : TVECTOR_TYPE) : string;
begin
  case AVT of
    vtVol: Result := '电压';
    vtCurrent: Result := '电流';
  end;
end;

function SetVTType(sStr : string):TVECTOR_TYPE;
var
  i : TVECTOR_TYPE;
begin
  Result := Low(TVECTOR_TYPE);
  for i := Low(TVECTOR_TYPE) to High(TVECTOR_TYPE) do
  begin
    if GetVTStr(i) = sStr then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function GetVTAllStr : string;
var
  i : TVECTOR_TYPE;
begin
  for i := Low(TVECTOR_TYPE) to High(TVECTOR_TYPE) do
  begin
    Result := Result + GetVTStr(i) + #13#10;
  end;
end;

{ TVECTOR_LIEN_INFO }


procedure TVECTOR_LIEN_INFO.Assign(Source: TVECTOR_LIEN_INFO);
begin
  if Assigned(Source) then
  begin
    FVName           := Source.VName      ;
    FVType           := Source.VType      ;
    FVColor          := Source.VColor     ;
    FVValue          := Source.VValue     ;
    FVAngle          := Source.VAngle     ;
    FIsDrawPoint     := Source.IsDrawPoint;
  end;
end;

constructor TVECTOR_LIEN_INFO.Create;
begin
  FCenterPoint := Point(0, 0);
  FVColor := clRed;
  FVValue := 220;
  FVAngle := 90;
  FVName := '未命名';
  FVType := vtVol;
  FScale := 1;
  FIsDrawPoint := True;
  FIsSelected := False;
  FIsMainSelect := False;
  FIsOver := False;
end;

procedure TVECTOR_LIEN_INFO.Draw;
var
  g: TGPGraphics;
  p: TGPPen;

  ACap : TGPAdjustableArrowCap;
  sf: TGPStringFormat;
  FontSize : Integer;

  // 画字母
  procedure DrawLetter;
  var
    sLastStr : string;
    APoint : TGPPointF;
    sb : TGPSolidBrush;
    font : TGPFont;
  begin

    sb := TGPSolidBrush.Create(ColorRefToARGB(FVColor));

    if Length(FVName) > 0 then
    begin
      sLastStr := Copy(FVName, 2, Length(FVName)-1);
      FontSize := Round(20*Scale);

      font := TGPFont.Create('宋体', FontSize, FontStyleRegular, UnitPixel );

      // 第一个字符和上面的点
      if (FVAngle > 90) and (FVAngle < 270) then
      begin
        if FIsDrawPoint then
        begin
          APoint.X := GetTextPoint.X+0.12*FontSize-FontSize*length(sLastStr)*0.35;
          APoint.y := GetTextPoint.Y-1.3*FontSize;
          g.DrawString('.', 1, font, APoint, sf, sb);
        end;
        APoint.X := GetTextPoint.X-FontSize*length(sLastStr)*0.35;
        APoint.y := GetTextPoint.Y-0.5*FontSize;

        g.DrawString(FVName[1], 1, font, APoint,sf, sb);
      end
      else
      begin
        if FIsDrawPoint then
        begin
          APoint.X := GetTextPoint.X+0.12*FontSize;
          APoint.y := GetTextPoint.Y-1.3*FontSize;
          g.DrawString('.', 1, font, APoint, sb);
        end;

        APoint.X := GetTextPoint.X;
        APoint.y := GetTextPoint.Y-0.5*FontSize;
        g.DrawString(FVName[1], -1, font, APoint,  sb);
      end;

      // 除去第一个字符后面字符都是右下标标示
      font.Free;
      font := TGPFont.Create('宋体', FontSize*0.7, FontStyleRegular, UnitPixel );

      if (FVAngle > 90) and (FVAngle < 270) then
      begin
        APoint.X := GetTextPoint.X-0.1*FontSize;
        APoint.y := GetTextPoint.Y-0.25*FontSize;

        g.DrawString(sLastStr, -1, font, APoint, sf, sb);
      end
      else
      begin
        APoint.X := GetTextPoint.X+ 0.5* FontSize;
        APoint.y := GetTextPoint.Y-0.25*FontSize;

        g.DrawString(sLastStr, -1, font, APoint ,  sb);
      end;

      font.Free;
      sb.Free;
    end;
  end;
begin
  if not Assigned(FCanvas) then
    Exit;

  g := TGPGraphics.Create(FCanvas.Handle);
  g.SetSmoothingMode(TSmoothingMode(2));
  p := TGPPen.Create(MakeColor(255,0,0), 2);
  ACap := TGPAdjustableArrowCap.Create(4,4,False);
  p.SetCustomStartCap(ACap);
  sf := TGPStringFormat.Create;
  sf.SetFormatFlags(StringFormatFlagsDirectionRightToLeft);

  // 画字母
  DrawLetter;

  //画向量
  if FIsSelected then
  begin
    p.SetColor(ColorRefToARGB(C_COLOR_SELECT));
  end
  else
  begin
    p.SetColor(ColorRefToARGB(FVColor));
  end;

  if FIsOver then
    p.SetWidth(3)
  else
    p.SetWidth(2);

  g.DrawLine(p, GetLastPoint.X ,GetLastPoint.Y,FCenterPoint.X,FCenterPoint.Y);



  g.Free;
  p.Free;
  ACap.free;
  sf.Free;
end;

function TVECTOR_LIEN_INFO.GetBaseValue: Integer;
begin
  if FVType = vtCurrent then
    Result := 6
  else
    Result := 220;
end;

function TVECTOR_LIEN_INFO.GetLastPoint: TGPPointF;
begin
  Result := MakePoint(FCenterPoint.X + (FVValue/GetBaseValue)*Cos(DegToRad(FVAngle ))*100*FScale,
    FCenterPoint.Y - (FVValue/GetBaseValue)*Sin(DegToRad(FVAngle ))*100*FScale);

end;

function TVECTOR_LIEN_INFO.GetTextPoint: TGPPointF;
begin
  Result := MakePoint(FCenterPoint.X + (FVValue/GetBaseValue)*Cos(DegToRad(FVAngle ))*110*FScale,
    FCenterPoint.Y - (FVValue/GetBaseValue)*Sin(DegToRad(FVAngle ))*100*FScale);
end;

function TVECTOR_LIEN_INFO.GetVTypeStr: string;
begin
  Result := GetVTStr(FVType);
end;

function TVECTOR_LIEN_INFO.IsInLine(APoint: TPoint): Boolean;
var
  APoint1,APoint2 : TGPPointF;
begin
  APoint1.X := CenterPoint.X;
  APoint1.Y := CenterPoint.Y;
  APoint2 := GetLastPoint;

  Result := IsInArea(APoint, APoint1, APoint2);
end;

procedure TVECTOR_LIEN_INFO.SetIsSelected(const Value: Boolean);
begin
  FIsSelected := Value;
end;

procedure TVECTOR_LIEN_INFO.SetVAngle(const Value: Double);
  function SetValue(dValue : Double) : Double;
  begin
    if dValue < 0 then
    begin
      Result := dValue + 360;

      if Result < 0 then
        Result := SetValue(Result);
    end
    else
      Result := dValue;
  end;

  function SetValue1(dValue : Double) : Double;
  begin
    if dValue > 360 then
    begin
      Result := dValue - 360;

      if Result > 360 then
        Result := SetValue1(Result)
    end
    else
     Result := dValue;
  end;
begin
  if Value < 0 then
  begin
    FVAngle := SetValue(Value);
  end
  else if Value > 360 then
  begin
    FVAngle := SetValue1(Value);
  end
  else
  begin
    FVAngle := Value;
  end;
end;

procedure TVECTOR_LIEN_INFO.SetVTypeStr(const Value: string);
begin
  FVType := SetVTType(Value);
end;

end.
