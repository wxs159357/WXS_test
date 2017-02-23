{===============================================================================
  Copyright(c) 2007-2009, 北京北研兴电力仪表有限责任公司
  All rights reserved.

  错误接线，向量图绘制单元
  + TPOWER_PHASE_MAP 向量图绘制
  + TPOWER_PHASE_MAP_COLOR 色彩索引

===============================================================================}

unit U_POWER_PHASE_MAP;

interface

uses SysUtils, Classes, Windows, Graphics, U_WE_EQUATION_DRAW, 
  U_CKM_DEVICE, system.Types, system.UITypes, U_POWER_STATUS;

CONST
  /// <summary>
  /// 画图的最小值
  /// </summary>
  C_DRAW_VALUE_MIN = 0.1;

const
  /// <summary>
  /// 画图时的放大系数
  /// </summary>
  C_DRAW_U_SCALE_RATE = 1.5;
  C_DRAW_I_SCALE_RATE = 1.2;
  
type
  /// <summary>
  /// 色彩索引
  /// </summary>
  TPOWER_PHASE_MAP_COLOR = record
    Background  : Integer;       // 向量图 底色
    DotLine     : Integer;       // 向量图 虚轴颜色

    PhaseLine   : Integer;       // 向量图 三线 相线颜色
    PhaseAB     : Integer;       // 向量图 三线 第一元件
    PhaseCB     : Integer;       // 向量图 三线 第二元件

    PhaseA      : Integer;       // 向量图 四线 第一元件
    PhaseB      : Integer;       // 向量图 四线 第二元件
    PhaseC      : Integer;       // 向量图 四线 第三元件

    Font        : Integer;       // 字体颜色
  end;
var
  PowerPhaseMap : TPOWER_PHASE_MAP_COLOR;

type
  /// <summary>
  /// 向量图
  /// </summary>
  TPOWER_PHASE_MAP = class( TComponent )
  private
    FPowerStatus: TPOWER_STATUS;
    FPower : TCKM_POWER;
    FMapCenter : TPoint;
    FCanvas : TCanvas;
    FRect : TRect;
    FMapColor : TPOWER_PHASE_MAP_COLOR;
    FUnitStep : Integer;
    FPenWidth : Integer;
    FEquationDraw : TWE_EQUATION_DRAW;
    FDefMapColor: TPOWER_PHASE_MAP_COLOR;
    procedure SetDefMapColor(const Value: TPOWER_PHASE_MAP_COLOR);
  protected
    /// <summary>
    /// 画背景
    /// </summary>
    procedure DrawBackground( AHasCrossLine, AHasBorder : Boolean );

    /// <summary>
    /// 画矢量线
    /// </summary>
    procedure DrawLine( APen : TPen; APos : TPoint; ALen, AAngle : Double;
      AHasArrow : Boolean; ASign : string );

    /// <summary>
    /// 画角度
    /// </summary>
    procedure DrawAngle( APen : TPen; APos : TPoint;
      ARadius, AStartAngle, AEndAngle : Double; AHasArrow : Boolean; ASign : string );

    /// <summary>
    /// 画箭头
    /// </summary>
    procedure DrawArrow( APen : TPen; APos : TPoint; AAngle : Double );

    /// <summary>
    /// 角度转换, 公式和画图的角度不同，需要转换
    /// </summary>
    function CovAngle( AValue : Double ) : Double;

    /// <summary>
    /// 画向量图
    /// </summary>
    procedure DrawPhase3Map;
    procedure DrawPhase4Map;

    /// <summary>
    /// 修正电流值
    /// </summary>
    function FixedIValue( Value : Double ) : Double;

    /// <summary>
    /// 修正电压值
    /// </summary>
    function FixedUValue( Value : Double ) : Double;
  public
    /// <summary>
    /// 画布
    /// </summary>
    property Canvas : TCanvas read FCanvas write FCanvas;

    /// <summary>
    /// 绘图的区域
    /// </summary>
    property Rect : TRect read FRect write FRect;

    /// <summary>
    /// 颜色索引
    /// </summary>
    property MapColor : TPOWER_PHASE_MAP_COLOR read FMapColor write FMapColor;

    /// <summary>
    /// 默认颜色索引
    /// </summary>
    property DefMapColor : TPOWER_PHASE_MAP_COLOR read FDefMapColor write SetDefMapColor;

    /// <summary>
    /// 单位长度
    /// </summary>
    property UnitStep : Integer read FUnitStep write FUnitStep;

    /// <summary>
    /// 画笔宽度
    /// </summary>
    property PenWidth : Integer read FPenWidth write FPenWidth;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>
    /// 绘制向量图
    /// </summary>
    procedure DrawPhaseMap( APowerStatus: TPOWER_STATUS; APower : TCKM_POWER );
  end;

implementation

uses U_WE_POWER_ANALYSIS;

{ TPOWER_PHASE_MAP }

function TPOWER_PHASE_MAP.CovAngle(AValue: Double): Double;
begin
  if AValue >= -90 then
    Result := (90 - AValue) / 180 * Pi
  else
    Result := (- AValue - 270) / 180 * Pi;
end;

constructor TPOWER_PHASE_MAP.Create(AOwner: TComponent);
begin
  inherited;
  FEquationDraw := TWE_EQUATION_DRAW.Create( nil );

  with FDefMapColor do
  begin
    Background              := clWhite;
    DotLine                 := $00D1D1D1;
    PhaseLine               := $005E5E5E;
    PhaseAB                 := $003D9DFE;
    PhaseCB                 := clRed    ;
    PhaseA                  := $003D9DFE;
    PhaseB                  := $00009700;
    PhaseC                  := clRed    ;
    Font                    := clBlack;
  end;

  FMapColor := FDefMapColor;
end;

destructor TPOWER_PHASE_MAP.Destroy;
begin
  FEquationDraw.Free;
  inherited;
end;

procedure TPOWER_PHASE_MAP.DrawAngle(APen: TPen; APos: TPoint; ARadius,
  AStartAngle, AEndAngle: Double; AHasArrow: Boolean; ASign: string);
var
  apPoint : array of TPoint;
  dAngle : Double;
  i : Integer;
  pO : TPoint;
begin
  if AStartAngle > AEndAngle then
  begin
    dAngle := AStartAngle;
    AStartAngle := AEndAngle;
    AEndAngle := dAngle;
  end;

  SetLength(apPoint, 0);
  pO := APos;

  if AEndAngle - AStartAngle < Pi then
  begin
    for i := Round(AStartAngle * 90) to Round(AEndAngle * 90) do
    begin
      SetLength(apPoint, Length(apPoint)+1);

      apPoint[High(apPoint)].X := pO.X + Round(ARadius * FUnitStep * Sin(i / 90));
      apPoint[High(apPoint)].Y := pO.Y - Round(ARadius * FUnitStep * Cos(i / 90));
    end;
  end
  else
  begin
    for i := Round(AEndAngle * 90) to Round(Pi * 90) do
    begin
      SetLength(apPoint, Length(apPoint)+1);

      apPoint[High(apPoint)].X := pO.X + Round(ARadius * FUnitStep * Sin(i / 90));
      apPoint[High(apPoint)].Y := pO.Y - Round(ARadius * FUnitStep * Cos(i / 90));
    end;

    for i := Round(- Pi * 90) to Round(AStartAngle * 90) do
    begin
      SetLength(apPoint, Length(apPoint)+1);

      apPoint[High(apPoint)].X := pO.X + Round(ARadius * FUnitStep * Sin(i / 90));
      apPoint[High(apPoint)].Y := pO.Y - Round(ARadius * FUnitStep * Cos(i / 90));
    end;
  end;

  // 画曲线
  with FCanvas do
  begin
    Pen := APen;
    Polyline(apPoint);
  end;

  // 画箭头
  if AHasArrow then
  begin
    if AEndAngle - AStartAngle < Pi then
    begin
      if Length(apPoint) >  2000 / FUnitStep then
      begin
        DrawArrow( APen, apPoint[0], AStartAngle - Pi/2 );
        DrawArrow( APen, apPoint[High(apPoint)], AEndAngle + Pi/2 );
      end;
    end
    else
    begin
      if Length(apPoint) > 2000 / FUnitStep then
      begin
        DrawArrow( APen, apPoint[0], AEndAngle - Pi / 2 );
        DrawArrow( APen, apPoint[High(apPoint)], AStartAngle + Pi/2 );
      end;
    end;
  end;
end;

procedure TPOWER_PHASE_MAP.DrawArrow(APen: TPen; APos: TPoint; AAngle: Double);
var
  pArw : TPoint;
  dPosX, dPosY : Double;
begin
  with FCanvas do
  begin
    Pen := APen;

    // 确定箭头大小
    if FUnitStep / 30 > 2 then
      dPosX := - FUnitStep / 30
    else
      dPosX := -2;

    dPosY := dPosX * 2;

    pArw.X := APos.X + Round(dPosY * Sin(AAngle) + dPosX * Cos(AAngle));
    pArw.Y := APos.Y - Round(dPosY * Cos(AAngle) - dPosX * Sin(AAngle));

    Polyline( [ APos, pArw ] );

    dPosX := - dPosX;

    pArw.X := APos.X + Round(dPosY * Sin(AAngle) + dPosX * Cos(AAngle));
    pArw.Y := APos.Y - Round(dPosY * Cos(AAngle) - dPosX * Sin(AAngle));

    Polyline( [ APos, pArw ] );
  end;
end;

procedure TPOWER_PHASE_MAP.DrawBackground(AHasCrossLine, AHasBorder: Boolean);
var
  pO, pLen : TPoint;
begin
  if not Assigned( FCanvas ) then
    Exit;

  with FCanvas do
  begin
    Brush.Color := FMapColor.Background;
    Brush.Style := bsSolid;
    FillRect( FRect );

    // 画交叉坐标
    if AHasCrossLine then
    begin
      Pen.Color := FMapColor.DotLine;
      Pen.Style := psDot;
      Pen.Width := FPenWidth;
      pO.X  := ( FRect.Right + FRect.Left ) div 2;
      pO.Y  := ( FRect.Bottom + FRect.Top ) div 2;
      pLen  := Point(  Round( 0.9 * ( FRect.Right - FRect.Left ) / 2 ),
        Round( 0.9 * ( FRect.Bottom - FRect.Top ) / 2 ) );

      MoveTo( pO.X - pLen.X, pO.Y );
      LineTo( pO.X + pLen.X, pO.Y );

      MoveTo( pO.X, pO.Y - pLen.Y );
      LineTo( pO.X, pO.Y + pLen.Y );
    end;

    // 画边界
    if AHasBorder then
    begin
      Pen.Color := FMapColor.DotLine;
      Pen.Style := psSolid;
      Pen.Width := FPenWidth;
      Brush.Style := bsClear;
      Rectangle( FRect );
    end;
  end;
end;

procedure TPOWER_PHASE_MAP.DrawLine(APen: TPen; APos: TPoint; ALen, AAngle: Double;
  AHasArrow: Boolean; ASign: string);
var
  pEnd : TPoint;
  pArw : TPoint;
begin
  // 计算 终点 位置
  pEnd.X := APos.X + Round( FUnitStep * ALen * Sin( AAngle ) );
  pEnd.Y := APos.Y - Round( FUnitStep * ALen * Cos( AAngle ) );

  // 画线
  with FCanvas do
  begin
    Pen := APen;
    Polyline( [ APos, pEnd ] );

    // 画箭头
    if AHasArrow then
      DrawArrow( APen, pEnd, AAngle );
  end;

  if ASign <> '' then
  begin
    if AAngle > 3 * Pi / 4 then              // 第四相线
    begin
      pArw.X := pEnd.X + FUnitStep div 15;
      pArw.Y := pEnd.Y - FUnitStep div 10;
    end
    else if AAngle > Pi / 2 then
    begin
      pArw.X := pEnd.X + FUnitStep div 15;
      pArw.Y := pEnd.Y - FUnitStep div 8;
    end
    else if AAngle > Pi / 4 then              // 第一相线
    begin
      pArw.X := pEnd.X + FUnitStep div 15;
      pArw.Y := pEnd.Y - FUnitStep div 10;
    end
    else if AAngle > 0 then
    begin
      pArw.X := pEnd.X - Round( FUnitStep * Length( ASign ) / 30 );
      pArw.Y := pEnd.Y - Round( FUnitStep / 3.5 );
    end
    else if AAngle > - Pi / 4 then
    begin
      pArw.X := pEnd.X - Round( FUnitStep * Length( ASign ) / 30 );
      pArw.Y := pEnd.Y - Round( FUnitStep / 3.5 );
    end
    else if AAngle > - Pi / 2.5 then
    begin
      pArw.X := pEnd.X - Round( FUnitStep * Length( ASign ) / 15 );
      pArw.Y := pEnd.Y - FUnitStep div 8;
    end
    else if AAngle > - Pi / 2 then
    begin
      pArw.X := pEnd.X - Round( FUnitStep * Length( ASign ) / 15 );
      pArw.Y := pEnd.Y - FUnitStep div 8;
    end
    else if AAngle > - 3 * Pi / 4 then
    begin
      pArw.X := pEnd.X - Round( FUnitStep * Length( ASign ) / 15 );
      pArw.Y := pEnd.Y - FUnitStep div 8;
    end
    else
    begin
      pArw.X := pEnd.X - Round( FUnitStep * Length( ASign ) / 15 );
      pArw.Y := pEnd.Y + FUnitStep div 15;
    end;

    FEquationDraw.DrawText( FCanvas, pArw, ASign, APen.Color,
      FMapColor.Background, FUnitStep / 75 );
  end;
end;

procedure TPOWER_PHASE_MAP.DrawPhase3Map;
  //  U12是否反
  function U12Reverse( dAngel : Double ) : Boolean;
  begin
    Result := Abs(dAngel - 120) > 6;
  end;

  function U23Reverse( dAngel : Double ) : Boolean;
  begin
    Result := Abs(dAngel + 180) > 6;
  end;
var
  dAngleU12 : Double;
  PStatus : TWE_POWER_STATUS;
begin
  // 画 线电压
  with FCanvas do
  begin
    Pen.Style := psSolid;
    Pen.Width := FPenWidth;
    Pen.Color := FMapColor.PhaseLine;
    DrawLine( Pen, FMapCenter, 1, CovAngle(90)  , True, '.U_1' );
    DrawLine( Pen, FMapCenter, 1, CovAngle(-30) , True, '.U_2' );
    DrawLine( Pen, FMapCenter, 1, CovAngle(-150), True, '.U_3' );
  end;

  // 确定U12的电压位置
  if Assigned( PowerAnalysis ) then
  begin
    PStatus.U       := FPower.VoltagePercent / 100;
    PStatus.I       := FPower.CurrentPercent / 100;
    PStatus.UIAngle := FPower.Angle;
    PStatus.U1      := FPowerStatus.U1;
    PStatus.I1      := FPowerStatus.I1;
    PStatus.O1      := FPowerStatus.O1;
    PStatus.U2      := FPowerStatus.U3;
    PStatus.I2      := FPowerStatus.I3;
    PStatus.O2      := FPowerStatus.O3;
    PStatus.OU1U2   := FPowerStatus.OU1U3;

    dAngleU12 := PowerAnalysis.GetUAngle( PStatus );
  end
  else
    dAngleU12 := 120;

  FCanvas.Pen.Color := FMapColor.PhaseAB;

  with FCanvas do
  begin
    if FPowerStatus.U1 > C_DRAW_VALUE_MIN then
    begin
      if U12Reverse(dAngleU12) then
        DrawLine( Pen, FMapCenter, FixedUValue( FPowerStatus.U1 ),
          CovAngle( dAngleU12 ), True, '.U_2_1' )
      else
        DrawLine( Pen, FMapCenter, FixedUValue( FPowerStatus.U1 ),
          CovAngle( dAngleU12 ), True, '.U_1_2' )
    end;

    if FPowerStatus.I1 > C_DRAW_VALUE_MIN then
      DrawLine( Pen, FMapCenter, FixedIValue( FPowerStatus.I1 ),
        CovAngle( dAngleU12 - FPowerStatus.O1 ), True, '.I_1' );

    FCanvas.Pen.Color := FMapColor.PhaseCB;

    if FPowerStatus.U3 > C_DRAW_VALUE_MIN then
    begin
      if U23Reverse(dAngleU12 - FPowerStatus.OU1U3) then
        DrawLine( Pen, FMapCenter, FixedUValue( FPowerStatus.U3 ),
          CovAngle( dAngleU12 - FPowerStatus.OU1U3 ), True, '.U_2_3' )
      else
        DrawLine( Pen, FMapCenter, FixedUValue( FPowerStatus.U3 ),
          CovAngle( dAngleU12 - FPowerStatus.OU1U3 ), True, '.U_3_2' );
    end;

    if FPowerStatus.I3 > C_DRAW_VALUE_MIN then
      DrawLine( Pen, FMapCenter, FixedIValue( FPowerStatus.I3 ),
        CovAngle( dAngleU12 - FPowerStatus.OU1U3 - FPowerStatus.O3 ), True, '.I_3' );
  end;
end;

procedure TPOWER_PHASE_MAP.DrawPhase4Map;
var
  dAngleU1 : Double;
begin
  FCanvas.Pen.Style := psSolid;
  FCanvas.Pen.Width := FPenWidth;
  dAngleU1 := 90;

  with FCanvas do
  begin
    FCanvas.Pen.Color := FMapColor.PhaseA;
    if FPowerStatus.U1 > C_DRAW_VALUE_MIN then
      DrawLine( Pen, FMapCenter, FixedUValue( FPowerStatus.U1 ),
        CovAngle( dAngleU1 ), True, '.U_1' );

    if FPowerStatus.I1 > C_DRAW_VALUE_MIN then
      DrawLine( Pen, FMapCenter, FixedIValue( FPowerStatus.I1 ),
        CovAngle( dAngleU1 - FPowerStatus.O1 ), True, '.I_1' );

    FCanvas.Pen.Color := FMapColor.PhaseB;
    if FPowerStatus.U2 > C_DRAW_VALUE_MIN then
      DrawLine( Pen, FMapCenter, FixedUValue( FPowerStatus.U2 ),
        CovAngle( dAngleU1 - FPowerStatus.OU1U2 ), True, '.U_2' );

    if FPowerStatus.I2 > C_DRAW_VALUE_MIN then
      DrawLine( Pen, FMapCenter, FixedIValue( FPowerStatus.I2 ),
        CovAngle( dAngleU1 - FPowerStatus.OU1U2 - FPowerStatus.O2 ), True, '.I_2' );

    FCanvas.Pen.Color := FMapColor.PhaseC;
    if FPowerStatus.U3 > C_DRAW_VALUE_MIN then
      DrawLine( Pen, FMapCenter, FixedUValue( FPowerStatus.U3 ),
        CovAngle( dAngleU1 - FPowerStatus.OU1U3 ), True, '.U_3' );

    if FPowerStatus.I3 > C_DRAW_VALUE_MIN then
      DrawLine( Pen, FMapCenter, FixedIValue( FPowerStatus.I3 ),
        CovAngle( dAngleU1 - FPowerStatus.OU1U3 - FPowerStatus.O3 ), True, '.I_3' );
  end;
end;

procedure TPOWER_PHASE_MAP.DrawPhaseMap(APowerStatus: TPOWER_STATUS; APower : TCKM_POWER);
begin
  FPowerStatus := APowerStatus;
  FPower := APower;
  
  // 初始化变量
  FMapCenter.X  := ( FRect.Right + FRect.Left )  div 2;
  FMapCenter.Y  := ( FRect.Bottom + FRect.Top )  div 2;
  FUnitStep := ( FRect.Right - FRect.Left )  div 5;
  FPenWidth := FUnitStep div 100;

  if FUnitStep = 0 then
    Exit;

  MapColor := PowerPhaseMap;

  DrawBackground( True, True );

  if Assigned( FPowerStatus ) then
    if FPowerStatus.PowerType = ptThree then
      DrawPhase3Map
    else
      DrawPhase4Map;
end;

function TPOWER_PHASE_MAP.FixedIValue(Value: Double): Double;
begin
  // 使其均在0.4以上
  Result := ( 0.4 + Value * 0.6 ) * C_DRAW_I_SCALE_RATE;
end;

function TPOWER_PHASE_MAP.FixedUValue(Value: Double): Double;
begin
  Result := Value * C_DRAW_U_SCALE_RATE;

  if Result > 2 then
    Result := 2;
end;

procedure TPOWER_PHASE_MAP.SetDefMapColor(const Value: TPOWER_PHASE_MAP_COLOR);
begin
  FDefMapColor := Value;
end;

end.
