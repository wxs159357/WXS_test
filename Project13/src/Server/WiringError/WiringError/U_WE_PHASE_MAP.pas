{===============================================================================
  Copyright(c) 2007-2009, 北京北研兴电力仪表有限责任公司
  All rights reserved.

  错误接线，向量图绘制单元
  + TWE_PHASE_MAP 向量图绘制
  + TWE_PHASE_MAP_COLOR 色彩索引

===============================================================================}

unit U_WE_PHASE_MAP;

interface

uses SysUtils, Classes, Windows, Graphics, U_WE_EQUATION_DRAW, U_WE_EQUATION,
  U_WIRING_ERROR, U_WE_EQUATION_MATH, U_WE_EXPRESSION, system.UITypes, System.Types;

type
  /// <summary>
  /// 色彩索引
  /// </summary>
  TWE_PHASE_MAP_COLOR = record
    Background  : Integer;       // 向量图 底色
    DotLine     : Integer;       // 向量图 虚轴颜色

    PhaseLine   : Integer;       // 向量图 三线 相线颜色
    PhaseAB     : Integer;       // 向量图 三线 第一元件
    PhaseCB     : Integer;       // 向量图 三线 第二元件

    PhaseA      : Integer;       // 向量图 四线 第一元件
    PhaseB      : Integer;       // 向量图 四线 第二元件
    PhaseC      : Integer;       // 向量图 四线 第三元件

    EquationBackground : Integer;       // 公式   底色
    EquationFont       : Integer;       // 公式   字体颜色
  end;
var
  PhaseMapColor : TWE_PHASE_MAP_COLOR;

type
  /// <summary>
  /// 向量图
  /// </summary>
  TWE_PHASE_MAP = class( TComponent )
  private
    FMapCenter : TPoint;
    FCanvas : TCanvas;
    FRect : TRect;
    FMapColor : TWE_PHASE_MAP_COLOR;
    FUnitStep : Integer;
    FPenWidth : Integer;
    FEquationDraw : TWE_EQUATION_DRAW;
    FUIAngle: Double;
    FDefMapColor: TWE_PHASE_MAP_COLOR;
    procedure SetUIAngle(const Value: Double);
    procedure SetDefMapColor(const Value: TWE_PHASE_MAP_COLOR);
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
  protected
    /// <summary>
    /// 角度转换, 公式和画图的角度不同，需要转换
    /// </summary>
    function CovAngle( AValue : Double ) : Double;

    /// <summary>
    /// 画独立元件
    /// </summary>
    procedure DrawSingleEquation( APhaseEquation : TWE_PHASE_EXPRESSION;
      APen : TPen; APos : TPoint; AAngleRadius : Double );

    /// <summary>
    /// 画向量图
    /// </summary>
    procedure DrawPhase3Map( AEquation : TWE_EQUATION );
    procedure DrawPhase4Map( AEquation : TWE_EQUATION );
    procedure DrawPhase2Map( AEquation : TWE_EQUATION );
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
    property MapColor : TWE_PHASE_MAP_COLOR read FMapColor write FMapColor;

    /// <summary>
    /// 默认颜色索引
    /// </summary>
    property DefMapColor : TWE_PHASE_MAP_COLOR read FDefMapColor write SetDefMapColor;

    /// <summary>
    /// 单位长度
    /// </summary>
    property UnitStep : Integer read FUnitStep write FUnitStep;

    /// <summary>
    /// 画笔宽度
    /// </summary>
    property PenWidth : Integer read FPenWidth write FPenWidth;

    /// <summary>
    /// UI夹角
    /// </summary>
    property UIAngle : Double read FUIAngle write SetUIAngle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>
    /// 绘制向量图
    /// </summary>
    procedure DrawPhaseMap( AEquation : TWE_EQUATION ; sCaption : string='');

  end;

implementation

{ TWE_PHASE_MAP }

function TWE_PHASE_MAP.CovAngle(AValue: Double): Double;
begin
  if AValue >= -90 then
    Result := (90 - AValue) / 180 * Pi
  else
    Result := (- AValue - 270) / 180 * Pi;
end;

constructor TWE_PHASE_MAP.Create(AOwner: TComponent);
begin
  inherited;
  FEquationDraw := TWE_EQUATION_DRAW.Create( nil );
  with FDefMapColor do
  begin
    Background              := clWhite  ;
    DotLine                 := $00D1D1D1;
    PhaseLine               := $005E5E5E;
    PhaseAB                 := $003D9DFE;
    PhaseCB                 := clRed    ;
    PhaseA                  := $003D9DFE;
    PhaseB                  := $00009700;
    PhaseC                  := clRed    ;
    EquationBackground      := clWhite  ;
    EquationFont            := clBlack  ;
  end;

  FMapColor := FDefMapColor;
//  PhaseMapColor := FDefMapColor;

  FUIAngle := 20;
end;

destructor TWE_PHASE_MAP.Destroy;
begin
  FEquationDraw.Free;
  inherited;
end;

procedure TWE_PHASE_MAP.DrawAngle(APen: TPen; APos: TPoint; ARadius,
  AStartAngle, AEndAngle: Double; AHasArrow: Boolean; ASign: string);
var
  apPoint : array of TPoint;
//  pSign : TPoint;
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

//  // 确定标注的位置
//  if AEndAngle - AStartAngle < Pi then
//  begin
//    dAngle := (AEndAngle + AStartAngle) / 2;
//  end
//  else
//  begin
//    dAngle := Pi - (AEndAngle + AStartAngle) / 2;
//  end;

//  if FUnitStep < 100 then
//  begin
//    if dAngle > 3 * Pi / 4 then
//    begin
//      pSign.X := pSign.X - 4 * (Length(ASign) - 2);
//      pSign.Y := pSign.Y + 3;
//    end
//    else if dAngle > Pi / 2 then
//    begin
//      pSign.X := pSign.X + 5;
//      pSign.Y := pSign.Y - 5;
//    end
//    else if dAngle > Pi / 4 then
//    begin
//      pSign.X := pSign.X + 5 ;
//      pSign.Y := pSign.Y - 15;
//    end
//    else if dAngle > - Pi / 4 then
//    begin
//      pSign.X := pSign.X - 3 * (Length(ASign) - 2);
//      pSign.Y := pSign.Y - 20;
//    end
//    else if dAngle > - Pi / 2 then
//    begin
//      pSign.X := pSign.X - 8 * (Length(ASign) - 2);
//      pSign.Y := pSign.Y - 10;
//    end
//    else if dAngle > - 3 * Pi / 4 then
//    begin
//      pSign.X := pSign.X  - 9 * (Length(ASign) - 2);
//      pSign.Y := pSign.Y - 7;
//    end
//    else if dAngle > - Pi then
//    begin
//      pSign.X := pSign.X - 6 * (Length(ASign) - 2);
//      pSign.Y := pSign.Y + 5;
//    end;
//  end
//  else
//  begin
//    if dAngle > 3 * Pi / 4 then
//    begin
//      pSign.X := pSign.X - 4 * (Length(ASign) - 2);
//      pSign.Y := pSign.Y + 3;
//    end
//    else if dAngle > Pi / 2 then
//    begin
//      pSign.X := pSign.X + 3;
//      pSign.Y := pSign.Y - 1;
//    end
//    else if dAngle > Pi / 4 then
//    begin
//      pSign.X := pSign.X + 5 ;
//      pSign.Y := pSign.Y - 15;
//    end
//    else if dAngle > - Pi / 4 then
//    begin
//      pSign.X := pSign.X - 3 * (Length(ASign) - 2);
//      pSign.Y := pSign.Y - 25;
//    end
//    else if dAngle > - Pi / 2 then
//    begin
//      pSign.X := pSign.X - 10 * (Length(ASign) - 2);
//      pSign.Y := pSign.Y - 18;
//    end
//    else if dAngle > - 3 * Pi / 4 then
//    begin
//      pSign.X := pSign.X  - 11 * (Length(ASign) - 2);
//      pSign.Y := pSign.Y - 5;
//    end
//    else if dAngle > - Pi then
//    begin
//      pSign.X := pSign.X - 6 * (Length(ASign) - 2);
//      pSign.Y := pSign.Y + 5;
//    end;
//  end;
end;

procedure TWE_PHASE_MAP.DrawArrow(APen: TPen; APos: TPoint; AAngle: Double);
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

procedure TWE_PHASE_MAP.DrawBackground(AHasCrossLine, AHasBorder: Boolean);
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

    // 画交叉坐标，田字格
    if AHasCrossLine then
    begin
      Pen.Color := FMapColor.DotLine;
      Pen.Style := psDot;
      Pen.Width := FPenWidth;
      pO.X  := ( FRect.Right + FRect.Left ) div 2;
      pO.Y  := ( FRect.Bottom + FRect.Top ) div 2;
      pLen  := Point(  Round( 0.9 * ( FRect.Right - FRect.Left ) / 2 ),
        Round( 0.9 * ( FRect.Bottom - FRect.Top ) / 2 ) );

      Rectangle( pO.X - pLen.X, pO.Y - pLen.Y, pO.X + pLen.X, pO.Y + pLen.Y );

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

procedure TWE_PHASE_MAP.DrawLine(APen: TPen; APos: TPoint; ALen, AAngle: Double;
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
    else if AAngle > - Pi then
    begin
      pArw.X := pEnd.X - Round( FUnitStep * Length( ASign ) / 15 );
      pArw.Y := pEnd.Y + FUnitStep div 15;
    end;

    FEquationDraw.DrawText( FCanvas, pArw, ASign, APen.Color,
      FMapColor.Background, FUnitStep / 75 );
  end;
end;

procedure TWE_PHASE_MAP.DrawPhase2Map(AEquation: TWE_EQUATION);
begin
  FCanvas.Pen.Style := psSolid;
  FCanvas.Pen.Width := FPenWidth;

  FCanvas.Pen.Color := FMapColor.PhaseA;
  DrawSingleEquation( AEquation.GetPhaseEquation( 0 ), FCanvas.Pen, FMapCenter, 0.3 );
end;

procedure TWE_PHASE_MAP.DrawPhase3Map( AEquation : TWE_EQUATION );
begin
  // 画 线电压
  with FCanvas do
  begin
    Pen.Style := psSolid;
    Pen.Color := FMapColor.PhaseLine;
    Pen.Width := FPenWidth;

    DrawLine( Pen, FMapCenter, 1, CovAngle(90)  , True, DotVar( 'U' ) + SuffixVar( C_EQ_SIGN_A ) );
    DrawLine( Pen, FMapCenter, 1, CovAngle(-30) , True, DotVar( 'U' ) + SuffixVar( C_EQ_SIGN_B ) );
    DrawLine( Pen, FMapCenter, 1, CovAngle(-150), True, DotVar( 'U' ) + SuffixVar( C_EQ_SIGN_C ) );
  end;

  FCanvas.Pen.Color := FMapColor.PhaseAB;
  DrawSingleEquation( AEquation.GetPhaseEquation( 0 ), FCanvas.Pen, FMapCenter, 0.3 );

  FCanvas.Pen.Color := FMapColor.PhaseCB;
  DrawSingleEquation( AEquation.GetPhaseEquation( 1 ), FCanvas.Pen, FMapCenter, 0.4 );
end;

procedure TWE_PHASE_MAP.DrawPhase4Map( AEquation : TWE_EQUATION );
begin
  FCanvas.Pen.Style := psSolid;
  FCanvas.Pen.Width := FPenWidth;

  FCanvas.Pen.Color := FMapColor.PhaseA;
  DrawSingleEquation( AEquation.GetPhaseEquation( 0 ), FCanvas.Pen, FMapCenter, 0.3 );

  FCanvas.Pen.Color := FMapColor.PhaseB;
  DrawSingleEquation( AEquation.GetPhaseEquation( 1 ), FCanvas.Pen, FMapCenter, 0.4 );

  FCanvas.Pen.Color := FMapColor.PhaseC;
  DrawSingleEquation( AEquation.GetPhaseEquation( 2 ), FCanvas.Pen, FMapCenter, 0.5 );
end;

procedure TWE_PHASE_MAP.DrawPhaseMap(AEquation: TWE_EQUATION; sCaption : string);
begin
  FUIAngle := AEquation.UIAngle;

  // 初始化变量
  FMapCenter.X  := ( FRect.Right + FRect.Left )  div 2;
  FMapCenter.Y  := ( FRect.Bottom + FRect.Top )  div 2;

  FUnitStep := ( FRect.Right - FRect.Left )  div 5;
  FPenWidth := FUnitStep div 100;

  if FUnitStep = 0 then
    Exit;

  MapColor := PhaseMapColor;

  // 画背景及坐标
  DrawBackground( True, False );

  FCanvas.Font.Size  := Round( FUnitStep /8 );
  FCanvas.Font.Name  := '宋体';
  FCanvas.Font.Style := [];
  FCanvas.Font.Color := clBlack;
  FCanvas.TextOut(FRect.Right-90, FRect.Top+30, sCaption);

  if AEquation.WiringError.PhaseType = ptThree then
    DrawPhase3Map( AEquation )
  else if (AEquation.WiringError.PhaseType = ptFour) or
    (AEquation.WiringError.PhaseType = ptFourPT) then
    DrawPhase4Map( AEquation )
  else if AEquation.WiringError.PhaseType = ptSingle then
  begin
    DrawPhase2Map( AEquation )
  end;
end;

procedure TWE_PHASE_MAP.DrawSingleEquation(APhaseEquation: TWE_PHASE_EXPRESSION;
  APen : TPen; APos: TPoint; AAngleRadius: Double);
var
  dOU, dOI : Double;
  dLen  : Double;
begin
  if not Assigned( APhaseEquation ) then
    Exit;

  with APhaseEquation do
  begin
    dOU := CovAngle( AngleU );
    dOI := CovAngle( AngleI - FUIAngle );

    // 画电压
    if ExpressU <> EmptyStr then
    begin
      if Pos( C_EQ_SIGN_SQRT3_2, ExpressU) > 0 then
        dLen  := 1.5
      else if Pos( C_EQ_SIGN_1_2, ExpressU) > 0 then
        dLen  := 0.866
      else if Pos(C_EQ_SIGN_SQRT3, ExpressU) > 0 then
        dLen  := 2.2
      else
      begin
        if Length( ExpressU ) > 3 then
          dLen  := 1.732
        else
          dLen := 1.5;
      end;

      DrawLine( APen, APos, dLen, dOU, True,
        StringReplace( ExpressUT, 'U', DotVar( 'U' ), [] ) );
    end;

    // 画电流
    if ExpressI <> EmptyStr then
    begin
      dLen := 1;

      if ( Pos('_a_c', ExpressI) > 0 ) or ( Pos('_c_a', ExpressI) > 0 ) then
        dLen := dLen * 1.732;

      if Pos( C_EQ_SIGN_1_2, ExpressI ) > 0 then
        dLen := dLen / 2;

      if Pos('2I', ExpressI ) > 0 then
        dLen := dLen * 2;

      DrawLine( APen, APos, dLen, dOI, True,
        StringReplace( ExpressIT, 'I', DotVar( 'I' ), [] ) );
    end;

    // 画角度
    if ( ExpressU <> EmptyStr ) and ( ExpressI <> EmptyStr ) then
      DrawAngle( APen, APos, AAngleRadius, dOU, dOI, True, ExpressO );
  end;
end;

procedure TWE_PHASE_MAP.SetDefMapColor(const Value: TWE_PHASE_MAP_COLOR);
begin
  FDefMapColor := Value;
end;

procedure TWE_PHASE_MAP.SetUIAngle(const Value: Double);
begin
  FUIAngle := Value;
end;

end.
