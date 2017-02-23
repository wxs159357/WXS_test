{===============================================================================
  Copyright(c) 2007-2009, 北京北研兴电力仪表有限责任公司
  All rights reserved.

  错误接线，公式计算单元
  + TWE_EQUATION     公式计算

===============================================================================}

unit U_WE_EQUATION;

interface

uses SysUtils, Classes, math, U_WIRING_ERROR, U_WE_EQUATION_MATH,
  U_WE_EXPRESSION, Dialogs, U_POWER_LIST_INFO;

const
  C_WE_POWER_RIGHT_3 = '#UIcos\o';
  C_WE_POWER_RIGHT_4 = '3UIcos\o';
  C_WE_POWER_RIGHT_SINGLE = 'UIcos\o';

type
  /// <summary>
  /// 错误接线公式
  /// </summary>
  TWE_EQUATION = class( TPersistent )
  private
    FExpression : TWE_EXPRESSION;
    FEquationP : TStrings;
    FEquationK : TStrings;
    FAnalysisK : TStrings;
    FPhaseEquations : TList;
    FKValue : Double;
    function GetWiringError : TWIRING_ERROR;
    procedure SetWiringError(const Value: TWIRING_ERROR);
    function GetUIAngle : Double;
    procedure SetUIAngle(const Value: Double);
  private
    FRunTpye: Integer;
	FFourPower: TFourPower;
    FThreePower: TThreePower;
    /// <summary>计算 三线公式  /// </summary>
    procedure CalEquationP3;
    /// <summary>计算 四线公式  /// </summary>
    procedure CalEquationP4;
    /// <summary>计算 四线PT公式  /// </summary>
    procedure CalEquationP4PT;
    /// <summary>计算 单相公式  /// </summary>
    procedure CalEquationSingle;

    /// <summary>计算 更正系数  /// </summary>
    procedure CalEquationK;
    /// <summary>分析更正系数  /// </summary>
    procedure CalAnalysisK( ACoefs : array of string; AIntCoefs : array of Integer );

    /// <summary>
    /// 最终的计算结果
    /// </summary>
    function FinalEquation : string;

    /// <summary>
    /// 分析公式
    /// </summary>
    procedure AnalysisExpression( APhaseEquation : TWE_PHASE_EXPRESSION );

    /// <summary>
    /// 清空公式
    /// </summary>
    procedure CleanEquation;

    /// <summary>
    /// 生成三线表达式
    /// </summary>
    procedure CreateExpression3;

    /// <summary>
    /// 生成四线表达式
    /// </summary>
    procedure CreateExpression4;

    /// <summary>
    /// 生成四线PT表达式
    /// </summary>
    procedure CreateExpression4PT;


  public
    /// <summary>
    /// 生成单相表达式
    /// </summary>
    procedure CreateExpressionSingle;
    /// <summary>
    /// 功率公式
    /// </summary>
    property EquationP : TStrings read FEquationP;

    /// <summary>
    /// 更正系数公式
    /// </summary>
    property EquationK : TStrings read FEquationK;

    /// <summary>
    /// 更正系数解析
    /// </summary>
    property AnalysisK : TStrings read FAnalysisK;

    /// <summary>
    /// UI夹角
    /// </summary>
    property UIAngle : Double read GetUIAngle write SetUIAngle;

    /// <summary>
    /// 错误接线
    /// </summary>
    property WiringError : TWIRING_ERROR read GetWiringError write SetWiringError;

    /// <summary>
    /// 运行类型 0正转， 1反转， 2不转
    /// </summary>
    property RunTpye : Integer read FRunTpye write FRunTpye;

    /// <summary>
    /// 当前更正系数K值
    /// </summary>
    property KValue : Double read FKValue write FKValue;
	
    /// <summary>
    /// 三相四线功率对象
    /// </summary>
    property FourPower : TFourPower read FFourPower write FFourPower;

    /// <summary>
    /// 三相三线功率对象
    /// </summary>
    property ThreePower : TThreePower read FThreePower write FThreePower;
  public
    constructor Create();
    destructor Destroy; override;

    /// <summary>
    /// 获取相公式
    /// </summary>
    function GetPhaseEquation( const AIndex : Integer ) : TWE_PHASE_EXPRESSION;

    /// <summary>
    /// 生成计算公式
    /// </summary>
    procedure GenerateEquations( AWE : TWIRING_ERROR; AUIAngle : Double ); overload;

    /// <summary>
    /// 生成公式
    /// </summary>
    procedure GenerateEquations; overload;

    /// <summary>
    /// 显示分析
    /// </summary>
    procedure ShowAnalysis( AText : TStrings; AMaxSize : Integer );
  end;

implementation

{ TWE_EQUATION }

procedure TWE_EQUATION.AnalysisExpression(APhaseEquation : TWE_PHASE_EXPRESSION);
var
  sCoUI, sCoI : string;   // U, I 系数
begin
  if not Assigned( APhaseEquation ) then
    Exit;

  with APhaseEquation do        // 初始化数据
  begin
    Result1  := '';
    Result2  := '';
    CoCos    := '';
    CoSin    := '';
  end;

  sCoUI := '';
  sCoI := '';

  // 如果该式结果为0
  if APhaseEquation.Expression = '0' then
    Exit;

  with APhaseEquation do
  begin
    //------------------------------------------------------------------------
    // 计算UI的系数, 同时生成结果公式
    //------------------------------------------------------------------------
    // 取U的系数
    if ExpressU[ 1 ] <> 'U' then
    begin
      sCoUI   := Copy( ExpressU, 1, Pos( 'U', ExpressU ) - 1 );
      sCoUI := StringReplace( sCoUI, '-', '', [] );
    end;

    // 取I的系数
    if ( Pos( '_a_c', ExpressI ) > 0 ) or
       ( Pos( '_c_a', ExpressI ) > 0 ) then
      sCoI := C_EQ_SIGN_SQRT3;

    if ExpressI[ 1 ] <> 'I' then
    begin
      sCoI := Copy( ExpressI, 1, Pos( 'I', ExpressI ) - 1 ) + sCoI;
      sCoI := StringReplace( sCoI, '-', '', [] );
      CleanSigns( sCoI );
    end;

    // 生成UI公式
    Result1 := sCoUI + 'U' + sCoI + 'I';

    // 化简系数
    sCoUI := sCoUI + sCoI;
    CleanSigns( sCoUI );

    Result2 := sCoUI + 'UI';

    //------------------------------------------------------------------------
    // 计算Cos/Sin的系数, 同时生成结果公式
    //------------------------------------------------------------------------
    if AngleO = 0 then
    begin
      // cos(0+O) = -cosO
      // cos(0-O) = -cosO
      Result1 := Result1 + 'cos\o';
      Result2 := Result2 + 'cos\o';

      // 最终系数
      CoCos := '1'; // 如果为空，则系数为 +
    end
    else if AngleO = 90 then
    begin
      if SignO then // cos(90+O) = -sinO
      begin
        Result1 := Result1 + '(-sin\o)';
        sCoUI   := '-' + sCoUI;
        Result2 := '-' + Result2 + 'sin\o';
      end
      else
      begin
        Result1 := Result1 + 'sin\o';
        Result2 := Result2 + 'sin\o';
      end;

      // 去掉可能出现的 --
      Result1 := StringReplace( Result1, '--', '', [] );
      Result2 := StringReplace( Result2, '--', '', [] );

      // 最终系数
      CoSin := sCoUI + CoSin;
      CoSin := StringReplace( CoSin, '--', '', [] );

      if CoSin = '' then // 如果为空，则系数为 @
        CoSin := '1';
    end
    else if AngleO = 180 then
    begin
      // cos(180+O) = -cosO
      // cos(180-O) = -cosO
      Result1 := Result1 + '(-cos\o)';

      sCoUI   := '-' + sCoUI;
      Result2 := '-' + Result2 + 'cos\o';

      // 去掉可能出现的 --
      Result1 := StringReplace( Result1, '--', '', [] );
      Result2 := StringReplace( Result2, '--', '', [] );

      // 最终系数
      CoCos := sCoUI + CoCos;
      CoCos := StringReplace( CoCos, '--', '', [] );

      if CoCos = '' then // 如果为空，则系数为 +
        CoCos := '1';
    end
    else
    begin
      // 进行三角公式计算
      // cos(@+O) = cos@cosO-sin@sinO
      // cos(@-O) = cos@cosO+sin@sinO
      CoCos  := 'cos' + IntToStr( AngleO ) + '`';
      CoSin  := 'sin' + IntToStr( AngleO ) + '`';

      if SignO then
        CoSin  := '-' + CoSin;

      // 生成公式1
      Result1 := Result1 + '(' + CoCos + 'cos\o' + '+' + CoSin + 'sin\o)';
      CleanSigns( Result1 );

      // 转化为分数
      ReplaceCosSinWithFrac( CoCos );
      ReplaceCosSinWithFrac( CoSin );

      // 生成结果2
      Result2 := Result2 + '(' + CoCos + 'cos\o' + '+' + CoSin + 'sin\o)';
      CleanSigns( Result2 );

      // 最终 UIcos, UIsin 系数
      CoCos := sCoUI + CoCos;
      CoSin := sCoUI + CoSin;
    end;
  end;
end;

procedure TWE_EQUATION.CalAnalysisK(ACoefs: array of string;
  AIntCoefs: array of Integer);
var
  sCoR, sCoWcos, sCoWsin : string;
  nR, nR3, nWCos, nW3Cos, nWSin, nW3Sin : Integer;
  d : Double;
  dtg : Double;
  dCos : Double;
  sCos : string;
  s : string;
  X, Y1, Y2 : Double;
  dTemp : Double;
  dAngle : Double;
begin
  sCoR    := ACoefs[ 0 ];
  sCoWcos := ACoefs[ 1 ];
  sCoWsin := ACoefs[ 2 ];
  nR     := AIntCoefs[ 0 ];
  nR3    := AIntCoefs[ 1 ];
  nWCos  := AIntCoefs[ 2 ];
  nW3Cos := AIntCoefs[ 3 ];
  nWSin  := AIntCoefs[ 4 ];
  nW3Sin := AIntCoefs[ 5 ];

  X := nR + nR3 * Sqrt( 3 );
  Y1 := nWCos + nW3Cos * Sqrt( 3 );
  Y2 := nWSin + nW3Sin * Sqrt( 3 );

  dAngle := DegToRad(UIAngle);
  dTemp := Cos(dAngle);

  // Xcoso / Y1coso = X/Y1
  if (sCoWcos <> '') and (sCoWsin = '') then
  begin
    try
      d := X / Y1;
      FKValue := d;

      // d = 0 的可能不存在, 分子不为 0
      if d < 0 then
        FAnalysisK.Add('表反转。')
      else if IsZero( d - 1 ) then  // dK = 1 可能存在，不确定
        FAnalysisK.Add('计量正确。')
      else if d < 1 then
        FAnalysisK.Add('表快。')
      else if d > 1 then
        FAnalysisK.Add('表慢。');

      if d < 0 then
        FRunTpye := 1
      else
        FRunTpye := 0;
    except
      FAnalysisK.Add('表不转。');  // 分母为 0, 不应该存在
      FRunTpye := 2;
    end;
  end
  // Xcoso / Y2Sino = 1 -> tg = X/Y2
  else if (sCoWcos = '') and (sCoWsin <> '') then
  begin
    try
      dtg := X / Y2;
      if (y2* Sin(dAngle)) <> 0 then
        FKValue := (X * Cos(dAngle))/(y2* Sin(dAngle))
      else
        FKValue := 0;

      // cosO^2 = 1 / ( 1 + tgO^2 )
      dCos := Sqrt(1 / ( 1 + Power( dtg, 2 )));
      sCos := FloatToStr( Round( dCos * 1000 ) / 1000 );

      FAnalysisK.Add('当 cosφ= 1 时，表不转。');

      if dtg < 0 then
      begin
        FAnalysisK.Add('当负载为感性，0 < cosφ < 1 时，表反转。');
        FAnalysisK.Add('当负载为容性，0 < cosφ < 1 时，表正转。');
      end
      else
      begin
        FAnalysisK.Add('当负载为感性，0 < cosφ < 1 时，表正转。');
        FAnalysisK.Add('当负载为容性，0 < cosφ < 1 时，表反转。');
      end;

      FAnalysisK.Add( Format( '当 cosφ < %s 时， 表快。', [ sCos ] ) );
      FAnalysisK.Add( Format( '当 cosφ > %s 时， 表慢。', [ sCos ] ) );

      if Cos(DegToRad(UIAngle))= 1 then
      begin
        FRunTpye := 2;
      end;

      if dtg < 0 then
      begin
        if UIAngle > 0 then
          FRunTpye := 0
        else
          FRunTpye := 1;
      end
      else
      begin
        if UIAngle > 0 then
          FRunTpye := 1
        else
          FRunTpye := 0;
      end;
    except
    end;
  end
  else
  // K = Xcos / ( Y1Cos + Y2Sin )  包含  sin cos
  begin
    try
      FKValue := (X * Cos(dAngle))/(y1 * Cos(dAngle) + y2* Sin(dAngle));
      // 计算临界值，即分母为0 的情况
      // Y1cos + Y2sin = 0 -> tg = - Y1/Y2
      dtg := - Y1 / Y2;
      dCos := Sqrt(1 / ( 1 + Power( dtg, 2 )));
      sCos := FloatToStr( Round( dCos * 1000 ) / 1000 );

      // 判断 K 正负, 计算K值
      // tg变大时，cos变小
      // K = X / ( Y1 + Y2tg ), X > 0， 只需判断分母即可
      d := Y1 + ( dtg + 0.1 ) * Y2;

      if Abs(d) > 0 then
        s := '当负载为感性，cosφ = %s 时，表不转；cosφ > %s 时，表反转；cosφ < %s 时，表正转。'
      else
        s := '当负载为容性，cosφ = %s 时，表不转；cosφ > %s 时，表正转；cosφ < %s 时，表反转。';

      FAnalysisK.Add( Format( s, [ sCos, sCos, sCos ] ) );

      if Abs(d) > 0 then
      begin
        if dTemp < dCos then
          FRunTpye := 1
        else if dTemp = dCos then
          FRunTpye := 2
        else
         FRunTpye := 0;
      end
      else
      begin
        if dTemp < dCos then
          FRunTpye := 0
        else if dTemp = dCos then
          FRunTpye := 2
        else
         FRunTpye := 1;
      end;
    except
    end;

    try
      // 计算正确时
      // K = Xcos / ( Y1Cos + Y2Sin ) = 1 -> tg = (X-Y1)/Y2       包含  sin cos
      dtg := ( X - Y1 ) / Y2;

      // coso的值
      dCos := Sqrt(1 / ( 1 + Power( dtg, 2 )));
      sCos := FloatToStr( Round( dCos * 1000 ) / 1000 );

      // 计算cosO变小时的K值, dtg变大，cos变小
      // K = X / ( Y1 + Y2tg )
      if dtg < 0 then
        dtg := dtg - 0.1
      else
        dtg := dtg + 0.1;

      d := X / ( Y1 + ( dtg ) * Y2 );

      if Abs(dtg) > 0 then
        s := '当负载为容性，'
      else
        s := '当负载为感性，';

      if d > 1 then
        s := s + 'cosφ = %s 时，计量正确；cosφ > %s 时，表快；cosφ < %s 时，表慢。'
      else
        s := s + 'cosφ = %s 时，计量正确；cosφ > %s 时，表慢；cosφ < %s 时，表快。';

      FAnalysisK.Add( Format( s, [ sCos, sCos, sCos ] ) );
    except
    end;

    try
      // 计算反转时计量正确
      // K = Xcos / ( Y1Cos + Y2Sin ) = -1 -> tg = -(X+Y1)/Y2
      dtg := -( X + Y1 ) / Y2;
      
      // coso的值
      dCos := Sqrt(1 / ( 1 + Power( dtg, 2 )));
      sCos := FloatToStr( Round( dCos * 1000 ) / 1000 );

      if Abs(dtg) > 0 then
        s := '当负载为容性，cosφ = %s 时，计量正确但表反转。'
      else
        s := '当负载为感性，cosφ = %s 时，计量正确但表反转。';

      if Abs(dtg) > 0 then
      begin
        if dTemp = dCos then
          FRunTpye := 1
      end;

      FAnalysisK.Add( Format( s, [ sCos ] ) );
    except
    end;
  end;

  FAnalysisK.Add( '更正系数K为' + FormatFloat('0.000', FKValue));
end;

procedure TWE_EQUATION.CalEquationK;
var
  sEquation : string;
  sRightE : string;
  sWrongE : string;

  // 系数
  sCoR, sCoW, sCoWcos, sCoWsin : string;
  s : string;
  sTemp : string;

  FracR, FracR3 : TWE_FRAC;
  FracWCos, FracW3Cos : TWE_FRAC;
  FracWSin, FracW3Sin : TWE_FRAC;
  nFactor, nCommonDen : Integer;
begin
  if FEquationP.Count = 0 then
    Exit;

  if GetWiringError.PhaseType = ptThree then
    sRightE := C_WE_POWER_RIGHT_3
  else if GetWiringError.PhaseType = ptSingle then
    sRightE := C_WE_POWER_RIGHT_SINGLE
  else
    sRightE := C_WE_POWER_RIGHT_4;

  sWrongE := FEquationP[FEquationP.Count -1];

  FEquationK.Add('P/P''');
  FEquationK.Add(sRightE + '/' + sWrongE);

  if sWrongE = '0' then
  begin
    FEquationK.Add('\q');    // 无穷大
    FAnalysisK.Add('表不转。');
    FRunTpye := 2;

    FKValue := -1;
    FAnalysisK.Add( '更正系数K为 无穷大');
  end
  else if sWrongE = sRightE then
  begin
    FEquationK.Add('1');
    FAnalysisK.Add('计量正确。');
    FRunTpye := 0;
    FKValue := 1;
    FAnalysisK.Add( '更正系数K为' + FormatFloat('0.000', FKValue));
  end
  else // 计算
  begin
    // 1. 提取系数
    DivStr( sRightE, 'U', sCoR, s );     // 正确公式

    DivStr( sWrongE, 'U', sCoW, s );     // 错误公式
    if sCoW = '' then sCoW := '1';
    sCoWsin := '';
    sCoWcos := '';
    s := Copy( s, 3, Length( s ) - 2 );   // 去掉UI

    if Pos('(', s ) > 0 then
    begin
      // 获取 cos, sin 的系数
      s := Copy( s, 2, Length( s ) - 2 );   // 如果有括号, 去掉括号
      DivStr( s, 'cos\o', sCoWcos, sTemp );
      s := Copy( sTemp, 6,  Length( sTemp ) - 5 ); // 去掉 cos\o
      DivStr( s, 'sin\o', sCoWsin, sTemp );
      sCoWcos := sCoW + sCoWcos;
      sCoWsin := sCoW + sCoWsin;
    end
    else
    begin
      if Pos('cos', sWrongE) > 0 then
        sCoWcos := sCoW
      else
        sCoWsin := sCoW;
    end;

    // 提取系数
    PlusCoefs( [ sCoR ], FracR, FracR3 );
    PlusCoefs( [ sCoWcos ], FracWCos, FracW3Cos );
    PlusCoefs( [ sCoWsin ], FracWSin, FracW3Sin );

    // 2. 系数化简
    // 提取公分母, K公式上下同时乘以公分母，消除分数
    nCommonDen := GetCommonDen( [ FracR, FracR3, FracWCos, FracW3Cos, FracWSin,
      FracW3Sin ] );
    
    FracR      := MultiplyFrac( FracR,      nCommonDen );
    FracR3    := MultiplyFrac( FracR3,    nCommonDen );
    FracWCos   := MultiplyFrac( FracWCos,   nCommonDen );
    FracW3Cos := MultiplyFrac( FracW3Cos, nCommonDen );
    FracWSin   := MultiplyFrac( FracWSin,   nCommonDen );
    FracW3Sin := MultiplyFrac( FracW3Sin, nCommonDen );

    // K公式上下用因数化简
    nFactor := GetFactor( [ FracR.n, FracR3.n, FracWCos.n, FracW3Cos.n,
      FracWSin.n, FracW3Sin.n ] );

    if ( nFactor > 1 ) or ( nFactor < 0 ) then
    begin
      FracR.n      := FracR.n      div nFactor;
      FracR3.n    := FracR3.n    div nFactor;
      FracWCos.n   := FracWCos.n   div nFactor;
      FracW3Cos.n := FracW3Cos.n div nFactor;
      FracWSin.n   := FracWSin.n   div nFactor;
      FracW3Sin.n := FracW3Sin.n div nFactor;
    end;

    // 如果分母为负数，则翻到分子
    nFactor := GetFactor( [ FracWCos.n, FracW3Cos.n, FracWSin.n, FracW3Sin.n ] );

    if nFactor < 0 then
    begin
      FracR.n      := FracR.n      * -1;
      FracR3.n    := FracR3.n    * -1;
      FracWCos.n   := FracWCos.n   * -1;
      FracW3Cos.n := FracW3Cos.n * -1;
      FracWSin.n   := FracWSin.n   * -1;
      FracW3Sin.n := FracW3Sin.n * -1;
    end;

    // 如果可以是sqrt3的倍数， 例如 3/4 + sqrt3/4 -> sqrt3( 1/4, sqrt3/4 )
    if HasSqrt3Factor( FracR.n, FracR3.n ) and
       HasSqrt3Factor( FracWCos.n, FracW3Cos.n ) and
       HasSqrt3Factor( FracWSin.n, FracW3Sin.n ) then
    begin
      DivSqrt3( FracR, FracR3 );
      DivSqrt3( FracWCos, FracW3Cos );
      DivSqrt3( FracWSin, FracW3Sin );
    end;

    // 3. 显示化简结果
    sCoR := PlusExpressions( GenCoef( FracR.n, False ), GenCoef( FracR3.n, True ) );
    if sCoR = '' then
      sCoR := '1';
    sCoWcos := PlusExpressions( GenCoef( FracWCos.n, False ), GenCoef( FracW3Cos.n, True ) );
    sCoWsin := PlusExpressions( GenCoef( FracWSin.n, False ), GenCoef( FracW3Sin.n, True ) );

    if sCoWsin = '' then  // *coso/*coso
      sEquation :=  RefinedCoef( Format( C_FMT_FRAC, [ sCoR, sCoWcos ] ) )
    else if sCoWcos = '' then
      sEquation := MultiplyExpressions(
        RefinedCoef( Format( C_FMT_FRAC, [ sCoR, sCoWsin ] ) ), 'ctg\o' )
    else
      sEquation := sCoR + '/' + PlusExpressions( sCoWcos,
        MultiplyExpressions( sCoWsin, 'tg\o' ) );

    FEquationK.Add(sEquation);

    if GetWiringError.PhaseType <> ptSingle then
    begin
      // 解析K
      CalAnalysisK( [ sCoR, sCoWcos, sCoWsin ], [ FracR.n, FracR3.n, FracWCos.n,
        FracW3Cos.n, FracWSin.n, FracW3Sin.n ] );
    end;
  end;
end;

procedure TWE_EQUATION.CalEquationP3;
var
  sEquation : string;
begin
  // 公式2
  FEquationP.Add('P_1''+P_2''');

  sEquation := GetPhaseEquation(0).Expression + '+' + GetPhaseEquation(1).Expression;
  CleanSigns( sEquation );
  FEquationP.Add( sEquation );

  if sEquation = '0' then
    Exit;

  // 结果公式1
  sEquation := CombineExpressions( [ GetPhaseEquation(0).Result1,
    GetPhaseEquation(1).Result1 ] );
  FEquationP.Add( sEquation );

  // 结果公式2
  sEquation := CombineExpressions( [ GetPhaseEquation(0).Result2,
    GetPhaseEquation(1).Result2 ] );

  if sEquation <> FEquationP[ FEquationP.Count - 1 ] then
    FEquationP.Add( sEquation );

  // 计算最终结果
  sEquation := FinalEquation;
  if sEquation <> FEquationP[ FEquationP.Count - 1 ] then
  begin
    FEquationP.Add( sEquation );
  end;
end;

procedure TWE_EQUATION.CalEquationP4;
var
  sEquation : string;
begin
  // 公式2
  FEquationP.Add( 'P_1''+P_2''+P_3''' );

  sEquation := GetPhaseEquation(0).Expression + '+' +
    GetPhaseEquation(1).Expression + '+' + GetPhaseEquation(2).Expression;
  CleanSigns( sEquation );
  FEquationP.Add( sEquation );

  if sEquation = '0' then
    Exit;

  // 结果公式1
  sEquation := CombineExpressions( [ GetPhaseEquation(0).Result1,
    GetPhaseEquation(1).Result1, GetPhaseEquation(2).Result1 ] );
  FEquationP.Add( sEquation );

  // 结果公式2
  sEquation := CombineExpressions( [ GetPhaseEquation(0).Result2,
    GetPhaseEquation(1).Result2, GetPhaseEquation(2).Result2 ] );

  if sEquation <> FEquationP[ FEquationP.Count - 1 ] then
    FEquationP.Add( sEquation );

  // 计算最终结果
  if sEquation <> FinalEquation then
  begin
    sEquation := FinalEquation;
    FEquationP.Add( sEquation );
  end;
end;

procedure TWE_EQUATION.CalEquationP4PT;
var
  sEquation : string;
begin
  // 公式2
  FEquationP.Add( 'P_1''+P_2''+P_3''' );

  sEquation := GetPhaseEquation(0).Expression + '+' +
    GetPhaseEquation(1).Expression + '+' + GetPhaseEquation(2).Expression;
  CleanSigns( sEquation );
  FEquationP.Add( sEquation );

  if sEquation = '0' then
    Exit;

  // 结果公式1
  sEquation := CombineExpressions( [ GetPhaseEquation(0).Result1,
    GetPhaseEquation(1).Result1, GetPhaseEquation(2).Result1 ] );
  FEquationP.Add( sEquation );

  // 结果公式2
  sEquation := CombineExpressions( [ GetPhaseEquation(0).Result2,
    GetPhaseEquation(1).Result2, GetPhaseEquation(2).Result2 ] );

  if sEquation <> FEquationP[ FEquationP.Count - 1 ] then
    FEquationP.Add( sEquation );

  // 计算最终结果
  if sEquation <> FinalEquation then
  begin
    sEquation := FinalEquation;
    FEquationP.Add( sEquation );
  end;
end;

procedure TWE_EQUATION.CalEquationSingle;
var
  sEquation : string;
begin
  // 公式2
  FEquationP.Add( 'P_1' );

  sEquation := GetPhaseEquation(0).Expression;
  CleanSigns( sEquation );
  FEquationP.Add( sEquation );

  if sEquation = '0' then
    Exit;

  // 结果公式1
  sEquation := CombineExpressions( [ GetPhaseEquation(0).Result1 ] );
  FEquationP.Add( sEquation );

  // 结果公式2
  sEquation := CombineExpressions( [ GetPhaseEquation(0).Result2] );

  if sEquation <> FEquationP[ FEquationP.Count - 1 ] then
    FEquationP.Add( sEquation );

  // 计算最终结果
  if sEquation <> FinalEquation then
  begin
    sEquation := FinalEquation;
    FEquationP.Add( sEquation );
  end;
end;

procedure TWE_EQUATION.CleanEquation;
var
  i: Integer;
begin
  for i := 0 to FPhaseEquations.Count - 1 do
    GetPhaseEquation( i ).Clear;

  FEquationP.Clear;
  FEquationK.Clear;
  FAnalysisK.Clear;
end;

constructor TWE_EQUATION.Create;
var
  i: Integer;
begin
  FExpression := TWE_EXPRESSION.Create;
  FEquationP := TStringList.Create;
  FEquationK := TStringList.Create;
  FAnalysisK := TStringList.Create;
  FPhaseEquations := TList.create;

  FFourPower:= TFourPower.create;
  FThreePower:= TThreePower.create;


  for i := 0 to 3 - 1 do
    FPhaseEquations.Add( TWE_PHASE_EXPRESSION.Create );

  FRunTpye := 0;
end;

procedure TWE_EQUATION.CreateExpression3;
  function GetUValue(sValue : string):Double;
  begin
    if sValue = '' then
      Result := -1
    else if Pos(C_EQ_SIGN_1_2, sValue) > 0 then
      Result := 0.5
    else if Pos(C_EQ_SIGN_SQRT3_2, sValue) > 0 then
      Result := 0.866
    else if Pos(C_EQ_SIGN_SQRT3, sValue) > 0 then
      Result := 1.732
    else
      Result := 1;
  end;

  function GetIValue(sValue : string):Double;
  begin
    if sValue = '' then
      Result := -1
    else
      Result := 1;
  end;

  function GetAngle(dA1,dA2 : Double):Double;
  begin
    Result := dA1 - dA2;
    if Result > 360 then
      Result := Result - 360;

    if Result < 0 then
      Result := Result + 360;
  end;
var
  i: Integer;
  AEXPRESSION : TWE_PHASE_EXPRESSION;
  dU1, dU2 : Double;
begin
  FExpression.GetExpressions3( [ GetPhaseEquation( 0 ), GetPhaseEquation( 1 ) ] );

  for i := 0 to 2 - 1 do
    AnalysisExpression( GetPhaseEquation( i ) );

  dU1 := 0;
  dU2 := 0;

  for i := 0 to 3 - 1 do
  begin
    AEXPRESSION := GetPhaseEquation( i );

    FThreePower.Errorcode  := WiringError.IDInStr;
    FThreePower.Errorcount := WiringError.ErrorCount;
    FThreePower.Angle      := FExpression.UIAngle;

    case i of
      0:
      begin
        FThreePower.U12 := GetUValue(AEXPRESSION.ExpressU);
        FThreePower.I1 := GetIValue(AEXPRESSION.ExpressI);
        if (FThreePower.U12 <> -1) and (FThreePower.I1 <> -1) then
          FThreePower.U12i1 := GetAngle(AEXPRESSION.AngleU, AEXPRESSION.AngleI)
        else
          FThreePower.U12i1 := -1;
        dU1 := AEXPRESSION.AngleU;

      end;
      1:
      begin
        FThreePower.U32 := GetUValue(AEXPRESSION.ExpressU);
        FThreePower.I3 := GetIValue(AEXPRESSION.ExpressI);
        if (FThreePower.U32 <> -1) and (FThreePower.I3 <> -1) then
          FThreePower.U32i3 := GetAngle(AEXPRESSION.AngleU, AEXPRESSION.AngleI)
        else
          FThreePower.U32i3 := -1;
        dU2 := AEXPRESSION.AngleU;
      end;
    end;
  end;
  if (FThreePower.U12 <> -1) and (FThreePower.U32 <> -1) then
    FThreePower.U12u32 := GetAngle(dU1, dU2)
  else
    FThreePower.U12u32 := -1;
end;

procedure TWE_EQUATION.CreateExpression4;
  function GetUIValue(sValue : string):Double;
  begin
    if sValue = '' then
      Result := -1
    else
      Result := 1;
  end;

  function GetAngle(dA1,dA2 : Double):Double;
  begin
    Result := dA1 - dA2;
    if Result > 360 then
      Result := Result - 360;

    if Result < 0 then
      Result := Result + 360;
  end;
var
  i: Integer;
  AEXPRESSION : TWE_PHASE_EXPRESSION;
  dU1, dU2, dU3 : Double;
begin
  FExpression.GetExpressions4( [ GetPhaseEquation( 0 ), GetPhaseEquation( 1 ),
    GetPhaseEquation( 2 ) ] );

  for i := 0 to 3 - 1 do
    AnalysisExpression( GetPhaseEquation( i ) );

  dU1 := 0;
  dU2 := 0;
  dU3 := 0;

  for i := 0 to 3 - 1 do
  begin
    AEXPRESSION := GetPhaseEquation( i );

    FFourPower.Errorcode  := WiringError.IDInStr;
    FFourPower.Errorcount  := WiringError.ErrorCount;
    FFourPower.Angle  := FExpression.UIAngle;
    case i of
      0:
      begin
        FFourPower.U1 := GetUIValue(AEXPRESSION.ExpressU);
        FFourPower.I1 := GetUIValue(AEXPRESSION.ExpressI);
        if (FFourPower.U1 <> -1) and (FFourPower.I1 <> -1) then
          FFourPower.U1i1 := GetAngle(AEXPRESSION.AngleU, AEXPRESSION.AngleI)
        else
          FFourPower.U1i1 := -1;

        dU1 := AEXPRESSION.AngleU;

      end;
      1:
      begin
        FFourPower.U2 := GetUIValue(AEXPRESSION.ExpressU);
        FFourPower.I2 := GetUIValue(AEXPRESSION.ExpressI);
        if (FFourPower.U2 <> -1) and (FFourPower.I2 <> -1) then
          FFourPower.U2i2 := GetAngle(AEXPRESSION.AngleU, AEXPRESSION.AngleI)
        else
          FFourPower.U2i2 := -1;
        dU2 := AEXPRESSION.AngleU;
      end;
      2:
      begin
        FFourPower.U3 := GetUIValue(AEXPRESSION.ExpressU);
        FFourPower.I3 := GetUIValue(AEXPRESSION.ExpressI);
        if (FFourPower.U3 <> -1) and (FFourPower.I3 <> -1) then
          FFourPower.U3i3 := GetAngle(AEXPRESSION.AngleU, AEXPRESSION.AngleI)
        else
          FFourPower.U3i3 := -1;
        dU3 := AEXPRESSION.AngleU;
      end;
    end;
  end;
  if (dU1 <> -1) and (dU2 <> -1) then
    FFourPower.U1u2 := GetAngle(dU1, dU2)
  else
    FFourPower.U1u2 := -1;
  if (dU2 <> -1) and (dU3 <> -1) then
    FFourPower.U2u3 := GetAngle(dU2, dU3)
  else
    FFourPower.U2u3 := -1;
  if (dU1 <> -1) and (dU3 <> -1) then
    FFourPower.U1u3 := GetAngle(dU1, dU3)
  else
    FFourPower.U1u3 := -1;
end;

procedure TWE_EQUATION.CreateExpression4PT;
var
  i: Integer;
begin
  FExpression.GetExpressions4PT( [ GetPhaseEquation( 0 ), GetPhaseEquation( 1 ),
    GetPhaseEquation( 2 ) ] );

  for i := 0 to 3 - 1 do
    AnalysisExpression( GetPhaseEquation( i ) );
end;

procedure TWE_EQUATION.CreateExpressionSingle;
begin
  FExpression.GetExpressionsSingle( GetPhaseEquation( 0 ));
  AnalysisExpression( GetPhaseEquation( 0 ) );
end;

destructor TWE_EQUATION.Destroy;
var
  i: Integer;
begin
  for i := 0 to 3 - 1 do
    GetPhaseEquation( i ).Free;

  FPhaseEquations.Free;
  FEquationP.Free;
  FEquationK.Free;
  FAnalysisK.Free;
  FExpression.Free;
  FFourPower.Free;
  FThreePower.Free;
  inherited;
end;

function TWE_EQUATION.FinalEquation : string;
var
  anCos, anSin : array of string;
  sCoCos, sCoSin : string;
  sCoUI : string;
  i: Integer;
  FracCos, FracR3Cos : TWE_FRAC;
  FracSin, FracR3Sin : TWE_FRAC;
  nUIFactor : Integer;
begin
  Result := '';

  SetLength( anCos, 3 );
  SetLength( anSin, 3 );

  for i := 0 to 3 - 1 do
  begin
    anCos[ i ] := GetPhaseEquation( i ).CoCos;
    anSin[ i ] := GetPhaseEquation( i ).CoSin;
  end;

  // 数据相加
  PlusCoefs( anCos, FracCos, FracR3Cos );
  PlusCoefs( anSin, FracSin, FracR3Sin );

  // 提取UI的系数
  nUIFactor := GetFactor( [ FracCos.n, FracR3Cos.n, FracSin.n, FracR3Sin.n ] );

  if Abs( nUIFactor ) = 1 then
  begin
    if nUIFactor < 0 then
      sCoUI := '-'
    else
      sCoUI := '';
  end
  else
    sCoUI := IntToStr( nUIFactor );

  // 获取完整的系数
  if ( nUIFactor > 1 ) or ( nUIFactor < 0 ) then
  begin
    FracCos.n := FracCos.n div nUIFactor;
    FracR3Cos.n := FracR3Cos.n div nUIFactor;
    FracSin.n := FracSin.n div nUIFactor;
    FracR3Sin.n := FracR3Sin.n div nUIFactor;
  end;

  sCoCos := PlusExpressions( GenCoef( FracCos, False ),
    GenCoef( FracR3Cos, True ) );

  sCoSin := PlusExpressions( GenCoef( FracSin, False ),
    GenCoef( FracR3Sin, True ) );

  if ( sCoCos = EmptyStr ) and ( sCoSin <> EmptyStr ) then
  begin
    Result := sCoSin + 'UIsin\o';
  end
  else if ( sCoCos <> EmptyStr ) and ( sCoSin = EmptyStr ) then
  begin
    Result := sCoCos + 'UIcos\o';
  end
  else if ( sCoCos <> EmptyStr ) and ( sCoSin <> EmptyStr ) then
  begin
    Result := Format( '%sUI(%s)', [ scoUI, PlusExpressions(
      MultiplyExpressions(  sCoCos, 'cos\o' ),
      MultiplyExpressions( sCoSin, 'sin\o' ) ) ] );
  end
  else
  begin
    Result := '0';
  end;

  Result := StringReplace( Result, '1U', 'U', [rfIgnoreCase] );
end;

procedure TWE_EQUATION.GenerateEquations( AWE : TWIRING_ERROR; AUIAngle : Double );
begin
  FExpression.UIAngle := AUIAngle;
  FExpression.WiringError := AWE;
  GenerateEquations;
end;

procedure TWE_EQUATION.GenerateEquations;
var
  WErr : TWIRING_ERROR;
begin
  CleanEquation;
  WErr := GetWiringError;

  if Assigned( WErr ) then
    if WErr.PhaseType = ptThree then
    begin
      CreateExpression3;
      CalEquationP3;
    end
    else if WErr.PhaseType = ptFour then
    begin
      CreateExpression4;
      CalEquationP4;
    end
    else if WErr.PhaseType = ptFourPT then
    begin
      CreateExpression4PT;
      CalEquationP4PT;
    end
    else if WErr.PhaseType = ptSingle then
    begin
      CreateExpression4;
      CalEquationSingle;
    end;

  CalEquationK;
end;

function TWE_EQUATION.GetPhaseEquation(
  const AIndex: Integer): TWE_PHASE_EXPRESSION;
begin
  if AIndex in [ 0.. FPhaseEquations.Count - 1 ]then
    Result := TWE_PHASE_EXPRESSION( FPhaseEquations.Items[ AIndex ] )
  else
    Result := nil;
end;

function TWE_EQUATION.GetUIAngle: Double;
begin
  Result := FExpression.UIAngle;
end;

function TWE_EQUATION.GetWiringError: TWIRING_ERROR;
begin
  Result := FExpression.WiringError;
end;

procedure TWE_EQUATION.SetUIAngle(const Value: Double);
begin
  FExpression.UIAngle := Value;
  GenerateEquations;
end;

procedure TWE_EQUATION.SetWiringError(const Value: TWIRING_ERROR);
begin
  FExpression.WiringError := Value;
  GenerateEquations;
end;

procedure TWE_EQUATION.ShowAnalysis(AText: TStrings; AMaxSize: Integer);
  procedure AddString( AStr : string; AIndex : Integer = -1 );
  var
    nPosComma : Integer; // 逗号位置
    sFirstStr : string;
  begin
    if AIndex = -1 then
      sFirstStr := '    '
    else
      sFirstStr := IntToStr( AIndex + 1 )+ '. ';

    // 字符串长度过长，需要拆分
    if Length( AStr ) > AMaxSize then
    begin
      nPosComma := AnsiPos( '，', AStr );
      AText.Add( sFirstStr + Copy( AStr, 1, nPosComma + 1 ));
      AddString( Copy( AStr, nPosComma + 2, Length( AStr ) - nPosComma - 1 ) );
    end
    else
      AText.Add( sFirstStr + AStr );
  end;  
var
  i : Integer;
  nPosSemi : Integer; // 分号位置
  sTemp : string;
begin
  AText.Clear;

  if FAnalysisK.Count = 0 then
    Exit
  else if FAnalysisK.Count = 1 then
    AText.Add( FAnalysisK[ 0 ] )
  else
    for i := 0 to FAnalysisK.Count - 1 do
    begin
      if Pos( '；', FAnalysisK[ i ] ) > 0 then  // 包含；号的 拆分
      begin
        nPosSemi := Pos('；', FAnalysisK[ i ] );
        AddString( Copy( FAnalysisK[ i ], 1, nPosSemi + 1), i );
        sTemp := Copy( FAnalysisK[ i ], nPosSemi + 2, Length( FAnalysisK[ i ] ) - nPosSemi );

        while sTemp <> '' do
        begin
          if Pos( '；', sTemp ) > 0 then       // 包含；号的 拆分
          begin
            nPosSemi := Pos('；', sTemp);
            AddString( Copy( sTemp, 1, nPosSemi + 1) );
            sTemp := Copy( sTemp, nPosSemi + 2, Length(sTemp) - nPosSemi);
          end
          else
          begin
            AddString( sTemp );
            sTemp := '';
          end;
        end;
      end
      else
        AddString( FAnalysisK[ i ], i );

      AText.Add( EmptyStr );
    end;
end;

end.

