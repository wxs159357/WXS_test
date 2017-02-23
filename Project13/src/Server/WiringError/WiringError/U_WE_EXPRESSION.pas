{===============================================================================
  Copyright(c) 2007-2009, 北京北研兴电力仪表有限责任公司
  All rights reserved.

  错误接线，表达式生成单元
  + TWE_PHASE_EXPRESSION     表达式生成

===============================================================================}

unit U_WE_EXPRESSION;

interface

uses SysUtils, Classes, U_WIRING_ERROR,U_WE_EQUATION_MATH, IniFiles, Forms, Math;

const
  C_EQ_SIGN_A = 'a';
  C_EQ_SIGN_B = 'b';
  C_EQ_SIGN_C = 'c';

const
  C_EQ_SIGN_1_2 = '{1,2}';     // 1/2
  C_EQ_SIGN_SQRT3 = '#';       // Sqrt3
  C_EQ_SIGN_SQRT3_2 = '{#,2}'; // Sqrt3/2

type
  /// <summary>
  /// 某一元件的表达式信息
  /// </summary>
  TWE_PHASE_EXPRESSION = class( TObject )
    ExpressionT : string; // 公式表达式 包含 U' / I'
    ExpressUT : string; // 电压表达式  包含 U'
    ExpressIT : string; // 电流表达式  包含 I'

    Expression : string;  // 公式表达式
    ExpressU : string;  // 电压表达式
    ExpressI : string;  // 电流表达式
    ExpressO : string;  // 角度表达式
    AngleU   : Integer;   // U角度
    AngleI   : Integer;   // I角度
    AngleO   : Integer;   // O角度
    SignO    : Boolean;   // O角度正负

    Result1  : string; // 结果表达式_1  不合并UI的系数
    Result2  : string; // 结果表达式_2  替换cos120`...
    CoCos    : string; // Cos系数
    CoSin    : string; // Sin系数
  public
    procedure Clear;
  end;

type
  /// <summary>
  /// 表达式生成
  /// </summary>
  TWE_EXPRESSION = class( TObject )
  private
    FUIAngle: Double;
    FWiringError: TWIRING_ERROR;
    procedure SetWiringError(const Value: TWIRING_ERROR);

    /// <summary>
    /// 生成独立元件的表达式
    /// </summary>
    procedure GenerateSingleExpression( APhaseEpr : TWE_PHASE_EXPRESSION );

    /// <summary>
    /// 获取四线电压
    /// </summary>
    procedure GetP4USign( var AUOrder : array of string );

    /// <summary>
    /// 获取四线电压带PT反向
    /// </summary>
    procedure GetP4USignPT( var AUOrder : array of string );

    /// <summary>
    /// 获取四线电流
    /// </summary>
    procedure GetP4ISign( var AIOrder : array of string );

    /// <summary>
    /// 获取三线电压
    /// </summary>
    procedure GetP3USign( out AU1, AU2 : string );

    /// <summary>
    /// 获取三线电压的角度
    /// </summary>
    function GetP3UAngle( const sU : string ) : Integer;

    /// <summary>
    /// 获取三线电流
    /// </summary>
    procedure GetP3ISign( out AI1, AI2 : string );

    /// <summary>
    /// 获取三线电流的角度
    /// </summary>
    procedure GetP3IAngle( var sI, sO : string; out nI : Integer; const sU : string );

    /// <summary>
    /// 获取电压电流的角度
    /// </summary>
    procedure GetUIO( const dAngle : Double; const nU, nI : Integer;
      var sO : string; var nO : Integer; var bO : Boolean );

    /// <summary>
    /// 改变标识的相序
    /// </summary>
    procedure ChangeSignSequence( var ASignList : array of string;
      ASequence : TWE_SEQUENCE_TYPE );
    function GetUIAngle: Double;
  public
    constructor Create;
    destructor Destroy; override;
    /// <summary>
    /// UI夹角
    /// </summary>
    property UIAngle : Double read GetUIAngle write FUIAngle;

    /// <summary>
    /// 错误接线
    /// </summary>
    property WiringError : TWIRING_ERROR read FWiringError write SetWiringError;

    /// <summary>
    /// 生成三线表达式
    /// </summary>
    procedure GetExpressions3( APhaseEprs : array of TWE_PHASE_EXPRESSION );

    /// <summary>
    /// 生成四线表达式
    /// </summary>
    procedure GetExpressions4( APhaseEprs : array of TWE_PHASE_EXPRESSION );

    /// <summary>
    /// 生成四线表达式（带PT反向）
    /// </summary>
    procedure GetExpressions4PT( APhaseEprs : array of TWE_PHASE_EXPRESSION );

    /// <summary>
    /// 生成单相表达式
    /// </summary>
    procedure GetExpressionsSingle( APhaseEprs : TWE_PHASE_EXPRESSION );
  end;
var
  /// <summary>
  /// 是否是感性角度
  /// </summary>
  bPubIsVectorAngle : Boolean;

implementation

{ TWE_EXPRESSION }

procedure TWE_EXPRESSION.GetP3IAngle(var sI, sO: string; out nI: Integer;
  const sU: string);
var
  bWithHalf : Boolean;
begin
  bWithHalf := Pos( C_EQ_SIGN_1_2, sI ) > 0;

  if bWithHalf then
    sI := StringReplace( sI, C_EQ_SIGN_1_2, '', [] );

  if sI = '+a' then
  begin
    nI := 90;
    sI := 'I' + SuffixVar( C_EQ_SIGN_A );
    sO := '\o' + SuffixVar( C_EQ_SIGN_A );
  end
  else if sI = '-a' then
  begin
    nI := -90;
    sI := '-I' + SuffixVar( C_EQ_SIGN_A );
    sO := '\o' + SuffixVar( C_EQ_SIGN_A );
  end
  else if sI = '+c' then
  begin
    nI := -150;
    sI := 'I' + SuffixVar( C_EQ_SIGN_C );
    sO := '\o' + SuffixVar( C_EQ_SIGN_C );
  end
  else if sI = '-c' then
  begin
    nI := 30;
    sI := '-I' + SuffixVar( C_EQ_SIGN_C );
    sO := '\o' + SuffixVar( C_EQ_SIGN_C );
  end
  else if ( sI = '+a+c' ) or ( sI = '+c+a' ) then
  begin
    nI := 150;
    sI := '-I' + SuffixVar( C_EQ_SIGN_B );
    sO := '\o' + SuffixVar( C_EQ_SIGN_B );
  end
  else if ( sI = '+a-c' ) or ( sI = '-c+a' ) then
  begin
    nI := 60;
    sI := 'I' + SuffixVar( C_EQ_SIGN_A ) + SuffixVar( C_EQ_SIGN_C );

    if Pos( 'a', sU ) > 0 then
      sO := '\o' + SuffixVar( C_EQ_SIGN_A )
    else
      sO := '\o' + SuffixVar( C_EQ_SIGN_C );
  end
  else if ( sI = '-a+c' ) or ( sI = '+c-a' ) then
  begin
    nI := -120;
    sI := 'I' + SuffixVar( C_EQ_SIGN_C ) + SuffixVar( C_EQ_SIGN_A );

    if Pos( 'a', sU ) > 0 then
      sO := '\o' + SuffixVar( C_EQ_SIGN_A )
    else
      sO := '\o' + SuffixVar( C_EQ_SIGN_C );
  end
  else if ( sI = '-a-c' ) or ( sI = '-c-a' ) then
  begin
    nI := -30;
    sI := 'I' + SuffixVar( C_EQ_SIGN_B );
    sO := '\o' + SuffixVar( C_EQ_SIGN_B );
  end
  else
  begin
    nI := 0;
    sI := '';
    sO := '';
  end;

  if bWithHalf and ( sI <> '' ) then
    sI := StringReplace( sI, 'I', C_EQ_SIGN_1_2+ 'I', [] );
end;

procedure TWE_EXPRESSION.GetExpressions4( APhaseEprs : array of TWE_PHASE_EXPRESSION );



var
  sU_Order : array[0..2] of string;        // 电压
  sI_Order : array[0..2] of string;        // 电流
  sO_Order : array[0..2] of string;        // 角度

  nU_Angle : array[0..2] of Integer;       // U角度
  nI_Angle : array[0..2] of Integer;       // I角度
  nO_Angle : array[0..2] of Integer;       // O角度
  bOI_Angle : array[0..2] of Boolean;      // 正负标识

  i : Integer;
begin
  GetP4USign( sU_Order );
  GetP4ISign( sI_Order );


  // 分析角度
  for i := 0 to 2 do
  begin
    IntToStr(i);
    // 电压
    if Pos( C_EQ_SIGN_A, sU_Order[i] ) > 0 then
      nU_Angle[i] := 90
    else if Pos( C_EQ_SIGN_B, sU_Order[i] ) > 0 then
      nU_Angle[i] := -30
    else if Pos( C_EQ_SIGN_C, sU_Order[i]) > 0 then
      nU_Angle[i] := -150
    else
      nU_Angle[i] := 0;



    // 电流
    if Pos( C_EQ_SIGN_A, sI_Order[i]) > 0 then
      nI_Angle[i] := 90
    else if Pos( C_EQ_SIGN_B, sI_Order[i]) > 0 then
      nI_Angle[i] := -30
    else if Pos( C_EQ_SIGN_C, sI_Order[i]) > 0 then
      nI_Angle[i] := -150
    else
      nI_Angle[i] := 0;

    if Pos('-', sI_Order[i]) > 0 then
    begin
//      if nI_Angle[i] > 0 then
//      begin
        nI_Angle[i] := nI_Angle[i] - 180;
//      end
//      else
//      begin
//        nI_Angle[i] := nI_Angle[i] + 180;
//      end;
    end;

    // o 角度 （容性感性）
    if GetUIAngle < 0 then
      bOI_Angle[i] := false
    else
      bOI_Angle[i] := true;
    // 分析电压电流夹角
    nO_Angle[i] := nU_Angle[i] - nI_Angle[i];

    sI_Order[i] := StringReplace( sI_Order[i], '-', '', [] );

    // 整理角度
    sO_Order[i] := StringReplace( sI_Order[i], '-', '', [] );
    sO_Order[i] := StringReplace( sO_Order[i], 'I', '\o', [] );

    GetUIO( GetUIAngle, nU_Angle[i], nI_Angle[i], sO_Order[i], nO_Angle[i], bOI_Angle[i] );
  end;

  //----------------------------------------------------------------------------
  // 公式初始化
  //----------------------------------------------------------------------------
  for i := 0 to 2 do
  begin
    with APhaseEprs[ i ] do
    begin
      ExpressU := sU_Order[ i ];
      ExpressI := sI_Order[ i ];
      ExpressO := sO_Order[ i ];
      AngleU   := nU_Angle[ i ];
      AngleI   := nI_Angle[ i ];
      AngleO   := nO_Angle[ i ];
      SignO    := bOI_Angle[ i ];
    end;

    GenerateSingleExpression( APhaseEprs[ i ] );
  end;
end;

procedure TWE_EXPRESSION.GetExpressions4PT(
  APhaseEprs: array of TWE_PHASE_EXPRESSION);
  function GetIsExist(aList : array of string; s : string) : Boolean;
  var
    j: Integer;
  begin
    Result := True;
    for j := 0 to Length(aList) - 1 do
    begin
      if Pos(aList[j], s) = 0 then
      begin
        Result := False;
        Break;
      end;
    end;
  end;

  // 获取s1在s2中出现次数
  function stringn(s1,s2:string):integer;
  begin
    result:=0;
    while pos(s1,s2)>0 do
    begin
     s2:=copy(s2,pos(s1,s2)+1,9999);   //假设s2最大长度为9999个字符
     result:=result+1 ;
   end;
  end;
var
  sU_Order : array[0..2] of string;        // 电压
  sI_Order : array[0..2] of string;        // 电流
  sO_Order : array[0..2] of string;        // 角度

  nU_Angle : array[0..2] of Integer;       // U角度
  nI_Angle : array[0..2] of Integer;       // I角度
  nO_Angle : array[0..2] of Integer;       // O角度
  bOI_Angle : array[0..2] of Boolean;      // 正负标识

  i : Integer;
  sIa, sIb, sIc : string;
begin
  sIa := 'I' + SuffixVar( C_EQ_SIGN_A );
  sIb := 'I' + SuffixVar( C_EQ_SIGN_B );
  sIc := 'I' + SuffixVar( C_EQ_SIGN_C );

  GetP4USignPT( sU_Order );
  GetP4ISign( sI_Order );


  // 分析角度
  for i := 0 to 2 do
  begin
    IntToStr(i);
    // 电压
    if Pos( C_EQ_SIGN_A, sU_Order[i] ) > 0 then
      nU_Angle[i] := 90
    else if Pos( C_EQ_SIGN_B, sU_Order[i] ) > 0 then
      nU_Angle[i] := -30
    else if Pos( C_EQ_SIGN_C, sU_Order[i]) > 0 then
      nU_Angle[i] := -150
    else
      nU_Angle[i] := 0;

    if Pos('-', sU_Order[i]) > 0 then
      nU_Angle[i] := nU_Angle[i] - 180;

    sU_Order[i] := StringReplace( sU_Order[i], '-', '', [] );



    if stringn('I',sI_Order[i]) = 3 then
    begin
      if sI_Order[i] = '-' + sIa + '-' + sIb + '-' + sIc then
      begin
        sI_Order[i] := '';
        nO_Angle[i] := 0;
      end
      else if sI_Order[i] = '-' + sIa + sIb + '-' + sIc then
      begin
        sI_Order[i] := '2'+sIb;
      end
      else if sI_Order[i] = '-' + sIa+ '-' + sIb  + sIc then
      begin
        sI_Order[i] := '2'+sIc;
      end
      else if sI_Order[i] = sIa+ '-' + sIb  + '-' + sIc then
      begin
        sI_Order[i] := '2'+sIa;
      end
      else if sI_Order[i] = sIa+ sIb  + '-' + sIc then
      begin
        sI_Order[i] := '-2'+sIc;
      end
      else if sI_Order[i] = sIa+ '-' + sIb  + sIc then
      begin
        sI_Order[i] := '-2'+sIb;
      end
      else if sI_Order[i] = sIa + sIb + sIc then
      begin
        sI_Order[i] := '';
      end
    end
    else if stringn('I',sI_Order[i]) = 2 then
    begin
      if sI_Order[i] = sIa + sIb then
      begin
        sI_Order[i] := '-' + sIc;
      end
      else if sI_Order[i] = sIa + sIc then
      begin
        sI_Order[i] := '-' + sIb;
      end
      else if sI_Order[i] = sIb + sIc then
      begin
        sI_Order[i] := '-' + sIa;
      end
      else if sI_Order[i] = '-' +sIa + '-' +sIb then
      begin
        sI_Order[i] := sIc;
      end
      else if sI_Order[i] = '-' +sIa + '-' +sIc then
      begin
        sI_Order[i] := sIb;
      end
      else if sI_Order[i] = '-' +sIb + '-' +sIc then
      begin
        sI_Order[i] := sIa;
      end
    end;


    // 电流
    if Pos( C_EQ_SIGN_A, sI_Order[i]) > 0 then
      nI_Angle[i] := 90
    else if Pos( C_EQ_SIGN_B, sI_Order[i]) > 0 then
      nI_Angle[i] := -30
    else if Pos( C_EQ_SIGN_C, sI_Order[i]) > 0 then
      nI_Angle[i] := -150
    else
      nI_Angle[i] := 0;

    if Pos('-', sI_Order[i]) > 0 then
    begin
//      if nI_Angle[i] > 0 then
//      begin
        nI_Angle[i] := nI_Angle[i] - 180;
//      end
//      else
//      begin
//        nI_Angle[i] := nI_Angle[i] + 180;
//      end;
    end;
    // o 角度
    if GetUIAngle < 0 then
      bOI_Angle[i] := True
    else
      bOI_Angle[i] := False;

    nO_Angle[i] := nU_Angle[i] - nI_Angle[i];
    bOI_Angle[i] := not bOI_Angle[i];

    sI_Order[i] := StringReplace( sI_Order[i], '-', '', [] );


    // 整理角度
    sO_Order[i] := StringReplace( sI_Order[i], '-', '', [] );
    sO_Order[i] := StringReplace( sO_Order[i], 'I', '\o', [] );
    GetUIO( GetUIAngle, nU_Angle[i], nI_Angle[i], sO_Order[i], nO_Angle[i], bOI_Angle[i] );
  end;

  //----------------------------------------------------------------------------
  // 公式初始化
  //----------------------------------------------------------------------------
  for i := 0 to 2 do
  begin
    with APhaseEprs[ i ] do
    begin
      ExpressU := sU_Order[ i ];
      ExpressI := sI_Order[ i ];
      ExpressO := sO_Order[ i ];
      AngleU   := nU_Angle[ i ];
      AngleI   := nI_Angle[ i ];
      AngleO   := nO_Angle[ i ];
      SignO    := bOI_Angle[ i ];
    end;

    GenerateSingleExpression( APhaseEprs[ i ] );
  end;
end;

procedure TWE_EXPRESSION.GetExpressionsSingle(APhaseEprs: TWE_PHASE_EXPRESSION);
var
  sU_Order : array[0..2] of string;        // 电压
  sI_Order : array[0..2] of string;        // 电流
  sO_Order : array[0..2] of string;        // 角度

  nU_Angle : array[0..2] of Integer;       // U角度
  nI_Angle : array[0..2] of Integer;       // I角度
  nO_Angle : array[0..2] of Integer;       // O角度
  bOI_Angle : array[0..2] of Boolean;      // 正负标识

  i : Integer;
begin
  GetP4USign( sU_Order );
  GetP4ISign( sI_Order );


  // 分析角度
  for i := 0 to 0 do
  begin
    IntToStr(i);
    // 电压
    if Pos( C_EQ_SIGN_A, sU_Order[i] ) > 0 then
      nU_Angle[i] := 90
    else if Pos( C_EQ_SIGN_B, sU_Order[i] ) > 0 then
      nU_Angle[i] := -30
    else if Pos( C_EQ_SIGN_C, sU_Order[i]) > 0 then
      nU_Angle[i] := -150
    else
      nU_Angle[i] := 0;


    // 电流
    if Pos( C_EQ_SIGN_A, sI_Order[i]) > 0 then
      nI_Angle[i] := 90
    else if Pos( C_EQ_SIGN_B, sI_Order[i]) > 0 then
      nI_Angle[i] := -30
    else if Pos( C_EQ_SIGN_C, sI_Order[i]) > 0 then
      nI_Angle[i] := -150
    else
      nI_Angle[i] := 0;

    if Pos('-', sI_Order[i]) > 0 then
    begin
      nI_Angle[i] := nI_Angle[i] - 180;
    end;

    // o 角度 （容性感性）
    if GetUIAngle < 0 then
      bOI_Angle[i] := false
    else
      bOI_Angle[i] := true;
    // 分析电压电流夹角
    nO_Angle[i] := nU_Angle[i] - nI_Angle[i];

    sI_Order[i] := StringReplace( sI_Order[i], '-', '', [] );

    // 整理角度
    sO_Order[i] := StringReplace( sI_Order[i], '-', '', [] );
    sO_Order[i] := StringReplace( sO_Order[i], 'I', '\o', [] );
    GetUIO( GetUIAngle, nU_Angle[i], nI_Angle[i], sO_Order[i], nO_Angle[i], bOI_Angle[i] );
  end;

  //----------------------------------------------------------------------------
  // 公式初始化
  //----------------------------------------------------------------------------

    with APhaseEprs do
    begin
      ExpressU := sU_Order[ 0 ];
      ExpressI := sI_Order[ 0 ];
      ExpressO := sO_Order[ 0 ];
      AngleU   := nU_Angle[ 0 ];
      AngleI   := nI_Angle[ 0 ];
      AngleO   := nO_Angle[ 0 ];
      SignO    := bOI_Angle[ 0 ];
    end;

    GenerateSingleExpression( APhaseEprs );
end;

constructor TWE_EXPRESSION.Create;
begin
  FWiringError := TWIRING_ERROR.Create;
  FUIAngle := 20;
end;

destructor TWE_EXPRESSION.Destroy;
begin
  FWiringError.Free;
  inherited;
end;

procedure TWE_EXPRESSION.GenerateSingleExpression(
  APhaseEpr: TWE_PHASE_EXPRESSION);

  function JointEpr( AU, AI, AO : string ) : string;
  begin
    if ( AU = EmptyStr ) or ( AI = EmptyStr ) then
    begin
      Result := '0';
      Exit;
    end;  

    Result := AU;

    if Pos( '-', AI ) > 0 then
      Result := Result + '(' + AI + ')'
    else
      Result := Result + AI;

    if Pos( '`', AO ) > 0 then
      Result := Result + 'cos(' + AO + ')'
    else
      Result := Result + 'cos' + AO;
  end;
begin
  if not Assigned( APhaseEpr ) then
    Exit;

  with APhaseEpr do
  begin
    Expression := JointEpr( ExpressU, ExpressI, ExpressO );

    ExpressUT := ExpressU;
    if ( Pos( C_EQ_SIGN_SQRT3, ExpressUT ) > 0 ) or
       ( Pos( C_EQ_SIGN_1_2, ExpressUT ) > 0 ) or
       ( Pos( C_EQ_SIGN_SQRT3_2, ExpressUT ) > 0 ) then
    begin
      ExpressUT := StringReplace( ExpressUT, C_EQ_SIGN_1_2, '', [rfIgnoreCase]);
      ExpressUT := StringReplace( ExpressUT, C_EQ_SIGN_SQRT3_2, '', [rfIgnoreCase]);
      ExpressUT := StringReplace( ExpressUT, C_EQ_SIGN_SQRT3, '', [rfIgnoreCase] );
      ExpressUT := ExpressUT + '''';
    end;

    ExpressIT := ExpressI;
    if ( Pos( C_EQ_SIGN_SQRT3, ExpressIT ) > 0 ) or
       ( Pos( C_EQ_SIGN_1_2, ExpressIT ) > 0 ) or
       ( Pos( C_EQ_SIGN_SQRT3_2, ExpressIT ) > 0 ) then
    begin
      ExpressIT := StringReplace( ExpressIT, C_EQ_SIGN_1_2, '', [rfIgnoreCase]);
      ExpressIT := StringReplace( ExpressIT, C_EQ_SIGN_SQRT3_2, '', [rfIgnoreCase]);
      ExpressIT := StringReplace( ExpressIT, C_EQ_SIGN_SQRT3, '', [rfIgnoreCase] );
      ExpressIT := ExpressIT + '''';
    end;

    ExpressionT := JointEpr( ExpressUT, ExpressIT, ExpressO );
  end;
end;

procedure TWE_EXPRESSION.GetExpressions3( APhaseEprs : array of TWE_PHASE_EXPRESSION );
var
  sU1, sU2 : string;
  sI1, sI2 : string;
  sO1, sO2 : string;

  nU1, nU2 : Integer;   // U角度
  nI1, nI2 : Integer;   // I角度
  nO1, nO2 : Integer;   // O角度
  bO1, bO2 : Boolean;   // 正负标识
begin
  // 获取电压和电流
  GetP3USign( sU1, sU2 );
  GetP3ISign( sI1, sI2 );

  // 计算电压角度
  nU1 := GetP3UAngle( sU1 );
  nU2 := GetP3UAngle( sU2 );

  // 电流及角度
  GetP3IAngle( sI1, sO1, nI1, sU1 );
  GetP3IAngle( sI2, sO2, nI2, sU2 );

  // 角度
  GetUIO( GetUIAngle, nU1, nI1, sO1, nO1, bO1 );
  GetUIO( GetUIAngle, nU2, nI2, sO2, nO2, bO2 );

  //----------------------------------------------------------------------------
  // 公式初始化
  //----------------------------------------------------------------------------
    sI1 := StringReplace( sI1, '-', '', [] );
    sI2 := StringReplace( sI2, '-', '', [] );
  // 公式一
  with APhaseEprs[ 0 ] do
  begin

    ExpressU := sU1;
    ExpressI := sI1;
    ExpressO := sO1;
    AngleU   := nU1;
    AngleI   := nI1;
    AngleO   := nO1;
    SignO    := bO1;
  end;

  // 公式二
  with APhaseEprs[ 1 ] do
  begin
    ExpressU := sU2;
    ExpressI := sI2;
    ExpressO := sO2;
    AngleU   := nU2;
    AngleI   := nI2;
    AngleO   := nO2;
    SignO    := bO2;
  end;

  GenerateSingleExpression( APhaseEprs[ 0 ] );
  GenerateSingleExpression( APhaseEprs[ 1 ] );
end;


procedure TWE_EXPRESSION.ChangeSignSequence(var ASignList: array of string;
  ASequence: TWE_SEQUENCE_TYPE);
var
  aNewSignList : array of string;
  i: Integer;
begin
  if Length( ASignList ) <> 3 then
    Exit;
  
  SetLength( aNewSignList, 3 );

  case ASequence of
    stABC:
    begin
      aNewSignList[0] := ASignList[0];
      aNewSignList[1] := ASignList[1];
      aNewSignList[2] := ASignList[2];
    end;

    stACB:
    begin
      aNewSignList[0] := ASignList[0];
      aNewSignList[1] := ASignList[2];
      aNewSignList[2] := ASignList[1];
    end;

    stBAC:
    begin
      aNewSignList[0] := ASignList[1];
      aNewSignList[1] := ASignList[0];
      aNewSignList[2] := ASignList[2];
    end;

    stBCA:
    begin
      aNewSignList[0] := ASignList[1];
      aNewSignList[1] := ASignList[2];
      aNewSignList[2] := ASignList[0];
    end;

    stCAB:
    begin
      aNewSignList[0] := ASignList[2];
      aNewSignList[1] := ASignList[0];
      aNewSignList[2] := ASignList[1];
    end;

    stCBA:
    begin
      aNewSignList[0] := ASignList[2];
      aNewSignList[1] := ASignList[1];
      aNewSignList[2] := ASignList[0];
    end;
  end;

  for i := 0 to 3 - 1 do
    ASignList[ i ] := aNewSignList[ i ];
end;

procedure TWE_EXPRESSION.GetP3ISign(out AI1, AI2: string);
var
  i : Integer;
  sI : array[0..3] of string;   // 电流相序
  nI : array[0..3] of Integer;  // 电流编号 a-1 b-2 c-3 断相-0
  sIa, sIb, sIc : string;
  sI1, sI2 : string;
  bHalf : Boolean;  // 是否分流

  function GetIOrder( ALineType : TWE_PHASE_LINE_TYPE ) : string;
  begin
    case ALineType of
      plA: Result := sIa;
      plC: Result := sIc;
      plN: Result := sIb;
    else
      Result := EmptyStr;
    end;
  end;

  procedure CleanIStr( var s : string );
  begin
    if Pos( C_EQ_SIGN_B, s ) > 0 then
      s := StringReplace( s, C_EQ_SIGN_B, '', [] );

    if Pos( '-', s ) > 0 then
    begin
      // 去掉+-，-+，--
      s := StringReplace( s, '+-', '-', [rfReplaceAll] );
      s := StringReplace( s, '-+', '-', [rfReplaceAll] );
      s := StringReplace( s, '--', '+', [rfReplaceAll] );
    end;
  end;

  /// <summary>
  /// 元件电流状态
  /// </summary>
  function IStatus( AID : Integer ) : Integer;
  var
    ID1, ID2 : Integer;
  begin
    // 1. 输入输出相等( Iin = Iout ), I = ''
    // 2. 输入输出中有一个断( Iin = X or Iout = X ), I = '' 或 电流叠加和分流
    // 3. 输入输出中有一个接In( Iin = In or Iout = In )，另外一个接 Ia, Ic,
    //    可能会引起电流叠加和分流
    // 4. 输入输出中没有In，都接Ia或Ic, 可能会引起电流叠加和分流
    if AID in [ 1, 2 ] then
    begin
      ID1 := ( AID - 1 ) * 2;
      ID2 := ( AID - 1 ) * 2 + 1;

      if nI[ ID1 ] = nI[ ID2 ] then
        Result := 1
      else if ( nI[ ID1 ] = 0 ) or ( nI[ ID2 ] = 0 ) then
        Result := 2
      else if ( nI[ ID1 ] = 2 ) or ( nI[ ID2 ] = 2 ) then
        Result := 3
      else
        Result := 4;
    end
    else
      raise Exception.Create( '电流序号错' );
  end;

  /// <summary>
  /// 计算两个元件电流
  /// </summary>
  procedure ProcI1I2;
  begin
    // 如果元件进入相同的电流
    // 一元件电流
    if nI[ 0 ] = nI[ 1 ] then
      sI1 := ''
    else if ( nI[ 0 ] = 0 ) or ( nI[ 1 ] = 0 ) then  // 如果为断相, 会报警
      sI1 := ''
    else if ( sI[ 1 ] = '' ) or ( sI[ 1 ] = C_EQ_SIGN_B ) then
      sI1 := sI[ 0 ]
    else
    begin
      sI1 := sI[ 0 ] + '-' + sI[ 1 ];
      CleanIStr( sI1 );
    end;  

    // 二元件电流
    if nI[ 2 ] = nI[ 3 ] then
      sI2 := ''
    else if ( nI[ 2 ] = 0 ) or ( nI[ 3 ] = 0 ) then  // 如果为断相, 会报警
      sI2 := ''
    else if ( sI[ 3 ] = '' ) or ( sI[ 3 ] = C_EQ_SIGN_B ) then
      sI2 := sI[ 2 ]
    else
    begin
      sI2 := sI[ 2 ] + '-' + sI[ 3 ];
      CleanIStr( sI2 );
    end;  

    if bHalf then   // 如果分流
    begin
      if sI1 <> '' then
        sI1 := C_EQ_SIGN_1_2 + sI1;

      if sI2 <> '' then
        sI2 := C_EQ_SIGN_1_2 + sI2;
    end;
  end;

  /// <summary>
  /// 处理两个元件都有断相的情况
  /// </summary>
  procedure ProcAllBroken;
  begin
    // 如果两元件都有断相
    if ( nI[ 0 ] = 0 ) and ( nI[ 2 ] = 0 ) then  // I1in <-> I2in
    begin
      nI[ 0 ] := nI[ 3 ];
      nI[ 2 ] := nI[ 1 ];
      sI[ 0 ] := sI[ 3 ];
      sI[ 2 ] := sI[ 1 ];
    end
    else if ( nI[ 0 ] = 0 ) and ( nI[ 3 ] = 0 ) then  // I1in <-> I2out
    begin
      nI[ 0 ] := nI[ 2 ];
      nI[ 3 ] := nI[ 1 ];
      sI[ 0 ] := sI[ 3 ];
      sI[ 3 ] := sI[ 1 ];
    end
    else if ( nI[ 1 ] = 0 ) and ( nI[ 2 ] = 0 ) then  // I1out <-> I2in
    begin
      nI[ 1 ] := nI[ 3 ];
      nI[ 2 ] := nI[ 0 ];
      sI[ 1 ] := sI[ 3 ];
      sI[ 2 ] := sI[ 0 ];
    end
    else // I1out <-> I2out
    begin
      // 如果是In断，则减半
      bHalf := True;

      nI[ 1 ] := nI[ 2 ];
      nI[ 3 ] := nI[ 0 ];
      sI[ 1 ] := sI[ 2 ];
      sI[ 3 ] := sI[ 0 ];
    end;
  end;

  /// <summary>
  /// 处理有一个In的情况, 另一个无In的情况
  /// </summary>
  procedure ProcOneIn;
  begin
    // 1元件没有In, 2元件接In
    if IStatus( 1 ) = 4 then
    begin
      if ( nI[ 0 ] = nI[ 2 ] ) or ( nI[ 0 ] = nI[ 3 ] ) then // I1进连接2元件
      begin
        // I1进->I2进, I2出接地
        if nI[ 0 ] = nI[ 2 ] then
        begin
          sI[ 3 ] := '-' + sI[ 1 ];
          CleanIStr( sI[ 3 ] );
          nI[ 3 ] := nI[ 1 ];
        end
        else  // I1进->I2出, I2进接地
        begin
          sI[ 2 ] := '-' + sI[ 1 ];
          CleanIStr( sI[ 2 ] );
          nI[ 2 ] := nI[ 1 ];
        end;

        nI[ 0 ] := 2;
        sI[ 0 ] := sIb;
      end
      else // I1出连接2元件
      begin
        // I1出->I2进, I2出接地
        if nI[ 1 ] = nI[ 2 ] then
        begin
          sI[ 3 ] := '-' + sI[ 0 ];
          CleanIStr( sI[ 3 ] );
          nI[ 3 ] := nI[ 0 ];
        end
        else  // I1出->I2出, I2进接地
        begin
          sI[ 2 ] := '-' + sI[ 0 ];
          CleanIStr( sI[ 2 ] );
          nI[ 2 ] := nI[ 0 ];
        end;

        nI[ 1 ] := 2;
        sI[ 1 ] := sIb;
      end;
    end
    else // 2元件没有In, 1元件接In
    begin
      if ( nI[ 2 ] = nI[ 0 ] ) or ( nI[ 2 ] = nI[ 1 ] ) then // I2进连接1元件
      begin
        // I2进->I1进, I1出接地
        if nI[ 2 ] = nI[ 0 ] then
        begin
          sI[ 1 ] := '-' + sI[ 3 ];
          CleanIStr( sI[ 1 ] );
          nI[ 1 ] := nI[ 3 ];
        end
        else  // I2进->I1出, I1进接地
        begin
          sI[ 0 ] := '-' + sI[ 3 ];
          CleanIStr( sI[ 0 ] );
          nI[ 0 ] := nI[ 3 ];
        end;

        nI[ 2 ] := 2;
        sI[ 2 ] := sIb;
      end
      else
      begin
        // I2出->I1进, I1出接地
        if nI[ 3 ] = nI[ 0 ] then
        begin
          sI[ 1 ] := '-' + sI[ 2 ];
          CleanIStr( sI[ 1 ] );
          nI[ 1 ] := nI[ 2 ];
        end
        else  // I2出->I1出, I1进接地
        begin
          sI[ 0 ] := '-' + sI[ 2 ];
          CleanIStr( sI[ 0 ] );
          nI[ 0 ] := nI[ 2 ];
        end;

        nI[ 3 ] := 2;
        sI[ 3 ] := sIb;
      end;
    end;  
  end;
begin
  bHalf := False;
  sIa := C_EQ_SIGN_A;
  sIb := C_EQ_SIGN_B;
  sIc := C_EQ_SIGN_C;

  // 电流副边反
  if sIa <> EmptyStr then
    if FWiringError.CT1Reverse then     // Ia 反
      sIa := '-' + sIa
    else
      sIa := '+' + sIa;

  if sIc <> EmptyStr then
    if FWiringError.CT2Reverse then     // Ic 反
      sIc := '-' + sIc
    else
      sIc := '+' + sIc;

  // 二次电流短路
  if FWiringError.CT1Short then
    sIa := sIb;

  if FWiringError.CT2Short then
    sIc := sIb;

  // 二次电流开路
  if FWiringError.InBroken then    // In 开路
    sIb := '';

  if FWiringError.IaBroken then
    sIa := '';

  if FWiringError.IcBroken then
    sIc := '';

  // 电流相序
  sI[ 0 ] := GetIOrder( FWiringError.I1In );
  sI[ 1 ] := GetIOrder( FWiringError.I1Out );
  sI[ 2 ] := GetIOrder( FWiringError.I2In );
  sI[ 3 ] := GetIOrder( FWiringError.I2Out );

  // 电流编号
  for i := 0 to 3 do
  begin
    if Pos( C_EQ_SIGN_A, sI[ i ] ) > 0 then
      nI[ i ] := 1
    else if Pos( C_EQ_SIGN_C, sI[ i ] ) > 0 then
      nI[ i ] := 3
    else if Pos( C_EQ_SIGN_B, sI[ i ] ) > 0 then
      nI[ i ] := 2
    else  // 断相
      nI[ i ] := 0;
  end;

  // 每个元件都有四种状态
  // 1. 输入输出相等( Iin = Iout ), I = ''
  // 2. 输入输出中有一个断( Iin = X or Iout = X ), I = '' 或 可能会引起电流叠加和分流
  // 3. 输入输出中有一个接In( Iin = In or Iout = In )，另外一个接 Ia, Ic, 可能会引起电流叠加和分流
  // 4. 输入输出中没有In，都接Ia或Ic, 可能会引起电流叠加和分流

  // 两个元件可能会出现16种情况
  case IStatus( 1 ) * 10 + IStatus( 2 ) of
    11, 12, 21, 13, 31, 23, 32 : ProcI1I2;
    14, 41 :
    begin
      bHalf := True; // 会分流
      ProcI1I2;
    end;

    22 :
    begin
      ProcAllBroken;
      ProcI1I2;
    end;  

    33 :
    begin
      // 如果是相同的电流, 则分流
      if ( ( nI[ 0 ] = nI[ 2 ] ) and ( nI[ 1 ] = nI[ 3 ] ) ) or
         ( ( nI[ 1 ] = nI[ 2 ] ) and ( nI[ 2 ] = nI[ 3 ] ) ) then
        bHalf := True;

      ProcI1I2;
    end;

    34, 43 :
    begin
      ProcOneIn;
      ProcI1I2;
    end;

    42, 24 :
    begin
      // 只有断n的情况，电流叠加
      bHalf := True;
      ProcI1I2;
    end;

    44 :
    begin
      bHalf := True;
      ProcI1I2;
    end;  
  end;

  AI1 := sI1;
  AI2 := sI2;
end;

procedure TWE_EXPRESSION.GetUIO(const dAngle: Double; const nU, nI: Integer;
  var sO: string; var nO: Integer; var bO: Boolean);
begin
  if bPubIsVectorAngle then
  begin
    bO := False;
  end
  else
  begin
    // 默认为负号， 电流在电压右边，逆向为负
    if dAngle < 0 then
      bO := True
    else
      bO := False;
  end;

  // 计算夹角
  nO := nU - nI;
  if nO > 360 then
    nO := nO - 360;

  bO := not bO;

  if nO > 180 then
  begin
    nO := 360 - nO;
    bO := not bO;
  end
  else if nO < - 180 then
  begin
    nO := 360 + nO;
  end
  else if nO < 0 then
  begin
    nO := -nO;
    bO := not bO;
  end;

  // 生成角度
  if bO then
  begin
    if nO <> 0 then
    begin
      if (nO = 180) and (dAngle > 0) then
      begin
        sO := IntToStr(nO) + '`' + '-' + sO;
      end
      else
      begin
        sO := IntToStr(nO) + '`' + '+' + sO;
      end;
    end;
  end
  else
  begin
    if nO <> 0 then
      sO := IntToStr(nO) + '`' + '-' + sO;
  end;
end;

function TWE_EXPRESSION.GetUIAngle: Double;
  function GetAngle(sCos : string) : Double;
  var
    dFi : Double;
    s : string;
    bC : Boolean;
  begin

    bC := Pos('C',sCos) > 0;
    if not bC then
      bC := Pos('c',sCos) > 0;

    s := StringReplace(sCos, 'L', '', [rfReplaceAll]);
    s := StringReplace(s, 'C', '', [rfReplaceAll]);
    s := StringReplace(s, 'l', '', [rfReplaceAll]);
    s := StringReplace(s, 'c', '', [rfReplaceAll]);

    TryStrToFloat(s, dFi);
    if dFi > 1 then
      dFi := 1;

    Result := ArcCos(dFi)/pi*180;

    if bC then
      Result := -Result;
  end;
begin
  with TIniFile.Create( ChangeFileExt( Application.ExeName, '.ini' ) ) do
  begin
    if ReadInteger( 'Like', 'CalcKType', 1) = 1 then
    begin
      Result := FUIAngle;
    end
    else
    begin
      Result := GetAngle(ReadString('Like', 'CalcKCos', '0.866'));
    end;

    Free;
  end;
end;

procedure TWE_EXPRESSION.SetWiringError(const Value: TWIRING_ERROR);
begin
  FWiringError.Assign( Value );
end;

function TWE_EXPRESSION.GetP3UAngle(const sU: string): Integer;
begin
  if sU = EmptyStr then
    Result := 0
  else if Pos('_a_b', sU) > 0 then
    Result := 120
  else if Pos('_a_c', sU) > 0 then
  begin
    // 如果系数有sqrt3，则是PT反引起的
    if ( pos( C_EQ_SIGN_SQRT3, sU ) > 0 ) then
    begin
      if FWiringError.PT1Reverse and ( not FWiringError.PT2Reverse ) then
        Result := -30
      else if ( not FWiringError.PT1Reverse ) and FWiringError.PT2Reverse then
        Result := 150
      else if FWiringError.PT1Reverse and FWiringError.PT2Reverse then
        Result := 60 // 应该不存在这种情况，如果两个都反，这里应该U.c.a
      else
        Result := 60;
    end
    else
      Result := 60;
  end
  else if Pos('_b_a', sU) > 0 then
    Result := -60
  else if Pos('_b_c', sU) > 0 then
    Result := 0
  else if Pos('_c_a', sU) > 0 then
  begin
    // 如果系数有sqrt3，则是PT反引起的
    if ( pos( C_EQ_SIGN_SQRT3, sU ) > 0 ) then
    begin
      if FWiringError.PT1Reverse and ( not FWiringError.PT2Reverse ) then
        Result := 150
      else if ( not FWiringError.PT1Reverse ) and FWiringError.PT2Reverse then
        Result := -30
      else if FWiringError.PT1Reverse and FWiringError.PT2Reverse then
        Result := -120
      else
        Result := -120;
    end
    else
      Result := -120;
  end
  else   // Ucb
    Result := 180;
end;

procedure TWE_EXPRESSION.GetP3USign(out AU1, AU2: string);
  /// <summary>
  /// 获取电压表达式
  /// </summary>
  function GetU( sUO1, sUO2 : string ) : string;
  begin
    case SubStrCount( '-', sUO1 + sUO2 ) of
      0 : // 没有电压反
        Result := 'U' + SuffixVar( sUO1 ) + SuffixVar( sUO2 );

      1 : // 一个电压反
        if ( Pos( C_EQ_SIGN_A, sUO1 + sUO2 ) > 0 ) and
           ( Pos( C_EQ_SIGN_C, sUO1 + sUO2 ) > 0 ) then
          Result := C_EQ_SIGN_SQRT3 + 'U' + SuffixVar( sUO1 ) + SuffixVar( sUO2 )
        else
          Result := 'U' + SuffixVar( sUO2 ) + SuffixVar( sUO1 );

      2 : // 两个电压反
        Result := 'U' + SuffixVar( sUO2 ) + SuffixVar( sUO1 );
    end;

    Result := StringReplace( Result, '-', '', [ rfReplaceAll ] );
  end;
var
  sU_Order : array[0..2] of string;   // 电压相序
  sU1, sU2 : string;
begin
  sU_Order[0] := C_EQ_SIGN_A;
  sU_Order[1] := C_EQ_SIGN_B;
  sU_Order[2] := C_EQ_SIGN_C;

  // 一次断相
  if FWiringError.UaBroken or FWiringError.UsaBroken then
    sU_Order[0] := EmptyStr;

  if FWiringError.UbBroken or FWiringError.UsbBroken then
    sU_Order[1] := EmptyStr;

  if FWiringError.UcBroken or FWiringError.UscBroken then
    sU_Order[2] := EmptyStr;

  // 电压副边 方向
  if FWiringError.PT1Reverse and ( sU_Order[0] <> EmptyStr ) then
    sU_Order[0] := '-' + sU_Order[0];  // PT1反

  if FWiringError.PT2Reverse and ( sU_Order[2] <> EmptyStr ) then
    sU_Order[2] := '-' + sU_Order[2];  // PT2反

  // 电压副边 相序
  ChangeSignSequence( sU_Order, FWiringError.USequence );

  // 合成U1/U2
  if ( sU_Order[ 0 ] <> EmptyStr ) and
     ( sU_Order[ 1 ] <> EmptyStr ) then
  begin
    sU1 := GetU( sU_Order[0], sU_Order[1] );
  end;

  if ( sU_Order[ 2 ] <> EmptyStr ) and
     ( sU_Order[ 1 ] <> EmptyStr ) then
  begin
    sU2 := GetU( sU_Order[2], sU_Order[1] );
  end;

  // 断2相以上时
  if ( ( sU_Order[ 0 ] = EmptyStr ) and ( sU_Order[ 1 ] = EmptyStr ) ) or
     ( ( sU_Order[ 0 ] = EmptyStr ) and ( sU_Order[ 2 ] = EmptyStr ) ) or
     ( ( sU_Order[ 1 ] = EmptyStr ) and ( sU_Order[ 2 ] = EmptyStr ) ) then
  begin
    sU1 := EmptyStr;
    sU2 := EmptyStr;
  end
  else if sU_Order[ 0 ] = EmptyStr then
  begin
    sU1 := EmptyStr;
  end
  else if sU_Order[ 2 ] = EmptyStr then
  begin
    sU2 := EmptyStr;
  end
  else if sU_Order[ 1 ] = EmptyStr then    // B 断相
  begin
    sU1 := C_EQ_SIGN_1_2 + GetU( sU_Order[0], sU_Order[2] );
    sU2 := C_EQ_SIGN_1_2 + GetU( sU_Order[2], sU_Order[0] );

    if FWiringError.UbBroken then   // 一次断相
      if FWiringError.PT1Reverse then
      begin
        sU2 := StringReplace( sU2, C_EQ_SIGN_SQRT3, '', [] );
        sU1 := sU2;
      end
      else if FWiringError.PT2Reverse  then
      begin
        sU1 := StringReplace( sU1, C_EQ_SIGN_SQRT3, '', [] );
        sU2 := sU1;
      end;
  end;

  // 清理数据, 返回值
  CleanSigns( sU1 );
  CleanSigns( sU2 );

  if Pos( C_EQ_SIGN_1_2 + C_EQ_SIGN_SQRT3, sU1 ) > 0 then
    AU1 := StringReplace( sU1, C_EQ_SIGN_1_2 + C_EQ_SIGN_SQRT3, C_EQ_SIGN_SQRT3_2, [] )
  else
    AU1 := sU1;

  if Pos( C_EQ_SIGN_1_2 + C_EQ_SIGN_SQRT3, sU2 ) > 0 then
    AU2 := StringReplace( sU2, C_EQ_SIGN_1_2 + C_EQ_SIGN_SQRT3, C_EQ_SIGN_SQRT3_2, [] )
  else
    AU2 := sU2;
end;

procedure TWE_EXPRESSION.GetP4ISign(var AIOrder: array of string);
  function GetValue(sI : string; bIsReverse : Boolean) : string;
  begin
    if sI = 'A' then
    begin
      Result := AIOrder[ 0 ];
      if bIsReverse then
        Result := '-' + Result;
    end
    else if sI = 'B' then
    begin
      Result := AIOrder[ 1 ];
      if bIsReverse then
        Result := '-' + Result;
    end
    else if sI = 'C' then
    begin
      Result := AIOrder[ 2 ];
      if bIsReverse then
        Result := '-' + Result;
    end
    else
    begin
      if bIsReverse then
      begin
        Result := AIOrder[ 0 ] + AIOrder[ 1 ] +AIOrder[ 2 ];
      end
      else
      begin
        if AIOrder[ 0 ] <> '' then
          Result := Result + '-'+AIOrder[ 0 ];
        if AIOrder[ 1 ] <> '' then
          Result := Result + '-'+AIOrder[ 1 ];
        if AIOrder[ 2 ] <> '' then
          Result := Result + '-'+AIOrder[ 2 ];
      end;
    end;
  end;
var
  sI1, sI2, sI3 : string;
  bI1Reverse, bI2Reverse, bI3Reverse : Boolean;
begin
  if Length( AIOrder ) <> 3 then
    Exit;

  AIOrder[ 0 ] := 'I' + SuffixVar( C_EQ_SIGN_A );
  AIOrder[ 1 ] := 'I' + SuffixVar( C_EQ_SIGN_B );
  AIOrder[ 2 ] := 'I' + SuffixVar( C_EQ_SIGN_C );

//  // 电流正反向
//  if (FWiringError.CT1Reverse and not FWiringError.I1Reverse) or
//    (not FWiringError.CT1Reverse and FWiringError.I1Reverse) then
//    AIOrder[ 0 ] := '-' + AIOrder[ 0 ];
//  if (FWiringError.CT2Reverse and not FWiringError.I2Reverse) or
//    (not FWiringError.CT2Reverse and FWiringError.I2Reverse) then
//    AIOrder[ 1 ] := '-' + AIOrder[ 1 ];
//  if (FWiringError.CT3Reverse and not FWiringError.I3Reverse) or
//    (not FWiringError.CT3Reverse and FWiringError.I3Reverse)  then
//    AIOrder[ 2 ] := '-' + AIOrder[ 2 ];

  // 电流正反向
  if FWiringError.CT1Reverse then
    AIOrder[ 0 ] := '-' + AIOrder[ 0 ];
  if FWiringError.CT2Reverse then
    AIOrder[ 1 ] := '-' + AIOrder[ 1 ];
  if FWiringError.CT3Reverse then
    AIOrder[ 2 ] := '-' + AIOrder[ 2 ];

  // 二次电流开路
  // 二次电流短路
  if FWiringError.IaBroken or FWiringError.CT1Short then
    AIOrder[ 0 ] := '';

  if FWiringError.IbBroken or FWiringError.CT2Short then
    AIOrder[ 1 ] := '';

  if FWiringError.IcBroken or FWiringError.CT3Short then
    AIOrder[ 2 ] := '';


  if FWiringError.IsCanSetClearLinkeError and FWiringError.IsClearLinke then
  begin
    FWiringError.GetClearLinkeISequence(sI1, sI2, sI3,bI1Reverse, bI2Reverse, bI3Reverse);

    sI1 := GetValue(sI1, bI1Reverse);
    sI2 := GetValue(sI2, bI2Reverse);
    sI3 := GetValue(sI3, bI3Reverse);


    AIOrder[ 0 ] := sI1;
    AIOrder[ 1 ] := sI2;
    AIOrder[ 2 ] := sI3;

//    // 电流正反向
//    if  bI1Reverse then
//      AIOrder[ 0 ] := '-' + AIOrder[ 0 ];
//    if bI2Reverse then
//      AIOrder[ 1 ] := '-' + AIOrder[ 1 ];
//    if bI3Reverse then
//      AIOrder[ 2 ] := '-' + AIOrder[ 2 ];


//    s := sI1 + sI2 + sI3;
//    if (s = 'ABC') or (s = 'ACB') or (s = 'BAC') or (s = 'BCA') or (s = 'CBA') or (s = 'CAB') then
//    begin
//      if s ='ABC' then FWiringError.ISequence := stABC;
//      if s ='ACB' then FWiringError.ISequence := stACB;
//      if s ='BAC' then FWiringError.ISequence := stBAC;
//      if s ='BCA' then FWiringError.ISequence := stBCA;
//      if s ='CBA' then FWiringError.ISequence := stCBA;
//      if s ='CAB' then FWiringError.ISequence := stCAB;
//
//      // 电流 相序
//      ChangeSignSequence( AIOrder, FWiringError.ISequence );
//
//
//    end
//    else
//    begin
//
//    end;


  end
  else
  begin
    // 电流 相序
    ChangeSignSequence( AIOrder, FWiringError.ISequence );

    // 电流正反向
    if  FWiringError.I1Reverse then
      AIOrder[ 0 ] := '-' + AIOrder[ 0 ];
    if FWiringError.I2Reverse then
      AIOrder[ 1 ] := '-' + AIOrder[ 1 ];
    if FWiringError.I3Reverse then
      AIOrder[ 2 ] := '-' + AIOrder[ 2 ];
  end;

  AIOrder[ 0 ] := StringReplace(AIOrder[ 0 ], '--', '', [rfReplaceAll]);
  AIOrder[ 1 ] := StringReplace(AIOrder[ 1 ], '--', '', [rfReplaceAll]);
  AIOrder[ 2 ] := StringReplace(AIOrder[ 2 ], '--', '', [rfReplaceAll]);
end;

procedure TWE_EXPRESSION.GetP4USign( var AUOrder : array of string );
begin
  if Length( AUOrder ) <> 3 then
    Exit;

  AUOrder[0] := 'U' + SuffixVar( C_EQ_SIGN_A );
  AUOrder[1] := 'U' + SuffixVar( C_EQ_SIGN_B );
  AUOrder[2] := 'U' + SuffixVar( C_EQ_SIGN_C );

  // 断相
  if FWiringError.UaBroken then
    AUOrder[0] := '';

  if FWiringError.UbBroken then
    AUOrder[1] := '';

  if FWiringError.UcBroken then
    AUOrder[2] := '';

  // 相序
  ChangeSignSequence( AUOrder, FWiringError.USequence );
end;

procedure TWE_EXPRESSION.GetP4USignPT(var AUOrder: array of string);
begin
  if Length( AUOrder ) <> 3 then
    Exit;

  AUOrder[0] := 'U' + SuffixVar( C_EQ_SIGN_A );
  AUOrder[1] := 'U' + SuffixVar( C_EQ_SIGN_B );
  AUOrder[2] := 'U' + SuffixVar( C_EQ_SIGN_C );

  //PT反向
  if FWiringError.PT1Reverse then
    AUOrder[0] := '-' + AUOrder[0];
  if FWiringError.PT2Reverse then
    AUOrder[1] := '-' + AUOrder[1];
  if FWiringError.PT3Reverse then
    AUOrder[2] := '-' + AUOrder[2];

  // 断相
  if (FWiringError.UaBroken) or (FWiringError.UsaBroken) then
    AUOrder[0] := '';

  if (FWiringError.UbBroken) or (FWiringError.UsbBroken) then
    AUOrder[1] := '';

  if (FWiringError.UcBroken) or (FWiringError.UscBroken) then
    AUOrder[2] := '';

  // 相序
  ChangeSignSequence( AUOrder, FWiringError.USequence );
end;

{ TWE_PHASE_EXPRESSION }

procedure TWE_PHASE_EXPRESSION.Clear;
begin
  ExpressionT := EmptyStr;
  ExpressUT := EmptyStr;
  ExpressIT := EmptyStr;

  Expression := EmptyStr;
  ExpressU := EmptyStr;  
  ExpressI := EmptyStr;  
  ExpressO := EmptyStr;
  AngleU   := 0;  
  AngleI   := 0;  
  AngleO   := 0;
  SignO    := True;

  Result1  := EmptyStr; 
  Result2  := EmptyStr; 
  CoCos    := EmptyStr; 
  CoSin    := EmptyStr;
end;

end.
