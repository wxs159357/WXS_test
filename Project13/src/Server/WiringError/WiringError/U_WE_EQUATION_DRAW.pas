{===============================================================================
  Copyright(c) 2007-2009, 北京北研兴电力仪表有限责任公司
  All rights reserved.

  错误接线，公式绘制单元
  + TWE_EQUATION_DRAW  画公式

===============================================================================}

unit U_WE_EQUATION_DRAW;

interface

uses SysUtils, Classes, Windows, Graphics, IniFiles, Forms,
  U_WE_EQUATION, U_WIRING_ERROR, U_WE_EXPRESSION, U_WE_EQUATION_MATH,
  system.Types;

const
  /// <summary>
  /// 最小字体
  /// </summary>
  C_FONT_SIZE_MIN = 8;

type
  /// <summary>
  /// 画公式
  /// </summary>
  TWE_EQUATION_DRAW = class( TComponent )
  private
    FCanvas : TCanvas;
    FRect: TRect;
    FScaleRate: Double;
//    Fabc : Boolean;
    Fabc : Integer;
    procedure SetRect(const Value: TRect);
    procedure SetScaleRate(const Value: Double);

    /// <summary>
    /// 拆分字符串
    /// </summary>
    procedure SeperateStr( AStr : string; out AStr1, AStr2 : string );
  protected
    /// <summary>
    /// 获取字体大小
    /// </summary>
    function DrawFontSize( ADefaultSize : Integer = 12 ) : Integer;

    /// <summary>
    /// 初始化区域
    /// </summary>
    procedure IniCanvas( AColorBg, AColor : Integer );

    /// <summary>
    /// 画单元件表达式
    /// </summary>
    procedure DrawPhaseEquation( AEquation : TWE_PHASE_EXPRESSION; APos : TPoint;
      var AYPos : Integer; AID : Integer );

    /// <summary>
    /// 缩放后大小
    /// </summary>
    function ScaledValue( n : Integer ) : Integer;

    /// <summary>
    /// 画分数
    /// </summary>
    function DrawFrac( APos : TPoint; AStr : string ) : TPoint;
  public
    /// <summary>
    /// 画布
    /// </summary>
    property Canvas : TCanvas read FCanvas write FCanvas;

    /// <summary>
    /// 绘图的区域
    /// </summary>
    property Rect : TRect read FRect write SetRect;

    /// <summary>
    /// 缩放比例
    /// </summary>
    property ScaleRate : Double read FScaleRate write SetScaleRate;
  public
    constructor Create(AOwner: TComponent); override;

    /// <summary>
    /// 画字符串
    /// </summary>
    function DrawText( ACanvas : TCanvas; APos : TPoint; AStr : string;
      AFontColor, ABgColor : TColor; AScale : Double ) : TPoint; overload;

    /// <summary>
    /// 画字符串
    /// </summary>
    function DrawText( APos : TPoint; AStr : string ) : TPoint; overload;

    /// <summary>
    /// 画字符串
    /// </summary>
    function DrawString( APos : TPoint; AString : string ) : Integer;

    /// <summary>
    /// 绘制功率P
    /// </summary>
    function DrawEquationP( AEquation : TWE_EQUATION; AColorBg : Integer = -1;
      AColor : Integer = -1 ) : Integer; overload;

    /// <summary>
    /// 绘制功率P
    /// </summary>
    function DrawEquationP( ACanvas : TCanvas; ARect : TRect; AEquation : TWE_EQUATION; AColorBg : Integer = -1; AColor : Integer = -1 ) : Integer; overload;

    /// <summary>
    /// 绘制PK
    /// </summary>
    function DrawEquationPK( AEquation : TWE_EQUATION; AColorBg : Integer = -1;
      AColor : Integer = -1 ) : Integer;

    /// <summary>
    /// 绘制K
    /// </summary>
    function DrawEquationK( AEquation : TWE_EQUATION; AColorBg : Integer = -1;
      AColor : Integer = -1 ) : Integer;

    /// <summary>
    /// 绘制K, 横向的
    /// </summary>
    function DrawEquationKH( AEquation : TWE_EQUATION; AColorBg : Integer = -1;
      AColor : Integer = -1 ) : Integer;

    /// <summary>
    /// 绘制分析
    /// </summary>
    function DrawAnalysis( AAnalysis : TStrings; AColorBg : Integer = -1;
      AColor : Integer = -1 ) : Integer;
  end;

implementation

{ TWE_EQUATION_DRAW }

constructor TWE_EQUATION_DRAW.Create(AOwner: TComponent);
begin
  inherited;
  FScaleRate := 1;
end;

function TWE_EQUATION_DRAW.DrawEquationP(AEquation: TWE_EQUATION;
  AColorBg, AColor : Integer ) : Integer;
var
  nYMax : Integer;  // 行位置
  s, s2 : string;
  i : Integer;
  ptPos : TPoint;
begin
  // 清理背景
  IniCanvas( AColorBg, AColor );

  if not ( Assigned( AEquation ) and Assigned( FCanvas ) ) then
  begin
    Result := FRect.Top;
    Exit;
  end;

  if AEquation.WiringError.PhaseType = ptThree then
    s := 'U_a_b=U_b_c=U_c_a=U  I_a=I_b=I_c=I  \o_a=\o_b=\o_c=\o'
  else if AEquation.WiringError.PhaseType = ptSingle then
    s := ''
  else
    s := 'U_a=U_b=U_c=U  I_a=I_b=I_c=I  \o_a=\o_b=\o_c=\o';

  FCanvas.Font.Size  := Round( FScaleRate * 11 );
  FCanvas.Font.Name  := '宋体';
  FCanvas.Font.Style := [];

  if AEquation.WiringError.PhaseType <> ptSingle then
  begin
    FCanvas.TextOut( FRect.Left + 20, FRect.Top + 13, '当三相系统对称时：' );
    DrawString( Point( FRect.Left + 150, FRect.Top + 10), s );
  end;

  ptPos := Point( FRect.Left + 20, FRect.Top + 50 );

  // 相功率表达式 P*'
  nYMax := ptPos.Y;
  DrawPhaseEquation( AEquation.GetPhaseEquation( 0 ), ptPos, nYMax, 1 );
  if AEquation.WiringError.PhaseType = ptThree then
    DrawPhaseEquation( AEquation.GetPhaseEquation( 1 ), ptPos, nYMax, 2 );

  if (AEquation.WiringError.PhaseType = ptFour ) or
     (AEquation.WiringError.PhaseType = ptFourPT ) then
  begin
    DrawPhaseEquation( AEquation.GetPhaseEquation( 1 ), ptPos, nYMax, 2 );
    DrawPhaseEquation( AEquation.GetPhaseEquation( 2 ), ptPos, nYMax, 3 );
  end;
    

  // 错误接线功率 P'
  DrawString( Point( ptPos.X, nYMax ), 'P''' );

  with AEquation do
  begin
    for i := 0 to EquationP.Count - 1 do
    begin
      if Length( EquationP[i] ) > 65 then
      begin
        SeperateStr( EquationP[ i ], s, s2 );
        nYMax := DrawString( Point(ptPos.X + Round( 25 * FScaleRate ) , nYMax),
          '=' + s );

        if s2 <> '' then
          nYMax := DrawString( Point(ptPos.X + Round( 25 * FScaleRate ) , nYMax),
            '   '+ s2 );
      end
      else
      begin
        nYMax := DrawString( Point( ptPos.X + Round( 25 * FScaleRate ), nYMax ),
          '=' + EquationP[ i ] );
      end;
    end;
  end;

  Result := nYMax;
end;

function TWE_EQUATION_DRAW.DrawAnalysis(AAnalysis: TStrings; AColorBg,
  AColor: Integer): Integer;
var
  i: Integer;
begin
  Result := FRect.Top;
  
  // 清理背景
  IniCanvas( AColorBg, AColor );

  if not ( Assigned( AAnalysis ) and Assigned( FCanvas ) ) then
    Exit;

  if AAnalysis.Count > 0 then
    with FCanvas do
    begin
      Font.Size := 10;
      Font.Style := [];
      Result := FRect.Top + 10;

      for i := 0 to AAnalysis.count - 1 do
      begin
        if Trim( AAnalysis[ i ] ) <> '' then
        begin
          TextOut( FRect.Left + 20, Result, AAnalysis[ i ] );
          Result := Result + 20;
        end
        else
          Result := Result + 10;
      end;
    end;
end;

function TWE_EQUATION_DRAW.DrawEquationK(AEquation: TWE_EQUATION; AColorBg,
  AColor: Integer) : Integer;
var
  i : Integer;
  ptPos : TPoint;
  nYMax : Integer;  // 横坐标
  sStr  : string;
begin
  // 清理背景
  IniCanvas( AColorBg, AColor );

  if not ( Assigned( AEquation ) and Assigned( FCanvas ) ) then
  begin
    Result := FRect.Top;
    Exit;
  end;

  // 初始化
  nYMax := FRect.Top + 20;
  ptPos := Point( FRect.Left + 20, FRect.Top + 20 );

  // 公式输出
  DrawString( ptPos, 'K' );
  DrawString( Point( ptPos.X + Round( 15 * FScaleRate ), ptPos.Y ), '='  );

  for i := 0 to AEquation.EquationK.Count - 1 do
  begin
    sStr := AEquation.EquationK[i];

    if i = 0 then
    begin
      nYMax := DrawString( Point(ptPos.X + Round( 30 * FScaleRate ), ptPos.Y), sStr );
    end
    else
    begin
      if Pos( '/', sStr ) > 0 then
        nYMax := nYMax + 10 ;

      if Pos( '{', Copy(sStr, 1, Pos( '/', sStr ))) > 0 then
        nYMax := nYMax + 20 ;

      DrawString( Point(ptPos.X + Round( 15 * FScaleRate ), nYMax), '=' );
      nYMax := DrawString( Point(ptPos.X + Round( 30 * FScaleRate ), nYMax), sStr );
    end;
  end;

  Result := nYMax;
end;

function TWE_EQUATION_DRAW.DrawEquationKH(AEquation: TWE_EQUATION; AColorBg,
  AColor: Integer): Integer;
var
  i : Integer;
  nYPos : Integer;
  ptPos : TPoint;
begin
  // 清理背景
  IniCanvas( AColorBg, AColor );

  if not ( Assigned( AEquation ) and Assigned( FCanvas ) ) then
  begin
    Result := FRect.Top;
    Exit;
  end;

  // 初始化
  nYPos := FRect.Top + 20;
  ptPos := Point( FRect.Left + 20, nYPos );

  // 公式输出
  ptPos := DrawText( ptPos, 'K ' );

  for i := 0 to AEquation.EquationK.Count - 1 do
  begin
    ptPos := DrawText( Point( ptPos.X, nYPos ), ' = ' );
    ptPos := DrawText( Point( ptPos.X, nYPos ), AEquation.EquationK[ i ] );
  end;

  Result := ptPos.Y;
end;

function TWE_EQUATION_DRAW.DrawEquationP(ACanvas: TCanvas; ARect : TRect;
  AEquation: TWE_EQUATION; AColorBg, AColor: Integer) : Integer;
begin
  FCanvas := ACanvas;
  FRect := ARect;
  Result := DrawEquationP( AEquation, AColorBg, AColor );
end;

function TWE_EQUATION_DRAW.DrawEquationPK(AEquation: TWE_EQUATION; AColorBg,
  AColor: Integer) : Integer;
var
  sEqK : string;
  nYMax : Integer;
  ptPos : TPoint;
begin
  // 清理背景
  IniCanvas( AColorBg, AColor );

  if not ( Assigned( AEquation ) and Assigned( FCanvas ) ) then
  begin
    Result := FRect.Top;
    Exit;
  end;

  ptPos := Point( FRect.Left + 20, FRect.Top + 20 );
  nYMax := ptPos.Y;

  with FCanvas do
  begin
    if AEquation.WiringError.PhaseType = ptThree then
      sEqK := 'U_a_b=U_b_c=U_c_a=U'
    else
      sEqK := 'U_a=U_b=U_c=U';

    Font.Size  := Round( FScaleRate * 11 );
    Font.Name  := '宋体';
    Font.Style := [];

    TextOut( FRect.Left + 20, FRect.Top + 13, '当三相系统对称时：');

    DrawString( Point(FRect.Left + 150, FRect.Top + 10), sEqK );
    sEqK := 'I_a=I_b=I_c=I  \o_a=\o_b=\o_c=\o';
    nYMax := DrawString( Point(FRect.Left + 150, FRect.Top + 30), sEqK );
  end;

  // 相功率表达式 P*'
  DrawPhaseEquation( AEquation.GetPhaseEquation( 0 ), ptPos, nYMax, 1 );
  DrawPhaseEquation( AEquation.GetPhaseEquation( 1 ), ptPos, nYMax, 2 );

  if AEquation.WiringError.PhaseType = ptFour then
    DrawPhaseEquation( AEquation.GetPhaseEquation( 2 ), ptPos, nYMax, 3 );

  // 绘制 P'
  if AEquation.EquationP.Count > 0 then
  begin
    sEqK := '=' + AEquation.EquationP[ AEquation.EquationP.Count - 1 ];
    DrawString( Point( ptPos.X, nYMax ), 'P''' );
    nYMax := DrawString( Point( ptPos.X + Round( 25 * FScaleRate ), nYMax ), sEqK );
  end;

  // 绘制 K
  if AEquation.EquationK.Count > 0 then
  begin
    sEqK := AEquation.EquationK[ AEquation.EquationK.Count - 1 ];
    DrawString( Point(ptPos.X, nYMax), 'K =' );
    DrawString( Point(ptPos.X + Round( 40 * FScaleRate ), nYMax), sEqK );
  end;

  Result := nYMax;
end;

procedure TWE_EQUATION_DRAW.DrawPhaseEquation(AEquation: TWE_PHASE_EXPRESSION;
  APos: TPoint; var AYPos: Integer; AID : Integer);
var
  ptPos : TPoint;
begin
  // 写 P'
  ptPos := APos;
  ptPos := DrawText( Point( ptPos.X, AYPos ), 'P' + SuffixVar( IntToStr( AID ) )
    + ''' = ' );

  with AEquation do
  begin
    ptPos := DrawText( Point( ptPos.X, AYPos ), ExpressionT );

    // 如果不存在断相
    if ExpressionT <> Expression then
      ptPos := DrawText( Point( ptPos.X, AYPos ), ' = ' + Expression );

    AYPos := ptPos.Y;
  end;
end;

function TWE_EQUATION_DRAW.DrawString(APos: TPoint; AString: string): Integer;
begin
  Result := DrawText( APos, AString ).Y;
end;

function TWE_EQUATION_DRAW.DrawText(APos: TPoint; AStr: string): TPoint;
var
  s, sText : string;
  pt, ptMax : TPoint;
  nPos : Integer;
  p : PChar;

  procedure IncPointX( var APt : TPoint; AVal : Integer );
  begin
    APt.X := APt.X + AVal;
  end;   

  procedure SetMaxPos( var APosMax : TPoint; APos : TPoint );
  begin
    if APos.X > APosMax.X then
      APosMax.X := APos.X;

    if APos.Y > APosMax.Y then
      APosMax.Y := APos.Y;
  end;

  function TextStr( APt : TPoint; AStr : string ) : TPoint;
  begin
    with FCanvas do
    begin
      TextOut( APt.X, APt.Y, AStr );
      Result := APt;
      IncPointX( Result, TextWidth( AStr ) );
    end;
  end;

  procedure SetFont( AName : string; ASize : Integer; AStyle : TFontStyles );
  begin
    with FCanvas do
    begin
      Font.Name := AName;
      Font.Style := AStyle;

      if ASize < C_FONT_SIZE_MIN then
        Font.Size := C_FONT_SIZE_MIN
      else
        Font.Size := ASize;
    end;
  end;

  procedure DrawDot( pt1, pt2 : TPoint );
  var
    nPos : Integer;
  begin
    nPos := ( pt2.X - pt1.X - ScaledValue( 2 ) ) div 2 + ScaledValue( 2 );
    FCanvas.Polyline([ Point( pt1.X + nPos, pt1.Y ),
      Point( pt1.X + nPos + ScaledValue( 2 ), pt1.Y)]);
  end;

  function DrawSqrt3( pt : TPoint ) : TPoint;
  begin
    SetFont( 'Times New Roman', ScaledValue( 12 ), [] );
    TextStr( Point( pt.X + ScaledValue( 9 ), pt.Y ), '3' );

    with FCanvas do
    begin
      Polyline( [ Point(pt.X, pt.Y + ScaledValue( 12 ) ),
                 Point(pt.X + ScaledValue( 2 ), pt.Y + ScaledValue( 11 )),
                 Point(pt.X + ScaledValue( 5 ), pt.Y + ScaledValue( 16 )),
                 Point(pt.X + ScaledValue( 9 ), pt.Y ),
                 Point(pt.X + ScaledValue( 18 ), pt.Y ) ] );

      Result := pt;
      IncPointX( Result, ScaledValue( 18 ) );
    end;
  end;    
begin
  if not Assigned( FCanvas ) then
  begin
    Result := APos;
    Exit;
  end;  

  sText := AStr;
  nPos := 1;
  Result := APos;
  pt := APos;

  // 输出分数
  if Pos( '/', sText ) > 0 then
  begin
    if Copy( sText, 1, 1 ) = '-' then
    begin
      sText := Copy( sText, 2, Length( sText ) - 1 );  // 去掉 -
      SetFont( 'Symbol', ScaledValue( 12 ), [] );
      pt := TextStr( pt, '-' );
      IncPointX( pt, ScaledValue( 4 ) );
    end;

    pt.Y := pt.Y + ScaledValue( 11 );

    Result := DrawFrac( pt, sText );
    Exit;
  end;

  repeat
    s := GetElementsFromExpression( sText, nPos );

    if s <> '' then
    begin
      p := PChar( s );

      case ElementType( s ) of
        etFrac :  // 分数
        begin
          // 去掉括号{}, 并将 ','-> '/'
          s:= StringReplace( Copy( s, 2, Length( s ) -2 ), ',', '/', [] );
          ptMax := DrawFrac( Point( pt.X, pt.Y + ScaledValue( 11 ) ), s );
          SetMaxPos( Result, ptMax );
          pt.X := ptMax.X;
          IncPointX( pt, ScaledValue( 2 ) );
        end;

        etSuffix : // 下标 '1', '2', '3', 'a', 'b', 'c', 'x'
        begin
          p := PChar( s );
          inc( p );
          if p^ = 'b' then
            IncPointX( pt, ScaledValue( 1 ) );
          //  下表 a, b, c, 改为u,v,w
          if CharInSet(p^, ['a','b','c'])  then
          begin
            with TIniFile.Create( ChangeFileExt( Application.ExeName, '.ini' ) ) do
            begin
              Fabc := ReadInteger('Like', 'abc', 0) ;
              Free;
            end;

           { if not Fabc then
            begin
              case p^ of
                'a': p := 'u';
                'b': p := 'v';
                'c': p := 'w';
              end;
            end;  }
            case Fabc of
              1:
              begin
                case p^ of
                  'a': p := 'u';
                  'b': p := 'v';
                  'c': p := 'w';
                end;
              end;
              2:
              begin
                case p^ of
                  'a': p := '1';
                  'b': p := '2';
                  'c': p := '3';
                end;
              end;
            end;
          end;

          SetFont( 'Times New Roman', ScaledValue( 8 ), [] );
          ptMax := TextStr( Point( pt.X, pt.Y + ScaledValue( 6 ) ), p^ );
          pt.X := ptMax.X;
        end;

        etDot:    // 矢量
        begin
          Inc( p );
          SetFont( 'Times New Roman', ScaledValue( 12 ), [ fsItalic ] );
          ptMax := TextStr( pt, p^ );
          DrawDot( pt, ptMax );
          pt.X := ptMax.X;
        end;

        etSpc:   // 特殊
        begin
          inc( p );
          SetFont( 'MS Reference Sans Serif', ScaledValue( 10 ), [ fsItalic ] );

          case p^ of
            'o' : pt := TextStr( pt, 'φ' );
            'a' : pt := TextStr( pt, 'α' );
            'b' : pt := TextStr( pt, 'β' );
            'q' : pt := TextStr( pt, '∞' );
          end;
        end;
      else
        case p^ of
          '(', ')' :
          begin
            IncPointX( pt, ScaledValue( 3 ) );
            SetFont( 'Symbol', ScaledValue( 12 ), [] );
            pt := TextStr( pt, p^ );
            IncPointX( pt, ScaledValue( 3 ) );
          end;

          '+', '-', '=':
          begin
            IncPointX( pt, ScaledValue( 3 ) );
            SetFont( 'Symbol', ScaledValue( 12 ), [] );
            pt := TextStr( pt, p^ );
            IncPointX( pt, ScaledValue( 4 ) );
          end;

          '`' :
          begin
            SetFont( 'Times New Roman', ScaledValue( 10 ), [] );
            pt := TextStr( pt, '°');
          end;

          'K', 'P', 'U', 'I' :
          begin
            SetFont( 'Times New Roman', ScaledValue( 12 ), [ fsItalic ] );
            IncPointX( pt, ScaledValue( 2 ) );
            pt := TextStr( pt, p^ );

            if p^ = '-' then
              IncPointX( pt, ScaledValue( 1 ) );
          end;
        else
          // '1', '2', '3', '4', '5', '6', '7', '8', '9', '0',
          // 's', 'n', 'i', 'c', 'o', 's', ''''...
          if p^ = '#' then
            pt := DrawSqrt3( pt )
          else
          begin
            SetFont( 'Times New Roman', ScaledValue( 12 ), [] );
            pt := TextStr( pt, s );
          end;
        end;
      end;
    end;
  until nPos = -1;

//  SetMaxPos( Result, Point( pt.X, pt.Y + ScaledValue( 40 ) ) );
  SetMaxPos( Result, Point( pt.X, pt.Y + ScaledValue( 35 ) ) );
end;

function TWE_EQUATION_DRAW.DrawFontSize(ADefaultSize: Integer): Integer;
begin
  Result := Round( ADefaultSize * FScaleRate );

  if Result < C_FONT_SIZE_MIN then
    Result := C_FONT_SIZE_MIN;
end;

function TWE_EQUATION_DRAW.DrawFrac( APos: TPoint; AStr: string ): TPoint;
  procedure SetMaxPos( var APosMax : TPoint; APos : TPoint );
  begin
    if APos.X > APosMax.X then
      APosMax.X := APos.X;

    if APos.Y > APosMax.Y then
      APosMax.Y := APos.Y;
  end;
var
  s1, s2 : string;
  pts1, pts2 : TPoint;
  pt1, pt2 : TPoint;
  rectOld, rectNew  : TRect;
  Bitmap : TBitmap;
  n : Integer;
  rectStore : TRect; // 用于存储
begin
  Result := APos;
  DivStr( AStr, '/', s1, s2 );            // 分割分子，分母
  s2 := Copy( s2, 2, Length( s2 ) - 1 );  // 去掉 /

  // 输出分子和分母
  pts1 := Point( APos.X, APos.Y - ScaledValue( 20 ) );
  pt1 := DrawText( pts1, s1 );

  if Pos( '{', s2 ) > 0 then      // 如果有分数，则下移，腾出空间
    ptS2 := Point( APos.X, APos.Y + ScaledValue( 12 ) )
  else
    ptS2 := Point( APos.X, APos.Y + ScaledValue( 2 ) );

  pt2 := DrawText( ptS2, s2 );

  SetMaxPos( Result, pt1 );
  SetMaxPos( Result, pt2 );

  // 调整画图位置
  if pt1.x <> pt2.x then
  begin
    if pt1.X < pt2.X then // 分子短，调整分子位置
    begin
      n := ( pt2.x - pt1.x ) div 2;

      if Pos( '{', s1 ) > 0 then   // 如果有分数，调整位置
      begin
        rectOld.TopLeft := Point( pts1.X, pts1.Y - ScaledValue( 8 ) );
        rectNew.TopLeft := Point( pts1.X + n, pts1.Y - ScaledValue( 8 ) )
      end
      else
      begin
        rectOld.TopLeft := pts1;
        rectNew.TopLeft := Point( pts1.X + n, pts1.Y );
      end;

      rectOld.BottomRight := Point( pt1.X, pt1.Y - ScaledValue( 15 ) );
      rectNew.BottomRight := Point( pt1.X + n, pt1.Y - ScaledValue( 15 ));
    end
    else if pt1.X > pt2.X then  // 调整分母
    begin
      n := ( pt1.X - pt2.X ) div 2;

      if Pos( '{', s2 ) > 0 then
      begin
        rectOld.TopLeft := Point( pts2.X, pts2.Y - ScaledValue( 8 ) );
        rectNew.TopLeft := Point( pts2.X + n, pts2.Y - ScaledValue( 8 ) )
      end
      else
      begin
        rectOld.TopLeft := pts2;
        rectNew.TopLeft := Point( pts2.X + n, pts2.Y );
      end;

      rectOld.BottomRight := Point( pt2.X, pt2.Y - ScaledValue( 15 ) );
      rectNew.BottomRight := Point( pt2.X + n, pt2.Y - ScaledValue( 15 ));
    end;

    rectStore.Top := rectOld.Top - rectOld.Top;
    rectStore.Bottom := rectOld.Bottom - rectOld.Top;
    rectStore.Left := rectOld.Left - rectOld.Left;
    rectStore.Right := rectOld.Right - rectOld.Left;

    with FCanvas do
    begin
      Bitmap := TBitmap.Create;
      Bitmap.Width := rectStore.Right + 1;
      Bitmap.Height := rectStore.Bottom + 1;
      Bitmap.Canvas.CopyRect( rectStore, FCanvas, rectOld );

      Brush.Style := bsSolid;
      FillRect(rectOld);
      Brush.Style := bsClear;
      CopyRect( rectNew, Bitmap.Canvas, rectStore);
      Bitmap.Free;
    end;
  end;

  FCanvas.Polyline( [ Point( APos.X, APos.Y ), Point( Result.X,  APos.Y ) ] );
end;

procedure TWE_EQUATION_DRAW.IniCanvas(AColorBg, AColor: Integer);
begin
  with FCanvas do
  begin
    Brush.Style := bsSolid;

    if AColorBg = -1 then
      Brush.Color := clWhite
    else
      Brush.Color := AColorBg;

    Pen.Style := psSolid;

    if AColor = -1 then
    begin
      Pen.Color := clBlack;
      Font.Color := clBlack;
    end
    else
    begin
      Pen.Color := AColor;
      Font.Color := AColor;
    end;    

    FillRect( FRect );
  end;  
end;

function TWE_EQUATION_DRAW.ScaledValue(n: Integer): Integer;
begin
  Result := Round( n * FScaleRate );
end;

procedure TWE_EQUATION_DRAW.SeperateStr(AStr: string; out AStr1,
  AStr2: string);
var
  p : PChar;
  nLen, nPos : Integer;
  i: Integer;
begin
  p := PChar( AStr );
  nLen := Length( AStr );
  nPos := -1;

  // 查询最后一个 + - 符号
  for i := nLen downto 1 do
  begin
    if nPos < 0 then  // 没有确定U的位置
    begin
      if ( p + i - 1 )^ = 'U' then  // 找到最后一个U，记录该位置
        nPos := i;
    end
    else // U的位置已确定，找符号的位置
    begin
      if CharInSet(( p + i - 1 )^, [ '+', '-' ]) then
      begin
        nPos := i;
        Break;
      end;
    end;
  end;

  if nPos > 0 then
  begin
    // 拆分
    AStr1 := Copy( AStr, 1, nPos - 1 );
    AStr2 := Copy( AStr, nPos, nLen - nPos + 1 );
  end
  else
  begin
    AStr1 := AStr;
    AStr2 := '';
  end;
end;

procedure TWE_EQUATION_DRAW.SetRect(const Value: TRect);
begin
  FRect := Value;
end;

procedure TWE_EQUATION_DRAW.SetScaleRate(const Value: Double);
begin
  FScaleRate := Value;
end;

function TWE_EQUATION_DRAW.DrawText(ACanvas: TCanvas; APos: TPoint; AStr: string;
  AFontColor, ABgColor: TColor; AScale: Double): TPoint;
begin
  if Assigned( ACanvas ) and ( AStr <> '' ) then
  begin
    FCanvas := ACanvas;
    FScaleRate := AScale;
    FCanvas.Brush.Style := bsClear;
    FCanvas.Brush.Color := ABgColor;
    FCanvas.Font.Color  := AFontColor;
    FCanvas.Pen.Color := AFontColor;
    FCanvas.Pen.Style := psSolid;

    Result := DrawText( APos, AStr );
  end
  else
    Result := APos;
end;

end.
