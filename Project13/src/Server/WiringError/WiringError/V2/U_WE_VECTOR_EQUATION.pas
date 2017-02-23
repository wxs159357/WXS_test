{===============================================================================
  Copyright(c) 2012, 北京北研兴电力仪表有限责任公司
  All rights reserved.

  错误接线，公式绘制单元
  + TWE_VECTOR_EQUATION  画公式

===============================================================================}

unit U_WE_VECTOR_EQUATION;

interface

uses SysUtils, Classes, Windows, StrUtils, Graphics, IniFiles, Forms,
  U_WE_EQUATION, U_WIRING_ERROR, U_WE_EXPRESSION, U_WE_EQUATION_MATH,
  U_DIAGRAM_TYPE, U_WE_ORGAN, U_WE_VECTOR_MAP, Math, system.Types, System.UITypes;

const
  /// <summary>
  /// 最小字体
  /// </summary>
  C_FONT_SIZE_MIN = 8;

type
  /// <summary>
  /// 画公式
  /// </summary>
  TWE_VECTOR_EQUATION = class( TComponent )
  private
    FCanvas : TCanvas;
    FRect: TRect;
    FScaleRate: Double;
//    Fabc : Boolean;
    Fabc : Integer;
    procedure SetRect(const Value: TRect);
    procedure SetScaleRate(const Value: Double);
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
    function DrawEquationP( ROrgans: TStringList;dUIAngle : Double;
      AColorBg : Integer = -1; AColor : Integer = -1 ) : Integer;
  end;

implementation

{ TWE_VECTOR_EQUATION }

constructor TWE_VECTOR_EQUATION.Create(AOwner: TComponent);
begin
  inherited;
  FScaleRate := 1;
end;

function TWE_VECTOR_EQUATION.DrawEquationP(ROrgans: TStringList;dUIAngle : Double;
  AColorBg, AColor : Integer ) : Integer;
var
  s : string;
  i : Integer;
  ptPos : TPoint;
  AOrgan: TWE_ORGAN;
  nTemp : Integer;
  slList : TStringList;
  
  procedure GetAngle;
  var
    AVectorIn, AVectorOut:TVECTOR_LINE;
    dTemp, dTemp1, dX, dY : Double;
    dLen, dUAngle : Double;
  begin
    //  电压角度
    AVectorIn:=VectorLines.GetLine(AOrgan.UInType);
    AVectorOut:= VectorLines.GetLine(AOrgan.UOutType);

    if Assigned(AVectorIn) and Assigned(AVectorOut) then
    begin
      dTemp := DegToRad(AVectorIn.LineAngle);
      dTemp1 := DegToRad(AVectorOut.LineAngle);
      dX := Cos(dTemp)-Cos(dTemp1);
      dY := Sin(dTemp)-Sin(dTemp1);

      if (dX <> 0) or (dY<>0) then
      begin
        GetLenAngle(0,0,dX*100,dY*100, dLen, dUAngle);
        dUAngle := dUAngle + DegToRad(30);
        AOrgan.UAngle := RadToDeg(dUAngle);
      end;
    end;

    // 电流角度
    AVectorIn:=VectorLines.GetLine(AOrgan.IInType);
    AVectorOut:= VectorLines.GetLine(AOrgan.IOutType);

    if Assigned(AVectorIn) then
      AOrgan.IAngle := AVectorIn.LineAngle
    else if Assigned(AVectorOut) then
      AOrgan.IAngle := AVectorOut.LineAngle+180
    else
      AOrgan.IAngle := 90;
  end;
begin
  // 清理背景
  IniCanvas( AColorBg, AColor );

  if not ( Assigned( ROrgans ) and Assigned( FCanvas ) ) then
  begin
    Result := FRect.Top;
    Exit;
  end;

  slList := TStringList.Create;

  FCanvas.Font.Size  := Round( FScaleRate * 11 );
  FCanvas.Font.Name  := '宋体';
  FCanvas.Font.Style := [];
  for i := 0 to ROrgans.Count - 1 do
  begin
    AOrgan := TWE_ORGAN(ROrgans.Objects[i]);
    GetAngle;

    nTemp := Round(Abs(AOrgan.UAngle - AOrgan.IAngle));

    if (AOrgan.UInType = AOrgan.UOutType) or (AOrgan.IInType = AOrgan.IOutType) then
    begin
      s := '0';
    end
    else
    begin
      if AOrgan.UAngle > AOrgan.IAngle then
      begin
        if nTemp > 180 then
        begin
          s := 'cos(' + IntToStr(360-nTemp)+'-\o)';
        end
        else
        begin
          s := 'cos(' + IntToStr(nTemp)+'+\o)';
        end;
      end
      else
      begin
        if nTemp > 180 then
        begin
          s := 'cos(' + IntToStr(360-nTemp)+'+\o)';
        end
        else
        begin
          s := 'cos(' + IntToStr(nTemp)+'-\o)';
        end;
      end;
      s := AOrgan.UType + AOrgan.IType + s;
      if AOrgan.UParam = pt1_2 then
        s := '{1,2}' + s;

      slList.Add('Q_'+inttostr(i+1)+'''');
    end;

    s := 'Q_'+inttostr(i+1)+''' = ' + s;
    ptPos.X := FRect.Left + 20;
    ptPos.Y := FRect.Top + 20+ 40*i;
    DrawText( ptPos, s );
  end;
  ptPos.y := ptPos.y+40;
  s := '';
  for i := 0 to slList.Count - 1 do
  begin
    if i = slList.Count - 1 then
      s := s + slList[i]
    else
      s := s + slList[i] + '+';
  end;
  if s = '' then
    s := '0';
  DrawText( ptPos, 'Q''   = ' + s );
  slList.Free;

  Result := ptPos.y + 40;
end;

procedure TWE_VECTOR_EQUATION.DrawPhaseEquation(AEquation: TWE_PHASE_EXPRESSION;
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

function TWE_VECTOR_EQUATION.DrawString(APos: TPoint; AString: string): Integer;
begin
  Result := DrawText( APos, AString ).Y;
end;

function TWE_VECTOR_EQUATION.DrawText(APos: TPoint; AStr: string): TPoint;
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
          if  CharInSet(p^, ['a','b','c'])then
          begin
            with TIniFile.Create( ChangeFileExt( Application.ExeName, '.ini' ) ) do
            begin
              Fabc := ReadInteger('Like', 'abc', 0) ;
              Free;
            end;

            {if not Fabc then
            begin
              case p^ of
                'a': p := 'u';
                'b': p := 'v';
                'c': p := 'w';
              end;
            end; }
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

function TWE_VECTOR_EQUATION.DrawFontSize(ADefaultSize: Integer): Integer;
begin
  Result := Round( ADefaultSize * FScaleRate );

  if Result < C_FONT_SIZE_MIN then
    Result := C_FONT_SIZE_MIN;
end;

function TWE_VECTOR_EQUATION.DrawFrac( APos: TPoint; AStr: string ): TPoint;
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

procedure TWE_VECTOR_EQUATION.IniCanvas(AColorBg, AColor: Integer);
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

function TWE_VECTOR_EQUATION.ScaledValue(n: Integer): Integer;
begin
  Result := Round( n * FScaleRate );
end;

procedure TWE_VECTOR_EQUATION.SetRect(const Value: TRect);
begin
  FRect := Value;
end;

procedure TWE_VECTOR_EQUATION.SetScaleRate(const Value: Double);
begin
  FScaleRate := Value;
end;

function TWE_VECTOR_EQUATION.DrawText(ACanvas: TCanvas; APos: TPoint; AStr: string;
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

