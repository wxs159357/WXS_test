unit FrmWEDetails2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,  U_WE_EQUATION_DRAW, U_WE_DIAGRAM2, U_WE_EQUATION,
  U_WE_PHASE_MAP, StdCtrls, ExtCtrls, ImgList, ActnList, XPStyleActnCtrls,
  ActnMan, Menus, U_WIRING_ERROR, Buttons, ComCtrls, U_WE_VECTOR_MAP,
  U_WE_ORGAN, U_WE_VECTOR_EQUATION, OleCtrls, xFunction,
  IniFiles, U_DIAGRAM_TYPE, System.Types;

type
  TfWEDetails2 = class(TForm)
    tbcMap: TTabControl;
    scrlbxMap: TScrollBox;
    imgMap: TImage;
//    ShockwaveFlash: TShockwaveFlash;
    pnl1: TPanel;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    lbl4: TLabel;
    lbl5: TLabel;
    edtKwh: TEdit;
    lbl6: TLabel;
    btnCalcKwh: TButton;
    edtSeeKwh: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tbcMapChange(Sender: TObject);
    procedure imgMapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgMapMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure imgMapMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure btnCalcKwhClick(Sender: TObject);
  private
    { Private declarations }
    ptBefore : TPoint;
    Bitmap : TBitmap;
    PhaseDraw : TWE_PHASE_MAP;
    VectorDraw : TWE_VECTOR_MAP;
    EquationDraw : TWE_EQUATION_DRAW;
    VectorEquation : TWE_VECTOR_EQUATION;
    DiagramDraw : TWE_Diagram2;
    Equation : TWE_EQUATION;

    /// <summary>
    /// 清画布
    /// </summary>
    procedure ClearMap;

    /// <summary>
    /// 初始化画布
    /// </summary>
    procedure IniMap;

    /// <summary>
    /// 画图
    /// </summary>
    procedure DrawMap;
  public
    { Public declarations }
    /// <summary>
    /// 接线图类型
    /// </summary>
    ADiagramType : TDiagramType;

    /// <summary>
    /// 显示错误接线信息
    /// </summary>
    procedure LoadEquation( AWiringError : TWIRING_ERROR; AAngle : Double ); overload;
  end;

var
  fWEDetails2: TfWEDetails2;

implementation

{$R *.dfm}

procedure TfWEDetails2.btnCalcKwhClick(Sender: TObject);
var
  dValue : Double;
begin
  TryStrToFloat(edtSeeKwh.Text, dValue);

  if Equation.KValue = -1 then
  begin
    edtKwh.Text := '正常月电量';
    lbl6.Caption := '= （ 无穷大 - 1 ） × '+
      FormatFloat('0.00', dValue);
  end
  else
  begin
    lbl6.Caption:='= （'+FormatFloat('0.00',Equation.KValue)+' - 1 ） × '+
      FormatFloat('0.00', dValue);
    edtKwh.Text := FormatFloat('0.00', (Equation.KValue - 1) * dValue);
  end;
end;

procedure TfWEDetails2.ClearMap;
begin
  with Bitmap.Canvas do     // 清画布
  begin
    Brush.Style := bsSolid;
    Brush.Color := clWhite;
    FillRect( Rect( 0, 0, Bitmap.Width, Bitmap.Height ) );
  end;
end;

procedure TfWEDetails2.DrawMap;
var
  nY : Integer;
  Analysis : TStringList;
  ROrgans : TStringList;
//  p3l3, p3l4,p3l4pt: TDiagramType;
begin
  ClearMap;

  pnl1.Visible := tbcMap.TabIndex = 4;

//  with TIniFile.Create( ChangeFileExt( Application.ExeName, '.ini' ) ) do
//  begin
//    p3l3 := TDiagramType(ReadInteger('Like', '3p3l', 0));
//    p3l4 := TDiagramType(ReadInteger('Like', '3p4l', 0) + 3);
//    p3l4pt := TDiagramType(ReadInteger('Like', '3p4lpt', 0)+ 6);
//    Free;
//  end;
//
//   {三相四线带电压互感器是新加的，胡红明2013.5.10}
//  if Equation.WiringError.PhaseType = ptThree then
//  begin
//    DiagramDraw.DiagramType := ADiagramType;
////    //在这里添加图的类型，最后根据协议类型设定
////    if Equation.WiringError.ProVersion = 'V2' then
////      DiagramDraw.DiagramType := dt3M;
////    else
////      DiagramDraw.DiagramType := dt3L4;
//  end
//  else if Equation.WiringError.PhaseType = ptFour then
//  begin
//    DiagramDraw.DiagramType := p3l4;
////    if Equation.WiringError.ProVersion = 'V2' then
////      DiagramDraw.DiagramType := dt4M_NoPT;
////    else
////      DiagramDraw.DiagramType := dt4_NoPT_L6;
//  end
//  else
//  begin
//    DiagramDraw.DiagramType := p3l4pt;
////    if Equation.WiringError.ProVersion = 'V2' then
////      DiagramDraw.DiagramType := dt4M_PT;
////    else
////      DiagramDraw.DiagramType := dt4_PT_L6 ;
//  end;

  case tbcMap.TabIndex of
    0 :  //接线图
    begin
      DiagramDraw.DiagramType := ADiagramType;
      DiagramDraw.WiringError := Equation.WiringError;
      DiagramDraw.DrawTo(Bitmap.Canvas, Point(20, 20));
    end;

    1 : //向量图
    begin
      PhaseDraw.Rect := Rect( 20, 20, 20 + 380, 20 + 320 );
//      PhaseDraw.Rect := Rect( 20, 20, 20 + 1000, 20 + 1000 );
      PhaseDraw.DrawPhaseMap( Equation, '有功向量图' );
      ROrgans := TStringList.Create;

      AnalysisOrgan( DiagramDraw.DiagramType, Equation.WiringError, nil, ROrgans );
      VectorDraw.Rect := Rect( 20+385, 20, 20 + 380+385, 20 + 320 );
      VectorDraw.DrawPhaseMap(Equation.WiringError.PhaseType, ROrgans,
        Equation.UIAngle, '无功向量图');

      ClearStringList(ROrgans);
      ROrgans.Free;
    end;

    2 : //功率表达式
    begin
      EquationDraw.Rect := Rect( 20, 20, Bitmap.Width - 20, Bitmap.Height - 20 );
      nY := EquationDraw.DrawEquationP( Equation );

      if (DiagramDraw.DiagramType = dt4_NoPT_L6)
        or (DiagramDraw.DiagramType = dt4_PT_L6)
        or (DiagramDraw.DiagramType = dt4M_NoPT)
        or (DiagramDraw.DiagramType = dt4M_PT)  
        or (DiagramDraw.DiagramType =  dt4_PT_CT_CLear)
        or (DiagramDraw.DiagramType = dt4Direct) then
      begin
        VectorEquation.Rect := Rect( 20, nY, Bitmap.Width - 20, Bitmap.Height - 20 );
        ROrgans := TStringList.Create;

        AnalysisOrgan( dt4_NoPT_L6, Equation.WiringError, nil, ROrgans );
        VectorEquation.DrawEquationP(ROrgans,Equation.UIAngle);

        ClearStringList(ROrgans);
        ROrgans.Free;
      end
      else if (DiagramDraw.DiagramType = dt3L4)
              or (DiagramDraw.DiagramType = dt3M )
              or (DiagramDraw.DiagramType = dt3CTClear ) then
      begin
        VectorEquation.Rect := Rect( 20, nY, Bitmap.Width - 20, Bitmap.Height - 20 );
        ROrgans := TStringList.Create;

        AnalysisOrgan( dt3L4, Equation.WiringError, nil, ROrgans );
        VectorEquation.DrawEquationP(ROrgans,Equation.UIAngle);

        ClearStringList(ROrgans);
        ROrgans.Free;
      end ;
    end;

    3 : //更正系数及分析
    begin
      EquationDraw.Rect := Rect( 20, 20, Bitmap.Width - 20, Bitmap.Height - 20 );
      nY := EquationDraw.DrawEquationK( Equation );

      Analysis := TStringList.Create;
      Equation.ShowAnalysis( Analysis, 80 );
      EquationDraw.Rect := Rect( 20, nY + 10, Bitmap.Width - 20, Bitmap.Height - 20 );
//      ShockwaveFlash.Top :=  EquationDraw.DrawAnalysis( Analysis ) + 20;
      Analysis.Free;
//
//      ShockwaveFlash.Left := 20;
//
//      // 转动方向
//      if Equation.RunTpye = 0 then
//        ShockwaveFlash.Movie := ExtractFilePath( Application.ExeName ) + '\flash\正转.swf'
//      else if Equation.RunTpye = 1 then
//        ShockwaveFlash.Movie := ExtractFilePath( Application.ExeName ) + '\flash\反转.swf'
//      else
//        ShockwaveFlash.Movie := ExtractFilePath( Application.ExeName ) + '\flash\不转.swf'
    end;
    4 : // 退补电量
    begin
      btnCalcKwhClick(nil);
    end;
  end;

  imgmap.Picture.Bitmap := Bitmap;
  scrlbxMap.HorzScrollBar.Position := 0;
  scrlbxMap.VertScrollBar.Position := 0;
end;

procedure TfWEDetails2.FormCreate(Sender: TObject);
begin
  VectorLines := TVECTOR_LINES.Create;  //胡红明添加2013.4.9
  VectorDraw := TWE_VECTOR_MAP.Create(nil);
  DoubleBuffered := True;
  scrlbxMap.DoubleBuffered := True;
  Bitmap := TBitmap.Create;
  Equation := TWE_EQUATION.Create;
  EquationDraw := TWE_EQUATION_DRAW.Create( nil );
  VectorEquation := TWE_VECTOR_EQUATION.Create( nil );
  PhaseDraw := TWE_PHASE_MAP.Create( nil );
  DiagramDraw := TWE_DIAGRAM2.Create( nil );
  DiagramDraw.Visible := False;

//  with TIniFile.Create(sPubIniFileName) do
//  begin
//    edtSeeKwh.Text := ReadString('Option', 'SeeKwh', '1000');
//    Free;
//  end;

  IniMap;
end;

procedure TfWEDetails2.FormDestroy(Sender: TObject);
begin
//  with TIniFile.Create(sPubIniFileName) do
//  begin
//    WriteString('Option', 'SeeKwh', edtSeeKwh.Text);
//    Free;
//  end;

  PhaseDraw.Free;
  EquationDraw.Free;
  VectorEquation.Free;
  DiagramDraw.Free;
  Equation.Free;
  VectorDraw.Free;
//  VectorLines.Free;
end;

procedure TfWEDetails2.FormShow(Sender: TObject);
begin
  PhaseDraw.MapColor := PhaseMapColor;
end;

procedure TfWEDetails2.imgMapMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ptBefore.X := X;
  ptBefore.Y := Y;
  Screen.Cursor := crSize;
end;

procedure TfWEDetails2.imgMapMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  nX, nY : Integer;
begin
  if ( ptBefore.X > 0 ) and ( ptBefore.Y > 0 ) then
  begin
    nX := ptBefore.X - X;
    nY := ptBefore.Y - Y;
    scrlbxMap.VertScrollBar.Position := scrlbxMap.VertScrollBar.Position + nY;
    scrlbxMap.HorzScrollBar.Position := scrlbxMap.HorzScrollBar.Position + nX;
  end;
end;

procedure TfWEDetails2.imgMapMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ptBefore := Point( -1, -1 );
  Screen.Cursor := crDefault;
end;

procedure TfWEDetails2.IniMap;
begin
  Bitmap.Width := 800;
  Bitmap.Height := 800;
  imgMap.Width := Bitmap.Width;
  imgMap.Height := Bitmap.Height;
  EquationDraw.Canvas := Bitmap.Canvas;
  VectorEquation.Canvas := Bitmap.Canvas;
  PhaseDraw.Canvas := Bitmap.Canvas;
  VectorDraw.Canvas := Bitmap.Canvas;
  EquationDraw.Rect := Rect( 0, 0, Bitmap.Width, Bitmap.Height );
  VectorEquation.Rect := Rect( 0, 0, Bitmap.Width, Bitmap.Height );
  PhaseDraw.Rect := Rect( 0, 0, Bitmap.Width, Bitmap.Height );
end;

procedure TfWEDetails2.LoadEquation(AWiringError: TWIRING_ERROR; AAngle: Double);
begin
  if AWiringError.PhaseType = ptThree then
  begin
    if not (ADiagramType in[ dt3CTClear..dt3L4]) then
    begin
      ADiagramType := dt3L4;
    end;
  end
  else if AWiringError.PhaseType = ptFour then
  begin
    if not (ADiagramType in[ dt4M_NoPT..dt4_NoPT_L6]) then
    begin
      ADiagramType := dt4_NoPT_L6;
    end;
  end
  else
  begin
    if not (ADiagramType in[ dt4M_PT..dt4_PT_L6]) then
    begin
      ADiagramType := dt4_PT_L6;
    end;
  end;


  Equation.GenerateEquations( AWiringError, AAngle );
  DrawMap;
end;

procedure TfWEDetails2.tbcMapChange(Sender: TObject);
begin
  DrawMap;
end;

end.
