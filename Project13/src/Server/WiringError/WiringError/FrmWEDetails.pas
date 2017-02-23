unit FrmWEDetails;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,  U_WE_EQUATION_DRAW, U_WE_DIAGRAM2,U_DIAGRAM_TYPE, U_WE_EQUATION, U_WE_PHASE_MAP,
  StdCtrls, ExtCtrls, ImgList, ActnList, XPStyleActnCtrls, ActnMan, Menus,
  U_WIRING_ERROR, IniFiles, System.Types, System.Actions, System.ImageList;

const
  C_ZOOM_RATE_STEP = 0.1;

type
  TfWEDetails = class(TForm)
    scrlbxMap: TScrollBox;
    imgMap: TImage;
    pmMap: TPopupMenu;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    actmgr1: TActionManager;
    actZoomIn: TAction;
    actZoomOut: TAction;
    actImageReset: TAction;
    actFitWindows: TAction;
    il1: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actZoomInExecute(Sender: TObject);
    procedure actZoomOutExecute(Sender: TObject);
    procedure actImageResetExecute(Sender: TObject);
    procedure actFitWindowsExecute(Sender: TObject);
    procedure imgMapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgMapMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure imgMapMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    dZoomRate : Double;
    ptBefore : TPoint;
    Bitmap : TBitmap;
    PhaseDraw : TWE_PHASE_MAP;
    EquationDraw : TWE_EQUATION_DRAW;
    DiagramDraw : TWE_DIAGRAM2;
    Equation : TWE_EQUATION;

    /// <summary>
    /// 改变图片大小
    /// </summary>
    procedure ResizeImages;

    /// <summary>
    /// 处理滚轮事件
    /// </summary>
    procedure ProcMouseWheel( var Msg : TWMMouseWheel ); message WM_MOUSEWHEEL;
  public
    { Public declarations }
    /// <summary>
    /// 接线图类型
    /// </summary>
    ADiagramType : TDiagramType;

    /// <summary>
    /// 显示错误接线信息
    /// </summary>
    procedure LoadEquation( AEquation : TWE_EQUATION ); overload;

    /// <summary>
    /// 显示错误接线信息
    /// </summary>
    procedure LoadEquation( AWiringError : TWIRING_ERROR; AAngle : Double ); overload;
  end;

var
  fWEDetails: TfWEDetails;

implementation

{$R *.dfm}

procedure TfWEDetails.actFitWindowsExecute(Sender: TObject);
begin
  dZoomRate := ClientWidth / imgMap.Picture.Width;
  ResizeImages;
end;

procedure TfWEDetails.actImageResetExecute(Sender: TObject);
begin
  dZoomRate := 1;
  ResizeImages;
end;

procedure TfWEDetails.actZoomInExecute(Sender: TObject);
begin
  if dZoomRate < 4 then
  begin
    dZoomRate := dZoomRate + C_ZOOM_RATE_STEP;
    ResizeImages;
  end;
end;

procedure TfWEDetails.actZoomOutExecute(Sender: TObject);
begin
  if dZoomRate > 0.3 then
  begin
    dZoomRate := dZoomRate - C_ZOOM_RATE_STEP;
    ResizeImages;
  end;
end;

procedure TfWEDetails.FormCreate(Sender: TObject);
begin
  imgMap.Left := 0;
  imgMap.Top  := 0;
  ClientWidth  := 1020;
  ClientHeight := 730;
  DoubleBuffered := True;
  scrlbxMap.DoubleBuffered := True;
  Bitmap := TBitmap.Create;
  Bitmap.Width := 1020;
  Bitmap.Height := 730;
  EquationDraw := TWE_EQUATION_DRAW.Create( nil );
  PhaseDraw := TWE_PHASE_MAP.Create( nil );
  DiagramDraw := TWE_DIAGRAM2.Create( nil );
  EquationDraw.Canvas := Bitmap.Canvas;
  PhaseDraw.Canvas := Bitmap.Canvas;
//  DiagramDraw.Canvas := Bitmap.Canvas;
  dZoomRate := 1;
  Equation := TWE_EQUATION.Create;
end;

procedure TfWEDetails.FormDestroy(Sender: TObject);
begin
  Bitmap.Free;
  PhaseDraw.Free;
  EquationDraw.Free;
  DiagramDraw.Free;
  Equation.Free;
end;

procedure TfWEDetails.FormShow(Sender: TObject);
begin
  PhaseDraw.MapColor := PhaseMapColor;
end;

procedure TfWEDetails.imgMapMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ptBefore.X := X;
  ptBefore.Y := Y;
  Screen.Cursor := crSize;
end;

procedure TfWEDetails.imgMapMouseMove(Sender: TObject; Shift: TShiftState; X,
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

procedure TfWEDetails.imgMapMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ptBefore := Point( -1, -1 );
  Screen.Cursor := crDefault;
end;

procedure TfWEDetails.LoadEquation(AWiringError: TWIRING_ERROR; AAngle: Double);
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
  LoadEquation( Equation );
end;

procedure TfWEDetails.LoadEquation(AEquation: TWE_EQUATION);
var
  nY : Integer;
  Analysis : TStrings;
//  p3l3, p3l4,p3l4pt: TDiagramType;
begin
  if not Assigned( AEquation ) then
    Exit;

  Caption := Format( '详细信息:(%s)%s', [ AEquation.WiringError.IDInStr,
    AEquation.WiringError.Description ] );

  with Bitmap.Canvas do     // 清画布
  begin
    Brush.Style := bsSolid;
    Brush.Color := clWhite;
    FillRect( Rect( 0, 0, Bitmap.Width, Bitmap.Height ) );
  end;

//  with TIniFile.Create( ChangeFileExt( Application.ExeName, '.ini' ) ) do
//  begin
//    p3l3 := TDiagramType(ReadInteger('Like', '3p3l', 0));
//    p3l4 := TDiagramType(ReadInteger('Like', '3p4l', 0) + 3);
//    p3l4pt := TDiagramType(ReadInteger('Like', '3p4lpt', 0)+ 6);
//    Free;
//  end;

////  DiagramDraw.Rect := Rect( 20, 30, 20 + 380, 30 + 320 );
//  if AEquation.WiringError.PhaseType = ptThree then
//  begin
//    DiagramDraw.DiagramType := p3l3;
////    if AEquation.WiringError.ProVersion = 'V2' then
////      DiagramDraw.DiagramType := dt3M ;
////    else
////      DiagramDraw.DiagramType := dt3L4;
//  end
//  else if AEquation.WiringError.PhaseType = ptFour then
//  begin
//    DiagramDraw.DiagramType := p3l4;
////    if AEquation.WiringError.ProVersion = 'V2' then
////    begin
////      DiagramDraw.DiagramType := dt4M_NoPT;
////    end;
////    else
////      DiagramDraw.DiagramType := dt4_NoPT_L6 ;
//  end
//  else  //胡红明2013.5.23
//  begin
//    DiagramDraw.DiagramType := p3l4pt;
////    DiagramDraw.DiagramType := dt4_PT_L6;
//  end;
  DiagramDraw.DiagramType := ADiagramType;
  DiagramDraw.WiringError := AEquation.WiringError;
  DiagramDraw.DrawTo(Bitmap.Canvas, Rect(20, 30, 20 + 380, 30 + 320));

  PhaseDraw.UIAngle := AEquation.UIAngle;
  PhaseDraw.Rect := Rect( 20, 380, 20 + 380, 380 + 320 );
  PhaseDraw.DrawPhaseMap( AEquation );

  EquationDraw.Rect := Rect( 400, 30, 1020 - 20, 30 + 450 );
  nY := EquationDraw.DrawEquationP( AEquation );

  EquationDraw.Rect := Rect( 400, nY, 1020 - 20, 730 - 20 );
  nY := EquationDraw.DrawEquationKH( AEquation );

  Analysis := TStringList.Create;
  AEquation.ShowAnalysis( Analysis, 120 );
  EquationDraw.Rect := Rect( 400, nY + 20, 1020 - 20, 730 - 20 );
  EquationDraw.DrawAnalysis( Analysis );
  Analysis.Free;

  imgmap.Picture.Bitmap := Bitmap;
  ResizeImages;
end;

procedure TfWEDetails.ProcMouseWheel(var Msg: TWMMouseWheel);
begin
  if Msg.Keys = 8 then
  begin
    if Msg.WheelDelta > 0 then
      actZoomInExecute( nil )
    else
      actZoomOutExecute( nil );
  end
  else
  begin
    with scrlbxMap.VertScrollBar do
      Position := Position -  Msg.WheelDelta;
  end;
end;

procedure TfWEDetails.ResizeImages;
begin
  imgMap.Width := Round( imgMap.Picture.Width * dZoomRate );
  imgMap.Height := Round( imgMap.Picture.Height * dZoomRate );
end;

end.
