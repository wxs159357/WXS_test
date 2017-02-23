unit FrmWEPhaseColor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, U_WE_PHASE_MAP, U_WE_EQUATION,
  U_WIRING_ERROR, U_WE_EQUATION_DRAW, U_POWER_PHASE_MAP, system.UITypes;

type
  TfWEPhaseColor = class(TForm)
    btnColor0: TBitBtn;
    btnColor1: TBitBtn;
    btnColor2: TBitBtn;
    btnColor3: TBitBtn;
    btnColor4: TBitBtn;
    imgColor0: TImage;
    imgColor1: TImage;
    imgColor2: TImage;
    imgColor3: TImage;
    imgColor4: TImage;
    imgPhasePreview: TImage;
    lbl1: TLabel;
    lbl3: TLabel;
    lblColor1: TLabel;
    lblColor2: TLabel;
    lblColor3: TLabel;
    dlgColor: TColorDialog;
    grp1: TGroupBox;
    rbPhase3Map: TRadioButton;
    rbPhase4Map: TRadioButton;
    bvl1: TBevel;
    bvl2: TBevel;
    btnOK: TButton;
    btnCancel: TButton;
    btnDefaultValue: TButton;
    procedure btnColor0Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbPhase3MapClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnDefaultValueClick(Sender: TObject);
  private
    { Private declarations }
    ColorPhaseMap : TWE_PHASE_MAP;
    ColorEquation : TWE_EQUATION;
    ColorWiringError : TWIRING_ERROR;
    TempMapColor : TWE_PHASE_MAP_COLOR;

    /// <summary>
    /// 刷新颜色
    /// </summary>
    procedure RefeshImgColor;
    procedure ChangeIMGColor( IMG : TImage; AColor : TColor );

    /// <summary>
    /// 刷新向量图
    /// </summary>
    procedure RefeshMap;

    /// <summary>
    /// 设置成默认值
    /// </summary>
    procedure SetDefColor;
  public
    { Public declarations }
    /// <summary>
    /// 重设向量图颜色
    /// </summary>
    procedure ResetMapColor;
  end;

var
  fWEPhaseColor: TfWEPhaseColor;

implementation

{$R *.dfm}

procedure TfWEPhaseColor.btnColor0Click(Sender: TObject);
var
  nIndex, i : Integer;
  AIMG : TImage;
begin
  nIndex := TBitBtn(Sender).Tag;
  AIMG := nil;
  
  for i := 0 to ComponentCount - 1 do
    if (Components[ i ] is TImage) and ( TImage( Components[ i ] ).Tag = nIndex ) then
      if Pos( 'Color', ( TImage( Components[ i ] ).Name )) > 0  then
        AIMG := TImage( Components[ i ] );

  if not Assigned( AIMG ) then
    Exit;
    
  dlgColor.Color := AIMG.Canvas.Brush.Color;

  if dlgColor.Execute then
  begin
    AIMG.Canvas.Brush.Color := dlgColor.Color;
    ChangeIMGColor(AIMG, dlgColor.Color );

    if rbPhase3Map.Checked then
    begin
      ColorEquation.WiringError.PhaseType := ptThree;
      case nIndex of
        0 : PhaseMapColor.Background := AIMG.Canvas.Brush.Color;
        1 : PhaseMapColor.PhaseLine  := AIMG.Canvas.Brush.Color;
        2 : PhaseMapColor.PhaseAB    := AIMG.Canvas.Brush.Color;
        3 : PhaseMapColor.PhaseCB    := AIMG.Canvas.Brush.Color;
        4 : PhaseMapColor.DotLine    := AIMG.Canvas.Brush.Color;
      end;
    end
    else
    begin
      ColorEquation.WiringError.PhaseType := ptFour;
      case nIndex of
        0 : PhaseMapColor.Background := AIMG.Canvas.Brush.Color;
        1 : PhaseMapColor.PhaseA     := AIMG.Canvas.Brush.Color;
        2 : PhaseMapColor.PhaseB     := AIMG.Canvas.Brush.Color;
        3 : PhaseMapColor.PhaseC     := AIMG.Canvas.Brush.Color;
        4 : PhaseMapColor.DotLine    := AIMG.Canvas.Brush.Color;
      end;
    end;

    RefeshMap;
  end;
end;

procedure TfWEPhaseColor.btnDefaultValueClick(Sender: TObject);
begin
  SetDefColor;

  RefeshImgColor;
  RefeshMap;
end;

procedure TfWEPhaseColor.FormCreate(Sender: TObject);
begin
  ColorPhaseMap := TWE_PHASE_MAP.Create(nil);
  ColorPhaseMap.Canvas := imgPhasePreview.Canvas;
  ColorPhaseMap.Rect := imgPhasePreview.ClientRect;

  ColorWiringError := TWIRING_ERROR.Create;
  ColorEquation := TWE_EQUATION.Create;
  ColorWiringError.PhaseType := ptThree;
  ColorEquation.GenerateEquations( ColorWiringError, 20 );
end;

procedure TfWEPhaseColor.FormDestroy(Sender: TObject);
begin
  ColorPhaseMap.Free;
  ColorEquation.Free;
  ColorWiringError.Free;
end;

procedure TfWEPhaseColor.FormShow(Sender: TObject);
begin
  TempMapColor := PhaseMapColor;

  RefeshImgColor;
  RefeshMap;
end;

procedure TfWEPhaseColor.rbPhase3MapClick(Sender: TObject);
begin
  if rbPhase3Map.Checked then
  begin
    lblColor1.Caption := 'Ua, Ub, Uc';
    lblColor2.Caption := 'Uab, Ia';
    lblColor3.Caption := 'Ucb, Ic';
  end
  else
  begin
    lblColor1.Caption := 'Ua, Ia';
    lblColor2.Caption := 'Ub, Ib';
    lblColor3.Caption := 'Uc, Ic';
  end;

  RefeshMap;
  RefeshImgColor;
end;


procedure TfWEPhaseColor.ChangeIMGColor(IMG: TImage; AColor: TColor);
begin
  IMG.Canvas.Brush.Color := AColor;
  IMG.Canvas.Rectangle( 0, 0, IMG.Width, IMG.Height );
end;

procedure TfWEPhaseColor.RefeshImgColor;
begin
  if rbPhase3Map.Checked then
  begin
    ChangeIMGColor(imgColor0, PhaseMapColor.Background );
    ChangeIMGColor(imgColor1, PhaseMapColor.PhaseLine);
    ChangeIMGColor(imgColor2, PhaseMapColor.PhaseAB);
    ChangeIMGColor(imgColor3, PhaseMapColor.PhaseCB);
    ChangeIMGColor(imgColor4, PhaseMapColor.DotLine);
  end
  else
  begin
    ChangeIMGColor(imgColor0, PhaseMapColor.Background);
    ChangeIMGColor(imgColor1, PhaseMapColor.PhaseA);
    ChangeIMGColor(imgColor2, PhaseMapColor.PhaseB);
    ChangeIMGColor(imgColor3, PhaseMapColor.PhaseC);
    ChangeIMGColor(imgColor4, PhaseMapColor.DotLine);
  end;
end;

procedure TfWEPhaseColor.RefeshMap;
begin
  if rbPhase3Map.Checked then
    ColorWiringError.PhaseType := ptThree
  else
    ColorWiringError.PhaseType := ptFour;
  
  ColorEquation.GenerateEquations( ColorWiringError, 20 );
  ColorPhaseMap.DrawPhaseMap( ColorEquation );
end;

procedure TfWEPhaseColor.ResetMapColor;
begin
  if ShowModal <> mrOk then
  begin
    PhaseMapColor := TempMapColor;
  end
  else
  begin
    PowerPhaseMap.Background := PhaseMapColor.Background;
    PowerPhaseMap.DotLine    := PhaseMapColor.DotLine;
    PowerPhaseMap.PhaseLine  := PhaseMapColor.PhaseLine;
    PowerPhaseMap.PhaseAB    := PhaseMapColor.PhaseAB;
    PowerPhaseMap.PhaseCB    := PhaseMapColor.PhaseCB;
    PowerPhaseMap.PhaseA     := PhaseMapColor.PhaseA;
    PowerPhaseMap.PhaseB     := PhaseMapColor.PhaseB;
    PowerPhaseMap.PhaseC     := PhaseMapColor.PhaseC;
    PowerPhaseMap.Font       := PhaseMapColor.EquationFont;
  end;
end;

procedure TfWEPhaseColor.SetDefColor;
begin
  PhaseMapColor            := ColorPhaseMap.DefMapColor;

  PowerPhaseMap.Background := PhaseMapColor.Background;
  PowerPhaseMap.DotLine    := PhaseMapColor.DotLine;
  PowerPhaseMap.PhaseLine  := PhaseMapColor.PhaseLine;
  PowerPhaseMap.PhaseAB    := PhaseMapColor.PhaseAB;
  PowerPhaseMap.PhaseCB    := PhaseMapColor.PhaseCB;
  PowerPhaseMap.PhaseA     := PhaseMapColor.PhaseA;
  PowerPhaseMap.PhaseB     := PhaseMapColor.PhaseB;
  PowerPhaseMap.PhaseC     := PhaseMapColor.PhaseC;
  PowerPhaseMap.Font       := PhaseMapColor.EquationFont;
end;

end.
