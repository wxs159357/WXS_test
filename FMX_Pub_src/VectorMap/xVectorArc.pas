unit xVectorArc;

interface

uses Classes, SysUtils, Math, System.Types, System.UITypes, FMX.Controls,
  FMX.StdCtrls, FMX.Objects, FMX.Graphics, xVectorType, xVectorLine, FMX.Types;


type
  /// <summary>
  /// 向量弧线
  /// </summary>
  TVectorArc = class

  private
    FArcR: Single;
    FEndVector: TVectorLineInfo;
    FVID: Integer;
    FStartVector: TVectorLineInfo;
    FVName: string;

    FCanvas: TControl;
    FCenterPoint: TPointF;
    FIsSelected: Boolean;
    FOnMouseDown: TMouseEvent;

    procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure MouseLeave(Sender: TObject);
    procedure DblClick(Sender: TObject);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
          Shift: TShiftState; X, Y: Single);
    procedure SetIsSelected(const Value: Boolean);


  public
    FArc : TArc;
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TVectorArc);

    /// <summary>
    /// 向量ID
    /// </summary>
    property VID : Integer read FVID write FVID;

    /// <summary>
    /// 向量名称
    /// </summary>
    property VName : string read FVName write FVName;

    /// <summary>
    /// 起始向量
    /// </summary>
    property StartVector : TVectorLineInfo read FStartVector write FStartVector;

    /// <summary>
    /// 终止向量
    /// </summary>
    property EndVector : TVectorLineInfo read FEndVector write FEndVector;

    /// <summary>
    /// 圆弧半径长度
    /// </summary>
    property ArcR : Single read FArcR write FArcR;

    /// <summary>
    /// 原点
    /// </summary>
    property CenterPoint : TPointF read FCenterPoint write FCenterPoint;

    /// <summary>
    /// 画布
    /// </summary>
    property Canvas : TControl read FCanvas write FCanvas;

    /// <summary>
    /// 画图
    /// </summary>
    procedure Draw;

    /// <summary>
    /// 是否被选中
    /// </summary>
    property IsSelected : Boolean read FIsSelected write SetIsSelected;

    /// <summary>
    /// 鼠标按下事件
    /// </summary>
    property OnMouseDown : TMouseEvent read FOnMouseDown write FOnMouseDown;
  end;

implementation

{ TVectorArc }

procedure TVectorArc.Assign(Source: TVectorArc);
begin
  if Assigned(Source) then
  begin
    FArcR       := Source.ArcR;
    FEndVector  := Source.EndVector;
    FVID        := Source.VID;
    FStartVector:= Source.StartVector;
    FVName      := Source.VName;
  end;
end;

constructor TVectorArc.Create;
begin
  FArcR := 15;
  FIsSelected := False;
end;

procedure TVectorArc.DblClick(Sender: TObject);
begin

end;

destructor TVectorArc.Destroy;
begin
  if Assigned(FArc) then
    FArc.Free;

  inherited;
end;

procedure TVectorArc.Draw;
  function AdjustAngle(dAngle : Double) : Double;
  var
    dTemp : Integer;
  begin
    dTemp := Round(dAngle*1000);

    dTemp := dTemp mod 360000;

    Result := dTemp / 1000;

    if Result < 0 then
      Result := 360 + Result;

  end;
begin
  if Assigned(FStartVector) and Assigned(FEndVector) then
  begin
    if not Assigned(FArc) then
      FArc := TArc.Create(FCanvas);

    FArc.Parent := FCanvas;
    FArc.Position.X := FCenterPoint.X - FArcR;
    FArc.Position.Y := FCenterPoint.Y - FArcR;
    FArc.Width := FArcR*2;
    FArc.Height := FArcR*2;
    FArc.Stroke.Color := FStartVector.VColor;
    FArc.Cursor := crHandPoint;

    FArc.StartAngle := -FStartVector.VAngle;
    FArc.EndAngle := AdjustAngle(FStartVector.VAngle-FEndVector.VAngle);


    FArc.OnMouseMove := MouseMove;
    FArc.OnMouseLeave := MouseLeave;
    FArc.OnDblClick := DblClick;
    FArc.OnMouseDown := MouseDown;


  end;
end;

procedure TVectorArc.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if Assigned(FOnMouseDown) then
  begin
    FOnMouseDown(Self, Button, Shift, X, Y);
  end;
end;

procedure TVectorArc.MouseLeave(Sender: TObject);
begin
  if FIsSelected then
  begin
    FArc.Stroke.Color := C_COLOR_SELECT;
  end
  else
  begin
    FArc.Stroke.Color := FStartVector.VColor;
  end;
end;

procedure TVectorArc.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  FArc.Stroke.Color := C_COLOR_SELECT;
end;

procedure TVectorArc.SetIsSelected(const Value: Boolean);
begin
  FIsSelected := Value;
end;

end.

