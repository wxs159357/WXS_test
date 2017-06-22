unit xVectorLine;

interface

uses Classes, SysUtils, Math, System.Types, System.UITypes, FMX.Controls,
  FMX.StdCtrls, FMX.Objects, FMX.Graphics, xVectorType, FMX.Types;



type
  /// <summary>
  /// 向量信息
  /// </summary>
  TVectorLineInfo = class

  private
    FVAngle: Double;
    FVID: Integer;
    FVValue: Double;
    FVType: tTVectorType;
    FIsSelected: Boolean;
    FVColor: TAlphaColor;

    FOnChange: TNotifyEvent;
    FCenterPoint: TPointF;
    FScale: Double;
    FVName: string;
    FCanvas: TControl;
    FIsDrawPoint: Boolean;
    FIsMainSelect: Boolean;
    FIsOver: Boolean;
    FVMaxWidth: Single;
    FLine, FLineCapTop, FLineCapBottom : TLine;
    FText: TText;
    FCircle : TCircle;
    FOnDblClick: TNotifyEvent;
    FIsCanSelect: Boolean;
    FVDash: TStrokeDash;
    FVMaxValue: Single;
    FOnMouseDown: TMouseEvent;

    procedure SetIsSelected(const Value: Boolean);
    function GetVTypeStr: string;
    procedure SetVTypeStr(const Value: string);
    procedure SetVAngle(const Value: Double);

    procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure MouseLeave(Sender: TObject);
    procedure DblClick(Sender: TObject);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
          Shift: TShiftState; X, Y: Single);

    /// <summary>
    /// 基本定压电流值
    /// </summary>
    function GetBaseValue : Single;

    /// <summary>
    /// 获取向量图颜色
    /// </summary>
    function GetVectorColor: TAlphaColor;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TVectorLineInfo);

    /// <summary>
    /// 向量最大值
    /// </summary>
    property VMaxValue : Single read FVMaxValue write FVMaxValue;

    /// <summary>
    /// 向量ID
    /// </summary>
    property VID : Integer read FVID write FVID;

    /// <summary>
    /// 向量名称
    /// </summary>
    property VName : string read FVName write FVName;

    /// <summary>
    /// 向量类型
    /// </summary>
    property VType : tTVectorType read FVType write FVType;

    /// <summary>
    /// 向量类型 字符串形式
    /// </summary>
    property VTypeStr : string read GetVTypeStr write SetVTypeStr;

    /// <summary>
    /// 向量颜色
    /// </summary>
    property VColor : TAlphaColor read FVColor write FVColor;

    /// <summary>
    /// 画笔类型
    /// </summary>
    property VDash: TStrokeDash read FVDash write FVDash;

    /// <summary>
    /// 向量值 填写电压电流值
    /// </summary>
    property VValue : Double read FVValue write FVValue;

    /// <summary>
    /// 向量满量程长度
    /// </summary>
    property VMaxWidth : Single read FVMaxWidth write FVMaxWidth;

    /// <summary>
    /// 角度 原点水平向右为零度，上移为正角度
    /// </summary>
    property VAngle : Double read FVAngle write SetVAngle;

    /// <summary>
    /// 是否被选中
    /// </summary>
    property IsSelected : Boolean read FIsSelected write SetIsSelected;

    /// <summary>
    /// 是否选择的是主向量
    /// </summary>
    property IsMainSelect : Boolean read FIsMainSelect write FIsMainSelect;

    /// <summary>
    /// 鼠标是否在上面
    /// </summary>
    property IsOver : Boolean read FIsOver write FIsOver;

    /// <summary>
    /// 改变事件
    /// </summary>
    property OnChange : TNotifyEvent read FOnChange write FOnChange;

    /// <summary>
    /// 原点
    /// </summary>
    property CenterPoint : TPointF read FCenterPoint write FCenterPoint;

    /// <summary>
    /// 画布
    /// </summary>
    property Canvas : TControl read FCanvas write FCanvas;

    /// <summary>
    /// 是否画点
    /// </summary>
    property IsDrawPoint : Boolean read FIsDrawPoint write FIsDrawPoint;

    /// <summary>
    /// 画图
    /// </summary>
    procedure Draw;

    /// <summary>
    /// 双击事件
    /// </summary>
    property OnDblClick : TNotifyEvent read FOnDblClick write FOnDblClick;

    /// <summary>
    /// 鼠标按下事件
    /// </summary>
    property OnMouseDown : TMouseEvent read FOnMouseDown write FOnMouseDown;

    /// <summary>
    /// 是否允许选择 默认 允许
    /// </summary>
    property IsCanSelect : Boolean read FIsCanSelect write FIsCanSelect;
  end;

implementation

{ TVectorLineInfo }


procedure TVectorLineInfo.Assign(Source: TVectorLineInfo);
begin
  if Assigned(Source) then
  begin
    FVName           := Source.VName      ;
    FVType           := Source.VType      ;
    FVColor          := Source.VColor     ;
    FVValue          := Source.VValue     ;
    FVAngle          := Source.VAngle     ;
    FIsDrawPoint     := Source.IsDrawPoint;
  end;
end;

constructor TVectorLineInfo.Create;
begin
  FCenterPoint := Point(0, 0);
  FVColor := TAlphaColorRec.Red;
  FVValue := 220;
  FVAngle := 90;
  FVName := '未命名';
  FVType := vtVol;
  FScale := 1;
  FIsDrawPoint := True;
  FIsSelected := False;
  FIsMainSelect := False;
  FIsOver := False;
  FVMaxWidth := 200;
  FIsCanSelect := True;
  FVDash := TStrokeDash.Solid;
  FVMaxValue := 220;
end;

procedure TVectorLineInfo.DblClick(Sender: TObject);
begin
  if Assigned(FOnDblClick) then
  begin
    FOnDblClick(Self);
  end;
end;

destructor TVectorLineInfo.Destroy;
begin
  if Assigned(FLineCapTop) then
    FLineCapTop.Free;
  if Assigned(FLineCapBottom) then
    FLineCapBottom.Free;
  if Assigned(FCircle) then
    FCircle.Free;
  if Assigned(FText) then
    FText.Free;
  if Assigned(FLine) then
    FLine.Free;

  inherited;
end;

procedure TVectorLineInfo.Draw;
var
  dWidth : Single;

  procedure CreatLine(AParent : TControl; var ALine : TLine);
  begin
    if not Assigned(ALine) then
      ALine := TLine.Create(AParent);

    ALine.Parent := AParent;
    ALine.LineType := TLineType.Top;
    ALine.OnMouseMove := MouseMove;
    ALine.OnMouseLeave := MouseLeave;
    ALine.OnDblClick := DblClick;
    ALine.OnMouseDown := MouseDown;
    ALine.Height := dWidth+3;
    ALine.Stroke.Thickness := dWidth;
    ALine.Stroke.Color := GetVectorColor;
    if FIsCanSelect then
      ALine.Cursor := crHandPoint
    else
      ALine.Cursor := crDefault;

    ALine.Stroke.Dash := FVDash;
  end;
begin
  dWidth := 2;

  // 向量线
  CreatLine(FCanvas, FLine);
  FLine.Position.X := FCenterPoint.X;
  FLine.Position.Y := FCenterPoint.Y;
  FLine.Width := FVMaxWidth * FVValue/GetBaseValue;
  FLine.RotationCenter.X := 0;
  FLine.RotationCenter.Y := 0;
  FLine.RotationAngle := - FVAngle;

  FLine.BringToFront;

  // 上箭头
  CreatLine(FLine, FLineCapTop);
  FLineCapTop.Position.X := FLine.Width-1;
  FLineCapTop.Position.Y := 1;
  FLineCapTop.Width := FVMaxWidth/20+1;
  FLineCapTop.RotationCenter.X := 0;
  FLineCapTop.RotationCenter.Y := 0;
  FLineCapTop.RotationAngle := -140;

  // 下箭头
  CreatLine(FLine, FLineCapBottom);
  FLineCapBottom.Position.X := FLine.Width-1;
  FLineCapBottom.Position.Y := 1;
  FLineCapBottom.Width := FVMaxWidth/20;
  FLineCapBottom.RotationCenter.X := 0;
  FLineCapBottom.RotationCenter.Y := 0;
  FLineCapBottom.RotationAngle := 140;
  FLineCapBottom.Stroke.Thickness := dWidth;

  // 向量描述
  if not Assigned(FText) then
    FText:= TText.Create(FLine);
  FText.Parent := FLine;
  FText.Text := FVName;



  FText.Width := Length(FVName) * FText.Font.Size;
  FText.Height := 15;
  FText.Position.X := FLine.Width;
  FText.Position.Y := 1- FText.Width/2;
  FText.Color := GetVectorColor;
  FText.Font.Family := 'Times New Roman';
  FText.Font.Style := [TFontStyle.fsItalic];
  FText.RotationAngle := -FLine.RotationAngle;

  if FIsDrawPoint then
  begin
    if not Assigned(FCircle) then
      FCircle := TCircle.Create(FText);

    FCircle.Parent := FText;
    FCircle.Width := 2;
    FCircle.Height := 2;
    FCircle.Position.X := 5.5*Length(FVName);
    FCircle.Position.y := -3;
    FCircle.Stroke.Color := GetVectorColor;
  end;
end;

function TVectorLineInfo.GetBaseValue: Single;
begin
  Result := FVMaxValue;
end;

function TVectorLineInfo.GetVectorColor: TAlphaColor;
begin
  if FIsSelected then
  begin
    if FIsMainSelect then
    begin
      Result := C_COLOR_SELECT_MAIN;
    end
    else
    begin
      Result := C_COLOR_SELECT;
    end;
  end
  else
  begin
    Result := FVColor;
  end;
end;

function TVectorLineInfo.GetVTypeStr: string;
begin
  Result := GetVTStr(FVType);
end;

procedure TVectorLineInfo.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if Assigned(FOnMouseDown) then
  begin
    FOnMouseDown(Self, Button, Shift, X, Y);
  end;
end;

procedure TVectorLineInfo.MouseLeave(Sender: TObject);
begin
  if FIsCanSelect then
  begin
    FLine.Stroke.Color := GetVectorColor;
    FLineCapTop.Stroke.Color := GetVectorColor;
    FLineCapBottom.Stroke.Color := GetVectorColor;
    FText.Color := GetVectorColor;
    if FIsDrawPoint then
      FCircle.Stroke.Color := GetVectorColor;
  end;
end;

procedure TVectorLineInfo.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
//var
//  dX, dY : Single;
//  dValue : Double;
var
  AColor : TAlphaColor;
begin
  if FIsCanSelect then
  begin
    if FIsMainSelect then
    begin
      AColor := C_COLOR_SELECT_MAIN;
    end
    else
    begin
      AColor := C_COLOR_SELECT;
    end;

    FLine.Stroke.Color := AColor;
    FLineCapTop.Stroke.Color := AColor;
    FLineCapBottom.Stroke.Color := AColor;
    FText.Color := AColor;
    if FIsDrawPoint then
      FCircle.Stroke.Color := AColor;



//    if (ssLeft in Shift) and FIsMainSelect then
//    begin
//
//      dX := X;
//      dY := Y-3;
//      if Abs(dY) >=0.3 then
//      begin
//        if dX = 0 then
//          dValue := 0
//        else
//          dValue := dY/dX;
//
//          FVAngle := FVAngle - RadToDeg(ArcTan(dValue));
//
//          FLine.RotationAngle := - FVAngle;
//        sTemp := FormatFloat('0',x) + ',' + FormatFloat('0',y);
//      end;
//    end;
  end;
end;

procedure TVectorLineInfo.SetIsSelected(const Value: Boolean);
begin
  FIsSelected := Value;
end;

procedure TVectorLineInfo.SetVAngle(const Value: Double);
  function SetValue(dValue : Double) : Double;
  begin
    if dValue < 0 then
    begin
      Result := dValue + 360;

      if Result < 0 then
        Result := SetValue(Result);
    end
    else
      Result := dValue;
  end;

  function SetValue1(dValue : Double) : Double;
  begin
    if dValue > 360 then
    begin
      Result := dValue - 360;

      if Result > 360 then
        Result := SetValue1(Result)
    end
    else
     Result := dValue;
  end;
begin
  if Value < 0 then
  begin
    FVAngle := SetValue(Value);
  end
  else if Value > 360 then
  begin
    FVAngle := SetValue1(Value);
  end
  else
  begin
    FVAngle := Value;
  end;
end;

procedure TVectorLineInfo.SetVTypeStr(const Value: string);
begin
  FVType := SetVTType(Value);
end;

end.




