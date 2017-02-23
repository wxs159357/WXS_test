unit U_WE_DIAGRAM2;

interface

uses
  SysUtils, Windows, Classes, Controls, StdCtrls,
  U_WIRING_ERROR, GDIPOBJ, GDIPAPI, IniFiles, Forms, Dialogs, U_DIAGRAM_TYPE,
  system.UITypes, system.Types, Vcl.Graphics;

type
  TDgMeterPhaseType = (dmptThree, dmptFour);
  TDgMeterEnergyType = (dmetActive, dmetReactive);
  TDgMeterPlugSign = (dmpsIa_in, dmpsUa, dmpsIa_out, dmpsIb_in, dmpsUb, dmpsIb_out,
    dmpsIc_in, dmpsUc, dmpsIc_out, dmpsUn1, dmpsUn2);

type
  /// <summary> 电表 </summary>
  TDgMeter = class
  private
    FCanvas: TCanvas;
    FPos: TPoint;
    FPhaseType: TDgMeterPhaseType;
    FEnergyType: TdgMeterEnergyType;
    FVisible: Boolean;

    procedure DrawOutline;
    procedure DrawPlugs;
    procedure DrawDetail;

    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetPlugCount: Integer;
  public
    constructor Create(ACanvas: TCanvas);
    procedure Draw;
    function GetPlugPos(Index: Integer): TPoint; overload;
    function GetPlugPos(APlugSign: TDgMeterPlugSign): TPoint; overload;
  public
    property Canvas: TCanvas read FCanvas write FCanvas;
    property Pos: TPoint  read FPos write FPos;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    /// <summary>
    /// 相线类型
    /// </summary>
    property PhaseType: TDgMeterPhaseType read FPhaseType write FPhaseType;
    /// <summary>
    /// 有功无功
    /// </summary>
    property EnergyType: TdgMeterEnergyType read FEnergyType write FEnergyType;
    /// <summary>
    /// 端子数量
    /// </summary>
    property PlugCount: Integer read GetPlugCount;
    /// <summary>
    /// 是否显示
    /// </summary>
    property Visible: Boolean read FVisible write FVisible default True;
  end;

type
  /// <summary> 电流互感器 </summary>
  TDgCT = class
  private
    FCanvas: TCanvas;
    FWindingPos: TPoint;
    FWindingWidth: Integer;
    FLeadLength: Integer;
    FVisible: Boolean;
    function GetLeftPlugPos: TPoint;
    function GetRightPlugPos: TPoint;
  public
    constructor Create(ACanvas: TCanvas);
    procedure Draw;
  public
    property Canvas: TCanvas read FCanvas write FCanvas;
    property WindingPos: TPoint read FWindingPos write FWindingPos;
    property WindingWidth: Integer read FWindingWidth write FWindingWidth;
    property LeadLength: Integer read FLeadLength write FLeadLength;
    property LeftPlugPos: TPoint read GetLeftPlugPos;
    property RightPlugPos: TPoint read GetRightPlugPos;
    property Visible: Boolean read FVisible write FVisible default True;
  end;

type
  /// <summary> 电压互感器 </summary>
  TDgPT = class
  private
    FCanvas: TCanvas;
    FClientRect: TRect;
    FLeadLength: Integer;
    FVisible: Boolean;
    function GetLeftTPlugPos: TPoint;
    function GetLeftBPlugPos: TPoint;
    function GetRightTPlugPos: TPoint;
    function GetRightBPlugPos: TPoint;
  public
    constructor Create(ACanvas: TCanvas);
    procedure Draw;
  public
    property Canvas: TCanvas read FCanvas write FCanvas;
    property ClientRect: TRect read FClientRect write FClientRect;
    property LeadLength: Integer read FLeadLength write FLeadLength;
    property LeftTPlugPos: TPoint read GetLeftTPlugPos;
    property LeftBPlugPos: TPoint read GetLeftBPlugPos;
    property RightTPlugPos: TPoint read GetRightTPlugPos;
    property RightBPlugPos: TPoint read GetRightBPlugPos;
    property Visible: Boolean read FVisible write FVisible default True;
  end;

type
  /// <summary> 接地符号 </summary>
  TDgGround = class
  private
    FCanvas: TCanvas;
    FPos: TPoint;
    FLeadLength: Integer;
    FVisible: Boolean;
  public
    constructor Create(ACanvas: TCanvas);
    procedure Draw;
  public
    property Canvas: TCanvas read FCanvas write FCanvas;
    property Pos: TPoint read FPos write FPos;
    property LeadLength: Integer read FLeadLength;
    property Visible: Boolean read FVisible write FVisible default True;
  end;

type
  /// <summary> 电压互感器组 </summary>
  TDgPTGroup = class
  private
    FCanvas: TCanvas;
    FPos: TPoint;
    FPT1, FPT2, FPT3: TDgPT;
    FHasThreePT: Boolean;
    FVisible: Boolean;
    procedure SetPos(const Value: TPoint);
    procedure SetHasThreePT(const Value: Boolean);
  public
    constructor Create(ACanvas: TCanvas);
    destructor Destroy; override;
    procedure Draw;
  public
    property Canvas: TCanvas read FCanvas write FCanvas;
    property Pos: TPoint read FPos write SetPos;
    property PT1: TDgPT read FPT1;
    property PT2: TDgPT read FPT2;
    property PT3: TDgPT read FPT3;
    property HasThreePT: Boolean read FHasThreePT write SetHasThreePT default False;
    property Visible: Boolean read FVisible write FVisible default True;
  end;

type
  /// <summary> 电线 </summary>
  TDgWire = class
  private
    FCanvas: TCanvas;
    FLength: Integer;
    FStartPos: TPoint;
    FBreakXPos2: Integer;
    FBreakXPos1: Integer;
    FText: string;
    FVisible: Boolean;
  public
    constructor Create(ACanvas: TCanvas);
    procedure Draw;
  public
    property Canvas: TCanvas read FCanvas write FCanvas;
    property StartPos: TPoint read FStartPos write FStartPos;
    property Length: Integer read FLength write FLength;
    property BreakXPos1: Integer read FBreakXPos1 write FBreakXPos1;
    property BreakXPos2: Integer read FBreakXPos2 write FBreakXPos2;
    property Text: string read FText write FText;
    property Visible: Boolean read FVisible write FVisible default True;
  end;

  TPointArray = array of TPoint;
  TDgLink = record
    InPortNo: Integer;
    OutPortNo: Integer;
  end;
  TDgLinkArray = array of TDgLink;

type
  /// <summary> 跳线盒 </summary>
  TDgJunctionBox = class
  private
    FCanvas: TCanvas;
    FInPorts: TPointArray;
    FOutPorts: TPointArray;
    FLinks: TDgLinkArray;
    FVisible: Boolean;
    FBoxName: string;
    procedure SetLinkCount(const Value: Integer);
    function GetLinkCount: Integer;
    function GetPortCount: Integer;
    procedure SetPortCount(const Value: Integer);
  public
    constructor Create(ACanvas: TCanvas; APortCount: Integer);
    procedure SetOutPortOrder(AOrderArray: array of Integer);
    procedure Draw; overload;
    procedure Draw(HiPos: Integer); overload;
  public
    property Canvas: TCanvas read FCanvas write FCanvas;
    property InPorts: TPointArray read FInPorts;
    property OutPorts: TPointArray read FOutPorts;
    property PortCount: Integer read GetPortCount write SetPortCount;
    property Links: TDgLinkArray read FLinks;
    property LinkCount: Integer read GetLinkCount write SetLinkCount;
    property Visible: Boolean read FVisible write FVisible default True;
    property BoxName : string read FBoxName write FBoxName;
  end;

procedure DgDrawCircle(ACanvas: TCanvas; ACenter: TPoint; ADiameter: Integer;
  AFillColor: TColor = clDefault);

procedure DgDrawArc(ACanvas: TCanvas; ACenter, AStart, AEnd: TPoint; ARadius: Integer);

procedure DgDrawJunction(ACanvas: TCanvas; ACenter: TPoint; ASolid: Boolean; ADiameter: Integer = 3);

function DgOffsetPoint(APoint: TPoint; AX, AY: Integer): TPoint;
function DgOffsetRectDef0(ARect: TRect; AX, AY: Integer): TRect;
procedure DgSwapPoints(var APoint1, APoint2: TPoint);

procedure DgDrawWinding(ACanvas: TCanvas; ARect: TRect; AIsDown: Boolean);

procedure DgDrawConnection(ACanvas: TCanvas; APosStart, APosEnd: TPoint;
  AStraightLine : Boolean = False; AColor : TColor = clBlack);
procedure DgDrawConnection3X(ACanvas: TCanvas; APosStart, APosEnd: TPoint;
  AXPos: Integer; AColor: TColor = clBlack);
procedure DgDrawConnection3Y(ACanvas: TCanvas; APosStart, APosEnd: TPoint;
  AYPos: Integer; AColor: TColor = clBlack);

type
  /// <summary> 接线图 </summary>
  TWE_Diagram2 = class(TCustomControl)
  private
    FBitmap: TBitmap;
    FDiagramType: TDiagramType;
    FWiringError: TWIRING_ERROR;

    FMeter1, FMeter2: TDgMeter;       //电表1#、2#
    FCT1, FCT2, FCT3: TDgCT;          //电流互感器A相, B相, C相
    FPTGroup: TDgPTGroup;             //电压互感器组
    FCTGround, FPTGround: TDgGround;  //CT及PT的接地标识符号
    FWire1, FWire2, FWire3, FWire4: TDgWire;  //电线A相, B相, C相，N相
    FJunctionBoxCt: TDgJunctionBox; //电流进出线接线盒
    FJunctionBoxCtA, FJunctionBoxCtB, FJunctionBoxCtC: TDgJunctionBox; //CT接线盒 A相, B相, C相
    FJunctionBoxPtDown: TDgJunctionBox;      //PT下接线盒
    FJunctionBoxPtUp: TDgJunctionBox;        //PT上接线盒

    ptUbUp: TPoint;       //PT出线B相公共点
    ptUbDown: TPoint;     //PT进线B相公共点

    /// <summary> 清除画布 </summary>
    procedure ClearCanvas;

    /// <summary> 调整元件布局 </summary>
    procedure AdjustElementLayout;

    /// <summary> 画接线断开 </summary>
    procedure DrawBroken(APosStart, APosEnd: TPoint);

    /// <summary> 改变跳线顺序 </summary>
    procedure ChangeSequence(ALinks: TDgLinkArray; ASequence: TWE_SEQUENCE_TYPE);

    /// <summary> 画全部元件 </summary>
    procedure DrawAllElement;

    procedure DrawType3CTClear;          //三相三线CT简化
    procedure DrawType3M;                //三相三线多功能
    procedure DrawType3L4;               //三相三线四线制
    procedure DrawType4M_NoPT;           //三相四线多功能无PT
    procedure DrawType4M_PT;             //三相四线多功能有PT
    procedure DrawType4_PT_CT_CLear;     //三相四线经PT，CT简化
    procedure DrawType4_PT_L6;           //三相四线经PT，六线制
    procedure DrawType4_NoPT_L6;         //三相四线无PT六线制
    procedure DrawType4Direct;           //三相四线直通
                                         
    procedure SetDiagramType(const Value: TDiagramType);
    procedure SetWiringError(const Value: TWIRING_ERROR);

    /// <summary> 画接线图 </summary>
    procedure Draw;
  protected
    procedure Paint; override;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary> 画接线图到指定画布 </summary>
    procedure DrawTo(ACanvas: TCanvas; ARect: TRect); overload;

    /// <summary> 画接线图到指定画布 </summary>
    procedure DrawTo(ACanvas: TCanvas; APos: TPoint); overload;
  public
    /// <summary> 接线错误 </summary>
    property WiringError: TWIRING_ERROR read FWiringError write SetWiringError;
  published
    /// <summary> 接线图类型 </summary>
    property DiagramType: TDiagramType read FDiagramType write SetDiagramType;

    property Visible;
    property Color default clWhite;
    property Anchors;

  end;

implementation

uses
  Math;

const
  C_PLUG_DISTANCE = 20;    //电表插孔间距

procedure DgDrawCircle(ACanvas: TCanvas; ACenter: TPoint; ADiameter: Integer;
  AFillColor: TColor);
var
  OldColor: TColor;
begin
  OldColor := ACanvas.Brush.Color;
  if AFillColor <> clDefault then
    ACanvas.Brush.Color := AFillColor;
  ACanvas.Ellipse(ACenter.X - ADiameter div 2, ACenter.Y - ADiameter div 2,
    ACenter.X + (ADiameter + 1) div 2, ACenter.Y + (ADiameter + 1) div 2);
  if AFillColor <> clDefault then
    ACanvas.Brush.Color := OldColor;
end;

procedure DgDrawArc(ACanvas: TCanvas; ACenter, AStart, AEnd: TPoint; ARadius: Integer);
begin
  ACanvas.Arc(ACenter.X - ARadius, ACenter.Y - ARadius, ACenter.X + ARadius, ACenter.Y + ARadius,
    AStart.X, AStart.Y, AEnd.X, AEnd.Y);
end;

function DgOffsetPoint(APoint: TPoint; AX, AY: Integer): TPoint;
begin
  Result.X := APoint.X + AX;
  Result.Y := APoint.Y + AY;
end;

function DgOffsetRectDef0(ARect: TRect; AX, AY: Integer): TRect;
begin
  Result := ARect;
  if not OffsetRect(Result, AX, AY) then
    Result := Rect(0, 0, 0, 0);
end;

procedure DgSwapPoints(var APoint1, APoint2: TPoint);
var
  ptTemp: TPoint;
begin
  ptTemp := APoint1;
  APoint1 := APoint2;
  APoint2 := ptTemp;
end;

procedure DgDrawJunction(ACanvas: TCanvas; ACenter: TPoint; ASolid: Boolean; ADiameter: Integer);
var
  OldColor: TColor;
  OldStyle: TBrushstyle;
begin
  with ACanvas do
  begin
    OldColor := Brush.Color;
    OldStyle := Brush.Style;

    Brush.Style := bsSolid;
    if ASolid then
      Brush.Color := clBlack
    else
      Brush.Color := clWhite;

    if ADiameter >= 4 then
      DgDrawCircle(ACanvas, ACenter, ADiameter)
    else
      Rectangle(ACenter.X - ADiameter div 2, ACenter.Y - ADiameter div 2,
        ACenter.X + ADiameter div 2 + ADiameter mod 2,
        ACenter.Y + ADiameter div 2 + ADiameter mod 2);

    Brush.Style := OldStyle;
    Brush.Color := OldColor;
  end;
end;

procedure DgDrawConnection(ACanvas: TCanvas; APosStart, APosEnd: TPoint;
  AStraightLine: Boolean; AColor: TColor);
var
  OldColor: TColor;
begin
  with ACanvas do
  begin
    OldColor := Pen.Color;
    Pen.Color := AColor;

    if ( APosStart.X = APosEnd.X ) or ( APosStart.Y = APosEnd.Y ) or
        AStraightLine then
      Polyline( [ APosStart, APosEnd ] )
    else
    begin
      Polyline( [ APosStart, Point( APosEnd.X, APosStart.Y ) ] );
      Polyline( [ Point( APosEnd.X, APosStart.Y ), APosEnd ] );
    end;
    
    Pen.Color := OldColor;
  end;
end;

procedure DgDrawConnection3X(ACanvas: TCanvas; APosStart, APosEnd: TPoint;
  AXPos: Integer; AColor: TColor);
var
  OldColor: TColor;
begin
  with ACanvas do
  begin
    OldColor := Pen.Color;
    Pen.Color := AColor;

    Polyline([APosStart, Point(AXPos, APosStart.Y)]);
    DgDrawConnection(ACanvas, APosEnd, Point(AXPos, APosStart.Y));

    Pen.Color := OldColor;
  end;
end;

procedure DgDrawConnection3Y(ACanvas: TCanvas; APosStart, APosEnd: TPoint;
  AYPos: Integer; AColor: TColor);
var
  OldColor: TColor;
begin
  with ACanvas do
  begin
    OldColor := Pen.Color;
    Pen.Color := AColor;

    Polyline([APosStart, Point(APosStart.X, AYPos)]);
    DgDrawConnection(ACanvas, Point(APosStart.X, AYPos), APosEnd);

    Pen.Color := OldColor;
  end;
end;

procedure DgDrawWinding(ACanvas: TCanvas; ARect: TRect; AIsDown: Boolean);
var
  nY, nX: Integer;
begin
  nX := (ARect.Left + ARect.Right) div 2;
  if AIsDown then
  begin
    nY := ARect.Top + 1;
    Dec(ARect.Top, ARect.Bottom - ARect.Top);
    ACanvas.Arc(ARect.Left, ARect.Top, nX + 1, ARect.Bottom,
      ARect.Left, nY, ARect.Right, nY);
    ACanvas.Arc(nX, ARect.Top, ARect.Right + 1, ARect.Bottom,
      ARect.Left, nY, ARect.Right, nY);
  end
  else
  begin
    nY := ARect.Bottom;
    Inc(ARect.Bottom, ARect.Bottom - ARect.Top);
    ACanvas.Arc(ARect.Left, ARect.Top, nX + 1, ARect.Bottom,
      ARect.Right, nY, ARect.Left, nY);
    ACanvas.Arc(nX, ARect.Top, ARect.Right + 1, ARect.Bottom,
      ARect.Right, nY, ARect.Left, nY);
  end;

end;

{ TWdMeter }

constructor TDgMeter.Create(ACanvas: TCanvas);
begin
  FCanvas := ACanvas;
  FPos := Point(0, 0);
  FVisible := True;
end;

procedure TDgMeter.Draw;
begin
  if not FVisible then
    Exit;

  DrawOutline;
  DrawPlugs;
  DrawDetail;
end;

procedure TDgMeter.DrawDetail;
var
  ptCircle1, ptCircle2, ptCircle3: TPoint;

  procedure DrawCircleAndPoint(ACenter: TPoint);
  var
    OldColor: TColor;
  begin
    with FCanvas do
    begin
      OldColor := Brush.Color;
      DgDrawCircle(FCanvas, ACenter, 25);
      Brush.Color := clBlack;
      Rectangle(Rect(ACenter.X - 5, ACenter.Y + 14, ACenter.X - 2, ACenter.Y + 17));
      Rectangle(Rect(ACenter.X - 16, ACenter.Y - 5, ACenter.X - 13, ACenter.Y - 2));
      Brush.Color := OldColor;
    end;
  end;

  procedure DrawThreeActive;
  begin
    with FCanvas do
    begin
      DgDrawConnection3Y(FCanvas, DgOffsetPoint(GetPlugPos(0), 0, -14), DgOffsetPoint(GetPlugPos(2), 0, -14), ptCircle1.Y);
      DgDrawConnection3Y(FCanvas, DgOffsetPoint(GetPlugPos(4), 0, -14), DgOffsetPoint(GetPlugPos(6), 0, -14), ptCircle2.Y);
      DgDrawConnection3Y(FCanvas, DgOffsetPoint(GetPlugPos(1), 0, -14), DgOffsetPoint(GetPlugPos(5), 0, -14), ptCircle1.Y - 40);
      Polyline([DgOffsetPoint(GetPlugPos(3), 0, -14), Point(GetPlugPos(3).X, ptCircle1.Y - 40)]);
      DgDrawJunction(FCanvas, Point(GetPlugPos(3).X, ptCircle1.Y - 40), True);
    end;
  end;

  procedure DrawThreeReactive;
  var
    rRect1, rRect2: TRect;
    ptRightBottom: TPoint;
  begin
    rRect1.TopLeft := DgOffsetPoint(ptCircle1, -3, -30);
    rRect2.TopLeft := DgOffsetPoint(ptCircle2, -3, -30);
    rRect1.BottomRight := DgOffsetPoint(rRect1.TopLeft, 6, 10);
    rRect2.BottomRight := DgOffsetPoint(rRect2.TopLeft, 6, 10);
    ptRightBottom := DgOffsetPoint(GetPlugPos(6), 8, -26);
    
    with FCanvas do
    begin
      Rectangle(rRect1);
      Rectangle(rRect2);
      DgDrawConnection3Y(FCanvas, DgOffsetPoint(GetPlugPos(0), 0, -14), DgOffsetPoint(GetPlugPos(2), 0, -14), ptCircle1.Y);
      DgDrawConnection3Y(FCanvas, DgOffsetPoint(GetPlugPos(4), 0, -14), DgOffsetPoint(GetPlugPos(6), 0, -14), ptCircle2.Y);
      DgDrawConnection3Y(FCanvas, DgOffsetPoint(GetPlugPos(1), 0, -14), Point(ptCircle2.X, rRect2.Bottom - 1), GetPlugPos(0).Y - 36);
      DgDrawConnection3Y(FCanvas, DgOffsetPoint(GetPlugPos(3), 0, -14), Point(ptCircle1.X, rRect1.Bottom - 1), GetPlugPos(0).Y - 46);
      DgDrawConnection(FCanvas, ptRightBottom, DgOffsetPoint(GetPlugPos(5), 0, -14));
      DgDrawConnection3Y(FCanvas, ptRightBottom, Point(ptCircle1.X, rRect1.Top), ptCircle1.Y - 40);
      Polyline([Point(ptCircle2.X, rRect2.Top), Point(ptCircle2.X, ptCircle2.Y - 40)]);
    end;
  end;

  procedure DrawFourActive;
  begin
    with FCanvas do
    begin
      DgDrawConnection3Y(FCanvas, DgOffsetPoint(GetPlugPos(0), 0, -14), DgOffsetPoint(GetPlugPos(2), 0, -14), ptCircle1.Y);
      DgDrawConnection3Y(FCanvas, DgOffsetPoint(GetPlugPos(3), 0, -14), DgOffsetPoint(GetPlugPos(5), 0, -14), ptCircle2.Y);
      DgDrawConnection3Y(FCanvas, DgOffsetPoint(GetPlugPos(6), 0, -14), DgOffsetPoint(GetPlugPos(8), 0, -14), ptCircle3.Y);
      DgDrawConnection3Y(FCanvas, DgOffsetPoint(GetPlugPos(9), 0, -14), DgOffsetPoint(GetPlugPos(10), 0, -14), GetPlugPos(10).Y - 50);
      DgDrawConnection3Y(FCanvas, DgOffsetPoint(GetPlugPos(1), 0, -14), DgOffsetPoint(GetPlugPos(10), -10, -50), ptCircle1.Y - 40);
      Polyline([DgOffsetPoint(GetPlugPos(4), 0, -14), Point(GetPlugPos(4).X, ptCircle1.Y - 40)]);
      Polyline([DgOffsetPoint(GetPlugPos(7), 0, -14), Point(GetPlugPos(7).X, ptCircle1.Y - 40)]);
    end;
  end;

  procedure DrawFourReactive;
  var
    ptLeftBottom, ptRightBottom, ptRightBottom2: TPoint;
  begin
    ptLeftBottom := DgOffsetPoint(GetPlugPos(0), -8, -26);
    ptRightBottom := DgOffsetPoint(GetPlugPos(8), 8, -26);
    ptRightBottom2 := DgOffsetPoint(ptRightBottom, -4, -10);
    with FCanvas do
    begin
      DgDrawConnection3Y(FCanvas, DgOffsetPoint(GetPlugPos(0), 0, -14), DgOffsetPoint(GetPlugPos(2), 0, -14), ptCircle1.Y);
      DgDrawConnection3Y(FCanvas, DgOffsetPoint(GetPlugPos(3), 0, -14), DgOffsetPoint(GetPlugPos(5), 0, -14), ptCircle2.Y);
      DgDrawConnection3Y(FCanvas, DgOffsetPoint(GetPlugPos(6), 0, -14), DgOffsetPoint(GetPlugPos(8), 0, -14), ptCircle3.Y);
      DgDrawConnection(FCanvas, ptLeftBottom, DgOffsetPoint(GetPlugPos(1), 0, -14));
      DgDrawConnection3Y(FCanvas, ptLeftBottom, ptCircle2, ptCircle2.Y - 40);
      DgDrawConnection3Y(FCanvas, ptCircle2, DgOffsetPoint(ptCircle2, 30, -30), ptCircle2.Y + 20);
      Polyline([DgOffsetPoint(GetPlugPos(4), 0, -14), Point(GetPlugPos(4).X, ptRightBottom2.Y)]);
      DgDrawConnection(FCanvas, ptRightBottom, DgOffsetPoint(GetPlugPos(7), 0, -14));
      DgDrawConnection3Y(FCanvas, ptCircle1, ptRightBottom, ptCircle1.Y - 30);
      DgDrawConnection(FCanvas, ptRightBottom2, ptCircle1);
      DgDrawConnection3Y(FCanvas, ptCircle3, ptRightBottom2, ptCircle3.Y - 20);
      DgDrawConnection(FCanvas, DgOffsetPoint(ptLeftBottom, 0, -20), ptCircle3);
    end;
  end;

begin
  ptCircle1 := DgOffsetPoint(GetPlugPos(1), 0, -90);
  if FPhaseType = dmptFour then
  begin
    ptCircle2 := DgOffsetPoint(GetPlugPos(4), 0, -90);
    ptCircle3 := DgOffsetPoint(GetPlugPos(7), 0, -90);
  end
  else if FPhaseType = dmptThree then
    ptCircle2 := DgOffsetPoint(GetPlugPos(5), 0, -90);

  DrawCircleAndPoint(ptCircle1);
  if FPhaseType = dmptFour then
  begin
    DrawCircleAndPoint(ptCircle2);
    DrawCircleAndPoint(ptCircle3);
    if FEnergyType = dmetActive then
      DrawFourActive
    else
      DrawFourReactive;
  end
  else if FPhaseType = dmptThree then
  begin
    DrawCircleAndPoint(ptCircle2);
    if FEnergyType = dmetActive then
      DrawThreeActive
    else
      DrawThreeReactive;
  end;
end;

procedure TDgMeter.DrawOutline;
const
  C_R = 10;      //电表外框倒角半径
  C_H = 20;      //电表外框下端高度
var
  ptLeftTop, ptRightTop, ptLeftMiddle, ptRightMiddle, ptLeftBottom, ptRightBottom: TPoint;
begin
  ptLeftTop := FPos;
  ptLeftMiddle := Point(FPos.X, FPos.Y + Height - C_H);
  ptLeftBottom := Point(FPos.X + C_R, FPos.Y + Height);
  ptRightTop := Point(FPos.X + Width, FPos.Y);
  ptRightMiddle := Point(FPos.X + Width, FPos.Y + Height - C_H);
  ptRightBottom := Point(FPos.X + Width - C_R, FPos.Y + Height);

  DgDrawArc(FCanvas, DgOffsetPoint(ptLeftTop, C_R + 1, C_R + 1),
    DgOffsetPoint(ptLeftTop, C_R, 0), DgOffsetPoint(ptLeftTop, 0, C_R), C_R);
  FCanvas.Polyline([DgOffsetPoint(ptLeftTop, C_R, 0), Point(ptRightTop.X - C_R + 1, ptRightTop.Y )]);
  DgDrawArc(FCanvas, DgOffsetPoint(ptRightTop, -C_R + 1, C_R + 1),
    DgOffsetPoint(ptRightTop, 0, C_R), DgOffsetPoint(ptRightTop, -C_R, 0), C_R);
  FCanvas.Polyline([DgOffsetPoint(ptRightTop, 0, C_R), Point(ptRightMiddle.X, ptRightMiddle.Y - C_R)]);
  DgDrawArc(FCanvas, DgOffsetPoint(ptRightMiddle, -C_R + 1, -C_R + 1),
    DgOffsetPoint(ptRightMiddle, -C_R, 0), DgOffsetPoint(ptRightMiddle, 0, -C_R), C_R);
  FCanvas.Polyline([DgOffsetPoint(ptRightMiddle, -C_R, 0), ptRightBottom]);
  FCanvas.Polyline([ptRightBottom, ptLeftBottom]);
  FCanvas.Polyline([ptLeftBottom, DgOffsetPoint(ptLeftMiddle, C_R, 0)]);
  DgDrawArc(FCanvas, DgOffsetPoint(ptLeftMiddle, C_R + 1, -C_R + 1),
    DgOffsetPoint(ptLeftMiddle, 0, -C_R), DgOffsetPoint(ptLeftMiddle, C_R, 0), C_R);
  FCanvas.Polyline([DgOffsetPoint(ptLeftMiddle, 0, -C_R), DgOffsetPoint(ptLeftTop, 0, C_R - 1)]);
end;

procedure TDgMeter.DrawPlugs;
var
  I: Integer;
  APoint, APoint1 : TPoint;
begin
  for I := 0 to PlugCount - 1 do
  begin
    APoint := DgOffsetPoint(GetPlugPos(I), -3, -14);
    APoint1 := DgOffsetPoint(GetPlugPos(I), 3, 1);
    FCanvas.Rectangle(Rect(APoint.X, APoint.Y, APoint1.X, APoint1.Y));
  end;
end;

function TDgMeter.GetHeight: Integer;
begin
  Result := 150;
end;

function TDgMeter.GetPlugCount: Integer;
begin
  if FPhaseType = dmptFour then
  begin
    if FEnergyType = dmetActive then
      Result := 11
    else
      Result := 9
  end
  else if FPhaseType = dmptThree then
    Result := 7
  else
    Result := 0;
end;

function TDgMeter.GetPlugPos(APlugSign: TDgMeterPlugSign): TPoint;
begin
  if FPhaseType = dmptFour then
  begin
    Result := GetPlugPos(Ord(APlugSign));
  end
  else
  begin
    case APlugSign of
      dmpsIa_in, dmpsUa, dmpsIa_out: Result := GetPlugPos(Ord(APlugSign));
      dmpsUb: Result := GetPlugPos(3);
      dmpsIc_in, dmpsUc, dmpsIc_out: Result := GetPlugPos(Ord(APlugSign) - 2);
    else
      raise Exception.Create('No Plug');
    end;
  end;
end;

function TDgMeter.GetPlugPos(Index: Integer): TPoint;
begin
  if (Index >= 0) and (Index < PlugCount) then
  begin
    Result.X := FPos.X + 20 + Index * C_PLUG_DISTANCE;
    Result.Y := FPos.Y + Height - 6;
  end
  else
    raise Exception.Create('Error');
end;

function TDgMeter.GetWidth: Integer;
begin
  if PlugCount <= 0 then
    raise Exception.Create('Error');
  Result := (PlugCount - 1) * C_PLUG_DISTANCE + 40 - 1;
end;

{ TDgTransformer }

constructor TDgCT.Create(ACanvas: TCanvas);
begin
  FVisible := True;
  FCanvas := ACanvas;
  FWindingPos := Point(0, 0);
  FWindingWidth := 40;
  FLeadLength := 8;
end;

procedure TDgCT.Draw;
var
  APoint : TPoint;
begin
  if not FVisible then
    Exit;

  DgDrawJunction(FCanvas, DgOffsetPoint(LeftPlugPos, 0, 2), False, 4);
  DgDrawJunction(FCanvas, DgOffsetPoint(RightPlugPos, 0, 2), False, 4);
  DgDrawJunction(FCanvas, DgOffsetPoint(FWindingPos, -5, 4), True);
  DgDrawJunction(FCanvas, DgOffsetPoint(FWindingPos, -5, -4), True);

  DgDrawConnection(FCanvas, DgOffsetPoint(LeftPlugPos, 0, 4), FWindingPos, True);
  DgDrawConnection(FCanvas, DgOffsetPoint(RightPlugPos, 0, 4),
    DgOffsetPoint(FWindingPos, FwindingWidth, 0), True);
  APoint := DgOffsetPoint(FWindingPos, FWindingWidth, FWindingWidth div 4);
  DgDrawWinding(FCanvas, Rect(FWindingPos.X, FWindingPos.Y , APoint.X, APoint.Y), True);
end;

function TDgCT.GetLeftPlugPos: TPoint;
begin
  Result := DgOffsetPoint(FWindingPos, 0, -FLeadLength - 2);
end;

function TDgCT.GetRightPlugPos: TPoint;
begin
  Result := DgOffsetPoint(LeftPlugPos, FWindingWidth, 0);
end;

{ TDgPT }

constructor TDgPT.Create(ACanvas: TCanvas);
begin
  FVisible := True;
  FCanvas := ACanvas;
  FClientRect := Rect(0, 0, 20, 20);
  FLeadLength := 4;
end;

procedure TDgPT.Draw;
var
  nRadius: Integer;
begin
  if not FVisible then
    Exit;

  with FClientRect do
  begin
    nRadius := (Right - Left) div 2;
    DgDrawJunction(FCanvas, TopLeft, False, 4);
    DgDrawJunction(FCanvas, Point(Left, Bottom), False, 4);
    DgDrawJunction(FCanvas, Point(Right, Top), False, 4);
    DgDrawJunction(FCanvas, BottomRight, False, 4);
    DgDrawJunction(FCanvas, Point(Left + 4, Top + FLeadLength), True, 2);
    DgDrawJunction(FCanvas, Point(Left + 4, Bottom - FLeadLength), True, 2);

    DgDrawConnection(FCanvas, Point(Left, Top + 2), Point(Left, Top + FLeadLength + 2), True);
    DgDrawConnection(FCanvas, Point(Left, Bottom - 2), Point(Left, Bottom - FLeadLength - 2), True);
    DgDrawConnection(FCanvas, Point(Right, Top + 2), Point(Right, Top + FLeadLength + 2), True);
    DgDrawConnection(FCanvas, Point(Right, Bottom - 2), Point(Right, Bottom - FLeadLength - 2), True);
    FCanvas.Polyline([Point(Left + 1, (Top + Bottom) div 2), Point(Right - 1, (Top + Bottom) div 2)]);

    DgDrawWinding(FCanvas, Rect(Left, Top + FLeadLength, Right, Top + FLeadLength + nRadius), True);
    DgDrawWinding(FCanvas, Rect(Left, Bottom - FLeadLength - nRadius, Right, Bottom - FLeadLength), False);
  end;
end;

function TDgPT.GetLeftBPlugPos: TPoint;
begin
  Result := Point(FClientRect.Left, FClientRect.Bottom + 1);
end;

function TDgPT.GetLeftTPlugPos: TPoint;
begin
  Result := Point(FClientRect.Left, FClientRect.Top - 2);
end;

function TDgPT.GetRightBPlugPos: TPoint;
begin
  Result := Point(FClientRect.Right, FClientRect.Bottom + 1);
end;

function TDgPT.GetRightTPlugPos: TPoint;
begin
  Result := Point(FClientRect.Right, FClientRect.Top - 2);
end;

{ TDgPTGroup }

constructor TDgPTGroup.Create(ACanvas: TCanvas);
begin
  FCanvas := ACanvas;
  FVisible := True;
  FPT1 := TDgPT.Create(FCanvas);
  FPT2 := TDgPT.Create(FCanvas);
  FPT3 := TDgPT.Create(FCanvas);
  SetPos(Point(0, 0));
  SetHasThreePT(False);
end;

destructor TDgPTGroup.Destroy;
begin
  FPT1.Free;
  FPT2.Free;
  FPT3.Free;
  inherited;
end;

procedure TDgPTGroup.Draw;
begin
  if not FVisible then
    Exit;

  FPT1.Draw;
  FPT2.Draw;
  FPT3.Draw;
end;

procedure TDgPTGroup.SetHasThreePT(const Value: Boolean);
begin
  FHasThreePT := Value;
  FPT1.Visible := True;
  FPT2.Visible := True;
  FPT3.Visible := FHasThreePT;
  SetPos(FPos);
end;

procedure TDgPTGroup.SetPos(const Value: TPoint);
begin
  FPos := Value;
  FPT1.ClientRect := Rect(FPos.X, FPos.Y, FPos.X + 13, FPos.Y + 25);
  FPT2.ClientRect := DgOffsetRectDef0(FPT1.ClientRect, 18, 0);
  FPT3.ClientRect := DgOffsetRectDef0(FPT2.ClientRect, 18, 0);
end;

{ TDgGroud }

constructor TDgGround.Create(ACanvas: TCanvas);
begin
  FCanvas := ACanvas;
  FPos := Point(0, 0);
  FVisible := True;
  FLeadLength := 5;
end;

procedure TDgGround.Draw;
begin
  if Not FVisible then
    Exit;
    
  FCanvas.Polyline([FPos, DgOffsetPoint(FPos, 0, FLeadLength)]);
  FCanvas.Polyline([DgOffsetPoint(FPos, -8, FLeadLength), DgOffsetPoint(FPos, 8, FLeadLength)]);
  FCanvas.Polyline([DgOffsetPoint(FPos, -6, FLeadLength + 3), DgOffsetPoint(FPos, 6, FLeadLength + 3)]);
  FCanvas.Polyline([DgOffsetPoint(FPos, -4, FLeadLength + 6), DgOffsetPoint(FPos, 4, FLeadLength + 6)]);
end;

{ TDgWire }

constructor TDgWire.Create(ACanvas: TCanvas);
begin
  FVisible := True;
  FCanvas := ACanvas;
  FStartPos := Point(0, 0);
  FLength := 20;
  FText := '';
  FBreakXPos1 := -1;
  FBreakXPos2 := -1;
end;

procedure TDgWire.Draw;
var
  OldColor: TColor;
  ptEnd: TPoint;
begin
  if not FVisible then
    Exit;

  OldColor := FCanvas.Brush.Color;
  ptEnd := Point(FStartPos.X + FLength, FStartPos.Y);
  FCanvas.Brush.Color := clWhite;
  DgDrawCircle(FCanvas, FStartPos, 6);
  if (FBreakXPos1 > FStartPos.X) and (FBreakXPos2 < ptEnd.X) and (FBreakXPos2 >= FBreakXPos1) then
  begin
    FCanvas.Polyline([DgOffsetPoint(FStartPos, 3, 0), Point(FBreakXPos1, FStartPos.Y)]);
    FCanvas.Polyline([Point(FBreakXPos2, FStartPos.Y), ptEnd]);
  end
  else
  begin
    FCanvas.Polyline([DgOffsetPoint(FStartPos, 3, 0), ptEnd]);
  end;
  FCanvas.TextOut(FStartPos.X - 15, FStartPos.Y - 6, FText);
  FCanvas.Brush.Color := clBlack;
  FCanvas.Polygon([ptEnd, DgOffsetPoint(ptEnd, -8, -3), DgOffsetPoint(ptEnd, -8, 3)]);
  FCanvas.Brush.Color := OldColor;
end;

{ TDgJunctionBox }

constructor TDgJunctionBox.Create(ACanvas: TCanvas; APortCount: Integer);
begin
  FCanvas := ACanvas;
  SetPortCount(APortCount);
  SetLinkCount(APortCount);
  FVisible := True;
end;

procedure TDgJunctionBox.Draw;
var
  I: Integer;
begin
  if not FVisible then
    Exit;

  for I := 0 to High(FLinks) do
    FCanvas.Polyline([FInPorts[FLinks[I].InPortNo], FOutPorts[FLinks[I].OutPortNo]]);


//   for I := 0 to High(FLinks) do
//   begin
//     FCanvas.TextOut(InPorts[i].X + 10, InPorts[i].Y, 'A');
////     FCanvas.Polyline([FInPorts[FLinks[I].InPortNo], FOutPorts[FLinks[I].OutPortNo]]);
//   end;
//
//   for I := 0 to High(FLinks) do
//   begin
//     FCanvas.TextOut(OutPorts[i].X + 10, OutPorts[i].Y, 'B');
//   end;

//  FCanvas.TextOut(FInPorts[2].x, FInPorts[2].y, 'C');
//  FCanvas.Font.Size := 5;
//  for i := 0 to Length(FInPorts) - 1 do
//  begin
//    FCanvas.Ellipse(FInPorts[i].x-1, FInPorts[i].y-1,FInPorts[i].x+1, FInPorts[i].y+1);
//    FCanvas.TextOut(FInPorts[i].x+3, FInPorts[i].y, FBoxName + 'I' + IntToStr(i+1));
//  end;
//
//  for i := 0 to Length(FOutPorts) - 1 do
//  begin
//    FCanvas.Ellipse(FOutPorts[i].x-1, FOutPorts[i].y-1,FOutPorts[i].x+1, FOutPorts[i].y+1);
//    FCanvas.TextOut(FOutPorts[i].x+3, FOutPorts[i].y, FBoxName + 'O' + IntToStr(i+1));
//  end;

end;

function TDgJunctionBox.GetPortCount: Integer;
begin
  Result := Length(FInPorts);
end;

procedure TDgJunctionBox.SetPortCount(const Value: Integer);
begin
  SetLength(FInPorts, Value);
  SetLength(FOutPorts, Value);
end;

procedure TDgJunctionBox.Draw(HiPos: Integer);
var
  I, nPos: Integer;
begin
  if not FVisible then
    Exit;
  if HiPos < 0 then
    Exit;
  nPos := Min(High(FLinks), HiPos);
  for I := 0 to nPos do
    FCanvas.Polyline([FInPorts[FLinks[I].InPortNo], FOutPorts[FLinks[I].OutPortNo]]);


end;

function TDgJunctionBox.GetLinkCount: Integer;
begin
  Result := Length(FLinks);
end;

procedure TDgJunctionBox.SetLinkCount(const Value: Integer);
var
  I: Integer;
begin
  SetLength(FLinks, Value);
  for I := 0 to High(FLinks) do
  begin
    FLinks[I].InPortNo := I;
    FLinks[I].OutPortNo := I;
  end;
end;

procedure TDgJunctionBox.SetOutPortOrder(AOrderArray: array of Integer);
var
  I: Integer;
begin
  if Length(AOrderArray) <> LinkCount  then
    raise Exception.Create('Error');
  for I := 0 to LinkCount - 1 do
    FLinks[I].OutPortNo := AOrderArray[I];
end;


{ TCkmMeterWiringDiagram }

constructor TWE_Diagram2.Create(AOwner: TComponent);
var
  //Fabc : Boolean;
  Fabc : Integer;
begin
  inherited;
  with TIniFile.Create( ChangeFileExt( Application.ExeName, '.ini' ) ) do
  begin
    Fabc := ReadInteger( 'Like', 'abc', 0 );
    Free;
  end;
  Color := clWhite;
  FBitmap := TBitmap.Create;
  FBitmap.SetSize(630, 700);
  Width := FBitmap.Width;
  Height := FBitmap.Height;

  FWiringError := TWIRING_ERROR.Create;

  FMeter1 := TDgMeter.Create(FBitmap.Canvas);
  FMeter2 := TDgMeter.Create(FBitmap.Canvas);

  FWire1 := TDgWire.Create(FBitmap.Canvas);
  FWire2 := TDgWire.Create(FBitmap.Canvas);
  FWire3 := TDgWire.Create(FBitmap.Canvas);
  FWire4 := TDgWire.Create(FBitmap.Canvas);

  {if not Fabc then
  begin
    FWire1.Text := 'U';
    FWire2.Text := 'V';
    FWire3.Text := 'W';
    FWire4.Text := 'N';
  end
  else
  begin
    FWire1.Text := 'A';
    FWire2.Text := 'B';
    FWire3.Text := 'C';
    FWire4.Text := 'N';
  end; }

  case Fabc of
    0:
    begin
      FWire1.Text := 'A';
      FWire2.Text := 'B';
      FWire3.Text := 'C';
      FWire4.Text := 'N';
    end;
    1:
    begin
      FWire1.Text := 'U';
      FWire2.Text := 'V';
      FWire3.Text := 'W';
      FWire4.Text := 'N';
    end;
    2:
    begin
      FWire1.Text := '1';
      FWire2.Text := '2';
      FWire3.Text := '3';
      FWire4.Text := 'N';
    end;  
  end;

  FCT1 := TDgCT.Create(FBitmap.Canvas);
  FCT2 := TDgCT.Create(FBitmap.Canvas);
  FCT3 := TDgCT.Create(FBitmap.Canvas);
  FPTGroup := TDgPTGroup.Create(FBitmap.Canvas);
  FCTGround := TDgGround.Create(FBitmap.Canvas);
  FPTGround := TDgGround.Create(FBitmap.Canvas);

  FJunctionBoxCt := TDgJunctionBox.Create(FBitmap.Canvas, 4);
  FJunctionBoxCt.BoxName := 'Ct';
  FJunctionBoxCtA := TDgJunctionBox.Create(FBitmap.Canvas, 2);
  FJunctionBoxCtA.BoxName := 'CtA';
  FJunctionBoxCtB := TDgJunctionBox.Create(FBitmap.Canvas, 2);
  FJunctionBoxCtB.BoxName := 'CtB';
  FJunctionBoxCtC := TDgJunctionBox.Create(FBitmap.Canvas, 2);
  FJunctionBoxCtC.BoxName := 'CtC';

  FJunctionBoxPtDown := TDgJunctionBox.Create(FBitmap.Canvas, 6); //原为4胡红明，2013.5.13
  FJunctionBoxPtDown.BoxName := 'Down';

  FJunctionBoxPtUp := TDgJunctionBox.Create(FBitmap.Canvas, 3);
  FJunctionBoxPtUp.BoxName := 'Up';
end;

procedure TWE_Diagram2.CreateWnd;
begin
  inherited;
  SetDiagramType(dt3CTClear);
  AdjustElementLayout;
end;

destructor TWE_Diagram2.Destroy;
begin
  FWiringError.Free;
  FMeter1.Free;
  FMeter2.Free;
  FCT1.Free;
  FCT2.Free;
  FCT3.Free;
  FPTGroup.Free;
  FCTGround.Free;
  FPTGround.Free;
  FWire1.Free;
  FWire2.Free;
  FWire3.Free;
  FWire4.Free;
  FJunctionBoxCt.Free;
  FJunctionBoxCtA.Free;
  FJunctionBoxCtB.Free;
  FJunctionBoxCtC.Free;
  FJunctionBoxPtDown.Free;
  FJunctionBoxPtUp.Free;
  FBitmap.Free;
  inherited;
end;

procedure TWE_Diagram2.Paint;
var
  g: TGPGraphics;
  gpBmp: TGPBitmap;
begin
  Draw;

  g := TGPGraphics.Create(Canvas.Handle);
  gpBmp := TGPBitmap.Create(FBitmap.Handle, FBitmap.Palette);
  g.SetInterpolationMode(InterpolationModeHighQuality);
  g.DrawImage(gpBmp, MakeRect(ClientRect));
  gpBmp.Free;
  g.Free;

  inherited;
end;

procedure TWE_Diagram2.AdjustElementLayout;
var
  I: Integer;
begin
  FMeter1.Pos := Point(120, 6);
  FMeter2.Pos := DgOffsetPoint(FMeter1.Pos, FMeter1.Width + 20, 0);

  FWire1.StartPos := Point(20, 450);
  FWire2.StartPos := DgOffsetPoint(FWire1.StartPos, 0, 30);
  FWire3.StartPos := DgOffsetPoint(FWire2.StartPos, 0, 30);
  FWire4.StartPos := DgOffsetPoint(FWire3.StartPos, 0, 30);
  FWire1.Length := FBitmap.Width - 23;
  FWire2.Length := FBitmap.Width - 23;
  FWire3.Length := FBitmap.Width - 23;
  FWire4.Length := FBitmap.Width - 23;

  FCT1.WindingPos := Point(FMeter1.GetPlugPos(dmpsIa_in).X, FWire1.StartPos.Y);
  FCT2.Visible := FMeter1.PhaseType = dmptFour;
  if FCT2.Visible then
    FCT2.WindingPos := Point(FMeter1.GetPlugPos(dmpsIb_in).X, FWire2.StartPos.Y);
  FCT3.WindingPos := Point(FMeter1.GetPlugPos(dmpsIc_in).X, FWire3.StartPos.Y);
  FCTGround.Pos := Point(FCT3.RightPlugPos.X + 80, FWire1.StartPos.Y - 20);

  FPTGroup.Pos := Point(FWire1.StartPos.X + 30, FWire1.StartPos.Y - 60);
  if FPTGroup.HasThreePT then
    FPTGround.Pos := DgOffsetPoint(FPTGroup.PT3.RightTPlugPos, 18, 6)    //18
  else
    FPTGround.Pos := DgOffsetPoint(FPTGroup.PT2.RightTPlugPos, 18, 6);

  FJunctionBoxCtA.InPorts[0] := FCT1.LeftPlugPos;
  FJunctionBoxCtA.InPorts[1] := FCT1.RightPlugPos;
  FJunctionBoxCtB.InPorts[0] := FCT2.LeftPlugPos;
  FJunctionBoxCtB.InPorts[1] := FCT2.RightPlugPos;
  FJunctionBoxCtC.InPorts[0] := FCT3.LeftPlugPos;
  FJunctionBoxCtC.InPorts[1] := FCT3.RightPlugPos;
  FJunctionBoxCtA.OutPorts[0] := DgOffsetPoint(FJunctionBoxCtA.InPorts[0], 0, -15);
  FJunctionBoxCtA.OutPorts[1] := DgOffsetPoint(FJunctionBoxCtA.InPorts[1], 0, -15);
  FJunctionBoxCtB.OutPorts[0] := DgOffsetPoint(FJunctionBoxCtB.InPorts[0], 0, -15);
  FJunctionBoxCtB.OutPorts[1] := DgOffsetPoint(FJunctionBoxCtB.InPorts[1], 0, -15);
  FJunctionBoxCtC.OutPorts[0] := DgOffsetPoint(FJunctionBoxCtC.InPorts[0], 0, -15);
  FJunctionBoxCtC.OutPorts[1] := DgOffsetPoint(FJunctionBoxCtC.InPorts[1], 0, -15);
  FJunctionBoxCtB.Visible := FCT2.Visible;

  FWire1.BreakXPos1 := -1;
  FWire1.BreakXPos2 := -1;
  FWire2.BreakXPos1 := -1;
  FWire2.BreakXPos2 := -1;
  FWire3.BreakXPos1 := -1;
  FWire3.BreakXPos2 := -1;
  FCT1.Visible := True;
  FCT3.Visible := True;
  FCTGround.Visible := True;
  case FDiagramType of
    dt3CTClear:
    begin


      FJunctionBoxCt.PortCount := 3;
      FJunctionBoxCt.LinkCount := 3;
      FJunctionBoxCt.InPorts[0] := Point(FCT1.LeftPlugPos.X, FWire1.StartPos.Y - 50);
      FJunctionBoxCt.InPorts[1] := Point(FCT3.LeftPlugPos.X, FWire1.StartPos.Y - 50);
      FJunctionBoxCt.InPorts[2] := Point(FCT3.RightPlugPos.X, FWire1.StartPos.Y - 50);
    end;

    dt3M, dt3L4:
    begin
      FJunctionBoxCt.PortCount := 4;
      FJunctionBoxCt.LinkCount := 4;
      FJunctionBoxCt.InPorts[0] := Point(FCT1.LeftPlugPos.X, FWire1.StartPos.Y - 50);
      FJunctionBoxCt.InPorts[1] := Point(FCT1.RightPlugPos.X, FWire1.StartPos.Y - 50);
      FJunctionBoxCt.InPorts[2] := Point(FCT3.LeftPlugPos.X, FWire1.StartPos.Y - 50);
      FJunctionBoxCt.InPorts[3] := Point(FCT3.RightPlugPos.X, FWire1.StartPos.Y - 50);
    end;

    dt4M_NoPT:
    begin
      FJunctionBoxCt.PortCount := 6;
      FJunctionBoxCt.LinkCount := 6;
      FJunctionBoxCt.InPorts[0] := Point(FCT1.LeftPlugPos.X, FWire1.StartPos.Y - 50);
      FJunctionBoxCt.InPorts[1] := Point(FCT2.LeftPlugPos.X, FWire1.StartPos.Y - 50);
      FJunctionBoxCt.InPorts[2] := Point(FCT3.LeftPlugPos.X, FWire1.StartPos.Y - 50);
      FJunctionBoxCt.InPorts[3] := Point(FCT1.RightPlugPos.X, FWire1.StartPos.Y - 50);
      FJunctionBoxCt.InPorts[4] := Point(FCT2.RightPlugPos.X, FWire1.StartPos.Y - 50);
      FJunctionBoxCt.InPorts[5] := Point(FCT3.RightPlugPos.X, FWire1.StartPos.Y - 50);
    end;

    dt4_PT_CT_CLear:
    begin
      FJunctionBoxCt.PortCount := 3;
      FJunctionBoxCt.LinkCount := 3;
      FJunctionBoxCt.InPorts[0] := Point(FCT1.LeftPlugPos.X, FWire1.StartPos.Y - 50);
      FJunctionBoxCt.InPorts[1] := Point(FCT2.LeftPlugPos.X, FWire1.StartPos.Y - 50);
      FJunctionBoxCt.InPorts[2] := Point(FCT3.LeftPlugPos.X, FWire1.StartPos.Y - 50);
    end;



    dt4M_PT, dt4_PT_L6, dt4_NoPT_L6:
    begin
//      FJunctionBoxCt.PortCount := 3;
//      FJunctionBoxCt.LinkCount := 3;
//      FJunctionBoxCt.InPorts[0] := Point(FCT1.LeftPlugPos.X, FWire1.StartPos.Y - 50);
//      FJunctionBoxCt.InPorts[1] := Point(FCT2.LeftPlugPos.X, FWire1.StartPos.Y - 50);
//      FJunctionBoxCt.InPorts[2] := Point(FCT3.LeftPlugPos.X, FWire1.StartPos.Y - 50);
      FJunctionBoxCt.PortCount := 6;
      FJunctionBoxCt.LinkCount := 6;
      FJunctionBoxCt.InPorts[0] := Point(FCT1.LeftPlugPos.X, FWire1.StartPos.Y - 50);
      FJunctionBoxCt.InPorts[1] := Point(FCT2.LeftPlugPos.X, FWire1.StartPos.Y - 50);
      FJunctionBoxCt.InPorts[2] := Point(FCT3.LeftPlugPos.X, FWire1.StartPos.Y - 50);
      FJunctionBoxCt.InPorts[3] := Point(FCT1.RightPlugPos.X, FWire1.StartPos.Y - 50);
      FJunctionBoxCt.InPorts[4] := Point(FCT2.RightPlugPos.X, FWire1.StartPos.Y - 50);
      FJunctionBoxCt.InPorts[5] := Point(FCT3.RightPlugPos.X, FWire1.StartPos.Y - 50);
    end;

    dt4Direct:
    begin
      FJunctionBoxCt.PortCount := 0;
      FJunctionBoxCt.LinkCount := 0;
      FWire1.BreakXPos1 := FMeter1.GetPlugPos(dmpsIa_in).X;
      FWire1.BreakXPos2 := FMeter2.GetPlugPos(dmpsIa_out).X;
      FWire2.BreakXPos1 := FMeter1.GetPlugPos(dmpsIb_in).X;
      FWire2.BreakXPos2 := FMeter2.GetPlugPos(dmpsIb_out).X;
      FWire3.BreakXPos1 := FMeter1.GetPlugPos(dmpsIc_in).X;
      FWire3.BreakXPos2 := FMeter2.GetPlugPos(dmpsIc_out).X;
      FCT1.Visible := False;
      FCT2.Visible := False;
      FCT3.Visible := False;
      FCTGround.Visible := False;
    end;
  end;
  for I := 0 to FJunctionBoxCt.PortCount - 1 do
    FJunctionBoxCt.OutPorts[I] := DgOffsetPoint(FJunctionBoxCt.InPorts[I], 0, -20);

  case FDiagramType of
    dt3M, dt3L4, dt3CTClear:
    begin
      FJunctionBoxPtDown.PortCount := 4;
      FJunctionBoxPtDown.LinkCount := 4;
    end
  else
    begin
      FJunctionBoxPtDown.PortCount := 6;
      FJunctionBoxPtDown.LinkCount := 6;
    end;
  end;

//

  FJunctionBoxPtDown.InPorts[0] := FPTGroup.PT1.LeftTPlugPos;
  FJunctionBoxPtDown.InPorts[1] := FPTGroup.PT1.RightTPlugPos;
  FJunctionBoxPtDown.InPorts[2] := FPTGroup.PT2.LeftTPlugPos;
  FJunctionBoxPtDown.InPorts[3] := FPTGroup.PT2.RightTPlugPos;
  
  if FJunctionBoxPtDown.PortCount = 6 then
  begin
    FJunctionBoxPtDown.InPorts[4] := FPTGroup.PT3.LeftTPlugPos;
    FJunctionBoxPtDown.InPorts[5] := FPTGroup.PT3.RightTPlugPos;
  end;

  for I := 0 to FJunctionBoxPtDown.PortCount - 1 do
    FJunctionBoxPtDown.OutPorts[I] := DgOffsetPoint(FJunctionBoxPtDown.InPorts[I], 0, -10);

  ptUbUp := DgOffsetPoint(FJunctionBoxPtDown.OutPorts[1],
    (FJunctionBoxPtDown.OutPorts[2].X - FJunctionBoxPtDown.OutPorts[1].X) div 2, 0);
  ptUbDown := DgOffsetPoint(FPTGroup.PT1.RightBPlugPos,
    (FPTGroup.PT2.LeftBPlugPos.X - FPTGroup.PT1.RightBPlugPos.X) div 2, 5);

  if FDiagramType = dt4Direct then
  begin
    FJunctionBoxPtUp.InPorts[0] := DgOffsetPoint(FMeter1.GetPlugPos(dmpsIa_in), 0, 120);
    FJunctionBoxPtUp.InPorts[1] := DgOffsetPoint(FMeter1.GetPlugPos(dmpsIb_in), 0, 120);
    FJunctionBoxPtUp.InPorts[2] := DgOffsetPoint(FMeter1.GetPlugPos(dmpsIc_in), 0, 120);
    for I := 0 to FJunctionBoxPtUp.PortCount - 1 do
      FJunctionBoxPtUp.OutPorts[I] := DgOffsetPoint(FJunctionBoxPtUp.InPorts[I], 0, -30);
  end
  else if FMeter1.PhaseType = dmptThree then
  begin
    FJunctionBoxPtUp.InPorts[0] := DgOffsetPoint(FPTGroup.PT1.LeftTPlugPos, 0, -40);
    FJunctionBoxPtUp.InPorts[1] := DgOffsetPoint(ptUbUp, 0, -30);
    FJunctionBoxPtUp.InPorts[2] := DgOffsetPoint(FPTGroup.PT2.RightTPlugPos, 0, -40);
    for I := 0 to FJunctionBoxPtUp.PortCount - 1 do
      FJunctionBoxPtUp.OutPorts[I] := DgOffsetPoint(FJunctionBoxPtUp.InPorts[I], 0, -10);
  end
  else
  begin
    FJunctionBoxPtUp.InPorts[0] := DgOffsetPoint(FPTGroup.PT1.LeftTPlugPos, 0, -40);
    FJunctionBoxPtUp.InPorts[1] := DgOffsetPoint(FPTGroup.PT2.LeftTPlugPos, 0, -40);
    FJunctionBoxPtUp.InPorts[2] := DgOffsetPoint(FPTGroup.PT3.LeftTPlugPos, 0, -40);
    for I := 0 to FJunctionBoxPtUp.PortCount - 1 do
      FJunctionBoxPtUp.OutPorts[I] := DgOffsetPoint(FJunctionBoxPtUp.InPorts[I], 0, -10);
  end;
end;

procedure TWE_Diagram2.ChangeSequence(ALinks: TDgLinkArray;
  ASequence: TWE_SEQUENCE_TYPE);
  procedure SetInPortOrder(APortNo0, APortNo1, APortNo2: Integer);
  begin
    ALinks[0].InPortNo := APortNo0;
    ALinks[1].InPortNo := APortNo1;
    ALinks[2].InPortNo := APortNo2;
  end;
  procedure SetInPortOrder1(APortNo0, APortNo1, APortNo2: Integer);
  begin
    ALinks[0].InPortNo := APortNo0;
    ALinks[1].InPortNo := APortNo1;
    ALinks[2].InPortNo := APortNo2;
    ALinks[3].InPortNo := APortNo0+3;
    ALinks[4].InPortNo := APortNo1+3;
    ALinks[5].InPortNo := APortNo2+3;
//    ALinks[0].InPortNo := APortNo0*2;
//    ALinks[1].InPortNo := APortNo0*2 + 1;
//    ALinks[2].InPortNo := APortNo1*2;
//    ALinks[3].InPortNo := APortNo1*2 + 1;
//    ALinks[4].InPortNo := APortNo2*2;
//    ALinks[5].InPortNo := APortNo2*2 + 1;
  end;
var
  I: Integer;
begin
  if (Length(ALinks) <> 3) and (Length(ALinks) <> 6) then
    raise Exception.Create('Errors');

  if Length(ALinks) = 3 then
  begin
    for I := 0 to 2 do
      ALinks[I].OutPortNo := i;

    case ASequence of
      stABC: SetInPortOrder(0, 1, 2);
      stACB: SetInPortOrder(0, 2, 1);
      stBAC: SetInPortOrder(1, 0, 2);
      stBCA: SetInPortOrder(1, 2, 0);
      stCAB: SetInPortOrder(2, 0, 1);
      stCBA: SetInPortOrder(2, 1, 0);
    end;
  end
  else if Length(ALinks) = 6 then
  begin
    for I := 0 to 5 do
      ALinks[I].OutPortNo := i;

    case ASequence of
      stABC: SetInPortOrder1(0, 1, 2);
      stACB: SetInPortOrder1(0, 2, 1);
      stBAC: SetInPortOrder1(1, 0, 2);
      stBCA: SetInPortOrder1(1, 2, 0);
      stCAB: SetInPortOrder1(2, 0, 1);
      stCBA: SetInPortOrder1(2, 1, 0);
    end;
  end;



end;

procedure TWE_Diagram2.ClearCanvas;
var
  g: TGPGraphics;
begin
  g := TGPGraphics.Create(FBitmap.Canvas.Handle);
  g.Clear(ColorRefToARGB(Color));
  g.Free;
end;


procedure TWE_Diagram2.Draw;
begin
  ClearCanvas;
  AdjustElementLayout;
  DrawAllElement;

  case FDiagramType of
    dt3CTClear:      DrawType3CTClear;
    dt3M:            DrawType3M;
    dt3L4:           DrawType3L4;
    dt4M_NoPT:       DrawType4M_NoPT;
    dt4M_PT:         DrawType4M_PT;
    dt4_PT_CT_CLear: DrawType4_PT_CT_CLear;
    dt4_PT_L6:       DrawType4_PT_L6;
    dt4_NoPT_L6:     DrawType4_NoPT_L6;
    dt4Direct:       DrawType4Direct;
  end;
end;

procedure TWE_Diagram2.DrawAllElement;
begin
  if FDiagramType = dt4_PT_CT_CLear then
  begin
    FMeter1.Draw;
  end
  else
  begin
    FMeter1.Draw;
    FMeter2.Draw;
  end;

  FCT1.Draw;
  FCT2.Draw;
  FCT3.Draw;
  FPTGroup.Draw;
  FCTGround.Draw;
  FWire1.Draw;
  FWire2.Draw;
  FWire3.Draw;
  FWire4.Draw;

  if FCT1.Visible then
  begin
    DgDrawConnection(FBitmap.Canvas, FCTGround.Pos, DgOffsetPoint(FCT1.RightPlugPos, 6, -6));
    DgDrawConnection(FBitmap.Canvas, DgOffsetPoint(FCT1.RightPlugPos, 6, -6), FCT1.RightPlugPos, True);
  end;
  if FCT2.Visible then
  begin
    DgDrawConnection(FBitmap.Canvas, FCTGround.Pos, DgOffsetPoint(FCT2.RightPlugPos, 6, -6));
    DgDrawConnection(FBitmap.Canvas, DgOffsetPoint(FCT2.RightPlugPos, 6, -6), FCT2.RightPlugPos, True);
    DgDrawJunction(FBitmap.Canvas, Point(FCT2.RightPlugPos.X + 6, FCTGround.Pos.Y), True);
  end;
  if FCT3.Visible then
  begin
    DgDrawConnection(FBitmap.Canvas, FCTGround.Pos, DgOffsetPoint(FCT3.RightPlugPos, 6, -6));
    DgDrawConnection(FBitmap.Canvas, DgOffsetPoint(FCT3.RightPlugPos, 6, -6), FCT3.RightPlugPos, True);
    DgDrawJunction(FBitmap.Canvas, Point(FCT3.RightPlugPos.X + 6, FCTGround.Pos.Y), True);
  end;

  if FPTGroup.Visible and FPTGround.Visible then
  begin
    //地线
    FPTGround.Draw;

    if FMeter1.PhaseType = dmptThree then
    begin
      DgDrawConnection(FBitmap.Canvas, DgOffsetPoint(ptUbUp, 0, -5), FPTGround.Pos);
      DgDrawJunction(FBitmap.Canvas, DgOffsetPoint(ptUbUp, 0, -5), True);
    end
    else
    begin
      //胡红明2013.5.14更改以下3句，旧代码屏蔽
      DgDrawConnection(FBitmap.Canvas,Point(FPTGroup.PT1.RightTPlugPos.X,
             FPTGroup.PT1.RightTPlugPos.Y - 10),FPTGround.Pos);
      DgDrawConnection(FBitmap.Canvas,Point(FPTGroup.PT2.RightTPlugPos.X,
             FPTGroup.PT2.RightTPlugPos.Y - 10),FPTGround.Pos);
      DgDrawConnection(FBitmap.Canvas,Point(FPTGroup.PT3.RightTPlugPos.X,
             FPTGroup.PT3.RightTPlugPos.Y - 10),FPTGround.Pos);

      DgDrawJunction(FBitmap.Canvas, DgOffsetPoint(FPTGroup.PT3.RightTPlugPos, 0, -10), True);
    end;
  end;
end;

procedure TWE_Diagram2.DrawBroken(APosStart, APosEnd: TPoint);
var
  pO : TPoint;
  OldColor: TColor;
begin
  pO.X := (APosStart.X + APosEnd.X) div 2;
  pO.Y := (APosStart.Y + APosEnd.Y) div 2;

  with FBitmap.Canvas do
  begin
    OldColor := Pen.Color;
    // 清连接线
    Pen.Color := Pixels[0, 0];
    Polyline([ APosStart, APosEnd ] );

    // 画 X
    Pen.Color := clRed;
    Polyline([Point(po.X - 4, pO.Y - 4),Point(pO.X + 5, pO.Y + 5)]);
    Polyline([Point(po.X - 4, pO.Y + 4),Point(pO.X + 5, pO.Y - 5)]);
    Pen.Color := OldColor;
  end;
end;

procedure TWE_Diagram2.DrawTo(ACanvas: TCanvas; ARect: TRect);
var
  g: TGPGraphics;
  gpBmp: TGPBitmap;
begin
  Draw;
  g := TGPGraphics.Create(ACanvas.Handle);
  g.SetInterpolationMode(InterpolationModeHighQuality);
  gpBmp := TGPBitmap.Create(FBitmap.Handle, FBitmap.Palette);
  g.DrawImage(gpBmp, MakeRect(ARect));
  gpBmp.Free;
  g.Free;
end;

procedure TWE_Diagram2.DrawTo(ACanvas: TCanvas; APos: TPoint);
begin
  Draw;
  ACanvas.Draw(APos.X, APos.Y, FBitmap);
end;

procedure TWE_Diagram2.DrawType3CTClear;
  procedure SetInPortOrder(APortNo0, APortNo1, APortNo2: Integer);
  begin
    with FWiringError do
    begin
      FJunctionBoxCt.Links[0].InPortNo := APortNo0;
      FJunctionBoxCt.Links[1].InPortNo := APortNo1;
      FJunctionBoxCt.Links[2].InPortNo := APortNo2;
    end;
  end;

  /// <param name="nType">0: 元件1反接  1: 元件2反接</param>
  procedure Draw(nType : Integer = 0);
    procedure DrawReverse(APosStart, APosEnd, APosStart1, APosEnd1: TPoint);
    var
      OldColor: TColor;
    begin

      with FBitmap.Canvas do
      begin
        OldColor := Pen.Color;

        Pen.Color := clWhite;
        //填充
        Polyline([ APosStart, APosStart1 ] );
        Polyline([ APosEnd1, APosEnd ] );
        Pen.Color := OldColor;

        // 画 X
        Polyline([ APosStart, APosEnd ] );
        Polyline([ APosStart1, APosEnd1 ] );
      end;
    end;
  begin
    if nType = 0 then
    begin
      DrawReverse(DgOffsetPoint(FMeter1.GetPlugPos(0), 0, 95), DgOffsetPoint(FMeter2.GetPlugPos(2), 0, 125),
          DgOffsetPoint(FMeter1.GetPlugPos(0), 0, 125), DgOffsetPoint(FMeter2.GetPlugPos(2), 0, 95));
    end
    else
    begin
      DrawReverse(DgOffsetPoint(FMeter1.GetPlugPos(4), 0, 135), DgOffsetPoint(FMeter2.GetPlugPos(6), 0, 165),
          DgOffsetPoint(FMeter1.GetPlugPos(4), 0, 165), DgOffsetPoint(FMeter2.GetPlugPos(6), 0, 135));
    end;
  end;
var
  I: Integer;
  ptMeterOutPut: TPoint;
begin
  //外部电线到PT的连线
  FBitmap.Canvas.Polyline([FPTGroup.PT1.LeftBPlugPos, Point(FPTGroup.PT1.LeftBPlugPos.X, FWire1.StartPos.Y)]);
  FBitmap.Canvas.Polyline([FPTGroup.PT2.RightBPlugPos, Point(FPTGroup.PT2.RightBPlugPos.X, FWire3.StartPos.Y)]);
  FBitmap.Canvas.Polyline([ptUbDown, Point(ptUbDown.X, FWire2.StartPos.Y)]);
  DgDrawConnection(FBitmap.Canvas, ptUbDown, FPTGroup.PT1.RightBPlugPos);
  DgDrawConnection(FBitmap.Canvas, ptUbDown, FPTGroup.PT2.LeftBPlugPos);

  //两个PT接线盒之间的连线
  FBitmap.Canvas.Polyline([FJunctionBoxPtDown.OutPorts[1], DgOffsetPoint(FJunctionBoxPtDown.OutPorts[2], 1, 0)]);
  FBitmap.Canvas.Polyline([FJunctionBoxPtDown.OutPorts[0], FJunctionBoxPtUp.InPorts[0]]);
  FBitmap.Canvas.Polyline([ptUbUp, FJunctionBoxPtUp.InPorts[1]]);
  FBitmap.Canvas.Polyline([FJunctionBoxPtDown.OutPorts[3], FJunctionBoxPtUp.InPorts[2]]);

  //PT上接线盒到电表的连线
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[0], FMeter1.GetPlugPos(dmpsUa), FMeter1.GetPlugPos(0).Y + 60);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[0], FMeter2.GetPlugPos(dmpsUa), FMeter1.GetPlugPos(0).Y + 60);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[1], FMeter1.GetPlugPos(dmpsUb), FMeter1.GetPlugPos(0).Y + 70);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[1], FMeter2.GetPlugPos(dmpsUb), FMeter1.GetPlugPos(0).Y + 70);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[2], FMeter1.GetPlugPos(dmpsUc), FMeter1.GetPlugPos(0).Y + 80);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[2], FMeter2.GetPlugPos(dmpsUc), FMeter1.GetPlugPos(0).Y + 80);

  //PT一次断相
  if FWiringError.UaBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT1.LeftBPlugPos, 0, 15), DgOffsetPoint(FPTGroup.PT1.LeftBPlugPos, 0, 25));
  if FWiringError.UbBroken then
    DrawBroken(DgOffsetPoint(ptUbDown, 0, 10), DgOffsetPoint(ptUbDown, 0, 20));
  if FWiringError.UcBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT2.RightBPlugPos, 0, 15), DgOffsetPoint(FPTGroup.PT2.RightBPlugPos, 0, 25));

  //PT二次断相
  if FWiringError.UsaBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT1.LeftTPlugPos, 0, -20), DgOffsetPoint(FPTGroup.PT1.LeftTPlugPos, 0, -30));
  if FWiringError.UsbBroken then
    DrawBroken(DgOffsetPoint(ptUbUp, 0, -10), DgOffsetPoint(ptUbUp, 0, -20));
  if FWiringError.UscBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT2.RightTPlugPos, 0, -20), DgOffsetPoint(FPTGroup.PT2.RightTPlugPos, 0, -30));

  // 接地
  if FWiringError.GroundBroken then
    DrawBroken(DgOffsetPoint(FPTGround.Pos, 0, -10), DgOffsetPoint(FPTGround.Pos, 0, 0));

  //PT极性反接
  if not FWiringError.PT1Reverse then
  begin
    FJunctionBoxPtDown.Links[0].OutPortNo := 0;
    FJunctionBoxPtDown.Links[1].OutPortNo := 1;
  end
  else
  begin
    FJunctionBoxPtDown.Links[0].OutPortNo := 1;
    FJunctionBoxPtDown.Links[1].OutPortNo := 0;
  end;
  if not FWiringError.PT2Reverse then
  begin
    FJunctionBoxPtDown.Links[2].OutPortNo := 2;
    FJunctionBoxPtDown.Links[3].OutPortNo := 3;
  end
  else
  begin
    FJunctionBoxPtDown.Links[2].OutPortNo := 3;
    FJunctionBoxPtDown.Links[3].OutPortNo := 2;
  end;
  FJunctionBoxPtDown.Draw;

  //PT表尾接线
  ChangeSequence(FjunctionBoxPtUp.Links, FWiringError.USequence);
  FJunctionBoxPtUp.Draw;

  //CT极性反接
  if not FWiringError.CT1Reverse then
    FJunctionBoxCtA.SetOutPortOrder([0, 1])
  else
    FJunctionBoxCtA.SetOutPortOrder([1, 0]);
  FJunctionBoxCtA.Draw;

  if not FWiringError.CT2Reverse then
    FJunctionBoxCtC.SetOutPortOrder([0, 1])
  else
    FJunctionBoxCtC.SetOutPortOrder([1, 0]);
  FJunctionBoxCtC.Draw;

  //CT短路
  if FWiringError.CT1Short then
    DgDrawConnection(FBitmap.Canvas, FCT1.LeftPlugPos, FCT1.RightPlugPos);

  if FWiringError.CT2Short then
    DgDrawConnection(FBitmap.Canvas, FCT3.LeftPlugPos, FCT3.RightPlugPos);

  // 电流开路
  if FWiringError.IaBroken then
  begin
    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[0], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[0], 0, 25));
//    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[3], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[3], 0, 25));
  end;
  if FWiringError.IbBroken then
  begin
    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[1], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[1], 0, 25));
//    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[4], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[4], 0, 25));
  end;
  if FWiringError.IcBroken then
  begin
    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[2], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[2], 0, 25));
//    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[5], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[5], 0, 25));
  end;

  //CT到CT接线盒的连线
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtA.OutPorts[0], FJunctionBoxCt.InPorts[0]);
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtA.OutPorts[1], FJunctionBoxCt.InPorts[2]);
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtC.OutPorts[0], FJunctionBoxCt.InPorts[1]);
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtC.OutPorts[1], FJunctionBoxCt.InPorts[2]);

  //CT接线盒到电表的连线, 以及电表之间的电流端子接线
  ptMeterOutPut.X := (FMeter2.GetPlugPos(dmpsIa_out).X + FMeter2.GetPlugPos(dmpsIc_out).X) div 2;
  ptMeterOutPut.Y := FMeter2.GetPlugPos(dmpsIa_out).Y + 190;
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxCt.OutPorts[0], FMeter1.GetPlugPos(dmpsIa_in), FJunctionBoxCt.OutPorts[0].Y - 40);   //40
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxCt.OutPorts[1], FMeter1.GetPlugPos(dmpsIc_in), FJunctionBoxCt.OutPorts[0].Y - 30);    //30
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxCt.OutPorts[2], ptMeterOutPut, FJunctionBoxCt.OutPorts[0].Y - 25);               //25
  DgDrawConnection3Y(FBitmap.Canvas, FMeter2.GetPlugPos(dmpsIa_out), FMeter2.GetPlugPos(dmpsIc_out), ptMeterOutPut.Y);
  DgDrawConnection3Y(FBitmap.Canvas, FMeter1.GetPlugPos(dmpsIa_out), FMeter2.GetPlugPos(dmpsIa_in), FMeter1.GetPlugPos(0).Y + 20);   //20
  DgDrawConnection3Y(FBitmap.Canvas, FMeter1.GetPlugPos(dmpsIc_out), FMeter2.GetPlugPos(dmpsIc_in), FMeter1.GetPlugPos(0).Y + 30);    //30

  // 电流接地断开
  if FWiringError.IaGroundBroken then
    DrawBroken( DgOffsetPoint(FCT1.RightPlugPos, 6, -10), DgOffsetPoint(FCT1.RightPlugPos, 6, -10));
  if FWiringError.IbGroundBroken then
    DrawBroken( DgOffsetPoint(FCT2.RightPlugPos, 6, -10), DgOffsetPoint(FCT2.RightPlugPos, 6, -10));
  if FWiringError.IcGroundBroken then
    DrawBroken( DgOffsetPoint(FCT3.RightPlugPos, 6, -10), DgOffsetPoint(FCT3.RightPlugPos, 6, -10));

  // 接地
  if FWiringError.IGroundBroken then
    DrawBroken(DgOffsetPoint(FCTGround.Pos, -15, -5), DgOffsetPoint(FCTGround.Pos, -15, 5));

   //CT接线盒连线
   for I := 0 to 2 do
    FJunctionBoxCt.Links[I].OutPortNo := I;

  with FWiringError do
  begin
    //ac
    if (I1In in [plA, plN]) and (I1Out in [plA, plN]) and (I2In in [plC, plN]) and (I2Out in [plC, plN]) then
    begin
      SetInPortOrder(0, 1, 2);

      if (I1In = plN) and (I1Out = plA) then
      begin
        Draw;
      end;

      if (I2In = plN) and (I2Out = plC) then
      begin
        Draw(1);
      end;
    end;

    //ca
    if (I1In in [plC, plN]) and (I1Out in [plC, plN]) and (I2In in [plA, plN]) and (I2Out in [plA, plN]) then
    begin
      SetInPortOrder(1, 0, 2);

      if (I1In = plN) and (I1Out = plC) then
      begin
        Draw;
      end;

      if (I2In = plN) and (I2Out = plA) then
      begin
        Draw(1);
      end;
    end;

    //ab
    if (I1In in [plA, plC]) and (I1Out in [plA, plC]) and (I2In in [plC, plN]) and (I2Out in [plC, plN]) then
    begin
      SetInPortOrder(0, 2, 1);

      if (I1In = plC) and (I1Out = plA) then
      begin
        Draw;
      end;

      if (I2In = plC) and (I2Out = plN) then
      begin
        Draw(1);
      end;
    end;

    //cb
    if (I1In in [plC, plA]) and (I1Out in [plC, plA]) and (I2In in [plA, plN]) and (I2Out in [plA, plN]) then
    begin
      SetInPortOrder(1, 2, 0);

      if (I1In = plA) and (I1Out = plC) then
      begin
        Draw;
      end;

      if (I2In = plA) and (I2Out = plN) then
      begin
        Draw(1);
      end;
    end;

    //bc
    if (I1In in [plN, plA]) and (I1Out in [plN, plA]) and (I2In in [plC, plA]) and (I2Out in [plC, plA]) then
    begin
      SetInPortOrder(2, 1, 0);

      if (I1In = plA) and (I1Out = plN) then
      begin
        Draw;
      end;

      if (I2In = plA) and (I2Out = plC) then
      begin
        Draw(1);
      end;
    end;

    //ba
    if (I1In in [plN, plC]) and (I1Out in [plN, plC]) and (I2In in [plA, plC]) and (I2Out in [plA, plC]) then
    begin
      SetInPortOrder(2, 0, 1);


      if (I1In = plC) and (I1Out = plN) then
      begin
        Draw;
      end;

      if (I2In = plC) and (I2Out = plA) then
      begin
        Draw(1);
      end;
    end;
  end;

  FJunctionBoxCt.Draw;

end;

procedure TWE_Diagram2.DrawType3L4;
var
  I: Integer;
begin

  //外部电线到PT的连线
  FBitmap.Canvas.Polyline([FPTGroup.PT1.LeftBPlugPos, Point(FPTGroup.PT1.LeftBPlugPos.X, FWire1.StartPos.Y)]);
  FBitmap.Canvas.Polyline([FPTGroup.PT2.RightBPlugPos, Point(FPTGroup.PT2.RightBPlugPos.X, FWire3.StartPos.Y)]);
  FBitmap.Canvas.Polyline([ptUbDown, Point(ptUbDown.X, FWire2.StartPos.Y)]);
  DgDrawConnection(FBitmap.Canvas, ptUbDown, FPTGroup.PT1.RightBPlugPos);
  DgDrawConnection(FBitmap.Canvas, ptUbDown, FPTGroup.PT2.LeftBPlugPos);

  //两个PT接线盒之间的连线
  FBitmap.Canvas.Polyline([FJunctionBoxPtDown.OutPorts[1], DgOffsetPoint(FJunctionBoxPtDown.OutPorts[2], 1, 0)]);
  FBitmap.Canvas.Polyline([FJunctionBoxPtDown.OutPorts[0], FJunctionBoxPtUp.InPorts[0]]);
  FBitmap.Canvas.Polyline([ptUbUp, FJunctionBoxPtUp.InPorts[1]]);
  FBitmap.Canvas.Polyline([FJunctionBoxPtDown.OutPorts[3], FJunctionBoxPtUp.InPorts[2]]);

  //PT上接线盒到电表的连线
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[0], FMeter1.GetPlugPos(dmpsUa), FMeter1.GetPlugPos(0).Y + 60);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[0], FMeter2.GetPlugPos(dmpsUa), FMeter1.GetPlugPos(0).Y + 60);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[1], FMeter1.GetPlugPos(dmpsUb), FMeter1.GetPlugPos(0).Y + 70);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[1], FMeter2.GetPlugPos(dmpsUb), FMeter1.GetPlugPos(0).Y + 70);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[2], FMeter1.GetPlugPos(dmpsUc), FMeter1.GetPlugPos(0).Y + 80);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[2], FMeter2.GetPlugPos(dmpsUc), FMeter1.GetPlugPos(0).Y + 80);

  //PT一次断相
  if FWiringError.UaBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT1.LeftBPlugPos, 0, 15), DgOffsetPoint(FPTGroup.PT1.LeftBPlugPos, 0, 25));
  if FWiringError.UbBroken then
    DrawBroken(DgOffsetPoint(ptUbDown, 0, 10), DgOffsetPoint(ptUbDown, 0, 20));
  if FWiringError.UcBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT2.RightBPlugPos, 0, 15), DgOffsetPoint(FPTGroup.PT2.RightBPlugPos, 0, 25));

  //PT二次断相
  if FWiringError.UsaBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT1.LeftTPlugPos, 0, -20), DgOffsetPoint(FPTGroup.PT1.LeftTPlugPos, 0, -30));
  if FWiringError.UsbBroken then
    DrawBroken(DgOffsetPoint(ptUbUp, 0, -10), DgOffsetPoint(ptUbUp, 0, -20));
  if FWiringError.UscBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT2.RightTPlugPos, 0, -20), DgOffsetPoint(FPTGroup.PT2.RightTPlugPos, 0, -30));

  // 接地
  if FWiringError.GroundBroken then
    DrawBroken(DgOffsetPoint(FPTGround.Pos, 0, -10), DgOffsetPoint(FPTGround.Pos, 0, 0));

  //PT极性反接
  if not FWiringError.PT1Reverse then
  begin
    FJunctionBoxPtDown.Links[0].OutPortNo := 0;
    FJunctionBoxPtDown.Links[1].OutPortNo := 1;
  end
  else
  begin
    FJunctionBoxPtDown.Links[0].OutPortNo := 1;
    FJunctionBoxPtDown.Links[1].OutPortNo := 0;
  end;
  if not FWiringError.PT2Reverse then
  begin
    FJunctionBoxPtDown.Links[2].OutPortNo := 2;
    FJunctionBoxPtDown.Links[3].OutPortNo := 3;
  end
  else
  begin
    FJunctionBoxPtDown.Links[2].OutPortNo := 3;
    FJunctionBoxPtDown.Links[3].OutPortNo := 2;
  end;
  FJunctionBoxPtDown.Draw(3);

  //PT表尾接线
  ChangeSequence(FjunctionBoxPtUp.Links, FWiringError.USequence);
  FJunctionBoxPtUp.Draw;

  //CT接线盒连线
  for I := 0 to 3 do
    FJunctionBoxCt.Links[I].OutPortNo := I;
  case FWiringError.I1In of
    plA: FJunctionBoxCt.Links[0].InPortNo := 0;
    plC: FJunctionBoxCt.Links[0].InPortNo := 2;
    plN: FJunctionBoxCt.Links[0].InPortNo := 1;
  end;
  case FWiringError.I1Out of
    plA: FJunctionBoxCt.Links[1].InPortNo := 0;
    plC: FJunctionBoxCt.Links[1].InPortNo := 2;
    plN: FJunctionBoxCt.Links[1].InPortNo := 1;
  end;
  case FWiringError.I2In of
    plA: FJunctionBoxCt.Links[2].InPortNo := 0;
    plC: FJunctionBoxCt.Links[2].InPortNo := 2;
    plN: FJunctionBoxCt.Links[2].InPortNo := 3;
  end;
  case FWiringError.I2Out of
    plA: FJunctionBoxCt.Links[3].InPortNo := 0;
    plC: FJunctionBoxCt.Links[3].InPortNo := 2;
    plN: FJunctionBoxCt.Links[3].InPortNo := 3;
  end;
  FJunctionBoxCt.Draw;

  //CT极性反接
  if not FWiringError.CT1Reverse then
    FJunctionBoxCtA.SetOutPortOrder([0, 1])
  else
    FJunctionBoxCtA.SetOutPortOrder([1, 0]);
  FJunctionBoxCtA.Draw;

  if not FWiringError.CT2Reverse then
    FJunctionBoxCtC.SetOutPortOrder([0, 1])
  else
    FJunctionBoxCtC.SetOutPortOrder([1, 0]);
  FJunctionBoxCtC.Draw;

  //CT短路
  if FWiringError.CT1Short then
    DgDrawConnection(FBitmap.Canvas, FCT1.LeftPlugPos, FCT1.RightPlugPos);

  if FWiringError.CT2Short then
    DgDrawConnection(FBitmap.Canvas, FCT3.LeftPlugPos, FCT3.RightPlugPos);

  // 电流开路
  if FWiringError.IaBroken then
  begin
    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[0], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[0], 0, 25));
//    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[3], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[3], 0, 25));
  end;
  if FWiringError.IbBroken then
  begin
    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[1], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[1], 0, 25));
//    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[4], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[4], 0, 25));
  end;
  if FWiringError.IcBroken then
  begin
    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[2], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[2], 0, 25));
//    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[5], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[5], 0, 25));
  end;

  //CT到CT接线盒的连线
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtA.OutPorts[0], FJunctionBoxCt.InPorts[0]);
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtA.OutPorts[1], FJunctionBoxCt.InPorts[1]);
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtC.OutPorts[0], FJunctionBoxCt.InPorts[2]);
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtC.OutPorts[1], FJunctionBoxCt.InPorts[3]);

  //CT接线盒到电表的连线, 以及电表之间的电流端子接线
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxCt.OutPorts[0], FMeter1.GetPlugPos(dmpsIa_in), FJunctionBoxCt.OutPorts[0].Y - 40);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxCt.OutPorts[1], FMeter2.GetPlugPos(dmpsIa_out), FJunctionBoxCt.OutPorts[0].Y - 35);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxCt.OutPorts[2], FMeter1.GetPlugPos(dmpsIc_in), FJunctionBoxCt.OutPorts[0].Y - 30);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxCt.OutPorts[3], FMeter2.GetPlugPos(dmpsIc_out), FJunctionBoxCt.OutPorts[0].Y - 25);
  DgDrawConnection3Y(FBitmap.Canvas, FMeter1.GetPlugPos(dmpsIa_out), FMeter2.GetPlugPos(dmpsIa_in), FMeter1.GetPlugPos(0).Y + 20);
  DgDrawConnection3Y(FBitmap.Canvas, FMeter1.GetPlugPos(dmpsIc_out), FMeter2.GetPlugPos(dmpsIc_in), FMeter1.GetPlugPos(0).Y + 30);

  // 电流接地断开
  if FWiringError.IaGroundBroken then
    DrawBroken( DgOffsetPoint(FCT1.RightPlugPos, 6, -10), DgOffsetPoint(FCT1.RightPlugPos, 6, -10));

  if FWiringError.IcGroundBroken then
    DrawBroken( DgOffsetPoint(FCT3.RightPlugPos, 6, -10), DgOffsetPoint(FCT3.RightPlugPos, 6, -10));


  // 接地
  if FWiringError.IGroundBroken then
    DrawBroken(DgOffsetPoint(FCTGround.Pos, -15, -5), DgOffsetPoint(FCTGround.Pos, -15, 5));
end;

procedure TWE_Diagram2.DrawType3M;
var
  I: Integer;
begin
  //外部电线到PT的连线
  FBitmap.Canvas.Polyline([FPTGroup.PT1.LeftBPlugPos, Point(FPTGroup.PT1.LeftBPlugPos.X, FWire1.StartPos.Y)]);
  FBitmap.Canvas.Polyline([FPTGroup.PT2.RightBPlugPos, Point(FPTGroup.PT2.RightBPlugPos.X, FWire3.StartPos.Y)]);
  FBitmap.Canvas.Polyline([ptUbDown, Point(ptUbDown.X, FWire2.StartPos.Y)]);
  DgDrawConnection(FBitmap.Canvas, ptUbDown, FPTGroup.PT1.RightBPlugPos);
  DgDrawConnection(FBitmap.Canvas, ptUbDown, FPTGroup.PT2.LeftBPlugPos);

  //两个PT接线盒之间的连线
  FBitmap.Canvas.Polyline([FJunctionBoxPtDown.OutPorts[1], DgOffsetPoint(FJunctionBoxPtDown.OutPorts[2], 1, 0)]);
  FBitmap.Canvas.Polyline([FJunctionBoxPtDown.OutPorts[0], FJunctionBoxPtUp.InPorts[0]]);
  FBitmap.Canvas.Polyline([ptUbUp, FJunctionBoxPtUp.InPorts[1]]);
  FBitmap.Canvas.Polyline([FJunctionBoxPtDown.OutPorts[3], FJunctionBoxPtUp.InPorts[2]]);

  //PT上接线盒到电表的连线
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[0], FMeter1.GetPlugPos(dmpsUa), FMeter1.GetPlugPos(0).Y + 60);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[1], FMeter1.GetPlugPos(dmpsUb), FMeter1.GetPlugPos(0).Y + 70);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[2], FMeter1.GetPlugPos(dmpsUc), FMeter1.GetPlugPos(0).Y + 80);

  //PT一次断相
  if FWiringError.UaBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT1.LeftBPlugPos, 0, 15), DgOffsetPoint(FPTGroup.PT1.LeftBPlugPos, 0, 25));
  if FWiringError.UbBroken then
    DrawBroken(DgOffsetPoint(ptUbDown, 0, 10), DgOffsetPoint(ptUbDown, 0, 20));
  if FWiringError.UcBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT2.RightBPlugPos, 0, 15), DgOffsetPoint(FPTGroup.PT2.RightBPlugPos, 0, 25));

  //PT二次断相
  if FWiringError.UsaBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT1.LeftTPlugPos, 0, -20), DgOffsetPoint(FPTGroup.PT1.LeftTPlugPos, 0, -30));
  if FWiringError.UsbBroken then
    DrawBroken(DgOffsetPoint(ptUbUp, 0, -10), DgOffsetPoint(ptUbUp, 0, -20));
  if FWiringError.UscBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT2.RightTPlugPos, 0, -20), DgOffsetPoint(FPTGroup.PT2.RightTPlugPos, 0, -30));

  // 接地
  if FWiringError.GroundBroken then
    DrawBroken(DgOffsetPoint(FPTGround.Pos, 0, -10), DgOffsetPoint(FPTGround.Pos, 0, 0));


//  //PT极性反接
//  if not FWiringError.PT1Reverse then
//  begin
//    FJunctionBoxPtDown.Links[0].OutPortNo := 0;
//    FJunctionBoxPtDown.Links[1].OutPortNo := 1;
//  end
//  else
//  begin
//    FJunctionBoxPtDown.Links[0].OutPortNo := 1;
//    FJunctionBoxPtDown.Links[1].OutPortNo := 0;
//  end;
//  if not FWiringError.PT2Reverse then
//  begin
//    FJunctionBoxPtDown.Links[2].OutPortNo := 2;
//    FJunctionBoxPtDown.Links[3].OutPortNo := 3;
//  end
//  else
//  begin
//    FJunctionBoxPtDown.Links[2].OutPortNo := 3;
//    FJunctionBoxPtDown.Links[3].OutPortNo := 2;
//  end;
//  FJunctionBoxPtDown.Draw;
  //PT极性反接
  if not FWiringError.PT1Reverse then
  begin
    FJunctionBoxPtDown.Links[0].OutPortNo := 0;
    FJunctionBoxPtDown.Links[1].OutPortNo := 1;
  end
  else
  begin
    FJunctionBoxPtDown.Links[0].OutPortNo := 1;
    FJunctionBoxPtDown.Links[1].OutPortNo := 0;
  end;
  if not FWiringError.PT2Reverse then
  begin
    FJunctionBoxPtDown.Links[2].OutPortNo := 2;
    FJunctionBoxPtDown.Links[3].OutPortNo := 3;
  end
  else
  begin
    FJunctionBoxPtDown.Links[2].OutPortNo := 3;
    FJunctionBoxPtDown.Links[3].OutPortNo := 2;
  end;
  FJunctionBoxPtDown.Draw;



  //PT表尾接线
  ChangeSequence(FjunctionBoxPtUp.Links, FWiringError.USequence);
  FJunctionBoxPtUp.Draw;

  //CT接线盒连线
  for I := 0 to 3 do
    FJunctionBoxCt.Links[I].OutPortNo := I;
  case FWiringError.I1In of
    plA: FJunctionBoxCt.Links[0].InPortNo := 0;
    plC: FJunctionBoxCt.Links[0].InPortNo := 2;
    plN: FJunctionBoxCt.Links[0].InPortNo := 1;
  end;
  case FWiringError.I1Out of
    plA: FJunctionBoxCt.Links[1].InPortNo := 0;
    plC: FJunctionBoxCt.Links[1].InPortNo := 2;
    plN: FJunctionBoxCt.Links[1].InPortNo := 1;
  end;
  case FWiringError.I2In of
    plA: FJunctionBoxCt.Links[2].InPortNo := 0;
    plC: FJunctionBoxCt.Links[2].InPortNo := 2;
    plN: FJunctionBoxCt.Links[2].InPortNo := 3;
  end;
  case FWiringError.I2Out of
    plA: FJunctionBoxCt.Links[3].InPortNo := 0;
    plC: FJunctionBoxCt.Links[3].InPortNo := 2;
    plN: FJunctionBoxCt.Links[3].InPortNo := 3;
  end;
  FJunctionBoxCt.Draw;

  //CT极性反接
  if not FWiringError.CT1Reverse then
    FJunctionBoxCtA.SetOutPortOrder([0, 1])
  else
    FJunctionBoxCtA.SetOutPortOrder([1, 0]);
  FJunctionBoxCtA.Draw;

  if not FWiringError.CT2Reverse then
    FJunctionBoxCtC.SetOutPortOrder([0, 1])
  else
    FJunctionBoxCtC.SetOutPortOrder([1, 0]);
  FJunctionBoxCtC.Draw;

  //CT短路
  if FWiringError.CT1Short then
    DgDrawConnection(FBitmap.Canvas, FCT1.LeftPlugPos, FCT1.RightPlugPos);

  if FWiringError.CT2Short then
    DgDrawConnection(FBitmap.Canvas, FCT3.LeftPlugPos, FCT3.RightPlugPos);

  // 电流开路
  if FWiringError.IaBroken then
  begin
    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[0], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[0], 0, 25));
//    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[3], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[3], 0, 25));
  end;
  if FWiringError.IbBroken then
  begin
    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[1], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[1], 0, 25));
//    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[4], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[4], 0, 25));
  end;
  if FWiringError.IcBroken then
  begin
    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[2], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[2], 0, 25));
//    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[5], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[5], 0, 25));
  end;
//
  //CT到CT接线盒的连线
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtA.OutPorts[0], FJunctionBoxCt.InPorts[0]);
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtA.OutPorts[1], FJunctionBoxCt.InPorts[1]);
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtC.OutPorts[0], FJunctionBoxCt.InPorts[2]);
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtC.OutPorts[1], FJunctionBoxCt.InPorts[3]);

  //CT接线盒到电表的连线
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxCt.OutPorts[0], FMeter1.GetPlugPos(dmpsIa_in), FJunctionBoxCt.OutPorts[0].Y - 40);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxCt.OutPorts[1], FMeter1.GetPlugPos(dmpsIa_out), FJunctionBoxCt.OutPorts[0].Y - 35);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxCt.OutPorts[2], FMeter1.GetPlugPos(dmpsIc_in), FJunctionBoxCt.OutPorts[0].Y - 30);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxCt.OutPorts[3], FMeter1.GetPlugPos(dmpsIc_out), FJunctionBoxCt.OutPorts[0].Y - 25);

  // 电流接地断开
  if FWiringError.IaGroundBroken then
    DrawBroken( DgOffsetPoint(FCT1.RightPlugPos, 6, -10), DgOffsetPoint(FCT1.RightPlugPos, 6, -10));
  if FWiringError.IbGroundBroken then
    DrawBroken( DgOffsetPoint(FCT2.RightPlugPos, 6, -10), DgOffsetPoint(FCT2.RightPlugPos, 6, -10));
  if FWiringError.IcGroundBroken then
    DrawBroken( DgOffsetPoint(FCT3.RightPlugPos, 6, -10), DgOffsetPoint(FCT3.RightPlugPos, 6, -10));

  // 接地
  if FWiringError.IGroundBroken then
    DrawBroken(DgOffsetPoint(FCTGround.Pos, -15, -5), DgOffsetPoint(FCTGround.Pos, -15, 5));
end;

procedure TWE_Diagram2.DrawType4Direct;
begin
  //外部电线到接线盒的连线
  FBitmap.Canvas.Polyline([FJunctionBoxPtUp.InPorts[0], Point(FJunctionBoxPtUp.InPorts[0].X, FWire1.StartPos.Y)]);
  FBitmap.Canvas.Polyline([FJunctionBoxPtUp.InPorts[1], Point(FJunctionBoxPtUp.InPorts[1].X, FWire2.StartPos.Y)]);
  FBitmap.Canvas.Polyline([FJunctionBoxPtUp.InPorts[2], Point(FJunctionBoxPtUp.InPorts[2].X, FWire3.StartPos.Y)]);

  //PT上接线盒到电表的连线
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[0], FMeter1.GetPlugPos(dmpsIa_in), FMeter1.GetPlugPos(0).Y + 60);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[1], FMeter1.GetPlugPos(dmpsIb_in), FMeter1.GetPlugPos(0).Y + 70);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[2], FMeter1.GetPlugPos(dmpsIc_in), FMeter1.GetPlugPos(0).Y + 80);

  //PT断相
  if FWiringError.UaBroken then
    DrawBroken(DgOffsetPoint(FJunctionBoxPtUp.InPorts[0], 0, 30), DgOffsetPoint(FJunctionBoxPtUp.InPorts[0], 0, 20));
  if FWiringError.UbBroken then
    DrawBroken(DgOffsetPoint(FJunctionBoxPtUp.InPorts[1], 0, 30), DgOffsetPoint(FJunctionBoxPtUp.InPorts[1], 0, 20));
  if FWiringError.UcBroken then
    DrawBroken(DgOffsetPoint(FJunctionBoxPtUp.InPorts[2], 0, 30), DgOffsetPoint(FJunctionBoxPtUp.InPorts[2], 0, 20));
  if FWiringError.UnBroken then
    DrawBroken(Point(FMeter1.GetPlugPos(dmpsUn1).X, FJunctionBoxPtUp.InPorts[0].Y + 30), Point(FMeter1.GetPlugPos(dmpsUn1).X, FJunctionBoxPtUp.InPorts[0].Y + 20));

  //表尾进线接线
  ChangeSequence(FjunctionBoxPtUp.Links, FWiringError.USequence);
  FJunctionBoxPtUp.Draw;

  DgDrawConnection(FBitmap.Canvas, Point(FWire1.BreakXPos2, FWire1.StartPos.Y), FMeter2.GetPlugPos(dmpsIa_out));
  DgDrawConnection(FBitmap.Canvas, Point(FWire2.BreakXPos2, FWire2.StartPos.Y), FMeter2.GetPlugPos(dmpsIb_out));
  DgDrawConnection(FBitmap.Canvas, Point(FWire3.BreakXPos2, FWire3.StartPos.Y), FMeter2.GetPlugPos(dmpsIc_out));
  FBitmap.Canvas.Polyline([FMeter1.GetPlugPos(dmpsUn1), Point(FMeter1.GetPlugPos(dmpsUn1).X, FWire4.StartPos.Y)]);

  DgDrawConnection3Y(FBitmap.Canvas, FMeter1.GetPlugPos(dmpsIa_out), FMeter2.GetPlugPos(dmpsIa_in), FMeter1.GetPlugPos(0).Y + 20);
  DgDrawConnection3Y(FBitmap.Canvas, FMeter1.GetPlugPos(dmpsIb_out), FMeter2.GetPlugPos(dmpsIb_in), FMeter1.GetPlugPos(0).Y + 30);
  DgDrawConnection3Y(FBitmap.Canvas, FMeter1.GetPlugPos(dmpsIc_out), FMeter2.GetPlugPos(dmpsIc_in), FMeter1.GetPlugPos(0).Y + 40);
end;

procedure TWE_Diagram2.DrawType4M_NoPT;
var
  nTemp : Integer;
begin
  //外部电线到接线盒的连线
  FBitmap.Canvas.Polyline([FJunctionBoxPtUp.InPorts[0], Point(FJunctionBoxPtUp.InPorts[0].X, FWire1.StartPos.Y)]);
  FBitmap.Canvas.Polyline([FJunctionBoxPtUp.InPorts[1], Point(FJunctionBoxPtUp.InPorts[1].X, FWire2.StartPos.Y)]);
  FBitmap.Canvas.Polyline([FJunctionBoxPtUp.InPorts[2], Point(FJunctionBoxPtUp.InPorts[2].X, FWire3.StartPos.Y)]);

  //PT上接线盒到电表的连线
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[0], FMeter1.GetPlugPos(dmpsUa), FMeter1.GetPlugPos(0).Y + 60);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[1], FMeter1.GetPlugPos(dmpsUb), FMeter1.GetPlugPos(0).Y + 70);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[2], FMeter1.GetPlugPos(dmpsUc), FMeter1.GetPlugPos(0).Y + 80);
  DgDrawConnection3Y(FBitmap.Canvas, Point(FPTGroup.PT3.RightTPlugPos.X, FWire4.StartPos.Y), FMeter1.GetPlugPos(dmpsUn1), FMeter1.GetPlugPos(0).Y + 90);

  //PT断相
  if FWiringError.UaBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT1.LeftTPlugPos, 0, -20), DgOffsetPoint(FPTGroup.PT1.LeftTPlugPos, 0, -30));
  if FWiringError.UbBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT2.LeftTPlugPos, 0, -20), DgOffsetPoint(FPTGroup.PT2.LeftTPlugPos, 0, -30));
  if FWiringError.UcBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT3.LeftTPlugPos, 0, -20), DgOffsetPoint(FPTGroup.PT3.LeftTPlugPos, 0, -30));
  if FWiringError.UnBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT3.RightTPlugPos, 0, -20), DgOffsetPoint(FPTGroup.PT3.RightTPlugPos, 0, -30));

  //PT表尾接线
  ChangeSequence(FjunctionBoxPtUp.Links, FWiringError.USequence);
  FJunctionBoxPtUp.Draw;

  //CT接线盒连线
  ChangeSequence(FJunctionBoxCt.Links, FWiringError.ISequence);

  // 表尾电流反接
  if FWiringError.I1Reverse then
  begin
    nTemp := FJunctionBoxCt.Links[0].InPortNo;
    FJunctionBoxCt.Links[0].InPortNo:=FJunctionBoxCt.Links[3].InPortNo;
    FJunctionBoxCt.Links[3].InPortNo:=nTemp;
  end;

  if FWiringError.I2Reverse then
  begin
    nTemp := FJunctionBoxCt.Links[1].InPortNo;
    FJunctionBoxCt.Links[1].InPortNo:=FJunctionBoxCt.Links[4].InPortNo;
    FJunctionBoxCt.Links[4].InPortNo:=nTemp;
  end;

  if FWiringError.I3Reverse then
  begin
    nTemp := FJunctionBoxCt.Links[2].InPortNo;
    FJunctionBoxCt.Links[2].InPortNo:=FJunctionBoxCt.Links[5].InPortNo;
    FJunctionBoxCt.Links[5].InPortNo:=nTemp;
  end;

  FJunctionBoxCt.Draw;

  //CT极性反接
  if not FWiringError.CT1Reverse then
    FJunctionBoxCtA.SetOutPortOrder([0, 1])
  else
    FJunctionBoxCtA.SetOutPortOrder([1, 0]);
  FJunctionBoxCtA.Draw;

  if not FWiringError.CT2Reverse then
    FJunctionBoxCtB.SetOutPortOrder([0, 1])
  else
    FJunctionBoxCtB.SetOutPortOrder([1, 0]);
  FJunctionBoxCtB.Draw;

  if not FWiringError.CT3Reverse then
    FJunctionBoxCtC.SetOutPortOrder([0, 1])
  else
    FJunctionBoxCtC.SetOutPortOrder([1, 0]);
  FJunctionBoxCtC.Draw;

  //CT短路
  if FWiringError.CT1Short then
    DgDrawConnection(FBitmap.Canvas, FCT1.LeftPlugPos, FCT1.RightPlugPos);

  if FWiringError.CT2Short then
    DgDrawConnection(FBitmap.Canvas, FCT2.LeftPlugPos, FCT2.RightPlugPos);

  if FWiringError.CT3Short then
    DgDrawConnection(FBitmap.Canvas, FCT3.LeftPlugPos, FCT3.RightPlugPos);

  // 电流开路
  if FWiringError.IaBroken then
  begin
    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[0], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[0], 0, 25));
//    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[3], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[3], 0, 25));
  end;
  if FWiringError.IbBroken then
  begin
    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[1], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[1], 0, 25));
//    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[4], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[4], 0, 25));
  end;
  if FWiringError.IcBroken then
  begin
    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[2], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[2], 0, 25));
//    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[5], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[5], 0, 25));
  end;

  //CT到CT接线盒的连线
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtA.OutPorts[0], FJunctionBoxCt.InPorts[0]);
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtB.OutPorts[0], FJunctionBoxCt.InPorts[1]);
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtC.OutPorts[0], FJunctionBoxCt.InPorts[2]);
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtA.OutPorts[1], FJunctionBoxCt.InPorts[3]);
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtB.OutPorts[1], FJunctionBoxCt.InPorts[4]);
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtC.OutPorts[1], FJunctionBoxCt.InPorts[5]);

  //CT接线盒到电表的连线, 以及电表之间的电流端子接线
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCt.OutPorts[0], FMeter1.GetPlugPos(dmpsIa_in));
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCt.OutPorts[1], FMeter1.GetPlugPos(dmpsIb_in));
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCt.OutPorts[2], FMeter1.GetPlugPos(dmpsIc_in));

  FJunctionBoxCtA.OutPorts[1].Y := FJunctionBoxCt.OutPorts[0].Y;
  FJunctionBoxCtB.OutPorts[1].Y := FJunctionBoxCt.OutPorts[1].Y;
  FJunctionBoxCtC.OutPorts[1].Y := FJunctionBoxCt.OutPorts[2].Y;
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtA.OutPorts[1], FMeter1.GetPlugPos(dmpsIa_out));
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtB.OutPorts[1], FMeter1.GetPlugPos(dmpsIb_out));
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtC.OutPorts[1], FMeter1.GetPlugPos(dmpsIc_out));

  // 电流接地断开
  if FWiringError.IaGroundBroken then
    DrawBroken( DgOffsetPoint(FCT1.RightPlugPos, 6, -10), DgOffsetPoint(FCT1.RightPlugPos, 6, -10));
  if FWiringError.IbGroundBroken then
    DrawBroken( DgOffsetPoint(FCT2.RightPlugPos, 6, -10), DgOffsetPoint(FCT2.RightPlugPos, 6, -10));
  if FWiringError.IcGroundBroken then
    DrawBroken( DgOffsetPoint(FCT3.RightPlugPos, 6, -10), DgOffsetPoint(FCT3.RightPlugPos, 6, -10));
  // 接地
  if FWiringError.IGroundBroken then
    DrawBroken(DgOffsetPoint(FCTGround.Pos, -15, -5), DgOffsetPoint(FCTGround.Pos, -15, 5));
end;

procedure TWE_Diagram2.DrawType4M_PT;
var
  nTemp : Integer;
begin
  //外部电线到PT的连线
  FBitmap.Canvas.Polyline([FPTGroup.PT1.LeftBPlugPos, Point(FPTGroup.PT1.LeftBPlugPos.X, FWire1.StartPos.Y)]);
  FBitmap.Canvas.Polyline([FPTGroup.PT2.LeftBPlugPos, Point(FPTGroup.PT2.LeftBPlugPos.X, FWire2.StartPos.Y)]);
  FBitmap.Canvas.Polyline([FPTGroup.PT3.LeftBPlugPos, Point(FPTGroup.PT3.LeftBPlugPos.X, FWire3.StartPos.Y)]);
  FBitmap.Canvas.Polyline([FPTGroup.PT3.RightBPlugPos, Point(FPTGroup.PT3.RightBPlugPos.X, FWire3.StartPos.Y)]);
  DgDrawConnection(FBitmap.Canvas, DgOffsetPoint(FPTGroup.PT3.RightBPlugPos, 0, 10), FPTGroup.PT1.RightBPlugPos);
  DgDrawConnection(FBitmap.Canvas, DgOffsetPoint(FPTGroup.PT3.RightBPlugPos, 0, 10), FPTGroup.PT2.RightBPlugPos);

//  //PT到PT接线盒之间的连线
//  FBitmap.Canvas.Polyline([FPTGroup.PT1.LeftTPlugPos, FJunctionBoxPtUp.InPorts[0]]);
//  FBitmap.Canvas.Polyline([FPTGroup.PT2.LeftTPlugPos, FJunctionBoxPtUp.InPorts[1]]);
//  FBitmap.Canvas.Polyline([FPTGroup.PT3.LeftTPlugPos, FJunctionBoxPtUp.InPorts[2]]);

  FBitmap.Canvas.Polyline([FJunctionBoxPtDown.OutPorts[0], FJunctionBoxPtUp.InPorts[0]]);
  FBitmap.Canvas.Polyline([FJunctionBoxPtDown.OutPorts[2], FJunctionBoxPtUp.InPorts[1]]);
  FBitmap.Canvas.Polyline([FJunctionBoxPtDown.OutPorts[4], FJunctionBoxPtUp.InPorts[2]]);

  //PT上接线盒到电表的连线
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[0], FMeter1.GetPlugPos(dmpsUa), FMeter1.GetPlugPos(0).Y + 60);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[1], FMeter1.GetPlugPos(dmpsUb), FMeter1.GetPlugPos(0).Y + 70);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[2], FMeter1.GetPlugPos(dmpsUc), FMeter1.GetPlugPos(0).Y + 80);
  DgDrawConnection3Y(FBitmap.Canvas, FPTGroup.PT3.RightTPlugPos, FMeter1.GetPlugPos(dmpsUn1), FMeter1.GetPlugPos(0).Y + 90);

  //PT二次断相
  if FWiringError.UsaBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT1.LeftTPlugPos, 0, -20), DgOffsetPoint(FPTGroup.PT1.LeftTPlugPos, 0, -30));
  if FWiringError.UsbBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT2.LeftTPlugPos, 0, -20), DgOffsetPoint(FPTGroup.PT2.LeftTPlugPos, 0, -30));
  if FWiringError.UscBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT3.LeftTPlugPos, 0, -20), DgOffsetPoint(FPTGroup.PT3.LeftTPlugPos, 0, -30));
  if FWiringError.UsnBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT3.RightTPlugPos, 0, -20), DgOffsetPoint(FPTGroup.PT3.RightTPlugPos, 0, -30));

  //PT一次断相
  if FWiringError.UaBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT1.LeftTPlugPos, 0, 40), DgOffsetPoint(FPTGroup.PT1.LeftTPlugPos, 0, 50));
  if FWiringError.UbBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT2.LeftTPlugPos, 0, 40), DgOffsetPoint(FPTGroup.PT2.LeftTPlugPos, 0, 50));
  if FWiringError.UcBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT3.LeftTPlugPos, 0, 40), DgOffsetPoint(FPTGroup.PT3.LeftTPlugPos, 0, 50));
  if FWiringError.UnBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT3.RightTPlugPos, 0, 40), DgOffsetPoint(FPTGroup.PT3.RightTPlugPos, 0, 50));

  // 接地
  if FWiringError.GroundBroken then
  DrawBroken(DgOffsetPoint(FPTGround.Pos, 0, -10), DgOffsetPoint(FPTGround.Pos, 0, 0));

  //PT极性反接
  if not FWiringError.PT1Reverse then
  begin
    FJunctionBoxPtDown.Links[0].OutPortNo := 0;
    FJunctionBoxPtDown.Links[1].OutPortNo := 1;
  end
  else
  begin
    FJunctionBoxPtDown.Links[0].OutPortNo := 1;
    FJunctionBoxPtDown.Links[1].OutPortNo := 0;
  end;
  if not FWiringError.PT2Reverse then
  begin
    FJunctionBoxPtDown.Links[2].OutPortNo := 2;
    FJunctionBoxPtDown.Links[3].OutPortNo := 3;
  end
  else
  begin
    FJunctionBoxPtDown.Links[2].OutPortNo := 3;
    FJunctionBoxPtDown.Links[3].OutPortNo := 2;
  end;
  if not FWiringError.PT3Reverse then
  begin
    FJunctionBoxPtDown.Links[4].OutPortNo := 4;
    FJunctionBoxPtDown.Links[5].OutPortNo := 5;
  end
  else
  begin
    FJunctionBoxPtDown.Links[4].OutPortNo := 5;
    FJunctionBoxPtDown.Links[5].OutPortNo := 4;
  end;

  FJunctionBoxPtDown.Draw;


  //PT表尾接线
  ChangeSequence(FjunctionBoxPtUp.Links, FWiringError.USequence);
  FJunctionBoxPtUp.Draw;

  //CT接线盒连线
  ChangeSequence(FJunctionBoxCt.Links, FWiringError.ISequence);
  // 表尾电流反接
  if FWiringError.I1Reverse then
  begin
    nTemp := FJunctionBoxCt.Links[0].InPortNo;
    FJunctionBoxCt.Links[0].InPortNo:=FJunctionBoxCt.Links[3].InPortNo;
    FJunctionBoxCt.Links[3].InPortNo:=nTemp;
  end;

  if FWiringError.I2Reverse then
  begin
    nTemp := FJunctionBoxCt.Links[1].InPortNo;
    FJunctionBoxCt.Links[1].InPortNo:=FJunctionBoxCt.Links[4].InPortNo;
    FJunctionBoxCt.Links[4].InPortNo:=nTemp;
  end;

  if FWiringError.I3Reverse then
  begin
    nTemp := FJunctionBoxCt.Links[2].InPortNo;
    FJunctionBoxCt.Links[2].InPortNo:=FJunctionBoxCt.Links[5].InPortNo;
    FJunctionBoxCt.Links[5].InPortNo:=nTemp;
  end;

  FJunctionBoxCt.Draw;

  //CT极性反接
  if not FWiringError.CT1Reverse then
    FJunctionBoxCtA.SetOutPortOrder([0, 1])
  else
    FJunctionBoxCtA.SetOutPortOrder([1, 0]);
  FJunctionBoxCtA.Draw;

  if not FWiringError.CT2Reverse then
    FJunctionBoxCtB.SetOutPortOrder([0, 1])
  else
    FJunctionBoxCtB.SetOutPortOrder([1, 0]);
  FJunctionBoxCtB.Draw;

  if not FWiringError.CT3Reverse then
    FJunctionBoxCtC.SetOutPortOrder([0, 1])
  else
    FJunctionBoxCtC.SetOutPortOrder([1, 0]);
  FJunctionBoxCtC.Draw;

  //CT短路
  if FWiringError.CT1Short then
    DgDrawConnection(FBitmap.Canvas, FCT1.LeftPlugPos, FCT1.RightPlugPos);

  if FWiringError.CT2Short then
    DgDrawConnection(FBitmap.Canvas, FCT2.LeftPlugPos, FCT2.RightPlugPos);

  if FWiringError.CT3Short then
    DgDrawConnection(FBitmap.Canvas, FCT3.LeftPlugPos, FCT3.RightPlugPos);

  // 电流开路
  if FWiringError.IaBroken then
  begin
    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[0], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[0], 0, 25));
//    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[3], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[3], 0, 25));
  end;
  if FWiringError.IbBroken then
  begin
    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[1], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[1], 0, 25));
//    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[4], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[4], 0, 25));
  end;
  if FWiringError.IcBroken then
  begin
    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[2], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[2], 0, 25));
//    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[5], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[5], 0, 25));
  end;

//  //CT到CT接线盒的连线
//  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtA.OutPorts[0], FJunctionBoxCt.InPorts[0]);
//  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtB.OutPorts[0], FJunctionBoxCt.InPorts[1]);
//  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtC.OutPorts[0], FJunctionBoxCt.InPorts[2]);
//
//  //CT接线盒到电表的连线, 以及电表之间的电流端子接线
//  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCt.OutPorts[0], FMeter1.GetPlugPos(dmpsIa_in));
//  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCt.OutPorts[1], FMeter1.GetPlugPos(dmpsIb_in));
//  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCt.OutPorts[2], FMeter1.GetPlugPos(dmpsIc_in));
//  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtA.OutPorts[1], FMeter1.GetPlugPos(dmpsIa_out));
//  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtB.OutPorts[1], FMeter1.GetPlugPos(dmpsIb_out));
//  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtC.OutPorts[1], FMeter1.GetPlugPos(dmpsIc_out));

  //CT到CT接线盒的连线
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtA.OutPorts[0], FJunctionBoxCt.InPorts[0]);
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtB.OutPorts[0], FJunctionBoxCt.InPorts[1]);
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtC.OutPorts[0], FJunctionBoxCt.InPorts[2]);
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtA.OutPorts[1], FJunctionBoxCt.InPorts[3]);
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtB.OutPorts[1], FJunctionBoxCt.InPorts[4]);
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtC.OutPorts[1], FJunctionBoxCt.InPorts[5]);

  //CT接线盒到电表的连线, 以及电表之间的电流端子接线
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCt.OutPorts[0], FMeter1.GetPlugPos(dmpsIa_in));
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCt.OutPorts[1], FMeter1.GetPlugPos(dmpsIb_in));
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCt.OutPorts[2], FMeter1.GetPlugPos(dmpsIc_in));

  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCt.OutPorts[3], FMeter1.GetPlugPos(dmpsIa_out));
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCt.OutPorts[4], FMeter1.GetPlugPos(dmpsIb_out));
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCt.OutPorts[5], FMeter1.GetPlugPos(dmpsIc_out));
  // 电流接地断开
  if FWiringError.IaGroundBroken then
    DrawBroken( DgOffsetPoint(FCT1.RightPlugPos, 6, -10), DgOffsetPoint(FCT1.RightPlugPos, 6, -10));
  if FWiringError.IbGroundBroken then
    DrawBroken( DgOffsetPoint(FCT2.RightPlugPos, 6, -10), DgOffsetPoint(FCT2.RightPlugPos, 6, -10));
  if FWiringError.IcGroundBroken then
    DrawBroken( DgOffsetPoint(FCT3.RightPlugPos, 6, -10), DgOffsetPoint(FCT3.RightPlugPos, 6, -10));

  // 接地
  if FWiringError.IGroundBroken then
    DrawBroken(DgOffsetPoint(FCTGround.Pos, -15, -5), DgOffsetPoint(FCTGround.Pos, -15, 5));

end;

procedure TWE_Diagram2.DrawType4_NoPT_L6;
var
  nTemp : Integer;
begin

//  //外部电线到PT上接线盒的连线
  FBitmap.Canvas.Polyline([FJunctionBoxPtUp.InPorts[0], Point(FPTGroup.PT1.LeftBPlugPos.X, FWire1.StartPos.Y)]);
  FBitmap.Canvas.Polyline([FJunctionBoxPtUp.InPorts[1], Point(FPTGroup.PT2.LeftBPlugPos.X, FWire2.StartPos.Y)]);
  FBitmap.Canvas.Polyline([FJunctionBoxPtUp.InPorts[2], Point(FPTGroup.PT3.LeftBPlugPos.X, FWire3.StartPos.Y)]);
  FBitmap.Canvas.Polyline([FPTGroup.PT3.RightBPlugPos, Point(FPTGroup.PT3.RightBPlugPos.X, FWire3.StartPos.Y)]);

  //PT上接线盒到电表的连线
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[0], FMeter1.GetPlugPos(dmpsUa), FMeter1.GetPlugPos(0).Y + 60);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[0], FMeter2.GetPlugPos(dmpsUa), FMeter1.GetPlugPos(0).Y + 60);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[1], FMeter1.GetPlugPos(dmpsUb), FMeter1.GetPlugPos(0).Y + 70);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[1], FMeter2.GetPlugPos(dmpsUb), FMeter1.GetPlugPos(0).Y + 70);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[2], FMeter1.GetPlugPos(dmpsUc), FMeter1.GetPlugPos(0).Y + 80);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[2], FMeter2.GetPlugPos(dmpsUc), FMeter1.GetPlugPos(0).Y + 80);
  DgDrawConnection3Y(FBitmap.Canvas, FPTGroup.PT3.RightTPlugPos, FMeter1.GetPlugPos(dmpsUn1), FMeter1.GetPlugPos(0).Y + 90);
  DgDrawConnection3Y(FBitmap.Canvas, Point(FPTGroup.PT3.RightTPlugPos.X, FWire4.StartPos.Y), FMeter1.GetPlugPos(dmpsUn1), FMeter1.GetPlugPos(0).Y + 90);

  //PT断相
  if FWiringError.UaBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT1.LeftTPlugPos, 0, -20), DgOffsetPoint(FPTGroup.PT1.LeftTPlugPos, 0, -30));
  if FWiringError.UbBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT2.LeftTPlugPos, 0, -20), DgOffsetPoint(FPTGroup.PT2.LeftTPlugPos, 0, -30));
  if FWiringError.UcBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT3.LeftTPlugPos, 0, -20), DgOffsetPoint(FPTGroup.PT3.LeftTPlugPos, 0, -30));
  if FWiringError.UnBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT3.RightTPlugPos, 0, -20), DgOffsetPoint(FPTGroup.PT3.RightTPlugPos, 0, -30));

  //PT表尾接线
  ChangeSequence(FjunctionBoxPtUp.Links, FWiringError.USequence);
  FJunctionBoxPtUp.Draw;

  //CT接线盒连线
  ChangeSequence(FJunctionBoxCt.Links, FWiringError.ISequence);

  // 表尾电流反接
  if FWiringError.I1Reverse then
  begin
    nTemp := FJunctionBoxCt.Links[0].InPortNo;
    FJunctionBoxCt.Links[0].InPortNo:=FJunctionBoxCt.Links[3].InPortNo;
    FJunctionBoxCt.Links[3].InPortNo:=nTemp;
  end;

  if FWiringError.I2Reverse then
  begin
    nTemp := FJunctionBoxCt.Links[1].InPortNo;
    FJunctionBoxCt.Links[1].InPortNo:=FJunctionBoxCt.Links[4].InPortNo;
    FJunctionBoxCt.Links[4].InPortNo:=nTemp;
  end;

  if FWiringError.I3Reverse then
  begin
    nTemp := FJunctionBoxCt.Links[2].InPortNo;
    FJunctionBoxCt.Links[2].InPortNo:=FJunctionBoxCt.Links[5].InPortNo;
    FJunctionBoxCt.Links[5].InPortNo:=nTemp;
  end;

  FJunctionBoxCt.Draw;


  //CT极性反接
  if not FWiringError.CT1Reverse then
    FJunctionBoxCtA.SetOutPortOrder([0, 1])
  else
    FJunctionBoxCtA.SetOutPortOrder([1, 0]);
  FJunctionBoxCtA.Draw;

  if not FWiringError.CT2Reverse then
    FJunctionBoxCtB.SetOutPortOrder([0, 1])
  else
    FJunctionBoxCtB.SetOutPortOrder([1, 0]);
  FJunctionBoxCtB.Draw;

  if not FWiringError.CT3Reverse then
    FJunctionBoxCtC.SetOutPortOrder([0, 1])
  else
    FJunctionBoxCtC.SetOutPortOrder([1, 0]);
  FJunctionBoxCtC.Draw;

  //CT短路
  if FWiringError.CT1Short then
    DgDrawConnection(FBitmap.Canvas, FCT1.LeftPlugPos, FCT1.RightPlugPos);

  if FWiringError.CT2Short then
    DgDrawConnection(FBitmap.Canvas, FCT2.LeftPlugPos, FCT2.RightPlugPos);

  if FWiringError.CT3Short then
    DgDrawConnection(FBitmap.Canvas, FCT3.LeftPlugPos, FCT3.RightPlugPos);

  // 电流开路
  if FWiringError.IaBroken then
  begin
    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[0], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[0], 0, 25));
//    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[3], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[3], 0, 25));
  end;
  if FWiringError.IbBroken then
  begin
    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[1], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[1], 0, 25));
//    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[4], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[4], 0, 25));
  end;
  if FWiringError.IcBroken then
  begin
    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[2], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[2], 0, 25));
//    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[5], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[5], 0, 25));
  end;

  //CT到CT接线盒的连线
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtA.OutPorts[0], FJunctionBoxCt.InPorts[0]);
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtB.OutPorts[0], FJunctionBoxCt.InPorts[1]);
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtC.OutPorts[0], FJunctionBoxCt.InPorts[2]);

  //CT接线盒到电表的连线, 以及电表之间的电流端子接线
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCt.OutPorts[0], FMeter1.GetPlugPos(dmpsIa_in));
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCt.OutPorts[1], FMeter1.GetPlugPos(dmpsIb_in));
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCt.OutPorts[2], FMeter1.GetPlugPos(dmpsIc_in));

  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxCt.OutPorts[3], FMeter2.GetPlugPos(dmpsIa_out), FJunctionBoxCt.OutPorts[0].Y - 20);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxCt.OutPorts[4], FMeter2.GetPlugPos(dmpsIb_out), FJunctionBoxCt.OutPorts[0].Y - 15);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxCt.OutPorts[5], FMeter2.GetPlugPos(dmpsIc_out), FJunctionBoxCt.OutPorts[0].Y - 10);

  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtA.OutPorts[1], FJunctionBoxCt.InPorts[3]);
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtB.OutPorts[1], FJunctionBoxCt.InPorts[4]);
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtC.OutPorts[1], FJunctionBoxCt.InPorts[5]);



  DgDrawConnection3Y(FBitmap.Canvas, FMeter1.GetPlugPos(dmpsIa_out), FMeter2.GetPlugPos(dmpsIa_in), FMeter1.GetPlugPos(0).Y + 20);
  DgDrawConnection3Y(FBitmap.Canvas, FMeter1.GetPlugPos(dmpsIb_out), FMeter2.GetPlugPos(dmpsIb_in), FMeter1.GetPlugPos(0).Y + 30);
  DgDrawConnection3Y(FBitmap.Canvas, FMeter1.GetPlugPos(dmpsIc_out), FMeter2.GetPlugPos(dmpsIc_in), FMeter1.GetPlugPos(0).Y + 40);




  // 电流接地断开
  if FWiringError.IaGroundBroken then
    DrawBroken( DgOffsetPoint(FCT1.RightPlugPos, 6, -10), DgOffsetPoint(FCT1.RightPlugPos, 6, -10));
  if FWiringError.IbGroundBroken then
    DrawBroken( DgOffsetPoint(FCT2.RightPlugPos, 6, -10), DgOffsetPoint(FCT2.RightPlugPos, 6, -10));
  if FWiringError.IcGroundBroken then
    DrawBroken( DgOffsetPoint(FCT3.RightPlugPos, 6, -10), DgOffsetPoint(FCT3.RightPlugPos, 6, -10));

  // 接地
  if FWiringError.IGroundBroken then
    DrawBroken(DgOffsetPoint(FCTGround.Pos, -15, -5), DgOffsetPoint(FCTGround.Pos, -15, 5));

end;

procedure TWE_Diagram2.DrawType4_PT_CT_CLear;
var
  AInLinks, AOutLinks : array of TPoint;
  nI1Value, nI2Value, nI3Value : integer;

  procedure DrawLine(nStartValue, nMidValue, nEndValue : Integer);
    procedure DrawConnection(nValue : Integer; APoint : TPoint);
    var
      OldColor: TColor;
    begin
      if nValue <> 8 then
        DgDrawConnection(FBitmap.Canvas, FMeter1.GetPlugPos(nValue), APoint);

      OldColor := FBitmap.Canvas.Brush.Color;
      FBitmap.Canvas.Brush.Color := clBlack;
      FBitmap.Canvas.Rectangle(Rect(APoint.X - 1, APoint.Y - 1, APoint.X + 2, APoint.Y + 2));
      FBitmap.Canvas.Brush.Color := OldColor;
    end;
  var
    AStartPoint, AMidPoint, AEndPoint : TPoint;
  begin
    AStartPoint := Point(FMeter1.GetPlugPos(nStartValue).x, FMeter1.GetPlugPos(nStartValue).y + 80);
    DrawConnection(nStartValue, AStartPoint);
    AMidPoint := Point(FMeter1.GetPlugPos(nMidValue).x, FMeter1.GetPlugPos(nMidValue).y + 80);
    DrawConnection(nMidValue, AMidPoint);
    AEndPoint := Point(FMeter1.GetPlugPos(nEndValue).x, FMeter1.GetPlugPos(nEndValue).y + 80);
    DrawConnection(nEndValue, AEndPoint);

    DgDrawConnection(FBitmap.Canvas, AStartPoint, AEndPoint);
  end;

  /// <summary>
  /// 初始化话连线坐标
  /// </summary>
  procedure InioLinks;
  var
    i : Integer;
    nPlus : Integer;
  begin
    for i := 0 to Length(AInLinks) - 1 do
    begin
      if i = Length(AInLinks) - 1 then
        nPlus := i * 3 - 1
      else
        nPlus := i * 3;

      AInLinks[i] := Point(FMeter1.GetPlugPos(nPlus).x, FMeter1.GetPlugPos(nPlus).y + 187);
      AOutLinks[i] := Point(FMeter1.GetPlugPos(nPlus).x, FMeter1.GetPlugPos(nPlus).y + 227);
    end;
  end;
  procedure DrawLinks;
  var
    nArry : array of Integer;
    i : Integer;
    OldColor: TColor;
  begin
    SetLength(nArry, Length(AInLinks));
    nArry[0] := nI1Value;
    nArry[1] := nI2Value;
    nArry[2] := nI3Value;
    nArry[3] := 6 - nI1Value - nI2Value - nI3Value;

    for i := 0 to Length(AInLinks) - 1 do
    begin
      if nArry[i] <> i then
      begin
        FBitmap.Canvas.Polyline([AInLinks[i],AOutLinks[nArry[i]]]);
        OldColor := FBitmap.Canvas.Pen.Color;
        FBitmap.Canvas.Pen.Color := clWhite;
        FBitmap.Canvas.Polyline([AInLinks[i],AOutLinks[i]]);
        FBitmap.Canvas.Pen.Color := OldColor;
      end;
    end;
    SetLength(nArry, 0);
  end;
  /// <param name="nValue">哪个元件[1,2,3]</param>
  procedure DrawReverse(nValue : Integer);
  var
    AIn1Point, AOut1Point, AIn2Point, AOut2Point : TPoint;
    nInValue, nOutValue : integer;
    OldColor : TColor;
  begin
    nInValue := (nValue - 1) * 3;
    nOutValue := nInValue + 2;
    AIn1Point := Point(FMeter1.GetPlugPos(nInValue).x, FMeter1.GetPlugPos(nInValue).y + 18);
    AOut1Point := Point(FMeter1.GetPlugPos(nInValue).x, FMeter1.GetPlugPos(nInValue).y + 50);
    AIn2Point := Point(FMeter1.GetPlugPos(nOutValue).x, FMeter1.GetPlugPos(nOutValue).y + 18);
    AOut2Point := Point(FMeter1.GetPlugPos(nOutValue).x, FMeter1.GetPlugPos(nOutValue).y + 50);

    FBitmap.Canvas.Polyline([AIn1Point,AOut2Point]);
    FBitmap.Canvas.Polyline([AIn2Point,AOut1Point]);

    OldColor := FBitmap.Canvas.pen.Color;
    FBitmap.Canvas.pen.Color := clWhite;
    FBitmap.Canvas.Polyline([AIn1Point,AOut1Point]);
    FBitmap.Canvas.Polyline([AIn2Point,AOut2Point]);
    FBitmap.Canvas.pen.Color := OldColor;
  end;
begin

  //外部电线到PT的连线
  FBitmap.Canvas.Polyline([FPTGroup.PT1.LeftBPlugPos, Point(FPTGroup.PT1.LeftBPlugPos.X, FWire1.StartPos.Y)]);
  FBitmap.Canvas.Polyline([FPTGroup.PT2.LeftBPlugPos, Point(FPTGroup.PT2.LeftBPlugPos.X, FWire2.StartPos.Y)]);
  FBitmap.Canvas.Polyline([FPTGroup.PT3.LeftBPlugPos, Point(FPTGroup.PT3.LeftBPlugPos.X, FWire3.StartPos.Y)]);
  FBitmap.Canvas.Polyline([FPTGroup.PT3.RightBPlugPos, Point(FPTGroup.PT3.RightBPlugPos.X, FWire3.StartPos.Y)]);
  DgDrawConnection(FBitmap.Canvas, DgOffsetPoint(FPTGroup.PT3.RightBPlugPos, 0, 10), FPTGroup.PT1.RightBPlugPos);
  DgDrawConnection(FBitmap.Canvas, DgOffsetPoint(FPTGroup.PT3.RightBPlugPos, 0, 10), FPTGroup.PT2.RightBPlugPos);
  DgDrawJunction(FBitmap.Canvas, DgOffsetPoint(FPTGroup.PT3.RightBPlugPos, 0, 10), True);

  FBitmap.Canvas.Polyline([FJunctionBoxPtDown.OutPorts[0], FJunctionBoxPtUp.InPorts[0]]);
  FBitmap.Canvas.Polyline([FJunctionBoxPtDown.OutPorts[2], FJunctionBoxPtUp.InPorts[1]]);
  FBitmap.Canvas.Polyline([FJunctionBoxPtDown.OutPorts[4], FJunctionBoxPtUp.InPorts[2]]);

//   for I := 0 to 2 do
//    FJunctionBoxCt.Links[I].OutPortNo := I;
//
//    with FWiringError do
//    begin
//      //ac
//      if (I1In in [plA, plN]) and (I1Out in [plA, plN]) and (I2In in [plC, plN]) and (I2Out in [plC, plN]) then
//      begin
//        SetInPortOrder(0, 1, 2);
//
//        if (I1In = plN) and (I1Out = plA) then
//        begin
//          DrawLine;
//        end;
//
//        if (I2In = plN) and (I2Out = plC) then
//        begin
//          DrawLine(1);
//        end;
//      end;
//    end;
    FJunctionBoxCt.Draw;

//  //PT上接线盒到电表的连线
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[0], FMeter1.GetPlugPos(dmpsUa), FMeter1.GetPlugPos(0).Y + 90);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[1], FMeter1.GetPlugPos(dmpsUb), FMeter1.GetPlugPos(0).Y + 100);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[2], FMeter1.GetPlugPos(dmpsUc), FMeter1.GetPlugPos(0).Y + 110);
  DgDrawConnection3Y(FBitmap.Canvas, FPTGroup.PT3.RightTPlugPos, FMeter1.GetPlugPos(dmpsUn1), FMeter1.GetPlugPos(0).Y + 120);

  //PT二次断相
  if FWiringError.UsaBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT1.LeftTPlugPos, 0, -20), DgOffsetPoint(FPTGroup.PT1.LeftTPlugPos, 0, -30));
  if FWiringError.UsbBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT2.LeftTPlugPos, 0, -20), DgOffsetPoint(FPTGroup.PT2.LeftTPlugPos, 0, -30));
  if FWiringError.UscBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT3.LeftTPlugPos, 0, -20), DgOffsetPoint(FPTGroup.PT3.LeftTPlugPos, 0, -30));
  if FWiringError.UsnBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT3.RightTPlugPos, 0, -20), DgOffsetPoint(FPTGroup.PT3.RightTPlugPos, 0, -30));

  //PT一次断相
  if FWiringError.UaBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT1.LeftTPlugPos, 0, 40), DgOffsetPoint(FPTGroup.PT1.LeftTPlugPos, 0, 50));
  if FWiringError.UbBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT2.LeftTPlugPos, 0, 40), DgOffsetPoint(FPTGroup.PT2.LeftTPlugPos, 0, 50));
  if FWiringError.UcBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT3.LeftTPlugPos, 0, 40), DgOffsetPoint(FPTGroup.PT3.LeftTPlugPos, 0, 50));
  if FWiringError.UnBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT3.RightTPlugPos, 0, 40), DgOffsetPoint(FPTGroup.PT3.RightTPlugPos, 0, 50));

  // 接地
  if FWiringError.GroundBroken then
  DrawBroken(DgOffsetPoint(FPTGround.Pos, 0, -10), DgOffsetPoint(FPTGround.Pos, 0, 0));

//  //PT极性反接
  if not FWiringError.PT1Reverse then
  begin
    FJunctionBoxPtDown.Links[0].OutPortNo := 0;
    FJunctionBoxPtDown.Links[1].OutPortNo := 1;
  end
  else
  begin
    FJunctionBoxPtDown.Links[0].OutPortNo := 1;
    FJunctionBoxPtDown.Links[1].OutPortNo := 0;
  end;
  if not FWiringError.PT2Reverse then
  begin
    FJunctionBoxPtDown.Links[2].OutPortNo := 2;
    FJunctionBoxPtDown.Links[3].OutPortNo := 3;
  end
  else
  begin
    FJunctionBoxPtDown.Links[2].OutPortNo := 3;
    FJunctionBoxPtDown.Links[3].OutPortNo := 2;
  end;
  if not FWiringError.PT3Reverse then
  begin
    FJunctionBoxPtDown.Links[4].OutPortNo := 4;
    FJunctionBoxPtDown.Links[5].OutPortNo := 5;
  end
  else
  begin
    FJunctionBoxPtDown.Links[4].OutPortNo := 5;
    FJunctionBoxPtDown.Links[5].OutPortNo := 4;
  end;

  FJunctionBoxPtDown.Draw;
//
//  //PT表尾接线
  ChangeSequence(FjunctionBoxPtUp.Links, FWiringError.USequence);
  FJunctionBoxPtUp.Draw;

  //CT接线盒连线
//  ChangeSequence(FJunctionBoxCt.Links, FWiringError.ISequence);
//  FJunctionBoxCt.Draw;

  //CT极性反接
  if not FWiringError.CT1Reverse then
    FJunctionBoxCtA.SetOutPortOrder([0, 1])
  else
    FJunctionBoxCtA.SetOutPortOrder([1, 0]);
  FJunctionBoxCtA.Draw;

  if not FWiringError.CT2Reverse then
    FJunctionBoxCtB.SetOutPortOrder([0, 1])
  else
    FJunctionBoxCtB.SetOutPortOrder([1, 0]);
  FJunctionBoxCtB.Draw;

  if not FWiringError.CT3Reverse then
    FJunctionBoxCtC.SetOutPortOrder([0, 1])
  else
    FJunctionBoxCtC.SetOutPortOrder([1, 0]);
  FJunctionBoxCtC.Draw;

  //CT短路
  if FWiringError.CT1Short then
    DgDrawConnection(FBitmap.Canvas, FCT1.LeftPlugPos, FCT1.RightPlugPos);

  if FWiringError.CT2Short then
    DgDrawConnection(FBitmap.Canvas, FCT2.LeftPlugPos, FCT2.RightPlugPos);

  if FWiringError.CT3Short then
    DgDrawConnection(FBitmap.Canvas, FCT3.LeftPlugPos, FCT3.RightPlugPos);

  // 电流开路
  if FWiringError.IaBroken then
  begin
    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[0], 0, 5), DgOffsetPoint(FJunctionBoxCt.InPorts[0], 0, 15));
  end;
  if FWiringError.IbBroken then
  begin
    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[1], 0, 5), DgOffsetPoint(FJunctionBoxCt.InPorts[1], 0, 15));
  end;
  if FWiringError.IcBroken then
  begin
    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[2], 0, 5), DgOffsetPoint(FJunctionBoxCt.InPorts[2], 0, 15));
  end;

  //CT到CT接线盒的连线
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtA.OutPorts[0], FJunctionBoxCt.InPorts[0]);
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtB.OutPorts[0], FJunctionBoxCt.InPorts[1]);
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtC.OutPorts[0], FJunctionBoxCt.InPorts[2]);

  //CT接线盒到电表的连线, 以及电表之间的电流端子接线
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCt.OutPorts[0], FMeter1.GetPlugPos(dmpsIa_in));
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCt.OutPorts[1], FMeter1.GetPlugPos(dmpsIb_in));
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCt.OutPorts[2], FMeter1.GetPlugPos(dmpsIc_in));
  //新增---
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtC.OutPorts[1], FMeter1.GetPlugPos(dmpsIc_out));

  //原始
  //DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxCtA.OutPorts[1], FMeter2.GetPlugPos(dmpsIa_out), FJunctionBoxCt.InPorts[0].Y + 20);
  //新增
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtA.OutPorts[1], FMeter1.GetPlugPos(dmpsIc_out));
//    DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxCtB.OutPorts[1], FMeter2.GetPlugPos(dmpsIb_out), FJunctionBoxCt.InPorts[0].Y + 20);
//  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxCtC.OutPorts[1], FMeter2.GetPlugPos(dmpsIc_out), FJunctionBoxCt.InPorts[0].Y + 20);


    DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtB.OutPorts[1], Point(FJunctionBoxCtB.OutPorts[1].X, FJunctionBoxCtB.OutPorts[1].Y - 30));
//  DgDrawConnection3Y(FBitmap.Canvas, FMeter1.GetPlugPos(dmpsIa_out), FMeter2.GetPlugPos(dmpsIa_in), FMeter1.GetPlugPos(0).Y + 20);
//  DgDrawConnection3Y(FBitmap.Canvas, FMeter1.GetPlugPos(dmpsIb_out), FMeter2.GetPlugPos(dmpsIb_in), FMeter1.GetPlugPos(0).Y + 30);
//  DgDrawConnection3Y(FBitmap.Canvas, FMeter1.GetPlugPos(dmpsIc_out), FMeter2.GetPlugPos(dmpsIc_in), FMeter1.GetPlugPos(0).Y + 40);
  // 电流接地断开
  if FWiringError.IaGroundBroken then
    DrawBroken( DgOffsetPoint(FCT1.RightPlugPos, 6, -10), DgOffsetPoint(FCT1.RightPlugPos, 6, -10));
  if FWiringError.IbGroundBroken then
    DrawBroken( DgOffsetPoint(FCT2.RightPlugPos, 6, -10), DgOffsetPoint(FCT2.RightPlugPos, 6, -10));
  if FWiringError.IcGroundBroken then
    DrawBroken( DgOffsetPoint(FCT3.RightPlugPos, 6, -10), DgOffsetPoint(FCT3.RightPlugPos, 6, -10));

  // 接地
  if FWiringError.IGroundBroken then
    DrawBroken(DgOffsetPoint(FCTGround.Pos, -15, -5), DgOffsetPoint(FCTGround.Pos, -15, 5));

  /////////////////////////////////////////////////////////////////////////
    DrawLine(2, 5, 8);
  /////////////////////////////////////////////////////////////////////////
  /////////////////////////////表尾接法//////////////////////////////////
    SetLength(AInLinks, 4);
    SetLength(AOutLinks, 4);
    InioLinks;
    with FWiringError do
    begin
      nI1Value := Ord(I1In);
      nI2Value := Ord(I2In);
      nI3Value := Ord(I3In);
      if I1Reverse then
      begin
        DrawReverse(1);
        nI1Value := Ord(I1Out);
      end;

      if I2Reverse then
      begin
        DrawReverse(2);
        nI2Value := Ord(I2Out);
      end;

      if I3Reverse then
      begin
        DrawReverse(3);
        nI3Value := Ord(I3Out);
      end;

      DrawLinks;
    end;
  ///////////////////////////////////////////////////////////////////////
end;

procedure TWE_Diagram2.DrawType4_PT_L6;
var
  nTemp : Integer;
begin
 
  //外部电线到PT的连线
  FBitmap.Canvas.Polyline([FPTGroup.PT1.LeftBPlugPos, Point(FPTGroup.PT1.LeftBPlugPos.X, FWire1.StartPos.Y)]);
  FBitmap.Canvas.Polyline([FPTGroup.PT2.LeftBPlugPos, Point(FPTGroup.PT2.LeftBPlugPos.X, FWire2.StartPos.Y)]);
  FBitmap.Canvas.Polyline([FPTGroup.PT3.LeftBPlugPos, Point(FPTGroup.PT3.LeftBPlugPos.X, FWire3.StartPos.Y)]);
  FBitmap.Canvas.Polyline([FPTGroup.PT3.RightBPlugPos, Point(FPTGroup.PT3.RightBPlugPos.X, FWire4.StartPos.Y)]);
  DgDrawConnection(FBitmap.Canvas, DgOffsetPoint(FPTGroup.PT3.RightBPlugPos, 0, 10), FPTGroup.PT1.RightBPlugPos);
  DgDrawConnection(FBitmap.Canvas, DgOffsetPoint(FPTGroup.PT3.RightBPlugPos, 0, 10), FPTGroup.PT2.RightBPlugPos);
  DgDrawJunction(FBitmap.Canvas, DgOffsetPoint(FPTGroup.PT3.RightBPlugPos, 0, 10), True);

  //PT到PT接线盒之间的连线
  //胡红明2013.5.14更改以下三句，旧代码屏蔽
  FBitmap.Canvas.Polyline([FJunctionBoxPtDown.OutPorts[0], FJunctionBoxPtUp.InPorts[0]]);
  FBitmap.Canvas.Polyline([FJunctionBoxPtDown.OutPorts[2], FJunctionBoxPtUp.InPorts[1]]);
  FBitmap.Canvas.Polyline([FJunctionBoxPtDown.OutPorts[4], FJunctionBoxPtUp.InPorts[2]]);
//  FBitmap.Canvas.Polyline([FPTGroup.PT1.LeftTPlugPos, FJunctionBoxPtUp.InPorts[0]]);
//  FBitmap.Canvas.Polyline([FPTGroup.PT2.LeftTPlugPos, FJunctionBoxPtUp.InPorts[1]]);
//  FBitmap.Canvas.Polyline([FPTGroup.PT3.LeftTPlugPos, FJunctionBoxPtUp.InPorts[2]]);

//  //PT上接线盒到电表的连线
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[0], FMeter1.GetPlugPos(dmpsUa), FMeter1.GetPlugPos(0).Y + 60);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[0], FMeter2.GetPlugPos(dmpsUa), FMeter1.GetPlugPos(0).Y + 60);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[1], FMeter1.GetPlugPos(dmpsUb), FMeter1.GetPlugPos(0).Y + 70);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[1], FMeter2.GetPlugPos(dmpsUb), FMeter1.GetPlugPos(0).Y + 70);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[2], FMeter1.GetPlugPos(dmpsUc), FMeter1.GetPlugPos(0).Y + 80);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxPtUp.OutPorts[2], FMeter2.GetPlugPos(dmpsUc), FMeter1.GetPlugPos(0).Y + 80);
  //胡红明2013.5.14更改以下一句，旧代码屏蔽
  DgDrawConnection3Y(FBitmap.Canvas, Point(FPTGroup.PT3.RightTPlugPos.X,
          FPTGroup.PT3.RightTPlugPos.Y - 10), FMeter1.GetPlugPos(dmpsUn1), FMeter1.GetPlugPos(0).Y + 90);
//  DgDrawConnection3Y(FBitmap.Canvas, FPTGroup.PT3.RightTPlugPos, FMeter1.GetPlugPos(dmpsUn1), FMeter1.GetPlugPos(0).Y + 90);

  //PT断相 胡红明2013.5.13  将几个负数改成正数
  if FWiringError.UaBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT1.LeftTPlugPos, 0, 50), DgOffsetPoint(FPTGroup.PT1.LeftTPlugPos, 0, 40));
  if FWiringError.UbBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT2.LeftTPlugPos, 0, 50), DgOffsetPoint(FPTGroup.PT2.LeftTPlugPos, 0, 40));
  if FWiringError.UcBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT3.LeftTPlugPos, 0, 50), DgOffsetPoint(FPTGroup.PT3.LeftTPlugPos, 0, 40));
  if FWiringError.UnBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT3.RightTPlugPos, 0, 50), DgOffsetPoint(FPTGroup.PT3.RightTPlugPos, 0, 40));

  //PT二次断相  胡红明2013.5.13
  if FWiringError.UsaBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT1.LeftTPlugPos, 0, -20), DgOffsetPoint(FPTGroup.PT1.LeftTPlugPos, 0, -30));
  if FWiringError.UsbBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT2.LeftTPlugPos, 0, -20), DgOffsetPoint(FPTGroup.PT2.LeftTPlugPos, 0, -30));
  if FWiringError.UscBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT3.LeftTPlugPos, 0, -20), DgOffsetPoint(FPTGroup.PT3.LeftTPlugPos, 0, -30));
  if FWiringError.UsnBroken then
    DrawBroken(DgOffsetPoint(FPTGroup.PT3.RightTPlugPos, 0, -20), DgOffsetPoint(FPTGroup.PT3.RightTPlugPos, 0, -30));
  if FWiringError.GroundBroken then
    DrawBroken(DgOffsetPoint(FPTGround.Pos, 0, -11), DgOffsetPoint(FPTGround.Pos, 0,  -1));

  //PT二次极性反 胡红明2013.5.13
  if not FWiringError.PT1Reverse then
  begin
    FJunctionBoxPtDown.Links[0].OutPortNo := 0;
    FJunctionBoxPtDown.Links[1].OutPortNo := 1;
  end
  else
  begin
    FJunctionBoxPtDown.Links[0].OutPortNo := 1;
    FJunctionBoxPtDown.Links[1].OutPortNo := 0;
  end;
  if not FWiringError.PT2Reverse then
  begin
    FJunctionBoxPtDown.Links[2].OutPortNo := 2;
    FJunctionBoxPtDown.Links[3].OutPortNo := 3;
  end
  else
  begin
    FJunctionBoxPtDown.Links[2].OutPortNo := 3;
    FJunctionBoxPtDown.Links[3].OutPortNo := 2;
  end;
  if not FWiringError.PT3Reverse then
  begin
    FJunctionBoxPtDown.Links[4].OutPortNo := 4;
    FJunctionBoxPtDown.Links[5].OutPortNo := 5;
  end
  else
  begin
    FJunctionBoxPtDown.Links[4].OutPortNo := 5;
    FJunctionBoxPtDown.Links[5].OutPortNo := 4;
  end;
  FJunctionBoxPtDown.Draw;

  //PT表尾接线
  ChangeSequence(FjunctionBoxPtUp.Links, FWiringError.USequence);
  FJunctionBoxPtUp.Draw;

  //CT接线盒连线
  ChangeSequence(FJunctionBoxCt.Links, FWiringError.ISequence);
  // 表尾电流反接
  if FWiringError.I1Reverse then
  begin
    nTemp := FJunctionBoxCt.Links[0].InPortNo;
    FJunctionBoxCt.Links[0].InPortNo:=FJunctionBoxCt.Links[3].InPortNo;
    FJunctionBoxCt.Links[3].InPortNo:=nTemp;
  end;

  if FWiringError.I2Reverse then
  begin
    nTemp := FJunctionBoxCt.Links[1].InPortNo;
    FJunctionBoxCt.Links[1].InPortNo:=FJunctionBoxCt.Links[4].InPortNo;
    FJunctionBoxCt.Links[4].InPortNo:=nTemp;
  end;

  if FWiringError.I3Reverse then
  begin
    nTemp := FJunctionBoxCt.Links[2].InPortNo;
    FJunctionBoxCt.Links[2].InPortNo:=FJunctionBoxCt.Links[5].InPortNo;
    FJunctionBoxCt.Links[5].InPortNo:=nTemp;
  end;
  FJunctionBoxCt.Draw;

  //CT极性反接
  if not FWiringError.CT1Reverse then
    FJunctionBoxCtA.SetOutPortOrder([0, 1])
  else
    FJunctionBoxCtA.SetOutPortOrder([1, 0]);
  FJunctionBoxCtA.Draw;

  if not FWiringError.CT2Reverse then
    FJunctionBoxCtB.SetOutPortOrder([0, 1])
  else
    FJunctionBoxCtB.SetOutPortOrder([1, 0]);
  FJunctionBoxCtB.Draw;

  if not FWiringError.CT3Reverse then
    FJunctionBoxCtC.SetOutPortOrder([0, 1])
  else
    FJunctionBoxCtC.SetOutPortOrder([1, 0]);
  FJunctionBoxCtC.Draw;

  //CT短路
  if FWiringError.CT1Short then
    DgDrawConnection(FBitmap.Canvas, FCT1.LeftPlugPos, FCT1.RightPlugPos);

  if FWiringError.CT2Short then
    DgDrawConnection(FBitmap.Canvas, FCT2.LeftPlugPos, FCT2.RightPlugPos);

  if FWiringError.CT3Short then
    DgDrawConnection(FBitmap.Canvas, FCT3.LeftPlugPos, FCT3.RightPlugPos);

  // 电流开路
  if FWiringError.IaBroken then
  begin
    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[0], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[0], 0, 25));
//    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[3], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[3], 0, 25));
  end;
  if FWiringError.IbBroken then
  begin
    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[1], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[1], 0, 25));
//    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[4], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[4], 0, 25));
  end;
  if FWiringError.IcBroken then
  begin
    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[2], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[2], 0, 25));
//    DrawBroken(DgOffsetPoint(FJunctionBoxCt.InPorts[5], 0, 15), DgOffsetPoint(FJunctionBoxCt.InPorts[5], 0, 25));
  end;

  //CT到CT接线盒的连线
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtA.OutPorts[0], FJunctionBoxCt.InPorts[0]);
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtB.OutPorts[0], FJunctionBoxCt.InPorts[1]);
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtC.OutPorts[0], FJunctionBoxCt.InPorts[2]);

  //CT接线盒到电表的连线, 以及电表之间的电流端子接线
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCt.OutPorts[0], FMeter1.GetPlugPos(dmpsIa_in));
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCt.OutPorts[1], FMeter1.GetPlugPos(dmpsIb_in));
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCt.OutPorts[2], FMeter1.GetPlugPos(dmpsIc_in));

//  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxCtA.OutPorts[1], FMeter2.GetPlugPos(dmpsIa_out), FJunctionBoxCt.InPorts[0].Y + 10);
//  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxCtB.OutPorts[1], FMeter2.GetPlugPos(dmpsIb_out), FJunctionBoxCt.InPorts[0].Y + 15);
//  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxCtC.OutPorts[1], FMeter2.GetPlugPos(dmpsIc_out), FJunctionBoxCt.InPorts[0].Y + 20);

  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxCt.OutPorts[3], FMeter2.GetPlugPos(dmpsIa_out), FJunctionBoxCt.OutPorts[0].Y - 20);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxCt.OutPorts[4], FMeter2.GetPlugPos(dmpsIb_out), FJunctionBoxCt.OutPorts[0].Y - 15);
  DgDrawConnection3Y(FBitmap.Canvas, FJunctionBoxCt.OutPorts[5], FMeter2.GetPlugPos(dmpsIc_out), FJunctionBoxCt.OutPorts[0].Y - 10);

  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtA.OutPorts[1], FJunctionBoxCt.InPorts[3]);
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtB.OutPorts[1], FJunctionBoxCt.InPorts[4]);
  DgDrawConnection(FBitmap.Canvas, FJunctionBoxCtC.OutPorts[1], FJunctionBoxCt.InPorts[5]);




  DgDrawConnection3Y(FBitmap.Canvas, FMeter1.GetPlugPos(dmpsIa_out), FMeter2.GetPlugPos(dmpsIa_in), FMeter1.GetPlugPos(0).Y + 20);
  DgDrawConnection3Y(FBitmap.Canvas, FMeter1.GetPlugPos(dmpsIb_out), FMeter2.GetPlugPos(dmpsIb_in), FMeter1.GetPlugPos(0).Y + 30);
  DgDrawConnection3Y(FBitmap.Canvas, FMeter1.GetPlugPos(dmpsIc_out), FMeter2.GetPlugPos(dmpsIc_in), FMeter1.GetPlugPos(0).Y + 40);

  // 电流接地断开
  if FWiringError.IaGroundBroken then
    DrawBroken( DgOffsetPoint(FCT1.RightPlugPos, 6, -10), DgOffsetPoint(FCT1.RightPlugPos, 6, -10));
  if FWiringError.IbGroundBroken then
    DrawBroken( DgOffsetPoint(FCT2.RightPlugPos, 6, -10), DgOffsetPoint(FCT2.RightPlugPos, 6, -10));
  if FWiringError.IcGroundBroken then
    DrawBroken( DgOffsetPoint(FCT3.RightPlugPos, 6, -10), DgOffsetPoint(FCT3.RightPlugPos, 6, -10));

  // 接地
  if FWiringError.IGroundBroken then
    DrawBroken(DgOffsetPoint(FCTGround.Pos, -15, -5), DgOffsetPoint(FCTGround.Pos, -15, 5));

end;

procedure TWE_Diagram2.SetDiagramType(const Value: TDiagramType);
begin
  FDiagramType := Value;
  if FDiagramType < dt4M_NoPT then
  begin
    FWiringError.PhaseType := ptThree;
    FWiringError.Clear;
    FMeter1.PhaseType := dmptThree;
    FMeter2.PhaseType := dmptThree;
  end
  else
  begin
    FWiringError.PhaseType := ptFour;
    FWiringError.Clear;
    FMeter1.PhaseType := dmptFour;
    FMeter2.PhaseType := dmptFour;
  end;
  FMeter2.Visible := FDiagramType in [dt3CTClear, dt3L4, dt4_PT_CT_CLear, dt4_PT_L6, dt4_NoPT_L6, dt4Direct];
  FMeter2.EnergyType := dmetReactive;
  FPTGroup.HasThreePT := FMeter1.PhaseType = dmptFour;
  FPTGroup.Visible := not (FDiagramType in [dt4M_NoPT, dt4_NoPT_L6, dt4Direct]);
  Invalidate;
end;

procedure TWE_Diagram2.SetWiringError(const Value: TWIRING_ERROR);
begin
  if (Value.PhaseType = ptThree) and not (FDiagramType in [dt3CTClear..dt3L4]) then
    FDiagramType := dt3L4
  else if (Value.PhaseType = ptFour) and not (FDiagramType in [dt4M_NoPT..dt4_NoPT_L6]) then
    FDiagramType := dt4_NoPT_L6
  else if (Value.PhaseType = ptFourPT) and not (FDiagramType in [dt4M_PT..dt4_PT_L6]) then
    FDiagramType := dt4_PT_L6;

  FWiringError.Assign(Value);

//  if ((FDiagramType < dt4M_PT) and (Value.PhaseType = ptThree))
//    or (Value.PhaseType = ptFour)
//    or (Value.PhaseType = ptFourPT) then  //这是新加的，胡红明2013.5.10
//    FWiringError.Assign(Value)
//  else
//    raise Exception.Create('Phase type conflict');
  Invalidate;
end;

end.
