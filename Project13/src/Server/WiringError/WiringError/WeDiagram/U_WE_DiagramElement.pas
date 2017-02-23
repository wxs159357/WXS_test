unit U_WE_DiagramElement;

interface

uses
  Classes, Windows, Graphics, SysUtils;

type
  TDgMeterPhaseType = (dmptThree, dmptFour);
  TDgMeterEnergyType = (dmetActive, dmetReactive);
  TDgMeterPlugSign = (dmpsIa_in, dmpsUa, dmpsIa_out, dmpsIb_in, dmpsUb, dmpsIb_out,
    dmpsIc_in, dmpsUc, dmpsIc_out, dmpsUn1, dmpsUn2);

type
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
    property PhaseType: TDgMeterPhaseType read FPhaseType write FPhaseType;
    property EnergyType: TdgMeterEnergyType read FEnergyType write FEnergyType;
    property PlugCount: Integer read GetPlugCount;
    property Visible: Boolean read FVisible write FVisible default True;
  end;

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

  TDgJunctionBox = class
  private
    FCanvas: TCanvas;
    FInPorts: TPointArray;
    FOutPorts: TPointArray;
    FLinks: TDgLinkArray;
    FVisible: Boolean;
    procedure SetLinkCount(const Value: Integer);
    function GetLinkCount: Integer;
    function GetPortCount: Integer;
    procedure SetPortCount(const Value: Integer);
  public
    constructor Create(ACanvas: TCanvas; APortCount: Integer);
    procedure SetOutPortOrder(AOrderArray: array of Integer);
    procedure Draw;
  public
    property Canvas: TCanvas read FCanvas write FCanvas;
    property InPorts: TPointArray read FInPorts;
    property OutPorts: TPointArray read FOutPorts;
    property PortCount: Integer read GetPortCount write SetPortCount;
    property Links: TDgLinkArray read FLinks;
    property LinkCount: Integer read GetLinkCount write SetLinkCount;
    property Visible: Boolean read FVisible write FVisible default True;
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
begin
  for I := 0 to PlugCount - 1 do
    FCanvas.Rectangle(Rect(DgOffsetPoint(GetPlugPos(I), -3, -14), DgOffsetPoint(GetPlugPos(I), 3, 1)));
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
  DgDrawWinding(FCanvas, Rect(FWindingPos, DgOffsetPoint(FWindingPos, FWindingWidth, FWindingWidth div 4)), True);
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

end.





