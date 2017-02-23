unit U_WE_DIAGRAM;

interface

uses
  Windows, Graphics, Classes, SysUtils, U_WE_DiagramElement, U_WIRING_ERROR;

type
  TDiagramType = (
    dt3CTClear,
    dt3M,
    dt3L4,

    dt4M_NoPT,
    dt4M_PT,
    dt4_PT_CT_CLear,
    dt4_PT_L6,
    dt4_NoPT_L6,
    dt4Direct
    );

function DiagramTypeToStr(ADiagramType: TDiagramType): string;

type
  TWE_DIAGRAM = class
  private
    FCanvas: TCanvas;
    FPos: TPoint;
    FDiagramType: TDiagramType;
    FWiringError: TWIRING_ERROR;

    FMeter1, FMeter2: TDgMeter;
    FCT1, FCT2, FCT3: TDgCT;
    FPTGroup: TDgPTGroup;
    FCTGround, FPTGround: TDgGround;
    FWire1, FWire2, FWire3, FWire4: TDgWire;
    FJunctionBoxCt: TDgJunctionBox; //CT接线盒
    FJunctionBoxCtA, FJunctionBoxCtB, FJunctionBoxCtC: TDgJunctionBox; //CT接线盒
    FJunctionBoxPtDown: TDgJunctionBox;      //PT下接线盒
    FJunctionBoxPtUp: TDgJunctionBox;        //PT上接线盒

    ptUbUp, ptUbDown: TPoint;

    procedure ClearCanvas;
    procedure AdjustElementLayout;
    procedure DrawBroken(APosStart, APosEnd: TPoint);
    procedure ChangeSequence(ALinks: TDgLinkArray; ASequence: TWE_SEQUENCE_TYPE);
    procedure DrawAllElement;
    procedure DrawType3CTClear;
    procedure DrawType3M;
    procedure DrawType3L4;
    procedure DrawType4M_NoPT;
    procedure DrawType4M_PT;
    procedure DrawType4_PT_CT_CLear;
    procedure DrawType4_PT_L6;
    procedure DrawType4_NoPT_L6;
    procedure DrawType4Direct;

    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetDiagramType(const Value: TDiagramType);
    procedure SetWiringError(const Value: TWIRING_ERROR);
  public
    constructor Create(ACanvas: TCanvas);
    destructor Destroy; override;
    procedure Draw;
  public
    property Canvas: TCanvas read FCanvas write FCanvas;
    property Pos: TPoint  read FPos write FPos;
    property DiagramType: TDiagramType read FDiagramType write SetDiagramType;
    property WiringError: TWIRING_ERROR read FWiringError write SetWiringError;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
  end;

implementation

function DiagramTypeToStr(ADiagramType: TDiagramType): string;
begin
  case ADiagramType of
    dt3CTClear:      Result := '三相三线CT简化';
    dt3M:            Result := '三相三线多功能';
    dt3L4:           Result := '三相三线四线制';

    dt4M_NoPT:       Result := '三相四线多功能无PT';
    dt4M_PT:         Result := '三相四线多功能有PT';
    dt4_PT_CT_CLear: Result := '三相四线经PT,CT简化';
    dt4_PT_L6:       Result := '三相四线经PT六线制';
    dt4_NoPT_L6:     Result := '三相四线无PT六线制';
    dt4Direct:       Result := '三相四线直通';
  else
    raise Exception.Create('Unrecognized');
  end;
end;

{ TDgMeter }

procedure TWE_DIAGRAM.AdjustElementLayout;
var
  I: Integer;
begin
  FMeter1.Pos := Point(FPos.X + 120, FPos.Y + 6);
  FMeter2.Pos := DgOffsetPoint(FMeter1.Pos, FMeter1.Width + 20, 0);

  FWire1.StartPos := Point(FPos.X + 20, FPos.Y + 350);
  FWire2.StartPos := DgOffsetPoint(FWire1.StartPos, 0, 30);
  FWire3.StartPos := DgOffsetPoint(FWire2.StartPos, 0, 30);
  FWire4.StartPos := DgOffsetPoint(FWire3.StartPos, 0, 30);
  FWire1.Length := Width - 23;
  FWire2.Length := Width - 23;
  FWire3.Length := Width - 23;
  FWire4.Length := Width - 23;

  FCT1.WindingPos := Point(FMeter1.GetPlugPos(dmpsIa_in).X, FWire1.StartPos.Y);
  FCT2.Visible := FMeter1.PhaseType = dmptFour;
  if FCT2.Visible then
    FCT2.WindingPos := Point(FMeter1.GetPlugPos(dmpsIb_in).X, FWire2.StartPos.Y);
  FCT3.WindingPos := Point(FMeter1.GetPlugPos(dmpsIc_in).X, FWire3.StartPos.Y);
  FCTGround.Pos := Point(FCT3.RightPlugPos.X + 80, FWire1.StartPos.Y - 20);

  FPTGroup.Pos := Point(FWire1.StartPos.X + 30, FWire1.StartPos.Y - 60);
  if FPTGroup.HasThreePT then
    FPTGround.Pos := DgOffsetPoint(FPTGroup.PT3.RightTPlugPos, 18, 6)
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

    dt4_PT_CT_CLear, dt4M_NoPT, dt4M_PT, dt4_PT_L6, dt4_NoPT_L6:
    begin
      FJunctionBoxCt.PortCount := 3;
      FJunctionBoxCt.LinkCount := 3;
      FJunctionBoxCt.InPorts[0] := Point(FCT1.LeftPlugPos.X, FWire1.StartPos.Y - 50);
      FJunctionBoxCt.InPorts[1] := Point(FCT2.LeftPlugPos.X, FWire1.StartPos.Y - 50);
      FJunctionBoxCt.InPorts[2] := Point(FCT3.LeftPlugPos.X, FWire1.StartPos.Y - 50);
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
    FJunctionBoxCt.OutPorts[I] := DgOffsetPoint(FJunctionBoxCt.InPorts[I], 0, -30);

  FJunctionBoxPtDown.InPorts[0] := FPTGroup.PT1.LeftTPlugPos;
  FJunctionBoxPtDown.InPorts[1] := FPTGroup.PT1.RightTPlugPos;
  FJunctionBoxPtDown.InPorts[2] := FPTGroup.PT2.LeftTPlugPos;
  FJunctionBoxPtDown.InPorts[3] := FPTGroup.PT2.RightTPlugPos;
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

procedure TWE_DIAGRAM.ChangeSequence(ALinks: TDgLinkArray;
  ASequence: TWE_SEQUENCE_TYPE);
  procedure SetInPortOrder(APortNo0, APortNo1, APortNo2: Integer);
  begin
    ALinks[0].InPortNo := APortNo0;
    ALinks[1].InPortNo := APortNo1;
    ALinks[2].InPortNo := APortNo2;
  end;
var
  I: Integer;
begin
  if Length(ALinks) <> 3 then
    raise Exception.Create('Errors');

  for I := 0 to 2 do
    ALinks[I].OutPortNo := I;

  case ASequence of
    stABC: SetInPortOrder(0, 1, 2);
    stACB: SetInPortOrder(0, 2, 1);
    stBAC: SetInPortOrder(1, 0, 2);
    stBCA: SetInPortOrder(1, 2, 0);
    stCAB: SetInPortOrder(2, 0, 1);
    stCBA: SetInPortOrder(2, 1, 0);
  end;
end;

procedure TWE_DIAGRAM.ClearCanvas;
begin
  FCanvas.Brush.Style := bsSolid;
  FCanvas.Brush.Color := $00FEF8ED;
  FCanvas.FillRect(Rect(FPos, Point(FPos.X + Width, FPos.Y + Height)));
end;

constructor TWE_DIAGRAM.Create(ACanvas: TCanvas);
begin
  FCanvas := ACanvas;
  FPos := Point(0, 0);
  FWiringError := TWIRING_ERROR.Create;

  FMeter1 := TDgMeter.Create(FCanvas);
  FMeter2 := TDgMeter.Create(FCanvas);

  FWire1 := TDgWire.Create(FCanvas);
  FWire2 := TDgWire.Create(FCanvas);
  FWire3 := TDgWire.Create(FCanvas);
  FWire4 := TDgWire.Create(FCanvas);
  FWire1.Text := 'A';
  FWire2.Text := 'B';
  FWire3.Text := 'C';
  FWire4.Text := 'N';

  FCT1 := TDgCT.Create(FCanvas);
  FCT2 := TDgCT.Create(FCanvas);
  FCT3 := TDgCT.Create(FCanvas);
  FPTGroup := TDgPTGroup.Create(FCanvas);
  FCTGround := TDgGround.Create(FCanvas);
  FPTGround := TDgGround.Create(FCanvas);

  FJunctionBoxCt := TDgJunctionBox.Create(FCanvas, 4);
  FJunctionBoxCtA := TDgJunctionBox.Create(FCanvas, 2);
  FJunctionBoxCtB := TDgJunctionBox.Create(FCanvas, 2);
  FJunctionBoxCtC := TDgJunctionBox.Create(FCanvas, 2);
  FJunctionBoxPtDown := TDgJunctionBox.Create(FCanvas, 4);
  FJunctionBoxPtUp := TDgJunctionBox.Create(FCanvas, 3);

  SetDiagramType(dt3CTClear);
  AdjustElementLayout;
end;

destructor TWE_DIAGRAM.Destroy;
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
  inherited;
end;

procedure TWE_DIAGRAM.Draw;
begin
  AdjustElementLayout;
  ClearCanvas;
  DrawAllElement;

  case FDiagramType of
    dt3CTClear: DrawType3CTClear;
    dt3M: DrawType3M;
    dt3L4: DrawType3L4;
    dt4M_NoPT: DrawType4M_NoPT;
    dt4M_PT: DrawType4M_PT;
    dt4_PT_CT_CLear: DrawType4_PT_CT_CLear;
    dt4_PT_L6: DrawType4_PT_L6;
    dt4_NoPT_L6: DrawType4_NoPT_L6;
    dt4Direct: DrawType4Direct;
  end;
end;

procedure TWE_DIAGRAM.DrawAllElement;
begin
  FMeter1.Draw;
  FMeter2.Draw;
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
    DgDrawConnection(FCanvas, FCTGround.Pos, DgOffsetPoint(FCT1.RightPlugPos, 6, -6));
    DgDrawConnection(FCanvas, DgOffsetPoint(FCT1.RightPlugPos, 6, -6), FCT1.RightPlugPos, True);
  end;
  if FCT2.Visible then
  begin
    DgDrawConnection(FCanvas, FCTGround.Pos, DgOffsetPoint(FCT2.RightPlugPos, 6, -6));
    DgDrawConnection(FCanvas, DgOffsetPoint(FCT2.RightPlugPos, 6, -6), FCT2.RightPlugPos, True);
    DgDrawJunction(FCanvas, Point(FCT2.RightPlugPos.X + 6, FCTGround.Pos.Y), True);
  end;
  if FCT3.Visible then
  begin
    DgDrawConnection(FCanvas, FCTGround.Pos, DgOffsetPoint(FCT3.RightPlugPos, 6, -6));
    DgDrawConnection(FCanvas, DgOffsetPoint(FCT3.RightPlugPos, 6, -6), FCT3.RightPlugPos, True);
    DgDrawJunction(FCanvas, Point(FCT3.RightPlugPos.X + 6, FCTGround.Pos.Y), True);
  end;

  if FPTGroup.Visible and FPTGround.Visible then
  begin
    FPTGround.Draw;
    if FMeter1.PhaseType = dmptThree then
    begin
      DgDrawConnection(FCanvas, DgOffsetPoint(ptUbUp, 0, -5), FPTGround.Pos);
      DgDrawJunction(FCanvas, DgOffsetPoint(ptUbUp, 0, -5), True);
    end
    else
    begin
      DgDrawConnection3Y(FCanvas, FPTGroup.PT1.RightTPlugPos, FPTGround.Pos, FPTGroup.PT1.RightTPlugPos.Y - 10);
      DgDrawConnection3Y(FCanvas, FPTGroup.PT2.RightTPlugPos, FPTGround.Pos, FPTGroup.PT1.RightTPlugPos.Y - 10);
      DgDrawConnection3Y(FCanvas, FPTGroup.PT3.RightTPlugPos, FPTGround.Pos, FPTGroup.PT1.RightTPlugPos.Y - 10);
      DgDrawJunction(FCanvas, DgOffsetPoint(FPTGroup.PT3.RightTPlugPos, 0, -10), True);
    end;
  end;
end;

procedure TWE_DIAGRAM.DrawBroken(APosStart, APosEnd: TPoint);
var
  pO : TPoint;
  OldColor: TColor;
begin
  pO.X := (APosStart.X + APosEnd.X) div 2;
  pO.Y := (APosStart.Y + APosEnd.Y) div 2;

  with FCanvas do
  begin
    OldColor := Pen.Color;
    // 清连接线
    Pen.Color := Pixels[FPos.X, FPos.Y];
    Polyline([ APosStart, APosEnd ] );

    // 画 X
    Pen.Color := clRed;
    Polyline([Point(po.X - 4, pO.Y - 4),Point(pO.X + 5, pO.Y + 5)]);
    Polyline([Point(po.X - 4, pO.Y + 4),Point(pO.X + 5, pO.Y - 5)]);
    Pen.Color := OldColor;
  end;
end;

procedure TWE_DIAGRAM.DrawType3CTClear;
var
  I: Integer;
  ptMeterOutPut: TPoint;
begin
  //外部电线到PT的连线
  FCanvas.Polyline([FPTGroup.PT1.LeftBPlugPos, Point(FPTGroup.PT1.LeftBPlugPos.X, FWire1.StartPos.Y)]);
  FCanvas.Polyline([FPTGroup.PT2.RightBPlugPos, Point(FPTGroup.PT2.RightBPlugPos.X, FWire3.StartPos.Y)]);
  FCanvas.Polyline([ptUbDown, Point(ptUbDown.X, FWire2.StartPos.Y)]);
  DgDrawConnection(FCanvas, ptUbDown, FPTGroup.PT1.RightBPlugPos);
  DgDrawConnection(FCanvas, ptUbDown, FPTGroup.PT2.LeftBPlugPos);

  //两个PT接线盒之间的连线
  FCanvas.Polyline([FJunctionBoxPtDown.OutPorts[1], DgOffsetPoint(FJunctionBoxPtDown.OutPorts[2], 1, 0)]);
  FCanvas.Polyline([FJunctionBoxPtDown.OutPorts[0], FJunctionBoxPtUp.InPorts[0]]);
  FCanvas.Polyline([ptUbUp, FJunctionBoxPtUp.InPorts[1]]);
  FCanvas.Polyline([FJunctionBoxPtDown.OutPorts[3], FJunctionBoxPtUp.InPorts[2]]);

  //PT上接线盒到电表的连线
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[0], FMeter1.GetPlugPos(dmpsUa), FMeter1.GetPlugPos(0).Y + 60);
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[0], FMeter2.GetPlugPos(dmpsUa), FMeter1.GetPlugPos(0).Y + 60);
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[1], FMeter1.GetPlugPos(dmpsUb), FMeter1.GetPlugPos(0).Y + 70);
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[1], FMeter2.GetPlugPos(dmpsUb), FMeter1.GetPlugPos(0).Y + 70);
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[2], FMeter1.GetPlugPos(dmpsUc), FMeter1.GetPlugPos(0).Y + 80);
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[2], FMeter2.GetPlugPos(dmpsUc), FMeter1.GetPlugPos(0).Y + 80);

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
  for I := 0 to 2 do
    FJunctionBoxCt.Links[I].OutPortNo := I;
  case FWiringError.I1In of
    plA: FJunctionBoxCt.Links[0].InPortNo := 0;
    plC: FJunctionBoxCt.Links[0].InPortNo := 1;
    plN: FJunctionBoxCt.Links[0].InPortNo := 2;
  end;
  case FWiringError.I2In of
    plA: FJunctionBoxCt.Links[1].InPortNo := 0;
    plC: FJunctionBoxCt.Links[1].InPortNo := 1;
    plN: FJunctionBoxCt.Links[1].InPortNo := 2;
  end;
  case FWiringError.I2Out of
    plA: FJunctionBoxCt.Links[2].InPortNo := 0;
    plC: FJunctionBoxCt.Links[2].InPortNo := 1;
    plN: FJunctionBoxCt.Links[2].InPortNo := 2;
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
    DgDrawConnection(FCanvas, FCT1.LeftPlugPos, FCT1.RightPlugPos);

  if FWiringError.CT2Short then
    DgDrawConnection(FCanvas, FCT3.LeftPlugPos, FCT3.RightPlugPos);

  //CT到CT接线盒的连线
  DgDrawConnection(FCanvas, FJunctionBoxCtA.OutPorts[0], FJunctionBoxCt.InPorts[0]);
  DgDrawConnection(FCanvas, FJunctionBoxCtA.OutPorts[1], FJunctionBoxCt.InPorts[2]);
  DgDrawConnection(FCanvas, FJunctionBoxCtC.OutPorts[0], FJunctionBoxCt.InPorts[1]);
  DgDrawConnection(FCanvas, FJunctionBoxCtC.OutPorts[1], FJunctionBoxCt.InPorts[2]);

  //CT接线盒到电表的连线, 以及电表之间的电流端子接线
  ptMeterOutPut.X := (FMeter2.GetPlugPos(dmpsIa_out).X + FMeter2.GetPlugPos(dmpsIc_out).X) div 2;
  ptMeterOutPut.Y := FMeter2.GetPlugPos(dmpsIa_out).Y + 50;
  DgDrawConnection3Y(FCanvas, FJunctionBoxCt.OutPorts[0], FMeter1.GetPlugPos(dmpsIa_in), FJunctionBoxCt.OutPorts[0].Y - 40);
  DgDrawConnection3Y(FCanvas, FJunctionBoxCt.OutPorts[1], FMeter1.GetPlugPos(dmpsIc_in), FJunctionBoxCt.OutPorts[0].Y - 30);
  DgDrawConnection3Y(FCanvas, FJunctionBoxCt.OutPorts[2], ptMeterOutPut, FJunctionBoxCt.OutPorts[0].Y - 25);
  DgDrawConnection3Y(FCanvas, FMeter2.GetPlugPos(dmpsIa_out), FMeter2.GetPlugPos(dmpsIc_out), ptMeterOutPut.Y);
  DgDrawConnection3Y(FCanvas, FMeter1.GetPlugPos(dmpsIa_out), FMeter2.GetPlugPos(dmpsIa_in), FMeter1.GetPlugPos(0).Y + 20);
  DgDrawConnection3Y(FCanvas, FMeter1.GetPlugPos(dmpsIc_out), FMeter2.GetPlugPos(dmpsIc_in), FMeter1.GetPlugPos(0).Y + 30);
end;

procedure TWE_DIAGRAM.DrawType3L4;
var
  I: Integer;
begin
  //外部电线到PT的连线
  FCanvas.Polyline([FPTGroup.PT1.LeftBPlugPos, Point(FPTGroup.PT1.LeftBPlugPos.X, FWire1.StartPos.Y)]);
  FCanvas.Polyline([FPTGroup.PT2.RightBPlugPos, Point(FPTGroup.PT2.RightBPlugPos.X, FWire3.StartPos.Y)]);
  FCanvas.Polyline([ptUbDown, Point(ptUbDown.X, FWire2.StartPos.Y)]);
  DgDrawConnection(FCanvas, ptUbDown, FPTGroup.PT1.RightBPlugPos);
  DgDrawConnection(FCanvas, ptUbDown, FPTGroup.PT2.LeftBPlugPos);

  //两个PT接线盒之间的连线
  FCanvas.Polyline([FJunctionBoxPtDown.OutPorts[1], DgOffsetPoint(FJunctionBoxPtDown.OutPorts[2], 1, 0)]);
  FCanvas.Polyline([FJunctionBoxPtDown.OutPorts[0], FJunctionBoxPtUp.InPorts[0]]);
  FCanvas.Polyline([ptUbUp, FJunctionBoxPtUp.InPorts[1]]);
  FCanvas.Polyline([FJunctionBoxPtDown.OutPorts[3], FJunctionBoxPtUp.InPorts[2]]);

  //PT上接线盒到电表的连线
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[0], FMeter1.GetPlugPos(dmpsUa), FMeter1.GetPlugPos(0).Y + 60);
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[0], FMeter2.GetPlugPos(dmpsUa), FMeter1.GetPlugPos(0).Y + 60);
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[1], FMeter1.GetPlugPos(dmpsUb), FMeter1.GetPlugPos(0).Y + 70);
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[1], FMeter2.GetPlugPos(dmpsUb), FMeter1.GetPlugPos(0).Y + 70);
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[2], FMeter1.GetPlugPos(dmpsUc), FMeter1.GetPlugPos(0).Y + 80);
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[2], FMeter2.GetPlugPos(dmpsUc), FMeter1.GetPlugPos(0).Y + 80);

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
    DgDrawConnection(FCanvas, FCT1.LeftPlugPos, FCT1.RightPlugPos);

  if FWiringError.CT2Short then
    DgDrawConnection(FCanvas, FCT3.LeftPlugPos, FCT3.RightPlugPos);

  //CT到CT接线盒的连线
  DgDrawConnection(FCanvas, FJunctionBoxCtA.OutPorts[0], FJunctionBoxCt.InPorts[0]);
  DgDrawConnection(FCanvas, FJunctionBoxCtA.OutPorts[1], FJunctionBoxCt.InPorts[1]);
  DgDrawConnection(FCanvas, FJunctionBoxCtC.OutPorts[0], FJunctionBoxCt.InPorts[2]);
  DgDrawConnection(FCanvas, FJunctionBoxCtC.OutPorts[1], FJunctionBoxCt.InPorts[3]);

  //CT接线盒到电表的连线, 以及电表之间的电流端子接线
  DgDrawConnection3Y(FCanvas, FJunctionBoxCt.OutPorts[0], FMeter1.GetPlugPos(dmpsIa_in), FJunctionBoxCt.OutPorts[0].Y - 40);
  DgDrawConnection3Y(FCanvas, FJunctionBoxCt.OutPorts[1], FMeter2.GetPlugPos(dmpsIa_out), FJunctionBoxCt.OutPorts[0].Y - 35);
  DgDrawConnection3Y(FCanvas, FJunctionBoxCt.OutPorts[2], FMeter1.GetPlugPos(dmpsIc_in), FJunctionBoxCt.OutPorts[0].Y - 30);
  DgDrawConnection3Y(FCanvas, FJunctionBoxCt.OutPorts[3], FMeter2.GetPlugPos(dmpsIc_out), FJunctionBoxCt.OutPorts[0].Y - 25);
  DgDrawConnection3Y(FCanvas, FMeter1.GetPlugPos(dmpsIa_out), FMeter2.GetPlugPos(dmpsIa_in), FMeter1.GetPlugPos(0).Y + 20);
  DgDrawConnection3Y(FCanvas, FMeter1.GetPlugPos(dmpsIc_out), FMeter2.GetPlugPos(dmpsIc_in), FMeter1.GetPlugPos(0).Y + 30);
end;

procedure TWE_DIAGRAM.DrawType3M;
var
  I: Integer;
begin
  //外部电线到PT的连线
  FCanvas.Polyline([FPTGroup.PT1.LeftBPlugPos, Point(FPTGroup.PT1.LeftBPlugPos.X, FWire1.StartPos.Y)]);
  FCanvas.Polyline([FPTGroup.PT2.RightBPlugPos, Point(FPTGroup.PT2.RightBPlugPos.X, FWire3.StartPos.Y)]);
  FCanvas.Polyline([ptUbDown, Point(ptUbDown.X, FWire2.StartPos.Y)]);
  DgDrawConnection(FCanvas, ptUbDown, FPTGroup.PT1.RightBPlugPos);
  DgDrawConnection(FCanvas, ptUbDown, FPTGroup.PT2.LeftBPlugPos);

  //两个PT接线盒之间的连线
  FCanvas.Polyline([FJunctionBoxPtDown.OutPorts[1], DgOffsetPoint(FJunctionBoxPtDown.OutPorts[2], 1, 0)]);
  FCanvas.Polyline([FJunctionBoxPtDown.OutPorts[0], FJunctionBoxPtUp.InPorts[0]]);
  FCanvas.Polyline([ptUbUp, FJunctionBoxPtUp.InPorts[1]]);
  FCanvas.Polyline([FJunctionBoxPtDown.OutPorts[3], FJunctionBoxPtUp.InPorts[2]]);

  //PT上接线盒到电表的连线
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[0], FMeter1.GetPlugPos(dmpsUa), FMeter1.GetPlugPos(0).Y + 60);
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[1], FMeter1.GetPlugPos(dmpsUb), FMeter1.GetPlugPos(0).Y + 70);
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[2], FMeter1.GetPlugPos(dmpsUc), FMeter1.GetPlugPos(0).Y + 80);

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
    DgDrawConnection(FCanvas, FCT1.LeftPlugPos, FCT1.RightPlugPos);

  if FWiringError.CT2Short then
    DgDrawConnection(FCanvas, FCT3.LeftPlugPos, FCT3.RightPlugPos);

  //CT到CT接线盒的连线
  DgDrawConnection(FCanvas, FJunctionBoxCtA.OutPorts[0], FJunctionBoxCt.InPorts[0]);
  DgDrawConnection(FCanvas, FJunctionBoxCtA.OutPorts[1], FJunctionBoxCt.InPorts[1]);
  DgDrawConnection(FCanvas, FJunctionBoxCtC.OutPorts[0], FJunctionBoxCt.InPorts[2]);
  DgDrawConnection(FCanvas, FJunctionBoxCtC.OutPorts[1], FJunctionBoxCt.InPorts[3]);

  //CT接线盒到电表的连线
  DgDrawConnection3Y(FCanvas, FJunctionBoxCt.OutPorts[0], FMeter1.GetPlugPos(dmpsIa_in), FJunctionBoxCt.OutPorts[0].Y - 40);
  DgDrawConnection3Y(FCanvas, FJunctionBoxCt.OutPorts[1], FMeter1.GetPlugPos(dmpsIa_out), FJunctionBoxCt.OutPorts[0].Y - 35);
  DgDrawConnection3Y(FCanvas, FJunctionBoxCt.OutPorts[2], FMeter1.GetPlugPos(dmpsIc_in), FJunctionBoxCt.OutPorts[0].Y - 30);
  DgDrawConnection3Y(FCanvas, FJunctionBoxCt.OutPorts[3], FMeter1.GetPlugPos(dmpsIc_out), FJunctionBoxCt.OutPorts[0].Y - 25);
end;

procedure TWE_DIAGRAM.DrawType4Direct;
begin
  //外部电线到接线盒的连线
  FCanvas.Polyline([FJunctionBoxPtUp.InPorts[0], Point(FJunctionBoxPtUp.InPorts[0].X, FWire1.StartPos.Y)]);
  FCanvas.Polyline([FJunctionBoxPtUp.InPorts[1], Point(FJunctionBoxPtUp.InPorts[1].X, FWire2.StartPos.Y)]);
  FCanvas.Polyline([FJunctionBoxPtUp.InPorts[2], Point(FJunctionBoxPtUp.InPorts[2].X, FWire3.StartPos.Y)]);

  //PT上接线盒到电表的连线
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[0], FMeter1.GetPlugPos(dmpsIa_in), FMeter1.GetPlugPos(0).Y + 60);
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[1], FMeter1.GetPlugPos(dmpsIb_in), FMeter1.GetPlugPos(0).Y + 70);
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[2], FMeter1.GetPlugPos(dmpsIc_in), FMeter1.GetPlugPos(0).Y + 80);

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

  DgDrawConnection(FCanvas, Point(FWire1.BreakXPos2, FWire1.StartPos.Y), FMeter2.GetPlugPos(dmpsIa_out));
  DgDrawConnection(FCanvas, Point(FWire2.BreakXPos2, FWire2.StartPos.Y), FMeter2.GetPlugPos(dmpsIb_out));
  DgDrawConnection(FCanvas, Point(FWire3.BreakXPos2, FWire3.StartPos.Y), FMeter2.GetPlugPos(dmpsIc_out));
  FCanvas.Polyline([FMeter1.GetPlugPos(dmpsUn1), Point(FMeter1.GetPlugPos(dmpsUn1).X, FWire4.StartPos.Y)]);

  DgDrawConnection3Y(FCanvas, FMeter1.GetPlugPos(dmpsIa_out), FMeter2.GetPlugPos(dmpsIa_in), FMeter1.GetPlugPos(0).Y + 20);
  DgDrawConnection3Y(FCanvas, FMeter1.GetPlugPos(dmpsIb_out), FMeter2.GetPlugPos(dmpsIb_in), FMeter1.GetPlugPos(0).Y + 30);
  DgDrawConnection3Y(FCanvas, FMeter1.GetPlugPos(dmpsIc_out), FMeter2.GetPlugPos(dmpsIc_in), FMeter1.GetPlugPos(0).Y + 40);
end;

procedure TWE_DIAGRAM.DrawType4M_NoPT;
begin
  //外部电线到接线盒的连线
  FCanvas.Polyline([FJunctionBoxPtUp.InPorts[0], Point(FJunctionBoxPtUp.InPorts[0].X, FWire1.StartPos.Y)]);
  FCanvas.Polyline([FJunctionBoxPtUp.InPorts[1], Point(FJunctionBoxPtUp.InPorts[1].X, FWire2.StartPos.Y)]);
  FCanvas.Polyline([FJunctionBoxPtUp.InPorts[2], Point(FJunctionBoxPtUp.InPorts[2].X, FWire3.StartPos.Y)]);

  //PT上接线盒到电表的连线
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[0], FMeter1.GetPlugPos(dmpsUa), FMeter1.GetPlugPos(0).Y + 60);
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[1], FMeter1.GetPlugPos(dmpsUb), FMeter1.GetPlugPos(0).Y + 70);
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[2], FMeter1.GetPlugPos(dmpsUc), FMeter1.GetPlugPos(0).Y + 80);
  DgDrawConnection3Y(FCanvas, Point(FPTGroup.PT3.RightTPlugPos.X, FWire4.StartPos.Y), FMeter1.GetPlugPos(dmpsUn1), FMeter1.GetPlugPos(0).Y + 90);

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
    DgDrawConnection(FCanvas, FCT1.LeftPlugPos, FCT1.RightPlugPos);

  if FWiringError.CT2Short then
    DgDrawConnection(FCanvas, FCT2.LeftPlugPos, FCT2.RightPlugPos);

  if FWiringError.CT3Short then
    DgDrawConnection(FCanvas, FCT3.LeftPlugPos, FCT3.RightPlugPos);

  //CT到CT接线盒的连线
  DgDrawConnection(FCanvas, FJunctionBoxCtA.OutPorts[0], FJunctionBoxCt.InPorts[0]);
  DgDrawConnection(FCanvas, FJunctionBoxCtB.OutPorts[0], FJunctionBoxCt.InPorts[1]);
  DgDrawConnection(FCanvas, FJunctionBoxCtC.OutPorts[0], FJunctionBoxCt.InPorts[2]);

  //CT接线盒到电表的连线, 以及电表之间的电流端子接线
  DgDrawConnection(FCanvas, FJunctionBoxCt.OutPorts[0], FMeter1.GetPlugPos(dmpsIa_in));
  DgDrawConnection(FCanvas, FJunctionBoxCt.OutPorts[1], FMeter1.GetPlugPos(dmpsIb_in));
  DgDrawConnection(FCanvas, FJunctionBoxCt.OutPorts[2], FMeter1.GetPlugPos(dmpsIc_in));
  DgDrawConnection(FCanvas, FJunctionBoxCtA.OutPorts[1], FMeter1.GetPlugPos(dmpsIa_out));
  DgDrawConnection(FCanvas, FJunctionBoxCtB.OutPorts[1], FMeter1.GetPlugPos(dmpsIb_out));
  DgDrawConnection(FCanvas, FJunctionBoxCtC.OutPorts[1], FMeter1.GetPlugPos(dmpsIc_out));
end;

procedure TWE_DIAGRAM.DrawType4M_PT;
begin
  //外部电线到PT的连线
  FCanvas.Polyline([FPTGroup.PT1.LeftBPlugPos, Point(FPTGroup.PT1.LeftBPlugPos.X, FWire1.StartPos.Y)]);
  FCanvas.Polyline([FPTGroup.PT2.LeftBPlugPos, Point(FPTGroup.PT2.LeftBPlugPos.X, FWire2.StartPos.Y)]);
  FCanvas.Polyline([FPTGroup.PT3.LeftBPlugPos, Point(FPTGroup.PT3.LeftBPlugPos.X, FWire3.StartPos.Y)]);
  FCanvas.Polyline([FPTGroup.PT3.RightBPlugPos, Point(FPTGroup.PT3.RightBPlugPos.X, FWire3.StartPos.Y)]);
  DgDrawConnection(FCanvas, DgOffsetPoint(FPTGroup.PT3.RightBPlugPos, 0, 10), FPTGroup.PT1.RightBPlugPos);
  DgDrawConnection(FCanvas, DgOffsetPoint(FPTGroup.PT3.RightBPlugPos, 0, 10), FPTGroup.PT2.RightBPlugPos);

//  //PT到PT接线盒之间的连线
  FCanvas.Polyline([FPTGroup.PT1.LeftTPlugPos, FJunctionBoxPtUp.InPorts[0]]);
  FCanvas.Polyline([FPTGroup.PT2.LeftTPlugPos, FJunctionBoxPtUp.InPorts[1]]);
  FCanvas.Polyline([FPTGroup.PT3.LeftTPlugPos, FJunctionBoxPtUp.InPorts[2]]);

//  //PT上接线盒到电表的连线
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[0], FMeter1.GetPlugPos(dmpsUa), FMeter1.GetPlugPos(0).Y + 60);
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[1], FMeter1.GetPlugPos(dmpsUb), FMeter1.GetPlugPos(0).Y + 70);
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[2], FMeter1.GetPlugPos(dmpsUc), FMeter1.GetPlugPos(0).Y + 80);
  DgDrawConnection3Y(FCanvas, FPTGroup.PT3.RightTPlugPos, FMeter1.GetPlugPos(dmpsUn1), FMeter1.GetPlugPos(0).Y + 90);

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
    DgDrawConnection(FCanvas, FCT1.LeftPlugPos, FCT1.RightPlugPos);

  if FWiringError.CT2Short then
    DgDrawConnection(FCanvas, FCT2.LeftPlugPos, FCT2.RightPlugPos);

  if FWiringError.CT3Short then
    DgDrawConnection(FCanvas, FCT3.LeftPlugPos, FCT3.RightPlugPos);

  //CT到CT接线盒的连线
  DgDrawConnection(FCanvas, FJunctionBoxCtA.OutPorts[0], FJunctionBoxCt.InPorts[0]);
  DgDrawConnection(FCanvas, FJunctionBoxCtB.OutPorts[0], FJunctionBoxCt.InPorts[1]);
  DgDrawConnection(FCanvas, FJunctionBoxCtC.OutPorts[0], FJunctionBoxCt.InPorts[2]);

  //CT接线盒到电表的连线, 以及电表之间的电流端子接线
  DgDrawConnection(FCanvas, FJunctionBoxCt.OutPorts[0], FMeter1.GetPlugPos(dmpsIa_in));
  DgDrawConnection(FCanvas, FJunctionBoxCt.OutPorts[1], FMeter1.GetPlugPos(dmpsIb_in));
  DgDrawConnection(FCanvas, FJunctionBoxCt.OutPorts[2], FMeter1.GetPlugPos(dmpsIc_in));
  DgDrawConnection(FCanvas, FJunctionBoxCtA.OutPorts[1], FMeter1.GetPlugPos(dmpsIa_out));
  DgDrawConnection(FCanvas, FJunctionBoxCtB.OutPorts[1], FMeter1.GetPlugPos(dmpsIb_out));
  DgDrawConnection(FCanvas, FJunctionBoxCtC.OutPorts[1], FMeter1.GetPlugPos(dmpsIc_out));
end;

procedure TWE_DIAGRAM.DrawType4_NoPT_L6;
begin
//  //外部电线到PT上接线盒的连线
  FCanvas.Polyline([FJunctionBoxPtUp.InPorts[0], Point(FPTGroup.PT1.LeftBPlugPos.X, FWire1.StartPos.Y)]);
  FCanvas.Polyline([FJunctionBoxPtUp.InPorts[1], Point(FPTGroup.PT2.LeftBPlugPos.X, FWire2.StartPos.Y)]);
  FCanvas.Polyline([FJunctionBoxPtUp.InPorts[2], Point(FPTGroup.PT3.LeftBPlugPos.X, FWire3.StartPos.Y)]);
  FCanvas.Polyline([FPTGroup.PT3.RightBPlugPos, Point(FPTGroup.PT3.RightBPlugPos.X, FWire3.StartPos.Y)]);

  //PT上接线盒到电表的连线
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[0], FMeter1.GetPlugPos(dmpsUa), FMeter1.GetPlugPos(0).Y + 60);
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[0], FMeter2.GetPlugPos(dmpsUa), FMeter1.GetPlugPos(0).Y + 60);
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[1], FMeter1.GetPlugPos(dmpsUb), FMeter1.GetPlugPos(0).Y + 70);
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[1], FMeter2.GetPlugPos(dmpsUb), FMeter1.GetPlugPos(0).Y + 70);
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[2], FMeter1.GetPlugPos(dmpsUc), FMeter1.GetPlugPos(0).Y + 80);
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[2], FMeter2.GetPlugPos(dmpsUc), FMeter1.GetPlugPos(0).Y + 80);
  DgDrawConnection3Y(FCanvas, FPTGroup.PT3.RightTPlugPos, FMeter1.GetPlugPos(dmpsUn1), FMeter1.GetPlugPos(0).Y + 90);
  DgDrawConnection3Y(FCanvas, Point(FPTGroup.PT3.RightTPlugPos.X, FWire4.StartPos.Y), FMeter1.GetPlugPos(dmpsUn1), FMeter1.GetPlugPos(0).Y + 90);

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
    DgDrawConnection(FCanvas, FCT1.LeftPlugPos, FCT1.RightPlugPos);

  if FWiringError.CT2Short then
    DgDrawConnection(FCanvas, FCT2.LeftPlugPos, FCT2.RightPlugPos);

  if FWiringError.CT3Short then
    DgDrawConnection(FCanvas, FCT3.LeftPlugPos, FCT3.RightPlugPos);

  //CT到CT接线盒的连线
  DgDrawConnection(FCanvas, FJunctionBoxCtA.OutPorts[0], FJunctionBoxCt.InPorts[0]);
  DgDrawConnection(FCanvas, FJunctionBoxCtB.OutPorts[0], FJunctionBoxCt.InPorts[1]);
  DgDrawConnection(FCanvas, FJunctionBoxCtC.OutPorts[0], FJunctionBoxCt.InPorts[2]);

  //CT接线盒到电表的连线, 以及电表之间的电流端子接线
  DgDrawConnection(FCanvas, FJunctionBoxCt.OutPorts[0], FMeter1.GetPlugPos(dmpsIa_in));
  DgDrawConnection(FCanvas, FJunctionBoxCt.OutPorts[1], FMeter1.GetPlugPos(dmpsIb_in));
  DgDrawConnection(FCanvas, FJunctionBoxCt.OutPorts[2], FMeter1.GetPlugPos(dmpsIc_in));

  DgDrawConnection3Y(FCanvas, FJunctionBoxCtA.OutPorts[1], FMeter2.GetPlugPos(dmpsIa_out), FJunctionBoxCt.InPorts[0].Y + 10);
  DgDrawConnection3Y(FCanvas, FJunctionBoxCtB.OutPorts[1], FMeter2.GetPlugPos(dmpsIb_out), FJunctionBoxCt.InPorts[0].Y + 15);
  DgDrawConnection3Y(FCanvas, FJunctionBoxCtC.OutPorts[1], FMeter2.GetPlugPos(dmpsIc_out), FJunctionBoxCt.InPorts[0].Y + 20);

  DgDrawConnection3Y(FCanvas, FMeter1.GetPlugPos(dmpsIa_out), FMeter2.GetPlugPos(dmpsIa_in), FMeter1.GetPlugPos(0).Y + 20);
  DgDrawConnection3Y(FCanvas, FMeter1.GetPlugPos(dmpsIb_out), FMeter2.GetPlugPos(dmpsIb_in), FMeter1.GetPlugPos(0).Y + 30);
  DgDrawConnection3Y(FCanvas, FMeter1.GetPlugPos(dmpsIc_out), FMeter2.GetPlugPos(dmpsIc_in), FMeter1.GetPlugPos(0).Y + 40);
end;

procedure TWE_DIAGRAM.DrawType4_PT_CT_CLear;
begin
  //外部电线到PT的连线
  FCanvas.Polyline([FPTGroup.PT1.LeftBPlugPos, Point(FPTGroup.PT1.LeftBPlugPos.X, FWire1.StartPos.Y)]);
  FCanvas.Polyline([FPTGroup.PT2.LeftBPlugPos, Point(FPTGroup.PT2.LeftBPlugPos.X, FWire2.StartPos.Y)]);
  FCanvas.Polyline([FPTGroup.PT3.LeftBPlugPos, Point(FPTGroup.PT3.LeftBPlugPos.X, FWire3.StartPos.Y)]);
  FCanvas.Polyline([FPTGroup.PT3.RightBPlugPos, Point(FPTGroup.PT3.RightBPlugPos.X, FWire3.StartPos.Y)]);
  DgDrawConnection(FCanvas, DgOffsetPoint(FPTGroup.PT3.RightBPlugPos, 0, 10), FPTGroup.PT1.RightBPlugPos);
  DgDrawConnection(FCanvas, DgOffsetPoint(FPTGroup.PT3.RightBPlugPos, 0, 10), FPTGroup.PT2.RightBPlugPos);
  DgDrawJunction(FCanvas, DgOffsetPoint(FPTGroup.PT3.RightBPlugPos, 0, 10), True);

//  //PT到PT接线盒之间的连线
  FCanvas.Polyline([FPTGroup.PT1.LeftTPlugPos, FJunctionBoxPtUp.InPorts[0]]);
  FCanvas.Polyline([FPTGroup.PT2.LeftTPlugPos, FJunctionBoxPtUp.InPorts[1]]);
  FCanvas.Polyline([FPTGroup.PT3.LeftTPlugPos, FJunctionBoxPtUp.InPorts[2]]);

//  //PT上接线盒到电表的连线
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[0], FMeter1.GetPlugPos(dmpsUa), FMeter1.GetPlugPos(0).Y + 60);
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[0], FMeter2.GetPlugPos(dmpsUa), FMeter1.GetPlugPos(0).Y + 60);
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[1], FMeter1.GetPlugPos(dmpsUb), FMeter1.GetPlugPos(0).Y + 70);
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[1], FMeter2.GetPlugPos(dmpsUb), FMeter1.GetPlugPos(0).Y + 70);
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[2], FMeter1.GetPlugPos(dmpsUc), FMeter1.GetPlugPos(0).Y + 80);
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[2], FMeter2.GetPlugPos(dmpsUc), FMeter1.GetPlugPos(0).Y + 80);
  DgDrawConnection3Y(FCanvas, FPTGroup.PT3.RightTPlugPos, FMeter1.GetPlugPos(dmpsUn1), FMeter1.GetPlugPos(0).Y + 90);

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
    DgDrawConnection(FCanvas, FCT1.LeftPlugPos, FCT1.RightPlugPos);

  if FWiringError.CT2Short then
    DgDrawConnection(FCanvas, FCT2.LeftPlugPos, FCT2.RightPlugPos);

  if FWiringError.CT3Short then
    DgDrawConnection(FCanvas, FCT3.LeftPlugPos, FCT3.RightPlugPos);

  //CT到CT接线盒的连线
  DgDrawConnection(FCanvas, FJunctionBoxCtA.OutPorts[0], FJunctionBoxCt.InPorts[0]);
  DgDrawConnection(FCanvas, FJunctionBoxCtB.OutPorts[0], FJunctionBoxCt.InPorts[1]);
  DgDrawConnection(FCanvas, FJunctionBoxCtC.OutPorts[0], FJunctionBoxCt.InPorts[2]);

  //CT接线盒到电表的连线, 以及电表之间的电流端子接线
  DgDrawConnection(FCanvas, FJunctionBoxCt.OutPorts[0], FMeter1.GetPlugPos(dmpsIa_in));
  DgDrawConnection(FCanvas, FJunctionBoxCt.OutPorts[1], FMeter1.GetPlugPos(dmpsIb_in));
  DgDrawConnection(FCanvas, FJunctionBoxCt.OutPorts[2], FMeter1.GetPlugPos(dmpsIc_in));

  DgDrawConnection3Y(FCanvas, FJunctionBoxCtA.OutPorts[1], FMeter2.GetPlugPos(dmpsIa_out), FJunctionBoxCt.InPorts[0].Y + 20);
  DgDrawConnection3Y(FCanvas, FJunctionBoxCtB.OutPorts[1], FMeter2.GetPlugPos(dmpsIb_out), FJunctionBoxCt.InPorts[0].Y + 20);
  DgDrawConnection3Y(FCanvas, FJunctionBoxCtC.OutPorts[1], FMeter2.GetPlugPos(dmpsIc_out), FJunctionBoxCt.InPorts[0].Y + 20);

  DgDrawConnection3Y(FCanvas, FMeter1.GetPlugPos(dmpsIa_out), FMeter2.GetPlugPos(dmpsIa_in), FMeter1.GetPlugPos(0).Y + 20);
  DgDrawConnection3Y(FCanvas, FMeter1.GetPlugPos(dmpsIb_out), FMeter2.GetPlugPos(dmpsIb_in), FMeter1.GetPlugPos(0).Y + 30);
  DgDrawConnection3Y(FCanvas, FMeter1.GetPlugPos(dmpsIc_out), FMeter2.GetPlugPos(dmpsIc_in), FMeter1.GetPlugPos(0).Y + 40);
end;

procedure TWE_DIAGRAM.DrawType4_PT_L6;
begin
  //外部电线到PT的连线
  FCanvas.Polyline([FPTGroup.PT1.LeftBPlugPos, Point(FPTGroup.PT1.LeftBPlugPos.X, FWire1.StartPos.Y)]);
  FCanvas.Polyline([FPTGroup.PT2.LeftBPlugPos, Point(FPTGroup.PT2.LeftBPlugPos.X, FWire2.StartPos.Y)]);
  FCanvas.Polyline([FPTGroup.PT3.LeftBPlugPos, Point(FPTGroup.PT3.LeftBPlugPos.X, FWire3.StartPos.Y)]);
  FCanvas.Polyline([FPTGroup.PT3.RightBPlugPos, Point(FPTGroup.PT3.RightBPlugPos.X, FWire3.StartPos.Y)]);
  DgDrawConnection(FCanvas, DgOffsetPoint(FPTGroup.PT3.RightBPlugPos, 0, 10), FPTGroup.PT1.RightBPlugPos);
  DgDrawConnection(FCanvas, DgOffsetPoint(FPTGroup.PT3.RightBPlugPos, 0, 10), FPTGroup.PT2.RightBPlugPos);
  DgDrawJunction(FCanvas, DgOffsetPoint(FPTGroup.PT3.RightBPlugPos, 0, 10), True);

//  //PT到PT接线盒之间的连线
  FCanvas.Polyline([FPTGroup.PT1.LeftTPlugPos, FJunctionBoxPtUp.InPorts[0]]);
  FCanvas.Polyline([FPTGroup.PT2.LeftTPlugPos, FJunctionBoxPtUp.InPorts[1]]);
  FCanvas.Polyline([FPTGroup.PT3.LeftTPlugPos, FJunctionBoxPtUp.InPorts[2]]);

//  //PT上接线盒到电表的连线
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[0], FMeter1.GetPlugPos(dmpsUa), FMeter1.GetPlugPos(0).Y + 60);
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[0], FMeter2.GetPlugPos(dmpsUa), FMeter1.GetPlugPos(0).Y + 60);
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[1], FMeter1.GetPlugPos(dmpsUb), FMeter1.GetPlugPos(0).Y + 70);
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[1], FMeter2.GetPlugPos(dmpsUb), FMeter1.GetPlugPos(0).Y + 70);
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[2], FMeter1.GetPlugPos(dmpsUc), FMeter1.GetPlugPos(0).Y + 80);
  DgDrawConnection3Y(FCanvas, FJunctionBoxPtUp.OutPorts[2], FMeter2.GetPlugPos(dmpsUc), FMeter1.GetPlugPos(0).Y + 80);
  DgDrawConnection3Y(FCanvas, FPTGroup.PT3.RightTPlugPos, FMeter1.GetPlugPos(dmpsUn1), FMeter1.GetPlugPos(0).Y + 90);

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
    DgDrawConnection(FCanvas, FCT1.LeftPlugPos, FCT1.RightPlugPos);

  if FWiringError.CT2Short then
    DgDrawConnection(FCanvas, FCT2.LeftPlugPos, FCT2.RightPlugPos);

  if FWiringError.CT3Short then
    DgDrawConnection(FCanvas, FCT3.LeftPlugPos, FCT3.RightPlugPos);

  //CT到CT接线盒的连线
  DgDrawConnection(FCanvas, FJunctionBoxCtA.OutPorts[0], FJunctionBoxCt.InPorts[0]);
  DgDrawConnection(FCanvas, FJunctionBoxCtB.OutPorts[0], FJunctionBoxCt.InPorts[1]);
  DgDrawConnection(FCanvas, FJunctionBoxCtC.OutPorts[0], FJunctionBoxCt.InPorts[2]);

  //CT接线盒到电表的连线, 以及电表之间的电流端子接线
  DgDrawConnection(FCanvas, FJunctionBoxCt.OutPorts[0], FMeter1.GetPlugPos(dmpsIa_in));
  DgDrawConnection(FCanvas, FJunctionBoxCt.OutPorts[1], FMeter1.GetPlugPos(dmpsIb_in));
  DgDrawConnection(FCanvas, FJunctionBoxCt.OutPorts[2], FMeter1.GetPlugPos(dmpsIc_in));

  DgDrawConnection3Y(FCanvas, FJunctionBoxCtA.OutPorts[1], FMeter2.GetPlugPos(dmpsIa_out), FJunctionBoxCt.InPorts[0].Y + 10);
  DgDrawConnection3Y(FCanvas, FJunctionBoxCtB.OutPorts[1], FMeter2.GetPlugPos(dmpsIb_out), FJunctionBoxCt.InPorts[0].Y + 15);
  DgDrawConnection3Y(FCanvas, FJunctionBoxCtC.OutPorts[1], FMeter2.GetPlugPos(dmpsIc_out), FJunctionBoxCt.InPorts[0].Y + 20);

  DgDrawConnection3Y(FCanvas, FMeter1.GetPlugPos(dmpsIa_out), FMeter2.GetPlugPos(dmpsIa_in), FMeter1.GetPlugPos(0).Y + 20);
  DgDrawConnection3Y(FCanvas, FMeter1.GetPlugPos(dmpsIb_out), FMeter2.GetPlugPos(dmpsIb_in), FMeter1.GetPlugPos(0).Y + 30);
  DgDrawConnection3Y(FCanvas, FMeter1.GetPlugPos(dmpsIc_out), FMeter2.GetPlugPos(dmpsIc_in), FMeter1.GetPlugPos(0).Y + 40);
end;

function TWE_DIAGRAM.GetHeight: Integer;
begin
  Result := 450;
end;

function TWE_DIAGRAM.GetWidth: Integer;
begin
  Result := 630;
end;

procedure TWE_DIAGRAM.SetDiagramType(const Value: TDiagramType);
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
end;

procedure TWE_DIAGRAM.SetWiringError(const Value: TWIRING_ERROR);
begin
  if ((FDiagramType < dt4M_PT) and (Value.PhaseType = ptThree))
    or (Value.PhaseType = ptFour) then
    FWiringError.Assign(Value)
  else
    raise Exception.Create('Phase type conflict');
end;

end.
