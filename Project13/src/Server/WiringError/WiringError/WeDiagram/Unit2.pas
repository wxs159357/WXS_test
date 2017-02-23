unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, U_WE_DIAGRAM, jpeg, StdCtrls, GR32_Image, GR32_Layers,
  FrmWESelect2;

type
  TForm2 = class(TForm)
    ComboBox1: TComboBox;
    Panel1: TPanel;
    btn1: TButton;
    img1: TImage32;
    pnl1: TPanel;
    ScrollBox1: TScrollBox;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure img1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure img1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);
    procedure img1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure img1Resize(Sender: TObject);
    procedure ScrollBox1Resize(Sender: TObject);
    procedure img1MouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure img1MouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
  private
    FBitmap: TBitmap;
    FWeDiagram: TWE_DIAGRAM;

    FWeSel2: TfWESelect2;

    ptBefore: TPoint;

    procedure WiringErrorChanged(Sender: TObject);

    procedure ResetCursor;

  public
    { Public declarations }
  end;

var
  Form2: TForm2;


implementation

uses
  U_WIRING_ERROR;

{$R *.dfm}

const
  crHandFlat = 1;
  crHandGrab = 2;

procedure TForm2.btn1Click(Sender: TObject);
begin
  img1.SetBounds(img1.Left, img1.Top, img1.Bitmap.Width, img1.Bitmap.Height);
  img1.SetFocus;
end;

procedure TForm2.ComboBox1Change(Sender: TObject);
begin
  if ComboBox1.ItemIndex < Ord(dt4M_NoPT) then
    FWeSel2.SetPhaseType(ptThree)
  else
    FWeSel2.SetPhaseType(ptFour);
  FWeDiagram.DiagramType := TDiagramType(ComboBox1.ItemIndex);
  FWeSel2.ChangeStatus(nil);
end;

procedure TForm2.FormCreate(Sender: TObject);
var
  DiagramType: TDiagramType;
begin
  FWeSel2 := TfWESelect2.Create(Self);
  FWeSel2.tsPhase3.TabVisible := False;
  FWeSel2.tsPhase4.TabVisible := False;
  FWeSel2.pgcErrors.ActivePageIndex := 0;
  FWeSel2.BorderStyle := bsNone;
  pnl1.Width := FWeSel2.ClientWidth;
  FWeSel2.Parent := pnl1;
  FWeSel2.Align := alClient;
  FWeSel2.WiringError.OnChanged := WiringErrorChanged;
  FWeSel2.Show;

  FBitmap := TBitmap.Create;
  FBitmap.SetSize(img1.Width, img1.Height);
  FWeDiagram := TWE_DIAGRAM.Create(FBitmap.Canvas);
  FWeDiagram.Pos := Point(20, 20);
  ComboBox1.Clear;
  for DiagramType := Low(DiagramType) to High(DiagramType) do
    ComboBox1.Items.Add(DiagramTypeToStr(DiagramType));
  ComboBox1.ItemIndex := Integer(dt4Direct);
  ComboBox1Change(nil);

  Screen.Cursors[crHandFlat] := LoadCursor(HInstance, 'HANDFLAT');
  Screen.Cursors[crHandGrab] := LoadCursor(HInstance, 'HANDGRAB');
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  FWeDiagram.Free;
  FBitmap.Free;
end;

procedure TForm2.img1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if img1.Cursor = crHandFlat then
  begin
    ptBefore := Point(X, Y);
    Screen.Cursor := crHandGrab;
  end;
end;

procedure TForm2.img1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer; Layer: TCustomLayer);
begin
  if (ptBefore.X > 0) and (ptBefore.Y > 0) then
    with ScrollBox1 do
    begin
      Screen.Cursor := crHandGrab;
      HorzScrollBar.Position := HorzScrollBar.Position - (X - ptBefore.X);
      VertScrollBar.Position := VertScrollBar.Position - (Y - ptBefore.Y);
    end;
end;

procedure TForm2.img1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  ptBefore := Point(-1, -1);
  Screen.Cursor := crDefault;
end;

procedure TForm2.img1MouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if Trunc(Img1.Width / 1.1) > 200 then
  begin
    Img1.Width := Trunc(Img1.Width / 1.2);
    Img1.Height := Trunc(Img1.Height / 1.2);
  end;
end;

procedure TForm2.img1MouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if Trunc(Img1.Width * 1.1) < 3000 then
  begin
    Img1.Width := Trunc(Img1.Width * 1.2);
    Img1.Height := Trunc(Img1.Height * 1.2);
  end;
end;

procedure TForm2.img1Resize(Sender: TObject);
begin
  ResetCursor;
end;

procedure TForm2.ResetCursor;
begin
  if (img1.Width > ScrollBox1.Width) or (img1.Height > ScrollBox1.Height) then
    img1.Cursor := crHandFlat
  else
    img1.Cursor := crDefault;
end;

procedure TForm2.ScrollBox1Resize(Sender: TObject);
begin
  ResetCursor;
end;

procedure TForm2.WiringErrorChanged(Sender: TObject);
begin
  FWeDiagram.WiringError := FWeSel2.WiringError;
  FWeDiagram.Draw;
  img1.Bitmap.Assign(FBitmap);
end;

end.
