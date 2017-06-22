unit Unit5;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, xVectorLine,
  xVectorMap, FMX.ScrollBox, FMX.Memo, xVectorType, xVectorArc, FMX.Colors,
  FMX.ListBox, uVectorMap;

type
  TForm5 = class(TForm)
    btn4: TButton;
    mmo1: TMemo;
    btn5: TButton;
    btn6: TButton;
    rctngl1: TRectangle;
    btn2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btn4Click(Sender: TObject);
    procedure btn5Click(Sender: TObject);
    procedure btn6Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
  private
    { Private declarations }

    FFormMap : TfVectorMap;
    FVLine : TVectorLineInfo;
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

{$R *.fmx}

procedure TForm5.btn2Click(Sender: TObject);
begin
  FFormMap.DrawVectorMap(mmo1.Lines.Text);
  FFormMap.ShowModal;

  mmo1.Lines.Text := FFormMap.GetVectorMapStr;
end;

procedure TForm5.btn4Click(Sender: TObject);
var
  AVectorU, AVectorI : TVectorLineInfo;
  AArc : TVectorArc;
begin
  AVectorControl.VectorList.Clear;


  AVectorControl.Canvas := rctngl1;
  AVectorControl.Rect := RectF(rctngl1.Position.X, rctngl1.Position.Y,
    rctngl1.Position.X + rctngl1.Width, rctngl1.Position.Y + rctngl1.Height);

//  FVControl.BackColor := $FF272727;
//  FVControl.BackColor := $FFFFFFFF;
  AVectorControl.VectorMapName := '有功测试向量图';
  AVectorU := AVectorControl.AddVector;
  AVectorU.VName := 'Ua';
  AVectorU.VType := vtVol;
  AVectorU.VAngle := 90;
  AVectorU.VValue := 220;
  AVectorU.VColor := C_COLOR_A;

  AVectorI := AVectorControl.AddVector;
  AVectorI.VName := 'Ia';
  AVectorI.VType := vtCurrent;
  AVectorI.VAngle := 200;
  AVectorI.VValue := 5;
  AVectorI.VColor := C_COLOR_A;

  AArc := AVectorControl.AddArc;
  AArc.StartVector := AVectorU;
  AArc.EndVector := AVectorI;

  AVectorU := AVectorControl.AddVector;
  AVectorU.VName := 'Ub';
  AVectorU.VType := vtVol;
  AVectorU.VAngle := 330;
  AVectorU.VValue := 220;
  AVectorU.VColor := C_COLOR_B;

  AVectorI := AVectorControl.AddVector;
  AVectorI.VName := 'Ib';
  AVectorI.VType := vtCurrent;
  AVectorI.VAngle := 310;
  AVectorI.VValue := 5;
  AVectorI.VColor := C_COLOR_B;

  AArc := AVectorControl.AddArc;
  AArc.StartVector := AVectorU;
  AArc.EndVector := AVectorI;

  AVectorU := AVectorControl.AddVector;
  AVectorU.VName := 'Uc';
  AVectorU.VType := vtVol;
  AVectorU.VAngle := 210;
  AVectorU.VValue := 220;
  AVectorU.VColor := C_COLOR_C;

  AVectorI := AVectorControl.AddVector;
  AVectorI.VName := 'Ic';
  AVectorI.VType := vtCurrent;
  AVectorI.VAngle := 350;
  AVectorI.VValue := 5;
  AVectorI.VColor := C_COLOR_C;

  AArc := AVectorControl.AddArc;
  AArc.StartVector := AVectorU;
  AArc.EndVector := AVectorI;

////////////////////////////////////////////////////////////////////
//  AVectorU := FVControl.AddVector;
//  AVectorU.VName := 'Uab';
//  AVectorU.VType := vtVol;
//  AVectorU.VAngle := 90;
//  AVectorU.VValue := 380;
//  AVectorU.VColor := C_COLOR_A;
//
//  AVectorI := FVControl.AddVector;
//  AVectorI.VName := 'Ia';
//  AVectorI.VType := vtCurrent;
//  AVectorI.VAngle := 200;
//  AVectorI.VValue := 5;
//  AVectorI.VColor := C_COLOR_A;
//
//  AArc := FVControl.AddArc;
//  AArc.StartVector := AVectorU;
//  AArc.EndVector := AVectorI;
//
//  AVectorU := FVControl.AddVector;
//  AVectorU.VName := 'Ubc';
//  AVectorU.VType := vtVol;
//  AVectorU.VAngle := 180;
//  AVectorU.VValue := 380;
//  AVectorU.VColor := C_COLOR_C;
//
//  AVectorI := FVControl.AddVector;
//  AVectorI.VName := 'Ic';
//  AVectorI.VType := vtCurrent;
//  AVectorI.VAngle := 350;
//  AVectorI.VValue := 5;
//  AVectorI.VColor := C_COLOR_C;
//
//  AArc := FVControl.AddArc;
//  AArc.StartVector := AVectorU;
//  AArc.EndVector := AVectorI;







  AVectorControl.Draw;
end;

procedure TForm5.btn5Click(Sender: TObject);
begin
  mmo1.Lines.Text := AVectorControl.VectorStr;
end;

procedure TForm5.btn6Click(Sender: TObject);
begin

  AVectorControl.Canvas := rctngl1;
  AVectorControl.Rect := RectF(rctngl1.Position.X, rctngl1.Position.Y,
    rctngl1.Position.X + rctngl1.Width, rctngl1.Position.Y + rctngl1.Height);

  AVectorControl.VectorStr := mmo1.Lines.Text;
  AVectorControl.Draw;
end;

procedure TForm5.FormCreate(Sender: TObject);
begin
  FVLine := TVectorLineInfo.Create;
  AVectorControl := TVectorMap.Create(Self);
  FFormMap := TfVectorMap.Create(Self);
end;

procedure TForm5.FormDestroy(Sender: TObject);
begin
  FVLine.Free;
  AVectorControl.Free;
  FFormMap.Free;
end;

end.


