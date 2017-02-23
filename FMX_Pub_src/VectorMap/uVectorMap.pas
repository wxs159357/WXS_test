unit uVectorMap;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects, FMX.Edit, FMX.ComboTrackBar,
  FMX.Colors, FMX.ComboEdit, FMX.ListBox, xVectorMap, xVectorArc, xVectorLine,
  xVectorType;

type
  TfVectorMap = class(TForm)
    rctngl1: TRectangle;
    pnl1: TPanel;
    spltr2: TSplitter;
    grpbx1: TGroupBox;
    edtVName: TComboEdit;
    lbl1: TLabel;
    cmbclrbxVColor: TComboColorBox;
    lbl2: TLabel;
    cmbtrckbrVAngle: TComboTrackBar;
    lbl4: TLabel;
    edtVValue: TEdit;
    lbl5: TLabel;
    btn1: TButton;
    btn2: TButton;
    cbbVType: TComboBox;
    lbl3: TLabel;
    btnDelArc: TButton;
    btnAddArc: TButton;
    procedure edtVValueChange(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure edtVNameChange(Sender: TObject);
    procedure cmbclrbxVColorChange(Sender: TObject);
    procedure cmbtrckbrVAngleChange(Sender: TObject);
    procedure cbbVTypeChange(Sender: TObject);
    procedure btnDelArcClick(Sender: TObject);
    procedure btnAddArcClick(Sender: TObject);
  private
    { Private declarations }
    FVector : TVectorLineInfo;
    FArc : TVectorArc;

    /// <summary>
    /// 向量选择
    /// </summary>
    procedure VectorSelect(Sender: TObject);

    /// <summary>
    /// 弧线选择
    /// </summary>
    procedure ArcSelect(Sender: tObject);
  public
    { Public declarations }
    /// <summary>
    /// 显示向量图
    /// </summary>
    procedure DrawVectorMap(sVectorStr : string);

    /// <summary>
    /// 获取绘制完的向量图
    /// </summary>
    function GetVectorMapStr : string;
  end;

var
  fVectorMap: TfVectorMap;

implementation

{$R *.fmx}

procedure TfVectorMap.ArcSelect(Sender: tObject);
begin
  btnDelArc.Enabled := Assigned(Sender);
  FArc := TVectorArc(Sender);
end;

procedure TfVectorMap.btn1Click(Sender: TObject);
var
  AVector : TVectorLineInfo;
  dValue : Double;
begin
  AVector := AVectorControl.AddVector;
  AVector.VName := edtVName.Text;
  AVector.VColor := cmbclrbxVColor.Color;
  AVector.VType := tTVectorType(cbbVType.ItemIndex);
  AVector.VAngle := cmbtrckbrVAngle.Value;
  TryStrToFloat(edtVValue.Text, dValue);
  AVector.VValue := dValue;

  AVector.Draw;
end;

procedure TfVectorMap.btn2Click(Sender: TObject);
begin
  AVectorControl.DelSelect;
end;

procedure TfVectorMap.btnAddArcClick(Sender: TObject);
begin
  AVectorControl.AddVectorArc;
end;

procedure TfVectorMap.btnDelArcClick(Sender: TObject);
begin
  if Assigned(FArc) then
  begin
    AVectorControl.DelArc(FArc.VID);
    btnDelArc.Enabled := False;
  end;
end;

procedure TfVectorMap.cbbVTypeChange(Sender: TObject);
begin
  if Assigned(FVector) then
  begin
    FVector.VType := tTVectorType(cbbVType.ItemIndex);
    AVectorControl.Draw;
  end;
end;

procedure TfVectorMap.cmbclrbxVColorChange(Sender: TObject);
begin
  if Assigned(FVector) then
  begin
    FVector.VColor := cmbclrbxVColor.Color;
    FVector.Draw;
  end;
end;

procedure TfVectorMap.cmbtrckbrVAngleChange(Sender: TObject);
begin
  if Assigned(FVector) then
  begin
    FVector.VAngle := cmbtrckbrVAngle.Value;
    AVectorControl.Draw;
  end;
end;

procedure TfVectorMap.DrawVectorMap(sVectorStr: string);
begin
  AVectorControl.Canvas := rctngl1;
  AVectorControl.Rect := RectF(rctngl1.Position.X, rctngl1.Position.Y,
    rctngl1.Position.X + rctngl1.Width, rctngl1.Position.Y + rctngl1.Height);

  AVectorControl.IsCanSelect := True;

  if sVectorStr <> '' then
    AVectorControl.VectorStr := sVectorStr;

  AVectorControl.Draw;

  AVectorControl.OnSelectVector := VectorSelect;
  AVectorControl.OnSelectArc := ArcSelect;
end;

procedure TfVectorMap.edtVNameChange(Sender: TObject);
begin
  if Assigned(FVector) then
  begin
    FVector.VName := edtVName.Text;
    FVector.Draw;
  end;
end;

procedure TfVectorMap.edtVValueChange(Sender: TObject);
var
  dValue : Double;
begin
  TryStrToFloat(cmbtrckbrVAngle.Text, dValue);
  cmbtrckbrVAngle.Text := FormatFloat('0.00', dValue);

  if Assigned(FVector) then
  begin
    TryStrToFloat(edtVValue.Text, dValue);
    FVector.VValue := dValue;
    FVector.Draw;
  end;
end;

procedure TfVectorMap.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  AVectorControl.OnSelectVector := nil;
end;

function TfVectorMap.GetVectorMapStr: string;
begin
  Result := AVectorControl.VectorStr;
end;

procedure TfVectorMap.VectorSelect(Sender: TObject);
var
  dValue : Double;
begin
  if Assigned(Sender) then
  begin
    FVector := TVectorLineInfo(Sender);
    edtVName.Text := FVector.VName;
    cmbclrbxVColor.Color := FVector.VColor;
    cbbVType.ItemIndex := Integer(FVector.VType);
    cmbtrckbrVAngle.Value := FVector.VAngle;
    TryStrToFloat(edtVValue.Text, dValue);
    edtVValue.Text := FormatFloat('0.00', FVector.VValue);
  end
  else
  begin
    FVector := nil;
  end;

  btnAddArc.Enabled := AVectorControl.GetSelectVectorCount = 2;
end;

end.
