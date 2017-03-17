unit FrmVectorMapInfo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin,  U_VECTOR_CONTROL, U_VECTOR_LIEN_INFO,
  Math, System.Types;

type
  TfVectorMapInfo = class(TForm)
    pnl2: TPanel;
    pnl3: TPanel;
    spltr1: TSplitter;
    grp1: TGroupBox;
    lblColor: TLabel;
    lblAngle: TLabel;
    lbl1: TLabel;
    lblType: TLabel;
    cbbName: TComboBox;
    btnAdd: TButton;
    edtValue: TEdit;
    btnDel: TButton;
    mmoShow: TMemo;
    btnModify: TButton;
    pnlClr: TPanel;
    cbbAngle: TComboBox;
    lbl2: TLabel;
    cbbType: TComboBox;
    imgMap: TImage;
    dlgColor: TColorDialog;
    btnSave: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure pnlClrClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure imgMapMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure cbbNameChange(Sender: TObject);
    procedure imgMapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure edtValueChange(Sender: TObject);
    procedure btnDelClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    FMapControl: TVECTOR_CONTROL;

    FVLSelectMain : TVECTOR_LIEN_INFO;
    FIsAutoSave : Boolean;

    procedure SaveInfo(AVLInfo : TVECTOR_LIEN_INFO);
    procedure ShowInfo(AVLInfo : TVECTOR_LIEN_INFO);

    procedure RefurshMap;
  public
    { Public declarations }
    /// <summary>
    /// 添加向量图
    /// </summary>
    procedure NewVMap(AMapControl : TVECTOR_CONTROL );

    /// <summary>
    /// 修改向量图
    /// </summary>
    procedure EditVMap(AMapControl : TVECTOR_CONTROL);
  end;

var
  fVectorMapInfo: TfVectorMapInfo;

implementation

{$R *.dfm}

{ TfVectorMapInfo }

procedure TfVectorMapInfo.btnAddClick(Sender: TObject);
var
  AVLInfo : TVECTOR_LIEN_INFO;
begin
  if Assigned(FMapControl) then
  begin
    AVLInfo := FMapControl.AddVector;
    SaveInfo(AVLInfo);
    FMapControl.SetNoSelect;
    FVLSelectMain := nil;

    RefurshMap;
  end;
end;

procedure TfVectorMapInfo.btnDelClick(Sender: TObject);
begin
  if Assigned(FMapControl) then
  begin
    FMapControl.DelSelect;
    RefurshMap;
  end;
end;

procedure TfVectorMapInfo.btnSaveClick(Sender: TObject);
begin
  if FIsAutoSave then
  begin
    if Assigned(FVLSelectMain) then
      SaveInfo(FVLSelectMain);

    RefurshMap;
  end;

end;

procedure TfVectorMapInfo.cbbNameChange(Sender: TObject);
  function GetIn(s: string; asarray :array of string) : Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := 0 to Length(asarray) - 1 do
    begin
      if asarray[i] = s then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
begin
  if GetIn(cbbName.Text,['Ua', 'Ia', 'Uab', 'Uac', 'Uba']) then
    pnlClr.Color := C_COLOR_A
  else if GetIn(cbbName.Text, ['Ub', 'Ib', 'Uab', 'Uac', 'Uba']) then
    pnlClr.Color := C_COLOR_B
  else
    pnlClr.Color := C_COLOR_C;

  if GetIn(cbbName.Text, ['Ia', 'Ib', 'Ic']) then
    cbbType.Text := GetVTStr(vtCurrent)
  else
    cbbType.Text := GetVTStr(vtVol);

  if cbbName.Text = 'Ua' then
    cbbAngle.Text := '90'
  else if cbbName.Text = 'Ub' then
    cbbAngle.Text := '210'
  else if cbbName.Text = 'Uc' then
    cbbAngle.Text := '330'
  else if cbbName.Text = 'Ia' then
    cbbAngle.Text := '70'
  else if cbbName.Text = 'Ib' then
    cbbAngle.Text := '190'
  else if cbbName.Text = 'Ic' then
    cbbAngle.Text := '310'
  else if cbbName.Text = 'Uab' then
    cbbAngle.Text := '120'
  else if cbbName.Text = 'Ucb' then
    cbbAngle.Text := '180'
  else if cbbName.Text = 'Uac' then
    cbbAngle.Text := '60'
  else if cbbName.Text = 'Ubc' then
    cbbAngle.Text := '0'
  else if cbbName.Text = 'Uba' then
    cbbAngle.Text := '300'
  else if cbbName.Text = 'Uca' then
    cbbAngle.Text := '240';

  if GetIn(cbbName.Text, ['Ia', 'Ib', 'Ic']) then
    edtValue.Text := '5'
  else if GetIn(cbbName.Text, ['Ua', 'Ub', 'Uc']) then
    edtValue.Text := '220'
  else
    edtValue.Text := '380';

end;

procedure TfVectorMapInfo.EditVMap(AMapControl: TVECTOR_CONTROL);
begin
  FMapControl := AMapControl;

  if not Assigned(FMapControl) then
    Exit;

  FMapControl.Canvas := imgMap.Canvas;

  FMapControl.Rect := Rect(0,0,imgMap.Width-1, imgMap.Height-1);

  RefurshMap;
end;

procedure TfVectorMapInfo.edtValueChange(Sender: TObject);
begin
  btnSaveClick(nil);
end;

procedure TfVectorMapInfo.FormCreate(Sender: TObject);
begin
  cbbType.Items.Text := GetVTAllStr;
  cbbType.ItemIndex := 0;
  FIsAutoSave := True;
  cbbNameChange(nil);
end;

procedure TfVectorMapInfo.FormResize(Sender: TObject);
begin
  if Assigned(FMapControl) then
  begin
    FMapControl.Rect := Rect(0,0,imgMap.Width-1, imgMap.Height-1);
    RefurshMap;
  end;
end;

procedure TfVectorMapInfo.imgMapMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  AVLInfo : TVECTOR_LIEN_INFO;
  bSelect : Boolean;
begin
  if not Assigned(FMapControl) then
    Exit;

  AVLInfo := FMapControl.PointInVLine(Point(x, y));

  FMapControl.SetNoSelect;
  FVLSelectMain := nil;

  bSelect := Assigned(AVLInfo);
  btnDel.Enabled := bSelect;

  if bSelect then
  begin
    AVLInfo.IsSelected := True;
    AVLInfo.IsMainSelect := True;
    FVLSelectMain := AVLInfo;
    ShowInfo(AVLInfo);
  end;

  RefurshMap;
end;

procedure TfVectorMapInfo.imgMapMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  dX, dY : Double;
  APoint : TPoint;
  dValue : Double;
begin
  if not Assigned(FMapControl) then
    Exit;

  FMapControl.MouseMove(Point(x, y));

  if Shift = [ssLeft] then
  begin
    if Assigned(FVLSelectMain) then
    begin
      APoint := FVLSelectMain.CenterPoint;
      dX := X - APoint.X;
      dY := APoint.Y - Y;
      if dX = 0 then
        dValue := 0
      else
        dValue := dY/dX;

      if x > APoint.X then
      begin
        FVLSelectMain.VAngle := RadToDeg(ArcTan(dValue));
      end
      else
      begin
        if x = APoint.X then
        begin
          if y > APoint.y then
            FVLSelectMain.VAngle := 270
          else
            FVLSelectMain.VAngle := 90;
        end
        else
        begin
          FVLSelectMain.VAngle := RadToDeg(ArcTan(dValue)) - 180;
        end;
      end;
      ShowInfo(FVLSelectMain);
    end;
  end;

  RefurshMap;
end;

procedure TfVectorMapInfo.NewVMap(AMapControl: TVECTOR_CONTROL);
begin
  FMapControl := AMapControl;

  if not Assigned(FMapControl) then
    Exit;

  FMapControl.ClearList;
  FMapControl.Canvas := imgMap.Canvas;

  FMapControl.Rect := Rect(0,0,imgMap.Width-1, imgMap.Height-1);

  RefurshMap;
end;

procedure TfVectorMapInfo.pnlClrClick(Sender: TObject);
begin
  dlgColor.Color := pnlClr.Color;

  if dlgColor.Execute then
  begin
    pnlClr.Color := dlgColor.Color;
    btnSaveClick(nil);
  end;
end;

procedure TfVectorMapInfo.RefurshMap;
begin
  if Assigned(FMapControl) then
  begin
    FMapControl.Draw;
    imgMap.Refresh;
    mmoShow.Lines.Text := FMapControl.VectorStr;
  end;
end;

procedure TfVectorMapInfo.SaveInfo(AVLInfo : TVECTOR_LIEN_INFO);
  function GetValue(s : string):Double;
  begin
    TryStrToFloat(s, Result);
  end;
begin
  AVLInfo.VName := cbbName.Text;
  AVLInfo.VColor := pnlClr.Color;
  AVLInfo.VTypeStr := cbbType.Text;
  AVLInfo.VValue := GetValue(edtValue.Text);
  AVLInfo.VAngle := GetValue(cbbAngle.Text);
  cbbAngle.Text := FormatFloat('0.00', AVLInfo.VAngle);
end;

procedure TfVectorMapInfo.ShowInfo(AVLInfo : TVECTOR_LIEN_INFO);
begin
  FIsAutoSave := False;
  cbbName.Text := AVLInfo.VName;
  pnlClr.Color := AVLInfo.VColor;
  cbbType.Text := AVLInfo.VTypeStr;
  edtValue.Text := FormatFloat('0.00', AVLInfo.VValue);
  cbbAngle.Text := FormatFloat('0.00', AVLInfo.VAngle);
  FIsAutoSave := True;
end;

end.
