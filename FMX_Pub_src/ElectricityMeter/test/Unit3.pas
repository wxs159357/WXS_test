unit Unit3;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, xElecBox,
  FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, xElecLine,
  xElecPoint, FMX.Edit, xElecFunction;

type
  TForm3 = class(TForm)
    pnl1: TPanel;
    mmo1: TMemo;
    spltr1: TSplitter;
    btn1: TButton;
    btn2: TButton;
    btn3: TButton;
    btn4: TButton;
    edt1: TEdit;
    btn6: TButton;
    btn7: TButton;
    btn5: TButton;
    lbl1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure btn4Click(Sender: TObject);
    procedure btn6Click(Sender: TObject);
    procedure btn7Click(Sender: TObject);
    procedure btn5Click(Sender: TObject);
  private
    { Private declarations }
    FElecBox : TElecBox;
    FElecPoint : TElecPoint;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

procedure TForm3.btn1Click(Sender: TObject);
  procedure SetValue(AElecLine: TElecLine);
  var
    j : Integer;
  begin
    mmo1.Lines.Add(AElecLine.LineName + ' 连接列表');

    for j := 0 to AElecLine.ConnPoints.Count - 1 do
    begin
      mmo1.Lines.Add(TElecLine(AElecLine.ConnPoints.Objects[j]).LineName);
    end;
  end;
begin
  SetValue(FElecBox.ElecBusLine.BusLineA);


  SetValue(FElecBox.ElecTV1.SecondVolH);
  SetValue(FElecBox.ElecTV2.SecondVolH);
  SetValue(FElecBox.ElecTV3.SecondVolH);
end;

procedure TForm3.btn2Click(Sender: TObject);
  procedure SetValue(AElecLine: TElecLine);
  begin
    mmo1.Lines.Add(AElecLine.LineName + '电压：' + FormatFloat('0.0', AElecLine.Voltage.Value) +
      '角度：' + FormatFloat('0.0', AElecLine.Voltage.Angle));
  end;
var
  i : Integer;
  APoint : TElecPoint;
begin
  mmo1.Lines.Add(' 电缆 电压值列表===========');

  SetValue(FElecBox.ElecBusLine.BusLineA);
  SetValue(FElecBox.ElecBusLine.BusLineB);
  SetValue(FElecBox.ElecBusLine.BusLineC);
  SetValue(FElecBox.ElecBusLine.BusLineN);

  mmo1.Lines.Add(' TV 一次高端===========');

  SetValue(FElecBox.ElecTV1.FirstVolH);
  SetValue(FElecBox.ElecTV2.FirstVolH);
  SetValue(FElecBox.ElecTV3.FirstVolH);

  mmo1.Lines.Add(' TV 一次低端===========');
  SetValue(FElecBox.ElecTV1.FirstVolL);
  SetValue(FElecBox.ElecTV2.FirstVolL);
  SetValue(FElecBox.ElecTV3.FirstVolL);

  mmo1.Lines.Add(' TV 二次高端===========');

  SetValue(FElecBox.ElecTV1.SecondVolH);
  SetValue(FElecBox.ElecTV2.SecondVolH);
  SetValue(FElecBox.ElecTV3.SecondVolH);

  mmo1.Lines.Add(' TV 二次低端===========');
  SetValue(FElecBox.ElecTV1.SecondVolL);
  SetValue(FElecBox.ElecTV2.SecondVolL);
  SetValue(FElecBox.ElecTV3.SecondVolL);

  mmo1.Lines.Add(' 接线盒进线电压===========');
  SetValue(FElecBox.ElecLineBox.BoxUA.InLineVol);
  SetValue(FElecBox.ElecLineBox.BoxUB.InLineVol);
  SetValue(FElecBox.ElecLineBox.BoxUC.InLineVol);
  SetValue(FElecBox.ElecLineBox.BoxUN.InLineVol);

  mmo1.Lines.Add(' 接线盒出线电压===========');
  SetValue(FElecBox.ElecLineBox.BoxUA.OutLineVol);
  SetValue(FElecBox.ElecLineBox.BoxUB.OutLineVol);
  SetValue(FElecBox.ElecLineBox.BoxUC.OutLineVol);
  SetValue(FElecBox.ElecLineBox.BoxUN.OutLineVol);

  mmo1.Lines.Add(' 电表 电压值列表===========');
  for i := 0 to FElecBox.ElecMeter.OrganList.Count - 1 do
  begin
    APoint := FElecBox.ElecMeter.OrganInfo[i].VolOrgan;

    mmo1.Lines.Add(FElecBox.ElecMeter.OrganInfo[i].OrganName +
      '电压：' + FormatFloat('0.0', APoint.Value) +
      '角度：' + FormatFloat('0.0', APoint.Angle));
  end;
end;

procedure TForm3.btn3Click(Sender: TObject);
  procedure SetValue(AElecH, AElecL: TElecLine);
  begin
    GetTwoPointCurrent(AElecH, AElecL, FElecPoint);
    mmo1.Lines.Add(AElecH.LineName + '-' + AElecL.LineName +
      '电流：' + FormatFloat('0.0', FElecPoint.Value) +
      '角度：' + FormatFloat('0.0', FElecPoint.Angle));
  end;
var
  i : Integer;
begin
  mmo1.Lines.Add('一次电流值=============');
  SetValue(FElecBox.ElecBusLine.BusLineA, FElecBox.ElecTA1.FirstCurrentH);
  SetValue(FElecBox.ElecBusLine.BusLineB, FElecBox.ElecTA2.FirstCurrentH);
  SetValue(FElecBox.ElecBusLine.BusLineC, FElecBox.ElecTA3.FirstCurrentH);

  SetValue(FElecBox.ElecTA1.FirstCurrentH,FElecBox.ElecTA1.FirstCurrentL);
  SetValue(FElecBox.ElecTA2.FirstCurrentH,FElecBox.ElecTA2.FirstCurrentL);
  SetValue(FElecBox.ElecTA3.FirstCurrentH,FElecBox.ElecTA3.FirstCurrentL);

  SetValue(FElecBox.ElecTA1.FirstCurrentL, FElecBox.ElecBusLine.BusLineN);
  SetValue(FElecBox.ElecTA2.FirstCurrentL, FElecBox.ElecBusLine.BusLineN);
  SetValue(FElecBox.ElecTA3.FirstCurrentL, FElecBox.ElecBusLine.BusLineN);

  mmo1.Lines.Add('二次电流值=============');

  SetValue(FElecBox.ElecTA1.SecondCurrentH, FElecBox.ElecLineBox.BoxIA.InLineCurrent1);
  SetValue(FElecBox.ElecTA2.SecondCurrentH, FElecBox.ElecLineBox.BoxIB.InLineCurrent1);
  SetValue(FElecBox.ElecTA3.SecondCurrentH, FElecBox.ElecLineBox.BoxIC.InLineCurrent1);

  SetValue(FElecBox.ElecLineBox.BoxIA.InLineCurrent1, FElecBox.ElecLineBox.BoxIA.OutLineCurrent1);
  SetValue(FElecBox.ElecLineBox.BoxIB.InLineCurrent1, FElecBox.ElecLineBox.BoxIB.OutLineCurrent1);
  SetValue(FElecBox.ElecLineBox.BoxIC.InLineCurrent1, FElecBox.ElecLineBox.BoxIC.OutLineCurrent1);

  SetValue(FElecBox.ElecLineBox.BoxIA.OutLineCurrent1, FElecBox.ElecMeter.TerminalInfoByName['Ia+']);
  SetValue(FElecBox.ElecLineBox.BoxIB.OutLineCurrent1, FElecBox.ElecMeter.TerminalInfoByName['Ib+']);
  SetValue(FElecBox.ElecLineBox.BoxIC.OutLineCurrent1, FElecBox.ElecMeter.TerminalInfoByName['Ic+']);

  SetValue(FElecBox.ElecMeter.TerminalInfoByName['Ia+'], FElecBox.ElecMeter.TerminalInfoByName['Ia-']);
  SetValue(FElecBox.ElecMeter.TerminalInfoByName['Ib+'], FElecBox.ElecMeter.TerminalInfoByName['Ib-']);
  SetValue(FElecBox.ElecMeter.TerminalInfoByName['Ic+'], FElecBox.ElecMeter.TerminalInfoByName['Ic-']);

  SetValue(FElecBox.ElecMeter.TerminalInfoByName['Ia-'], FElecBox.ElecLineBox.BoxIA.OutLineCurrent3);
  SetValue(FElecBox.ElecMeter.TerminalInfoByName['Ib-'], FElecBox.ElecLineBox.BoxIB.OutLineCurrent3);
  SetValue(FElecBox.ElecMeter.TerminalInfoByName['Ic-'], FElecBox.ElecLineBox.BoxIC.OutLineCurrent3);

  SetValue(FElecBox.ElecLineBox.BoxIA.OutLineCurrent3, FElecBox.ElecLineBox.BoxIA.InLineCurrent2);
  SetValue(FElecBox.ElecLineBox.BoxIB.OutLineCurrent3, FElecBox.ElecLineBox.BoxIB.InLineCurrent2);
  SetValue(FElecBox.ElecLineBox.BoxIC.OutLineCurrent3, FElecBox.ElecLineBox.BoxIC.InLineCurrent2);

  SetValue(FElecBox.ElecLineBox.BoxIA.InLineCurrent2, FElecBox.ElecTA1.SecondCurrentL);
  SetValue(FElecBox.ElecLineBox.BoxIB.InLineCurrent2, FElecBox.ElecTA2.SecondCurrentL);
  SetValue(FElecBox.ElecLineBox.BoxIC.InLineCurrent2, FElecBox.ElecTA3.SecondCurrentL);

  for i := 0 to FElecBox.ElecMeter.OrganList.Count - 1 do
  begin
    SetValue(FElecBox.ElecMeter.OrganInfo[i].CurrentPointIn,FElecBox.ElecMeter.OrganInfo[i].CurrentPointOut);
  end;







//
//  mmo1.Lines.Add(' TA 一次高端电流值===========');
//
//  SetValue(FElecBox.ElecTA1.FirstCurrentH);
//  SetValue(FElecBox.ElecTA2.FirstCurrentH);
//  SetValue(FElecBox.ElecTA3.FirstCurrentH);
//
//  mmo1.Lines.Add(' TA 一次低端电流值===========');
//  SetValue(FElecBox.ElecTA1.FirstCurrentL);
//  SetValue(FElecBox.ElecTA2.FirstCurrentL);
//  SetValue(FElecBox.ElecTA3.FirstCurrentL);
//
//  mmo1.Lines.Add(' TA 二次高端电流值===========');
//
//  SetValue(FElecBox.ElecTA1.SecondCurrentH);
//  SetValue(FElecBox.ElecTA2.SecondCurrentH);
//  SetValue(FElecBox.ElecTA3.SecondCurrentH);
//
//  mmo1.Lines.Add(' TA 二次低端电流值===========');
//  SetValue(FElecBox.ElecTA1.SecondCurrentL);
//  SetValue(FElecBox.ElecTA2.SecondCurrentL);
//  SetValue(FElecBox.ElecTA3.SecondCurrentL);

//  mmo1.Lines.Add(' 接线盒进线电压权值列表===========');
//  SetValue(FElecBox.ElecLineBox.BoxUA.InLineVol);
//  SetValue(FElecBox.ElecLineBox.BoxUB.InLineVol);
//  SetValue(FElecBox.ElecLineBox.BoxUC.InLineVol);
//  SetValue(FElecBox.ElecLineBox.BoxUN.InLineVol);
//
//  mmo1.Lines.Add(' 接线盒出线电压权值列表===========');
//  SetValue(FElecBox.ElecLineBox.BoxUA.OutLineVol);
//  SetValue(FElecBox.ElecLineBox.BoxUB.OutLineVol);
//  SetValue(FElecBox.ElecLineBox.BoxUC.OutLineVol);
//  SetValue(FElecBox.ElecLineBox.BoxUN.OutLineVol);










//  for i := 0 to FElecBox.ElecMeter.OrganList.Count - 1 do
//  begin
////    SetValue(AMeter.OrganInfo[i].VolPointIn);
////    SetValue(AMeter.OrganInfo[i].VolPointOut);
//
//    SetValue(FElecBox.ElecMeter.OrganInfo[i].CurrentPointIn);
//    SetValue(FElecBox.ElecMeter.OrganInfo[i].CurrentPointOut);
//  end;


//var
//  i : Integer;
//  APoint : TElecPoint;
//begin
//  mmo1.Lines.Add(' 电流值列表=====================');
//  for i := 0 to FElecBox.ElecMeter.OrganList.Count - 1 do
//  begin
//    APoint := FElecBox.ElecMeter.OrganInfo[i].GetOrganCurrent;
//
//    mmo1.Lines.Add(FElecBox.ElecMeter.OrganInfo[i].OrganName +
//      '电流：' + FormatFloat('0.0', APoint.Value) +
//      '角度：' + FormatFloat('0.0', APoint.Angle));
//  end;
end;

procedure TForm3.btn4Click(Sender: TObject);
  procedure SetValue(AElecLine: TElecLine);
  var
    AWValue : TWeightValue;
    j : Integer;
    nValue : Integer;
  begin
    TryStrToInt(edt1.Text, nValue);
    for j := 0 to AElecLine.Current.WValueList.count - 1 do
    begin
      AWValue := TWeightValue(AElecLine.Current.WValueList.Objects[j]);

      if AWValue.WID = nValue then
      begin
        mmo1.Lines.Add(AElecLine.LineName +'[' + IntToStr(AWValue.WID) + ',' +
          IntToStr(AWValue.WValue) +']');
      end;

    end;
  end;
var
  i : Integer;
begin
  mmo1.Lines.Add('电缆权值列表');
  SetValue(FElecBox.ElecBusLine.BusLineA);
  SetValue(FElecBox.ElecBusLine.BusLineB);
  SetValue(FElecBox.ElecBusLine.BusLineC);
  SetValue(FElecBox.ElecBusLine.BusLineN);

  mmo1.Lines.Add(' TA 一次高端权值列表===========');

  SetValue(FElecBox.ElecTA1.FirstCurrentH);
  SetValue(FElecBox.ElecTA2.FirstCurrentH);
  SetValue(FElecBox.ElecTA3.FirstCurrentH);

  mmo1.Lines.Add(' TA 一次低端权值列表===========');
  SetValue(FElecBox.ElecTA1.FirstCurrentL);
  SetValue(FElecBox.ElecTA2.FirstCurrentL);
  SetValue(FElecBox.ElecTA3.FirstCurrentL);



  mmo1.Lines.Add(' TA 二次低端权值列表===========');
  SetValue(FElecBox.ElecTA1.SecondCurrentL);
  SetValue(FElecBox.ElecTA2.SecondCurrentL);
  SetValue(FElecBox.ElecTA3.SecondCurrentL);

  mmo1.Lines.Add(' 接线盒进线2===========');
  SetValue(FElecBox.ElecLineBox.BoxIA.InLineCurrent2);
  SetValue(FElecBox.ElecLineBox.BoxIB.InLineCurrent2);
  SetValue(FElecBox.ElecLineBox.BoxIC.InLineCurrent2);


  mmo1.Lines.Add(' 接线盒出线3===========');
  SetValue(FElecBox.ElecLineBox.BoxIA.OutLineCurrent3);
  SetValue(FElecBox.ElecLineBox.BoxIB.OutLineCurrent3);
  SetValue(FElecBox.ElecLineBox.BoxIC.OutLineCurrent3);

  mmo1.Lines.Add(' 电表端子低端===========');
  SetValue(FElecBox.ElecMeter.TerminalInfoByName['Ia-']);
  SetValue(FElecBox.ElecMeter.TerminalInfoByName['Ib-']);
  SetValue(FElecBox.ElecMeter.TerminalInfoByName['Ic-']);


  mmo1.Lines.Add(' 电表原件===========');
  for i := 0 to FElecBox.ElecMeter.OrganList.Count - 1 do
  begin
    SetValue(FElecBox.ElecMeter.OrganInfo[i].CurrentPointOut);
    SetValue(FElecBox.ElecMeter.OrganInfo[i].CurrentPointIn);

  end;

  mmo1.Lines.Add(' 电表端子高===========');
  SetValue(FElecBox.ElecMeter.TerminalInfoByName['Ia+']);
  SetValue(FElecBox.ElecMeter.TerminalInfoByName['Ib+']);
  SetValue(FElecBox.ElecMeter.TerminalInfoByName['Ic+']);

  mmo1.Lines.Add(' 接线盒出线1===========');
  SetValue(FElecBox.ElecLineBox.BoxIA.OutLineCurrent1);
  SetValue(FElecBox.ElecLineBox.BoxIB.OutLineCurrent1);
  SetValue(FElecBox.ElecLineBox.BoxIC.OutLineCurrent1);

  mmo1.Lines.Add(' 接线盒进线1===========');
  SetValue(FElecBox.ElecLineBox.BoxIA.InLineCurrent1);
  SetValue(FElecBox.ElecLineBox.BoxIB.InLineCurrent1);
  SetValue(FElecBox.ElecLineBox.BoxIC.InLineCurrent1);

  mmo1.Lines.Add(' TA 二次高端权值列表===========');
  SetValue(FElecBox.ElecTA1.SecondCurrentH);
  SetValue(FElecBox.ElecTA2.SecondCurrentH);
  SetValue(FElecBox.ElecTA3.SecondCurrentH);

end;

procedure TForm3.btn5Click(Sender: TObject);
begin
  mmo1.Lines.Clear;
end;

procedure TForm3.btn6Click(Sender: TObject);
begin
  FElecBox.RefurshValue;
end;

procedure TForm3.btn7Click(Sender: TObject);
var
  i : Integer;
begin
  mmo1.Lines.Add(' 功率值列表=====================');

  mmo1.Lines.Add('有功功率:' + FormatFloat('0.00', FElecBox.ElecMeter.ActivePower));
  mmo1.Lines.Add('无功功率:' + FormatFloat('0.00', FElecBox.ElecMeter.ReactivePower));
  mmo1.Lines.Add('功率因数:' + FormatFloat('0.00', FElecBox.ElecMeter.PowerFactor));
  mmo1.Lines.Add('正向有功功率:' + FormatFloat('0.00', FElecBox.ElecMeter.PositiveActivePower));
  mmo1.Lines.Add('反向有功功率:' + FormatFloat('0.00', FElecBox.ElecMeter.ReverseActivePower));
  mmo1.Lines.Add('正向无功功率:' + FormatFloat('0.00', FElecBox.ElecMeter.PositiveReactivePower));
  mmo1.Lines.Add('反向无功功率:' + FormatFloat('0.00', FElecBox.ElecMeter.ReverseReactivePower));

  for i := 1 to 4 do
    mmo1.Lines.Add('第'+inttostr(i)+'象限无功功率:' + FormatFloat('0.00', FElecBox.ElecMeter.GetQuadrantReactivePower(i)));
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  FElecPoint := TElecPoint.Create;
  FElecBox := TElecBox.Create;
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  FElecBox.Free;
  FElecPoint.Free;
end;

end.
