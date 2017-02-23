unit FrmWESelect1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, ExtCtrls, ComCtrls, U_WIRING_ERROR;

type
  TfWESelect1 = class(TForm)
    pgcErrors: TPageControl;
    tsPhase3: TTabSheet;
    GroupBox1: TGroupBox;
    Label9: TLabel;
    pnl3BrokenS: TPanel;
    chk3BrokenSa: TCheckBox;
    chk3BrokenSb: TCheckBox;
    chk3BrokenSc: TCheckBox;
    pnl3BrokenU: TPanel;
    chk3BrokenUc: TCheckBox;
    chk3BrokenUb: TCheckBox;
    chk3BrokenUa: TCheckBox;
    pnl3Uab: TPanel;
    rb3UabN: TRadioButton;
    rb3UabP: TRadioButton;
    pnl3Ucb: TPanel;
    rb3UcbP: TRadioButton;
    rb3UcbN: TRadioButton;
    pnl3UOrder: TPanel;
    rb3Uabc: TRadioButton;
    rb3Uacb: TRadioButton;
    rb3Ubca: TRadioButton;
    rb3Ubac: TRadioButton;
    rb3Ucab: TRadioButton;
    rb3Ucba: TRadioButton;
    GroupBox2: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    pnl3Ia: TPanel;
    rb3IaN: TRadioButton;
    rb3IaP: TRadioButton;
    pnl3Ic: TPanel;
    rb3IcP: TRadioButton;
    rb3IcN: TRadioButton;
    tsPhase4: TTabSheet;
    grp4Voltage: TGroupBox;
    lbl1: TLabel;
    pnl4BrokenU: TPanel;
    chk4BrokenUc: TCheckBox;
    chk4BrokenUb: TCheckBox;
    chk4BrokenUa: TCheckBox;
    chk4BrokenUn: TCheckBox;
    pnl4UOrder: TPanel;
    rb4Uabc: TRadioButton;
    rb4Uacb: TRadioButton;
    rb4Ubca: TRadioButton;
    rb4Ubac: TRadioButton;
    rb4Ucab: TRadioButton;
    rb4Ucba: TRadioButton;
    grp4Current: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    pnl4Ia: TPanel;
    rb4IaN: TRadioButton;
    rb4IaP: TRadioButton;
    pnl4Ib: TPanel;
    rb4IbP: TRadioButton;
    rb4IbN: TRadioButton;
    pnl4Ic: TPanel;
    rb4IcP: TRadioButton;
    rb4IcN: TRadioButton;
    pnl4IOrder: TPanel;
    rb4Iabc: TRadioButton;
    rb4Iacb: TRadioButton;
    rb4Ibca: TRadioButton;
    rb4Ibac: TRadioButton;
    rb4Icab: TRadioButton;
    rb4Icba: TRadioButton;
    sgd3Order: TStringGrid;
    lbl2: TLabel;
    lbl7: TLabel;
    lbl3: TLabel;
    lbl4: TLabel;
    lbl5: TLabel;
    lbl6: TLabel;
    lbl8: TLabel;
    lbl9: TLabel;
    lbl10: TLabel;
    lbl11: TLabel;
    lbl12: TLabel;
    procedure ChangeStatus(Sender : TObject);
    procedure FormCreate(Sender: TObject);
    procedure sgd3OrderDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure sgd3OrderClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pgcErrorsChange(Sender: TObject);
  private
    { Private declarations }
    bSetStatus : Boolean;

    /// <summary>
    /// 保存状态
    /// </summary>
    procedure SaveStatusFour;
    procedure SaveStatusThree;

    /// <summary>
    /// 设置状态
    /// </summary>
    procedure SetStatusFour;
    procedure SetStatusThree;
  public
    { Public declarations }
    WiringError : TWIRING_ERROR;
    procedure SetStatus( AWiringError : TWIRING_ERROR );
    procedure SetPhaseType( APhaseType : TWE_PHASE_TYPE );
  end;

var
  fWESelect1: TfWESelect1;

implementation

{$R *.dfm}

{ TfWESelect1 }

procedure TfWESelect1.ChangeStatus(Sender: TObject);
begin
  if WiringError.PhaseType = ptThree then
    SaveStatusThree
  else
    SaveStatusFour;

  Caption := WiringError.IDInStr;

  if Assigned( WiringError.OnChanged ) then
    WiringError.OnChanged( WiringError );
end;

procedure TfWESelect1.FormCreate(Sender: TObject);
begin
  WiringError := TWIRING_ERROR.Create;

  with sgd3Order do
  begin
    Cells[1, 0] := 'Ia';
    Cells[2, 1] := 'Ic';
    Cells[0, 0] := '元件1';
    Cells[0, 1] := '元件2';
  end;
end;

procedure TfWESelect1.FormDestroy(Sender: TObject);
begin
  WiringError.Free;
end;

procedure TfWESelect1.sgd3OrderDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  with sgd3Order do
  begin
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(Rect);
    Canvas.Font.Name := '宋体';
    Canvas.Font.Size := 10;
    DrawText(Canvas.Handle, PChar(Cells[ACol,ARow]), Length(Cells[ACol,ARow]),
          Rect,DT_CENTER or DT_SINGLELINE or DT_VCENTER);
  end;
end;

procedure TfWESelect1.sgd3OrderClick(Sender: TObject);
begin
  with sgd3Order do
  begin
    if Col > 0 then
    begin
      if Cells[Col, Row] <> '' then
      begin
        if Pos( '-', Cells[ Col, Row ] ) > 0 then
        begin
          Cells[ Col, Row ] := Copy( Cells[ Col, Row ], 2, 2 );
        end
        else
        begin
          Cells[ Col, Row ] := '-' + Cells[ Col, Row ];
        end;
      end
      else
      begin
        if (( Col = 1 ) and ( Row = 0 )) or
           (( Col = 2 ) and ( Row = 1 )) then
        begin
          Cells[1, 0] := 'Ia';
          Cells[1, 1] := '';
          Cells[2, 0] := '';
          Cells[2, 1] := 'Ic';
        end
        else
        begin
          Cells[1, 0] := '';
          Cells[1, 1] := 'Ia';
          Cells[2, 0] := 'Ic';
          Cells[2, 1] := '';
        end;
      end;
    end;
  end;

  ChangeStatus(Self);
end;

procedure TfWESelect1.FormShow(Sender: TObject);
begin
  ChangeStatus( nil );
end;

procedure TfWESelect1.pgcErrorsChange(Sender: TObject);
begin
  WiringError.PhaseType := TWE_PHASE_TYPE( pgcErrors.TabIndex );
  ChangeStatus( nil );
end;

procedure TfWESelect1.SaveStatusFour;
begin
  if bSetStatus then    // 如果是设置状态，则不保存状态
    Exit;

  with WiringError do
  begin
    Clear;
    
    // 断相
    UaBroken := chk4BrokenUa.State = cbchecked;
    UbBroken := chk4BrokenUb.State = cbchecked;
    UcBroken := chk4BrokenUc.State = cbchecked;
    UnBroken := chk4BrokenUn.State = cbchecked;

    // 相序 U
    if rb4Uabc.Checked then USequence := stABC;
    if rb4Uacb.Checked then USequence := stACB;
    if rb4Ubac.Checked then USequence := stBAC;
    if rb4Ubca.Checked then USequence := stBCA;
    if rb4Ucab.Checked then USequence := stCAB;
    if rb4Ucba.Checked then USequence := stCBA;

    // 电流反向
    CT1Reverse := rb4IaN.Checked ;
    CT2Reverse := rb4IbN.Checked ;
    CT3Reverse := rb4IcN.Checked ;

    // 相序 I
    if rb4Iabc.Checked then ISequence := stABC;
    if rb4Iacb.Checked then ISequence := stACB;
    if rb4Ibac.Checked then ISequence := stBAC;
    if rb4Ibca.Checked then ISequence := stBCA;
    if rb4Icab.Checked then ISequence := stCAB;
    if rb4Icba.Checked then ISequence := stCBA;
  end;
end;

procedure TfWESelect1.SaveStatusThree;
begin
  if bSetStatus then    // 如果是设置状态，则不保存状态
    Exit;

  with WiringError do
  begin
    Clear;

    // 相序 U
    if rb3Uabc.Checked then USequence := stABC;
    if rb3Uacb.Checked then USequence := stACB;
    if rb3Ubac.Checked then USequence := stBAC;
    if rb3Ubca.Checked then USequence := stBCA;
    if rb3Ucab.Checked then USequence := stCAB;
    if rb3Ucba.Checked then USequence := stCBA;

    // 一次断相
    UaBroken := chk3BrokenSa.State = cbchecked;
    UbBroken := chk3BrokenSb.State = cbchecked;
    UcBroken := chk3BrokenSc.State = cbchecked;

    // 二次断相
    UsaBroken := chk3BrokenUa.State = cbchecked;
    UsbBroken := chk3BrokenUb.State = cbchecked;
    UscBroken := chk3BrokenUc.State = cbchecked;

    // 电压电流正反向
    PT1Reverse := rb3UabN.Checked;
    PT2Reverse := rb3UcbN.Checked;

    CT1Reverse := rb3IaN.Checked;
    CT2Reverse := rb3IcN.Checked;

    // 表尾电流接线
    if sgd3Order.Cells[1, 1] = '' then   // Ia 进 1 元件
    begin
      if ( sgd3Order.Cells[1, 0] = '-Ia' ) then  // Ia 反进
      begin
        I1In := plN;
        I1Out := plA;
      end
      else
      begin
        I1In := plA;
        I1Out := plN;
      end;

      if ( sgd3Order.Cells[2, 1] = '-Ic') then  // Ic 反进
      begin
        I2In := plN;
        I2Out := plC;
      end
      else
      begin
        I2In := plC;
        I2Out := plN;
      end;
    end
    else
    begin
      if ( sgd3Order.Cells[1, 1] = '-Ia' ) then  // 反进
      begin
        I2In := plN;
        I2Out := plA;
      end
      else
      begin
        I2In := plA;
        I2Out := plN;
      end;

      if ( sgd3Order.Cells[2, 0] = '-Ic') then
      begin
        I1In := plN;
        I1Out := plC;
      end
      else
      begin
        I1In := plC;
        I1Out := plN;
      end;
    end;  
  end;
end;

procedure TfWESelect1.SetPhaseType(APhaseType: TWE_PHASE_TYPE);
begin
  WiringError.PhaseType := APhaseType;

  if WiringError.PhaseType = ptFour then
  begin
    tsPhase3.TabVisible := False;
    tsPhase4.TabVisible := True;
    pgcErrors.ActivePage := tsPhase4;
  end
  else
  begin
    tsPhase3.TabVisible := True;
    tsPhase4.TabVisible := False;
    pgcErrors.ActivePage := tsPhase3;
  end;
end;

procedure TfWESelect1.SetStatus( AWiringError : TWIRING_ERROR );
begin
  SetFocus;
  bSetStatus := True;  // 开始设置状态
  WiringError.Assign( AWiringError );

  if WiringError.PhaseType = ptThree then
  begin
    SetStatusThree;
    tsPhase3.Show;
  end
  else
  begin
    SetStatusFour;
    tsPhase4.Show;
  end;

  bSetStatus := False;
  ChangeStatus( nil );
end;

procedure TfWESelect1.SetStatusFour;
begin
  // 三相四线
  with WiringError do
  begin
    chk4BrokenUa.State := TCheckBoxState( UaBroken );
    chk4BrokenUb.State := TCheckBoxState( UbBroken );
    chk4BrokenUc.State := TCheckBoxState( UcBroken );
    chk4BrokenUn.State := TCheckBoxState( UnBroken );

    case USequence of
      stABC: rb4Uabc.Checked := True;
      stACB: rb4Uacb.Checked := True;
      stBAC: rb4Ubac.Checked := True;
      stBCA: rb4Ubca.Checked := True;
      stCAB: rb4Ucab.Checked := True;
      stCBA: rb4Ucba.Checked := True;
    end;

    case ISequence of
      stABC: rb4Iabc.Checked := True;
      stACB: rb4Iacb.Checked := True;
      stBAC: rb4Ibac.Checked := True;
      stBCA: rb4Ibca.Checked := True;
      stCAB: rb4Icab.Checked := True;
      stCBA: rb4Icba.Checked := True;
    end;

    // 电流正反向
    if CT1Reverse then
      rb4IaN.Checked := True
    else
      rb4IaP.Checked := True;

    if CT2Reverse then
      rb4IbN.Checked := True
    else
      rb4IbP.Checked := True;

    if CT3Reverse then
      rb4IcN.Checked := True
    else
      rb4IcP.Checked := True;
  end;
end;

procedure TfWESelect1.SetStatusThree;
begin
  with WiringError do
  begin
    chk3BrokenSa.State := TCheckBoxState( UaBroken );
    chk3BrokenSb.State := TCheckBoxState( UbBroken );
    chk3BrokenSc.State := TCheckBoxState( UcBroken );
    chk3BrokenUa.State := TCheckBoxState( UsaBroken );
    chk3BrokenUb.State := TCheckBoxState( UsbBroken );
    chk3BrokenUc.State := TCheckBoxState( UscBroken );

    // 电压反
    if PT1Reverse then
      rb3UabN.Checked := True
    else
      rb3UabP.Checked := True;

    if PT2Reverse then
      rb3UcbN.Checked := True
    else
      rb3UcbP.Checked := True;

    // 电流反
    if CT1Reverse then
      rb3IaN.Checked := True
    else
      rb3IaP.Checked := True;

    if CT2Reverse then
      rb3IcN.Checked := True
    else
      rb3IcP.Checked := True;

    case USequence of
      stABC: rb3Uabc.Checked := True;
      stACB: rb3Uacb.Checked := True;
      stBAC: rb3Ubac.Checked := True;
      stBCA: rb3Ubca.Checked := True;
      stCAB: rb3Ucab.Checked := True;
      stCBA: rb3Ucba.Checked := True;
    end;

    if ( I1In = plA ) or ( I1Out = plA ) then  // Ia进一元件
    begin
      sgd3Order.Cells[1, 1] := '';
      sgd3Order.Cells[2, 0] := '';

      if I1In = plA then
        sgd3Order.Cells[1, 0] := 'Ia'
      else
        sgd3Order.Cells[1, 0] := '-Ia';

      if I2In = plC then
        sgd3Order.Cells[2, 1] := 'Ic'
      else
        sgd3Order.Cells[2, 1] := '-Ic';
    end
    else // Ic 进 一元件
    begin
      sgd3Order.Cells[1, 0] := '';
      sgd3Order.Cells[2, 1] := '';

      if I1In = plC then
        sgd3Order.Cells[2, 0] := 'Ic'
      else
        sgd3Order.Cells[2, 0] := '-Ic';

      if I2In = plA then
        sgd3Order.Cells[1, 1] := 'Ia'
      else
        sgd3Order.Cells[1, 1] := '-Ia';
    end;
  end;
end;

end.
