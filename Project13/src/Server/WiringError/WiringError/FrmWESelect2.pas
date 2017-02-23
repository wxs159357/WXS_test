unit FrmWESelect2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Grids, StdCtrls, ComCtrls, ImgList, Buttons, Contnrs,
  Menus, U_WIRING_ERROR, IniFiles, U_CONFIG;

type
  TfWESelect2 = class(TForm)
    pgcErrors: TPageControl;
    tsPhase3: TTabSheet;
    GroupBox1: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
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
    pnl3UOrder: TPanel;
    rb3Uabc: TRadioButton;
    rb3Uacb: TRadioButton;
    rb3Ubca: TRadioButton;
    rb3Ubac: TRadioButton;
    rb3Ucab: TRadioButton;
    rb3Ucba: TRadioButton;
    GroupBox2: TGroupBox;
    Label11: TLabel;
    pnl3CTPN: TPanel;
    tsPhase4: TTabSheet;
    grp4Voltage: TGroupBox;
    lbl1: TLabel;
    Label1: TLabel;
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
    Label4: TLabel;
    pnl4CTPN: TPanel;
    pnl4IOrder: TPanel;
    rb4Iabc: TRadioButton;
    rb4Iacb: TRadioButton;
    rb4Ibca: TRadioButton;
    rb4Ibac: TRadioButton;
    rb4Icab: TRadioButton;
    rb4Icba: TRadioButton;
    chk3PTPN1: TCheckBox;
    chk3PTPN2: TCheckBox;
    chk3CTPN1: TCheckBox;
    chk3CTPN2: TCheckBox;
    lbl3: TLabel;
    lbl4: TLabel;
    pnl3CTOpen: TPanel;
    chk3CTOpenA: TCheckBox;
    chk3CTOpenC: TCheckBox;
    pnlCTShort: TPanel;
    chk3CTShort1: TCheckBox;
    chk3CTShort2: TCheckBox;
    lbl5: TLabel;
    lbl7: TLabel;
    lbl8: TLabel;
    lbl9: TLabel;
    chk4CTPN1: TCheckBox;
    chk4CTPN2: TCheckBox;
    chk4CTPN3: TCheckBox;
    pnl4CTOpen: TPanel;
    chk4CTOpen1: TCheckBox;
    chk4CTOpen2: TCheckBox;
    chk4CTOpen3: TCheckBox;
    pnl4CTShort: TPanel;
    chk4CTShort1: TCheckBox;
    chk4CTShort2: TCheckBox;
    chk4CTShort3: TCheckBox;
    lbl10: TLabel;
    lbl11: TLabel;
    lbl12: TLabel;
    lbl13: TLabel;
    chk3CTOpenN: TCheckBox;
    tsPhase4PT: TTabSheet;
    grp1: TGroupBox;
    lbl15: TLabel;
    lbl16: TLabel;
    lbl17: TLabel;
    lbl18: TLabel;
    lbl19: TLabel;
    pnl1: TPanel;
    chk4PTBrokenUc: TCheckBox;
    chk4PTBrokenUb: TCheckBox;
    chk4PTBrokenUa: TCheckBox;
    chk4PTBrokenUn: TCheckBox;
    pnl2: TPanel;
    rb4PTUabc: TRadioButton;
    rb4PTUacb: TRadioButton;
    rb4PTUbca: TRadioButton;
    rb4PTUbac: TRadioButton;
    rb4PTUcab: TRadioButton;
    rb4PTUcba: TRadioButton;
    pnl3: TPanel;
    chk4PTBrokenUsc: TCheckBox;
    chk4PTBrokenUsb: TCheckBox;
    chk4PTBrokenUsa: TCheckBox;
    chk4PTBrokenUsn: TCheckBox;
    chk4PTGround: TCheckBox;
    pnl4: TPanel;
    chk4PT1: TCheckBox;
    chk4PT2: TCheckBox;
    chk4PT3: TCheckBox;
    grp2: TGroupBox;
    lbl20: TLabel;
    lbl21: TLabel;
    lbl22: TLabel;
    lbl23: TLabel;
    lbl24: TLabel;
    pnl4PTCTPN: TPanel;
    chk4PTCTPN1: TCheckBox;
    chk4PTCTPN2: TCheckBox;
    chk4PTCTPN3: TCheckBox;
    pnl4PTIOrder: TPanel;
    rb4PTIabc: TRadioButton;
    rb4PTIacb: TRadioButton;
    rb4PTIbca: TRadioButton;
    rb4PTIbac: TRadioButton;
    rb4PTIcab: TRadioButton;
    rb4PTIcba: TRadioButton;
    pnl4PTCTOpen: TPanel;
    chk4PTCTOpen1: TCheckBox;
    chk4PTCTOpen2: TCheckBox;
    chk4PTCTOpen3: TCheckBox;
    pnl4PTCTShort: TPanel;
    chk4PTCTShort1: TCheckBox;
    chk4PTCTShort2: TCheckBox;
    chk4PTCTShort3: TCheckBox;
    pnl5: TPanel;
    pnl6: TPanel;
    chckbxIaGroundBroken3: TCheckBox;
    chckbxIcGroundBroken3: TCheckBox;
    lbl2: TLabel;
    lbl6: TLabel;
    pnl7: TPanel;
    chckbxIaGroundBroken4: TCheckBox;
    chckbxIbGroundBroken4: TCheckBox;
    chckbxIcGroundBroken4: TCheckBox;
    lbl14: TLabel;
    pnl8: TPanel;
    chckbxIaGroundBroken4PT: TCheckBox;
    chckbxIbGroundBroken4PT: TCheckBox;
    chckbxIcGroundBroken4PT: TCheckBox;
    chk3Ground: TCheckBox;
    chkI3Ground: TCheckBox;
    chkI4Ground: TCheckBox;
    chkI4PTGround: TCheckBox;
    lbl25: TLabel;
    pnl9: TPanel;
    chkI2Reverse: TCheckBox;
    chkI1Reverse: TCheckBox;
    pnl10: TPanel;
    chk4I2Reverse: TCheckBox;
    chk4I1Reverse: TCheckBox;
    lbl26: TLabel;
    chk4I3Reverse: TCheckBox;
    lbl27: TLabel;
    pnl11: TPanel;
    chk4I2ReversePT: TCheckBox;
    chk4I1ReversePT: TCheckBox;
    chk4I3ReversePT: TCheckBox;
    rbIcIa: TRadioButton;
    rbIaIc: TRadioButton;
    rbIcIb: TRadioButton;
    rbIaIb: TRadioButton;
    rbIbIa: TRadioButton;
    rbIbIc: TRadioButton;
    strngrd1: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure ChangeStatus(Sender : TObject);
    procedure sgd3OrderDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure sgd3OrderClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pgcErrorsChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbb1Change(Sender: TObject);
    procedure strngrd1SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
  private
    { Private declarations }
    bSetStatus : Boolean;
    Fabc : Integer;
    Fptct : Boolean;
    FIsShowCurrentGround: Boolean;

    /// <summary>
    /// 保存状态
    /// </summary>
    procedure SaveStatusFour;
    procedure SaveStatusThree;
    procedure SaveStatusFourPT;

    /// <summary>
    /// 设置状态
    /// </summary>
    procedure SetStatusFour;
    procedure SetStatusThree;
    procedure SetStatusFourPT;

    /// <summary>
    /// 显示喜好界面
    /// </summary>
    procedure ShowLikeUI(babc: Integer; bPtct : Boolean);
  public
    { Public declarations }
    WiringError : TWIRING_ERROR;
    procedure SetPhaseType( APhaseType : TWE_PHASE_TYPE );
    procedure SetStatus( AWiringError : TWIRING_ERROR );
    /// <summary>
    /// 是否显示电流接地
    /// </summary>
    property IsShowCurrentGround : Boolean read FIsShowCurrentGround write FIsShowCurrentGround;
  end;

var
  fWESelect2 : TfWESelect2;

implementation

{$R *.dfm}

{ TfWESelect2 }

procedure TfWESelect2.FormCreate(Sender: TObject);
begin
  with TIniFile.Create( ChangeFileExt( Application.ExeName, '.ini' ) ) do
  begin
    Fabc := ReadInteger( 'Like', 'abc', 0 );
    Fptct := ReadBool( 'Like', 'PTCT', True );
    Free;
  end;

  WiringError := TWIRING_ERROR.Create;

  with strngrd1 do
  begin
    if Fabc = 0 then
    begin
      Cells[0, 1] := 'A';
      Cells[0, 2] := 'B';
      Cells[0, 3] := 'C';
      Cells[0, 4] := 'N';
    end
    else if Fabc = 1 then
    begin
      Cells[0, 1] := 'U';
      Cells[0, 2] := 'V';
      Cells[0, 3] := 'W';
      Cells[0, 4] := 'N';
    end
    else
    begin
      Cells[0, 1] := '1';
      Cells[0, 2] := '2';
      Cells[0, 3] := '3';
      Cells[0, 4] := 'N';
    end;
    Cells[1, 0] := 'I1';
    Cells[2, 0] := 'I2';
    Cells[3, 0] := 'I3';
  end;


//  with sgd3Order do
//  begin
//    ColWidths[ 0 ] := 53;
//    if Fabc then
//    begin
//      Cells[1, 0] := 'Ia';
//      Cells[2, 0] := 'Ic';
//    end
//    else
//    begin
//      Cells[1, 0] := 'Iu';
//      Cells[2, 0] := 'Iw';
//    end;
//
//    Cells[3, 0] := 'In';
//
//    Cells[0, 1] := '1元件进';
//    Cells[0, 2] := '1元件出';
//    Cells[0, 3] := '2元件进';
//    Cells[0, 4] := '2元件出';
//
//    Cells[ 1, 1 ] := '*';
//    Cells[ 3, 2 ] := '*';
//    Cells[ 2, 3 ] := '*';
//    Cells[ 3, 4 ] := '*';
//  end;

  ShowLikeUI(Fabc, Fptct);
  FIsShowCurrentGround := False;
end;

procedure TfWESelect2.FormDestroy(Sender: TObject);
begin
  WiringError.Free;
end;

procedure TfWESelect2.cbb1Change(Sender: TObject);
begin
  ChangeStatus(Sender);
end;

procedure TfWESelect2.ChangeStatus(Sender: TObject);
begin
  if WiringError.PhaseType = ptThree then
    SaveStatusThree
  else if WiringError.PhaseType = ptFour then
    SaveStatusFour
  else //ptFourPT 胡红明2013.5.13
    SaveStatusFourPT;
  Caption := WiringError.IDInStr;

  if Assigned( WiringError.OnChanged ) then
    WiringError.OnChanged( WiringError );


end;

procedure TfWESelect2.sgd3OrderDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
//  with sgd3Order do
//  begin
//    Canvas.Brush.Color := clBtnFace;
//    Canvas.FillRect(Rect);
//    Canvas.Font.Name := '宋体';
//    Canvas.Font.Size := 10;
//    DrawText(Canvas.Handle, PChar(Cells[ACol,ARow]), Length(Cells[ACol,ARow]),
//          Rect,DT_CENTER or DT_SINGLELINE or DT_VCENTER);
//  end;
end;

procedure TfWESelect2.ShowLikeUI(babc: Integer; bPtct: Boolean);
var
  i: Integer;
begin

  for i := 0 to ComponentCount - 1 do
  begin

    if Components[i] is TRadioButton then
    begin
      case babc of
        0:
        begin

        end;
        1:
        begin
          TRadioButton(Components[i]).Caption :=
            StringReplace( TRadioButton(Components[i]).Caption, 'a', 'u',[] );
          TRadioButton(Components[i]).Caption :=
            StringReplace( TRadioButton(Components[i]).Caption, 'b', 'v',[] );

          TRadioButton(Components[i]).Caption :=
            StringReplace( TRadioButton(Components[i]).Caption, 'A', 'U',[] );
          TRadioButton(Components[i]).Caption :=
            StringReplace( TRadioButton(Components[i]).Caption, 'B', 'V',[] );

          if (Pos('t', TRadioButton(Components[i]).Caption) = 0) and
            (Pos('T', TRadioButton(Components[i]).Caption) = 0)  then
          begin
            TRadioButton(Components[i]).Caption :=
              StringReplace( TRadioButton(Components[i]).Caption, 'c', 'w',[] );

            TRadioButton(Components[i]).Caption :=
              StringReplace( TRadioButton(Components[i]).Caption, 'C', 'W',[] );
          end;
        end;
        2:
        begin
          TRadioButton(Components[i]).Caption :=
            StringReplace( TRadioButton(Components[i]).Caption, 'a', '1',[] );
          TRadioButton(Components[i]).Caption :=
            StringReplace( TRadioButton(Components[i]).Caption, 'b', '2',[] );

          TRadioButton(Components[i]).Caption :=
            StringReplace( TRadioButton(Components[i]).Caption, 'A', '1',[] );
          TRadioButton(Components[i]).Caption :=
            StringReplace( TRadioButton(Components[i]).Caption, 'B', '2',[] );

          if (Pos('t', TRadioButton(Components[i]).Caption) = 0) and
            (Pos('T', TRadioButton(Components[i]).Caption) = 0)  then
          begin
            TRadioButton(Components[i]).Caption :=
              StringReplace( TRadioButton(Components[i]).Caption, 'c', '3',[] );

            TRadioButton(Components[i]).Caption :=
              StringReplace( TRadioButton(Components[i]).Caption, 'C', '3',[] );
          end;
        end;
      end;
      {if NOT babc then
      begin
        TRadioButton(Components[i]).Caption :=
          StringReplace( TRadioButton(Components[i]).Caption, 'a', 'u',[] );
        TRadioButton(Components[i]).Caption :=
          StringReplace( TRadioButton(Components[i]).Caption, 'b', 'v',[] );

        TRadioButton(Components[i]).Caption :=
          StringReplace( TRadioButton(Components[i]).Caption, 'A', 'U',[] );
        TRadioButton(Components[i]).Caption :=
          StringReplace( TRadioButton(Components[i]).Caption, 'B', 'V',[] );

        if (Pos('t', TRadioButton(Components[i]).Caption) = 0) and
          (Pos('T', TRadioButton(Components[i]).Caption) = 0)  then
        begin
          TRadioButton(Components[i]).Caption :=
            StringReplace( TRadioButton(Components[i]).Caption, 'c', 'w',[] );

          TRadioButton(Components[i]).Caption :=
            StringReplace( TRadioButton(Components[i]).Caption, 'C', 'W',[] );
        end;
      end; }

      if not bPtct then
      begin
        TRadioButton(Components[i]).Caption :=
          StringReplace( TRadioButton(Components[i]).Caption, 'PT', 'TV',[] );
        TRadioButton(Components[i]).Caption :=
          StringReplace( TRadioButton(Components[i]).Caption, 'CT', 'TA',[] );
      end;
    end
    else if Components[i] is TCheckBox then
    begin
      case babc of
        0:
        begin

        end;
        1:
        begin
          TCheckBox(Components[i]).Caption :=
            StringReplace( TCheckBox(Components[i]).Caption, 'a', 'u',[] );
          TCheckBox(Components[i]).Caption :=
            StringReplace( TCheckBox(Components[i]).Caption, 'b', 'v',[] );

          TCheckBox(Components[i]).Caption :=
            StringReplace( TCheckBox(Components[i]).Caption, 'A', 'U',[] );
          TCheckBox(Components[i]).Caption :=
            StringReplace( TCheckBox(Components[i]).Caption, 'B', 'V',[] );


          if (Pos('t', TCheckBox(Components[i]).Caption) = 0) and
            (Pos('T', TCheckBox(Components[i]).Caption) = 0)  then
          begin
            TCheckBox(Components[i]).Caption :=
              StringReplace( TCheckBox(Components[i]).Caption, 'c', 'w',[] );

            TCheckBox(Components[i]).Caption :=
              StringReplace( TCheckBox(Components[i]).Caption, 'C', 'W',[] );
          end;
        end;
        2:
        begin
          TCheckBox(Components[i]).Caption :=
            StringReplace( TCheckBox(Components[i]).Caption, 'a', '1',[] );
          TCheckBox(Components[i]).Caption :=
            StringReplace( TCheckBox(Components[i]).Caption, 'b', '2',[] );

          TCheckBox(Components[i]).Caption :=
            StringReplace( TCheckBox(Components[i]).Caption, 'A', '1',[] );
          TCheckBox(Components[i]).Caption :=
            StringReplace( TCheckBox(Components[i]).Caption, 'B', '2',[] );


          if (Pos('t', TCheckBox(Components[i]).Caption) = 0) and
            (Pos('T', TCheckBox(Components[i]).Caption) = 0)  then
          begin
            TCheckBox(Components[i]).Caption :=
              StringReplace( TCheckBox(Components[i]).Caption, 'c', '3',[] );

            TCheckBox(Components[i]).Caption :=
              StringReplace( TCheckBox(Components[i]).Caption, 'C', '3',[] );
          end;
        end;
      end;


      if not bPtct then
      begin
        TCheckBox(Components[i]).Caption :=
          StringReplace( TCheckBox(Components[i]).Caption, 'PT', 'TV',[] );
        TCheckBox(Components[i]).Caption :=
          StringReplace( TCheckBox(Components[i]).Caption, 'CT', 'TA',[] );
      end;
    end;
  end;
end;

procedure TfWESelect2.strngrd1SelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
  function GetValue(n1, n2: Integer) : Integer;
  var
    i: Integer;
  begin
    Result := 1;
    for i := 1 to 4 do
    begin
      if (n1 <> i) and (n2 <> i) then
      begin
        Result := i;
        Break;
      end;
    end;
  end;
  function GetRowID(nCol : Integer) : Integer;
  var
    i: Integer;
  begin
    Result := 0;
    for i := 1 to 4 do
    begin
      if strngrd1.Cells[nCol, i] = '●' then
      begin
        Result := i;
      end;
    end;
  end;
var
  nI1ID, nI2ID, nI3ID : Integer;
  k : Integer;
  l: Integer;
begin
  nI1ID := GetRowID(1);
  nI2ID := GetRowID(2);
  nI3ID := GetRowID(3);

//  for k := 1 to 4 do
//    strngrd1.Cells[ACol, k] := '';
//  strngrd1.Cells[ACol, ARow] := '●';

  case ACol of
    1 :
    begin
      nI1ID := ARow;
      if nI2ID = ARow then
      begin
        nI2ID := GetValue(nI1ID, nI3ID);
      end;
      if nI3ID = ARow then
      begin
        nI3ID := GetValue(nI1ID, nI2ID);
      end;
    end;
    2 :
    begin
      nI2ID := ARow;
      if nI1ID = ARow then
      begin
        nI1ID := GetValue(nI2ID, nI3ID);
      end;
      if nI3ID = ARow then
      begin
        nI3ID := GetValue(nI1ID, nI2ID);
      end;
    end;
    3 :
    begin
      nI3ID := ARow;
      if nI1ID = ARow then
      begin
        nI1ID := GetValue(nI2ID, nI3ID);
      end;
      if nI2ID = ARow then
      begin
        nI2ID := GetValue(nI1ID, nI3ID);
      end;
    end;
  end;

  for l := 1 to 3 do
    for k := 1 to 4 do
      strngrd1.Cells[l, k] := '';

  if nI1ID <> 0 then
    strngrd1.Cells[ 1, nI1ID ] := '●';
  if nI2ID <> 0 then
    strngrd1.Cells[ 2, nI2ID ] := '●';
  if nI3ID <> 0 then
    strngrd1.Cells[ 3, nI3ID ] := '●';

  ChangeStatus(Sender);
end;

procedure TfWESelect2.sgd3OrderClick(Sender: TObject);
//  function GetILineSelected( ARow : Integer ) : Integer;
//  var
//    i : Integer;
//  begin
//    Result := 0;
//
//    for i := 0 to 3 - 1 do
//      if sgd3Order.Cells[ i + 1, ARow ] <> '' then
//      begin
//        case i of
//          0 : Result := 1;
//          1 : Result := 2;
//          2 : Result := 3;
//        end;
//      end;
//  end;
//  procedure SetValue( ARow : Integer ; val : Integer);
//  var
//    i : Integer;
//  begin
//    for i := 0 to 3 - 1 do
//    begin
//      if (i+1)=val  then
//        sgd3Order.Cells[ i + 1, ARow ]:='*'
//      else
//        sgd3Order.Cells[ i + 1, ARow ]:='';
//    end;
//  end;
//  function getValue1Or2(val:integer):Integer;
//  begin
//    if val=1 then
//      Result:=2
//    else
//      Result:=1;
//  end;
//var
//  i : Integer;
//  temp : Integer;
//  oldVal:array[1..4] of Integer;
//const
//  IThree=3;
begin
//  with sgd3Order do
//  begin
//    if ( Col > 0 ) and ( Row > 0 ) then
//    begin
//      oldVal[1]:=GetILineSelected(1);
//      oldVal[2]:=GetILineSelected(2);
//      oldVal[3]:=GetILineSelected(3);
//      oldVal[4]:=GetILineSelected(4);
//
//      for i := 1 to 3 do
//      begin
//        if i = Col then
//          Cells[ i, Row ] := '*'
//        else
//          Cells[ i, Row ] := '';
//      end;
//      temp:=GetILineSelected(Row);
//      if temp<>oldVal[Row] then
//      case Row of
//        1:
//        begin
//          if temp=IThree then
//          begin
//            SetValue(2,oldVal[Row]);
//          end
//          else
//          begin
//            SetValue(2,IThree);
//            if oldVal[3]=IThree then
//              SetValue(4,getValue1Or2(temp))
//            else
//              SetValue(3,getValue1Or2(temp));
//          end;
//        end;
//        2:
//        begin
//          if temp=IThree then
//          begin
//            SetValue(1,oldVal[Row]);
//          end
//          else
//          begin
//            SetValue(1,IThree);
//            if oldVal[3]=IThree then
//              SetValue(4,getValue1Or2(temp))
//            else
//              SetValue(3,getValue1Or2(temp));
//          end;
//        end;
//        3:
//        begin
//          if temp=IThree then
//          begin
//            SetValue(4,oldVal[Row]);
//          end
//          else
//          begin
//            SetValue(4,IThree);
//            if oldVal[1]=IThree then
//              SetValue(2,getValue1Or2(temp))
//            else
//              SetValue(1,getValue1Or2(temp));
//          end;
//        end;
//        4:
//        begin
//          if temp=IThree then
//          begin
//            SetValue(3,oldVal[Row]);
//          end
//          else
//          begin
//            SetValue(3,IThree);
//            if oldVal[1]=IThree then
//              SetValue(2,getValue1Or2(temp))
//            else
//              SetValue(1,getValue1Or2(temp));
//          end;
//        end;
//      end;
//    end;
//  end;

  ChangeStatus(Self);
end;

procedure TfWESelect2.FormShow(Sender: TObject);
begin
  ChangeStatus( nil );

  lbl2.Visible := FIsShowCurrentGround;
  pnl6.Visible := FIsShowCurrentGround;
  lbl6.Visible := FIsShowCurrentGround;
  pnl7.Visible := FIsShowCurrentGround;
  lbl14.Visible := FIsShowCurrentGround;
  pnl8.Visible := FIsShowCurrentGround;

end;

procedure TfWESelect2.pgcErrorsChange(Sender: TObject);
begin
  WiringError.PhaseType := TWE_PHASE_TYPE( pgcErrors.TabIndex );
  ChangeStatus( nil );
end;

procedure TfWESelect2.SaveStatusFour;
begin
  if bSetStatus then    // 如果是设置状态，则不保存状态
    Exit;

  with WiringError do
  begin
    Clear;

    // 二次电流开路
    IaBroken := chk4CTOpen1.State = cbChecked;
    IbBroken := chk4CTOpen2.State = cbChecked;
    IcBroken := chk4CTOpen3.State = cbChecked;

    CT1Short := chk4CTShort1.State = cbChecked ;
    CT2Short := chk4CTShort2.State = cbChecked ;
    CT3Short := chk4CTShort3.State = cbChecked ;

    // 断相
    UsaBroken := chk4BrokenUa.State = cbchecked;
    UsbBroken := chk4BrokenUb.State = cbchecked;
    UscBroken := chk4BrokenUc.State = cbchecked;
    UsnBroken := chk4BrokenUn.State = cbchecked;

    // 相序 U
    if rb4Uabc.Checked then USequence := stABC;
    if rb4Uacb.Checked then USequence := stACB;
    if rb4Ubac.Checked then USequence := stBAC;
    if rb4Ubca.Checked then USequence := stBCA;
    if rb4Ucab.Checked then USequence := stCAB;
    if rb4Ucba.Checked then USequence := stCBA;

    // 电流反向
    CT1Reverse := chk4CTPN1.State = cbChecked ;
    CT2Reverse := chk4CTPN2.State = cbChecked ;
    CT3Reverse := chk4CTPN3.State = cbChecked ;

    // 相序 I
    if rb4Iabc.Checked then ISequence := stABC;
    if rb4Iacb.Checked then ISequence := stACB;
    if rb4Ibac.Checked then ISequence := stBAC;
    if rb4Ibca.Checked then ISequence := stBCA;
    if rb4Icab.Checked then ISequence := stCAB;
    if rb4Icba.Checked then ISequence := stCBA;

    // 表尾电流反接
    I1Reverse := chk4I1Reverse.Checked;
    I2Reverse := chk4I2Reverse.Checked;
    I3Reverse := chk4I3Reverse.Checked;

    // 电流接地断开
    IaGroundBroken := chckbxIaGroundBroken4.State = cbChecked;
    IbGroundBroken := chckbxIbGroundBroken4.State = cbChecked;
    IcGroundBroken := chckbxIcGroundBroken4.State = cbChecked;

    IGroundBroken := chkI4Ground.State = cbChecked;
  end;
end;

procedure TfWESelect2.SaveStatusFourPT;
var
  sI1, sI2, sI3: string;

  procedure GetISequenceFromValue;
    function GetRowID(nCol : Integer) : string;
    var
      i: Integer;
    begin
      Result := '';
      for i := 1 to 4 do
      begin
        if strngrd1.Cells[nCol, i] = '●' then
        begin
          Break;
        end;
      end;

      case i of
        1 : Result := 'A';
        2 : Result := 'B';
        3 : Result := 'C';
        4 : Result := 'N';

      end;
    end;
  begin
    sI1 := GetRowID(1);
    sI2 := GetRowID(2);
    sI3 := GetRowID(3);
  end;


begin
  if bSetStatus then    // 如果是设置状态，则不保存状态
    Exit;

  with WiringError do
  begin
    Clear;

    // 二次电流开路
    IaBroken := chk4PTCTOpen1.State = cbChecked;
    IbBroken := chk4PTCTOpen2.State = cbChecked;
    IcBroken := chk4PTCTOpen3.State = cbChecked;

    CT1Short := chk4PTCTShort1.State = cbChecked ;
    CT2Short := chk4PTCTShort2.State = cbChecked ;
    CT3Short := chk4PTCTShort3.State = cbChecked ;

    // 断相
    UaBroken := chk4PTBrokenUa.State = cbchecked;
    UbBroken := chk4PTBrokenUb.State = cbchecked;
    UcBroken := chk4PTBrokenUc.State = cbchecked;
    UnBroken := chk4PTBrokenUn.State = cbchecked;
    //二次断相和极性反， 胡红明2013.5.13
    // 二次断相
    UsaBroken := chk4PTBrokenUsa.State = cbchecked;
    UsbBroken := chk4PTBrokenUsb.State = cbchecked;
    UscBroken := chk4PTBrokenUsc.State = cbchecked;
    UsnBroken := chk4PTBrokenUsn.State = cbchecked;
    //地线断
    GroundBroken := chk4PTGround.State = cbChecked;

    //极性反
    PT1Reverse := chk4PT1.State = cbChecked;
    PT2Reverse := chk4PT2.State = cbChecked;
    PT3Reverse := chk4PT3.State = cbChecked;

    // 相序 U
    if rb4PTUabc.Checked then USequence := stABC;
    if rb4PTUacb.Checked then USequence := stACB;
    if rb4PTUbac.Checked then USequence := stBAC;
    if rb4PTUbca.Checked then USequence := stBCA;
    if rb4PTUcab.Checked then USequence := stCAB;
    if rb4PTUcba.Checked then USequence := stCBA;

    // 电流反向
    CT1Reverse := chk4PTCTPN1.State = cbChecked ;
    CT2Reverse := chk4PTCTPN2.State = cbChecked ;
    CT3Reverse := chk4PTCTPN3.State = cbChecked ;

    // 保存
//    // 简化接线
//    if IsClearLinke then
//    begin
      // 表尾电流反接
      I1Reverse := chk4I1Reversept.Checked;
      I2Reverse := chk4I2Reversept.Checked;
      I3Reverse := chk4I3Reversept.Checked;
      GetISequenceFromValue;
      SetClearLinkeISequence(sI1, sI2, sI3, I1Reverse, I2Reverse, I3Reverse);

//
//
//    end
//    else
//    begin
      // 相序 I
      if rb4PTIabc.Checked then ISequence := stABC;
      if rb4PTIacb.Checked then ISequence := stACB;
      if rb4PTIbac.Checked then ISequence := stBAC;
      if rb4PTIbca.Checked then ISequence := stBCA;
      if rb4PTIcab.Checked then ISequence := stCAB;
      if rb4PTIcba.Checked then ISequence := stCBA;
//
//      // 表尾电流反接
//      I1Reverse := chk4I1Reversept.Checked;
//      I2Reverse := chk4I2Reversept.Checked;
//      I3Reverse := chk4I3Reversept.Checked;
//    end;





    // 电流接地断开

    IaGroundBroken := chckbxIaGroundBroken4PT.State = cbChecked;
    IbGroundBroken := chckbxIbGroundBroken4PT.State = cbChecked;
    IcGroundBroken := chckbxIcGroundBroken4PT.State = cbChecked;

    IGroundBroken := chkI4PTGround.State = cbChecked;
  end;
end;

procedure TfWESelect2.SaveStatusThree;
//  function GetILineType( ARow : Integer ) : TWE_PHASE_LINE_TYPE;
//  var
//    i : Integer;
//  begin
//    Result := plN;
//
//    for i := 0 to 3 - 1 do
//      if sgd3Order.Cells[ i + 1, ARow ] <> '' then
//      begin
//        case i of
//          0 : Result := plA;
//          1 : Result := plC;
//          2 : Result := plN;
//        end;
//      end;
//  end;
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

    // 一次电压断路
    UaBroken := chk3BrokenSa.State = cbchecked;
    UbBroken := chk3BrokenSb.State = cbchecked;
    UcBroken := chk3BrokenSc.State = cbchecked;

    // 二次电压断路
    UsaBroken := chk3BrokenUa.State = cbchecked;
    UsbBroken := chk3BrokenUb.State = cbchecked;
    UscBroken := chk3BrokenUc.State = cbchecked;

    GroundBroken := chk3Ground.State = cbChecked;

    // 电压正反向
    PT1Reverse := chk3PTPN1.State = cbChecked;
    PT2Reverse := chk3PTPN2.State = cbChecked;

    // 电流正反向
    CT1Reverse := chk3CTPN1.State = cbChecked;
    CT2Reverse := chk3CTPN2.State = cbChecked;

    // 二次电流开路
    IaBroken := chk3CTOpenA.State = cbChecked;
    IcBroken := chk3CTOpenC.State = cbChecked;
    InBroken := chk3CTOpenN.State = cbChecked;

    // 二次电流短路
    CT1Short := chk3CTShort1.State = cbChecked;
    CT2Short := chk3CTShort2.State = cbChecked;

    // 表尾电流接线
    if rbIaIc.Checked then
    begin
      if chkI1Reverse.Checked then
      begin
        I1In  := plN;
        I1Out := plA;
      end
      else
      begin
        I1In  := plA;
        I1Out := plN;
      end;

      if chkI2Reverse.Checked then
      begin
        I2In  := plN;
        I2Out := plC;
      end
      else
      begin
        I2In  := plC;
        I2Out := plN;
      end;
    end
    else if rbIcIa.Checked then
    begin
      if chkI1Reverse.Checked then
      begin
        I1In  := plN;
        I1Out := plC;
      end
      else
      begin
        I1In  := plC;
        I1Out := plN;
      end;

      if chkI2Reverse.Checked then
      begin
        I2In  := plN;
        I2Out := plA;
      end
      else
      begin
        I2In  := plA;
        I2Out := plN;
      end;
    end
    else if rbIaIb.Checked then
    begin
      if chkI1Reverse.Checked then
      begin
        I1In  := plC;
        I1Out := plA;
      end
      else
      begin
        I1In  := plA;
        I1Out := plC;
      end;

      if chkI2Reverse.Checked then
      begin
        I2In  := plC;
        I2Out := plN;
      end
      else
      begin
        I2In  := plN;
        I2Out := plC;
      end;
    end
    else if rbIcIb.Checked then
    begin
      if chkI1Reverse.Checked then
      begin
        I1In  := plA;
        I1Out := plC;
      end
      else
      begin
        I1In  := plC;
        I1Out := plA;
      end;

      if chkI2Reverse.Checked then
      begin
        I2In  := plA;
        I2Out := plN;
      end
      else
      begin
        I2In  := plN;
        I2Out := plA;
      end;
    end
    else if rbIbIc.Checked then
    begin
      if chkI1Reverse.Checked then
      begin
        I1In  := plA;
        I1Out := plN;
      end
      else
      begin
        I1In  := plN;
        I1Out := plA;
      end;

      if chkI2Reverse.Checked then
      begin
        I2In  := plA;
        I2Out := plC;
      end
      else
      begin
        I2In  := plC;
        I2Out := plA;
      end;
    end
    else if rbIbIa.Checked then
    begin
      if chkI1Reverse.Checked then
      begin
        I1In  := plC;
        I1Out := plN;
      end
      else
      begin
        I1In  := plN;
        I1Out := plC;
      end;

      if chkI2Reverse.Checked then
      begin
        I2In  := plC;
        I2Out := plA;
      end
      else
      begin
        I2In  := plA;
        I2Out := plC;
      end;
    end;


//    // 表尾电流接线
//    if rbIaIc.Checked then
//    begin
//      if chkI1Reverse.Checked then
//      begin
//        I1In  := plN;
//        I1Out := plA;
//      end
//      else
//      begin
//        I1In  := plA;
//        I1Out := plN;
//      end;
//
//      if chkI2Reverse.Checked then
//      begin
//        I2In  := plN;
//        I2Out := plC;
//      end
//      else
//      begin
//        I2In  := plC;
//        I2Out := plN;
//      end;
//    end
//    else
//    begin
//      if chkI1Reverse.Checked then
//      begin
//        I1In  := plN;
//        I1Out := plC;
//      end
//      else
//      begin
//        I1In  := plC;
//        I1Out := plN;
//
//      end;
//
//      if chkI2Reverse.Checked then
//      begin
//        I2In  := plN;
//        I2Out := plA;
//      end
//      else
//      begin
//        I2In  := plA;
//        I2Out := plN;
//      end;
//    end;

//    if rbIaIc.Checked then
//    begin
//      I1In  := plA;
//      I1Out := plN;
//      I2In  := plC;
//      I2Out := plN;
//    end
//    else
//    begin
//      I1In  := plC;
//      I1Out := plN;
//      I2In  := plA;
//      I2Out := plN;
//    end;

    // 电流接地断开
    IaGroundBroken := chckbxIaGroundBroken3.State = cbChecked;
    IcGroundBroken := chckbxIcGroundBroken3.State = cbChecked;

    IGroundBroken := chkI3Ground.State = cbChecked;
  end;
end;

procedure TfWESelect2.SetPhaseType(APhaseType: TWE_PHASE_TYPE);
begin
  WiringError.PhaseType := APhaseType;

  if WiringError.PhaseType = ptFour   then //胡红明2013.5.10
  begin
//    pnl4CTOpen.Visible := False;
    tsPhase3.TabVisible := False;
    tsPhase4PT.TabVisible := False;
    tsPhase4.TabVisible := True;
    pgcErrors.ActivePage := tsPhase4;

  end
  else if WiringError.PhaseType = ptFourPT then
  begin
//    pnl4PTCTOpen.Visible := False;
    tsPhase3.TabVisible := False;
    tsPhase4.TabVisible := False;
    tsPhase4PT.TabVisible := True;
    pgcErrors.ActivePage := tsPhase4PT;
  end
  else
  begin
//    pnl3CTOpen.Visible := False;
    tsPhase3.TabVisible := True;
    tsPhase4.TabVisible := False;
    tsPhase4PT.TabVisible := False;
    pgcErrors.ActivePage := tsPhase3;
  end;
end;

procedure TfWESelect2.SetStatus( AWiringError : TWIRING_ERROR );
begin
  SetFocus;
  bSetStatus := True;  // 开始设置状态
  WiringError.Assign( AWiringError );

  if WiringError.PhaseType = ptThree then
  begin
    SetStatusThree;
    tsPhase3.Show;
  end
  else if WiringError.PhaseType = ptFour then
  begin 
    SetStatusFour;
    tsPhase4.Show;
  end
  else
  begin
    SetStatusFourPT;
    tsPhase4PT.Show;
  end;

  bSetStatus := False;
  ChangeStatus( nil );
end;

procedure TfWESelect2.SetStatusFour;
begin
  // 三相四线
  with WiringError do
  begin
    chk4BrokenUa.State := TCheckBoxState( UsaBroken );
    chk4BrokenUb.State := TCheckBoxState( UsbBroken );
    chk4BrokenUc.State := TCheckBoxState( UscBroken );
    chk4BrokenUn.State := TCheckBoxState( UsnBroken );

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

    // 表尾电流反接
    chk4I1Reverse.Checked := I1Reverse;
    chk4I2Reverse.Checked := I2Reverse;
    chk4I3Reverse.Checked := I3Reverse;


    // 二次电流开路
    chk4CTOpen1.State := TCheckBoxState( IaBroken );
    chk4CTOpen2.State := TCheckBoxState( IbBroken );
    chk4CTOpen3.State := TCheckBoxState( IcBroken );

    // 二次电流短路
    chk4CTShort1.State := TCheckBoxState( CT1Short );
    chk4CTShort2.State := TCheckBoxState( CT2Short );
    chk4CTShort3.State := TCheckBoxState( CT3Short );

    // 电流正反向
    chk4CTPN1.State := TCheckBoxState( CT1Reverse );
    chk4CTPN2.State := TCheckBoxState( CT2Reverse );
    chk4CTPN3.State := TCheckBoxState( CT3Reverse );

    // 电流接地断开
    chckbxIaGroundBroken4.State := TCheckBoxState(IaGroundBroken );
    chckbxIbGroundBroken4.State := TCheckBoxState(IbGroundBroken );
    chckbxIcGroundBroken4.State := TCheckBoxState(IcGroundBroken );
    chkI4Ground.State := TCheckBoxState( IGroundBroken );
  end;
end;

procedure TfWESelect2.SetStatusFourPT;  //胡红明 2013.5.13
var
  sI1, sI2, sI3: string;
  bI1Reverse, bI2Reverse, bI3Reverse: Boolean;


  // 设置电流相序界面
  procedure SetISequenceFrom;
    function GetRow(sValue : string) : Integer;
    begin
      if sValue = 'A' then
        Result := 1
      else if sValue = 'B' then
        Result := 2
      else if sValue = 'C' then
        Result := 3
      else if sValue = 'N' then
        Result := 4
      else
        Result := 5;
    end;
  var
    l, k : Integer;
  begin
    for l := 1 to 3 do
      for k := 1 to 4 do
        strngrd1.Cells[l, k] := '';

    strngrd1.Cells[1, GetRow(sI1)] := '●';
    strngrd1.Cells[2, GetRow(sI2)] := '●';
    strngrd1.Cells[3, GetRow(sI3)] := '●';

  end;
begin
  // 三相四线带电压互感器
  with WiringError do
  begin
    chk4PTBrokenUa.State := TCheckBoxState( UaBroken );
    chk4PTBrokenUb.State := TCheckBoxState( UbBroken );
    chk4PTBrokenUc.State := TCheckBoxState( UcBroken );
    chk4PTBrokenUn.State := TCheckBoxState( UnBroken );

    chk4PTBrokenUsa.State := TCheckBoxState( UsaBroken );
    chk4PTBrokenUsb.State := TCheckBoxState( UsbBroken );
    chk4PTBrokenUsc.State := TCheckBoxState( UscBroken );
    chk4PTBrokenUsn.State := TCheckBoxState( UsnBroken );

    chk4PT1.State := TCheckBoxState( PT1Reverse );
    chk4PT2.State := TCheckBoxState( PT2Reverse );
    chk4PT3.State := TCheckBoxState( PT3Reverse );
    chk4PTGround.State := TCheckBoxState( GroundBroken );

    case USequence of
      stABC: rb4PTUabc.Checked := True;
      stACB: rb4PTUacb.Checked := True;
      stBAC: rb4PTUbac.Checked := True;
      stBCA: rb4PTUbca.Checked := True;
      stCAB: rb4PTUcab.Checked := True;
      stCBA: rb4PTUcba.Checked := True;
    end;

    strngrd1.Visible := IsClearLinke and IsCanSetClearLinkeError;

    // 显示
//    // 简化接线
//    if IsClearLinke then
//    begin
//
//
      // 电流相序
      GetClearLinkeISequence(sI1, sI2, sI3, bI1Reverse, bI2Reverse, bI3Reverse);
      SetISequenceFrom;
//
//      I1Reverse := bI1Reverse;
//      I2Reverse := bI2Reverse;
//      I3Reverse := bI3Reverse;
//
//      // 表尾电流反接
//      chk4I1Reversept.Checked := I1Reverse;
//      chk4I2Reversept.Checked := I2Reverse;
//      chk4I3Reversept.Checked := I3Reverse;
//    end
//    else
////    begin
      // 电流相序
      case ISequence of
        stABC: rb4PTIabc.Checked := True;
        stACB: rb4PTIacb.Checked := True;
        stBAC: rb4PTIbac.Checked := True;
        stBCA: rb4PTIbca.Checked := True;
        stCAB: rb4PTIcab.Checked := True;
        stCBA: rb4PTIcba.Checked := True;
      end;
//

////    end;
///
    // 表尾电流反接
    chk4I1Reversept.Checked := I1Reverse;
    chk4I2Reversept.Checked := I2Reverse;
    chk4I3Reversept.Checked := I3Reverse;

    // 二次电流开路
    chk4PTCTOpen1.State := TCheckBoxState( IaBroken );
    chk4PTCTOpen2.State := TCheckBoxState( IbBroken );
    chk4PTCTOpen3.State := TCheckBoxState( IcBroken );

    // 二次电流短路
    chk4PTCTShort1.State := TCheckBoxState( CT1Short );
    chk4PTCTShort2.State := TCheckBoxState( CT2Short );
    chk4PTCTShort3.State := TCheckBoxState( CT3Short );

    // 电流正反向
    chk4PTCTPN1.State := TCheckBoxState( CT1Reverse );
    chk4PTCTPN2.State := TCheckBoxState( CT2Reverse );
    chk4PTCTPN3.State := TCheckBoxState( CT3Reverse );

    // 电流接地断开
    chckbxIaGroundBroken4PT.State := TCheckBoxState(IaGroundBroken );
    chckbxIbGroundBroken4PT.State := TCheckBoxState(IbGroundBroken );
    chckbxIcGroundBroken4PT.State := TCheckBoxState(IcGroundBroken );
    chkI4PTGround.State := TCheckBoxState( IGroundBroken );
  end;
end;

procedure TfWESelect2.SetStatusThree;
//  procedure SetILine( ARow : Integer; AValue : TWE_PHASE_LINE_TYPE );
//  begin
//    sgd3Order.Cells[ 1, ARow ] := '';
//    sgd3Order.Cells[ 2, ARow ] := '';
//    sgd3Order.Cells[ 3, ARow ] := '';
//
//    case AValue of
//      plA: sgd3Order.Cells[ 1, ARow ] := '*';
//      plC: sgd3Order.Cells[ 2, ARow ] := '*';
//      plN: sgd3Order.Cells[ 3, ARow ] := '*';
//    end;
//  end;
begin
  with WiringError do
  begin
    chk3BrokenSa.State := TCheckBoxState( UaBroken );
    chk3BrokenSb.State := TCheckBoxState( UbBroken );
    chk3BrokenSc.State := TCheckBoxState( UcBroken );
    chk3BrokenUa.State := TCheckBoxState( UsaBroken );
    chk3BrokenUb.State := TCheckBoxState( UsbBroken );
    chk3BrokenUc.State := TCheckBoxState( UscBroken );

    case USequence of
      stABC: rb3Uabc.Checked := True;
      stACB: rb3Uacb.Checked := True;
      stBAC: rb3Ubac.Checked := True;
      stBCA: rb3Ubca.Checked := True;
      stCAB: rb3Ucab.Checked := True;
      stCBA: rb3Ucba.Checked := True;
    end;

    //地线断


    chk3Ground.State := TCheckBoxState( GroundBroken );

    // 电压反
    chk3PTPN1.State := TCheckBoxState( PT1Reverse );
    chk3PTPN2.State := TCheckBoxState( PT2Reverse );

    // 电流反
    chk3CTPN1.State := TCheckBoxState( CT1Reverse );
    chk3CTPN2.State := TCheckBoxState( CT2Reverse );

    // 二次电流开路
    chk3CTOpenA.State := TCheckBoxState( IaBroken );
    chk3CTOpenC.State := TCheckBoxState( IcBroken );
    chk3CTOpenN.State := TCheckBoxState( InBroken );

    // 二次电流短路
    chk3CTShort1.State := TCheckBoxState( CT1Short );
    chk3CTShort2.State := TCheckBoxState( CT2Short );

//    // 相序 I
//    chkI1Reverse.Checked := I1In = plN;
//    chkI2Reverse.Checked := I2In = plN;
//
//    rbIaIc.Checked :=  (I1In = plA) or (I1Out = plA);
//    rbIaIc.Checked := (I2In = plC) or (I2Out = plC);


//    rbIaIc.Checked := I1In = plA;
//    rbIcIa.Checked := not (I1In = plA);

    // 电流接地断开
    chckbxIaGroundBroken3.State := TCheckBoxState(IaGroundBroken );
    chckbxIcGroundBroken3.State := TCheckBoxState(IcGroundBroken );

    chkI3Ground.State := TCheckBoxState( IGroundBroken );


    chkI3Ground.Visible := IsClearLinke;
    chckbxIcGroundBroken3.Visible := not IsClearLinke;
    chckbxIaGroundBroken3.Visible := not IsClearLinke;





    rbIaIb.Visible := IsClearLinke and IsCanSetClearLinkeError;
    rbIbIc.Visible := IsClearLinke and IsCanSetClearLinkeError;
    rbIbIa.Visible := IsClearLinke and IsCanSetClearLinkeError;
    rbIcIb.Visible := IsClearLinke and IsCanSetClearLinkeError;

    if not (IsClearLinke and IsCanSetClearLinkeError) then
    begin
      if not (rbIaIc.Checked or rbIcIa.Checked) then
      begin
        rbIaIc.Checked := True;
      end;

      chkI1Reverse.Checked := I1In = plN;
      chkI2Reverse.Checked := I2In = plN;
    end
    else
    begin
      // 表尾电流接线
      rbIaIc.Checked := (I1In in [plA, plN]) and (I1Out in [plA, plN]) and (I2In in [plC, plN]) and (I2Out in [plC, plN]);
      rbIcIa.Checked := (I1In in [plC, plN]) and (I1Out in [plC, plN]) and (I2In in [plA, plN]) and (I2Out in [plA, plN]);
      rbIaIb.Checked := (I1In in [plA, plC]) and (I1Out in [plA, plC]) and (I2In in [plC, plN]) and (I2Out in [plC, plN]);
      rbIcIb.Checked := (I1In in [plC, plA]) and (I1Out in [plC, plA]) and (I2In in [plA, plN]) and (I2Out in [plA, plN]);
      rbIbIa.Checked := (I1In in [plN, plC]) and (I1Out in [plN, plC]) and (I2In in [plC, plA]) and (I2Out in [plC, plA]);
      rbIbIc.Checked := (I1In in [plN, plA]) and (I1Out in [plN, plA]) and (I2In in [plA, plC]) and (I2Out in [plA, plC]);


      if rbIaIc.Checked then
      begin
        chkI1Reverse.Checked := (I1In = plN) and (I1Out = plA);
        chkI2Reverse.Checked := (I2In = plN) and (I2Out = plC);
      end
      else if rbIcIa.Checked then
      begin
        chkI1Reverse.Checked := (I1In = plN) and (I1Out = plC);
        chkI2Reverse.Checked := (I2In = plN) and (I2Out = plA);
      end
      else if rbIaIb.Checked then
      begin
        chkI1Reverse.Checked := (I1In = plC) and (I1Out = plA);
        chkI2Reverse.Checked := (I2In = plC) and (I2Out = plN);
      end
      else if rbIcIb.Checked then
      begin
        chkI1Reverse.Checked := (I1In = plA) and (I1Out = plC);
        chkI2Reverse.Checked := (I2In = plA) and (I2Out = plN);
      end
      else if rbIbIc.Checked then
      begin
        chkI1Reverse.Checked := (I1In = plA) and (I1Out = plN);
        chkI2Reverse.Checked := (I2In = plA) and (I2Out = plC);
      end
      else if rbIbIa.Checked then
      begin
        chkI1Reverse.Checked := (I1In = plC) and (I1Out = plN);
        chkI2Reverse.Checked := (I2In = plC) and (I2Out = plA);
      end;

    end;
  end;
end;

end.
