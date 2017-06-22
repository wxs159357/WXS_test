unit FrmErrorSelect;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, xWiringError;

type
  TfErrorSelect = class(TForm)
    scrlbx1: TScrollBox;
    grpbxgrp1: TGroupBox;
    grpbxgrp2: TGroupBox;
    lvSecondCurrentBreak: TListView;
    lvSecondCurrent: TListView;
    grpbxgrp3: TGroupBox;
    lvSecondVol: TListView;
    grpbxFirstError: TGroupBox;
    lvFirstError: TListView;
    grpbx1: TGroupBox;
    grpbxgrp6: TGroupBox;
    lvMeterVol: TListView;
    rdgrpMeterUSequence: TRadioGroup;
    grpbxgrp5: TGroupBox;
    lvMeterCurrent: TListView;
    rdgrpMeterISequence: TRadioGroup;
    procedure rdgrpMeterUSequenceClick(Sender: TObject);
    procedure rdgrpMeterISequenceClick(Sender: TObject);
    procedure lvFirstErrorClick(Sender: TObject);
    procedure lvSecondVolClick(Sender: TObject);
    procedure lvSecondCurrentClick(Sender: TObject);
    procedure lvSecondCurrentBreakClick(Sender: TObject);
    procedure lvMeterCurrentClick(Sender: TObject);
    procedure lvMeterVolClick(Sender: TObject);

  private
    { Private declarations }
    FError : TWIRINGF_ERROR;

    FOnChange: TNotifyEvent;
    FPTCT : Boolean;
    FABC : Integer;

    /// <summary>
    /// 错误改变
    /// </summary>
    procedure ErrorChange;


    /// <summary>
    /// 显示电表错误信息
    /// </summary>
    procedure ShowMeterInfo(AMeterInfo : TMETER_ERROR);

    /// <summary>
    /// 初始化界面
    /// </summary>
    procedure INIForm;
  public
    { Public declarations }
    /// <summary>
    /// 显示错误信息（在界面设置错误时实时保存到对象里） nAbc (0:abc, 1:uvw, 2:123)
    /// </summary>
    procedure ShowInfo(AError: TWIRINGF_ERROR; nAbc: Integer=0; bPTCT : Boolean=True);

    property OnChange : TNotifyEvent read FOnChange write FOnChange;

  end;

implementation

{$R *.dfm}

{ TfErrorSelect }

procedure TfErrorSelect.ShowMeterInfo(AMeterInfo: TMETER_ERROR);
var
  i : Integer;
begin
  if not Assigned(FError)then
    Exit;

  if not Assigned(AMeterInfo)then
    Exit;

  with AMeterInfo do
  begin
    if FError.PhaseType = ptfFour then
    begin
      // 电压部分
      lvMeterVol.Items[0].Checked := UBroken.AValue;
      lvMeterVol.Items[1].Checked := UBroken.BValue;
      lvMeterVol.Items[2].Checked := UBroken.CValue;

      // 电流部分
      lvMeterCurrent.Items[0].Checked := IBroken.AValue;
      lvMeterCurrent.Items[1].Checked := IBroken.BValue;
      lvMeterCurrent.Items[2].Checked := IBroken.CValue;
      lvMeterCurrent.Items[3].Checked := IShort.AValue;
      lvMeterCurrent.Items[4].Checked := IShort.BValue;
      lvMeterCurrent.Items[5].Checked := IShort.CValue;
      lvMeterCurrent.Items[6].Checked := IReverse.AValue;
      lvMeterCurrent.Items[7].Checked := IReverse.BValue;
      lvMeterCurrent.Items[8].Checked := IReverse.CValue;

      // 电压相序
      for i := 0 to rdgrpMeterUSequence.Items.Count - 1 do
      begin
        if Pos(USequence.OrganStr, rdgrpMeterUSequence.Items[i]) > 0 then
        begin
          rdgrpMeterUSequence.ItemIndex := i;
          Break;
        end;
      end;


      // 电流相序
      for i := 0 to rdgrpMeterISequence.Items.Count - 1 do
      begin
        if Pos(ISequence.OrganStr, rdgrpMeterISequence.Items[i]) > 0 then
        begin
          rdgrpMeterISequence.ItemIndex := i;
          Break;
        end;
      end;
    end
    else if FError.PhaseType = ptfThree then
    begin
      // 电压部分
      lvMeterVol.Items[0].Checked := UBroken.AValue;
      lvMeterVol.Items[1].Checked := UBroken.BValue;
      lvMeterVol.Items[2].Checked := UBroken.CValue;

      // 电流部分
      lvMeterCurrent.Items[0].Checked := IBroken.AValue;
      lvMeterCurrent.Items[1].Checked := IBroken.CValue;
      lvMeterCurrent.Items[2].Checked := IShort.AValue;
      lvMeterCurrent.Items[3].Checked := IShort.CValue;
      lvMeterCurrent.Items[4].Checked := IReverse.AValue;
      lvMeterCurrent.Items[5].Checked := IReverse.CValue;
      // 电压相序
      for i := 0 to rdgrpMeterUSequence.Items.Count - 1 do
      begin
        if Pos(USequence.OrganStr, rdgrpMeterUSequence.Items[i]) > 0 then
        begin
          rdgrpMeterUSequence.ItemIndex := i;
          Break;
        end;
      end;


      // 电流相序
      for i := 0 to rdgrpMeterISequence.Items.Count - 1 do
      begin
        if Pos(ISequence.OrganStr, rdgrpMeterISequence.Items[i]) > 0 then
        begin
          rdgrpMeterISequence.ItemIndex := i;
          Break;
        end;
      end;
    end;
  end;
end;

procedure TfErrorSelect.ErrorChange;
begin
  if Assigned(FOnChange) then
    FOnChange(FError);
end;

procedure TfErrorSelect.INIForm;
  function GetABC(nPhase : Byte;  bU : Boolean = False; nShowType : Integer = 0) : string;
  begin
    if nShowType = 0 then
    begin
      case FABC of
        0:
        begin
          case nPhase of
            2 : Result := 'b';
            3 : Result := 'c';
          else
            Result := 'a';
          end;
        end;
        1:
        begin
          case nPhase of
            2 : Result := 'v';
            3 : Result := 'w';
          else
            Result := 'u';
          end;
        end;
        2:
        begin
          case nPhase of
            2 : Result := '2';
            3 : Result := '3';
          else
            Result := '1';
          end;
        end;
      end;
    end
    else if nShowType = 1 then
    begin
      case FABC of
        0:
        begin
          case nPhase of
            2 : Result := '二元件';
            3 : Result := '三元件';
          else
            Result := '一元件';
          end;
        end;
        1:
        begin
          case nPhase of
            2 : Result := 'v';
            3 : Result := 'w';
          else
            Result := 'u';
          end;
        end;
        2:
        begin
          case nPhase of
            2 : Result := '2';
            3 : Result := '3';
          else
            Result := '1';
          end;
        end;
      end;
    end;

    if bU then
      Result := UpperCase(Result)
  end;

  function GetPTCT( bIsPT : Boolean = True):string;
  begin
    if FPTCT then
    begin
      if bIsPT then
        Result := 'PT'
      else
        Result := 'CT';
    end
    else
    begin
      if bIsPT then
        Result := 'TV'
      else
        Result := 'TA';
    end;
  end;
var
  i: Integer;
  s : string;
begin
  // 一次
  lvFirstError.Clear;
  for i := 1 to 3 do
    lvFirstError.Items.Add.Caption:= GetABC(i, True) + '相断相';

  // 二次
  if FError.IsTrans then
  begin
    // 电压
    lvSecondVol.Clear;
    lvSecondVol.Items.Add.Caption:= GetABC(1) + '相失压';
    lvSecondVol.Items.Add.Caption:= GetABC(2) + '相失压';
    lvSecondVol.Items.Add.Caption:= GetABC(3) + '相失压';

    lvSecondVol.Items.Add.Caption:=GetPTCT() + '1极性反';
    lvSecondVol.Items.Add.Caption:=GetPTCT() + '2极性反';
    if FError.PhaseType = ptfFour then
      lvSecondVol.Items.Add.Caption:=GetPTCT() +'3极性反';
    lvSecondVol.Items.Add.Caption:='接地断开';

    // 电流
    lvSecondCurrent.Clear;
    lvSecondCurrent.Items.Add.Caption:=GetPTCT(False) + '1开路';
    lvSecondCurrent.Items.Add.Caption:=GetPTCT(False) + '2开路';
    if FError.PhaseType = ptfFour then
    lvSecondCurrent.Items.Add.Caption:=GetPTCT(False) + '3开路';

    lvSecondCurrent.Items.Add.Caption:=GetPTCT(False) + '1短路';
    lvSecondCurrent.Items.Add.Caption:=GetPTCT(False) + '2短路';
    if FError.PhaseType = ptfFour then
      lvSecondCurrent.Items.Add.Caption:=GetPTCT(False) + '3短路';
    lvSecondCurrent.Items.Add.Caption:=GetPTCT(False) + '1极性反';
    lvSecondCurrent.Items.Add.Caption:=GetPTCT(False) + '2极性反';
    if FError.PhaseType = ptfFour then
      lvSecondCurrent.Items.Add.Caption:=GetPTCT(False) + '3极性反';

    // 接地
    lvSecondCurrentBreak.Clear;
    lvSecondCurrentBreak.Items.Add.Caption:=GetPTCT(False) + '1接地断开';
    lvSecondCurrentBreak.Items.Add.Caption:=GetPTCT(False) + '2接地断开';
    if FError.PhaseType = ptfFour then
      lvSecondCurrentBreak.Items.Add.Caption:=GetPTCT(False) + '3接地断开';

    // 电表
    lvMeterVol.Clear;
    lvMeterVol.Items.Add.Caption:=GetABC(1,False) + '相失压';
    lvMeterVol.Items.Add.Caption:=GetABC(2,False) + '相失压';
    lvMeterVol.Items.Add.Caption:=GetABC(3,False) + '相失压';

    rdgrpMeterUSequence.Items.Text := GetUSequenceStrAll;

    lvMeterCurrent.Clear;
    if FError.DiagramType <= 1 then
      s := '断相'
    else
      s := '开路';

    lvMeterCurrent.Items.Add.Caption:=GetABC(1,False)  + s;
    if FError.PhaseType = ptfFour then
      lvMeterCurrent.Items.Add.Caption:=GetABC(2,False)  + s;
    lvMeterCurrent.Items.Add.Caption:=GetABC(3,False)  + s;
    lvMeterCurrent.Items.Add.Caption:=GetABC(1,False) + '短路';
    if FError.PhaseType = ptfFour then
      lvMeterCurrent.Items.Add.Caption:=GetABC(2,False) + '短路';
    lvMeterCurrent.Items.Add.Caption:=GetABC(3,False) + '短路';
    lvMeterCurrent.Items.Add.Caption:=GetABC(1,False) + '反接';
    if FError.PhaseType = ptfFour then
      lvMeterCurrent.Items.Add.Caption:=GetABC(2,False) + '反接';
    lvMeterCurrent.Items.Add.Caption:=GetABC(3,False) + '反接';

    rdgrpMeterISequence.Items.Text := GetISequenceStrAll(FError.PhaseType = ptfThree, FError.DiagramType in [4,5]);

    rdgrpMeterISequence.Caption := '电流相序';

    grpbxgrp1.Visible := FError.IsTrans;

    if FError.DiagramType <= 1 then
    begin
      rdgrpMeterISequence.Caption := '相序';



    end
    else
    begin
      rdgrpMeterISequence.Caption := '电流相序';
    end;
  end;
end;

procedure TfErrorSelect.lvFirstErrorClick(Sender: TObject);
begin
  FError.FirError.AValue := lvFirstError.Items[0].Checked;
  FError.FirError.BValue := lvFirstError.Items[1].Checked;
  FError.FirError.CValue := lvFirstError.Items[2].Checked;

  ErrorChange;
end;

procedure TfErrorSelect.lvMeterCurrentClick(Sender: TObject);
begin
  with FError.MeterError do
  begin
    if FError.PhaseType = ptfFour then
      begin
        IBroken.AValue := lvMeterCurrent.Items[0].Checked;
        IBroken.BValue := lvMeterCurrent.Items[1].Checked;
        IBroken.CValue := lvMeterCurrent.Items[2].Checked;
        IShort.AValue :=  lvMeterCurrent.Items[3].Checked;
        IShort.BValue :=  lvMeterCurrent.Items[4].Checked;
        IShort.CValue :=  lvMeterCurrent.Items[5].Checked;
        IReverse.AValue := lvMeterCurrent.Items[6].Checked;
        IReverse.BValue := lvMeterCurrent.Items[7].Checked;
        IReverse.CValue := lvMeterCurrent.Items[8].Checked;
      end
    else
    begin
      IBroken.AValue := lvMeterCurrent.Items[0].Checked;
      IBroken.CValue := lvMeterCurrent.Items[1].Checked;
      IShort.AValue :=  lvMeterCurrent.Items[2].Checked;
      IShort.CValue :=  lvMeterCurrent.Items[3].Checked;
      IReverse.AValue := lvMeterCurrent.Items[4].Checked;
      IReverse.CValue := lvMeterCurrent.Items[5].Checked;
    end;

    if FError.DiagramType <= 1 then
    begin
      UBroken.Assign(IBroken);
    end;
  end;

  ErrorChange;
end;

procedure TfErrorSelect.lvMeterVolClick(Sender: TObject);
begin
  with FError.MeterError do
  begin
    UBroken.AValue := lvMeterVol.Items[0].Checked;
    UBroken.BValue := lvMeterVol.Items[1].Checked;
    UBroken.CValue := lvMeterVol.Items[2].Checked;
  end;
  ErrorChange;
end;

procedure TfErrorSelect.lvSecondCurrentBreakClick(Sender: TObject);
begin
   if FError.PhaseType = ptfFour then
  begin
    FError.SenError.SenIGroundBroken.AValue :=lvSecondCurrentBreak.Items[0].Checked;
    FError.SenError.SenIGroundBroken.BValue :=lvSecondCurrentBreak.Items[1].Checked;
    FError.SenError.SenIGroundBroken.CValue :=lvSecondCurrentBreak.Items[2].Checked;
  end
  else
  if FError.PhaseType = ptfThree then
  begin
    FError.SenError.SenIGroundBroken.AValue :=lvSecondCurrentBreak.Items[0].Checked;
    FError.SenError.SenIGroundBroken.BValue :=lvSecondCurrentBreak.Items[1].Checked;
  end;

  ErrorChange;
end;

procedure TfErrorSelect.lvSecondCurrentClick(Sender: TObject);
begin
  Self.Enabled := False;
  Screen.Cursor := crHourGlass;
  if FError.PhaseType = ptfFour then
    begin
      FError.SenError.SenIBroken.AValue := lvSecondCurrent.Items[0].Checked;
      FError.SenError.SenIBroken.BValue := lvSecondCurrent.Items[1].Checked;
      FError.SenError.SenIBroken.CValue := lvSecondCurrent.Items[2].Checked;
      FError.SenError.SenIShort.AValue :=  lvSecondCurrent.Items[3].Checked;
      FError.SenError.SenIShort.BValue :=  lvSecondCurrent.Items[4].Checked;
      FError.SenError.SenIShort.CValue :=  lvSecondCurrent.Items[5].Checked;
      FError.SenError.SenIReverse.AValue := lvSecondCurrent.Items[6].Checked;
      FError.SenError.SenIReverse.BValue := lvSecondCurrent.Items[7].Checked;
      FError.SenError.SenIReverse.CValue := lvSecondCurrent.Items[8].Checked;
    end
  else
  if FError.PhaseType = ptfThree then
  begin
    FError.SenError.SenIBroken.AValue := lvSecondCurrent.Items[0].Checked;
    FError.SenError.SenIBroken.BValue := lvSecondCurrent.Items[1].Checked;
    FError.SenError.SenIShort.AValue :=  lvSecondCurrent.Items[2].Checked;
    FError.SenError.SenIShort.BValue :=  lvSecondCurrent.Items[3].Checked;
    FError.SenError.SenIReverse.AValue := lvSecondCurrent.Items[4].Checked;
    FError.SenError.SenIReverse.BValue := lvSecondCurrent.Items[5].Checked;
  end;
  Screen.Cursor := crDefault;
  Self.Enabled := True;
  ErrorChange;
end;

procedure TfErrorSelect.lvSecondVolClick(Sender: TObject);
begin
  FError.SenError.SenUBroken.AValue := lvSecondVol.Items[0].Checked;

  if FError.PhaseType = ptfThree then
  begin
    FError.SenError.SenUBroken.BValue := lvSecondVol.Items[1].Checked;
    FError.SenError.SenUBroken.CValue := lvSecondVol.Items[2].Checked;
    FError.SenError.SenUReverse.AValue := lvSecondVol.Items[3].Checked;
    FError.SenError.SenUReverse.BValue := lvSecondVol.Items[4].Checked;
    FError.SenError.SenUGroundBroken := lvSecondVol.Items[5].Checked;
  end
  else
  begin
    FError.SenError.SenUBroken.BValue := lvSecondVol.Items[1].Checked;
    FError.SenError.SenUBroken.CValue := lvSecondVol.Items[2].Checked;
    FError.SenError.SenUReverse.AValue := lvSecondVol.Items[3].Checked;
    FError.SenError.SenUReverse.BValue := lvSecondVol.Items[4].Checked;
    FError.SenError.SenUReverse.CValue := lvSecondVol.Items[5].Checked;
    FError.SenError.SenUGroundBroken := lvSecondVol.Items[6].Checked;
  end;
  ErrorChange;
end;

procedure TfErrorSelect.rdgrpMeterISequenceClick(Sender: TObject);
var
  s : string;
begin

  s := rdgrpMeterISequence.Items[rdgrpMeterISequence.ItemIndex];
  FError.MeterError.ISequence.OrganStr := StringReplace(s, 'I', '', [rfReplaceAll]);

  if FError.DiagramType <= 1 then  // 直通表
  begin
    FError.MeterError.USequence:= FError.MeterError.ISequence;
  end;

  ErrorChange;
end;

procedure TfErrorSelect.rdgrpMeterUSequenceClick(Sender: TObject);
var
  s : string;
begin
  s := rdgrpMeterUSequence.Items[rdgrpMeterUSequence.ItemIndex];
  FError.MeterError.USequence.OrganStr := StringReplace(s, 'U', '', [rfReplaceAll]);

  ErrorChange;
end;

procedure TfErrorSelect.ShowInfo(AError: TWIRINGF_ERROR; nAbc: Integer; bPTCT : Boolean);
begin
  FError := AError;
  FABC := nAbc;
  FPTCT := bPTCT;
  INIForm;
  with FError do
  begin
    // 一次
    lvFirstError.Items[0].Checked := FError.FirError.AValue;
    lvFirstError.Items[1].Checked := FError.FirError.BValue;
    lvFirstError.Items[2].Checked := FError.FirError.CValue;
    // 二次 电压

    if FError.PhaseType = ptfFour then
    begin
      lvSecondVol.Items[0].Checked := FError.SenError.SenUBroken.AValue;
      lvSecondVol.Items[1].Checked := FError.SenError.SenUBroken.BValue;
      lvSecondVol.Items[2].Checked := FError.SenError.SenUBroken.CValue;

      lvSecondVol.Items[3].Checked := FError.SenError.SenUReverse.AValue;
      lvSecondVol.Items[4].Checked := FError.SenError.SenUReverse.BValue;

      lvSecondVol.Items[5].Checked := FError.SenError.SenUReverse.CValue;
      lvSecondVol.Items[6].Checked := FError.SenError.SenUGroundBroken;
    end
    else
    begin
      lvSecondVol.Items[0].Checked := FError.SenError.SenUBroken.AValue;
      lvSecondVol.Items[1].Checked := FError.SenError.SenUBroken.CValue;

      lvSecondVol.Items[2].Checked := FError.SenError.SenUReverse.AValue;
      lvSecondVol.Items[3].Checked := FError.SenError.SenUReverse.BValue;

      lvSecondVol.Items[4].Checked := FError.SenError.SenUGroundBroken;
    end;

    // 二次 电流
    if FError.PhaseType = ptfFour then
    begin
      lvSecondCurrent.Items[0].Checked := FError.SenError.SenIBroken.AValue;
      lvSecondCurrent.Items[1].Checked := FError.SenError.SenIBroken.BValue;
      lvSecondCurrent.Items[2].Checked := FError.SenError.SenIBroken.CValue;
      lvSecondCurrent.Items[3].Checked := FError.SenError.SenIShort.AValue;
      lvSecondCurrent.Items[4].Checked := FError.SenError.SenIShort.BValue;
      lvSecondCurrent.Items[5].Checked := FError.SenError.SenIShort.CValue;
      lvSecondCurrent.Items[6].Checked := FError.SenError.SenIReverse.AValue;
      lvSecondCurrent.Items[7].Checked := FError.SenError.SenIReverse.BValue;
      lvSecondCurrent.Items[8].Checked := FError.SenError.SenIReverse.CValue;
    end
    else
    if FError.PhaseType = ptfThree then
    begin
      lvSecondCurrent.Items[0].Checked := FError.SenError.SenIBroken.AValue;
      lvSecondCurrent.Items[1].Checked := FError.SenError.SenIBroken.BValue;
      lvSecondCurrent.Items[2].Checked := FError.SenError.SenIShort.AValue;
      lvSecondCurrent.Items[3].Checked := FError.SenError.SenIShort.BValue;
      lvSecondCurrent.Items[4].Checked := FError.SenError.SenIReverse.AValue;
      lvSecondCurrent.Items[5].Checked := FError.SenError.SenIReverse.BValue;
    end;

    if FError.PhaseType = ptfFour then
    begin
      lvSecondCurrentBreak.Items[0].Checked := FError.SenError.SenIGroundBroken.AValue;
      lvSecondCurrentBreak.Items[1].Checked := FError.SenError.SenIGroundBroken.BValue;
      lvSecondCurrentBreak.Items[2].Checked := FError.SenError.SenIGroundBroken.CValue;
    end
    else
    if FError.PhaseType = ptfThree then
    begin
      lvSecondCurrentBreak.Items[0].Checked := FError.SenError.SenIGroundBroken.AValue;
      lvSecondCurrentBreak.Items[1].Checked := FError.SenError.SenIGroundBroken.BValue;
    end;

    ShowMeterInfo(FError.MeterError);
  end;
end;
end.



