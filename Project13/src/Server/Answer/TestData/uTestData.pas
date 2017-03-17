unit uTestData;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.CheckLst, Vcl.ComCtrls,
  xTestDataInfo;

type
  TfTestData = class(TForm)
    pgcntrl1: TPageControl;
    tbsht1: TTabSheet;
    tbsht2: TTabSheet;
    grpbx1: TGroupBox;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    lbl4: TLabel;
    lbl5: TLabel;
    lbl6: TLabel;
    lbl7: TLabel;
    lbl8: TLabel;
    lbl9: TLabel;
    lbl10: TLabel;
    lbl11: TLabel;
    lbl12: TLabel;
    lbl13: TLabel;
    lbl14: TLabel;
    lbl15: TLabel;
    lbl16: TLabel;
    lbl17: TLabel;
    lbl18: TLabel;
    lbl19: TLabel;
    lbl20: TLabel;
    lbl21: TLabel;
    lbl22: TLabel;
    lbl23: TLabel;
    lbl24: TLabel;
    lbl25: TLabel;
    lbl26: TLabel;
    lbl27: TLabel;
    lbl28: TLabel;
    lbl29: TLabel;
    lbl30: TLabel;
    lbl31: TLabel;
    lbl32: TLabel;
    lbl36: TLabel;
    lbl33: TLabel;
    lbl34: TLabel;
    lbl35: TLabel;
    lbl37: TLabel;
    lbl38: TLabel;
    lbl39: TLabel;
    lbl40: TLabel;
    edt3U12: TEdit;
    edt3U32: TEdit;
    edt3U13: TEdit;
    edt3I1: TEdit;
    edt3I3: TEdit;
    edt3U12I1: TEdit;
    edt3U32I3: TEdit;
    edt3U12U32: TEdit;
    cbb3USequence: TComboBox;
    lst3PTReverse: TCheckListBox;
    cbb3ISequence: TComboBox;
    cbb3Load: TComboBox;
    grpbx2: TGroupBox;
    lbl41: TLabel;
    lbl42: TLabel;
    lbl43: TLabel;
    lbl44: TLabel;
    lbl45: TLabel;
    lbl46: TLabel;
    lbl47: TLabel;
    lbl48: TLabel;
    lbl49: TLabel;
    lbl50: TLabel;
    lbl51: TLabel;
    lbl52: TLabel;
    lbl56: TLabel;
    lbl57: TLabel;
    lbl58: TLabel;
    lbl59: TLabel;
    lbl60: TLabel;
    lbl61: TLabel;
    lbl62: TLabel;
    lbl63: TLabel;
    lbl64: TLabel;
    lbl65: TLabel;
    lbl66: TLabel;
    lbl67: TLabel;
    lbl68: TLabel;
    lbl69: TLabel;
    lbl70: TLabel;
    lbl71: TLabel;
    lbl72: TLabel;
    lbl73: TLabel;
    lbl74: TLabel;
    lbl75: TLabel;
    lbl76: TLabel;
    lbl77: TLabel;
    lbl78: TLabel;
    lbl79: TLabel;
    lbl80: TLabel;
    edt4U1: TEdit;
    edt4U2: TEdit;
    edt4U3: TEdit;
    edt4I1: TEdit;
    edt4U1I1: TEdit;
    edt4U2I2: TEdit;
    edt4U1U2: TEdit;
    cbb4USequence: TComboBox;
    lst4PTReverse: TCheckListBox;
    cbb4ISequence: TComboBox;
    cbb4Load: TComboBox;
    edt4I2: TEdit;
    lbl55: TLabel;
    lbl54: TLabel;
    lbl53: TLabel;
    edt4I3: TEdit;
    lbl81: TLabel;
    lbl82: TLabel;
    lbl83: TLabel;
    lbl84: TLabel;
    lbl85: TLabel;
    lbl86: TLabel;
    lbl87: TLabel;
    lbl88: TLabel;
    lbl89: TLabel;
    lbl90: TLabel;
    edt4U3I3: TEdit;
    lst3CTReverse: TCheckListBox;
    lbl91: TLabel;
    lst4CTReverse: TCheckListBox;
    lbl92: TLabel;
    lbl93: TLabel;
    lbl94: TLabel;
    lbl95: TLabel;
    lbl96: TLabel;
    lbl97: TLabel;
    lbl98: TLabel;
    lbl99: TLabel;
    lbl100: TLabel;
    lbl101: TLabel;
    lbl102: TLabel;
    lbl103: TLabel;
    lbl104: TLabel;
    lbl105: TLabel;
    lbl106: TLabel;
    lbl107: TLabel;
    lbl108: TLabel;
    lbl109: TLabel;
    lbl110: TLabel;
  private
    { Private declarations }
    FInfo : TTestDataInfo;

    function UValueToStr(dValue : Double) : string;
    function IValueToStr(dValue : Double) : string;
    function OValueToStr(dValue : Double) : string;

    function StrToValue(s : string) : Double;
  public
    { Public declarations }
    procedure ShowInfo(AInfo : TTestDataInfo);
    procedure SaveInfo;
  end;

var
  fTestData: TfTestData;

implementation

{$R *.dfm}

{ TfTestData }

function TfTestData.IValueToStr(dValue: Double): string;
begin
  Result := FormatFloat('0.000', dValue);
end;

function TfTestData.OValueToStr(dValue: Double): string;
begin
  Result := FormatFloat('0', dValue);
end;

procedure TfTestData.SaveInfo;
begin
  if Assigned(FInfo) then
  begin
    with FInfo do
    begin
      if PhaseType = tptThree then
      begin
        U1 := StrToValue(edt3U12.Text);
        U3 := StrToValue(edt3U32.Text);
        U2 := StrToValue(edt3U13.Text);
        I1 := StrToValue(edt3I1.Text);
        I3 := StrToValue(edt3I3.Text);

        O1 := StrToValue(edt3U12I1.Text);
        O3 := StrToValue(edt3U32I3.Text);
        OU1U2 := StrToValue(edt3U12U32.Text);

        USequence := TSequenceType(cbb3USequence.ItemIndex);
        ISequence := TSequenceType(cbb3ISequence.ItemIndex);
        IsLoadL := cbb3Load.ItemIndex = 0;

        IsPT1Reverse := lst3PTReverse.Checked[0];
        IsPT2Reverse := lst3PTReverse.Checked[1];
        IsPT3Reverse := lst3PTReverse.Checked[2];

        IsCT1Reverse := lst3CTReverse.Checked[0];
        IsCT2Reverse := lst3CTReverse.Checked[1];
        IsCT3Reverse := lst3CTReverse.Checked[2];

      end
      else if PhaseType = tptFour then
      begin
        U1 := StrToValue(edt4U1.Text);
        U2 := StrToValue(edt4U2.Text);
        U3 := StrToValue(edt4U3.Text);
        I1 := StrToValue(edt4I1.Text);
        I2 := StrToValue(edt4I2.Text);
        I3 := StrToValue(edt4I3.Text);

        O1 := StrToValue(edt4U1I1.Text);
        O2 := StrToValue(edt4U2I2.Text);
        O3 := StrToValue(edt4U3I3.Text);
        OU1U2 := StrToValue(edt4U1U2.Text);

        USequence := TSequenceType(cbb4USequence.ItemIndex);
        ISequence := TSequenceType(cbb4ISequence.ItemIndex);

        IsLoadL := cbb4Load.ItemIndex = 0;

        IsPT1Reverse := lst4PTReverse.Checked[0];
        IsPT2Reverse := lst4PTReverse.Checked[1];
        IsPT3Reverse := lst4PTReverse.Checked[2];

        IsCT1Reverse := lst4CTReverse.Checked[0];
        IsCT2Reverse := lst4CTReverse.Checked[1];
        IsCT3Reverse := lst4CTReverse.Checked[2];
      end;
    end;
  end;
end;

procedure TfTestData.ShowInfo(AInfo: TTestDataInfo);
begin
  FInfo := AInfo;

  if Assigned(FInfo) then
  begin
    with FInfo do
    begin
      tbsht1.TabVisible := PhaseType = tptThree;
      tbsht2.TabVisible := PhaseType = tptFour;

      if PhaseType = tptThree then
      begin
        edt3U12.Text := UValueToStr(U1);
        edt3U32.Text := UValueToStr(U3);
        edt3U13.Text := UValueToStr(U2);
        edt3I1.Text  := IValueToStr(I1);
        edt3I3.Text  := IValueToStr(I3);

        edt3U12I1.Text  := OValueToStr(O1);
        edt3U32I3.Text  := OValueToStr(O3);
        edt3U12U32.Text := OValueToStr(OU1U2);

        cbb3USequence.ItemIndex := Integer(USequence);
        cbb3ISequence.ItemIndex := Integer(ISequence);

        if IsLoadL then
          cbb3Load.ItemIndex := 0
        else
          cbb3Load.ItemIndex := 1;

        lst3PTReverse.Checked[0] := IsPT1Reverse;
        lst3PTReverse.Checked[1] := IsPT2Reverse;
        lst3PTReverse.Checked[2] := IsPT3Reverse;

        lst3CTReverse.Checked[0] := IsCT1Reverse;
        lst3CTReverse.Checked[1] := IsCT2Reverse;
        lst3CTReverse.Checked[2] := IsCT3Reverse;

      end
      else if PhaseType = tptFour then
      begin
        edt4U1.Text := UValueToStr(U1);
        edt4U2.Text := UValueToStr(U2);
        edt4U3.Text := UValueToStr(U3);
        edt4I1.Text := IValueToStr(I1);
        edt4I2.Text := IValueToStr(I2);
        edt4I3.Text := IValueToStr(I3);

        edt4U1I1.Text  := OValueToStr(O1);
        edt4U2I2.Text  := OValueToStr(O2);
        edt4U3I3.Text  := OValueToStr(O3);
        edt4U1U2.Text  := OValueToStr(OU1U2);

        cbb4USequence.ItemIndex := Integer(USequence);
        cbb4ISequence.ItemIndex := Integer(ISequence);

        if IsLoadL then
          cbb4Load.ItemIndex := 0
        else
          cbb4Load.ItemIndex := 1;

        lst4PTReverse.Checked[0] := IsPT1Reverse;
        lst4PTReverse.Checked[1] := IsPT2Reverse;
        lst4PTReverse.Checked[2] := IsPT3Reverse;

        lst4CTReverse.Checked[0] := IsCT1Reverse;
        lst4CTReverse.Checked[1] := IsCT2Reverse;
        lst4CTReverse.Checked[2] := IsCT3Reverse;
      end;
    end;
  end;
end;

function TfTestData.StrToValue(s: string): Double;
begin
  TryStrToFloat(s, Result);
end;

function TfTestData.UValueToStr(dValue: Double): string;
begin
  Result := FormatFloat('0.0', dValue);
end;

end.
