object fWESelect2: TfWESelect2
  Left = 181
  Top = 124
  Caption = #36873#25321#32771#39064
  ClientHeight = 464
  ClientWidth = 278
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pgcErrors: TPageControl
    Left = 0
    Top = 0
    Width = 278
    Height = 464
    ActivePage = tsPhase3
    Align = alClient
    TabHeight = 25
    TabOrder = 0
    TabWidth = 120
    OnChange = pgcErrorsChange
    object tsPhase3: TTabSheet
      Caption = #19977#30456#19977#32447
      object GroupBox1: TGroupBox
        AlignWithMargins = True
        Left = 10
        Top = 5
        Width = 250
        Height = 202
        Margins.Left = 10
        Margins.Top = 5
        Margins.Right = 10
        Align = alTop
        Caption = #30005#21387#37096#20998
        TabOrder = 0
        object Label5: TLabel
          Left = 16
          Top = 21
          Width = 55
          Height = 13
          AutoSize = False
          Caption = #19968#27425#26029#30456
        end
        object Label6: TLabel
          Left = 16
          Top = 157
          Width = 33
          Height = 13
          AutoSize = False
          Caption = #25509#32447
        end
        object Label9: TLabel
          Left = 16
          Top = 97
          Width = 90
          Height = 13
          AutoSize = False
          Caption = #20108#27425#26497#24615#21453#25509
        end
        object lbl7: TLabel
          Left = 16
          Top = 138
          Width = 33
          Height = 13
          AutoSize = False
          Caption = #34920#23614
        end
        object lbl8: TLabel
          Left = 16
          Top = 48
          Width = 55
          Height = 13
          AutoSize = False
          Caption = #20108#27425#26029#30456
        end
        object pnl3BrokenS: TPanel
          Left = 71
          Top = 15
          Width = 178
          Height = 25
          TabOrder = 0
          object chk3BrokenSa: TCheckBox
            Left = 14
            Top = 4
            Width = 45
            Height = 17
            Caption = 'A'#30456
            TabOrder = 0
            OnClick = ChangeStatus
          end
          object chk3BrokenSb: TCheckBox
            Left = 70
            Top = 4
            Width = 45
            Height = 17
            Caption = 'B'#30456
            TabOrder = 1
            OnClick = ChangeStatus
          end
          object chk3BrokenSc: TCheckBox
            Left = 126
            Top = 4
            Width = 45
            Height = 17
            Caption = 'C'#30456
            TabOrder = 2
            OnClick = ChangeStatus
          end
        end
        object pnl3BrokenU: TPanel
          Left = 71
          Top = 42
          Width = 178
          Height = 47
          TabOrder = 1
          object chk3BrokenUc: TCheckBox
            Left = 126
            Top = 5
            Width = 45
            Height = 17
            Caption = 'C'#30456
            TabOrder = 2
            OnClick = ChangeStatus
          end
          object chk3BrokenUb: TCheckBox
            Left = 70
            Top = 5
            Width = 45
            Height = 17
            Caption = 'B'#30456
            TabOrder = 1
            OnClick = ChangeStatus
          end
          object chk3BrokenUa: TCheckBox
            Left = 14
            Top = 5
            Width = 45
            Height = 17
            Caption = 'A'#30456
            TabOrder = 0
            OnClick = ChangeStatus
          end
          object chk3Ground: TCheckBox
            Left = 14
            Top = 28
            Width = 47
            Height = 14
            Caption = #22320#32447
            TabOrder = 3
            OnClick = ChangeStatus
          end
        end
        object pnl3Uab: TPanel
          Left = 126
          Top = 91
          Width = 123
          Height = 26
          TabOrder = 2
          object chk3PTPN1: TCheckBox
            Left = 15
            Top = 4
            Width = 47
            Height = 17
            Caption = 'PT1'
            TabOrder = 0
            OnClick = ChangeStatus
          end
          object chk3PTPN2: TCheckBox
            Left = 71
            Top = 4
            Width = 47
            Height = 17
            Caption = 'PT2'
            TabOrder = 1
            OnClick = ChangeStatus
          end
        end
        object pnl3UOrder: TPanel
          Left = 71
          Top = 119
          Width = 178
          Height = 73
          TabOrder = 3
          object rb3Uabc: TRadioButton
            Left = 19
            Top = 8
            Width = 60
            Height = 17
            Caption = 'Uabc'
            Checked = True
            TabOrder = 0
            TabStop = True
            OnClick = ChangeStatus
          end
          object rb3Uacb: TRadioButton
            Left = 106
            Top = 8
            Width = 60
            Height = 17
            Caption = 'Uacb'
            TabOrder = 1
            OnClick = ChangeStatus
          end
          object rb3Ubca: TRadioButton
            Left = 106
            Top = 28
            Width = 60
            Height = 17
            Caption = 'Ubca'
            TabOrder = 3
            OnClick = ChangeStatus
          end
          object rb3Ubac: TRadioButton
            Left = 19
            Top = 28
            Width = 60
            Height = 17
            Caption = 'Ubac'
            TabOrder = 2
            OnClick = ChangeStatus
          end
          object rb3Ucab: TRadioButton
            Left = 19
            Top = 48
            Width = 60
            Height = 17
            Caption = 'Ucab'
            TabOrder = 4
            OnClick = ChangeStatus
          end
          object rb3Ucba: TRadioButton
            Left = 106
            Top = 48
            Width = 60
            Height = 17
            Caption = 'Ucba'
            TabOrder = 5
            OnClick = ChangeStatus
          end
        end
      end
      object GroupBox2: TGroupBox
        AlignWithMargins = True
        Left = 10
        Top = 215
        Width = 250
        Height = 250
        Margins.Left = 10
        Margins.Top = 5
        Margins.Right = 10
        Align = alTop
        Caption = #30005#27969#37096#20998
        TabOrder = 1
        object Label11: TLabel
          Left = 16
          Top = 18
          Width = 88
          Height = 13
          AutoSize = False
          Caption = #20108#27425#26497#24615#21453#25509
        end
        object lbl3: TLabel
          Left = 16
          Top = 76
          Width = 79
          Height = 13
          AutoSize = False
          Caption = #30005#27969#24320#36335
        end
        object lbl4: TLabel
          Left = 16
          Top = 46
          Width = 65
          Height = 15
          AutoSize = False
          Caption = #20108#27425#30701#36335
        end
        object lbl5: TLabel
          Left = 16
          Top = 131
          Width = 55
          Height = 13
          AutoSize = False
          Caption = #34920#23614#25509#32447
        end
        object lbl2: TLabel
          Left = 16
          Top = 101
          Width = 104
          Height = 13
          AutoSize = False
          Caption = #30005#27969#25509#22320#26029#24320
        end
        object lbl25: TLabel
          Left = 16
          Top = 186
          Width = 55
          Height = 12
          AutoSize = False
          Caption = #34920#23614#21453#25509
        end
        object pnl3CTPN: TPanel
          Left = 126
          Top = 11
          Width = 123
          Height = 27
          TabOrder = 0
          object chk3CTPN1: TCheckBox
            Left = 15
            Top = 5
            Width = 50
            Height = 17
            Caption = 'CT1'
            TabOrder = 0
            OnClick = ChangeStatus
          end
          object chk3CTPN2: TCheckBox
            Left = 71
            Top = 5
            Width = 50
            Height = 17
            Caption = 'CT2'
            TabOrder = 1
            OnClick = ChangeStatus
          end
        end
        object pnl3CTOpen: TPanel
          Left = 71
          Top = 68
          Width = 179
          Height = 25
          TabOrder = 2
          object chk3CTOpenA: TCheckBox
            Left = 70
            Top = 4
            Width = 50
            Height = 17
            Caption = 'a'#30456
            TabOrder = 1
            OnClick = ChangeStatus
          end
          object chk3CTOpenC: TCheckBox
            Left = 126
            Top = 4
            Width = 50
            Height = 17
            Caption = 'c'#30456
            TabOrder = 2
            OnClick = ChangeStatus
          end
          object chk3CTOpenN: TCheckBox
            Left = 14
            Top = 4
            Width = 50
            Height = 17
            Caption = 'n'#30456
            TabOrder = 0
            Visible = False
            OnClick = ChangeStatus
          end
        end
        object pnlCTShort: TPanel
          Left = 126
          Top = 40
          Width = 123
          Height = 27
          TabOrder = 1
          object chk3CTShort1: TCheckBox
            Left = 15
            Top = 5
            Width = 50
            Height = 17
            Caption = 'CT1'
            TabOrder = 0
            OnClick = ChangeStatus
          end
          object chk3CTShort2: TCheckBox
            Left = 71
            Top = 5
            Width = 50
            Height = 17
            Caption = 'CT2'
            TabOrder = 1
            OnClick = ChangeStatus
          end
        end
        object pnl5: TPanel
          Left = 77
          Top = 124
          Width = 161
          Height = 53
          TabOrder = 3
          object rbIcIa: TRadioButton
            Left = 93
            Top = -3
            Width = 60
            Height = 17
            Caption = 'IcIa'
            TabOrder = 0
            OnClick = ChangeStatus
          end
          object rbIaIc: TRadioButton
            Left = 19
            Top = -3
            Width = 60
            Height = 17
            Caption = 'IaIc'
            TabOrder = 1
            OnClick = ChangeStatus
          end
          object rbIcIb: TRadioButton
            Left = 93
            Top = 14
            Width = 60
            Height = 17
            Caption = 'IcIb'
            TabOrder = 2
            OnClick = ChangeStatus
          end
          object rbIaIb: TRadioButton
            Left = 19
            Top = 14
            Width = 60
            Height = 17
            Caption = 'IaIb'
            TabOrder = 3
            OnClick = ChangeStatus
          end
          object rbIbIa: TRadioButton
            Left = 93
            Top = 30
            Width = 60
            Height = 17
            Caption = 'IbIa'
            TabOrder = 4
            OnClick = ChangeStatus
          end
          object rbIbIc: TRadioButton
            Left = 19
            Top = 30
            Width = 60
            Height = 17
            Caption = 'IbIc'
            Checked = True
            TabOrder = 5
            TabStop = True
            OnClick = ChangeStatus
          end
        end
        object pnl6: TPanel
          Left = 96
          Top = 96
          Width = 153
          Height = 24
          TabOrder = 4
          object chckbxIaGroundBroken3: TCheckBox
            Left = 43
            Top = 4
            Width = 50
            Height = 17
            Caption = 'a'#30456
            TabOrder = 0
            OnClick = ChangeStatus
          end
          object chckbxIcGroundBroken3: TCheckBox
            Left = 98
            Top = 4
            Width = 50
            Height = 17
            Caption = 'c'#30456
            TabOrder = 1
            OnClick = ChangeStatus
          end
          object chkI3Ground: TCheckBox
            Left = 44
            Top = 5
            Width = 47
            Height = 14
            Caption = #22320#32447
            TabOrder = 2
            Visible = False
            OnClick = ChangeStatus
          end
        end
        object pnl9: TPanel
          Left = 76
          Top = 177
          Width = 161
          Height = 29
          TabOrder = 5
          object chkI2Reverse: TCheckBox
            Left = 106
            Top = 6
            Width = 50
            Height = 17
            Caption = #20803#20214'2'
            TabOrder = 0
            OnClick = ChangeStatus
          end
          object chkI1Reverse: TCheckBox
            Left = 19
            Top = 6
            Width = 50
            Height = 17
            Caption = #20803#20214'1'
            TabOrder = 1
            OnClick = ChangeStatus
          end
        end
      end
    end
    object tsPhase4: TTabSheet
      Caption = #19977#30456#22235#32447
      ImageIndex = 1
      object grp4Voltage: TGroupBox
        AlignWithMargins = True
        Left = 10
        Top = 5
        Width = 250
        Height = 177
        Margins.Left = 10
        Margins.Top = 5
        Margins.Right = 10
        Align = alTop
        Caption = #30005#21387#37096#20998
        TabOrder = 0
        object lbl1: TLabel
          Left = 16
          Top = 43
          Width = 36
          Height = 13
          AutoSize = False
          Caption = #26029#30456
        end
        object Label1: TLabel
          Left = 16
          Top = 139
          Width = 33
          Height = 13
          AutoSize = False
          Caption = #25509#32447
        end
        object lbl13: TLabel
          Left = 16
          Top = 123
          Width = 33
          Height = 13
          AutoSize = False
          Caption = #34920#23614
        end
        object pnl4BrokenU: TPanel
          Left = 72
          Top = 18
          Width = 177
          Height = 63
          TabOrder = 0
          object chk4BrokenUc: TCheckBox
            Left = 14
            Top = 33
            Width = 49
            Height = 17
            Caption = 'C'#30456
            TabOrder = 2
            OnClick = ChangeStatus
          end
          object chk4BrokenUb: TCheckBox
            Left = 91
            Top = 1
            Width = 41
            Height = 17
            Caption = 'B'#30456
            TabOrder = 1
            OnClick = ChangeStatus
          end
          object chk4BrokenUa: TCheckBox
            Left = 14
            Top = 1
            Width = 41
            Height = 17
            Caption = 'A'#30456
            TabOrder = 0
            OnClick = ChangeStatus
          end
          object chk4BrokenUn: TCheckBox
            Left = 91
            Top = 33
            Width = 49
            Height = 17
            Caption = 'N'#30456
            TabOrder = 3
            Visible = False
            OnClick = ChangeStatus
          end
        end
        object pnl4UOrder: TPanel
          Left = 72
          Top = 105
          Width = 177
          Height = 62
          TabOrder = 1
          object rb4Uabc: TRadioButton
            Left = 14
            Top = 4
            Width = 73
            Height = 17
            Caption = 'Uabc'
            Checked = True
            TabOrder = 0
            TabStop = True
            OnClick = ChangeStatus
          end
          object rb4Uacb: TRadioButton
            Left = 94
            Top = 4
            Width = 73
            Height = 17
            Caption = 'Uacb'
            TabOrder = 1
            OnClick = ChangeStatus
          end
          object rb4Ubca: TRadioButton
            Left = 94
            Top = 22
            Width = 73
            Height = 17
            Caption = 'Ubca'
            TabOrder = 3
            OnClick = ChangeStatus
          end
          object rb4Ubac: TRadioButton
            Left = 14
            Top = 22
            Width = 73
            Height = 17
            Caption = 'Ubac'
            TabOrder = 2
            OnClick = ChangeStatus
          end
          object rb4Ucab: TRadioButton
            Left = 14
            Top = 40
            Width = 73
            Height = 17
            Caption = 'Ucab'
            TabOrder = 4
            OnClick = ChangeStatus
          end
          object rb4Ucba: TRadioButton
            Left = 94
            Top = 40
            Width = 73
            Height = 17
            Caption = 'Ucba'
            TabOrder = 5
            OnClick = ChangeStatus
          end
        end
      end
      object grp4Current: TGroupBox
        AlignWithMargins = True
        Left = 10
        Top = 190
        Width = 250
        Height = 242
        Margins.Left = 10
        Margins.Top = 5
        Margins.Right = 10
        Align = alTop
        Caption = #30005#27969#37096#20998
        TabOrder = 1
        object Label4: TLabel
          Left = 16
          Top = 175
          Width = 33
          Height = 13
          AutoSize = False
          Caption = #25509#32447
        end
        object lbl9: TLabel
          Left = 16
          Top = 22
          Width = 50
          Height = 13
          AutoSize = False
          Caption = #26497#24615#21453#25509
        end
        object lbl10: TLabel
          Left = 16
          Top = 84
          Width = 50
          Height = 13
          AutoSize = False
          Caption = #20108#27425#24320#36335
        end
        object lbl11: TLabel
          Left = 16
          Top = 53
          Width = 50
          Height = 13
          AutoSize = False
          Caption = #20108#27425#30701#36335
        end
        object lbl12: TLabel
          Left = 16
          Top = 156
          Width = 33
          Height = 13
          AutoSize = False
          Caption = #34920#23614
        end
        object lbl6: TLabel
          Left = 16
          Top = 113
          Width = 50
          Height = 13
          AutoSize = False
          Caption = #25509#22320#26029#24320
        end
        object lbl26: TLabel
          Left = 17
          Top = 212
          Width = 55
          Height = 13
          AutoSize = False
          Caption = #34920#23614#21453#25509
        end
        object pnl4CTPN: TPanel
          Left = 72
          Top = 16
          Width = 177
          Height = 26
          TabOrder = 0
          object chk4CTPN1: TCheckBox
            Left = 16
            Top = 5
            Width = 43
            Height = 17
            Caption = 'CT1'
            TabOrder = 0
            OnClick = ChangeStatus
          end
          object chk4CTPN2: TCheckBox
            Left = 67
            Top = 5
            Width = 43
            Height = 17
            Caption = 'CT2'
            TabOrder = 1
            OnClick = ChangeStatus
          end
          object chk4CTPN3: TCheckBox
            Left = 123
            Top = 5
            Width = 43
            Height = 17
            Caption = 'CT3'
            TabOrder = 2
            OnClick = ChangeStatus
          end
        end
        object pnl4IOrder: TPanel
          Left = 72
          Top = 135
          Width = 177
          Height = 66
          TabOrder = 3
          object rb4Iabc: TRadioButton
            Left = 16
            Top = 8
            Width = 73
            Height = 17
            Caption = 'Iabc'
            Checked = True
            TabOrder = 0
            TabStop = True
            OnClick = ChangeStatus
          end
          object rb4Iacb: TRadioButton
            Left = 96
            Top = 8
            Width = 73
            Height = 17
            Caption = 'Iacb'
            TabOrder = 1
            OnClick = ChangeStatus
          end
          object rb4Ibca: TRadioButton
            Left = 96
            Top = 27
            Width = 73
            Height = 17
            Caption = 'Ibca'
            TabOrder = 3
            OnClick = ChangeStatus
          end
          object rb4Ibac: TRadioButton
            Left = 16
            Top = 27
            Width = 73
            Height = 17
            Caption = 'Ibac'
            TabOrder = 2
            OnClick = ChangeStatus
          end
          object rb4Icab: TRadioButton
            Left = 16
            Top = 45
            Width = 73
            Height = 17
            Caption = 'Icab'
            TabOrder = 4
            OnClick = ChangeStatus
          end
          object rb4Icba: TRadioButton
            Left = 96
            Top = 45
            Width = 73
            Height = 17
            Caption = 'Icba'
            TabOrder = 5
            OnClick = ChangeStatus
          end
        end
        object pnl4CTOpen: TPanel
          Left = 72
          Top = 79
          Width = 177
          Height = 26
          TabOrder = 2
          object chk4CTOpen1: TCheckBox
            Left = 16
            Top = 5
            Width = 43
            Height = 17
            Caption = 'a'#30456
            TabOrder = 0
            OnClick = ChangeStatus
          end
          object chk4CTOpen2: TCheckBox
            Left = 67
            Top = 5
            Width = 43
            Height = 17
            Caption = 'b'#30456
            TabOrder = 1
            OnClick = ChangeStatus
          end
          object chk4CTOpen3: TCheckBox
            Left = 123
            Top = 5
            Width = 43
            Height = 17
            Caption = 'c'#30456
            TabOrder = 2
            OnClick = ChangeStatus
          end
        end
        object pnl4CTShort: TPanel
          Left = 72
          Top = 47
          Width = 177
          Height = 26
          TabOrder = 1
          object chk4CTShort1: TCheckBox
            Left = 16
            Top = 5
            Width = 43
            Height = 17
            Caption = 'CT1'
            TabOrder = 0
            OnClick = ChangeStatus
          end
          object chk4CTShort2: TCheckBox
            Left = 67
            Top = 5
            Width = 43
            Height = 17
            Caption = 'CT2'
            TabOrder = 1
            OnClick = ChangeStatus
          end
          object chk4CTShort3: TCheckBox
            Left = 123
            Top = 5
            Width = 43
            Height = 17
            Caption = 'CT3'
            TabOrder = 2
            OnClick = ChangeStatus
          end
        end
        object pnl7: TPanel
          Left = 72
          Top = 107
          Width = 177
          Height = 26
          TabOrder = 4
          object chckbxIaGroundBroken4: TCheckBox
            Left = 16
            Top = 5
            Width = 43
            Height = 17
            Caption = 'a'#30456
            TabOrder = 0
            OnClick = ChangeStatus
          end
          object chckbxIbGroundBroken4: TCheckBox
            Left = 67
            Top = 5
            Width = 43
            Height = 17
            Caption = 'b'#30456
            TabOrder = 1
            OnClick = ChangeStatus
          end
          object chckbxIcGroundBroken4: TCheckBox
            Left = 123
            Top = 5
            Width = 43
            Height = 17
            Caption = 'c'#30456
            TabOrder = 2
            OnClick = ChangeStatus
          end
          object chkI4Ground: TCheckBox
            Left = 150
            Top = 0
            Width = 47
            Height = 14
            Caption = #22320#32447
            TabOrder = 3
            Visible = False
            OnClick = ChangeStatus
          end
        end
        object pnl10: TPanel
          Left = 72
          Top = 203
          Width = 177
          Height = 30
          TabOrder = 5
          object chk4I2Reverse: TCheckBox
            Left = 62
            Top = 5
            Width = 50
            Height = 17
            Caption = #20803#20214'2'
            TabOrder = 0
            OnClick = ChangeStatus
          end
          object chk4I1Reverse: TCheckBox
            Left = 6
            Top = 5
            Width = 50
            Height = 17
            Caption = #20803#20214'1'
            TabOrder = 1
            OnClick = ChangeStatus
          end
          object chk4I3Reverse: TCheckBox
            Left = 118
            Top = 5
            Width = 50
            Height = 17
            Caption = #20803#20214'3'
            TabOrder = 2
            OnClick = ChangeStatus
          end
        end
      end
    end
    object tsPhase4PT: TTabSheet
      Caption = #19977#30456#22235#32447'(PT)'
      ImageIndex = 2
      object grp1: TGroupBox
        AlignWithMargins = True
        Left = 10
        Top = 5
        Width = 250
        Height = 177
        Margins.Left = 10
        Margins.Top = 5
        Margins.Right = 10
        Align = alTop
        Caption = #30005#21387#37096#20998
        TabOrder = 0
        object lbl15: TLabel
          Left = 9
          Top = 20
          Width = 57
          Height = 13
          AutoSize = False
          Caption = #19968#27425#26029#30456
        end
        object lbl16: TLabel
          Left = 16
          Top = 149
          Width = 33
          Height = 13
          AutoSize = False
          Caption = #25509#32447
        end
        object lbl17: TLabel
          Left = 16
          Top = 130
          Width = 33
          Height = 13
          AutoSize = False
          Caption = #34920#23614
        end
        object lbl18: TLabel
          Left = 9
          Top = 48
          Width = 52
          Height = 13
          Caption = #20108#27425#26029#30456
        end
        object lbl19: TLabel
          Left = 9
          Top = 92
          Width = 80
          Height = 13
          AutoSize = False
          Caption = #20108#27425#26497#24615#21453#25509
        end
        object pnl1: TPanel
          Left = 72
          Top = 18
          Width = 177
          Height = 22
          TabOrder = 0
          object chk4PTBrokenUc: TCheckBox
            Left = 121
            Top = 3
            Width = 49
            Height = 17
            Caption = 'C'#30456
            TabOrder = 2
            OnClick = ChangeStatus
          end
          object chk4PTBrokenUb: TCheckBox
            Left = 63
            Top = 1
            Width = 41
            Height = 17
            Caption = 'B'#30456
            TabOrder = 1
            OnClick = ChangeStatus
          end
          object chk4PTBrokenUa: TCheckBox
            Left = 6
            Top = 1
            Width = 39
            Height = 17
            Caption = 'A'#30456
            TabOrder = 0
            OnClick = ChangeStatus
          end
          object chk4PTBrokenUn: TCheckBox
            Left = 155
            Top = 4
            Width = 49
            Height = 17
            Caption = 'N'#30456
            TabOrder = 3
            Visible = False
            OnClick = ChangeStatus
          end
        end
        object pnl2: TPanel
          Left = 72
          Top = 111
          Width = 177
          Height = 62
          TabOrder = 1
          object rb4PTUabc: TRadioButton
            Left = 14
            Top = 4
            Width = 73
            Height = 17
            Caption = 'Uabc'
            Checked = True
            TabOrder = 0
            TabStop = True
            OnClick = ChangeStatus
          end
          object rb4PTUacb: TRadioButton
            Left = 94
            Top = 4
            Width = 73
            Height = 17
            Caption = 'Uacb'
            TabOrder = 1
            OnClick = ChangeStatus
          end
          object rb4PTUbca: TRadioButton
            Left = 94
            Top = 22
            Width = 73
            Height = 17
            Caption = 'Ubca'
            TabOrder = 3
            OnClick = ChangeStatus
          end
          object rb4PTUbac: TRadioButton
            Left = 14
            Top = 22
            Width = 73
            Height = 17
            Caption = 'Ubac'
            TabOrder = 2
            OnClick = ChangeStatus
          end
          object rb4PTUcab: TRadioButton
            Left = 14
            Top = 40
            Width = 73
            Height = 17
            Caption = 'Ucab'
            TabOrder = 4
            OnClick = ChangeStatus
          end
          object rb4PTUcba: TRadioButton
            Left = 94
            Top = 40
            Width = 73
            Height = 17
            Caption = 'Ucba'
            TabOrder = 5
            OnClick = ChangeStatus
          end
        end
        object pnl3: TPanel
          Left = 72
          Top = 45
          Width = 177
          Height = 40
          TabOrder = 2
          object chk4PTBrokenUsc: TCheckBox
            Left = 121
            Top = 1
            Width = 49
            Height = 17
            Caption = 'C'#30456
            TabOrder = 2
            OnClick = ChangeStatus
          end
          object chk4PTBrokenUsb: TCheckBox
            Left = 63
            Top = 1
            Width = 41
            Height = 17
            Caption = 'B'#30456
            TabOrder = 1
            OnClick = ChangeStatus
          end
          object chk4PTBrokenUsa: TCheckBox
            Left = 5
            Top = 1
            Width = 40
            Height = 17
            Caption = 'A'#30456
            TabOrder = 0
            OnClick = ChangeStatus
          end
          object chk4PTBrokenUsn: TCheckBox
            Left = 6
            Top = 21
            Width = 49
            Height = 17
            Caption = 'N'#30456
            TabOrder = 3
            Visible = False
            OnClick = ChangeStatus
          end
          object chk4PTGround: TCheckBox
            Left = 63
            Top = 24
            Width = 47
            Height = 14
            Caption = #22320#32447
            TabOrder = 4
            OnClick = ChangeStatus
          end
        end
        object pnl4: TPanel
          Left = 95
          Top = 88
          Width = 154
          Height = 26
          TabOrder = 3
          object chk4PT1: TCheckBox
            Left = 5
            Top = 3
            Width = 47
            Height = 17
            Caption = 'PT1'
            TabOrder = 0
            OnClick = ChangeStatus
          end
          object chk4PT2: TCheckBox
            Left = 56
            Top = 3
            Width = 47
            Height = 17
            Caption = 'PT2'
            TabOrder = 1
            OnClick = ChangeStatus
          end
          object chk4PT3: TCheckBox
            Left = 109
            Top = 3
            Width = 42
            Height = 17
            Caption = 'PT3'
            TabOrder = 2
            OnClick = ChangeStatus
          end
        end
      end
      object grp2: TGroupBox
        AlignWithMargins = True
        Left = 10
        Top = 190
        Width = 250
        Height = 243
        Margins.Left = 10
        Margins.Top = 5
        Margins.Right = 10
        Align = alTop
        Caption = #30005#27969#37096#20998
        TabOrder = 1
        object lbl20: TLabel
          Left = 16
          Top = 176
          Width = 33
          Height = 13
          AutoSize = False
          Caption = #25509#32447
        end
        object lbl21: TLabel
          Left = 16
          Top = 22
          Width = 50
          Height = 13
          AutoSize = False
          Caption = #26497#24615#21453#25509
        end
        object lbl22: TLabel
          Left = 16
          Top = 84
          Width = 50
          Height = 13
          AutoSize = False
          Caption = #20108#27425#24320#36335
        end
        object lbl23: TLabel
          Left = 16
          Top = 53
          Width = 50
          Height = 13
          AutoSize = False
          Caption = #20108#27425#30701#36335
        end
        object lbl24: TLabel
          Left = 16
          Top = 157
          Width = 33
          Height = 13
          AutoSize = False
          Caption = #34920#23614
        end
        object lbl14: TLabel
          Left = 16
          Top = 116
          Width = 50
          Height = 13
          AutoSize = False
          Caption = #25509#22320#26029#24320
        end
        object lbl27: TLabel
          Left = 17
          Top = 215
          Width = 55
          Height = 13
          AutoSize = False
          Caption = #34920#23614#21453#25509
        end
        object pnl4PTCTPN: TPanel
          Left = 72
          Top = 16
          Width = 177
          Height = 26
          TabOrder = 0
          object chk4PTCTPN1: TCheckBox
            Left = 16
            Top = 5
            Width = 43
            Height = 17
            Caption = 'CT1'
            TabOrder = 0
            OnClick = ChangeStatus
          end
          object chk4PTCTPN2: TCheckBox
            Left = 67
            Top = 5
            Width = 43
            Height = 17
            Caption = 'CT2'
            TabOrder = 1
            OnClick = ChangeStatus
          end
          object chk4PTCTPN3: TCheckBox
            Left = 123
            Top = 5
            Width = 43
            Height = 17
            Caption = 'CT3'
            TabOrder = 2
            OnClick = ChangeStatus
          end
        end
        object pnl4PTIOrder: TPanel
          Left = 72
          Top = 142
          Width = 177
          Height = 53
          TabOrder = 3
          object rb4PTIabc: TRadioButton
            Left = 16
            Top = 0
            Width = 73
            Height = 17
            Caption = 'Iabc'
            Checked = True
            TabOrder = 0
            TabStop = True
            OnClick = ChangeStatus
          end
          object rb4PTIacb: TRadioButton
            Left = 96
            Top = 0
            Width = 73
            Height = 17
            Caption = 'Iacb'
            TabOrder = 1
            OnClick = ChangeStatus
          end
          object rb4PTIbca: TRadioButton
            Left = 96
            Top = 16
            Width = 73
            Height = 17
            Caption = 'Ibca'
            TabOrder = 3
            OnClick = ChangeStatus
          end
          object rb4PTIbac: TRadioButton
            Left = 16
            Top = 16
            Width = 73
            Height = 17
            Caption = 'Ibac'
            TabOrder = 2
            OnClick = ChangeStatus
          end
          object rb4PTIcab: TRadioButton
            Left = 16
            Top = 32
            Width = 73
            Height = 17
            Caption = 'Icab'
            TabOrder = 4
            OnClick = ChangeStatus
          end
          object rb4PTIcba: TRadioButton
            Left = 96
            Top = 32
            Width = 73
            Height = 17
            Caption = 'Icba'
            TabOrder = 5
            OnClick = ChangeStatus
          end
        end
        object pnl4PTCTOpen: TPanel
          Left = 72
          Top = 78
          Width = 177
          Height = 26
          TabOrder = 2
          object chk4PTCTOpen1: TCheckBox
            Left = 18
            Top = 2
            Width = 43
            Height = 17
            Caption = 'a'#30456
            TabOrder = 0
            OnClick = ChangeStatus
          end
          object chk4PTCTOpen2: TCheckBox
            Left = 67
            Top = 2
            Width = 43
            Height = 17
            Caption = 'b'#30456
            TabOrder = 1
            OnClick = ChangeStatus
          end
          object chk4PTCTOpen3: TCheckBox
            Left = 123
            Top = 2
            Width = 43
            Height = 17
            Caption = 'c'#30456
            TabOrder = 2
            OnClick = ChangeStatus
          end
        end
        object pnl4PTCTShort: TPanel
          Left = 72
          Top = 47
          Width = 177
          Height = 26
          TabOrder = 1
          object chk4PTCTShort1: TCheckBox
            Left = 16
            Top = 5
            Width = 43
            Height = 17
            Caption = 'CT1'
            TabOrder = 0
            OnClick = ChangeStatus
          end
          object chk4PTCTShort2: TCheckBox
            Left = 67
            Top = 5
            Width = 43
            Height = 17
            Caption = 'CT2'
            TabOrder = 1
            OnClick = ChangeStatus
          end
          object chk4PTCTShort3: TCheckBox
            Left = 123
            Top = 5
            Width = 43
            Height = 17
            Caption = 'CT3'
            TabOrder = 2
            OnClick = ChangeStatus
          end
        end
        object pnl8: TPanel
          Left = 72
          Top = 110
          Width = 177
          Height = 26
          TabOrder = 4
          object chckbxIaGroundBroken4PT: TCheckBox
            Left = 16
            Top = 5
            Width = 43
            Height = 17
            Caption = 'a'#30456
            TabOrder = 0
            OnClick = ChangeStatus
          end
          object chckbxIbGroundBroken4PT: TCheckBox
            Left = 67
            Top = 5
            Width = 43
            Height = 17
            Caption = 'b'#30456
            TabOrder = 1
            OnClick = ChangeStatus
          end
          object chckbxIcGroundBroken4PT: TCheckBox
            Left = 123
            Top = 5
            Width = 43
            Height = 17
            Caption = 'c'#30456
            TabOrder = 2
            OnClick = ChangeStatus
          end
          object chkI4PTGround: TCheckBox
            Left = 142
            Top = -7
            Width = 47
            Height = 14
            Caption = #22320#32447
            TabOrder = 3
            Visible = False
            OnClick = ChangeStatus
          end
        end
        object pnl11: TPanel
          Left = 72
          Top = 206
          Width = 177
          Height = 30
          TabOrder = 5
          object chk4I2ReversePT: TCheckBox
            Left = 62
            Top = 5
            Width = 50
            Height = 17
            Caption = #20803#20214'2'
            TabOrder = 0
            OnClick = ChangeStatus
          end
          object chk4I1ReversePT: TCheckBox
            Left = 6
            Top = 5
            Width = 50
            Height = 17
            Caption = #20803#20214'1'
            TabOrder = 1
            OnClick = ChangeStatus
          end
          object chk4I3ReversePT: TCheckBox
            Left = 118
            Top = 5
            Width = 50
            Height = 17
            Caption = #20803#20214'3'
            TabOrder = 2
            OnClick = ChangeStatus
          end
        end
        object strngrd1: TStringGrid
          Left = 72
          Top = 137
          Width = 177
          Height = 68
          ColCount = 4
          DefaultColWidth = 30
          DefaultRowHeight = 12
          TabOrder = 6
          OnSelectCell = strngrd1SelectCell
          RowHeights = (
            12
            12
            12
            12
            12)
        end
      end
    end
  end
end
