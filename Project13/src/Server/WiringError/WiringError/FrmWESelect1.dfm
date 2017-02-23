object fWESelect1: TfWESelect1
  Left = 35
  Top = 164
  Caption = #36873#25321#32771#39064
  ClientHeight = 439
  ClientWidth = 291
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pgcErrors: TPageControl
    Left = 0
    Top = 0
    Width = 291
    Height = 439
    ActivePage = tsPhase3
    Align = alClient
    MultiLine = True
    TabHeight = 25
    TabOrder = 0
    TabWidth = 100
    OnChange = pgcErrorsChange
    object tsPhase3: TTabSheet
      Caption = #19977#30456#19977#32447
      object GroupBox1: TGroupBox
        AlignWithMargins = True
        Left = 10
        Top = 5
        Width = 263
        Height = 205
        Margins.Left = 10
        Margins.Top = 5
        Margins.Right = 10
        Align = alTop
        Caption = #30005#21387#37096#20998
        TabOrder = 0
        object Label9: TLabel
          Left = 16
          Top = 78
          Width = 26
          Height = 13
          Caption = #20108#27425
        end
        object lbl2: TLabel
          Left = 16
          Top = 97
          Width = 26
          Height = 13
          Caption = #26497#24615
        end
        object lbl7: TLabel
          Left = 16
          Top = 142
          Width = 26
          Height = 13
          Caption = #34920#23614
        end
        object lbl3: TLabel
          Left = 16
          Top = 161
          Width = 26
          Height = 13
          Caption = #25509#32447
        end
        object lbl6: TLabel
          Left = 16
          Top = 21
          Width = 55
          Height = 13
          AutoSize = False
          Caption = #19968#27425#26029#30456
        end
        object lbl8: TLabel
          Left = 16
          Top = 46
          Width = 55
          Height = 13
          AutoSize = False
          Caption = #20108#27425#26029#30456
        end
        object pnl3BrokenS: TPanel
          Left = 88
          Top = 15
          Width = 161
          Height = 25
          TabOrder = 0
          object chk3BrokenSa: TCheckBox
            Left = 18
            Top = 4
            Width = 41
            Height = 17
            Caption = 'A'#30456
            TabOrder = 0
            OnClick = ChangeStatus
          end
          object chk3BrokenSb: TCheckBox
            Left = 65
            Top = 4
            Width = 41
            Height = 17
            Caption = 'B'#30456
            TabOrder = 1
            OnClick = ChangeStatus
          end
          object chk3BrokenSc: TCheckBox
            Left = 112
            Top = 4
            Width = 41
            Height = 17
            Caption = 'C'#30456
            TabOrder = 2
            OnClick = ChangeStatus
          end
        end
        object pnl3BrokenU: TPanel
          Left = 88
          Top = 40
          Width = 161
          Height = 25
          TabOrder = 1
          object chk3BrokenUc: TCheckBox
            Left = 112
            Top = 5
            Width = 41
            Height = 17
            Caption = 'c'#30456
            TabOrder = 2
            OnClick = ChangeStatus
          end
          object chk3BrokenUb: TCheckBox
            Left = 65
            Top = 5
            Width = 41
            Height = 17
            Caption = 'b'#30456
            TabOrder = 1
            OnClick = ChangeStatus
          end
          object chk3BrokenUa: TCheckBox
            Left = 18
            Top = 5
            Width = 41
            Height = 17
            Caption = 'a'#30456
            TabOrder = 0
            OnClick = ChangeStatus
          end
        end
        object pnl3Uab: TPanel
          Left = 56
          Top = 69
          Width = 193
          Height = 26
          TabOrder = 2
          object rb3UabN: TRadioButton
            Left = 107
            Top = 4
            Width = 65
            Height = 17
            Caption = 'PT1'#21453
            TabOrder = 1
            OnClick = ChangeStatus
          end
          object rb3UabP: TRadioButton
            Left = 19
            Top = 4
            Width = 65
            Height = 17
            Caption = 'PT1'#27491
            Checked = True
            TabOrder = 0
            TabStop = True
            OnClick = ChangeStatus
          end
        end
        object pnl3Ucb: TPanel
          Left = 56
          Top = 95
          Width = 193
          Height = 26
          TabOrder = 3
          object rb3UcbP: TRadioButton
            Left = 19
            Top = 5
            Width = 65
            Height = 17
            Caption = 'PT2'#27491
            Checked = True
            TabOrder = 0
            TabStop = True
            OnClick = ChangeStatus
          end
          object rb3UcbN: TRadioButton
            Left = 106
            Top = 5
            Width = 65
            Height = 17
            Caption = 'PT2'#21453
            TabOrder = 1
            OnClick = ChangeStatus
          end
        end
        object pnl3UOrder: TPanel
          Left = 56
          Top = 123
          Width = 193
          Height = 73
          TabOrder = 4
          object rb3Uabc: TRadioButton
            Left = 19
            Top = 8
            Width = 73
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
            Width = 73
            Height = 17
            Caption = 'Uacb'
            TabOrder = 1
            OnClick = ChangeStatus
          end
          object rb3Ubca: TRadioButton
            Left = 106
            Top = 28
            Width = 73
            Height = 17
            Caption = 'Ubca'
            TabOrder = 3
            OnClick = ChangeStatus
          end
          object rb3Ubac: TRadioButton
            Left = 19
            Top = 28
            Width = 73
            Height = 17
            Caption = 'Ubac'
            TabOrder = 2
            OnClick = ChangeStatus
          end
          object rb3Ucab: TRadioButton
            Left = 19
            Top = 48
            Width = 73
            Height = 17
            Caption = 'Ucab'
            TabOrder = 4
            OnClick = ChangeStatus
          end
          object rb3Ucba: TRadioButton
            Left = 106
            Top = 48
            Width = 73
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
        Top = 218
        Width = 263
        Height = 177
        Margins.Left = 10
        Margins.Top = 5
        Margins.Right = 10
        Align = alTop
        Caption = #30005#27969#37096#20998
        TabOrder = 1
        object Label10: TLabel
          Left = 17
          Top = 28
          Width = 26
          Height = 13
          Caption = #20108#27425
        end
        object Label11: TLabel
          Left = 17
          Top = 48
          Width = 26
          Height = 13
          Caption = #26497#24615
        end
        object lbl4: TLabel
          Left = 16
          Top = 97
          Width = 33
          Height = 13
          AutoSize = False
          Caption = #34920#23614
        end
        object lbl5: TLabel
          Left = 16
          Top = 116
          Width = 33
          Height = 13
          AutoSize = False
          Caption = #25509#32447
        end
        object pnl3Ia: TPanel
          Left = 72
          Top = 16
          Width = 177
          Height = 30
          TabOrder = 0
          object rb3IaN: TRadioButton
            Left = 96
            Top = 8
            Width = 65
            Height = 17
            Caption = 'CT1'#21453
            TabOrder = 1
            OnClick = ChangeStatus
          end
          object rb3IaP: TRadioButton
            Left = 16
            Top = 8
            Width = 65
            Height = 17
            Caption = 'CT1'#27491
            Checked = True
            TabOrder = 0
            TabStop = True
            OnClick = ChangeStatus
          end
        end
        object pnl3Ic: TPanel
          Left = 72
          Top = 48
          Width = 177
          Height = 30
          TabOrder = 1
          object rb3IcP: TRadioButton
            Left = 16
            Top = 8
            Width = 65
            Height = 17
            Caption = 'CT2'#27491
            Checked = True
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = #23435#20307
            Font.Style = []
            ParentFont = False
            TabOrder = 0
            TabStop = True
            OnClick = ChangeStatus
          end
          object rb3IcN: TRadioButton
            Left = 96
            Top = 8
            Width = 65
            Height = 17
            Caption = 'CT2'#21453
            TabOrder = 1
            OnClick = ChangeStatus
          end
        end
        object sgd3Order: TStringGrid
          Left = 72
          Top = 93
          Width = 178
          Height = 43
          Cursor = crHandPoint
          ColCount = 3
          Ctl3D = False
          DefaultColWidth = 58
          DefaultRowHeight = 20
          DefaultDrawing = False
          RowCount = 2
          FixedRows = 0
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine]
          ParentCtl3D = False
          ScrollBars = ssNone
          TabOrder = 2
          OnClick = sgd3OrderClick
          OnDrawCell = sgd3OrderDrawCell
          RowHeights = (
            20
            20)
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
        Width = 263
        Height = 177
        Margins.Left = 10
        Margins.Top = 5
        Margins.Right = 10
        Align = alTop
        Caption = #30005#21387#37096#20998
        TabOrder = 0
        object lbl1: TLabel
          Left = 16
          Top = 38
          Width = 33
          Height = 13
          AutoSize = False
          Caption = #26029#30456
        end
        object lbl11: TLabel
          Left = 16
          Top = 127
          Width = 33
          Height = 13
          AutoSize = False
          Caption = #25509#32447
        end
        object lbl12: TLabel
          Left = 16
          Top = 108
          Width = 33
          Height = 13
          AutoSize = False
          Caption = #34920#23614
        end
        object pnl4BrokenU: TPanel
          Left = 72
          Top = 19
          Width = 177
          Height = 50
          TabOrder = 0
          object chk4BrokenUc: TCheckBox
            Left = 16
            Top = 28
            Width = 49
            Height = 17
            Caption = 'Uc'
            TabOrder = 2
            OnClick = ChangeStatus
          end
          object chk4BrokenUb: TCheckBox
            Left = 96
            Top = 4
            Width = 41
            Height = 17
            Caption = 'Ub'
            TabOrder = 1
            OnClick = ChangeStatus
          end
          object chk4BrokenUa: TCheckBox
            Left = 16
            Top = 4
            Width = 41
            Height = 17
            Caption = 'Ua'
            TabOrder = 0
            OnClick = ChangeStatus
          end
          object chk4BrokenUn: TCheckBox
            Left = 96
            Top = 29
            Width = 49
            Height = 17
            Caption = 'Un'
            TabOrder = 3
            OnClick = ChangeStatus
          end
        end
        object pnl4UOrder: TPanel
          Left = 72
          Top = 80
          Width = 177
          Height = 89
          TabOrder = 1
          object rb4Uabc: TRadioButton
            Left = 16
            Top = 11
            Width = 73
            Height = 17
            Caption = 'Uabc'
            Checked = True
            TabOrder = 0
            TabStop = True
            OnClick = ChangeStatus
          end
          object rb4Uacb: TRadioButton
            Left = 96
            Top = 11
            Width = 73
            Height = 17
            Caption = 'Uacb'
            TabOrder = 1
            OnClick = ChangeStatus
          end
          object rb4Ubca: TRadioButton
            Left = 96
            Top = 35
            Width = 73
            Height = 17
            Caption = 'Ubca'
            TabOrder = 3
            OnClick = ChangeStatus
          end
          object rb4Ubac: TRadioButton
            Left = 16
            Top = 35
            Width = 73
            Height = 17
            Caption = 'Ubac'
            TabOrder = 2
            OnClick = ChangeStatus
          end
          object rb4Ucab: TRadioButton
            Left = 16
            Top = 59
            Width = 73
            Height = 17
            Caption = 'Ucab'
            TabOrder = 4
            OnClick = ChangeStatus
          end
          object rb4Ucba: TRadioButton
            Left = 96
            Top = 59
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
        Width = 263
        Height = 205
        Margins.Left = 10
        Margins.Top = 5
        Margins.Right = 10
        Align = alTop
        Caption = #30005#27969#37096#20998
        TabOrder = 1
        object Label2: TLabel
          Left = 16
          Top = 40
          Width = 33
          Height = 13
          AutoSize = False
          Caption = #20108#27425
        end
        object Label3: TLabel
          Left = 16
          Top = 59
          Width = 33
          Height = 13
          AutoSize = False
          Caption = #26497#24615
        end
        object lbl9: TLabel
          Left = 16
          Top = 128
          Width = 33
          Height = 13
          AutoSize = False
          Caption = #34920#23614
        end
        object lbl10: TLabel
          Left = 16
          Top = 147
          Width = 33
          Height = 13
          AutoSize = False
          Caption = #25509#32447
        end
        object pnl4Ia: TPanel
          Left = 72
          Top = 16
          Width = 177
          Height = 25
          TabOrder = 0
          object rb4IaN: TRadioButton
            Left = 96
            Top = 5
            Width = 65
            Height = 17
            Caption = 'CT1'#21453
            TabOrder = 1
            OnClick = ChangeStatus
          end
          object rb4IaP: TRadioButton
            Left = 16
            Top = 5
            Width = 65
            Height = 17
            Caption = 'CT1'#27491
            Checked = True
            TabOrder = 0
            TabStop = True
            OnClick = ChangeStatus
          end
        end
        object pnl4Ib: TPanel
          Left = 72
          Top = 42
          Width = 177
          Height = 25
          TabOrder = 1
          object rb4IbP: TRadioButton
            Left = 16
            Top = 5
            Width = 65
            Height = 17
            Caption = 'CT2'#27491
            Checked = True
            TabOrder = 0
            TabStop = True
            OnClick = ChangeStatus
          end
          object rb4IbN: TRadioButton
            Left = 96
            Top = 5
            Width = 65
            Height = 17
            Caption = 'CT2'#21453
            TabOrder = 1
            OnClick = ChangeStatus
          end
        end
        object pnl4Ic: TPanel
          Left = 72
          Top = 68
          Width = 177
          Height = 25
          TabOrder = 2
          object rb4IcP: TRadioButton
            Left = 16
            Top = 5
            Width = 65
            Height = 17
            Caption = 'CT3'#27491
            Checked = True
            TabOrder = 0
            TabStop = True
            OnClick = ChangeStatus
          end
          object rb4IcN: TRadioButton
            Left = 96
            Top = 5
            Width = 65
            Height = 17
            Caption = 'CT3'#21453
            TabOrder = 1
            OnClick = ChangeStatus
          end
        end
        object pnl4IOrder: TPanel
          Left = 72
          Top = 104
          Width = 177
          Height = 81
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
            Top = 32
            Width = 73
            Height = 17
            Caption = 'Ibca'
            TabOrder = 3
            OnClick = ChangeStatus
          end
          object rb4Ibac: TRadioButton
            Left = 16
            Top = 32
            Width = 73
            Height = 17
            Caption = 'Ibac'
            TabOrder = 2
            OnClick = ChangeStatus
          end
          object rb4Icab: TRadioButton
            Left = 16
            Top = 56
            Width = 73
            Height = 17
            Caption = 'Icab'
            TabOrder = 4
            OnClick = ChangeStatus
          end
          object rb4Icba: TRadioButton
            Left = 96
            Top = 56
            Width = 73
            Height = 17
            Caption = 'Icba'
            TabOrder = 5
            OnClick = ChangeStatus
          end
        end
      end
    end
  end
end
