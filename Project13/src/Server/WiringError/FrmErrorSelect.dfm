object fErrorSelect: TfErrorSelect
  Left = 0
  Top = 0
  Caption = #38169#35823#36873#25321
  ClientHeight = 542
  ClientWidth = 310
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object scrlbx1: TScrollBox
    Left = 0
    Top = 0
    Width = 310
    Height = 542
    Align = alClient
    TabOrder = 0
    object grpbxgrp1: TGroupBox
      Left = 3
      Top = 55
      Width = 300
      Height = 215
      Caption = #20108#27425#25925#38556
      TabOrder = 0
      object grpbxgrp2: TGroupBox
        Left = 2
        Top = 97
        Width = 296
        Height = 116
        Align = alClient
        Caption = #30005#27969#37096#20998
        TabOrder = 1
        object lvSecondCurrentBreak: TListView
          Left = 2
          Top = 78
          Width = 292
          Height = 36
          Align = alClient
          Checkboxes = True
          Columns = <
            item
              Caption = 'A'
            end
            item
              Caption = 'B'
            end
            item
              Caption = 'C'
            end>
          Items.ItemData = {
            05720000000300000000000000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000
            00064100F876A5633057AD65005F00000000FFFFFFFFFFFFFFFF00000000FFFF
            FFFF00000000064200F876A5633057AD65005F00000000FFFFFFFFFFFFFFFF00
            000000FFFFFFFF00000000064300F876A5633057AD65005F}
          ReadOnly = True
          TabOrder = 1
          ViewStyle = vsList
          OnClick = lvSecondCurrentBreakClick
          OnDblClick = lvSecondCurrentBreakClick
        end
        object lvSecondCurrent: TListView
          Left = 2
          Top = 15
          Width = 292
          Height = 63
          Align = alTop
          Checkboxes = True
          Columns = <
            item
              Caption = 'A'
            end
            item
              Caption = 'B'
            end
            item
              Caption = 'C'
            end>
          Items.ItemData = {
            053E0100000900000000000000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000
            00044100F876005FEF8D00000000FFFFFFFFFFFFFFFF00000000FFFFFFFF0000
            0000044200F876005FEF8D00000000FFFFFFFFFFFFFFFF00000000FFFFFFFF00
            000000044300F876005FEF8D00000000FFFFFFFFFFFFFFFF00000000FFFFFFFF
            00000000044100F876ED77EF8D00000000FFFFFFFFFFFFFFFF00000000FFFFFF
            FF00000000044200F876ED77EF8D00000000FFFFFFFFFFFFFFFF00000000FFFF
            FFFF00000000044300F876ED77EF8D00000000FFFFFFFFFFFFFFFF00000000FF
            FFFFFF000000000654004100310081672760CD5300000000FFFFFFFFFFFFFFFF
            00000000FFFFFFFF000000000654004100320081672760CD5300000000FFFFFF
            FFFFFFFFFF00000000FFFFFFFF000000000654004100330081672760CD53}
          ReadOnly = True
          TabOrder = 0
          ViewStyle = vsList
          OnClick = lvSecondCurrentClick
          OnDblClick = lvSecondCurrentClick
        end
      end
      object grpbxgrp3: TGroupBox
        Left = 2
        Top = 15
        Width = 296
        Height = 82
        Align = alTop
        Caption = #30005#21387#37096#20998
        TabOrder = 0
        object lvSecondVol: TListView
          Left = 2
          Top = 15
          Width = 292
          Height = 65
          Align = alClient
          Checkboxes = True
          Columns = <>
          Items.ItemData = {
            05FA0000000700000000000000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000
            00044100F87631598B5300000000FFFFFFFFFFFFFFFF00000000FFFFFFFF0000
            0000044200F87631598B5300000000FFFFFFFFFFFFFFFF00000000FFFFFFFF00
            000000044300F87631598B5300000000FFFFFFFFFFFFFFFF00000000FFFFFFFF
            000000000654005600310081672760CD5300000000FFFFFFFFFFFFFFFF000000
            00FFFFFFFF000000000654005600320081672760CD5300000000FFFFFFFFFFFF
            FFFF00000000FFFFFFFF000000000654005600330081672760CD5300000000FF
            FFFFFFFFFFFFFF00000000FFFFFFFF0000000004A5633057AD65005F}
          ReadOnly = True
          TabOrder = 0
          ViewStyle = vsList
          OnClick = lvSecondVolClick
          OnDblClick = lvSecondVolClick
        end
      end
    end
    object grpbxFirstError: TGroupBox
      Left = 3
      Top = 3
      Width = 300
      Height = 52
      Caption = #19968#27425#25925#38556
      TabOrder = 1
      object lvFirstError: TListView
        Left = 2
        Top = 15
        Width = 296
        Height = 35
        Align = alClient
        Checkboxes = True
        Columns = <
          item
            Caption = 'A'
          end
          item
            Caption = 'B'
          end
          item
            Caption = 'C'
          end>
        Items.ItemData = {
          05660000000300000000000000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000
          00044100F876AD65F87600000000FFFFFFFFFFFFFFFF00000000FFFFFFFF0000
          0000044200F876AD65F87600000000FFFFFFFFFFFFFFFF00000000FFFFFFFF00
          000000044300F876AD65F876}
        ReadOnly = True
        TabOrder = 0
        ViewStyle = vsList
        OnClick = lvFirstErrorClick
        OnDblClick = lvFirstErrorClick
      end
    end
    object grpbx1: TGroupBox
      Left = 3
      Top = 271
      Width = 301
      Height = 267
      Caption = #30005#33021#34920#25925#38556
      TabOrder = 2
      object grpbxgrp6: TGroupBox
        Left = 2
        Top = 15
        Width = 297
        Height = 108
        Align = alTop
        Caption = #30005#21387#37096#20998
        TabOrder = 0
        object lvMeterVol: TListView
          Left = 2
          Top = 15
          Width = 293
          Height = 34
          Align = alTop
          Checkboxes = True
          Columns = <
            item
              Caption = 'A'
            end
            item
              Caption = 'B'
            end
            item
              Caption = 'C'
            end>
          Items.ItemData = {
            05660000000300000000000000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000
            00044100F87631598B5300000000FFFFFFFFFFFFFFFF00000000FFFFFFFF0000
            0000044200F87631598B5300000000FFFFFFFFFFFFFFFF00000000FFFFFFFF00
            000000044300F87631598B53}
          ReadOnly = True
          TabOrder = 0
          ViewStyle = vsList
          OnClick = lvMeterVolClick
        end
        object rdgrpMeterUSequence: TRadioGroup
          Left = 2
          Top = 49
          Width = 293
          Height = 57
          Align = alClient
          Caption = #30005#21387#30456#24207
          Columns = 4
          Items.Strings = (
            'Uabc'
            'Uacb'
            'Ubac'
            'Ubca'
            'Ucba'
            'Ucab')
          TabOrder = 1
          OnClick = rdgrpMeterUSequenceClick
        end
      end
      object grpbxgrp5: TGroupBox
        Left = 2
        Top = 123
        Width = 297
        Height = 142
        Align = alClient
        Caption = #30005#27969#37096#20998
        TabOrder = 1
        object lvMeterCurrent: TListView
          Left = 2
          Top = 15
          Width = 293
          Height = 63
          Align = alTop
          Checkboxes = True
          Columns = <
            item
              Caption = 'A'
            end
            item
              Caption = 'B'
            end
            item
              Caption = 'C'
            end>
          Items.ItemData = {
            05320100000900000000000000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000
            00044100F876005FEF8D00000000FFFFFFFFFFFFFFFF00000000FFFFFFFF0000
            0000044200F876005FEF8D00000000FFFFFFFFFFFFFFFF00000000FFFFFFFF00
            000000044300F876005FEF8D00000000FFFFFFFFFFFFFFFF00000000FFFFFFFF
            00000000044100F876ED77EF8D00000000FFFFFFFFFFFFFFFF00000000FFFFFF
            FF00000000044200F876ED77EF8D00000000FFFFFFFFFFFFFFFF00000000FFFF
            FFFF00000000044300F876ED77EF8D00000000FFFFFFFFFFFFFFFF00000000FF
            FFFFFF00000000044100F876CD53A56300000000FFFFFFFFFFFFFFFF00000000
            FFFFFFFF00000000044200F876CD53A56300000000FFFFFFFFFFFFFFFF000000
            00FFFFFFFF00000000044300F876CD53A563}
          ReadOnly = True
          TabOrder = 0
          ViewStyle = vsList
          OnClick = lvMeterCurrentClick
          OnDblClick = lvMeterCurrentClick
        end
        object rdgrpMeterISequence: TRadioGroup
          Left = 2
          Top = 78
          Width = 293
          Height = 59
          Align = alTop
          Caption = #30005#27969#30456#24207
          Columns = 4
          Items.Strings = (
            'Iabc'
            'Iacb'
            'Ibac'
            'Ibca'
            'Icba'
            'Icab')
          TabOrder = 1
          OnClick = rdgrpMeterISequenceClick
        end
      end
    end
  end
end
