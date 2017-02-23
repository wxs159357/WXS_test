object fUserNew: TfUserNew
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #29992#25143#20449#24687
  ClientHeight = 313
  ClientWidth = 419
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblNotice: TLabel
    Left = 14
    Top = 286
    Width = 30
    Height = 13
    Caption = 'Notice'
    Visible = False
  end
  object pgc1: TPageControl
    AlignWithMargins = True
    Left = 10
    Top = 10
    Width = 399
    Height = 265
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 10
    Margins.Bottom = 10
    ActivePage = ts1
    Align = alTop
    TabOrder = 0
    object ts1: TTabSheet
      Caption = #29992#25143#20449#24687
      object lbl1: TLabel
        Left = 24
        Top = 24
        Width = 36
        Height = 13
        Caption = #29992#25143#21517
      end
      object lbl2: TLabel
        Left = 24
        Top = 56
        Width = 48
        Height = 13
        Caption = #29992#25143#20840#31216
      end
      object lbl3: TLabel
        Left = 24
        Top = 91
        Width = 48
        Height = 13
        Caption = #29992#25143#35828#26126
      end
      object lbl6: TLabel
        Left = 24
        Top = 128
        Width = 24
        Height = 13
        Caption = #23494#30721
      end
      object lbl7: TLabel
        Left = 24
        Top = 168
        Width = 48
        Height = 13
        Caption = #30830#35748#23494#30721
      end
      object edtLognName: TEdit
        Left = 120
        Top = 21
        Width = 217
        Height = 21
        MaxLength = 15
        TabOrder = 0
      end
      object edtName: TEdit
        Left = 120
        Top = 53
        Width = 217
        Height = 21
        MaxLength = 25
        TabOrder = 1
      end
      object edtDesc: TEdit
        Left = 120
        Top = 88
        Width = 217
        Height = 21
        MaxLength = 50
        TabOrder = 2
      end
      object edtPass: TEdit
        Left = 120
        Top = 125
        Width = 217
        Height = 21
        MaxLength = 25
        PasswordChar = '*'
        TabOrder = 3
      end
      object edtYesPass: TEdit
        Left = 120
        Top = 165
        Width = 217
        Height = 21
        MaxLength = 25
        PasswordChar = '*'
        TabOrder = 4
      end
    end
    object ts2: TTabSheet
      Caption = #26435#38480
      ImageIndex = 1
      object lbl4: TLabel
        Left = 24
        Top = 24
        Width = 48
        Height = 13
        Caption = #20998#32452#21517#31216
      end
      object lbl5: TLabel
        Left = 24
        Top = 56
        Width = 24
        Height = 13
        Caption = #26435#38480
      end
      object cbbGroup: TComboBox
        Left = 104
        Top = 21
        Width = 233
        Height = 21
        AutoComplete = False
        Style = csDropDownList
        TabOrder = 0
        OnChange = cbbGroupChange
      end
      object lvGroup: TListView
        Left = 104
        Top = 56
        Width = 233
        Height = 161
        Columns = <
          item
            Caption = #26435#38480
            Width = 100
          end
          item
            Caption = #26435#38480#35828#26126
            Width = 150
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 1
        ViewStyle = vsReport
      end
    end
  end
  object btnOK: TButton
    Left = 225
    Top = 281
    Width = 75
    Height = 25
    Caption = #30830#23450
    Default = True
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 306
    Top = 281
    Width = 75
    Height = 25
    Caption = #21462#28040
    TabOrder = 2
    OnClick = btnCancelClick
  end
end
