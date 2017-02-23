object fUserGroupInfo: TfUserGroupInfo
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #32452#20449#24687
  ClientHeight = 316
  ClientWidth = 457
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
    Width = 437
    Height = 265
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 10
    Margins.Bottom = 10
    ActivePage = ts1
    Align = alTop
    TabOrder = 0
    object ts1: TTabSheet
      Caption = #20998#32452#20449#24687
      object lbl1: TLabel
        Left = 24
        Top = 24
        Width = 48
        Height = 13
        Caption = #20998#32452#21517#31216
      end
      object lbl3: TLabel
        Left = 24
        Top = 56
        Width = 48
        Height = 13
        Caption = #20998#32452#35828#26126
      end
      object edtGroupName: TEdit
        Left = 120
        Top = 21
        Width = 217
        Height = 21
        MaxLength = 15
        TabOrder = 0
      end
      object edtGroupDesc: TEdit
        Left = 120
        Top = 53
        Width = 217
        Height = 21
        MaxLength = 50
        TabOrder = 1
      end
    end
    object ts2: TTabSheet
      Caption = #26435#38480
      ImageIndex = 1
      object lbl4: TLabel
        Left = 16
        Top = 16
        Width = 48
        Height = 13
        Caption = #25152#26377#26435#38480
      end
      object lbl5: TLabel
        Left = 211
        Top = 15
        Width = 48
        Height = 13
        Caption = #20998#32452#26435#38480
      end
      object lstAllRight: TListBox
        Left = 16
        Top = 34
        Width = 137
        Height = 191
        ItemHeight = 13
        TabOrder = 0
        OnDblClick = btnAddClick
      end
      object lstCurrRight: TListBox
        Left = 211
        Top = 34
        Width = 144
        Height = 191
        ItemHeight = 13
        TabOrder = 1
        OnDblClick = btnDelClick
      end
      object btnAdd: TButton
        Left = 167
        Top = 39
        Width = 28
        Height = 26
        Caption = '>'
        TabOrder = 2
        OnClick = btnAddClick
      end
      object btnDel: TButton
        Left = 167
        Top = 66
        Width = 28
        Height = 26
        Caption = '<'
        TabOrder = 3
        OnClick = btnDelClick
      end
      object btnAddAll: TButton
        Left = 167
        Top = 93
        Width = 28
        Height = 26
        Caption = '>>'
        TabOrder = 4
        OnClick = btnAddAllClick
      end
      object btnDelAll: TButton
        Left = 167
        Top = 121
        Width = 28
        Height = 26
        Caption = '<<'
        TabOrder = 5
        OnClick = btnDelAllClick
      end
    end
  end
  object btnCancel: TButton
    Left = 306
    Top = 281
    Width = 75
    Height = 25
    Caption = #21462#28040
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object btnOK: TButton
    Left = 225
    Top = 281
    Width = 75
    Height = 25
    Caption = #30830#23450
    Default = True
    TabOrder = 2
    OnClick = btnOKClick
  end
end
