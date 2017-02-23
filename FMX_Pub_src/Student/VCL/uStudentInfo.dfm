object fStudentInfo: TfStudentInfo
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = #23398#21592#20449#24687
  ClientHeight = 346
  ClientWidth = 489
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lbl5: TLabel
    Left = 305
    Top = -30
    Width = 60
    Height = 13
    Caption = #36523#20221#35777#21495#65306
  end
  object btn1: TBitBtn
    Left = 315
    Top = 311
    Width = 75
    Height = 25
    Caption = #30830#23450
    Default = True
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    NumGlyphs = 2
    TabOrder = 0
    OnClick = btn1Click
  end
  object btn2: TBitBtn
    Left = 396
    Top = 311
    Width = 75
    Height = 25
    Align = alCustom
    Caption = #21462#28040
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 1
  end
  object pnlStuInfo: TPanel
    Left = 0
    Top = 0
    Width = 489
    Height = 305
    Align = alTop
    TabOrder = 2
    object lbl1: TLabel
      Left = 28
      Top = 19
      Width = 24
      Height = 13
      Caption = #22995#21517
    end
    object lbl2: TLabel
      Left = 28
      Top = 60
      Width = 36
      Height = 13
      Caption = #30331#24405#21517
    end
    object lbl3: TLabel
      Left = 28
      Top = 99
      Width = 24
      Height = 13
      Caption = #22320#21306
    end
    object lbl4: TLabel
      Left = 284
      Top = 19
      Width = 24
      Height = 13
      Caption = #24615#21035
    end
    object lbl6: TLabel
      Left = 284
      Top = 60
      Width = 24
      Height = 13
      Caption = #23494#30721
    end
    object lbl7: TLabel
      Left = 284
      Top = 99
      Width = 48
      Height = 13
      Caption = #36523#20221#35777#21495
    end
    object lbl8: TLabel
      Left = 28
      Top = 140
      Width = 24
      Height = 13
      Caption = #30005#35805
    end
    object lbl9: TLabel
      Left = 28
      Top = 176
      Width = 24
      Height = 13
      Caption = #22791#27880
    end
    object lbl10: TLabel
      Left = 208
      Top = 23
      Width = 8
      Height = 16
      Caption = '*'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object lbl11: TLabel
      Left = 208
      Top = 61
      Width = 8
      Height = 16
      Caption = '*'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object edtName: TEdit
      Left = 82
      Top = 18
      Width = 121
      Height = 21
      TabOrder = 0
    end
    object edtLogin: TEdit
      Left = 82
      Top = 57
      Width = 121
      Height = 21
      TabOrder = 3
    end
    object edtPassWord: TEdit
      Left = 350
      Top = 57
      Width = 121
      Height = 21
      TabOrder = 4
    end
    object edtTel: TEdit
      Left = 82
      Top = 135
      Width = 121
      Height = 21
      TabOrder = 7
    end
    object edtIDcard: TEdit
      Left = 350
      Top = 96
      Width = 121
      Height = 21
      TabOrder = 6
    end
    object mmoRemark: TMemo
      Left = 82
      Top = 176
      Width = 389
      Height = 121
      TabOrder = 8
    end
    object cbbArea: TComboBox
      Left = 82
      Top = 96
      Width = 121
      Height = 21
      TabOrder = 5
    end
    object rbMan: TRadioButton
      Left = 350
      Top = 18
      Width = 113
      Height = 17
      Caption = #30007
      TabOrder = 1
    end
    object rbWoMan: TRadioButton
      Left = 408
      Top = 18
      Width = 113
      Height = 17
      Caption = #22899
      TabOrder = 2
    end
  end
end
