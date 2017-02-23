object fStudentLogin: TfStudentLogin
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = #23398#21592#30331#24405
  ClientHeight = 201
  ClientWidth = 325
  Color = clBtnFace
  Constraints.MaxHeight = 233
  Constraints.MaxWidth = 331
  Constraints.MinHeight = 229
  Constraints.MinWidth = 331
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lbl2: TLabel
    Left = 50
    Top = 71
    Width = 36
    Height = 13
    Caption = #30331#24405#21517
  end
  object lbl3: TLabel
    Left = 50
    Top = 107
    Width = 24
    Height = 13
    Caption = #23494#30721
  end
  object bvl1: TBevel
    Left = 0
    Top = 154
    Width = 325
    Height = 47
    Align = alBottom
    Shape = bsTopLine
    ExplicitTop = 115
    ExplicitWidth = 323
  end
  object edtName: TEdit
    Left = 118
    Top = 68
    Width = 153
    Height = 21
    TabOrder = 0
    Text = '002'
  end
  object edtPassword: TEdit
    Left = 118
    Top = 104
    Width = 153
    Height = 21
    TabOrder = 1
  end
  object btnLogin: TBitBtn
    Left = 159
    Top = 166
    Width = 75
    Height = 25
    Caption = #30331#24405
    TabOrder = 2
    OnClick = btnLoginClick
  end
  object btnCancel: TButton
    Left = 240
    Top = 166
    Width = 75
    Height = 25
    Caption = #21462#28040
    ModalResult = 2
    TabOrder = 3
  end
  object chk1: TCheckBox
    Left = 14
    Top = 170
    Width = 97
    Height = 17
    Caption = #31243#24207#21551#21160#26174#31034
    TabOrder = 4
  end
end
