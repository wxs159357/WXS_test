object fUpUserPass: TfUpUserPass
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #20462#25913#23494#30721
  ClientHeight = 159
  ClientWidth = 359
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
  object lblConfirm: TLabel
    Left = 29
    Top = 82
    Width = 48
    Height = 13
    Caption = #30830#35748#23494#30721
  end
  object lblNew: TLabel
    Left = 29
    Top = 49
    Width = 36
    Height = 13
    Caption = #26032#23494#30721
  end
  object Bevel1: TBevel
    Left = 8
    Top = 103
    Width = 337
    Height = 9
    Shape = bsBottomLine
  end
  object lblOld: TLabel
    Left = 29
    Top = 18
    Width = 36
    Height = 13
    Caption = #26087#23494#30721
  end
  object btnCancel: TButton
    Left = 257
    Top = 118
    Width = 75
    Height = 25
    Caption = #21462#28040
    TabOrder = 0
    OnClick = btnCancelClick
  end
  object btnOK: TButton
    Left = 161
    Top = 118
    Width = 75
    Height = 25
    Caption = #30830#23450
    TabOrder = 1
    OnClick = btnOKClick
  end
  object edtConfirm: TEdit
    Left = 115
    Top = 77
    Width = 217
    Height = 21
    MaxLength = 21
    PasswordChar = '*'
    TabOrder = 2
  end
  object edtNew: TEdit
    Left = 115
    Top = 46
    Width = 217
    Height = 21
    MaxLength = 21
    PasswordChar = '*'
    TabOrder = 3
  end
  object edtOld: TEdit
    Left = 115
    Top = 10
    Width = 217
    Height = 21
    MaxLength = 21
    PasswordChar = '*'
    TabOrder = 4
  end
end
