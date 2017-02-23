object fMainTest: TfMainTest
  Left = 0
  Top = 0
  Caption = 'Test'
  ClientHeight = 155
  ClientWidth = 431
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lbl2: TLabel
    Left = 89
    Top = 13
    Width = 140
    Height = 13
    Caption = #21021#22987#30331#24405#21517'admin,'#23494#30721#20026#31354
  end
  object lbl1: TLabel
    Left = 89
    Top = 40
    Width = 48
    Height = 13
    Caption = #29992#25143#20449#24687
  end
  object btn1: TButton
    Left = 8
    Top = 9
    Width = 75
    Height = 25
    Caption = #30331#24405
    TabOrder = 0
    OnClick = btn1Click
  end
  object btn5: TButton
    Left = 8
    Top = 57
    Width = 75
    Height = 25
    Caption = #27880#38144
    TabOrder = 1
    OnClick = btn5Click
  end
  object btn2: TButton
    Left = 8
    Top = 88
    Width = 75
    Height = 25
    Caption = #20462#25913#23494#30721
    TabOrder = 2
    OnClick = btn2Click
  end
  object btn7: TButton
    Left = 8
    Top = 119
    Width = 75
    Height = 25
    Caption = #29992#25143#32452#20449#24687
    TabOrder = 3
    OnClick = btn7Click
  end
  object mmo1: TMemo
    Left = 89
    Top = 59
    Width = 237
    Height = 85
    Lines.Strings = (
      '')
    TabOrder = 4
  end
end
