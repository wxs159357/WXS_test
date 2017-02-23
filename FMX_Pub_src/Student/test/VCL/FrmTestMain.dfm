object fTestMain: TfTestMain
  Left = 0
  Top = 0
  Caption = 'fTestMain'
  ClientHeight = 349
  ClientWidth = 627
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btn1: TButton
    Left = 56
    Top = 32
    Width = 75
    Height = 25
    Caption = #23398#21592#31649#29702
    TabOrder = 0
    OnClick = btn1Click
  end
  object btn2: TButton
    Left = 168
    Top = 32
    Width = 75
    Height = 25
    Caption = #36873#25321#23398#21592
    TabOrder = 1
    OnClick = btn2Click
  end
  object mmo1: TMemo
    Left = 56
    Top = 88
    Width = 233
    Height = 153
    Lines.Strings = (
      'mmo1')
    TabOrder = 2
  end
end
