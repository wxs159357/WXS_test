object Form8: TForm8
  Left = 0
  Top = 0
  Caption = 'Form8'
  ClientHeight = 403
  ClientWidth = 679
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
  object mmo1: TMemo
    Left = 40
    Top = 16
    Width = 433
    Height = 289
    Lines.Strings = (
      'mmo1')
    TabOrder = 0
  end
  object rdgrp1: TRadioGroup
    Left = 486
    Top = 32
    Width = 155
    Height = 105
    Caption = #25130#22270#31867#22411
    ItemIndex = 0
    Items.Strings = (
      #30028#38754#21306#22495#25130#22270
      #30028#38754#30011#24067#25130#22270)
    TabOrder = 1
    OnClick = rdgrp1Click
  end
end
