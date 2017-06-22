object fRevScreenMain: TfRevScreenMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = #25509#25910#23631#24149
  ClientHeight = 446
  ClientWidth = 639
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object img1: TImage
    Left = 0
    Top = 0
    Width = 639
    Height = 357
    Align = alClient
    ExplicitLeft = 64
    ExplicitTop = 24
    ExplicitWidth = 105
    ExplicitHeight = 105
  end
  object mmo1: TMemo
    Left = 0
    Top = 357
    Width = 639
    Height = 89
    Align = alBottom
    Lines.Strings = (
      'mmo1')
    TabOrder = 0
    Visible = False
  end
end
