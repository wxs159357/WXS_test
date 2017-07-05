object fRevScreenMain: TfRevScreenMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = #25509#25910#23631#24149
  ClientHeight = 495
  ClientWidth = 645
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
  object spltr1: TSplitter
    Left = 0
    Top = 403
    Width = 645
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 0
    ExplicitWidth = 406
  end
  object mmo1: TMemo
    Left = 0
    Top = 406
    Width = 645
    Height = 89
    Align = alBottom
    Lines.Strings = (
      'mmo1')
    TabOrder = 0
    Visible = False
  end
  object scrlbx1: TScrollBox
    Left = 0
    Top = 0
    Width = 645
    Height = 403
    Align = alClient
    TabOrder = 1
    object img1: TImage
      Left = 0
      Top = 0
      Width = 641
      Height = 401
      Cursor = crHandPoint
      OnMouseDown = img1MouseDown
      OnMouseMove = img1MouseMove
      OnMouseUp = img1MouseUp
    end
  end
  object tmr1: TTimer
    OnTimer = tmr1Timer
    Left = 88
    Top = 48
  end
end
