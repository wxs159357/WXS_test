object fQInfo: TfQInfo
  Left = 0
  Top = 0
  Caption = #32771#39064#35814#32454#20449#24687
  ClientHeight = 655
  ClientWidth = 1003
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object spltr1: TSplitter
    Left = 313
    Top = 0
    Height = 655
    ExplicitLeft = 280
    ExplicitTop = 3
    ExplicitHeight = 476
  end
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 313
    Height = 655
    Align = alLeft
    TabOrder = 0
    object tbcntrl1: TTabControl
      Left = 1
      Top = 1
      Width = 311
      Height = 22
      Align = alTop
      TabOrder = 0
      Tabs.Strings = (
        #19977#30456#19977#32447
        #19977#30456#22235#32447)
      TabIndex = 0
      OnChange = tbcntrl1Change
      object chkIsElec: TCheckBox
        Left = 136
        Top = 2
        Width = 170
        Height = 17
        Caption = #34920#31665#22806#22771#24102#30005
        TabOrder = 0
        OnClick = chkIsElecClick
      end
    end
  end
  object pnl2: TPanel
    Left = 316
    Top = 0
    Width = 687
    Height = 655
    Align = alClient
    TabOrder = 1
    object lbl1: TLabel
      Left = 1
      Top = 641
      Width = 685
      Height = 13
      Align = alBottom
      Caption = 'lbl1'
      ExplicitLeft = 288
      ExplicitTop = 608
      ExplicitWidth = 16
    end
  end
end
