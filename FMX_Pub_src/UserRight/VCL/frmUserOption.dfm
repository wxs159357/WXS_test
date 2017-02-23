object fUserOption: TfUserOption
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #29992#25143#36134#21495#31649#29702
  ClientHeight = 515
  ClientWidth = 688
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object btnCtlList: TCategoryButtons
    Left = 0
    Top = 0
    Width = 145
    Height = 515
    Align = alLeft
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderWidth = 1
    ButtonFlow = cbfVertical
    ButtonOptions = [boFullSize, boGradientFill, boShowCaptions, boBoldCaptions]
    Categories = <
      item
        Caption = #29992#25143#21644#32452
        Color = 15395839
        Collapsed = False
        Items = <>
      end>
    RegularButtonColor = clWhite
    SelectedButtonColor = 15132390
    TabOrder = 0
    OnButtonClicked = btnCtlListButtonClicked
  end
  object pnlOptions: TPanel
    Left = 145
    Top = 0
    Width = 543
    Height = 515
    Align = alClient
    TabOrder = 1
  end
end
