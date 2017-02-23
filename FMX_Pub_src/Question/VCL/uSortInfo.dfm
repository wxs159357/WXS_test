object fSortInfo: TfSortInfo
  Left = 0
  Top = 0
  VertScrollBar.Visible = False
  BorderIcons = [biSystemMenu]
  Caption = #39064#24211#20449#24687
  ClientHeight = 265
  ClientWidth = 397
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblMemo: TLabel
    Left = 14
    Top = 53
    Width = 48
    Height = 13
    Caption = #39064#24211#22791#27880
  end
  object lbl1: TLabel
    Left = 14
    Top = 23
    Width = 48
    Height = 13
    Caption = #39064#24211#21517#31216
  end
  object mmoSortRemark: TMemo
    Left = 68
    Top = 50
    Width = 307
    Height = 151
    Lines.Strings = (
      '')
    MaxLength = 100
    TabOrder = 1
  end
  object edtSortName: TEdit
    Left = 68
    Top = 20
    Width = 307
    Height = 21
    TabOrder = 0
  end
  object pnl1: TPanel
    Left = 0
    Top = 224
    Width = 397
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      397
      41)
    object bvl1: TBevel
      Left = 0
      Top = 0
      Width = 397
      Height = 4
      Align = alTop
      Shape = bsBottomLine
    end
    object btnOK: TButton
      Left = 232
      Top = 10
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = #30830#23450'(&O)'
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 313
      Top = 10
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = #21462#28040'(&C)'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
