object fQuestionInfo: TfQuestionInfo
  Left = 0
  Top = 0
  Caption = #32771#39064#35814#32454#20449#24687
  ClientHeight = 631
  ClientWidth = 844
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
  object pgcntrl1: TPageControl
    Left = 0
    Top = 0
    Width = 844
    Height = 590
    ActivePage = tbsht2
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 788
    ExplicitHeight = 457
    object tbsht1: TTabSheet
      Caption = #22522#26412#20449#24687
      ExplicitWidth = 780
      ExplicitHeight = 429
      object lbl1: TLabel
        Left = 24
        Top = 32
        Width = 48
        Height = 13
        Caption = #32771#39064#21517#31216
      end
      object lbl2: TLabel
        Left = 24
        Top = 64
        Width = 48
        Height = 13
        Caption = #32771#39064#25551#36848
      end
      object lbl3: TLabel
        Left = 24
        Top = 166
        Width = 48
        Height = 13
        Caption = #32771#39064#22791#27880
      end
      object edtQuestionName: TEdit
        Left = 78
        Top = 29
        Width = 491
        Height = 21
        TabOrder = 0
      end
      object mmoQuestionDescribe: TMemo
        Left = 78
        Top = 61
        Width = 491
        Height = 89
        TabOrder = 1
      end
      object mmoQuestionRemark: TMemo
        Left = 78
        Top = 163
        Width = 491
        Height = 102
        TabOrder = 2
      end
    end
    object tbsht2: TTabSheet
      Caption = #35814#32454#20449#24687
      ImageIndex = 1
      ExplicitWidth = 780
      ExplicitHeight = 429
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 590
    Width = 844
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 1
    ExplicitTop = 456
    ExplicitWidth = 375
    object pnlBR: TPanel
      Left = 624
      Top = 0
      Width = 220
      Height = 41
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        220
        41)
      object btnOK: TButton
        Left = 55
        Top = 9
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = #30830#23450'(&O)'
        ModalResult = 1
        TabOrder = 0
        ExplicitLeft = 212
      end
      object btnCancel: TButton
        Left = 137
        Top = 9
        Width = 74
        Height = 25
        Anchors = [akTop, akRight]
        Caption = #21462#28040'(&C)'
        ModalResult = 2
        TabOrder = 1
        ExplicitLeft = 294
      end
    end
  end
end
