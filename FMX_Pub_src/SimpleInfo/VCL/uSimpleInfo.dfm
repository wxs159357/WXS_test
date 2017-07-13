object fSimpleInfo: TfSimpleInfo
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #22522#26412#20449#24687
  ClientHeight = 312
  ClientWidth = 471
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
  object lbl1: TLabel
    Left = 32
    Top = 40
    Width = 24
    Height = 13
    Caption = #21517#31216
  end
  object lbl2: TLabel
    Left = 32
    Top = 80
    Width = 24
    Height = 13
    Caption = #22791#27880
  end
  object pnl1: TPanel
    Left = 0
    Top = 271
    Width = 471
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      471
      41)
    object bvl1: TBevel
      Left = 0
      Top = 0
      Width = 471
      Height = 2
      Align = alTop
      Shape = bsTopLine
      ExplicitWidth = 498
    end
    object btn1: TButton
      Left = 306
      Top = 9
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = #30830#23450
      ModalResult = 1
      TabOrder = 0
      ExplicitLeft = 333
    end
    object btn2: TButton
      Left = 385
      Top = 9
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = #21462#28040
      ModalResult = 2
      TabOrder = 1
      ExplicitLeft = 412
    end
  end
  object edtSIName: TEdit
    Left = 72
    Top = 37
    Width = 361
    Height = 21
    MaxLength = 100
    TabOrder = 1
    Text = 'edtSIName'
  end
  object mmoSIRemark: TMemo
    Left = 72
    Top = 77
    Width = 361
    Height = 132
    Lines.Strings = (
      'mmoSIRemark')
    MaxLength = 200
    TabOrder = 2
  end
end
