object fExamInfo: TfExamInfo
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #32771#35797#20449#24687
  ClientHeight = 210
  ClientWidth = 403
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
    Left = 151
    Top = 67
    Width = 12
    Height = 13
    Caption = #20998
  end
  object edtExamName: TLabeledEdit
    Left = 72
    Top = 24
    Width = 289
    Height = 21
    EditLabel.Width = 48
    EditLabel.Height = 13
    EditLabel.Caption = #32771#35797#21517#31216
    LabelPosition = lpLeft
    TabOrder = 0
  end
  object edtTime: TLabeledEdit
    Left = 72
    Top = 64
    Width = 73
    Height = 21
    EditLabel.Width = 48
    EditLabel.Height = 13
    EditLabel.Caption = #32771#35797#26102#38388
    LabelPosition = lpLeft
    TabOrder = 1
    OnKeyPress = edtTimeKeyPress
  end
  object pnl1: TPanel
    Left = 0
    Top = 169
    Width = 403
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitLeft = 32
    ExplicitTop = 160
    ExplicitWidth = 185
    DesignSize = (
      403
      41)
    object bvl1: TBevel
      Left = 0
      Top = 0
      Width = 403
      Height = 50
      Align = alTop
      Shape = bsTopLine
      ExplicitLeft = 168
      ExplicitTop = -8
      ExplicitWidth = 50
    end
    object btn1: TButton
      Left = 233
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = #30830#23450
      ModalResult = 1
      TabOrder = 0
      ExplicitLeft = 223
    end
    object btn2: TButton
      Left = 314
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = #21462#28040
      ModalResult = 2
      TabOrder = 1
      ExplicitLeft = 304
    end
  end
end
