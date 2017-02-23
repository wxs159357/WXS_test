object fWEPhaseColor: TfWEPhaseColor
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #21521#37327#22270#39068#33394#35774#32622
  ClientHeight = 285
  ClientWidth = 458
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
  DesignSize = (
    458
    285)
  PixelsPerInch = 96
  TextHeight = 13
  object imgPhasePreview: TImage
    Left = 226
    Top = 16
    Width = 220
    Height = 216
  end
  object bvl2: TBevel
    Left = 0
    Top = 242
    Width = 458
    Height = 43
    Align = alBottom
    Shape = bsTopLine
    ExplicitTop = 228
    ExplicitWidth = 454
  end
  object grp1: TGroupBox
    Left = 18
    Top = 16
    Width = 202
    Height = 216
    Caption = #39068#33394#35774#32622
    TabOrder = 0
    object imgColor0: TImage
      Left = 80
      Top = 24
      Width = 73
      Height = 17
    end
    object imgColor1: TImage
      Tag = 1
      Left = 80
      Top = 127
      Width = 73
      Height = 17
    end
    object imgColor2: TImage
      Tag = 2
      Left = 80
      Top = 151
      Width = 73
      Height = 17
    end
    object imgColor3: TImage
      Tag = 3
      Left = 80
      Top = 175
      Width = 73
      Height = 17
    end
    object imgColor4: TImage
      Tag = 4
      Left = 80
      Top = 47
      Width = 73
      Height = 17
    end
    object lbl1: TLabel
      Left = 16
      Top = 48
      Width = 63
      Height = 17
      AutoSize = False
      Caption = 'XY'#36724
    end
    object lbl3: TLabel
      Left = 16
      Top = 25
      Width = 63
      Height = 17
      AutoSize = False
      Caption = #32972#26223
    end
    object lblColor1: TLabel
      Left = 16
      Top = 128
      Width = 63
      Height = 17
      AutoSize = False
      Caption = 'Ua, Ub, Uc'
    end
    object lblColor2: TLabel
      Left = 16
      Top = 152
      Width = 63
      Height = 17
      AutoSize = False
      Caption = 'Uab, Ia'
    end
    object lblColor3: TLabel
      Left = 16
      Top = 176
      Width = 63
      Height = 17
      AutoSize = False
      Caption = 'Ucb, Ic'
    end
    object bvl1: TBevel
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 192
      Height = 62
      Align = alTop
      Shape = bsBottomLine
    end
    object btnColor0: TBitBtn
      Left = 152
      Top = 24
      Width = 31
      Height = 17
      Caption = '...'
      TabOrder = 0
      OnClick = btnColor0Click
    end
    object btnColor1: TBitBtn
      Tag = 1
      Left = 152
      Top = 127
      Width = 31
      Height = 17
      Caption = '...'
      TabOrder = 1
      OnClick = btnColor0Click
    end
    object btnColor2: TBitBtn
      Tag = 2
      Left = 152
      Top = 151
      Width = 31
      Height = 17
      Caption = '...'
      TabOrder = 2
      OnClick = btnColor0Click
    end
    object btnColor3: TBitBtn
      Tag = 3
      Left = 152
      Top = 175
      Width = 31
      Height = 17
      Caption = '...'
      TabOrder = 3
      OnClick = btnColor0Click
    end
    object btnColor4: TBitBtn
      Tag = 4
      Left = 152
      Top = 47
      Width = 31
      Height = 17
      Caption = '...'
      TabOrder = 4
      OnClick = btnColor0Click
    end
    object rbPhase3Map: TRadioButton
      Left = 16
      Top = 101
      Width = 66
      Height = 17
      Caption = #19977#30456#19977#32447
      Checked = True
      TabOrder = 5
      TabStop = True
      OnClick = rbPhase3MapClick
    end
    object rbPhase4Map: TRadioButton
      Left = 110
      Top = 101
      Width = 66
      Height = 17
      Caption = #19977#30456#22235#32447
      TabOrder = 6
      OnClick = rbPhase3MapClick
    end
  end
  object btnOK: TButton
    Left = 291
    Top = 252
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = #30830#23450'(&O)'
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 371
    Top = 252
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = #21462#28040'(&C)'
    ModalResult = 2
    TabOrder = 2
  end
  object btnDefaultValue: TButton
    Left = 210
    Top = 252
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = #40664#35748#20540'(&D)'
    TabOrder = 3
    OnClick = btnDefaultValueClick
  end
  object dlgColor: TColorDialog
    Left = 40
    Top = 187
  end
end
