object fPaperInfoBase: TfPaperInfoBase
  Left = 0
  Top = 0
  Caption = #32771#29983#32771#35797#35814#32454#32467#26524
  ClientHeight = 674
  ClientWidth = 756
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
  object spltr1: TSplitter
    Left = 0
    Top = 113
    Width = 756
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitWidth = 528
  end
  object pnl1: TPanel
    Left = 0
    Top = 641
    Width = 756
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      756
      33)
    object lbl3: TLabel
      Left = 177
      Top = 9
      Width = 12
      Height = 13
      Anchors = [akTop, akRight]
      Caption = #20998
      Visible = False
    end
    object lbl1: TLabel
      Left = 16
      Top = 9
      Width = 48
      Height = 13
      Caption = #32771#29983#24471#20998
    end
    object btnCancel: TButton
      Left = 671
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = #21462#28040
      ModalResult = 2
      TabOrder = 1
    end
    object btnSave: TButton
      Left = 590
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = #30830#23450
      ModalResult = 1
      TabOrder = 0
    end
    object btnPrint: TButton
      Left = 507
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = #25171#21360#32771#21367
      TabOrder = 2
      Visible = False
    end
    object edtScore: TEdit
      Left = 69
      Top = 6
      Width = 103
      Height = 21
      TabOrder = 3
      OnKeyPress = edtScoreKeyPress
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 116
    Width = 756
    Height = 525
    Align = alClient
    TabOrder = 1
    ExplicitTop = 33
    ExplicitHeight = 608
  end
  object lvQuestList: TListView
    Left = 0
    Top = 0
    Width = 756
    Height = 113
    Align = alTop
    Columns = <
      item
        Caption = #24207#21495
        Width = 45
      end
      item
        Caption = #32771#39064#32534#21495
        Width = 120
      end
      item
        Caption = #32771#39064#21517#31216
        Width = 400
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 2
    ViewStyle = vsReport
    ExplicitLeft = 8
    ExplicitTop = 8
    ExplicitWidth = 647
  end
end
