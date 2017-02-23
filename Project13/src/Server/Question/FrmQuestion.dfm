object fQuestion: TfQuestion
  Left = 0
  Top = 0
  Caption = #32771#39064
  ClientHeight = 464
  ClientWidth = 641
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pnl1: TPanel
    Left = 0
    Top = 423
    Width = 641
    Height = 41
    Align = alBottom
    TabOrder = 0
    object btn1: TButton
      Left = 15
      Top = 8
      Width = 75
      Height = 25
      Caption = #26032#24314
      TabOrder = 0
    end
    object btn2: TButton
      Left = 96
      Top = 8
      Width = 75
      Height = 25
      Caption = #20462#25913
      TabOrder = 1
    end
    object btn3: TButton
      Left = 179
      Top = 8
      Width = 75
      Height = 25
      Caption = #21024#38500
      TabOrder = 2
    end
  end
  object grp1: TGroupBox
    Left = 0
    Top = 0
    Width = 641
    Height = 423
    Align = alClient
    Caption = #32771#39064#21015#34920
    TabOrder = 1
    object lv1: TListView
      Left = 2
      Top = 15
      Width = 637
      Height = 406
      Align = alClient
      Columns = <
        item
          Caption = #32534#21495
        end
        item
          Caption = #31867#22411
        end
        item
          Caption = #21517#31216
          Width = 200
        end
        item
          Caption = #38590#24230#31561#32423
          Width = 65
        end
        item
          Caption = #25551#36848
          Width = 500
        end>
      TabOrder = 0
      ViewStyle = vsReport
    end
  end
end
