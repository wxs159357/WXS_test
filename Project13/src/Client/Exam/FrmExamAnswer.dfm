object fExamAnswer: TfExamAnswer
  Left = 0
  Top = 0
  Caption = #32771#35797
  ClientHeight = 517
  ClientWidth = 809
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  PixelsPerInch = 96
  TextHeight = 13
  object spltr1: TSplitter
    Left = 201
    Top = 0
    Height = 517
    ExplicitLeft = 408
    ExplicitTop = 224
    ExplicitHeight = 100
  end
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 201
    Height = 517
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object lvSubList: TListView
      Left = 0
      Top = 41
      Width = 201
      Height = 435
      Align = alClient
      Columns = <
        item
          Caption = #32771#39064#32534#21495
          Width = 75
        end
        item
          Caption = #32771#39064#21517#31216
          Width = 200
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      ExplicitLeft = 1
      ExplicitTop = 1
      ExplicitWidth = 199
      ExplicitHeight = 515
    end
    object pnlLastTime: TPanel
      Left = 0
      Top = 0
      Width = 201
      Height = 41
      Align = alTop
      Caption = '00:30:00'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -21
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentBackground = False
      ParentFont = False
      TabOrder = 1
      ExplicitWidth = 289
    end
    object pnl3: TPanel
      Left = 0
      Top = 476
      Width = 201
      Height = 41
      Align = alBottom
      TabOrder = 2
      ExplicitLeft = 8
      ExplicitTop = 240
      ExplicitWidth = 185
      object btnPostPaper: TButton
        Left = 1
        Top = 1
        Width = 199
        Height = 39
        Align = alClient
        Caption = #20132#21367
        Font.Charset = ANSI_CHARSET
        Font.Color = clGreen
        Font.Height = -21
        Font.Name = #23435#20307
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        ExplicitLeft = 48
        ExplicitTop = 8
        ExplicitWidth = 75
        ExplicitHeight = 25
      end
    end
  end
  object pnl2: TPanel
    Left = 204
    Top = 0
    Width = 605
    Height = 517
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 368
    ExplicitTop = 208
    ExplicitWidth = 185
    ExplicitHeight = 41
  end
  object tmr1: TTimer
    Enabled = False
    OnTimer = tmr1Timer
    Left = 56
    Top = 80
  end
end
