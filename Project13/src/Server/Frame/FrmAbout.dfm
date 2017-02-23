object fAbout: TfAbout
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #20851#20110
  ClientHeight = 274
  ClientWidth = 420
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object img1: TImage
    Left = 0
    Top = 0
    Width = 420
    Height = 179
    Align = alClient
    Proportional = True
    ExplicitHeight = 209
  end
  object lblCopyright: TLabel
    Left = 8
    Top = 160
    Width = 217
    Height = 13
    AutoSize = False
    Caption = #29256#26435#25152#26377
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = #23435#20307
    Font.Style = []
    ParentFont = False
    OnClick = img1Click
  end
  object lblVersion: TLabel
    Left = 251
    Top = 135
    Width = 148
    Height = 13
    AutoSize = False
    Caption = #29256#26412#20449#24687
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = #23435#20307
    Font.Style = []
    ParentFont = False
    OnClick = img1Click
  end
  object lblweb: TLabel
    Left = 251
    Top = 160
    Width = 161
    Height = 13
    AutoSize = False
    Caption = 'web'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -12
    Font.Name = #23435#20307
    Font.Style = []
    ParentFont = False
    OnClick = lblwebClick
    OnMouseMove = lblwebMouseMove
    OnMouseLeave = lblwebMouseLeave
  end
  object lbl1: TLabel
    Left = 8
    Top = 135
    Width = 57
    Height = 13
    AutoSize = False
    Caption = #29256#26435#25152#26377
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = #23435#20307
    Font.Style = []
    ParentFont = False
    OnClick = img1Click
  end
  object mmoVersion: TMemo
    Left = 0
    Top = 179
    Width = 420
    Height = 61
    Align = alBottom
    Lines.Strings = (
      '       '#26412#36719#20214#30340#19968#20999#33879#20316#26435#12289#21830#26631#26435#12289#19987#21033#26435#12289#21830#19994#31192#23494#31561#30693#35782#20135#26435#65292#20197#21450
      #19982#26412#36719#20214#30456#20851#30340#25152#26377#20449#24687#20869#23481#22343#21463#20013#21326#20154#27665#20849#21644#22269#27861#24459#27861#35268#21644#30456#24212#30340#22269#38469
      #26465#32422#20445#25252#65292#25105#26041#20139#26377#19978#36848#30693#35782#20135#26435#12290
      ''
      '       '#26410#32463#25105#26041#20070#38754#21516#24847#65292#24744#19981#24471#20026#20219#20309#21830#19994#25110#38750#21830#19994#30446#30340#33258#34892#25110#35768#21487#20219#20309
      #31532#19977#26041#23454#26045#12289#21033#29992#12289#36716#35753#19978#36848#30693#35782#20135#26435#65292#25105#26041#20445#30041#36861#31350#19978#36848#34892#20026#27861#24459#36131#20219
      #30340#26435#21033#12290)
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object pnl1: TPanel
    Left = 0
    Top = 240
    Width = 420
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btn1: TButton
      Left = 340
      Top = 5
      Width = 75
      Height = 25
      Caption = #30830#23450
      ModalResult = 1
      TabOrder = 0
    end
  end
end
