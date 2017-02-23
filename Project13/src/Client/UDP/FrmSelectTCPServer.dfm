object fSelectTCPServer: TfSelectTCPServer
  Left = 0
  Top = 0
  Caption = #26381#21153#22120#36873#25321
  ClientHeight = 291
  ClientWidth = 424
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lv1: TListView
    Left = 0
    Top = 41
    Width = 424
    Height = 209
    Align = alClient
    Columns = <
      item
        Caption = #32534#21495
      end
      item
        Caption = #26381#21153#22120'IP'
        Width = 120
      end
      item
        Caption = #31471#21475
        Width = 80
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
  end
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 424
    Height = 41
    Align = alTop
    TabOrder = 1
    object lbl1: TLabel
      Left = 16
      Top = 14
      Width = 312
      Height = 12
      Caption = #23616#22495#32593#20869#25628#32034#21040#22810#20010#26381#21153#22120#65292#35831#25163#21160#36873#25321#20854#20013#19968#20010#26381#21153#22120#12290
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = #23435#20307
      Font.Style = []
      ParentFont = False
    end
  end
  object pnl2: TPanel
    Left = 0
    Top = 250
    Width = 424
    Height = 41
    Align = alBottom
    TabOrder = 2
    DesignSize = (
      424
      41)
    object btn1: TButton
      Left = 258
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = #30830#23450
      ModalResult = 1
      TabOrder = 0
    end
    object btn2: TButton
      Left = 339
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = #21462#28040
      ModalResult = 2
      TabOrder = 1
    end
  end
  object idtcpclnt1: TIdTCPClient
    ConnectTimeout = 0
    IPVersion = Id_IPv4
    Port = 0
    ReadTimeout = -1
    Left = 208
    Top = 152
  end
end
