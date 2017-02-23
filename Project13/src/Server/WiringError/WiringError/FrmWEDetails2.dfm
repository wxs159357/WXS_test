object fWEDetails2: TfWEDetails2
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'fWEDetails2'
  ClientHeight = 619
  ClientWidth = 604
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
  PixelsPerInch = 96
  TextHeight = 13
  object tbcMap: TTabControl
    Left = 0
    Top = 0
    Width = 604
    Height = 619
    Align = alClient
    TabHeight = 25
    TabOrder = 0
    Tabs.Strings = (
      #25509#32447#22270
      #21521#37327#22270
      #21151#29575#34920#36798#24335
      #26356#27491#31995#25968#21450#20998#26512
      #36864#34917#30005#37327)
    TabIndex = 0
    TabWidth = 100
    OnChange = tbcMapChange
    object scrlbxMap: TScrollBox
      Left = 4
      Top = 31
      Width = 596
      Height = 584
      Align = alClient
      BorderStyle = bsNone
      TabOrder = 0
      object imgMap: TImage
        Left = 0
        Top = 0
        Width = 105
        Height = 105
        Stretch = True
        OnMouseDown = imgMapMouseDown
        OnMouseMove = imgMapMouseMove
        OnMouseUp = imgMapMouseUp
      end
      object pnl1: TPanel
        Left = 0
        Top = 0
        Width = 596
        Height = 584
        Align = alClient
        Color = clWindow
        ParentBackground = False
        TabOrder = 0
        object lbl1: TLabel
          Left = 40
          Top = 40
          Width = 48
          Height = 13
          Caption = #25220#35265#30005#37327
        end
        object lbl2: TLabel
          Left = 237
          Top = 40
          Width = 21
          Height = 13
          Caption = 'kWh'
        end
        object lbl3: TLabel
          Left = 40
          Top = 76
          Width = 167
          Height = 13
          Caption = #36864#34917#30005#37327' = '#65288'K - 1'#65289#215' '#25220#35265#30005#37327
        end
        object lbl4: TLabel
          Left = 91
          Top = 136
          Width = 8
          Height = 13
          Caption = '='
        end
        object lbl5: TLabel
          Left = 237
          Top = 136
          Width = 21
          Height = 13
          Caption = 'kWh'
        end
        object lbl6: TLabel
          Left = 91
          Top = 106
          Width = 8
          Height = 13
          Caption = '='
        end
        object edtKwh: TEdit
          Left = 105
          Top = 136
          Width = 121
          Height = 21
          BevelInner = bvNone
          BevelOuter = bvNone
          BorderStyle = bsNone
          ImeName = #20013#25991' ('#31616#20307') - '#25628#29399#25340#38899#36755#20837#27861
          ReadOnly = True
          TabOrder = 1
        end
        object btnCalcKwh: TButton
          Left = 288
          Top = 35
          Width = 75
          Height = 25
          Caption = #37325#26032#35745#31639
          TabOrder = 0
          OnClick = btnCalcKwhClick
        end
        object edtSeeKwh: TEdit
          Left = 94
          Top = 37
          Width = 121
          Height = 21
          TabOrder = 2
          Text = '1000'
        end
      end
    end
  end
end
