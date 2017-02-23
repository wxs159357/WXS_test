object fAnalysisMap: TfAnalysisMap
  Left = 0
  Top = 0
  BorderStyle = bsNone
  ClientHeight = 241
  ClientWidth = 248
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Visible = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object tbcntrl1: TTabControl
    Left = 0
    Top = 0
    Width = 248
    Height = 221
    Align = alClient
    TabOrder = 0
    Tabs.Strings = (
      '1'
      '2'
      '3'
      '4')
    TabIndex = 0
    OnChange = tbcntrl1Change
    object imgMap: TImage
      Left = 4
      Top = 24
      Width = 240
      Height = 193
      Align = alClient
      ExplicitLeft = 152
      ExplicitTop = 152
      ExplicitWidth = 105
      ExplicitHeight = 105
    end
  end
  object pnl1: TPanel
    Left = 0
    Top = 221
    Width = 248
    Height = 20
    Align = alBottom
    TabOrder = 1
    object stsbrMain: TStatusBar
      Left = 1
      Top = 1
      Width = 225
      Height = 18
      Align = alClient
      Panels = <
        item
          Width = 80
        end
        item
          Width = 60
        end
        item
          Width = 50
        end>
    end
    object btn1: TButton
      Left = 226
      Top = 1
      Width = 21
      Height = 18
      Align = alRight
      Caption = '...'
      TabOrder = 1
      OnClick = btn1Click
    end
  end
end
