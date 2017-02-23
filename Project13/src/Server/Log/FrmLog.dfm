object fLog: TfLog
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = #35774#22791#36890#35759#35760#24405
  ClientHeight = 384
  ClientWidth = 560
  Color = clBtnFace
  Constraints.MinHeight = 100
  Constraints.MinWidth = 476
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object spl1: TSplitter
    Left = 443
    Top = 0
    Height = 384
    Align = alRight
    ExplicitLeft = 280
    ExplicitTop = 152
    ExplicitHeight = 100
  end
  object redt1: TRichEdit
    Left = 0
    Top = 0
    Width = 443
    Height = 384
    Align = alClient
    Font.Charset = GB2312_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
    Zoom = 100
  end
  object pnl2: TPanel
    Left = 446
    Top = 0
    Width = 114
    Height = 384
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      114
      384)
    object chkRev: TCheckBox
      Left = 6
      Top = 34
      Width = 89
      Height = 17
      Caption = #35760#24405#25509#25910#21629#20196
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object chkSend: TCheckBox
      Left = 6
      Top = 11
      Width = 97
      Height = 17
      Caption = #35760#24405#21457#36865#21629#20196
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object btnClear: TButton
      Left = 6
      Top = 320
      Width = 99
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = #28165#31354#35760#24405
      TabOrder = 0
      OnClick = btnClearClick
    end
    object chkHex: TCheckBox
      Left = 6
      Top = 57
      Width = 105
      Height = 17
      Caption = #21629#20196'16'#36827#21046#26174#31034
      Checked = True
      State = cbChecked
      TabOrder = 3
      Visible = False
    end
    object btn1: TBitBtn
      Left = 6
      Top = 348
      Width = 99
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = #36864#20986
      TabOrder = 4
      OnClick = btn1Click
    end
  end
end
