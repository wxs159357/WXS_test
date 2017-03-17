object fVectorMapInfo: TfVectorMapInfo
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #21521#37327#22270
  ClientHeight = 472
  ClientWidth = 744
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object pnl2: TPanel
    Left = 0
    Top = 0
    Width = 744
    Height = 472
    Align = alClient
    TabOrder = 0
    ExplicitHeight = 428
    object spltr1: TSplitter
      Left = 540
      Top = 1
      Height = 470
      Align = alRight
      ExplicitLeft = 408
      ExplicitTop = 208
      ExplicitHeight = 100
    end
    object imgMap: TImage
      Left = 1
      Top = 1
      Width = 539
      Height = 470
      Align = alClient
      OnMouseDown = imgMapMouseDown
      OnMouseMove = imgMapMouseMove
      ExplicitTop = -4
      ExplicitHeight = 426
    end
    object pnl3: TPanel
      Left = 543
      Top = 1
      Width = 200
      Height = 470
      Align = alRight
      TabOrder = 0
      ExplicitHeight = 426
      object grp1: TGroupBox
        Left = 1
        Top = 1
        Width = 198
        Height = 468
        Align = alClient
        Caption = #21521#37327#20449#24687
        TabOrder = 0
        ExplicitHeight = 424
        object lblColor: TLabel
          Left = 28
          Top = 60
          Width = 24
          Height = 13
          Caption = #39068#33394
        end
        object lblAngle: TLabel
          Left = 28
          Top = 113
          Width = 24
          Height = 13
          Caption = #35282#24230
        end
        object lbl1: TLabel
          Left = 28
          Top = 140
          Width = 24
          Height = 13
          Caption = #25968#20540
        end
        object lblType: TLabel
          Left = 28
          Top = 33
          Width = 24
          Height = 13
          Caption = #21517#31216
        end
        object lbl2: TLabel
          Left = 28
          Top = 86
          Width = 24
          Height = 13
          Caption = #31867#21035
        end
        object cbbName: TComboBox
          Left = 82
          Top = 30
          Width = 95
          Height = 21
          ItemIndex = 0
          TabOrder = 0
          Text = 'Ua'
          OnChange = cbbNameChange
          Items.Strings = (
            'Ua'
            'Ia'
            'Ub'
            'Ib'
            'Uc'
            'Ic'
            'Uab'
            'Ucb'
            'Uac'
            'Ubc'
            'Uba'
            'Uca ')
        end
        object btnAdd: TButton
          Left = 21
          Top = 187
          Width = 75
          Height = 25
          Caption = #28155#21152
          TabOrder = 1
          OnClick = btnAddClick
        end
        object edtValue: TEdit
          Left = 82
          Top = 137
          Width = 95
          Height = 21
          TabOrder = 2
          Text = '220'
          OnChange = edtValueChange
        end
        object btnDel: TButton
          Left = 102
          Top = 187
          Width = 75
          Height = 25
          Caption = #21024#38500
          TabOrder = 3
          OnClick = btnDelClick
        end
        object mmoShow: TMemo
          Left = 2
          Top = 293
          Width = 194
          Height = 173
          Align = alBottom
          Lines.Strings = (
            'mmoShow')
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 4
          Visible = False
          ExplicitTop = 249
        end
        object btnModify: TButton
          Left = 92
          Top = 514
          Width = 62
          Height = 25
          Caption = #20462#25913
          TabOrder = 5
          Visible = False
        end
        object pnlClr: TPanel
          Left = 82
          Top = 56
          Width = 95
          Height = 21
          Color = clYellow
          ParentBackground = False
          TabOrder = 6
          OnClick = pnlClrClick
        end
        object cbbAngle: TComboBox
          Left = 82
          Top = 110
          Width = 95
          Height = 21
          ItemIndex = 0
          TabOrder = 7
          Text = '0'
          OnChange = edtValueChange
          Items.Strings = (
            '0'
            '30'
            '60'
            '90'
            '120'
            '150'
            '180'
            '210'
            '240'
            '270'
            '300'
            '330')
        end
        object cbbType: TComboBox
          Left = 82
          Top = 84
          Width = 95
          Height = 21
          TabOrder = 8
          Text = 'cbbVType'
          OnChange = edtValueChange
        end
        object btnSave: TButton
          Left = 64
          Top = 160
          Width = 75
          Height = 25
          Caption = #20445#23384
          TabOrder = 9
          Visible = False
          OnClick = btnSaveClick
        end
      end
    end
  end
  object dlgColor: TColorDialog
    Color = clYellow
    Left = 152
    Top = 40
  end
end
