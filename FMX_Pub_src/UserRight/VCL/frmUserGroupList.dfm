object fUserGroupList: TfUserGroupList
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #32452#20449#24687#31649#29702
  ClientHeight = 311
  ClientWidth = 457
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lv1: TListView
    Left = 0
    Top = 0
    Width = 457
    Height = 311
    Align = alClient
    Columns = <
      item
        Caption = #32452#21517#31216
        Width = 200
      end
      item
        Caption = #32452#35828#26126
        Width = 250
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    PopupMenu = pm1
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = actEditGroupExecute
  end
  object pm1: TPopupMenu
    Left = 216
    Top = 184
    object NewGroup1: TMenuItem
      Action = actNewGroup
    end
    object DeleteGroup2: TMenuItem
      Caption = '-'
    end
    object EditGroup1: TMenuItem
      Action = actEditGroup
    end
    object DeleteGroup3: TMenuItem
      Action = actDelGroup
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Refresh2: TMenuItem
      Action = actRefresh
    end
  end
  object actmgr1: TActionManager
    Left = 216
    Top = 248
    StyleName = 'XP Style'
    object actNewGroup: TAction
      Caption = #26032#22686#20998#32452
      OnExecute = actNewGroupExecute
    end
    object actRefresh: TAction
      Caption = #21047#26032
      OnExecute = actRefreshExecute
    end
    object actDelGroup: TAction
      Caption = #21024#38500#20998#32452
      OnExecute = actDelGroupExecute
    end
    object actEditGroup: TAction
      Caption = #32534#36753#20998#32452
      OnExecute = actEditGroupExecute
    end
  end
end
