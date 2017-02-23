object fUserList: TfUserList
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #29992#25143#31649#29702
  ClientHeight = 380
  ClientWidth = 564
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
    Width = 564
    Height = 380
    Align = alClient
    Columns = <
      item
        Caption = #29992#25143#21517
        Width = 80
      end
      item
        Caption = #29992#25143#20840#31216
        Width = 120
      end
      item
        Caption = #29992#25143#35828#26126
        Width = 250
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    PopupMenu = pm1
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = actEditUserExecute
  end
  object pm1: TPopupMenu
    Left = 232
    Top = 104
    object NewUser2: TMenuItem
      Action = actNewUser
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object ChangePassword2: TMenuItem
      Action = actChangePassword
    end
    object EditUser1: TMenuItem
      Action = actEditUser
    end
    object DeleteUser2: TMenuItem
      Action = actDeleteUser
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object Refresh2: TMenuItem
      Action = actRefresh
    end
  end
  object actmgr1: TActionManager
    Left = 232
    Top = 56
    StyleName = 'XP Style'
    object actNewUser: TAction
      Caption = #28155#21152#29992#25143
      OnExecute = actNewUserExecute
    end
    object actRefresh: TAction
      Caption = #21047#26032
      OnExecute = actRefreshExecute
    end
    object actChangePassword: TAction
      Caption = #20462#25913#23494#30721
      OnExecute = actChangePasswordExecute
    end
    object actDeleteUser: TAction
      Caption = #21024#38500#29992#25143
      OnExecute = actDeleteUserExecute
    end
    object actEditUser: TAction
      Caption = #32534#36753#29992#25143
      OnExecute = actEditUserExecute
    end
  end
end
