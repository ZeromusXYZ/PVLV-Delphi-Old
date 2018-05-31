object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Packet Viewer Log Viewer'
  ClientHeight = 561
  ClientWidth = 1084
  Color = clBtnFace
  DefaultMonitor = dmDesktop
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Consolas'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    1084
    561)
  PixelsPerInch = 96
  TextHeight = 15
  object LInfo: TLabel
    Left = 456
    Top = 20
    Width = 28
    Height = 15
    Caption = 'Info'
  end
  object BtnLoadFile: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Load File'
    TabOrder = 0
    OnClick = BtnLoadFileClick
  end
  object LBPackets: TListBox
    Left = 8
    Top = 39
    Width = 435
    Height = 514
    Style = lbOwnerDrawFixed
    Anchors = [akLeft, akTop, akBottom]
    ExtendedSelect = False
    ItemHeight = 15
    PopupMenu = PMPacketList
    TabOrder = 1
    OnClick = LBPacketsClick
    OnDrawItem = LBPacketsDrawItem
  end
  object MInfo: TMemo
    Left = 456
    Top = 369
    Width = 620
    Height = 184
    Anchors = [akLeft, akRight, akBottom]
    Lines.Strings = (
      'Press load file to start')
    ReadOnly = True
    TabOrder = 2
  end
  object CBAppend: TCheckBox
    Left = 96
    Top = 16
    Width = 121
    Height = 17
    Caption = 'Append to list'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object SG: TStringGrid
    Left = 456
    Top = 41
    Width = 620
    Height = 322
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 2
    RowCount = 2
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing]
    TabOrder = 4
    RowHeights = (
      24
      24)
  end
  object OpenDialogLogFiles: TOpenDialog
    DefaultExt = '*.log'
    Filter = 'Log Files|*.log|All Files|*.*'
    Options = [ofReadOnly, ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'Open Log file'
    Left = 48
    Top = 72
  end
  object PMPacketList: TPopupMenu
    OnPopup = PMPacketListPopup
    Left = 136
    Top = 72
    object PMPacketListOpenFile: TMenuItem
      Caption = 'Open File'
      OnClick = PMPacketListOpenFileClick
    end
    object PMPacketListShow: TMenuItem
      Caption = 'Show'
    end
    object PMPacketListN1: TMenuItem
      Caption = '-'
    end
    object PMPacketListOnlyShow: TMenuItem
      Caption = 'Only Show this'
      OnClick = PMPacketListOnlyShowClick
    end
    object PMPacketListHideThis: TMenuItem
      Caption = 'Hide this type'
      OnClick = PMPacketListHideThisClick
    end
    object PMPacketListN2: TMenuItem
      Caption = '-'
    end
    object PMPacketListOnlyOut: TMenuItem
      Caption = 'Show Only Outgoing'
      OnClick = PMPacketListOnlyOutClick
    end
    object PMPacketListOnlyIn: TMenuItem
      Caption = 'Show Only Incomming'
      OnClick = PMPacketListOnlyInClick
    end
    object PMPacketListN3: TMenuItem
      Caption = '-'
    end
    object PMPacketListReset: TMenuItem
      Caption = 'Reset all filters'
      OnClick = PMPacketListResetClick
    end
  end
end
