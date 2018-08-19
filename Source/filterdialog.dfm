object DlgFilter: TDlgFilter
  Left = 227
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Filter'
  ClientHeight = 371
  ClientWidth = 517
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    517
    371)
  PixelsPerInch = 96
  TextHeight = 14
  object OKBtn: TButton
    Left = 434
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 0
  end
  object CancelBtn: TButton
    Left = 434
    Top = 39
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object GBOutFilter: TGroupBox
    Left = 3
    Top = 0
    Width = 209
    Height = 369
    Caption = '=> Outgoing Packets'
    Color = clSkyBlue
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentBackground = False
    ParentColor = False
    ParentFont = False
    TabOrder = 2
    DesignSize = (
      209
      369)
    object Label1: TLabel
      Left = 16
      Top = 120
      Width = 45
      Height = 13
      Caption = 'Filter ID'#39's'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object RBOutOff: TRadioButton
      Left = 16
      Top = 22
      Width = 169
      Height = 17
      Caption = 'Don'#39't filter'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object RBOutHide: TRadioButton
      Left = 16
      Top = 45
      Width = 169
      Height = 17
      Caption = 'Hide specified packet IDs'
      TabOrder = 1
    end
    object RBOutShow: TRadioButton
      Left = 16
      Top = 68
      Width = 169
      Height = 17
      Caption = 'Show only specified IDs'
      TabOrder = 2
    end
    object RBOutNone: TRadioButton
      Left = 16
      Top = 91
      Width = 169
      Height = 17
      Caption = 'Hide all outgoing'
      TabOrder = 3
    end
    object LBOut: TListBox
      Left = 16
      Top = 139
      Width = 177
      Height = 188
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      MultiSelect = True
      TabOrder = 4
    end
    object CBOut: TComboBox
      Left = 16
      Top = 333
      Width = 137
      Height = 21
      Anchors = [akLeft, akBottom]
      TabOrder = 5
    end
    object BtnOutAdd: TButton
      Left = 167
      Top = 333
      Width = 26
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '+'
      TabOrder = 6
      OnClick = BtnOutAddClick
      ExplicitLeft = 159
      ExplicitTop = 319
    end
    object BtnOutDel: TButton
      Left = 128
      Top = 113
      Width = 65
      Height = 25
      Anchors = [akLeft, akRight]
      Caption = 'remove'
      TabOrder = 7
      OnClick = BtnOutDelClick
    end
  end
  object GBInFilter: TGroupBox
    Left = 218
    Top = 0
    Width = 209
    Height = 369
    Caption = '<= Incomming Packets'
    Color = clMoneyGreen
    ParentBackground = False
    ParentColor = False
    TabOrder = 3
    DesignSize = (
      209
      369)
    object Label2: TLabel
      Left = 16
      Top = 120
      Width = 50
      Height = 14
      Caption = 'Filter ID'#39's'
    end
    object RBInOff: TRadioButton
      Left = 16
      Top = 22
      Width = 169
      Height = 17
      Caption = 'Don'#39't filter'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object RBInHide: TRadioButton
      Left = 16
      Top = 45
      Width = 169
      Height = 17
      Caption = 'Hide specified packet IDs'
      TabOrder = 1
    end
    object RBInShow: TRadioButton
      Left = 16
      Top = 68
      Width = 169
      Height = 17
      Caption = 'Show only specified IDs'
      TabOrder = 2
    end
    object RBInNone: TRadioButton
      Left = 16
      Top = 91
      Width = 169
      Height = 17
      Caption = 'Hide all incomming'
      TabOrder = 3
    end
    object LBIn: TListBox
      Left = 16
      Top = 139
      Width = 177
      Height = 188
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      MultiSelect = True
      TabOrder = 4
    end
    object BtnInAdd: TButton
      Left = 167
      Top = 333
      Width = 26
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '+'
      TabOrder = 5
      OnClick = BtnInAddClick
    end
    object BtnInDel: TButton
      Left = 136
      Top = 113
      Width = 57
      Height = 25
      Anchors = [akLeft, akRight]
      Caption = 'remove'
      TabOrder = 6
      OnClick = BtnInDelClick
    end
    object CBIn: TComboBox
      Left = 16
      Top = 333
      Width = 137
      Height = 21
      Anchors = [akLeft, akBottom]
      TabOrder = 7
    end
  end
  object BtnSaveAs: TButton
    Left = 434
    Top = 331
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Save'
    TabOrder = 4
    OnClick = BtnSaveAsClick
  end
  object BtnLoad: TButton
    Left = 434
    Top = 302
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Load'
    TabOrder = 5
    OnClick = BtnLoadClick
  end
  object FilterSaveDialog: TSaveDialog
    DefaultExt = '.pfl'
    Filter = 'Filter Files|*.pfl|All Files|*.*'
    InitialDir = 'filters'
    Options = [ofHideReadOnly, ofNoChangeDir, ofPathMustExist, ofEnableSizing, ofDontAddToRecent]
    Title = 'Save Filter as ...'
    Left = 456
    Top = 168
  end
  object FilterOpenDialog: TOpenDialog
    DefaultExt = '.pfl'
    Filter = 'Filter Files|*.pfl|All Files|*.*'
    Options = [ofHideReadOnly, ofNoChangeDir, ofFileMustExist, ofEnableSizing]
    Left = 456
    Top = 224
  end
end
