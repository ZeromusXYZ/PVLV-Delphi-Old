object DlgSearch: TDlgSearch
  Left = 227
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Search'
  ClientHeight = 209
  ClientWidth = 370
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Consolas'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object BtnOK: TButton
    Left = 264
    Top = 16
    Width = 98
    Height = 33
    Caption = 'Find Next'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 1
  end
  object BtnCancel: TButton
    Left = 264
    Top = 60
    Width = 98
    Height = 32
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object RGDirection: TRadioGroup
    Left = 8
    Top = 8
    Width = 249
    Height = 41
    Caption = 'Data Direction'
    Columns = 3
    ItemIndex = 0
    Items.Strings = (
      '&Any'
      '&Outgoing'
      '&Incomming')
    TabOrder = 3
    OnClick = RGDirectionClick
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 55
    Width = 249
    Height = 146
    Caption = 'Search For'
    TabOrder = 0
    object Label1: TLabel
      Left = 14
      Top = 19
      Width = 48
      Height = 13
      Caption = 'PacketID'
    end
    object Label2: TLabel
      Left = 14
      Top = 46
      Width = 66
      Height = 13
      Caption = 'Sync number'
    end
    object Label3: TLabel
      Left = 14
      Top = 73
      Width = 96
      Height = 13
      Caption = 'Containing Value'
    end
    object Label4: TLabel
      Left = 47
      Top = 92
      Width = 186
      Height = 13
      Alignment = taRightJustify
      Caption = 'Values between 0 and 0xFFFFFFFF'
      Font.Charset = ANSI_CHARSET
      Font.Color = clSilver
      Font.Height = -11
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
    end
    object EPacketID: TEdit
      Left = 168
      Top = 16
      Width = 65
      Height = 21
      TabOrder = 0
      OnChange = EPacketIDChange
    end
    object ESync: TEdit
      Left = 168
      Top = 43
      Width = 65
      Height = 21
      TabOrder = 1
      OnChange = ESyncChange
    end
    object CBShowMatchesOnly: TCheckBox
      Left = 14
      Top = 118
      Width = 219
      Height = 17
      Caption = 'Use search result as filter'
      Enabled = False
      TabOrder = 3
    end
    object EValue: TEdit
      Left = 168
      Top = 70
      Width = 65
      Height = 21
      TabOrder = 2
      OnChange = EValueChange
    end
  end
end
