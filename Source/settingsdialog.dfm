object DlgSettings: TDlgSettings
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Settings'
  ClientHeight = 207
  ClientWidth = 410
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 106
  TextHeight = 14
  object Label1: TLabel
    Left = 8
    Top = 19
    Width = 82
    Height = 14
    Caption = 'Grid Font Type'
  end
  object LRestartWarning: TLabel
    Left = 102
    Top = 179
    Width = 300
    Height = 14
    Caption = 'some changed setting requires a restart to take effect'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Visible = False
  end
  object BtnClose: TButton
    Left = 8
    Top = 175
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 0
    OnClick = BtnCloseClick
  end
  object CBUseVLC: TCheckBox
    Left = 8
    Top = 52
    Width = 393
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Use libVLC to enable video linking'
    TabOrder = 1
    OnClick = CBUseVLCClick
  end
  object CBGridFont: TComboBox
    Left = 288
    Top = 16
    Width = 113
    Height = 22
    Style = csDropDownList
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemIndex = 0
    ParentFont = False
    TabOrder = 2
    Text = 'Normal Font'
    OnChange = CBGridFontChange
    Items.Strings = (
      'Normal Font'
      'Small Font')
  end
end
