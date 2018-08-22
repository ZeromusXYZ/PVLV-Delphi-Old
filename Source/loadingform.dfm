object FormLoading: TFormLoading
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Loading ...'
  ClientHeight = 33
  ClientWidth = 503
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PB: TProgressBar
    Left = 0
    Top = 0
    Width = 503
    Height = 33
    Align = alClient
    TabOrder = 0
  end
end
