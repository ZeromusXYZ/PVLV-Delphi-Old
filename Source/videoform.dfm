object VideoLink: TVideoLink
  Left = 0
  Top = 0
  Caption = 'VideoLink'
  ClientHeight = 370
  ClientWidth = 510
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    510
    370)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 315
    Width = 22
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Time'
    OnClick = Label1Click
  end
  object BtnPlay: TButton
    Left = 176
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Play ||>'
    TabOrder = 0
    OnClick = BtnPlayClick
  end
  object BtnStop: TButton
    Left = 347
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Stop [ ]'
    TabOrder = 1
    Visible = False
    OnClick = BtnStopClick
  end
  object BtnOpen: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Open Video'
    TabOrder = 2
    OnClick = BtnOpenClick
  end
  object VideoPanel: TPanel
    Left = 16
    Top = 39
    Width = 480
    Height = 270
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Video'
    TabOrder = 3
  end
  object BtnClose: TButton
    Left = 89
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 4
    OnClick = BtnCloseClick
  end
  object BtnPause: TButton
    Left = 257
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Pause ||'
    TabOrder = 5
    OnClick = BtnPauseClick
  end
  object TrackBar1: TTrackBar
    Left = 8
    Top = 332
    Width = 488
    Height = 30
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 6
    ThumbLength = 10
    TickMarks = tmBoth
    OnChange = TrackBar1Change
  end
  object BtnSetSync: TButton
    Left = 428
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Set Sync'
    TabOrder = 7
    OnClick = BtnSetSyncClick
  end
  object CBFollow: TCheckBox
    Left = 328
    Top = 315
    Width = 168
    Height = 17
    Alignment = taLeftJustify
    Anchors = [akRight, akBottom]
    Caption = 'Packetview follows playback'
    TabOrder = 8
  end
  object OpenVideoDialog: TOpenDialog
    Filter = 'Video files|*.avi;*.mp4;*.mpg;*.mpeg;*.ts|All files|*.*'
    Options = [ofHideReadOnly, ofNoChangeDir, ofFileMustExist, ofEnableSizing, ofDontAddToRecent]
    Left = 48
    Top = 104
  end
  object PosUpdateTimer: TTimer
    Interval = 500
    OnTimer = PosUpdateTimerTimer
    Left = 144
    Top = 104
  end
  object ApplicationEventsVideo: TApplicationEvents
    OnIdle = ApplicationEventsVideoIdle
    Left = 240
    Top = 103
  end
end
