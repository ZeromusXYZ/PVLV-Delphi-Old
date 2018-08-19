unit VideoForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls,
  datalookups;

type
  TVideoLink = class(TForm)
    BtnPlay: TButton;
    BtnStop: TButton;
    BtnOpen: TButton;
    VideoPanel: TPanel;
    OpenVideoDialog: TOpenDialog;
    BtnClose: TButton;
    BtnPause: TButton;
    Label1: TLabel;
    PosUpdateTimer: TTimer;
    TrackBar1: TTrackBar;
    BtnSetSync: TButton;
    CBFollow: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure BtnPlayClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
    procedure BtnOpenClick(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
    procedure BtnPauseClick(Sender: TObject);
    procedure PosUpdateTimerTimer(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnSetSyncClick(Sender: TObject);
  private
    { Private declarations }
    fCurrentSize : Int64 ;
    fCurrentPos : Int64 ;
  public
    { Public declarations }
    IsAvailable : Boolean ;
    LinkOffset : Int64 ;
    LinkFile : String ;
    LinkSourceFile : String ;

    Function TryOpenVideoLink(FN : String):Boolean;
    Function SaveVideoLink(FN : String):Boolean;
    Function OpenVideo(VideoFileName : String):Boolean;
    Procedure ShowVideoForm ;
    Procedure MoveToTimePacketOffset(TimeOffset : TDateTime);
    Procedure MoveToOffset(TimeOffset : Int64);

    Property CurrentSize : Int64 read fCurrentSize ;
    Property CurrentPos : Int64 read fCurrentPos ;

  end;

  plibvlc_instance_t        = type Pointer;
  plibvlc_media_player_t    = type Pointer;
  plibvlc_media_t           = type Pointer;
  libvlc_time_t_ptr = ^libvlc_time_t;
  libvlc_time_t = Int64;
  libvlc_state_t = (
      libvlc_NothingSpecial,
      libvlc_Opening,
      libvlc_Buffering,
      libvlc_Playing,
      libvlc_Paused,
      libvlc_Stopped,
      libvlc_Ended,
      libvlc_Error
      ) ;

var
  VideoLink: TVideoLink;

implementation

Uses Main, Registry , settingsdialog;

{$R *.dfm}

var
  libvlc_media_new_path              : function(p_instance : Plibvlc_instance_t; path : PAnsiChar) : Plibvlc_media_t; cdecl;
  libvlc_media_new_location          : function(p_instance : plibvlc_instance_t; psz_mrl : PAnsiChar) : Plibvlc_media_t; cdecl;
  libvlc_media_player_new_from_media : function(p_media : Plibvlc_media_t) : Plibvlc_media_player_t; cdecl;
  libvlc_media_player_set_hwnd       : procedure(p_media_player : Plibvlc_media_player_t; drawable : Pointer); cdecl;
  libvlc_media_player_play           : procedure(p_media_player : Plibvlc_media_player_t); cdecl;
  libvlc_media_player_stop           : procedure(p_media_player : Plibvlc_media_player_t); cdecl;
  libvlc_media_player_pause          : procedure(p_media_player : Plibvlc_media_player_t); cdecl;
  libvlc_media_player_set_pause      : procedure(p_media_player : Plibvlc_media_player_t; do_pause:integer); cdecl;
  libvlc_media_player_release        : procedure(p_media_player : Plibvlc_media_player_t); cdecl;
  libvlc_media_player_is_playing     : function(p_media_player : Plibvlc_media_player_t) : Integer; cdecl;
  libvlc_media_release               : procedure(p_media : Plibvlc_media_t); cdecl;
  libvlc_new                         : function(argc : Integer; argv : PAnsiChar) : Plibvlc_instance_t; cdecl;
  libvlc_release                     : procedure(p_instance : Plibvlc_instance_t); cdecl;
  libvlc_media_player_get_position   : function(p_media : Plibvlc_media_t) : double ; cdecl;
  libvlc_media_player_get_time       : function(p_media : Plibvlc_media_t) : libvlc_time_t ; cdecl;
  libvlc_media_player_get_length     : function(p_media : Plibvlc_media_t) : libvlc_time_t ; cdecl;
  libvlc_media_player_set_time       : procedure(p_media : Plibvlc_media_t; i_time:libvlc_time_t) ; cdecl;
  libvlc_media_player_next_frame     : procedure(p_instance : Plibvlc_instance_t); cdecl;
  libvlc_media_player_get_state      : function(p_media : Plibvlc_media_t) : libvlc_state_t ; cdecl;

  vlcLib: integer;
  vlcInstance: plibvlc_instance_t;
  vlcMedia: plibvlc_media_t;
  vlcMediaPlayer: plibvlc_media_player_t;

procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings) ;
begin
   ListOfStrings.Clear;
   ListOfStrings.Delimiter       := Delimiter;
   ListOfStrings.StrictDelimiter := True;
   ListOfStrings.DelimitedText   := Str;
end;

// -----------------------------------------------------------------------------
// Read registry to get VLC installation path
// -----------------------------------------------------------------------------
function GetVLCLibPath: String;
var
  // Handle: HKEY;
  // RegType: Integer;
  // DataSize: Cardinal;
  Key: PWideChar;
  // Res : Integer ;
  Reg : TRegistry ;
begin
  Result := '';
  Key := 'Software\VideoLAN\VLC';
  Reg := TRegistry.Create(STANDARD_RIGHTS_READ or
                          KEY_QUERY_VALUE
                          or KEY_WOW64_32KEY
                          );
  Try
    Reg.RootKey := HKEY_LOCAL_MACHINE ;
    If Reg.OpenKey(Key,False) Then
    Begin
      Result := Reg.ReadString('InstallDir');
    End Else
    Begin
      // ShowMessage('Can''t open '+Key);
      Result := '' ;
    End;
  Finally
    FreeAndNil(Reg);
  End;
{
  If Result = '' then
  Begin
    // Little fallback in case registry fails
    if FileExists('C:\Program Files\VideoLAN\VLC\libvlccore.dll') then
      Result := 'C:\Program Files\VideoLAN\VLC' ;
  End;
}
end;

// -----------------------------------------------------------------------------
// Load libvlc library into memory
// -----------------------------------------------------------------------------
function LoadVLCLibrary(APath: string): integer;
begin
  {Result := }LoadLibrary(PWideChar(APath + '\libvlccore.dll'));
  Result := LoadLibrary(PWideChar(APath + '\libvlc.dll'));
end;

// -----------------------------------------------------------------------------
function GetAProcAddress(handle: integer; var addr: Pointer; procName: string; failedList: TStringList): integer;
begin
  addr := GetProcAddress(handle, PWideChar(procName));
  if Assigned(addr) then Result := 0
  else begin
    if Assigned(failedList) then failedList.Add(procName);
    Result := -1;
  end;
end;

// -----------------------------------------------------------------------------
// Get address of libvlc functions
// -----------------------------------------------------------------------------
function LoadVLCFunctions(vlcHandle: integer; failedList: TStringList): Boolean;
begin
  GetAProcAddress(vlcHandle, @libvlc_new, 'libvlc_new', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_new_location, 'libvlc_media_new_location', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_player_new_from_media, 'libvlc_media_player_new_from_media', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_release, 'libvlc_media_release', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_player_set_hwnd, 'libvlc_media_player_set_hwnd', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_player_play, 'libvlc_media_player_play', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_player_stop, 'libvlc_media_player_stop', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_player_pause, 'libvlc_media_player_pause', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_player_set_pause, 'libvlc_media_player_set_pause', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_player_release, 'libvlc_media_player_release', failedList);
  GetAProcAddress(vlcHandle, @libvlc_release, 'libvlc_release', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_player_is_playing, 'libvlc_media_player_is_playing', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_new_path, 'libvlc_media_new_path', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_player_get_position, 'libvlc_media_player_get_position', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_player_get_time, 'libvlc_media_player_get_time', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_player_get_length, 'libvlc_media_player_get_length', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_player_set_time, 'libvlc_media_player_set_time', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_player_next_frame, 'libvlc_media_player_next_frame', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_player_get_state, 'libvlc_media_player_get_state', failedList);
  // if all functions loaded, result is an empty list, otherwise result is a list of functions failed
  Result := failedList.Count = 0;
end;


// -----------------------------------------------------------------------------
procedure TVideoLink.BtnPlayClick(Sender: TObject);
begin
  if not Assigned(vlcMediaPlayer) then
    BtnOpen.Click ;

  if not Assigned(vlcMediaPlayer) then Exit ;
  // play media
  libvlc_media_player_play(vlcMediaPlayer);
end;

procedure TVideoLink.BtnSetSyncClick(Sender: TObject);
begin
  LinkOffset := Round(MainForm.CurrentDateTimeOffset * 24 * 60 * 60 * 1000) - CurrentPos ;
end;

procedure TVideoLink.BtnStopClick(Sender: TObject);
VAR
  C : Integer ;
begin
  if not Assigned(vlcMediaPlayer) then begin
    Showmessage('Not playing');
    Exit;
  end;
  // stop vlc media player
  libvlc_media_player_stop(vlcMediaPlayer);
  // and wait until it completely stops
  C := 0 ;
  while (libvlc_media_player_is_playing(vlcMediaPlayer) = 1)and(C < 100) do
  begin
    Sleep(100);
    C := C + 1 ; // timeout after 10 seconds
  end;
end;

procedure TVideoLink.BtnOpenClick(Sender: TObject);
VAR
  FN : AnsiString ;
  PFN : PAnsiChar ;
begin
  if Assigned(vlcMediaPlayer) Then BtnClose.Click ;

  If Not OpenVideoDialog.Execute() Then Exit ;


  FN := AnsiString(OpenVideoDialog.FileName);
  PFN := Pointer(FN);

  // create new vlc instance
  vlcInstance := libvlc_new(0, nil);
  // create new vlc media from file
  vlcMedia := libvlc_media_new_path(vlcInstance, PFN);

  // if you want to play from network, use libvlc_media_new_location instead
  // vlcMedia := libvlc_media_new_location(vlcInstance, 'udp://@225.2.1.27:5127');

  // create new vlc media player
  vlcMediaPlayer := libvlc_media_player_new_from_media(vlcMedia);

  // now no need the vlc media, free it
  libvlc_media_release(vlcMedia);

  // play video in a TPanel, if not call this routine, vlc media will open a new window
  libvlc_media_player_set_hwnd(vlcMediaPlayer, Pointer(VideoPanel.Handle));

  // move to start
  libvlc_media_player_play(vlcMediaPlayer);
  libvlc_media_player_pause(vlcMediaPlayer);
  libvlc_media_player_set_time(vlcMediaPlayer, 1);
  libvlc_media_player_next_frame(vlcMediaPlayer);
  // BtnPlay.Click;

  LinkFile := OpenVideoDialog.FileName ;
end;

procedure TVideoLink.BtnCloseClick(Sender: TObject);
begin
  If Assigned(vlcMediaPlayer) Then
  Begin
    // release vlc media player
    libvlc_media_player_release(vlcMediaPlayer);
    vlcMediaPlayer := nil;

    // release vlc instance
    libvlc_release(vlcInstance);
  End;
  LinkFile := '' ;
  LinkOffset := 0 ;
  Close ;
end;

procedure TVideoLink.BtnPauseClick(Sender: TObject);
begin
  if not Assigned(vlcMediaPlayer) then begin
    Showmessage('Not playing');
    Exit;
  end;
  // pause vlc media player

  libvlc_media_player_pause(vlcMediaPlayer);
end;

procedure TVideoLink.FormCreate(Sender: TObject);
var
  sL: TStringList;
begin
  IsAvailable := False ;
  fCurrentPos := -1 ;
  fCurrentSize := 0 ;
  if Not DlgSettings.UseLibVLC then Exit ;

  // load vlc library
  vlclib := LoadVLCLibrary(GetVLCLibPath());
  if vlclib = 0 then begin
    // Showmessage('Load vlc library failed');
    Exit;
  end;
  // sL will contains list of functions fail to load
  sL := TStringList.Create;
  if not LoadVLCFunctions(vlclib, sL) then begin
    Showmessage('VLC: Some functions failed to load : ' + #13#10 + sL.Text);
    FreeLibrary(vlclib);
    sL.Free;
    Exit;
  end;
  sL.Free;
  IsAvailable := True ;
  MainForm.MMVideo.Enabled := True ;
end;


procedure TVideoLink.FormDestroy(Sender: TObject);
begin
  BtnClose.Click ;
  // unload vlc library
  FreeLibrary(vlclib);
end;

procedure TVideoLink.Label1Click(Sender: TObject);
begin
  PosUpdateTimerTimer(nil);
end;

procedure TVideoLink.PosUpdateTimerTimer(Sender: TObject);
VAR
  Pos : Double ;
  State : libvlc_state_t ;
  T : String ;
begin
  T := 'Media Not Loaded' ;
  If Assigned(vlcMediaPlayer) Then
  Begin
    State := libvlc_media_player_get_state(vlcMediaPlayer);
    Pos := libvlc_media_player_get_position(vlcMediaPlayer);
    If (Pos < 0) Then
    Begin
      T := 'Not started' ;
    End Else
    Begin
      fCurrentPos := libvlc_media_player_get_time(vlcMediaPlayer);
      fCurrentSize := libvlc_media_player_get_length(vlcMediaPlayer);
      //Label1.Caption := 'Pos: '+ FloatToStr(Round(Pos * 100.0))+'%' ;
      T := 'Pos: '+ FloatToStr(Round(Pos * 100.0))+'% - ' + IntToStr(fCurrentPos)+' / ' + IntToStr(fCurrentSize) ;

      TrackBar1.Tag := 1 ; // using this as update flag
      TrackBar1.Frequency := 15 ;
      TrackBar1.Min := 0 ;
      TrackBar1.Max := fCurrentSize div 1000 ;
      TrackBar1.Position := fCurrentPos div 1000 ;
      TrackBar1.Tag := 0 ; // using this as update flag

      If CBFollow.Checked Then
        MainForm.MoveToOffset(CurrentPos+LinkOffset);

    End;
    case State of
      libvlc_NothingSpecial: ;
      libvlc_Opening: T := T + ' - Opening' ;
      libvlc_Buffering: T := T + ' - Buffering'  ;
      libvlc_Playing: T := T + ' - Playing'  ;
      libvlc_Paused: T := T + ' - Paused'  ;
      libvlc_Stopped: T := T + ' - Stopped'  ;
      libvlc_Ended: T := T + ' - Ended'  ;
      libvlc_Error: T := T + ' - ERROR'  ;
    end;
  End;
  Label1.Caption := T;
end;

procedure TVideoLink.TrackBar1Change(Sender: TObject);
begin
  If Assigned(vlcMediaPlayer) and (TrackBar1.Tag = 0) Then
  Begin
    libvlc_media_player_set_time(vlcMediaPlayer,TrackBar1.Position * 1000);
  End;
end;

Function TVideoLink.TryOpenVideoLink(FN : String):Boolean;
VAR
  SL, Line : TStringList ;
  I : Integer ;
Begin
  Result := False ;
  LinkOffset := 0 ;
  LinkFile := '' ;
  LinkSourceFile := FN ;

  If (not VideoLink.IsAvailable) or (Not FileExists(FN)) Then Exit ;

  // Clear our stuff
  Try
    SL := TStringList.Create ;
    Line := TStringList.Create ;
    If FileExists(FN) Then
      SL.LoadFromFile(FN)
    Else
      SL.Clear ;

    For I := 0 To SL.Count-1 Do
    Begin
      // loop all the lines
      Split(';',SL[I],Line);
      If (Line.Count >= 2) Then
      Begin
        If (LowerCase(Line[0]) = 'video') Then LinkFile := Line[1];
        If (LowerCase(Line[0]) = 'offset') Then
        Begin
          If Not TryStrToInt64(Line[1],LinkOffset) Then LinkOffset := 0 ;
        End;
      End;
    End;

  Finally
    FreeAndNil(SL);
    FreeAndNil(Line);
  End;

  // Try added path first
  If FileExists(ExtractFilePath(FN)+LinkFile) Then
    LinkFile := ExtractFilePath(FN)+LinkFile ;

  If FileExists(LinkFile) Then
    Result := OpenVideo(LinkFile);
End;

Function TVideoLink.OpenVideo(VideoFileName : String):Boolean;
VAR
  FN : AnsiString ;
  PFN : PAnsiChar ;
begin
  Result := False ;
  If (not VideoLink.IsAvailable) or (Not FileExists(VideoFileName)) Then Exit ;
  if Assigned(vlcMediaPlayer) Then BtnClose.Click ;

  FN := AnsiString(VideoFileName);
  PFN := Pointer(FN);

  // create new vlc instance
  vlcInstance := libvlc_new(0, nil);
  // create new vlc media from file
  vlcMedia := libvlc_media_new_path(vlcInstance, PFN);

  // if you want to play from network, use libvlc_media_new_location instead
  // vlcMedia := libvlc_media_new_location(vlcInstance, 'udp://@225.2.1.27:5127');

  // create new vlc media player
  vlcMediaPlayer := libvlc_media_player_new_from_media(vlcMedia);

  // now no need the vlc media, free it
  libvlc_media_release(vlcMedia);

  // play video in a TPanel, if not call this routine, vlc media will open a new window
  libvlc_media_player_set_hwnd(vlcMediaPlayer, Pointer(VideoPanel.Handle));

  // move to start
  libvlc_media_player_play(vlcMediaPlayer);
  libvlc_media_player_pause(vlcMediaPlayer);
  libvlc_media_player_set_time(vlcMediaPlayer, 1);
  libvlc_media_player_next_frame(vlcMediaPlayer);
  // BtnPlay.Click;

  Result := True ;
End;

procedure TVideoLink.ShowVideoForm ;
Begin
  Show;
  Top := MainForm.Top ;
  Left := MainForm.Left + MainForm.Width ;
End;

procedure TVideoLink.MoveToTimePacketOffset(TimeOffset : TDateTime);
VAR
  V : Double ;
  I : Int64 ;
Begin
  V := TimeOffset * 24 * 60 * 60 * 1000 ;
  I := Round(V) + LinkOffset ;
  If (I >= 0)and(I < CurrentSize) Then
    MoveToOffset(I);
End;

procedure TVideoLink.MoveToOffset(TimeOffset : Int64);
Begin
  If Not Assigned(vlcMediaPlayer) Then Exit ;
  TrackBar1.Tag := 1 ;
  libvlc_media_player_set_time(vlcMediaPlayer,TimeOffset);
  TrackBar1.Tag := 0 ;
End;

Function TVideoLink.SaveVideoLink(FN : String):Boolean;
VAR
  SL : TStringList ;
Begin
  Result := False ;
  SL := TStringList.Create ;
  Try
    SL.Add('rem;packetviewerlogviewer video link file');
    SL.Add('video;'+LinkFile);
    SL.Add('offset;'+IntToStr(LinkOffset));
    SL.SaveToFile(FN);
    Result := True ;
  Except
    ShowMessage('Error saving link file: '+FN);
  End;
  FreeAndNil(SL);
End;


end.
