unit settingsdialog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TDlgSettings = class(TForm)
    BtnClose: TButton;
    CBUseVLC: TCheckBox;
    CBGridFont: TComboBox;
    Label1: TLabel;
    LRestartWarning: TLabel;
    CBVirtualTime: TCheckBox;
    procedure BtnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBUseVLCClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CBGridFontChange(Sender: TObject);
    procedure CBVirtualTimeClick(Sender: TObject);
  private
    { Private declarations }
    IsLoading: Boolean;
  public
    { Public declarations }
    UseLibVLC: Boolean;
    GridFontType: Integer;
    UseVirtualTime: Boolean;

    Procedure LoadDefaults;
    Procedure LoadSettings;
    Procedure SaveSettings;
  end;

var
  DlgSettings: TDlgSettings;

implementation

{$R *.dfm}

Uses Registry;

CONST
  RegKey = 'Software\ZeromusXYZ\PacketViewerLogViewer';
  RegRoot = HKEY_CURRENT_USER;
  RegKey_UseVLC = 'UseLibVLC';
  RegKey_GridFont = 'GridFontType';
  RegKey_UseVirtualTime = 'VideoTimeSmoothing';

Procedure TDlgSettings.LoadSettings;
VAR
  Reg: TRegistry;
Begin
  IsLoading := True;
  Try
    Reg := TRegistry.Create(STANDARD_RIGHTS_READ or KEY_QUERY_VALUE or
      KEY_WOW64_32KEY);
    Reg.RootKey := RegRoot;
    LoadDefaults;
    If Reg.OpenKeyReadOnly(RegKey) Then
    Begin
      if Reg.ValueExists(RegKey_UseVLC) Then
        UseLibVLC := Reg.ReadBool(RegKey_UseVLC);
      if Reg.ValueExists(RegKey_GridFont) Then
        GridFontType := Reg.ReadInteger(RegKey_GridFont);
      if Reg.ValueExists(RegKey_UseVirtualTime) Then
        UseVirtualTime := Reg.ReadBool(RegKey_UseVirtualTime);
    End;

  Finally
    if Assigned(Reg) then
      FreeAndNil(Reg);
  End;
  CBUseVLC.Checked := UseLibVLC;
  if (GridFontType >= 0) and (GridFontType < CBGridFont.Items.Count) then
  Begin
    CBGridFont.ItemIndex := GridFontType;
  End
  Else
  Begin
    CBGridFont.ItemIndex := 0;
  End;
  CBVirtualTime.Checked := UseVirtualTime;

  IsLoading := False;
End;

Procedure TDlgSettings.LoadDefaults;
Begin
  UseLibVLC := True;
  GridFontType := 0;
  UseVirtualTime := True;
End;

Procedure TDlgSettings.SaveSettings;
VAR
  Reg: TRegistry;
Begin
  Try
    Reg := TRegistry.Create(KEY_WRITE or KEY_WOW64_32KEY);
    Reg.RootKey := RegRoot;
    Reg.OpenKey(RegKey, True);
    Reg.WriteBool(RegKey_UseVLC, UseLibVLC);
    Reg.WriteInteger(RegKey_GridFont, GridFontType);
    Reg.WriteBool(RegKey_UseVirtualTime, UseVirtualTime);
    Reg.CloseKey;
  Except
    On E: Exception do
      ShowMessage('Failed saving settings'#10#13 + E.Message);
  End;
  if Assigned(Reg) then
    FreeAndNil(Reg);
End;

procedure TDlgSettings.BtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TDlgSettings.CBGridFontChange(Sender: TObject);
begin
  if not IsLoading then
  Begin
    GridFontType := (Sender as TComboBox).ItemIndex;
  End;
end;

procedure TDlgSettings.CBUseVLCClick(Sender: TObject);
begin
  if not IsLoading then
  Begin
    LRestartWarning.Visible := True;
    UseLibVLC := (Sender as TCheckBox).Checked;
  End;
end;

procedure TDlgSettings.CBVirtualTimeClick(Sender: TObject);
begin
  if not IsLoading then
  Begin
    LRestartWarning.Visible := True;
    UseVirtualTime := (Sender as TCheckBox).Checked;
  End;
end;

procedure TDlgSettings.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveSettings;
end;

procedure TDlgSettings.FormCreate(Sender: TObject);
begin
  LoadSettings;
end;

end.
