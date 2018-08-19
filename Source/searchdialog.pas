unit searchdialog;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls;

type
  TDlgSearch = class(TForm)
    BtnOK: TButton;
    BtnCancel: TButton;
    RGDirection: TRadioGroup;
    GroupBox1: TGroupBox;
    EPacketID: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    ESync: TEdit;
    CBShowMatchesOnly: TCheckBox;
    Label3: TLabel;
    EValue: TEdit;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure RGDirectionClick(Sender: TObject);
    procedure EPacketIDChange(Sender: TObject);
    procedure ESyncChange(Sender: TObject);
    procedure EValueChange(Sender: TObject);
  private
    { Private declarations }
    fPacketFilterOn : Boolean ;
    fPacketFilter : Word ;
    fSyncFilterOn : Boolean ;
    fSyncFilter : Word ;
    fSearchOut : Boolean ;
    fSearchIn : Boolean ;
    fByteFilterOn : Boolean ;
    fByteFilter : Byte ;
    fUInt16FilterOn : Boolean ;
    fUInt16Filter : Word ;
    fUInt32FilterOn : Boolean ;
    fUInt32Filter : LongWord ;
    fFormValid : Boolean ;
    Procedure ValidateForm ;
  public
    { Public declarations }
    Property FormValid : Boolean read fFormValid ;
    Property PacketFilterOn : Boolean read fPacketFilterOn ;
    Property PacketFilter : Word read fPacketFilter ;
    Property SyncFilterOn : Boolean read fSyncFilterOn ;
    Property SyncFilter : Word read fSyncFilter ;
    Property SearchOut : Boolean read fSearchOut ;
    Property SearchIn : Boolean read fSearchIn ;
    Property ByteFilterOn : Boolean read fByteFilterOn ;
    Property ByteFilter : Byte read fByteFilter ;
    Property UInt16FilterOn : Boolean read fUInt16FilterOn ;
    Property UInt16Filter : Word read fUInt16Filter ;
    Property UInt32FilterOn : Boolean read fUInt32FilterOn ;
    Property UInt32Filter : LongWord read fUInt32Filter ;
  end;

var
  DlgSearch: TDlgSearch;

implementation

{$R *.dfm}

procedure TDlgSearch.EPacketIDChange(Sender: TObject);
begin
  ValidateForm ;
end;

procedure TDlgSearch.ESyncChange(Sender: TObject);
begin
  ValidateForm ;
end;

procedure TDlgSearch.EValueChange(Sender: TObject);
begin
  ValidateForm ;
end;

procedure TDlgSearch.FormCreate(Sender: TObject);
begin
  fFormValid := False ;
  fPacketFilterOn := False ;
  fPacketFilter := $000 ;
  fSyncFilterOn := False ;
  fSyncFilter := 0 ;
  fSearchOut := True ;
  fSearchIn := True ;
  fByteFilterOn := False ;
  fByteFilter := 0 ;
  fUInt16FilterOn := False ;
  fUInt16Filter := 0 ;
  fUInt32FilterOn := False ;
  fUInt32Filter := 0 ;
end;

procedure TDlgSearch.RGDirectionClick(Sender: TObject);
begin
  If (RGDirection.ItemIndex = 1) Then
  Begin
    fSearchOut := True ;
    fSearchIn := False ;
  End Else
  If (RGDirection.ItemIndex = 2) Then
  Begin
    fSearchOut := False ;
    fSearchIn := True ;
  End Else
  Begin
    fSearchOut := True ;
    fSearchIn := True ;
  End;
  ValidateForm ;
end;

Procedure TDlgSearch.ValidateForm ;
VAR
  Valid, HasData : Boolean ;
  I : Integer ;
Begin
  Valid := True ;
  HasData := False ;

  // PacketID
  fPacketFilterOn := False ;
  If TryStrToInt(EPacketID.Text,I) Then
  Begin
    If (I >= 0) and (I <= $FFF) Then
    Begin
      fPacketFilter := I ;
      fPacketFilterOn := True ;
      HasData := True ;
    End;
  End Else
  If (EPacketID.Text <> '') Then
  Begin
    Valid := False ;
  End;

  // Sync Number
  fSyncFilterOn := False ;
  If TryStrToInt(ESync.Text,I) Then
  Begin
    If (I >= 0) and (I <= $FFFF) Then
    Begin
      fSyncFilter := I ;
      fSyncFilterOn := True ;
      HasData := True ;
    End;
  End Else
  If (ESync.Text <> '') Then
  Begin
    Valid := False ;
  End;

  // Value
  fByteFilterOn := False ;
  fUInt16FilterOn := False ;
  fUInt32FilterOn := False ;
  If TryStrToInt(EValue.Text,I) Then
  Begin
    If (I >= 0) and (I <= $FF) Then
    Begin
      fByteFilter := I ;
      fByteFilterOn := True ;
      HasData := True ;
    End else
    If (I >= $100) and (I <= $FFFF) Then
    Begin
      fUInt16Filter := I ;
      fUInt16FilterOn := True ;
      HasData := True ;
    End Else
    If (I >= $10000) {and (I <= $FFFFFFFF)} Then
    Begin
      fUInt32Filter := I ;
      fUInt32FilterOn := True ;
      HasData := True ;
    End;
  End Else
  If (EValue.Text <> '') Then
  Begin
    Valid := False ;
  End;

  fFormValid := Valid and HasData ;
  BtnOK.Enabled := FormValid ;
End;


end.
