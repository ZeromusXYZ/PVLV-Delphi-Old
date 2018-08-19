unit filterdialog;

interface

uses Winapi.Windows, System.SysUtils, System.Classes, System.UITypes,
  Vcl.Graphics, Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons,
  Vcl.ExtCtrls, Vcl.Dialogs, packetdefs;

type
  TDlgFilter = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    GBOutFilter: TGroupBox;
    Label1: TLabel;
    RBOutOff: TRadioButton;
    RBOutHide: TRadioButton;
    RBOutShow: TRadioButton;
    RBOutNone: TRadioButton;
    LBOut: TListBox;
    CBOut: TComboBox;
    BtnOutAdd: TButton;
    BtnOutDel: TButton;
    GBInFilter: TGroupBox;
    Label2: TLabel;
    RBInOff: TRadioButton;
    RBInHide: TRadioButton;
    RBInShow: TRadioButton;
    RBInNone: TRadioButton;
    LBIn: TListBox;
    BtnInAdd: TButton;
    BtnInDel: TButton;
    CBIn: TComboBox;
    BtnSaveAs: TButton;
    FilterSaveDialog: TSaveDialog;
    FilterOpenDialog: TOpenDialog;
    BtnLoad: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BtnOutAddClick(Sender: TObject);
    procedure BtnSaveAsClick(Sender: TObject);
    procedure BtnOutDelClick(Sender: TObject);
    procedure BtnInAddClick(Sender: TObject);
    procedure BtnLoadClick(Sender: TObject);
    procedure BtnInDelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Procedure CopySettingsFromPacketList(PL : TPacketList);
    Procedure LoadFromFile(FN : String);
    Procedure SaveToFile(FN : String);
    procedure ClearFilters ;
  end;

var
  DlgFilter: TDlgFilter;

implementation

{$R *.dfm}

uses datalookups;

procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings) ;
begin
   ListOfStrings.Clear;
   ListOfStrings.Delimiter       := Delimiter;
   ListOfStrings.StrictDelimiter := True;
   ListOfStrings.DelimitedText   := Str;
end;

procedure TDlgFilter.BtnInAddClick(Sender: TObject);
VAR
  I,P,V : Integer ;
  S, VS : String ;
begin
  S := CBIn.Text ;
  P := Pos(' ',S);
  If (P > 0) Then S := Copy(S,1,P-1);

  If TryStrToInt(S,V) Then
  Begin
    If (V < 0) or (V > $FFF) Then
    Begin
      ShowMessage('Please enter a number between 0 and 0xFFF (0 -> 4095)');
      Exit ;
    End;
    VS := '0x'+IntToHex(V,3) ;

    For I := 0 To LBIn.Count-1 Do
    If Copy(LBIn.Items[I],1,Length(VS)) = VS Then
    Begin
      ShowMessage('Already in list');
      Exit ;
    End;

    LBIn.Items.Add(VS+' - '+NLU(LU_PacketIn).GetVal(V));
  End Else
  Begin
    ShowMessage('Not a valid number');
  End;
end;

procedure TDlgFilter.BtnInDelClick(Sender: TObject);
begin
  LBIn.DeleteSelected ;
end;

procedure TDlgFilter.BtnLoadClick(Sender: TObject);
begin
  If FilterOpenDialog.Execute() Then
    LoadFromFile(FilterOpenDialog.FileName);
end;

procedure TDlgFilter.BtnOutAddClick(Sender: TObject);
VAR
  I,P,V : Integer ;
  S, VS : String ;
begin
  S := CBOut.Text ;
  P := Pos(' ',S);
  If (P > 0) Then S := Copy(S,1,P-1);

  If TryStrToInt(S,V) Then
  Begin
    If (V < 0) or (V > $FFF) Then
    Begin
      ShowMessage('Please enter a number between 0 and 0xFFF (0 -> 4095)');
      Exit ;
    End;
    VS := '0x'+IntToHex(V,3) ;

    For I := 0 To LBOut.Count-1 Do
    If Copy(LBOut.Items[I],1,Length(VS)) = VS Then
    Begin
      ShowMessage('Already in list');
      Exit ;
    End;

    LBOut.Items.Add(VS+' - '+NLU(LU_PacketOut).GetVal(V));
  End Else
  Begin
    ShowMessage('Not a valid number');
  End;
end;

procedure TDlgFilter.BtnOutDelClick(Sender: TObject);
begin
  LBOut.DeleteSelected ;
end;

procedure TDlgFilter.BtnSaveAsClick(Sender: TObject);
begin
  If FilterSaveDialog.Execute() Then
    SaveToFile(FilterSaveDialog.FileName);
end;

procedure TDlgFilter.FormCreate(Sender: TObject);
VAR
  I : Integer ;
begin
  FilterSaveDialog.InitialDir := ExtractFilePath(Application.ExeName)+'filters\' ;

  CBOut.Clear ;
  For I := 0 To NLU(LU_PacketOut).Count-1 Do
    CBOut.Items.Add('0x'+IntToHex(NLU(LU_PacketOut).GetItem(I).ID,3)+' '+NLU(LU_PacketOut).GetItem(I).Val);

  CBIn.Clear ;
  For I := 0 To NLU(LU_PacketIn).Count-1 Do
    CBIn.Items.Add('0x'+IntToHex(NLU(LU_PacketIn).GetItem(I).ID,3)+' '+NLU(LU_PacketIn).GetItem(I).Val);
  ClearFilters ;
end;

Procedure TDlgFilter.CopySettingsFromPacketList(PL : TPacketList);
VAR
  I : Integer ;
Begin
  LBOut.Clear ;
  LBIn.Clear ;
  RBOutOff.Checked := (PL.FilterOutType = ftFilterOff);
  RBOutHide.Checked := (PL.FilterOutType = ftHidePackets);
  RBOutShow.Checked := (PL.FilterOutType = ftShowPackets);
  RBOutNone.Checked := (PL.FilterOutType = ftAllowNone);
  RBInOff.Checked := (PL.FilterInType = ftFilterOff);
  RBInHide.Checked := (PL.FilterInType = ftHidePackets);
  RBInShow.Checked := (PL.FilterInType = ftShowPackets);
  RBInNone.Checked := (PL.FilterInType = ftAllowNone);

  For I := 0 To Length(PL.FilterOutList)-1 Do
    LBOut.Items.Add('0x' + IntToHex(PL.FilterOutList[I],3) +' - '+NLU(LU_PacketOut).GetVal(PL.FilterOutList[I]));

  For I := 0 To Length(PL.FilterInList)-1 Do
    LBIn.Items.Add('0x' + IntToHex(PL.FilterInList[I],3) +' - '+NLU(LU_PacketIn).GetVal(PL.FilterInList[I]));

End;

Procedure TDlgFilter.LoadFromFile(FN : String);
VAR
  SL , Line : TStringList ;
  I , V : Integer ;
  Key , Val : String ;
Begin
  SL := TStringList.Create ;
  Line := TStringList.Create ;
  Try
    SL.LoadFromFile(FN);
    ClearFilters ;

    For I := 0 to SL.Count-1 do
    Begin
      Split(';',LowerCase(SL[I]),Line);
      If Line.Count >= 1 Then Key := Line[0] else Key := '' ;
      If Line.Count >= 2 Then Val := Line[1] else Val := '' ;

      If (Key = 'outtype') Then
      Begin
        If (Val = 'off') Then RBOutOff.Checked := True ;
        If (Val = 'hide') Then RBOutHide.Checked := True ;
        If (Val = 'show') Then RBOutShow.Checked := True ;
        If (Val = 'none') Then RBOutNone.Checked := True ;
      End;
      If (Key = 'intype') Then
      Begin
        If (Val = 'off') Then RBInOff.Checked := True ;
        If (Val = 'hide') Then RBInHide.Checked := True ;
        If (Val = 'show') Then RBInShow.Checked := True ;
        If (Val = 'none') Then RBInNone.Checked := True ;
      End;
      If (Key = 'out') Then
      Begin
        If TryStrToInt(Val,V) Then
          LBOut.Items.Add('0x'+IntToHex(V,3) +' - '+NLU(LU_PacketOut).GetVal(V));
      End;
      If (Key = 'in') Then
      Begin
        If TryStrToInt(Val,V) Then
          LBIn.Items.Add('0x'+IntToHex(V,3) +' - '+NLU(LU_PacketIn).GetVal(V));
      End;

    End;

  Except
    on E: Exception do
      MessageDlg('Failed to load '+FN+#10#13+E.Message,mtError,[mbAbort],0);
  End;
  FreeAndNil(Line);
  FreeAndNil(SL);
End;

Procedure TDlgFilter.SaveToFile(FN : String);
VAR
  SL : TStringList ;
  I , P , V : Integer ;
  S : String ;
Begin
  SL := TStringList.Create ;
  Try
    SL.Add('rem;original-file;'+ExtractFileName(FN));
    If (RBOutOff.Checked) Then SL.Add('outtype;off');
    If (RBOutHide.Checked) Then SL.Add('outtype;hide');
    If (RBOutShow.Checked) Then SL.Add('outtype;show');
    If (RBOutNone.Checked) Then SL.Add('outtype;none');

    For I := 0 To DlgFilter.LBOut.Count-1 Do
    Begin
      S := DlgFilter.LBOut.Items[I];
      P := Pos(' ',S);
      If (P > 0) Then S := Copy(S,1,P-1);

      If TryStrToInt(S,V) Then
      Begin
        SL.Add('out;0x'+IntToHex(V,3)+';'+NLU(LU_PacketOut).GetVal(V));
      End;
    End;

    If (RBInOff.Checked) Then SL.Add('intype;off');
    If (RBInHide.Checked) Then SL.Add('intype;hide');
    If (RBInShow.Checked) Then SL.Add('intype;show');
    If (RBInNone.Checked) Then SL.Add('intype;none');

    For I := 0 To DlgFilter.LBIn.Count-1 Do
    Begin
      S := DlgFilter.LBIn.Items[I];
      P := Pos(' ',S);
      If (P > 0) Then S := Copy(S,1,P-1);

      If TryStrToInt(S,V) Then
      Begin
        SL.Add('in;0x'+IntToHex(V,3)+';'+NLU(LU_PacketIn).GetVal(V));
      End;
    End;

    SL.SaveToFile(FN);
  Except
    on E: Exception do
      MessageDlg('Failed to save '+FN+#10#13+E.Message,mtError,[mbAbort],0);
  End;
  FreeAndNil(SL);
End;

procedure TDlgFilter.ClearFilters ;
begin
  RBOutOff.Checked := True ;
  RBOutHide.Checked := False ;
  RBOutShow.Checked := False ;
  RBOutNone.Checked := False ;
  LBOut.Clear ;

  RBInOff.Checked := True ;
  RBInHide.Checked := False ;
  RBInShow.Checked := False ;
  RBInNone.Checked := False ;
  LBIn.Clear ;
end;


end.
