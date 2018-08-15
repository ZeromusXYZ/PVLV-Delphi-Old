unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, packetdefs, Vcl.CheckLst,
  Vcl.Tabs, Vcl.Menus, Vcl.Grids, Vcl.ComCtrls, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls, videoform;

type
  TMainForm = class(TForm)
    OpenDialogLogFiles: TOpenDialog;
    PMPacketList: TPopupMenu;
    PMPacketListShow: TMenuItem;
    PMPacketListN1: TMenuItem;
    PMPacketListOnlyShow: TMenuItem;
    PMPacketListHideThis: TMenuItem;
    PMPacketListN2: TMenuItem;
    PMPacketListReset: TMenuItem;
    PMPacketListOpenFile: TMenuItem;
    PMPacketListN3: TMenuItem;
    PMPacketListOnlyOut: TMenuItem;
    PMPacketListOnlyIn: TMenuItem;
    AL: TActionList;
    ALSearchNext: TAction;
    ALSearchNew: TAction;
    LeftPanel: TPanel;
    LBPackets: TListBox;
    SplitterVertical: TSplitter;
    DataPanel: TPanel;
    MM: TMainMenu;
    MMFile: TMenuItem;
    MMFileOpen: TMenuItem;
    MMFileAppend: TMenuItem;
    MMFileN1: TMenuItem;
    MMFileExit: TMenuItem;
    MMSearch: TMenuItem;
    MMSearchFind: TMenuItem;
    MMSearchFindNext: TMenuItem;
    About1: TMenuItem;
    OpenonGitHub1: TMenuItem;
    MMAboutAbout: TMenuItem;
    ALOpenFile: TAction;
    ALAppendFile: TAction;
    ALOpenSource: TAction;
    ALAbout: TAction;
    MMFilter: TMenuItem;
    MMFilterEdit: TMenuItem;
    MMFilterN1: TMenuItem;
    MMFilterReset: TMenuItem;
    MMFilterApply: TMenuItem;
    MMFilterApplyN1: TMenuItem;
    InfoPanel: TPanel;
    SG: TStringGrid;
    LInfo: TLabel;
    LShowBlock: TLabel;
    CBShowBlock: TComboBox;
    PanelData: TPanel;
    MInfo: TRichEdit;
    CBOriginalData: TCheckBox;
    SplitterHorizontal: TSplitter;
    ALGridFont1: TAction;
    ALGridFont2: TAction;
    MMFont: TMenuItem;
    ALGridFont11: TMenuItem;
    ALGridFont21: TMenuItem;
    PMPacketListN4: TMenuItem;
    PMPacketListEditParser: TMenuItem;
    SaveDialogRawPacket: TSaveDialog;
    PMPacketListSavePacket: TMenuItem;
    ALAppendClipboard: TAction;
    MMFileAddClipboard: TMenuItem;
    AutoExecTimer: TTimer;
    ALVideoLink: TAction;
    MMVideo: TMenuItem;
    MMOpenVideoLink: TMenuItem;
    ALVideoLinkSave: TAction;
    MMVideoLinkSave: TMenuItem;
    MMAboutN1: TMenuItem;
    MMAboutVideoLAN: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LBPacketsClick(Sender: TObject);
    procedure LBPacketsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure PMPacketListPopup(Sender: TObject);
    procedure PMPacketListOpenFileClick(Sender: TObject);
    procedure PMPacketListResetClick(Sender: TObject);
    procedure PMPacketListOnlyShowClick(Sender: TObject);
    procedure PMPacketListHideThisClick(Sender: TObject);
    procedure PMPacketListOnlyOutClick(Sender: TObject);
    procedure PMPacketListOnlyInClick(Sender: TObject);
    procedure ALSearchNextExecute(Sender: TObject);
    procedure ALSearchNewExecute(Sender: TObject);
    procedure SplitterVerticalMoved(Sender: TObject);
    procedure MMFileExitClick(Sender: TObject);
    procedure ALOpenFileExecute(Sender: TObject);
    procedure ALAppendFileExecute(Sender: TObject);
    procedure ALOpenSourceExecute(Sender: TObject);
    procedure ALAboutExecute(Sender: TObject);
    procedure MMFilterClick(Sender: TObject);
    procedure MMFilterApplyN1Click(Sender: TObject);
    procedure MMFilterDeleteN1Click(Sender: TObject);
    procedure MMFileClick(Sender: TObject);
    procedure MMFilterEditClick(Sender: TObject);
    procedure MMFilterResetClick(Sender: TObject);
    procedure CBShowBlockClick(Sender: TObject);
    procedure SGFixedCellClick(Sender: TObject; ACol, ARow: Integer);
    procedure CBOriginalDataClick(Sender: TObject);
    procedure SGDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure ALGridFont1Execute(Sender: TObject);
    procedure ALGridFont2Execute(Sender: TObject);
    procedure PMPacketListEditParserClick(Sender: TObject);
    procedure PMPacketListSavePacketClick(Sender: TObject);
    procedure ALAppendClipboardExecute(Sender: TObject);
    procedure AutoExecTimerTimer(Sender: TObject);
    procedure ALVideoLinkExecute(Sender: TObject);
    procedure MMVideoLinkSaveClick(Sender: TObject);
    procedure MMVideoClick(Sender: TObject);
    procedure MMAboutVideoLANClick(Sender: TObject);
  private
    { Private declarations }
    MyAppName : String ;
    FilterList : TStringList ;
    Procedure MoveToSync;
    procedure FillListBox ;
    Procedure AddSGRow(Pos : Integer; VarName:String; Val:String;DataSize : Integer = 1);
    Procedure SearchNext ;
    Procedure ApplyFromDialog;
    Procedure UpdatePacketDetails(ShowBlock:String);
    Procedure PrintRawBytesAsHexRE(PD : TPacketData ; RE : TRichedit);
  public
    { Public declarations }
    PL, PLLoaded : TPacketList ;
    CurrentSync : Word ;
    CurrentDateTimeOffset : TDateTime ;

    Procedure MoveToOffset(Offset : Int64);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses System.UITypes, System.Types, System.StrUtils, shellapi, Vcl.Clipbrd,
  packetparser, searchdialog, filterdialog, loadingform;

procedure GetBuildInfo(var V1, V2, V3, V4: word);
var
  VerInfoSize, VerValueSize, Dummy: DWORD;
  VerInfo: Pointer;
  VerValue: PVSFixedFileInfo;
begin
  VerInfoSize := GetFileVersionInfoSize(PChar(ParamStr(0)), Dummy);
  if VerInfoSize > 0 then
  begin
      GetMem(VerInfo, VerInfoSize);
      try
        if GetFileVersionInfo(PChar(ParamStr(0)), 0, VerInfoSize, VerInfo) then
        begin
          VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
          with VerValue^ do
          begin
            V1 := dwFileVersionMS shr 16;
            V2 := dwFileVersionMS and $FFFF;
            V3 := dwFileVersionLS shr 16;
            V4 := dwFileVersionLS and $FFFF;
          end;
        end;
      finally
        FreeMem(VerInfo, VerInfoSize);
      end;
  end;
end;

function GetBuildInfoAsString: string;
var
  V1, V2, V3, V4: word;
begin
  GetBuildInfo(V1, V2, V3, V4);
  Result := IntToStr(V1) + '.' + IntToStr(V2) + '.' +
    IntToStr(V3) + '.' + IntToStr(V4);
end;

Procedure TMainForm.MoveToSync;
VAR
  I :Integer ;
Begin
  For I := 0 to PL.Count-1 Do
  If PL.GetPacket(I).PacketSync = CurrentSync Then
  Begin
    LBPackets.ItemIndex := I ;
    LBPackets.Invalidate ;
    Exit ;
  End;
End;

Procedure TMainForm.MoveToOffset(Offset : Int64);
VAR
  I :Integer ;
  FindTime : TDateTime ;
Begin
  If PLLoaded.Count <= 0 Then Exit ;
  FindTime := PLLoaded.GetPacket(0).TimeStamp + (Offset / 24 / 60 / 60 / 1000);

  For I := 0 to PL.Count-1 Do
  If PL.GetPacket(I).TimeStamp >= FindTime Then
  Begin
    If (LBPackets.ItemIndex <> I) Then
    Begin
      LBPackets.ItemIndex := I ;
      LBPackets.Invalidate ;
    End;
    UpdatePacketDetails('-');
    Exit ;
  End;
End;



procedure TMainForm.FillListBox ;
VAR
  I : Integer ;
Begin
  LBPackets.Cursor := crHourGlass ;
  Application.ProcessMessages ;
  LBPackets.Clear ;

  FormLoading.Show ;
  FormLoading.BringToFront ;
  If (Random(100) >= 95) Then
    FormLoading.Caption := 'Sacrificing Taru-Taru''s, please wait ...'
  Else
    FormLoading.Caption := 'Populating ListBox, please wait ...' ;

  For I := 0 to PL.Count-1 Do
  Begin

    If FormLoading.Visible and ((I mod 100) = 0) Then
    Begin
      FormLoading.Repaint ;
      FormLoading.PB.Max := PL.Count ;
      FormLoading.PB.Min := 0 ;
      FormLoading.PB.Position := I ;
    End;

    Case PL.GetPacket(I).PacketLogType of
      pltOut : LBPackets.Items.Add('=> '+PL.GetPacket(I).Header);
      pltIn : LBPackets.Items.Add('<= '+PL.GetPacket(I).Header);
    Else
      LBPackets.Items.Add('?? '+PL.GetPacket(I).Header);
    End;
  End;
  If LBPackets.Count > 0 Then
  Begin
    LBPackets.ItemIndex := 0 ;
  End;
  LBPackets.Invalidate;
  LBPackets.Cursor := crDefault ;
  If Assigned(FormLoading) Then FormLoading.Hide ;
End;

Procedure TMainForm.PrintRawBytesAsHexRE(PD : TPacketData ; RE : TRichedit);

  Procedure W(Txt : String;Col : TColor = -1;EoL:Boolean=True);
  Begin
    RE.SelStart := Length(RE.Text);
    RE.SelLength := 0 ;
    If Col <> -1 Then
      RE.SelAttributes.Color := Col ;
    If EoL Then
      RE.SelText := Txt + CR
    Else
      RE.SelText := Txt ;
    RE.SelStart := Length(RE.Text);
    RE.SelLength := 0 ;
  End;

VAR
  Result , S : String ;
  I , L , Line : Integer ;
  B : Byte ;
  NCol : Integer ;
  C : TColor ;
Begin
  RE.Clear ;
  RE.Brush.Color := clWhite ;
  RE.Font.Color := clBlack ;
  NCol := 0 ;

  W(RawDataHeader1,clBlack);
  W(RawDataHeader2);
  L := 0 ;
  For I := 0 To PD.RawSize-1 Do
  Begin
    // C := DataCol(NCol);
    C := RGB($CC,$CC,$CC);

    If ((I mod ValuesPerRow) = 0) Then
    Begin
      W(IntToHex(L,2) + ' | ',clBlack,False);
    End;

    B := PD.GetByteAtPos(I);
    S := IntToHex(B,2);
    W(S,C,False);
    // Result := Result + S ;

    If (I mod 4) = 3 Then
    Begin
      W(' ',clBlack,False); // Result := Result + ' ' ; // extra spacing every 4 bytes
      NCol := NCol + 1 ;
    End;

    If (I mod ValuesPerRow) = ValuesPerRow-1 Then
    Begin
      W(' ');
      // Result := Result + #13#10 ;
      L := L + 1 ;
    End Else
    Begin
      W(' ',clBlack,False);
      // Result := Result + ' ' ;
    End;
  End;
End;


procedure TMainForm.MMAboutVideoLANClick(Sender: TObject);
begin
  if Not MMVideo.Enabled then
    ShowMessage('This viewer requires the 32bits version of VLC to be installed');
  ShellExecute(Handle, 'open','https://www.videolan.org/',nil,nil, SW_SHOWNORMAL) ;
end;

procedure TMainForm.MMFileClick(Sender: TObject);
begin
  MMFileAppend.Enabled := PLLoaded.Count > 0 ;
  MMFileAddClipboard.Enabled := (Clipboard.AsText <> '');
end;

procedure TMainForm.MMFileExitClick(Sender: TObject);
begin
  Close ;
end;

procedure TMainForm.MMFilterApplyN1Click(Sender: TObject);
Var
  MI : TMenuItem ;
begin
  // Apply filter
  If (Sender is TMenuItem) Then MI := (Sender as TMenuItem) else MI := nil ;

  If Assigned(MI) and (MI.Tag >= 0) Then
  Begin
    Try
      DlgFilter.LoadFromFile(ExtractFilePath(Application.ExeName)+'filters\'+FilterList[MI.Tag]);
      ApplyFromDialog ;
      // ShowMessage('Apply -> ' + FilterList[MI.Tag]);
    Except

    End;
  End;
end;

procedure TMainForm.MMFilterClick(Sender: TObject);
Var
  DI : TSearchRec ;
  I , Res : Integer ;
  MI : TMenuItem ;
begin
  // Populate filter menu items
  MMFilterReset.Enabled := (PL.FilterOutType <> ftFilterOff) or (PL.FilterInType <> ftFilterOff);
  MMFilterApply.Enabled := PLLoaded.Count > 0 ;
  Try
    // Clear menu items
    For I := MMFilterApply.Count-1 DownTo 0 Do
    If (MMFilterApply.Items[I].Tag >= 0) Then
    Begin
      MMFilterApply.Items[I].Free ;
      //MMFilterApply.Delete(I);
    End;
    FilterList.Clear ;

    Res := FindFirst(ExtractFilePath(Application.ExeName)+'filters\*.pfl',faAnyFile,DI);
    While (Res = 0) Do
    Begin
      If ((DI.Attr and faDirectory) = 0) Then
      Begin
        FilterList.Add(DI.Name);
        MI := TMenuItem.Create(MMFilterApply);
        MI.Caption := ChangeFileExt(DI.Name,'');
        MI.Tag := FilterList.Count-1 ; // list offset
        MI.OnClick := MMFilterApplyN1Click ;
        MMFilterApply.Add(MI);
      End;

      Res := FindNext(DI);
    End;
    FindClose(DI);
  Finally

  End;
end;

procedure TMainForm.MMFilterDeleteN1Click(Sender: TObject);
begin
  // Delete stuff
end;

procedure TMainForm.MMFilterEditClick(Sender: TObject);
begin
  DlgFilter.CopySettingsFromPacketList(PL);
  If DlgFilter.ShowModal <> mrOk Then Exit ;
  // Copy settings from form

  ApplyFromDialog ;
End;

Procedure TMainForm.ApplyFromDialog;
VAR
  I, V, P : Integer ;
  S : String ;
Begin
  If (DlgFilter.RBOutOff.Checked) Then PL.FilterOutType := ftFilterOff ;
  If (DlgFilter.RBOutHide.Checked) Then PL.FilterOutType := ftHidePackets ;
  If (DlgFilter.RBOutShow.Checked) Then PL.FilterOutType := ftShowPackets ;
  If (DlgFilter.RBOutNone.Checked) Then PL.FilterOutType := ftAllowNone ;

  If (DlgFilter.RBInOff.Checked) Then PL.FilterInType := ftFilterOff ;
  If (DlgFilter.RBInHide.Checked) Then PL.FilterInType := ftHidePackets ;
  If (DlgFilter.RBInShow.Checked) Then PL.FilterInType := ftShowPackets ;
  If (DlgFilter.RBInNone.Checked) Then PL.FilterInType := ftAllowNone ;

  SetLength(PL.FilterOutList,0);
  For I := 0 To DlgFilter.LBOut.Count-1 Do
  Begin
    S := DlgFilter.LBOut.Items[I];
    P := Pos(' ',S);
    If (P > 0) Then S := Copy(S,1,P-1);

    If TryStrToInt(S,V) Then
    Begin
      SetLength(PL.FilterOutList,Length(PL.FilterOutList)+1);
      PL.FilterOutList[Length(PL.FilterOutList)-1] := V ;
    End;
  End;

  SetLength(PL.FilterInList,0);
  For I := 0 To DlgFilter.LBIn.Count-1 Do
  Begin
    S := DlgFilter.LBIn.Items[I];
    P := Pos(' ',S);
    If (P > 0) Then S := Copy(S,1,P-1);

    If TryStrToInt(S,V) Then
    Begin
      SetLength(PL.FilterInList,Length(PL.FilterInList)+1);
      PL.FilterInList[Length(PL.FilterInList)-1] := V ;
    End;
  End;

  PL.FilterFrom(PLLoaded);
  FillListBox;
  MoveToSync;
end;

procedure TMainForm.AutoExecTimerTimer(Sender: TObject);
VAR
  I : Integer ;
  FN : String ;
  Stuffloaded : Boolean ;
begin
  AutoExecTimer.Enabled := False ;

  Caption := MyAppName ;
  StuffLoaded := False ;
  For I := 1 to ParamCount do
  Begin
    FN := ParamStr(I);
    If (FN <> '') and FileExists(FN) Then
    Begin
      If PLLoaded.LoadFromFile(FN,'') Then
      Begin
        StuffLoaded := True ;
        Caption := Caption + ' <= ' + FN ;
      End;
    End;

  End;

  If Stuffloaded Then
  Begin
    LBPackets.Clear ;
    PL.Clear ;
    PL.ClearFilters ;
    PL.CopyFrom(PLLoaded);

    FillListBox ;
  End;

end;

procedure TMainForm.CBOriginalDataClick(Sender: TObject);
begin
  LBPacketsClick(nil);
end;

procedure TMainForm.CBShowBlockClick(Sender: TObject);
begin
  UpdatePacketDetails(CBShowBlock.Text);
end;

procedure TMainForm.MMFilterResetClick(Sender: TObject);
begin
  PL.ClearFilters ;
  PL.CopyFrom(PLLoaded);
  FillListBox;
  MoveToSync;
end;

procedure TMainForm.MMVideoClick(Sender: TObject);
begin
  MMVideoLinkSave.Enabled := (VideoLink.LinkFile <> '')
end;

procedure TMainForm.MMVideoLinkSaveClick(Sender: TObject);
begin
  If (Not VideoLink.IsAvailable) or (VideoLink.CurrentPos < 0) Then Exit ;

  If FileExists(VideoLink.LinkFile) Then
    VideoLink.SaveVideoLink(VideoLink.LinkSourceFile);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Randomize;
  PLLoaded := TPacketList.Create(True); // NOTE: PLLoaded actually owns all TPacketData
  PL := TPacketList.Create(False); // NOTE: PL just copies reference of TPacketData as needed by the filter
  LBPackets.Clear ;
  MyAppName := Caption ;
  CurrentSync := $FFFF ;
  FilterList := TStringList.Create ;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FilterList);
  FreeAndNil(PL);
  FreeAndNil(PLLoaded);
end;



procedure TMainForm.ALAboutExecute(Sender: TObject);
begin
  // If you contribute to the code, please add your nickname here
  MessageDlg('Made by ZeromusXYZ'+
    #10#13+
    'Version ' + GetBuildInfoAsString,mtInformation,[mbClose],-1);
end;

procedure TMainForm.ALAppendFileExecute(Sender: TObject);
begin
  If OpenDialogLogFiles.Execute() Then
  Begin
    LBPackets.Clear ;
    If PLLoaded.LoadFromFile(OpenDialogLogFiles.FileName,'') Then
    Begin
      // Fill listbox
      PL.ClearFilters;
      PL.CopyFrom(PLLoaded);

      FillListBox ;

      Caption := MyAppName + ' - multiple files loaded' ;
      VideoLink.LinkSourceFile := '' ; // disable link file
    End else
    Begin
      // Clear Listbox
      LBPackets.Items.Add('Failed Loading Data');
      Caption := MyAppName ;
    End;
  End;
end;

procedure TMainForm.ALGridFont1Execute(Sender: TObject);
begin
  SG.Font.Name := 'Consolas' ;
  SG.Font.Size := 10 ;
  SG.Font.Style := [] ;
  SG.Invalidate ;
  SG.DefaultRowHeight := 24 ;
  SG.Invalidate ;
end;

procedure TMainForm.ALGridFont2Execute(Sender: TObject);
begin
  SG.Font.Name := 'Consolas' ;
//  SG.Font.Name := 'Fixedsys' ;
  SG.Font.Size := 8 ;
  SG.Font.Style := [] ;
  SG.DefaultRowHeight := 16 ;
  SG.Invalidate ;
end;

procedure TMainForm.ALOpenFileExecute(Sender: TObject);
begin
  If OpenDialogLogFiles.Execute() Then
  Begin
    LBPackets.Clear ;
    PL.Clear ;
    PL.ClearFilters ;
    PLLoaded.Clear ;
    If PLLoaded.LoadFromFile(OpenDialogLogFiles.FileName,'') Then
    Begin
      // Fill listbox
      PL.ClearFilters;
      PL.CopyFrom(PLLoaded);

      FillListBox ;

      Caption := MyAppName + ' - ' + OpenDialogLogFiles.FileName ;
      If VideoLink.TryOpenVideoLink(ChangeFileExt(OpenDialogLogFiles.FileName,'.pvlvvl')) Then
        VideoLink.ShowVideoForm ;
    End else
    Begin
      // Clear Listbox
      LBPackets.Items.Add('Failed Loading Data');
      Caption := MyAppName ;
    End;
  End;
end;

procedure TMainForm.ALOpenSourceExecute(Sender: TObject);
begin
  ShellExecute(Handle, 'open','https://github.com/ZeromusXYZ/PacketViewerLogViewer',nil,nil, SW_SHOWNORMAL) ;
end;

procedure TMainForm.ALSearchNewExecute(Sender: TObject);
begin
  // Nothnig loaded, prompt the dialog
  If PLLoaded.Count <= 0 Then
  Begin
    ALOpenFile.Execute;
  End;
  // Still nothing loaded ?
  If PLLoaded.Count <= 0 Then
  Begin
    ShowMessage('Doesn''t seem like we have anything in the current file to find');
    Exit ;
  End;

  // Show search Dialog
  If DlgSearch.ShowModal = mrOk Then
  Begin
    // Let's see if we can find it
    SearchNext ;
    LBPackets.SetFocus;
  End;
end;

procedure TMainForm.ALSearchNextExecute(Sender: TObject);
begin
  If ((DlgSearch.FormValid) and (DlgSearch.CBShowMatchesOnly.Checked = False)) Then
  Begin
    // When pressing F3 and we didn't filter on previous search, then search next ...
    SearchNext ;
  End Else
  Begin
    ALSearchNew.Execute ;
  End;
end;

procedure TMainForm.ALVideoLinkExecute(Sender: TObject);
begin
  If VideoLink.IsAvailable Then
    VideoLink.ShowVideoForm ;
end;

procedure TMainForm.ALAppendClipboardExecute(Sender: TObject);
VAR
  LastCount : Integer ;
  ClipStr : String ;
begin
  LastCount := PLLoaded.Count ;

  If (not Clipboard.HasFormat(CF_TEXT)) or (Clipboard.AsText = '') Then Exit ;

  ClipStr := Clipboard.AsText ;
  // Some messings around so copy/paste from various sources/os work
  ClipStr := ReplaceStr(ClipStr,#10#13,#13);
  ClipStr := ReplaceStr(ClipStr,#13#10,#13);
  ClipStr := ReplaceStr(ClipStr,#13,#13#10);

  If PLLoaded.LoadFromFile('',ClipStr) Then
  Begin
    If PLLoaded.Count <= LastCount Then
    Begin
      ShowMessage('Clipboard did not seem to contain any valid packet data ...'#10#13+ClipStr);
      Caption := MyAppName ;
      Exit ;
    End;

    LBPackets.Clear ;
    // Fill listbox
    PL.ClearFilters;
    PL.CopyFrom(PLLoaded);

    FillListBox ;

    Caption := MyAppName + ' - clipboard data loaded' ;
    VideoLink.LinkSourceFile := '' ; // disable link file
  End else
  Begin
    // Clear Listbox
    LBPackets.Items.Add('Failed Loading Data');
    Caption := MyAppName ;
  End;
  //
end;

Procedure TMainForm.AddSGRow(Pos : Integer; VarName:String; Val:String;DataSize : Integer = 1);
Begin
  SG.RowCount := SG.RowCount + 1 ;
  SG.Cells[0,SG.RowCount-1] := '0x'+IntToHex(Pos,2);
  SG.Cells[1,SG.RowCount-1] := IntToStr(DataSize);
  SG.Cells[2,SG.RowCount-1] := VarName ;
  SG.Cells[3,SG.RowCount-1] := Val ;
End;

Procedure TMainForm.UpdatePacketDetails(ShowBlock:String);
VAR
  PD : TPacketData ;
  S : String ;
  I : Integer ;
Begin
  PD := PL.GetPacket(LBPackets.ItemIndex);
  If Not Assigned(PD) Then Exit ;
  CurrentSync := PD.PacketSync ;
  LInfo.Caption := PD.OriginalHeader ;
  // MInfo.Text := PD.RawText.Text ;
  MInfo.Lines.Clear ;
  Case PD.PacketLogType Of
    1 : S := 'OUT' ;
    2 : S := 'IN ' ;
  Else
    S := '???' ;
  End;

  // Raw Data viewer
  MInfo.Lines.Clear ;
  If (CBOriginalData.Checked) Then
  Begin
    MInfo.SelAttributes.Color := clBlack ;
    MInfo.Lines.Add('Source:');
    MInfo.Lines.Add(PD.RawText.Text);
    UpdateActiveRE := nil ; // Disable color fields
  End Else
  Begin
    // MInfo.Lines.Add('RAW Data:');
    // MInfo.Lines.Add(PD.PrintRawBytesAsHex);
    UpdateActiveRE := MInfo ; // Enable color fields
    PrintRawBytesAsHexRE(PD,MInfo);
  End;

  // Reset StringGrid
  SG.RowCount := 0 ;
  SG.ColCount := 4 ;
  SG.ColWidths[0] := 40 ;
  SG.ColWidths[1] := 0 ;
  SG.ColWidths[2] := 150 ;
  SG.ColWidths[3] := SG.Width - 220 ;
  SG.Cols[0].Text := 'Pos' ;
  SG.Cols[1].Text := 'Size' ;
  SG.Cols[2].Text := 'VAR' ;
  SG.Cols[3].Text := 'Value' ;

  // Add general header
  AddSGRow($0,'ID',S + ' 0x' + IntToHex(PD.PacketID,3)+ ' - ' + PacketTypeToString(PD.PacketLogType,PD.PacketID),2 );
  AddSGRow($2,'Size',IntToStr(PD.PacketDataSize) + ' (0x'+IntToHex(PD.PacketDataSize,2)+')',2);
  AddSGRow($2,'Sync',IntToStr(PD.PacketSync) + ' (0x'+ IntToHex(PD.PacketSync,4)+')',2);

  // Clear switch block info
  CBShowBlock.Enabled := False ;
  CBShowBlock.Text := '' ;
  // Fill info grid
  AddPacketInfoToStringGrid(PD,SG,ShowBlock);
  // Re-Mark headers
  SG.FixedCols := 1 ;
  SG.FixedRows := 1 ;

  // Block switch combobox
  If (AvailableBlocks.Count > 0) Then
  Begin
    CBShowBlock.Clear ;
    CBShowBlock.Items.Add('');
    CBShowBlock.Items.AddStrings(AvailableBlocks);
    CBShowBlock.Text := '' ;
    CBShowBlock.ItemIndex := 0 ;
    CBShowBlock.Enabled := True ;
  End else
  Begin
    CBShowBlock.Text := '' ;
    CBShowBlock.Clear ;
    CBShowBlock.Enabled := False ;
  End;

  CBShowBlock.Visible := AvailableBlocks.Count > 0;
  LShowBlock.Visible := CBShowBlock.Visible ;

  For I := CBShowBlock.Items.Count-1 DownTo 0  Do
  If CBShowBlock.Items[I] = ShowBlock Then
  Begin
    CBShowBlock.ItemIndex := I ;
    Break ;
  End;

  UpdateActiveRE := nil ;
End;

procedure TMainForm.LBPacketsClick(Sender: TObject);
begin
  If (LBPackets.ItemIndex < 0) or (LBPackets.ItemIndex >= LBPackets.Count) Then
  Begin
    MInfo.Text := 'Please select a valid item from the left' ;
  End else
  Begin
    UpdatePacketDetails('-');
    CurrentDateTimeOffset := PL.GetPacket(LBPackets.ItemIndex).TimeStamp - PLLoaded.GetPacket(0).TimeStamp ;
    If VideoLink.IsAvailable Then
      VideoLink.MoveToTimePacketOffset(CurrentDateTimeOffset) ;
  End;
  LBPackets.Invalidate;
end;

procedure TMainForm.LBPacketsDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
VAR
  S : String ;
  B, BarOn : Boolean ;
  Flags : LongInt ;
  PD : TPacketData ;
  BarCol, TextCol, BackCol : TColor ;
  BarRect : TRect ;
begin
  S := LBPackets.Items[Index];
  B := (Index = LBPackets.ItemIndex);
  BarCol := clBlack ;
  BarOn := False ;
  PD := PL.GetPacket(Index);
  With (Control as TListBox) Do
  Begin
    if (Assigned(PD) and (PD.PacketLogType = 1)) Then
    Begin
      TextCol := clBlue ;
      If B THen
      Begin
        BackCol := RGB($CC,$CC,$FF);
        If (CurrentSync = PD.PacketSync) Then BarOn := True ;
      End else
      If (CurrentSync = PD.PacketSync) Then
      Begin
        BackCol := RGB($88,$88,$FF);
        BarOn := True ;
      End Else
        BackCol := RGB($EE,$EE,$FF);
    End Else
    if (Assigned(PD) and (PD.PacketLogType = 2)) Then
    Begin
      TextCol := clGreen ;
      If B Then
      Begin
        BackCol := RGB($CC,$FF,$CC);
        If (CurrentSync = PD.PacketSync) Then BarOn := True ;
      end else
      If (CurrentSync = PD.PacketSync) Then
      Begin
        BackCol := RGB($88,$FF,$88);
        BarOn := True ;
      End Else
        BackCol := RGB($EE,$FF,$EE);
    End Else
    Begin
      TextCol := clBlack ;
      BackCol := clWhite;
    End;
    // Draw Background
    Canvas.Brush.Color := BackCol ;
    Canvas.FillRect(Rect);

    Canvas.Font.Color := TextCol ;
    Canvas.Brush.Color := BackCol ;
    Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
    DrawText(Canvas.Handle, Items[Index], Length(Items[Index]), Rect, Flags);

    // Draw Side-bar markings if needed
    If (BarOn) Then
    Begin
      Canvas.Brush.Color := BarCol ;
      BarRect := Rect ;
      If (B) Then
        BarRect.Left := Rect.Right - 12
      Else
        BarRect.Left := Rect.Right - 8 ;

      Canvas.FillRect(BarRect);
      Canvas.Brush.Color := BackCol ;
    End;

  End;
end;

procedure TMainForm.PMPacketListEditParserClick(Sender: TObject);
VAR
  FN , FullFileName : String ;
  PD : TPacketData ;
  SL : TStringList ;
begin
  PD := PL.GetPacket(LBPackets.ItemIndex);

  If Assigned(PD) and (PD.PacketLogType <> pltUnknown) Then
  Begin
    FN := 'parse\' ;
    If (PD.PacketLogType = pltIn) Then FN := FN + 'in-' ;
    If (PD.PacketLogType = pltOut) Then FN := FN + 'out-' ;
    FN := FN + '0x' + IntToHex(PD.PacketID,3) + '.txt' ;
    FullFileName := ExtractFilePath(Application.ExeName) + FN ;

    If Not FileExists(FullFileName) Then
      If MessageDlg('No parser file for '+FN+#10#13'Do you want to create one ?',TMsgDlgType.mtConfirmation,[mbYes,mbNo],-1) = mrYes Then
      Begin
        SL := TStringList.Create ;
        SL.Add('file;'+ChangeFileExt(ExtractFileName(FN),'') +';Unknown;Newly created parser.');
        SL.Add('');
        SL.Add('rem;Add your parser lines here');
        Try
          SL.SaveToFile(FullFileName);
        Except
          On E: Exception Do
            ShowMessage('Error: ' + E.Message);
        End;
        FreeAndNil(SL);
      End;


    If FileExists(FullFileName) Then
      ShellExecute(Handle, 'open',PChar(FullFileName),nil,nil, SW_SHOWNORMAL)
    Else
      ShowMessage('File not found to edit');
  End;


end;

procedure TMainForm.PMPacketListHideThisClick(Sender: TObject);
VAR
  PD : TPacketData ;
begin
  PD := PL.GetPacket(LBPackets.ItemIndex);

  If Assigned(PD) and (PD.PacketLogType = pltOut) Then
  Begin
    If (PL.FilterOutType <> ftHidePackets) Then
    Begin
      SetLength(PL.FilterOutList,0);
      PL.FilterOutType := ftHidePackets ;
    End;
    SetLength(PL.FilterOutList,Length(PL.FilterOutList)+1);
    PL.FilterOutList[Length(PL.FilterOutList)-1] := PD.PacketID ;

    PL.FilterFrom(PLLoaded);
    FillListBox;
    MoveToSync;
    Exit ;
  End Else
  If Assigned(PD) and (PD.PacketLogType = pltIn) Then
  Begin
    If (PL.FilterInType <> ftHidePackets) Then
    Begin
      SetLength(PL.FilterInList,0);
      PL.FilterInType := ftHidePackets ;
    End;

    SetLength(PL.FilterInList,Length(PL.FilterInList)+1);
    PL.FilterInList[Length(PL.FilterInList)-1] := PD.PacketID ;

    PL.FilterFrom(PLLoaded);
    FillListBox;
    MoveToSync;
    Exit ;
  End;

end;

procedure TMainForm.PMPacketListOnlyInClick(Sender: TObject);
begin
  PL.FilterOutType := ftAllowNone ;
  If PL.FilterInType = ftAllowNone Then
    PL.FilterInType := ftFilterOff ;

  PL.FilterFrom(PLLoaded);
  FillListBox;
  MoveToSync;
end;

procedure TMainForm.PMPacketListOnlyOutClick(Sender: TObject);
begin
  PL.FilterInType := ftAllowNone ;
  If PL.FilterOutType = ftAllowNone Then
    PL.FilterOutType := ftFilterOff ;

  PL.FilterFrom(PLLoaded);
  FillListBox;
  MoveToSync;
end;

procedure TMainForm.PMPacketListOnlyShowClick(Sender: TObject);
VAR
  PD : TPacketData ;
begin
  PD := PL.GetPacket(LBPackets.ItemIndex);

  If Assigned(PD) and (PD.PacketLogType = pltOut) Then
  Begin
    If (PL.FilterOutType <> ftShowPackets) Then
    Begin
      SetLength(PL.FilterOutList,0);
      PL.FilterOutType := ftShowPackets ;
    End;
    SetLength(PL.FilterOutList,Length(PL.FilterOutList)+1);
    PL.FilterOutList[Length(PL.FilterOutList)-1] := PD.PacketID ;

    PL.FilterFrom(PLLoaded);
    FillListBox;
    MoveToSync;
    Exit ;
  End Else
  If Assigned(PD) and (PD.PacketLogType = pltIn) Then
  Begin
    If (PL.FilterInType <> ftShowPackets) Then
    Begin
      SetLength(PL.FilterInList,0);
      PL.FilterInType := ftShowPackets ;
    End;

    SetLength(PL.FilterInList,Length(PL.FilterInList)+1);
    PL.FilterInList[Length(PL.FilterInList)-1] := PD.PacketID ;

    PL.FilterFrom(PLLoaded);
    FillListBox;
    MoveToSync;
    Exit ;
  End;

end;

procedure TMainForm.PMPacketListOpenFileClick(Sender: TObject);
begin
  ALOpenFile.Execute ;
end;

procedure TMainForm.PMPacketListPopup(Sender: TObject);
VAR
  PD : TPacketData ;
begin
  If (PLLoaded.Count = 0) Then
  Begin
    PMPacketListOpenFile.Visible := True ;
    PMPacketListShow.Visible := False ;
    PMPacketListOnlyShow.Visible := False ;
    PMPacketListHideThis.Visible := False ;
    PMPacketListReset.Visible := False ;
    PMPacketListOnlyOut.Visible := False ;
    PMPacketListOnlyIn.Visible := False ;
    PMPacketListEditParser.Visible := False ;
    PMPacketListSavePacket.Visible := False ;
  End Else
  Begin
    PMPacketListOpenFile.Visible := False ;

    PD := PL.GetPacket(LBPackets.ItemIndex);
    If Assigned(PD) Then
    Begin
      PMPacketListShow.Visible := True ;
      PMPacketListShow.Caption := 'Packet 0x' + IntToHex(PD.PacketID,4);
      PMPacketListSavePacket.Visible := True ;
      Case PD.PacketLogType Of
        pltOut : Begin
          PMPacketListShow.Caption := 'Outgoing 0x' + IntToHex(PD.PacketID,3);
          PMPacketListEditParser.Caption := 'Edit  parse\out-0x' + IntToHex(PD.PacketID,3)+'.txt' ;
          PMPacketListEditParser.Visible := True ;
          PMPacketListEditParser.Enabled := True ;
        End;
        pltIn  : Begin
          PMPacketListShow.Caption := 'Incomming 0x' + IntToHex(PD.PacketID,3);
          PMPacketListEditParser.Caption := 'Edit  parse\in-0x' + IntToHex(PD.PacketID,3)+'.txt' ;
          PMPacketListEditParser.Visible := True ;
          PMPacketListEditParser.Enabled := True ;
        End
      Else
        Begin
          PMPacketListShow.Caption := 'Unknown Packet Type 0x' + IntToHex(PD.PacketID,3);
          PMPacketListEditParser.Caption := 'Edit not supported';
          PMPacketListEditParser.Visible := True ;
          PMPacketListEditParser.Enabled := False ;
        End;
      End;

      PMPacketListOnlyShow.Visible := (PD.PacketLogType = pltOut) or (PD.PacketLogType = pltIn);
      PMPacketListOnlyShow.Enabled := PMPacketListOnlyShow.Visible and
        ( // (
          (PD.PacketLogType = pltOut)// and (PL.FilterOutType = ft (PD.PacketID <> PL.FilterOutOnly))
          or // (
          (PD.PacketLogType = pltIn)// and (PD.PacketID <> PL.FilterInOnly))
        );

      PMPacketListHideThis.Visible := (PD.PacketLogType = pltOut) or (PD.PacketLogType = pltIn);
      PMPacketListHideThis.Enabled := PMPacketListHideThis.Visible and PMPacketListOnlyShow.Enabled and
        ( // (
        (PD.PacketLogType = pltOut) // and (Not WordInArray(PD.PacketID,PL.FilterOutList)))
        or // (
        (PD.PacketLogType = pltIn) // and (Not WordInArray(PD.PacketID,PL.FilterInList)))
        );

      PMPacketListOnlyOut.Visible := (PL.FilterOutType <> ftAllowNone);
      PMPacketListOnlyIn.Visible := (PL.FilterInType <> ftAllowNone);

      PMPacketListReset.Visible := (PL.FilterOutType <> ftFilterOff) or (PL.FilterInType <> ftFilterOff);
    End Else
    Begin

    End;


  End;


end;

procedure TMainForm.PMPacketListResetClick(Sender: TObject);
begin
  PL.ClearFilters ;
  PL.CopyFrom(PLLoaded);
  FillListBox;
  MoveToSync;
end;

procedure TMainForm.PMPacketListSavePacketClick(Sender: TObject);
VAR
  PD : TPacketData ;
  BS : TBinaryWriter  ;
  I : Integer ;
  DefName : String ;
begin
  PD := PL.GetPacket(LBPackets.ItemIndex);

  DefName := ExtractFileName(SaveDialogRawPacket.FileName);
  If PD.PacketLogType = pltOut Then
    DefName := 'o'+IntToHex(PD.PacketID,3)
  Else If PD.PacketLogType = pltIn Then
    DefName := 'i'+IntToHex(PD.PacketID,3)
  Else
    DefName := 'u'+IntToHex(PD.PacketID,3);
  SaveDialogRawPacket.FileName := DefName ;


  If Assigned(PD) and SaveDialogRawPacket.Execute() Then
  Begin
    Try
      BS := TBinaryWriter.Create(SaveDialogRawPacket.FileName,False);
      For I := 0 To PD.RawSize-1 Do
        BS.Write(PD.GetByteAtPos(I));
      BS.Close ;
    Except
      On E: Exception Do
        ShowMessage('ERROR: '+E.Message);
    End;
    If Assigned(BS) Then FreeAndNil(BS);
  End;

end;

Procedure TMainForm.SearchNext ;
VAR
  StartIndex , Pos : Integer ;
  PD : TPacketData ;
  FoundThis , CheckCount : Integer ;
Begin
  // Nothing ?
  If LBPackets.Count <= 0 Then Exit ;

  // Safeguard start location
  If (LBPackets.ItemIndex < 0) or (LBPackets.ItemIndex >= LBPackets.Count) Then
  Begin
    StartIndex := 0 ;
    Pos := 0 ;
    LBPackets.ItemIndex := 0 ;
  End Else
  Begin
    StartIndex := LBPackets.ItemIndex ;
    Pos := StartIndex + 1 ;
  End;

  // Save Location
  Repeat
    PD := PL.GetPacket(Pos);

    If ((PD.PacketLogType = pltOut)and(DlgSearch.SearchOut)) or
       ((PD.PacketLogType = pltIn)and(DlgSearch.SearchIn)) Then
    Begin
      // Only search types we selected
      FoundThis := 0 ;
      CheckCount := 0 ;

      If (DlgSearch.PacketFilterOn) Then
      Begin
        CheckCount := CheckCount + 1 ;
        If (PD.PacketID = DlgSearch.PacketFilter) Then FoundThis := FoundThis + 1 ;
      End;

      If (DlgSearch.SyncFilterOn) Then
      Begin
        CheckCount := CheckCount + 1 ;
        If (PD.PacketSync = DlgSearch.SyncFilter) Then FoundThis := FoundThis + 1 ;
      End;

      If (DlgSearch.ByteFilterOn) Then
      Begin
        CheckCount := CheckCount + 1 ;
        If (PD.FindByte(DlgSearch.ByteFilter) >= 0) Then FoundThis := FoundThis + 1 ;
      End;

      If (DlgSearch.UInt16FilterOn) Then
      Begin
        CheckCount := CheckCount + 1 ;
        If (PD.FindUInt16(DlgSearch.UInt16Filter) >= 0) Then FoundThis := FoundThis + 1 ;
      End;

      If (DlgSearch.UInt32FilterOn) Then
      Begin
        CheckCount := CheckCount + 1 ;
        If (PD.FindUInt32(DlgSearch.UInt32Filter) >= 0) Then FoundThis := FoundThis + 1 ;
      End;

      If (CheckCount > 0) and (FoundThis = CheckCount) Then
      Begin
        LBPackets.ItemIndex := Pos ;
        LBPacketsClick(LBPackets);
        LBPackets.Invalidate ;
        Exit ;
      End;

    End;

    Pos := Pos + 1 ;
    If Pos >= LBPackets.Count Then Pos := 0 ;
  Until Pos = StartIndex ;

  ShowMessage('No more matches found!');
End;

procedure TMainForm.SGDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
VAR
  SG : TStringGrid ;
  Canvas : TCanvas ;
begin
  If (ACol = 0) and (CBOriginalData.Checked = false) Then
  Begin
    SG := (Sender as TStringGrid);
    Canvas := SG.Canvas ;
    Canvas.Brush.Color := clWindow ;
    // Canvas.Brush.Color := RGB($EE,$EE,$EE);
    Canvas.FillRect(Rect);
    If (ARow > 3) Then
      Canvas.Font.Color := DataCol(ARow-3)
    Else
      Canvas.Font.Color := clBlack ;

    Canvas.TextOut(Rect.Left + BevelWidth+1,Rect.CenterPoint.Y - (Canvas.TextHeight('0') div 2) - BevelWidth,(Sender as TStringGrid).Cells[ACol,ARow]);
  End;
end;

procedure TMainForm.SGFixedCellClick(Sender: TObject; ACol, ARow: Integer);
VAR
  N,S : Integer ;
begin
  // Only try if using custom raw data, and clicking a used row header
  If (CBOriginalData.Checked = False) and (ACol = 0) and (ARow > 0) then
    If TryStrToInt(SG.Cells[ACol,ARow],N) Then
    Begin
      MInfo.SelectAll ;
      MInfo.SelAttributes.Color := clGray ;
      // Try to grab size from grid
      If Not TryStrToInt(SG.Cells[1,ARow],S) Then S := 1 ; // default to 1
      MarkREBytes(MInfo,N,S,clBlue);
    End;
end;

procedure TMainForm.SplitterVerticalMoved(Sender: TObject);
begin
  LBPackets.Invalidate ;
end;

end.
