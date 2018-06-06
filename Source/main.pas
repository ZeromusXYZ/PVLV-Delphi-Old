unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, packetdefs, Vcl.CheckLst,
  Vcl.Tabs, Vcl.Menus, Vcl.Grids, Vcl.ComCtrls, System.Actions, Vcl.ActnList,
  Vcl.ExtCtrls;

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
    Splitter1: TSplitter;
    Panel1: TPanel;
    LInfo: TLabel;
    SG: TStringGrid;
    MInfo: TMemo;
    CBOriginalData: TCheckBox;
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
    About2: TMenuItem;
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
    procedure Splitter1Moved(Sender: TObject);
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
  private
    { Private declarations }
    MyAppName : String ;
    FilterList : TStringList ;
    Procedure MoveToSync;
    procedure FillListBox ;
    Procedure AddSGRow(VarName:String;Val:String);
    Procedure SearchNext ;
    Procedure ApplyFromDialog;
  public
    { Public declarations }
    PL, PLLoaded : TPacketList ;
    CurrentSync : Word ;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses System.StrUtils, shellapi, packetparser, searchdialog, filterdialog;

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

procedure TMainForm.FillListBox ;
VAR
  I : Integer ;
Begin
  LBPackets.Cursor := crHourGlass ;
  Application.ProcessMessages ;
  LBPackets.Clear ;
  For I := 0 to PL.Count-1 Do
  Begin
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
End;

procedure TMainForm.MMFileClick(Sender: TObject);
begin
  MMFileAppend.Enabled := PLLoaded.Count > 0 ;
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

procedure TMainForm.MMFilterResetClick(Sender: TObject);
begin
  PL.ClearFilters ;
  PL.CopyFrom(PLLoaded);
  FillListBox;
  MoveToSync;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
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
VAR
  I : Integer ;
begin
  If OpenDialogLogFiles.Execute() Then
  Begin
    LBPackets.Clear ;
    If PLLoaded.LoadFromFile(OpenDialogLogFiles.FileName) Then
    Begin
      // Fill listbox
      PL.ClearFilters;
      PL.CopyFrom(PLLoaded);

      FillListBox ;

      Caption := MyAppName + ' - multiple files loaded' ;
    End else
    Begin
      // Clear Listbox
      LBPackets.Items.Add('Failed Loading Data');
      Caption := MyAppName ;
    End;
  End;
end;

procedure TMainForm.ALOpenFileExecute(Sender: TObject);
VAR
  I : Integer ;
begin
  If OpenDialogLogFiles.Execute() Then
  Begin
    LBPackets.Clear ;
    PL.Clear ;
    PL.ClearFilters ;
    PLLoaded.Clear ;
    If PLLoaded.LoadFromFile(OpenDialogLogFiles.FileName) Then
    Begin
      // Fill listbox
      PL.ClearFilters;
      PL.CopyFrom(PLLoaded);

      FillListBox ;

      Caption := MyAppName + ' - ' + OpenDialogLogFiles.FileName ;
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

Procedure TMainForm.AddSGRow(VarName:String;Val:String);
Begin
  SG.RowCount := SG.RowCount + 1 ;
  SG.Cells[0,SG.RowCount-1] := VarName ;
  SG.Cells[1,SG.RowCount-1] := Val ;
End;

procedure TMainForm.LBPacketsClick(Sender: TObject);
VAR
  PD : TPacketData ;
  S : String ;
  I : Integer ;
  ExtraInfoStart : Integer ;
begin
  If (LBPackets.ItemIndex < 0) or (LBPackets.ItemIndex >= LBPackets.Count) Then
  Begin
    MInfo.Text := 'Please select a valid item from the left' ;
  End else
  Begin
    PD := PL.GetPacket(LBPackets.ItemIndex);
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

    // Reset StringGrid
    SG.RowCount := 0 ;
    SG.ColCount := 2 ;
    SG.ColWidths[0] := 150 ;
    SG.ColWidths[1] := SG.Width - 155 ;
    SG.Cols[0].Text := 'VAR' ;
    SG.Cols[1].Text := 'Value' ;
    ExtraInfoStart := $4 ;

    AddSGRow('ID',S + ' 0x' + IntToHex(PD.PacketID,3)+ ' - ' + PacketTypeToString(PD.PacketLogType,PD.PacketID) );
    AddSGRow('Size',IntToStr(PD.PacketDataSize) + ' (0x'+IntToHex(PD.PacketDataSize,2)+')');
    AddSGRow('Sync',IntToStr(PD.PacketSync) + ' (0x'+ IntToHex(PD.PacketSync,4)+')');

    AddPacketInfoToStringGrid(PD,SG);

    (*
    If (PD.PacketLogType = pltIn) Then
    Case PD.PacketID of
      $00D {PC Update} : AddSGRow('UpdateFlag',IntToStr(PD.GetByteAtPos($0A)));

      $01B {Job Info} : Begin
        AddSGRow('Unknown uint',IntToStr(PD.GetUInt32AtPos($4))); // Observed value of 5
        AddSGRow('Main Job',IntToStr(PD.GetByteAtPos($8)));
        AddSGRow('Main Job Lv?',IntToStr(PD.GetByteAtPos($9)));
        AddSGRow('Sub Job Lv?',IntToStr(PD.GetByteAtPos($A)));
        AddSGRow('Sub Job',IntToStr(PD.GetByteAtPos($B)));
        AddSGRow('Sub Job flag',IntToStr(PD.GetUInt32AtPos($C))); // Indicate whether subjob is unlocked and which jobs are unlocked. lsb of 0x0C indicates subjob unlock.
        AddSGRow('Unknown byte',IntToStr(PD.GetByteAtPos($10)));
        For I := $00 to $0F Do
          AddSGRow('Job'+IntToStr(I),IntToStr(PD.GetByteAtPos($11+I)));
        AddSGRow('Base STR',IntToStr(PD.GetWordAtPos($20)));
        AddSGRow('Base DEX',IntToStr(PD.GetWordAtPos($22)));
        AddSGRow('Base VIT',IntToStr(PD.GetWordAtPos($24)));
        AddSGRow('Base AGI',IntToStr(PD.GetWordAtPos($26)));
        AddSGRow('Base INT',IntToStr(PD.GetWordAtPos($28)));
        AddSGRow('Base MND',IntToStr(PD.GetWordAtPos($2A)));
        AddSGRow('Base CHR',IntToStr(PD.GetWordAtPos($2C)));
        AddSGRow('?Max HP',IntToStr(PD.GetWordAtPos($2E)));
        AddSGRow('Max MP',IntToStr(PD.GetWordAtPos($30)));
        AddSGRow('Unknown Data','14 bytes');  //-- 2E   Flags and junk? Hard to say. All 0s observed.
        *)
        (*
    {ctype='unsigned int',      label='Flags'},                                 -- 44   Looks like a bunch of flags. Observed value if 01 00 00 00
    {ctype='unsigned char',     label='_unknown5'},                             -- 48   Potential flag to signal the list start. Observed value of 01
    {ref=types.job_level,       lookup={res.jobs, 0x01},    count=0x16},        -- 49
    {ctype='unsigned char',     label='Current Monster Level'},                 -- 5F
    {ctype='unsigned int',      label='Encumbrance Flags'},                     -- 60   [legs, hands, body, head, ammo, range, sub, main,] [back, right_ring, left_ring, right_ear, left_ear, waist, neck, feet] [HP, CHR, MND, INT, AGI, VIT, DEX, STR,] [X X X X X X X MP]
        *)
        (*
      End;

      $050 {Equip} : Begin
        AddSGRow('Inventory Index',IntToStr(PD.GetByteAtPos($4)));
        AddSGRow('Equipment Slot',EquipmentSlotName(PD.GetByteAtPos($5)) + ' - ' + IntToStr(PD.GetByteAtPos($5)));
        AddSGRow('Inventory Bag',ContainerName(PD.GetByteAtPos($6)) + ' - ' + IntToStr(PD.GetByteAtPos($6)));
        ExtraInfoStart := $7 ;
      End;
    End;
    *)

    (*
    I := ExtraInfoStart ;
    While I < PD.PacketDataSize Do
    Begin
      AddSGRow('?Byte 0x'+IntToHex(I,2)+' ' +IntToStr(I), '0x' + IntToHex(PD.GetByteAtPos(I),2) + '  ' + BytetoBit(PD.GetByteAtPos(I)) + '  ' + IntToStr(PD.GetByteAtPos(I)));
      I := I + 1 ;
    End;
    *)

    // AddSGRow('??','0x' + IntToHex(PD.ra,3));


    // MInfo.Lines.Add('ID: '+S+' 0x'+ IntToHex(PD.PacketID,3) + ' -- SIZE: ' + IntToStr(PD.PacketDataSize) + ' (0x'+IntToHex(PD.PacketDataSize,2)+')');
    // MInfo.Lines.Add('SYNC: ' + IntToStr(PD.PacketSync) + ' (0x'+ IntToHex(PD.PacketSync,4)+')');
    MInfo.Lines.Clear ;
    If (CBOriginalData.Checked) Then
    Begin
      MInfo.Lines.Add('Source:');
      MInfo.Lines.Add(PD.RawText.Text)
    End Else
    Begin
      // MInfo.Lines.Add('RAW Data:');
      MInfo.Lines.Add(PD.PrintRawBytesAsHex);
    End;
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
  TextCol := clBlack ;
  BackCol := clWhite ;
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
  End Else
  Begin
    PMPacketListOpenFile.Visible := False ;

    PD := PL.GetPacket(LBPackets.ItemIndex);
    If Assigned(PD) Then
    Begin
      PMPacketListShow.Visible := True ;
      PMPacketListShow.Caption := 'Packet 0x' + IntToHex(PD.PacketID,4);
      Case PD.PacketLogType Of
        pltOut : PMPacketListShow.Caption := 'Outgoing 0x' + IntToHex(PD.PacketID,4);
        pltIn  : PMPacketListShow.Caption := 'Incomming 0x' + IntToHex(PD.PacketID,4);
      Else
        PMPacketListShow.Caption := 'Unknown Packet Type 0x' + IntToHex(PD.PacketID,4);
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

procedure TMainForm.Splitter1Moved(Sender: TObject);
begin
  LBPackets.Invalidate ;
end;

end.
