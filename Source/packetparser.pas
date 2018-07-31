unit packetparser;

interface

uses packetdefs, Vcl.Grids, System.Classes, System.Contnrs, Vcl.ComCtrls, System.UITypes, Vcl.Graphics ;

CONST
  RawDataHeader1 = '   |  0  1  2  3   4  5  6  7   8  9  A  B   C  D  E  F' ;
  RawDataHeader2 = '---+----------------------------------------------------';
  CR = #13 ; // #13#10 ; // We only use CR because the TRichEdit ignores the LF when adding stuff
  RawDataRowHeaderSize = 5 ;
  ValuesPerRow = 16 ;

VAR
  AvailableBlocks : TStringList ;
  UpdateActiveRE : TRichEdit ;

Function AddPacketInfoToStringGrid(PD : TPacketData;SG : TStringGrid;var BlockName : String):Boolean;
Procedure MarkREBytes(RE : TRichEdit;FirstByte,DataSize:Integer;Col : TColor);
Function GetREPosForRawByte(Pos : Integer):Integer;
Function DataCol(ColIndex : Integer):TColor;

implementation

uses System.SysUtils, System.StrUtils , datalookups, System.DateUtils ;

CONST
//  DataColors : Array[0..9] of TColor = (clBlack,clRed,clLime,clBlue,clFuchsia,clDkGray,clMaroon,clGreen,clNavy,clPurple);
  DataColors : Array[0..7] of TColor = (clBlack,clRed,clGreen,clBlue,clPurple,clDkGray,clMaroon,clNavy);

Function DataCol(ColIndex : Integer):TColor;
Begin
  Result := DataColors[ColIndex mod Length(DataColors)];
End;

Procedure AddSGRow(SG: TStringGrid; Pos : Integer; VarName:String; Val:String;DataSize : Integer = 1);
Begin
  SG.RowCount := SG.RowCount + 1 ;
  SG.Cells[0,SG.RowCount-1] := '0x'+IntToHex(Pos,2);
  SG.Cells[1,SG.RowCount-1] := IntToStr(DataSize);
  SG.Cells[2,SG.RowCount-1] := VarName ;
  SG.Cells[3,SG.RowCount-1] := Val ;
End;

procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings) ;
begin
   ListOfStrings.Clear;
   ListOfStrings.Delimiter       := Delimiter;
   ListOfStrings.StrictDelimiter := True;
   ListOfStrings.DelimitedText   := Str;
end;

Function GetREPosForRawByte(Pos : Integer):Integer;
CONST
  BeginOfData = Length(RawDataHeader1) + Length(RawDataHeader2) + (Length(CR)*2) + RawDataRowHeaderSize ;
VAR
  I, N : Integer ;
Begin
  N := BeginOfData ; // Base
  For I := 0 To Pos-1 Do
  Begin
    // New Line
    If (I > 0) and ((I mod ValuesPerRow) = 0) Then
      N := N + RawDataRowHeaderSize ;

    // 1 Byte + space
    N := N + 3 ;

    // Extra spaces every 4 bytes
    If (I mod 4) = 3 Then
      N := N + 1 ;

    // EoL if needed
    If ((I mod ValuesPerRow) = ValuesPerRow-1) Then
      N := N + Length(CR);

  End;
  Result := N ;
End;

Procedure MarkREBytes(RE : TRichEdit;FirstByte,DataSize:Integer;Col : TColor);
VAR
  EndPos : Integer ;
begin
  RE.SelStart := GetREPosForRawByte(FirstByte);
  EndPos := GetREPosForRawByte(FirstByte + DataSize);
  RE.SelLength := EndPos - RE.SelStart ;
  RE.SelAttributes.Color := Col ;
  RE.SelLength := 0 ;
end;

Function ActionCategoryToStr(Cat : Byte):String;
Begin
  Case Cat Of
    0 : Result := 'None' ;
    1 : Result := 'Melee Attack' ;
    2 : Result := 'Ranged Attack Finish' ;
    3 : Result := 'Weapon Skill Finish' ;
    4 : Result := 'Magic Finish' ;
    5 : Result := 'Item Finish' ;
    6 : Result := 'Job Ability Finish' ;
    7 : Result := 'Weapon Skill Start' ;
    8 : Result := 'Magic Start' ;
    9 : Result := 'Item Start' ;
    10 : Result := 'Job Ability Start' ;
    11 : Result := 'NPC Ability Finish' ;
    12 : Result := 'Ranged Attack Start' ;
    13 : Result := 'Pet Mob Ability Finish' ;
    14 : Result := 'Job Ability DNC' ;
    15 : Result := 'Job Ability RUN' ;

    // DSP stuff (not actually used as max value would be 15 with just 4 bits)
    29 : Result := 'ACTION_ITEM_INTERRUPT' ;
    31 : Result := 'ACTION_MAGIC_INTERRUPT' ;
    32 : Result := 'ACTION_RANGED_INTERRUPT' ;
    33 : Result := 'ACTION_MOBABILITY_START' ;
    35 : Result := 'ACTION_MOBABILITY_INTERRUPT' ;
    37 : Result := 'ACTION_RAISE_MENU_SELECTION' ;
  Else
    Result := 'Unknown 0x' + IntToHex(Cat,2);
  End;
End;

Function ActionReactionToStr(Reaction : Byte):String;
Begin
  Case Reaction Of
    $00 : Result := 'REACTION_NONE' ;
    $01 : Result := 'REACTION_MISS' ;
    $02 : Result := 'REACTION_PARRY' ;
    $04 : Result := 'REACTION_BLOCK' ;
    $08 : Result := 'REACTION_HIT' ;
    $09 : Result := 'REACTION_EVADE' ;
    $14 : Result := 'REACTION_GUARD' ; // mnk guard (20 dec)
  Else
    Result := 'REACTION_UNKNOWN_0x' + IntToHex(Reaction,2);
  End;
End;


Function AddPacketInfoIn0x028(PD : TPacketData;SG : TStringGrid):Integer; // Returns last used Byte
VAR
  pSize : Byte ;
  pActor : UInt32 ;
  pTargetCount : UInt16 ;
  pActionCategory : Byte ;
  pActionID : UInt16 ;
  pUnknown1 : UInt16 ;
  pRecast : UInt32 ;
  FirstTargetOffset : Integer ;
  LastBit : Integer ;
  Offset : Integer ;
  //
  pActionTargetID : UInt32 ;
  pActionTargetIDSize : Byte ;
  pTargetCountLoopCounter : Integer ;
  //
  tReaction : Byte ;
  tAnimation : UInt16 ;
  tSpecialEffect : Byte ;
  tKnockback : Byte ;
  tParam : Int32 ;
  tMessageID : UInt16 ;
  tUnknown : UInt32 ;
  tTargetEffectLoopCounter : Integer ;
  //
  tAdditionalEffect : UInt16 ;
  tAddEffectParam : Int32 ;
  tAddEffectMessage : UInt16 ;


Begin
  If (PD.PacketLogType <> pltIn) or (PD.PacketID <> $28) Then
  Begin
    AddSGRow(SG,0,'Error','Wrong parser option for this packet type',0);
    Result := 0 ;
    Exit;
  End;

  pSize := PD.GetByteAtPos($04);
  pActor := PD.GetUInt32AtPos($05);
  pTargetCount := PD.GetBitsAtPos($09,0,10);
  // First group contains info about the size instead of actual data ?
  // The bit offset is a pain to work with however >.>
  pActionCategory := PD.GetBitsAtPos($0A,2,4);
  pActionID := PD.GetBitsAtPos($0A,6,16);
  pUnknown1 := PD.GetBitsAtPos($0C,6,16);
  pRecast := PD.GetBitsAtPos($0E,6,32);

  AddSGRow(SG,$4,'Info Size',IntToStr(pSize),1);
  AddSGRow(SG,$5,'Actor','0x'+InttoHex(pActor,8)+' - '+IntToStr(pActor),2);
  AddSGRow(SG,$9,'Target Count',IntToStr(pTargetCount),2);
  AddSGRow(SG,$A,'Action Cat',IntToStr(pActionCategory) + ' - ' + ActionCategoryToStr(pActionCategory),1);
  AddSGRow(SG,$A,'Action ID',IntToStr(pActionID),2);
  AddSGRow(SG,$C,'Unknown1',IntToStr(pUnknown1),2);
  AddSGRow(SG,$E,'Recast',IntToStr(pRecast),4);

  FirstTargetOffset := 150 ; // $12:6
  LastBit := PD.RawSize * 8 ;

  Offset := FirstTargetOffset ;
  pTargetCountLoopCounter := 0 ;

  While (Offset < LastBit) and (pTargetCountLoopCounter < pTargetCount) Do
  Begin
    pTargetCountLoopCounter := pTargetCountLoopCounter + 1 ;

    pActionTargetID := PD.GetBitsAtPos(Offset,32);
    AddSGRow(SG,(Offset div 8),'#' + IntToStr(pTargetCountLoopCounter)+' : Target ID','0x' + IntToHex(pActionTargetID,8)+' - '+ IntToStr(pActionTargetID),4);
    Offset := Offset + 32 ;

    pActionTargetIDSize := PD.GetBitsAtPos(Offset,4);
    AddSGRow(SG,(Offset div 8),'#' + IntToStr(pTargetCountLoopCounter)+' : Count',IntToStr(pActionTargetIDSize),1);
    Offset := Offset + 4 ;

    tTargetEffectLoopCounter := 0 ;
    While (Offset < LastBit)and(tTargetEffectLoopCounter < pActionTargetIDSize) Do
    Begin
      tTargetEffectLoopCounter := tTargetEffectLoopCounter + 1 ;

      tReaction := PD.GetBitsAtPos(Offset,5);
      AddSGRow(SG,(Offset div 8), ' #' + IntToStr(pTargetCountLoopCounter)+' '+IntToStr(tTargetEffectLoopCounter)+'/'+IntToStr(pActionTargetIDSize) +
        ' : Reaction',IntToStr(tReaction)+' - '+ActionReactionToStr(tReaction) ,
        1);
      Offset := Offset + 5 ;

      tAnimation := PD.GetBitsAtPos(Offset,12);
      AddSGRow(SG,(Offset div 8),' #' + IntToStr(pTargetCountLoopCounter)+' '+IntToStr(tTargetEffectLoopCounter)+'/'+IntToStr(pActionTargetIDSize) +
        ' : Animation','0x' + IntToHex(tAnimation,4) + ' - ' + IntToStr(tAnimation),
        2);
      Offset := Offset + 12 ;

      tSpecialEffect := PD.GetBitsAtPos(Offset,7);
      AddSGRow(SG,(Offset div 8),' #' + IntToStr(pTargetCountLoopCounter)+' '+IntToStr(tTargetEffectLoopCounter)+'/'+IntToStr(pActionTargetIDSize) +
        ' : SpecialEffect','0x'+IntToHex(tSpecialEffect,2)+' - '+IntToStr(tSpecialEffect),
        2);
      Offset := Offset + 7 ;

      tKnockback := PD.GetBitsAtPos(Offset,3);
      AddSGRow(SG,(Offset div 8),' #' + IntToStr(pTargetCountLoopCounter)+' '+IntToStr(tTargetEffectLoopCounter)+'/'+IntToStr(pActionTargetIDSize) +
        ' : Knockback','0x'+IntToHex(tKnockback,2)+' - '+IntToStr(tKnockback),
        1);
      Offset := Offset + 3 ;

      tParam := PD.GetBitsAtPos(Offset,17);
      AddSGRow(SG,(Offset div 8),' #' + IntToStr(pTargetCountLoopCounter)+' '+IntToStr(tTargetEffectLoopCounter)+'/'+IntToStr(pActionTargetIDSize) +
        ' : Param','0x'+IntToHex(tParam,3)+' - '+IntToStr(tParam),
        3);
      Offset := Offset + 17 ;

      tMessageID := PD.GetBitsAtPos(Offset,10);
      AddSGRow(SG,(Offset div 8),' #' + IntToStr(pTargetCountLoopCounter)+' '+IntToStr(tTargetEffectLoopCounter)+'/'+IntToStr(pActionTargetIDSize) +
        ' : MessageID','0x'+IntToHex(tMessageID,3)+' - '+IntToStr(tMessageID),
        2);
      Offset := Offset + 10 ;

      tUnknown := PD.GetBitsAtPos(Offset,31);
      AddSGRow(SG,(Offset div 8),' #' + IntToStr(pTargetCountLoopCounter)+' '+IntToStr(tTargetEffectLoopCounter)+'/'+IntToStr(pActionTargetIDSize) +
        ' : ??? 31bits','0x'+IntToHex(tUnknown,8)+' - '+IntToStr(tUnknown),
        2);
      Offset := Offset + 31 ;

      // Has additional effect ?
      If (PD.GetBitsAtPos(Offset,1) <> 0) Then
      Begin
        // Yes
        Offset := Offset + 1 ;

        tAdditionalEffect := PD.GetBitsAtPos(Offset,10);
        AddSGRow(SG,(Offset div 8),' #' + IntToStr(pTargetCountLoopCounter)+' '+IntToStr(tTargetEffectLoopCounter)+'/'+IntToStr(pActionTargetIDSize) +
          ' : Added Effect','0x'+IntToHex(tAdditionalEffect,2)+' - '+IntToStr(tAdditionalEffect),
          2);
        Offset := Offset + 10 ;

        tAddEffectParam := PD.GetBitsAtPos(Offset,17);
        AddSGRow(SG,(Offset div 8),' #' + IntToStr(pTargetCountLoopCounter)+' '+IntToStr(tTargetEffectLoopCounter)+'/'+IntToStr(pActionTargetIDSize) +
          ' : Effect Param','0x'+IntToHex(tAddEffectParam,5)+' - '+IntToStr(tAddEffectParam),
          3);
        Offset := Offset + 17 ;

        tAddEffectMessage := PD.GetBitsAtPos(Offset,10);
        AddSGRow(SG,(Offset div 8),' #' + IntToStr(pTargetCountLoopCounter)+' '+IntToStr(tTargetEffectLoopCounter)+'/'+IntToStr(pActionTargetIDSize) +
          ' : Effect Msg','0x'+IntToHex(tAddEffectMessage,2)+' - '+IntToStr(tAddEffectMessage),
          2);
        Offset := Offset + 10 ;
      End Else
      Begin
        // No ? Let's just go the next bit
        AddSGRow(SG,(Offset div 8),' #' + IntToStr(pTargetCountLoopCounter)+' '+IntToStr(tTargetEffectLoopCounter)+'/'+IntToStr(pActionTargetIDSize) +
          ' : Added Effect','NO',
          1);
        Offset := Offset + 1 ;
      End;

      // Has spike effect ?
      If (PD.GetBitsAtPos(Offset,1) <> 0) Then
      Begin
        // Yes
        Offset := Offset + 1 ;

        tAdditionalEffect := PD.GetBitsAtPos(Offset,10);
        AddSGRow(SG,(Offset div 8),' #' + IntToStr(pTargetCountLoopCounter)+' '+IntToStr(tTargetEffectLoopCounter)+'/'+IntToStr(pActionTargetIDSize) +
          ' : Spike Effect','0x'+IntToHex(tAdditionalEffect,2)+' - '+IntToStr(tAdditionalEffect),
          2);
        Offset := Offset + 10 ;

        tAddEffectParam := PD.GetBitsAtPos(Offset,14);
        AddSGRow(SG,(Offset div 8),' #' + IntToStr(pTargetCountLoopCounter)+' '+IntToStr(tTargetEffectLoopCounter)+'/'+IntToStr(pActionTargetIDSize) +
          ' : Spike Param','0x'+IntToHex(tAddEffectParam,4)+' - '+IntToStr(tAddEffectParam),
          2);
        Offset := Offset + 14 ;

        tAddEffectMessage := PD.GetBitsAtPos(Offset,10);
        AddSGRow(SG,(Offset div 8),' #' + IntToStr(pTargetCountLoopCounter)+' '+IntToStr(tTargetEffectLoopCounter)+'/'+IntToStr(pActionTargetIDSize) +
          ' : Spike Msg','0x'+IntToHex(tAddEffectMessage,2)+' - '+IntToStr(tAddEffectMessage),
          2);
        Offset := Offset + 10 ;
      End Else
      Begin
        // No ? Let's just go the next bit
        AddSGRow(SG,(Offset div 8),' #' + IntToStr(pTargetCountLoopCounter)+' '+IntToStr(tTargetEffectLoopCounter)+'/'+IntToStr(pActionTargetIDSize) +
          ' : Spikes Effect','NO',
          1);
        Offset := Offset + 1 ;
      End;






    End; // tTargetEffectLoopCounter

  End; // pTargetCountLoopCounter



  If (Offset mod 8) > 0 Then Result := (Offset div 8) + 1 else Result := Offset div 8 ;
End;


Function AddPacketInfoToStringGrid(PD : TPacketData;SG : TStringGrid;var BlockName : String):Boolean;
VAR
  FN : String ;
  SL, Line : TStringList ;
  LastPos, DataSize, P : Integer ;
  I, N, C, FoundCount, LOffset , LSubOffset, SwitchVal, LSizeOffset : Integer ;
  LType, LOffsetStr, LSubOffsetStr, LRangeOffsetStr, LName, LDescription : String ;
  S, CurrentBlock : String ;
  AllowAutoSwitchBlock : Boolean ;
  DT : TDateTime ;
  ColIndex : Integer ;
  T : String ;
Begin
  Result := True ;
  ColIndex := 0 ;
  AllowAutoSwitchBlock := False ;
  If (BlockName = '-') Then
  Begin
    AllowAutoSwitchBlock := True ;
    BlockName := '' ;
  End;
  FN := 'parse\' ;
  If (PD.PacketLogType = pltIn) Then FN := FN + 'in-' ;
  If (PD.PacketLogType = pltOut) Then FN := FN + 'out-' ;
  FN := FN + '0x' + IntToHex(PD.PacketID,3) + '.txt' ;
  LastPos := 4 ;
  CurrentBlock := '' ;
  Try
    SL := TStringList.Create ;
    Line := TStringList.Create;
    // TODO: Cache the files ?
    If FileExists(FN) Then
      SL.LoadFromFile(FN)
    Else
      SL.Clear ;
    // Parse all the stuff !
    AvailableBlocks.Clear;

    For I := 0 To SL.Count-1 Do
    Begin
      // loop all the lines

      If (Copy(SL[I],1,2) = '[[') THen
      Begin
        CurrentBlock := Copy(SL[I],3,Length(SL[I])-4);
        // AddSGRow(SG,0,'ADDBLOCK',CurrentBlock );
        If (CurrentBlock <> '') Then
            AvailableBlocks.Add(CurrentBlock);
      End;

      // Skip if we aren't in the current blockname
      If (BlockName <> CurrentBlock) and (CurrentBlock <> '') Then Continue ;
      // If (BlockName <> CurrentBlock) and (CurrentBlock <> '') and (BlockName <> '') Then Continue ;

      Split(';',SL[I],Line);
      If (Line.Count >= 2) Then
      Begin
        // We want a  type:offset[:name[:description]] format
        LType := LowerCase(Line[0]);
        LOffsetStr := LowerCase(Line[1]);
        LSubOffsetStr := '0' ;
        LRangeOffsetStr := '0' ;
        LOffset := 4 ;
        If (Line.Count > 2) Then LName := Line[2] else LName := '' ;
        If (Line.Count > 3) Then LDescription := Line[3] else LDescription := '???' ; // we're not actually using this (yet)

        // Find SubOffset Value (if any)
        P := Pos(':',LOffsetStr);
        If (P > 0) Then
        Begin
          LSubOffsetStr := Copy(LOffsetStr,P+1,Length(LOffsetStr));
          LOffsetStr := Copy(LOffsetStr,1,P-1);
        End;
        // Find RangeOffset Value (if any)
        P := Pos('-',LSubOffsetStr);
        If (P > 0) Then
        Begin
          LRangeOffsetStr := Copy(LSubOffsetStr,P+1,Length(LSubOffsetStr));
          LSubOffsetStr := Copy(LSubOffsetStr,1,P-1);
        End;

        If (TryStrToInt(LOffsetStr,LOffset) = False) Then
        Begin
          // Invalid main offset value, skip this line
          Continue;
        End;
        If (TryStrToInt(LSubOffsetStr,LSubOffset) = False) Then
        Begin
          // Invalid suboffset value, default it to 0
          LSubOffset := 0 ;
        End;
        If (TryStrToInt(LRangeOffsetStr,LSizeOffset) = False) Then
        Begin
          // Invalid rangeoffset value, default it to 0
          LSizeOffset := 0 ;
        End;

        If (LName = '') Then
        Begin
          LName := LType + ' @ 0x' + IntToHex(LOffset,2) + ' ' + IntToStr(LOffset);
        End;

        If ((LType = 'file') or (LType = 'rem') or (LType = 'comment')) Then
        Begin
          // It's just a comment or header, ignore ...
        End Else

        If (LType = 'info') Then
        Begin
          ColIndex := ColIndex + 1 ;
          // Just displays the comment field of this line
          AddSGRow(SG,LOffset,LName,LDescription,0);
        End Else

        If (LType = 'switchblock') Then
        Begin
          If (AllowAutoSwitchBlock) Then
          Begin
            // switchblock;checkpos;checkval;blockname
            // Compares BYTE value at checkpos, if checkval matches, activate blockname as current block

            // enable auto-switching of freashly loaded blocks
            If (TryStrToInt(LName,SwitchVal) = True) Then
            Begin
              // OK switchval seems valid, next compare it
              If PD.GetByteAtPos(LOffset) = SwitchVal Then
              Begin
                BlockName := LDescription ;
                AllowAutoSwitchBlock := False ;
                Continue;
              End;
            End;
          End;
        End Else

        If ((LType = 'switchbitsblock') or (LType = 'bitsswitchblock')) Then
        Begin
          If (AllowAutoSwitchBlock) Then
          Begin
            // switchblock;checkpos:bitoffset-bitsize;checkval;blockname
            // Compares BYTE value at checkpos, if checkval matches, activate blockname as current block

            // enable auto-switching of freashly loaded blocks
            If (TryStrToInt(LName,SwitchVal) = True) Then
            Begin
              // OK switchval seems valid, next compare it
              If PD.GetBitsAtPos(LOffset,LSubOffset,LSizeOffset) = SwitchVal Then
              Begin
                BlockName := LDescription ;
                AllowAutoSwitchBlock := False ;
                Continue;
              End;
            End;
          End;
        End Else

        If ((LType = 'byte') or (LType = 'char') or (LType = 'b') or (LType = 'unsigned char')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 1 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,1,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,'0x' + IntToHex(PD.GetByteAtPos(LOffset),2) + '  ' + IntToStr(PD.GetByteAtPos(LOffset)));
        End Else
        If ((LType = 'byteflag') or (LType = 'flag')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 1 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,1,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName, '0x' + IntToHex(PD.GetByteAtPos(LOffset),2) + '  ' + BytetoBit(PD.GetByteAtPos(LOffset)) + '  ' + IntToStr(PD.GetByteAtPos(LOffset)) );
        End Else
        If ((LType = 'bit') or (LType = 'bool')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 1 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,1,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName, BoolToStr( PD.GetBitAtPos(LOffset,LSubOffset),True ));
        End Else
        If ((LType = 'bits') or (LType = 'bitrange')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 1 ;
          If (LSizeOffset <= 0) Then LSizeOffset := 1 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,(LSizeOffset div 8)+1,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName, '0x' + IntToHex( PD.GetBitsAtPos(LOffset,LSubOffset,LSizeOffset),2 ) + ' - ' + IntToStr( PD.GetBitsAtPos(LOffset,LSubOffset,LSizeOffset) ) , (LSizeOffset div 8)+1 );
        End Else
        If ((LType = 'word') or (LType = 'uint16') or (LType = 'ushort') or (LType = 'w') or (LType = 'unsigned short') ) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,2,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,'0x' + IntToHex(PD.GetWordAtPos(LOffset),4) + '  ' + IntToStr(PD.GetWordAtPos(LOffset)),2);
        End Else
        If ((LType = 'int16') or (LType = 'short')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,2,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,'0x' + IntToHex(PD.GetInt16AtPos(LOffset),4) + '  ' + IntToStr( PD.GetInt16AtPos(LOffset)),2);
        End Else
        If ((LType = 'int') or (LType = 'int32') or (LType = 'i')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 4 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,4,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,'0x' + IntToHex(PD.GetInt32AtPos(LOffset),8) + '  ' + IntToStr(PD.GetInt32AtPos(LOffset)),4);
        End Else
        If ((LType = 'dword') or (LType = 'long') or (LType = 'longword') or (LType = 'uint32') or (LType = 'u') or (LType = 'unsigned int') ) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 4 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,4,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,'0x' + IntToHex(PD.GetUInt32AtPos(LOffset),8) + '  ' + IntToStr(PD.GetUInt32AtPos(LOffset)),4);
        End Else
        If ((LType = 'float') or (LType = 'f')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 4 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,4,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,IntToStr(PD.GetUInt32AtPos(LOffset)),4);

        End Else
        // Special types
        If (LType = 'pos') Then
        Begin
          // Position coordinates, float x3
          If (LOffset >= LastPos) Then LastPos := LOffset + 12 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,12,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,
            'X:'  + FormatFloat('0.00',PD.GetFloatAtPos(LOffset)) +
            ' Y:' + FormatFloat('0.00',PD.GetFloatAtPos(LOffset+4)) +
            ' Z:' + FormatFloat('0.00',PD.GetFloatAtPos(LOffset+8)),
            12);

        End Else

        If (LType = 'dir') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 1 ;
          // Direction as byte
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,1,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,ByteToRotation(PD.GetByteAtPos(LOffset)),1);

        End Else

        If (LType = 'ms') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 4 ;
          // Milliseconds
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,4,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,MSToStr(PD.GetUInt32AtPos(LOffset)),4);

        End Else

        If (LType = 'timestamp') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 4 ;
          // Milliseconds
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,4,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,FFXITimeStampToStr(PD.GetUInt32AtPos(LOffset)),4);

        End Else

        If (LType = 'vanatime') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 4 ;
          // date/time
          // DT := EncodeDate(1970,1,1) + PD.GetUInt32AtPos(LOffset) / 86400 ;
          // AddSGRow(SG,LOffset,LName,FormatDateTime('yyyy/mm/dd hh:nn:ss',DT));
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,4,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName, DWordToVanaTime( PD.GetUInt32AtPos(LOffset) ) , 4);
        End Else

        If ((LType = 'frames') or (LType = 'frame')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 4 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,4,DataCol(ColIndex));
          // Milliseconds
          AddSGRow(SG,LOffset,LName,FramesToStr(PD.GetUInt32AtPos(LOffset)) , 4);

        End Else

        If (Copy(LType,1,6) = 'string') Then
        Begin
          // Zero terminated String
          ColIndex := ColIndex + 1 ;
          DataSize := StrToIntDef(Copy(LType,7,Length(LType)),-1);
          If Assigned(UpdateActiveRE) Then
            MarkREBytes(UpdateActiveRE,LOffset,Length(PD.GetStringAtPos(LOffset,DataSize))+1,DataCol(ColIndex));

          AddSGRow(SG,LOffset,LName,PD.GetStringAtPos(LOffset,DataSize),Length(PD.GetStringAtPos(LOffset,DataSize))+1);
          If (LOffset >= LastPos) Then LastPos := LOffset + Length(PD.GetStringAtPos(LOffset))+1 ;

        End Else


        If (LType = 'linkshellstring') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 16 ;

          // Zero terminated String
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then
            MarkREBytes(UpdateActiveRE,LOffset,15,DataCol(ColIndex));

          AddSGRow(SG,LOffset,LName,PD.GetPackedString16AtPos(LOffset,EncodeLinkshellStr), 15);
        End Else

        If (LType = 'inscribestring') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 16 ;

          // Zero terminated String
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then
            MarkREBytes(UpdateActiveRE,LOffset,15,DataCol(ColIndex));

          AddSGRow(SG,LOffset,LName,PD.GetPackedString16AtPos(LOffset,EncodeItemStr), 15);
        End Else

        If (Copy(LType,1,4) = 'data') Then
        Begin
          // simple byte data
          ColIndex := ColIndex + 1 ;
          DataSize := StrToIntDef(Copy(LType,5,Length(LType)),1);
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,DataSize,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,PD.GetDataAtPos(LOffset,DataSize),DataSize);
          If (LOffset >= LastPos) Then LastPos := LOffset + DataSize ;

        End Else

        If ((LType = 'ip4') or (LType = 'ip')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 4 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,4,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,'0x' + IntToHex(PD.GetUInt32AtPos(LOffset),8) + ' => ' + PD.GetIP4AtPos(LOffset),4);
        End Else

        // Specialized items

        If ((LType = 'equipsetitem') or (LType = 'equipsetbuild')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 4 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,4,DataCol(ColIndex));
          // Slot
          T := '' ;
          If PD.GetBitAtPos(LOffset,0) Then T := T + 'Active ' ;
          If PD.GetBitAtPos(LOffset,1) Then T := T + 'Bit1Set? ' ;
          T := T + 'Bag: ' + NLU(LU_Container).GetVal(PD.GetBitsAtPos(LOffset,2,6))+' ' ;
          T := T + 'InvIndex: ' + IntToStr(PD.GetByteAtPos(LOffset+1)) +' ' ;
          T := T + 'Item: ' + NLU(LU_Item).GetVal(PD.GetWordAtPos(LOffset+2)) ;
          AddSGRow(SG,LOffset,LName,'0x'+IntToHex(PD.GetUInt32AtPos(LOffset),8) + ' => ' + T,1);

        End Else

        If ((LType = 'equipsetitemlist') or (LType = 'equipsetbuildlist')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 4 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,4,DataCol(ColIndex));

          If (LSubOffset <= 0) then LSubOffset := PD.GetByteAtPos($04);

          C := 0 ;
          N := LOffset ;
          While (C < LSubOffset) and (N <= PD.RawSize-4) do
          Begin
            T := '' ;
            If PD.GetBitAtPos(N,0) Then T := T + 'Active ' ;
            If PD.GetBitAtPos(N,1) Then T := T + 'Bit1Set? ' ;
            T := T + 'Bag: ' + NLU(LU_Container).GetVal(PD.GetBitsAtPos(N,2,6))+' ' ;
            T := T + 'InvIndex: ' + IntToStr(PD.GetByteAtPos(N+1)) +' ' ;
            T := T + 'Item: ' + NLU(LU_Item).GetVal(PD.GetWordAtPos(N+2)) ;
            AddSGRow(SG,LOffset,LName+' #'+IntToStr(C),'0x'+IntToHex(PD.GetUInt32AtPos(N),8) + ' => ' + T,1);
            C := C + 1 ;
            N := N + 4 ;
          End;
        End Else

        If (LType = 'abilityrecastlist') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 8 ;

          If (LSubOffset <= 0) then LSubOffset := 0 ;

          C := 0 ;
          N := LOffset ;
          While ((C < LSubOffset) or (LSubOffset = 0)) and (N <= PD.RawSize-8) do
          Begin

            ColIndex := ColIndex + 1 ;
            If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,N,8,DataCol(ColIndex));

            T := '' ;
            T := T + 'ID: 0x' + IntToHex(PD.GetByteAtPos(N+3),2) +' ('+ NLU(LU_ARecast).GetVal(PD.GetByteAtPos(N+3)) +') ' ;
            T := T + 'Duration: ' + IntToStr(PD.GetWordAtPos(N))+'  - ' ;
            T := T + 'byte@2: 0x' + IntToHex(PD.GetByteAtPos(N+2),2) +' ' ;
            T := T + 'uint32@4: 0x' + IntToHex(PD.GetUInt32AtPos(N+4),8) + ' ' ;

            If (PD.GetByteAtPos(N+3) <> 0) or (C = 0) Then
              AddSGRow(SG,N,LName+' #'+IntToStr(C),T,8); // Only show used recasts

            C := C + 1 ;
            N := N + 8 ;
          End;
          LastPos := N-1 ;

        End Else


        If ((LType = 'equipslot') or (LType = 'slot')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 1 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,1,DataCol(ColIndex));
          // Slot
          AddSGRow(SG,LOffset,LName,EquipmentSlotName(PD.GetByteAtPos(LOffset)),1);
        End Else

        If ((LType = 'container') or (LType = 'inventory') or (LType = 'bag')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 1 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,1,DataCol(ColIndex));
          // Inventory Bag
          AddSGRow(SG,LOffset,LName,ContainerName(PD.GetByteAtPos(LOffset)),1);

        End Else

        If ((LType = 'zone') or (LType = 'map')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,2,DataCol(ColIndex));
          // Inventory Bag
          AddSGRow(SG,LOffset,LName,IntToStr(PD.GetWordAtPos(LOffset)) + ' => ' + NLU(LU_Zones).GetVal(PD.GetWordAtPos(LOffset)),2);

        End Else

        If (LType = 'job') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 1 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,1,DataCol(ColIndex));
          // Job
          AddSGRow(SG,LOffset,LName,IntToStr(PD.GetByteAtPos(LOffset)) + ' => ' + NLU(LU_Job).GetVal(PD.GetByteAtPos(LOffset)),1);

        End Else

        If (LType = 'jobflags') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 4 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,4,DataCol(ColIndex));
          // Job unlock flags
          AddSGRow(SG,LOffset,LName,'0x'+IntToHex(PD.GetUint32AtPos(LOffset),8) + ' => ' + PD.GetJobflagsAtPos(LOffset),4);
        End Else

        If (LType = 'partymemberflag') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,2,DataCol(ColIndex));
          // Party Member Flags
          C := PD.GetWordAtPos(LOffset);
          T := '' ;
          If ((C and $0001) <> 0) Then T := T + '2nd_Party ' ;
          If ((C and $0002) <> 0) Then T := T + '3nd_Party ' ;
          If ((C and $0004) <> 0) Then T := T + 'Party_Leader ' ;
          If ((C and $0008) <> 0) Then T := T + 'Alliance_Leader ' ;
          If ((C and $0010) <> 0) Then T := T + 'Quartermaster ' ;
          If ((C and $0020) <> 0) Then T := T + 'Flag0x0020 ' ;
          If ((C and $0040) <> 0) Then T := T + 'Flag0x0040 ' ;
          If ((C and $0080) <> 0) Then T := T + 'Flag0x0080 ' ;
          If ((C and $0100) <> 0) Then T := T + 'LevelSync ' ;
          If ((C and $0200) <> 0) Then T := T + 'Flag0x0200 ' ;
          If ((C and $0400) <> 0) Then T := T + 'Flag0x0400 ' ;
          If ((C and $0800) <> 0) Then T := T + 'Flag0x0800 ' ;
          If ((C and $1000) <> 0) Then T := T + 'Flag0x1000 ' ;
          If ((C and $2000) <> 0) Then T := T + 'Flag0x2000 ' ;
          If ((C and $4000) <> 0) Then T := T + 'Flag0x4000 ' ;
          If ((C and $8000) <> 0) Then T := T + 'Flag0x8000 ' ;
          If (T = '') Then T := 'None' ;

          AddSGRow(SG,LOffset,LName,'0x'+IntToHex(C,4) + ' => ' + T,2);
        End Else

        If (LType = 'blacklistentry') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 20 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,20,DataCol(ColIndex));
          // Blacklist entry
          AddSGRow(SG,LOffset,LName,'ID: 0x'+IntToHex(PD.GetUint32AtPos(LOffset),8) + ' => ' + PD.GetStringAtPos(LOffset+4,16),20);
        End Else

        If (LType = 'meritentries') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset ;

          N := LOffset ;
          C := 0 ;

          // Subvalue is the adress for the counter to use
          If LSubOffset > 1 Then LSubOffset := PD.GetByteAtPos(LSubOffset);

          While (N < PD.RawSize-4) and (C < LSubOffset) Do
          Begin
            C := C + 1 ;

            ColIndex := ColIndex + 1 ;
            If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,4,DataCol(ColIndex));

            AddSGRow(SG,N,LName + ' #'+IntToStr(C),
              'ID: 0x'+IntToHex(PD.GetWordAtPos(N),4)+ '  ' + NLU(LU_Merit).GetVal(PD.GetWordAtPos(N)) + ' - ' +
              'Next Cost: '+IntToStr(PD.GetByteAtPos(N+2)) + ' - ' +
              'Value: '+IntToStr(PD.GetByteAtPos(N+3)) ,4);

            N := N + 4 ;
            If (N >= LastPos) Then LastPos := N ;
          End;
        End Else

        If (LType = 'playercheckitems') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset ;

          N := LOffset ;
          C := 0 ;

          // Subvalue is the adress for the counter to use, defaults to previous byte
          If LSubOffset <= 1 Then LSubOffset := PD.GetByteAtPos(LOffset-1);

          While (N < PD.RawSize-$1C) and (C < LSubOffset) Do
          Begin
            C := C + 1 ;

            ColIndex := ColIndex + 1 ;
            If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,$1C,DataCol(ColIndex));

            AddSGRow(SG,N,LName + ' #'+IntToStr(C)+' ID',
              '0x'+IntToHex(PD.GetWordAtPos(N),4)+ '  ' + NLU(LU_Item).GetVal(PD.GetWordAtPos(N)),
              2);
            AddSGRow(SG,N+2,LName + ' #'+IntToStr(C)+' Slot',
              '0x'+IntToHex(PD.GetByteAtPos(N+2),2)+ '  ' + NLU(LU_EquipmentSlots).GetVal(PD.GetByteAtPos(N+2)),
              1);
            AddSGRow(SG,N+3,LName + ' #'+IntToStr(C)+' ???',
              '0x'+IntToHex(PD.GetByteAtPos(N+3),2)+ '  ' + IntToStr(PD.GetByteAtPos(N+3)),
              1);
            AddSGRow(SG,N+4,LName + ' #'+IntToStr(C)+' ExtData',
              PD.GetDataAtPos(N+4,24),
              24);

            N := N + $1C ;
            If (N >= LastPos) Then LastPos := N ;
          End;
        End Else

        If (LType = 'bufficons') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset ;

          N := LOffset ;
          C := 0 ;

          // Subvalue is count to use
          If LSubOffset <= 1 Then LSubOffset := 1 ;

          While (N < PD.RawSize-2) and (C < LSubOffset) Do
          Begin
            C := C + 1 ;

            ColIndex := ColIndex + 1 ;
            If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,N,2,DataCol(ColIndex));

            AddSGRow(SG,N,LName + ' #'+IntToStr(C),
              '0x'+IntToHex(PD.GetWordAtPos(N),4)+ '  ' + NLU('buffs').GetVal(PD.GetWordAtPos(N)),
              2);

            N := N + 2 ;
            If (N >= LastPos) Then LastPos := N ;
          End;
        End Else

        If (LType = 'bufftimers') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset ;

          N := LOffset ;
          C := 0 ;

          // Subvalue is count to use
          If LSubOffset <= 1 Then LSubOffset := 1 ;

          While (N < PD.RawSize-4) and (C < LSubOffset) Do
          Begin
            C := C + 1 ;

            ColIndex := ColIndex + 1 ;
            If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,N,4,DataCol(ColIndex));

            AddSGRow(SG,N,LName + ' #'+IntToStr(C),
              '0x'+IntToHex(PD.GetInt32AtPos(N),8)+ ' - ' + MSToStr(PD.GetInt32AtPos(N)),
              4);

            N := N + 4 ;
            If (N >= LastPos) Then LastPos := N ;
          End;
        End Else

        If (LType = 'buffs') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset ;

          // Subvalue is count to use
          If LSubOffset <= 1 Then LSubOffset := 1 ;
          If LSizeOffset <= 1 Then LSizeOffset := 1 ;

          N := LOffset ;
          C := 0 ;

          While (N + (LSizeOffset*2) < PD.RawSize-4) and (C < LSizeOffset) Do
          Begin
            ColIndex := ColIndex + 1 ;
            If Assigned(UpdateActiveRE) Then
            Begin
              MarkREBytes(UpdateActiveRE,N,2,DataCol(ColIndex));
              MarkREBytes(UpdateActiveRE,LOffset + (LSizeOffset*2) + (C * 4),4,DataCol(ColIndex));
            End;

            AddSGRow(SG,N,LName + ' #'+IntToStr(C),
              '0x'+IntToHex(PD.GetWordAtPos(N),4)+ ' ('+IntToStr(PD.GetWordAtPos(N))+') => ' + NLU('buffs').GetVal(PD.GetWordAtPos(N)) +
              '  - EndTime: 0x'+IntToHex(PD.GetUInt32AtPos(LOffset + (LSizeOffset*2) + (C * 4)),8) + '  ' + FFXITimeStampToStr(PD.GetInt32AtPos(LOffset + (LSizeOffset*2) + (C * 4)))
              ,2);

            N := N + 2 ;
            C := C + 1 ;
          End;
          LastPos := LOffset + (LSizeOffset*6) + 4 ;
        End Else



        If (LType = 'jobpointentries') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset ;

          N := LOffset ;
          C := 0 ;

          While (N < PD.RawSize-4) Do
          Begin
            C := C + 1 ;

            ColIndex := ColIndex + 1 ;
            If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset+C-1,4,DataCol(ColIndex));

            AddSGRow(SG,N,LName + ' #'+IntToStr(C),
              'ID: 0x'+IntToHex(PD.GetWordAtPos(N),4)+ '  ' + NLU(LU_JobPoint).GetVal(PD.GetWordAtPos(N)) + ' - ' +
              'Level: '+IntToStr(PD.GetBitsAtPos(N+3,2)) + ' - ' +
              '???: '+IntToStr(PD.GetBitsAtPos(N+2,10)) ,1);

            N := N + 4 ;
            If (N >= LastPos) Then LastPos := N ;
          End;
        End Else

        If (LType = 'bitflaglist') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset ;

          // Subvalue amount of bits to use, default to 64, just because
          If LSubOffset <= 0 Then LSubOffset := 64 ;

          // Description will be the lookup filename for every field

          N := LOffset ;
          C := -1 ;

          FoundCount := 0 ;
          While (N < PD.RawSize) and (C < LSubOffset) Do
          Begin
            C := C + 1 ;


            If PD.GetBitsAtPos(LOffset+(C div 8),C mod 8,1) > 0 Then
            Begin
              FoundCount := FoundCount + 1 ;

              ColIndex := ColIndex + 1 ;
              If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,N,1,DataCol(ColIndex));

              If NLU(LDescription).GetVal(C) <> '' Then
                AddSGRow(SG,N,LName + ' 0x'+IntToHex(C,4),'Name: ' + NLU(LDescription).GetVal(C) ,1)
              Else
                AddSGRow(SG,N,LName + ' 0x'+IntToHex(C,4),'???',1);
            End;

            If (C mod 8) = 7 Then N := N + 1 ;
            If (N >= LastPos) Then LastPos := N ;
          End;
          If FoundCount <= 0 Then
          Begin
            ColIndex := ColIndex + 1 ;
            If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,LSubOffset div 8,DataCol(ColIndex));

            AddSGRow(SG,LOffset,LName,'No bits set',LSubOffset div 8);
          End;
        End Else


        If (LType = 'shopitems') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset ;

          N := LOffset ;
          C := 0 ;
          While (N < PD.RawSize-1) Do
          Begin
            C := C + 1 ;

            ColIndex := ColIndex + 1 ;
            If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,$C,DataCol(ColIndex));


            AddSGRow(SG,N,LName,
              '#'+IntToStr(C)+ ': ' +
              'Slot?: 0x'+IntToHex(PD.GetWordAtPos(N+6),4)+ ' - ' +
              NLU(LU_Item).GetVal(PD.GetWordAtPos(N+4)) +
              ' => Gil: '+IntToStr(PD.GetUInt32AtPos(N)) +
              ' Skill: 0x'+IntToHex(PD.GetWordAtPos(N+8),4)+
              ' Rank: 0x'+IntToHex(PD.GetWordAtPos(N+$A),4) ,14);

            N := N + $C ;
            If (N >= LastPos) Then LastPos := N ;
          End;
        End Else

        If (LType = 'guildshopitems') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset ;

          For C := 0 to 29 Do
          Begin
            N := LOffset + (C * 8);

            ColIndex := ColIndex + 1 ;
            If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,8,DataCol(ColIndex));

            AddSGRow(SG,N,LName,
              '#'+IntToStr(C+1)+ ': ' +
              'Item: 0x'+IntToHex(PD.GetWordAtPos(N),4)+ ' - ' +
              NLU(LU_Item).GetVal(PD.GetWordAtPos(N)) +
              ' => Price: '+IntToStr(PD.GetUInt32AtPos(N+4)) +
              ' - Stock: '+IntToStr(PD.GetByteAtPos(N+2)) + ' / ' + IntToStr(PD.GetWordAtPos(N+3)) ,8);

            If (N+8 >= LastPos) Then LastPos := N + 8 ;
          End;
        End Else

        If ((LType = 'item') or (LType = 'itemid')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,2,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,'0x' + IntToHex(PD.GetWordAtPos(LOffset),4) + '  ' + IntToStr(PD.GetWordAtPos(LOffset)) + ' ' + NLU(LU_Item).GetVal(PD.GetWordAtPos(LOffset)) ,2);
        End Else

        If (LType = 'item-head') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,2,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,'0x' + IntToHex(PD.GetWordAtPos(LOffset),4) + ' => ' + IntToStr(PD.GetWordAtPos(LOffset) - $1000) + ' ' + NLU(LU_ItemModel).GetVal(PD.GetWordAtPos(LOffset)) ,2);
        End Else

        If (LType = 'item-body') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,2,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,'0x' + IntToHex(PD.GetWordAtPos(LOffset),4) + ' => ' + IntToStr(PD.GetWordAtPos(LOffset) - $2000) + ' ' + NLU(LU_ItemModel).GetVal(PD.GetWordAtPos(LOffset)) ,2);
        End Else

        If (LType = 'item-hands') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,2,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,'0x' + IntToHex(PD.GetWordAtPos(LOffset),4) + ' => ' + IntToStr(PD.GetWordAtPos(LOffset) - $3000) + ' ' + NLU(LU_ItemModel).GetVal(PD.GetWordAtPos(LOffset)) ,2);
        End Else

        If (LType = 'item-legs') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,2,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,'0x' + IntToHex(PD.GetWordAtPos(LOffset),4) + ' => ' + IntToStr(PD.GetWordAtPos(LOffset) - $4000) + ' ' + NLU(LU_ItemModel).GetVal(PD.GetWordAtPos(LOffset)) ,2);
        End Else

        If (LType = 'item-feet') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,2,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,'0x' + IntToHex(PD.GetWordAtPos(LOffset),4) + ' => ' + IntToStr(PD.GetWordAtPos(LOffset) - $5000) + ' ' + NLU(LU_ItemModel).GetVal(PD.GetWordAtPos(LOffset)) ,2);
        End Else

        If (LType = 'item-main') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,2,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,'0x' + IntToHex(PD.GetWordAtPos(LOffset),4) + ' => ' + IntToStr(PD.GetWordAtPos(LOffset) - $6000) + ' ' + NLU(LU_ItemModel).GetVal(PD.GetWordAtPos(LOffset)) ,2);
        End Else

        If (LType = 'item-sub') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,2,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,'0x' + IntToHex(PD.GetWordAtPos(LOffset),4) + ' => ' + IntToStr(PD.GetWordAtPos(LOffset) - $7000) + ' ' + NLU(LU_ItemModel).GetVal(PD.GetWordAtPos(LOffset)) ,2);
        End Else

        If ((LType = 'item-ranged') or (LType = 'item-range')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,2,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,'0x' + IntToHex(PD.GetWordAtPos(LOffset),4) + ' => ' + IntToStr(PD.GetWordAtPos(LOffset) - $8000) + ' ' + NLU(LU_ItemModel).GetVal(PD.GetWordAtPos(LOffset)) ,2);
        End Else

        If ((LType = 'music') or (LType = 'bgm')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,2,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,'0x' + IntToHex(PD.GetWordAtPos(LOffset),4) + '  ' + IntToStr(PD.GetWordAtPos(LOffset)) + ' ' + NLU(LU_Music).GetVal(PD.GetWordAtPos(LOffset)) ,2);
        End Else

        If (LType = 'weather') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,2,DataCol(ColIndex));
          // Weather
          AddSGRow(SG,LOffset,LName,IntToStr(PD.GetWordAtPos(LOffset)) + ' => ' + NLU(LU_Weather).GetVal(PD.GetWordAtPos(LOffset)),2);

        End Else

        If (LType = 'nation') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 1 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,1,DataCol(ColIndex));
          Case PD.GetByteAtPos(LOffset) Of
            0 : S := 'San d''Oria' ;
            1 : S := 'Bastok' ;
            2 : S := 'Windurst' ;
            3 : S := 'Jeuno?' ;
          Else
            S := 'Nation? ' + IntToStr(PD.GetByteAtPos(LOffset))
          End;
          AddSGRow(SG,LOffset,LName,'0x' + IntToHex(PD.GetByteAtPos(LOffset),2) + ' => ' + S);
        End Else

        If (LType = 'unity') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 1 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,1,DataCol(ColIndex));
          Case PD.GetByteAtPos(LOffset) Of
            0 : S := 'None' ;
            1 : S := 'Pieuje' ;
            2 : S := 'Ayame' ;
            3 : S := 'Invincible Shield' ;
            4 : S := 'Apururu' ;
            5 : S := 'Maat' ;
            6 : S := 'Aldo' ;
            7 : S := 'Jakoh Wahcondalo' ;
            8 : S := 'Naja Salaheem' ;
            9 : S := 'Flavira' ;
            10: S := 'Sylvie' ;
            11: S := 'Yoran-Oran' ;
          Else
            S := 'Unity? ' + IntToStr(PD.GetByteAtPos(LOffset))
          End;
          AddSGRow(SG,LOffset,LName,'0x' + IntToHex(PD.GetByteAtPos(LOffset),2) + ' => ' + S);
        End Else

        If (LType = 'combatskill') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,2,DataCol(ColIndex));
          // Combat Skill
          If ((PD.GetWordAtPos(LOffset) and $8000) <> 0) Then
            AddSGRow(SG,LOffset,LName,'0x'+IntToHex(PD.GetWordAtPos(LOffset),4) + ' => ' + IntToStr(PD.GetWordAtPos(LOffset) mod $8000) + ' (Capped)',2)
          Else
            AddSGRow(SG,LOffset,LName,'0x'+IntToHex(PD.GetWordAtPos(LOffset),4) + ' => ' + IntToStr(PD.GetWordAtPos(LOffset) mod $8000),2);
        End Else

        If (LType = 'craftskill') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,2,DataCol(ColIndex));
          // Combat Skill
          If ((PD.GetWordAtPos(LOffset) and $8000) <> 0) Then
            AddSGRow(SG,LOffset,LName,'0x'+IntToHex(PD.GetWordAtPos(LOffset),4) +
              ' => Rank:' + IntToStr(PD.GetWordAtPos(LOffset) and $001F) +
              ' Level:' + IntToStr( ((PD.GetWordAtPos(LOffset) shr 5) and $03FF) ) +
              ' (Capped)',2)
          Else
            AddSGRow(SG,LOffset,LName,'0x'+IntToHex(PD.GetWordAtPos(LOffset),4) +
              ' => Rank:' + IntToStr(PD.GetWordAtPos(LOffset) and $001F) +
              ' Level:' + IntToStr( ((PD.GetWordAtPos(LOffset) shr 5) and $03FF) ) ,2);
        End Else

        If (LType = 'jobpoints') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 6 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,6,DataCol(ColIndex));
          // Job unlock flags
          AddSGRow(SG,LOffset,LName,IntToStr(PD.GetWordAtPos(LOffset)) + ' CP   ' + IntToStr(PD.GetWordAtPos(LOffset+2)) + ' JP   ' + IntToStr(PD.GetWordAtPos(LOffset+4)) + ' Spent JP',6);
        End Else


        If (LType = 'roequest') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 4 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,4,DataCol(ColIndex));
          // Job unlock flags
          C := PD.GetBitsAtPos(LOffset,0,12);
          AddSGRow(SG,LOffset,LName,'ID: 0x'+ IntToHex(C,3) + ' => (' + NLU(LU_RoE).GetVal(C) + ')  Progress: ' + IntToStr(PD.GetBitsAtPos(LOffset+1,4,20)) + ' / ' + NLU(LU_RoE).GetExtra(C) ,4);
        End Else

        If (LType = 'packet-in-0x028') Then // Need to build a custom parser for this (atm)
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset ;
          LastPos := AddPacketInfoIn0x028(PD,SG);
          {
          While (N < PD.RawSize-1) Do
          Begin
            C := C + 1 ;

            ColIndex := ColIndex + 1 ;
            If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,$C,DataCol(ColIndex));


            AddSGRow(SG,N,LName,
              '#'+IntToStr(C)+ ': ' +
              'Slot?: 0x'+IntToHex(PD.GetWordAtPos(N+6),4)+ ' - ' +
              ItemNames.GetVal(PD.GetWordAtPos(N+4)) +
              ' => Gil: '+IntToStr(PD.GetUInt32AtPos(N)) +
              ' Skill: 0x'+IntToHex(PD.GetWordAtPos(N+8),4)+
              ' Rank: 0x'+IntToHex(PD.GetWordAtPos(N+$A),4) ,14);

            N := N + $C ;
            If (N >= LastPos) Then LastPos := N ;
          End;
          }
        End Else



        Begin
          // this is a what now ?
          If (LOffset >= LastPos) Then LastPos := LOffset + 0 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,1,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,'unknown type ' + LType );
        End;

      End; // end linecount >= 2

    End; // end for lines

    I := LastPos ;
    While I < PD.PacketDataSize Do
    Begin
      ColIndex := ColIndex + 1 ;
      If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,I,1,DataCol(ColIndex));
      AddSGRow(SG,I,'?Byte 0x'+IntToHex(I,2)+' ' +IntToStr(I), '0x' + IntToHex(PD.GetByteAtPos(I),2) + '  ' + BytetoBit(PD.GetByteAtPos(I)) + '  ' + IntToStr(PD.GetByteAtPos(I)));
      I := I + 1 ;
    End;


  Except
    FreeAndNil(SL);
    FreeAndNil(Line);
    Result := False ;
    Exit ;
  End;
End;

Initialization
  AvailableBlocks := TStringList.Create ;
  UpdateActiveRE := nil ;

Finalization
  FreeAndNil(AvailableBlocks);

end.
