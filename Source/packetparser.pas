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

Function AddPacketInfoToStringGrid(PD : TPacketData;SG : TStringGrid;BlockName : String):Boolean;
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

Function AddPacketInfoToStringGrid(PD : TPacketData;SG : TStringGrid;BlockName : String):Boolean;
VAR
  FN : String ;
  SL, Line : TStringList ;
  LastPos, DataSize, P : Integer ;
  I, N, C, LOffset , LSubOffset, SwitchVal, LSizeOffset : Integer ;
  LType, LOffsetStr, LSubOffsetStr, LRangeOffsetStr, LName, LDescription : String ;
  S, CurrentBlock : String ;
  AllowAutoSwitchBlock : Boolean ;
  DT : TDateTime ;
  ColIndex : Integer ;
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
        If ((LType = 'ms') or (LType = 'timestamp')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 4 ;
          // Milliseconds
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,4,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,MSToStr(PD.GetUInt32AtPos(LOffset)),4);

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
        {
        If (LType = 'string') Then
        Begin
          // Zero terminated String
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,0,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,PD.GetStringAtPos(LOffset));
          If (LOffset >= LastPos) Then LastPos := LOffset + Length(PD.GetStringAtPos(LOffset))+1 ;

        End Else
        }

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
          AddSGRow(SG,LOffset,LName,IntToStr(PD.GetWordAtPos(LOffset)) + ' => ' + Zones.GetVal(PD.GetWordAtPos(LOffset)),2);

        End Else

        If (LType = 'job') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 1 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,1,DataCol(ColIndex));
          // Job
          AddSGRow(SG,LOffset,LName,IntToStr(PD.GetByteAtPos(LOffset)) + ' => ' + JobNames.GetVal(PD.GetByteAtPos(LOffset)),1);

        End Else

        If (LType = 'jobflags') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 4 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,4,DataCol(ColIndex));
          // Job unlock flags
          AddSGRow(SG,LOffset,LName,'0x'+IntToHex(PD.GetUint32AtPos(LOffset),8) + ' => ' + PD.GetJobflagsAtPos(LOffset),4);
        End Else

        If (LType = 'blacklistentry') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 20 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,20,DataCol(ColIndex));
          // Blacklist entry
          AddSGRow(SG,LOffset,LName,'ID: 0x'+IntToHex(PD.GetUint32AtPos(LOffset),8) + ' => ' + PD.GetStringAtPos(LOffset+4,16),20);
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
              ItemNames.GetVal(PD.GetWordAtPos(N+4)) +
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
              ItemNames.GetVal(PD.GetWordAtPos(N)) +
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
          AddSGRow(SG,LOffset,LName,'0x' + IntToHex(PD.GetWordAtPos(LOffset),4) + '  ' + IntToStr(PD.GetWordAtPos(LOffset)) + ' ' + ItemNames.GetVal(PD.GetWordAtPos(LOffset)) ,2);
        End Else

        If (LType = 'item-head') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,2,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,'0x' + IntToHex(PD.GetWordAtPos(LOffset),4) + ' => ' + IntToStr(PD.GetWordAtPos(LOffset) - $1000) + ' ' + ItemModelNames.GetVal(PD.GetWordAtPos(LOffset)) ,2);
        End Else

        If (LType = 'item-body') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,2,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,'0x' + IntToHex(PD.GetWordAtPos(LOffset),4) + ' => ' + IntToStr(PD.GetWordAtPos(LOffset) - $2000) + ' ' + ItemModelNames.GetVal(PD.GetWordAtPos(LOffset)) ,2);
        End Else

        If (LType = 'item-hands') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,2,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,'0x' + IntToHex(PD.GetWordAtPos(LOffset),4) + ' => ' + IntToStr(PD.GetWordAtPos(LOffset) - $3000) + ' ' + ItemModelNames.GetVal(PD.GetWordAtPos(LOffset)) ,2);
        End Else

        If (LType = 'item-legs') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,2,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,'0x' + IntToHex(PD.GetWordAtPos(LOffset),4) + ' => ' + IntToStr(PD.GetWordAtPos(LOffset) - $4000) + ' ' + ItemModelNames.GetVal(PD.GetWordAtPos(LOffset)) ,2);
        End Else

        If (LType = 'item-feet') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,2,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,'0x' + IntToHex(PD.GetWordAtPos(LOffset),4) + ' => ' + IntToStr(PD.GetWordAtPos(LOffset) - $5000) + ' ' + ItemModelNames.GetVal(PD.GetWordAtPos(LOffset)) ,2);
        End Else

        If (LType = 'item-main') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,2,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,'0x' + IntToHex(PD.GetWordAtPos(LOffset),4) + ' => ' + IntToStr(PD.GetWordAtPos(LOffset) - $6000) + ' ' + ItemModelNames.GetVal(PD.GetWordAtPos(LOffset)) ,2);
        End Else

        If (LType = 'item-sub') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,2,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,'0x' + IntToHex(PD.GetWordAtPos(LOffset),4) + ' => ' + IntToStr(PD.GetWordAtPos(LOffset) - $7000) + ' ' + ItemModelNames.GetVal(PD.GetWordAtPos(LOffset)) ,2);
        End Else

        If ((LType = 'item-ranged') or (LType = 'item-range')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,2,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,'0x' + IntToHex(PD.GetWordAtPos(LOffset),4) + ' => ' + IntToStr(PD.GetWordAtPos(LOffset) - $8000) + ' ' + ItemModelNames.GetVal(PD.GetWordAtPos(LOffset)) ,2);
        End Else

        If ((LType = 'music') or (LType = 'bgm')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,2,DataCol(ColIndex));
          AddSGRow(SG,LOffset,LName,'0x' + IntToHex(PD.GetWordAtPos(LOffset),4) + '  ' + IntToStr(PD.GetWordAtPos(LOffset)) + ' ' + MusicNames.GetVal(PD.GetWordAtPos(LOffset)) ,2);
        End Else

        If (LType = 'weather') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          ColIndex := ColIndex + 1 ;
          If Assigned(UpdateActiveRE) Then MarkREBytes(UpdateActiveRE,LOffset,2,DataCol(ColIndex));
          // Weather
          AddSGRow(SG,LOffset,LName,IntToStr(PD.GetWordAtPos(LOffset)) + ' => ' + WeatherNames.GetVal(PD.GetWordAtPos(LOffset)),2);

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
