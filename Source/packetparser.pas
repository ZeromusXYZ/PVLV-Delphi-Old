unit packetparser;

interface

uses packetdefs, Vcl.Grids;

Function AddPacketInfoToStringGrid(PD : TPacketData;SG : TStringGrid):Boolean;

implementation

uses System.SysUtils, System.StrUtils, System.Contnrs, System.Classes ,
  datalookups;

Procedure AddSGRow(SG : TStringGrid ; VarName:String; Val:String);
Begin
  SG.RowCount := SG.RowCount + 1 ;
  SG.Cells[0,SG.RowCount-1] := VarName ;
  SG.Cells[1,SG.RowCount-1] := Val ;
End;

procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings) ;
begin
   ListOfStrings.Clear;
   ListOfStrings.Delimiter       := Delimiter;
   ListOfStrings.StrictDelimiter := True;
   ListOfStrings.DelimitedText   := Str;
end;


Function AddPacketInfoToStringGrid(PD : TPacketData;SG : TStringGrid):Boolean;
VAR
  FN : String ;
  SL, Line : TStringList ;
  I, LOffset , LSubOffset : Integer ;
  LType, LOffsetStr, LSubOffsetStr, LName, LDescription, S : String ;
  LastPos, DataSize, P : Integer ;
Begin
  Result := True ;
  FN := 'parse\' ;
  If (PD.PacketLogType = pltIn) Then FN := FN + 'in-' ;
  If (PD.PacketLogType = pltOut) Then FN := FN + 'out-' ;
  FN := FN + '0x' + IntToHex(PD.PacketID,3) + '.txt' ;
  LastPos := 4 ;
  Try
    SL := TStringList.Create ;
    Line := TStringList.Create;
    // TODO: Cache the files ?
    If FileExists(FN) Then
      SL.LoadFromFile(FN)
    Else
      SL.Clear ;
    // Parse all the stuff !
    For I := 0 To SL.Count-1 Do
    Begin
      // loop all the lines
      Split(';',SL[I],Line);
      If (Line.Count >= 2) Then
      Begin
        // We want a  type:offset[:name[:description]] format
        LType := LowerCase(Line[0]);
        LOffsetStr := LowerCase(Line[1]);
        LSubOffsetStr := '0' ;
        LOffset := 4 ;
        If (Line.Count > 2) Then LName := Line[2] else LName := '' ;
        If (Line.Count > 3) Then LDescription := Line[3] else LDescription := '???' ; // we're not actually using this (yet)


        P := Pos(':',LOffsetStr);
        If (P > 0) Then
        Begin
          LSubOffsetStr := Copy(LOffsetStr,P+1,Length(LOffsetStr));
          LOffsetStr := Copy(LOffsetStr,1,P-1);
        End;

        If (TryStrToInt(LOffsetStr,LOffset) = False) Then
        Begin
          // Invalid offset value, skip this line
          Continue;
        End;
        If (TryStrToInt(LSubOffsetStr,LSubOffset) = False) Then
        Begin
          // Invalid suboffset value, default it to 0
          LSubOffset := 0 ;
        End;


        If (LName = '') Then
        Begin
          LName := LType + ' @ 0x' + IntToHex(LOffset,2) + ' ' + IntToStr(LOffset);
        End;

        If ((LType = 'file') or (LType = 'rem') or (LType = 'comment')) Then
        Begin
          // It's just a comment or header, ignore ...
        End Else
        If ((LType = 'byte') or (LType = 'char') or (LType = 'b')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 1 ;
          AddSGRow(SG,LName,'0x' + IntToHex(PD.GetByteAtPos(LOffset),2) + '  ' + IntToStr(PD.GetByteAtPos(LOffset)));
        End Else
        If ((LType = 'byteflag') or (LType = 'flag') or (LType = 'bits')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 1 ;
          AddSGRow(SG,LName, '0x' + IntToHex(PD.GetByteAtPos(LOffset),2) + '  ' + BytetoBit(PD.GetByteAtPos(LOffset)) + '  ' + IntToStr(PD.GetByteAtPos(LOffset)) );
        End Else
        If ((LType = 'bit') or (LType = 'bool')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 1 ;
          AddSGRow(SG,LName, BoolToStr( PD.GetBitAtPos(LOffset,LSubOffset),True ) );
        End Else
        If ((LType = 'word') or (LType = 'uint16') or (LType = 'ushort') or (LType = 'w')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          AddSGRow(SG,LName,'0x' + IntToHex(PD.GetWordAtPos(LOffset),4) + '  ' + IntToStr(PD.GetWordAtPos(LOffset)));
        End Else
        If ((LType = 'int16') or (LType = 'short')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          AddSGRow(SG,LName,'0x' + IntToHex(PD.GetInt16AtPos(LOffset),4) + '  ' + IntToStr( PD.GetInt16AtPos(LOffset)));
        End Else
        If ((LType = 'int') or (LType = 'int32') or (LType = 'i')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 4 ;
          AddSGRow(SG,LName,'0x' + IntToHex(PD.GetInt32AtPos(LOffset),8) + '  ' + IntToStr(PD.GetInt32AtPos(LOffset)));
        End Else
        If ((LType = 'dword') or (LType = 'long') or (LType = 'longword') or (LType = 'uint32') or (LType = 'u')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 4 ;
          AddSGRow(SG,LName,'0x' + IntToHex(PD.GetUInt32AtPos(LOffset),8) + '  ' + IntToStr(PD.GetUInt32AtPos(LOffset)));
        End Else
        If ((LType = 'float') or (LType = 'f')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 4 ;
          AddSGRow(SG,LName,IntToStr(PD.GetUInt32AtPos(LOffset)));

        End Else
        // Special types
        If (LType = 'pos') Then
        Begin
          // Position coordinates, float x3
          If (LOffset >= LastPos) Then LastPos := LOffset + 12 ;
          AddSGRow(SG,LName,
            'X:'  + FormatFloat('0.00',PD.GetFloatAtPos(LOffset)) +
            ' Y:' + FormatFloat('0.00',PD.GetFloatAtPos(LOffset+4)) +
            ' Z:' + FormatFloat('0.00',PD.GetFloatAtPos(LOffset+8)));

        End Else
        If (LType = 'dir') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 1 ;
          // Direction as byte
          AddSGRow(SG,LName,ByteToRotation(PD.GetByteAtPos(LOffset)));

        End Else
        If ((LType = 'ms') or (LType = 'timestamp')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 4 ;
          // Milliseconds
          AddSGRow(SG,LName,MSToStr(PD.GetUInt32AtPos(LOffset)));

        End Else

        If ((LType = 'frames') or (LType = 'frame')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 4 ;
          // Milliseconds
          AddSGRow(SG,LName,FramesToStr(PD.GetUInt32AtPos(LOffset)));

        End Else

        If (Copy(LType,1,6) = 'string') Then
        Begin
          // Zero terminated String
          DataSize := StrToIntDef(Copy(LType,7,Length(LType)),-1);

          AddSGRow(SG,LName,PD.GetStringAtPos(LOffset,DataSize));
          If (LOffset >= LastPos) Then LastPos := LOffset + Length(PD.GetStringAtPos(LOffset))+1 ;

        End Else
        {
        If (LType = 'string') Then
        Begin
          // Zero terminated String
          AddSGRow(SG,LName,PD.GetStringAtPos(LOffset));
          If (LOffset >= LastPos) Then LastPos := LOffset + Length(PD.GetStringAtPos(LOffset))+1 ;

        End Else
        }

        If (Copy(LType,1,4) = 'data') Then
        Begin
          // simple byte data
          DataSize := StrToIntDef(Copy(LType,5,Length(LType)),1);
          AddSGRow(SG,LName,PD.GetDataAtPos(LOffset,DataSize));
          If (LOffset >= LastPos) Then LastPos := LOffset + DataSize ;

        End Else

        If ((LType = 'ip4') or (LType = 'ip')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 4 ;
          AddSGRow(SG,LName,'0x' + IntToHex(PD.GetUInt32AtPos(LOffset),8) + ' => ' + PD.GetIP4AtPos(LOffset));
        End Else

        // Specialized items

        If ((LType = 'equipslot') or (LType = 'slot')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 1 ;
          // Slot
          AddSGRow(SG,LName,EquipmentSlotName(PD.GetByteAtPos(LOffset)));

        End Else
        If ((LType = 'container') or (LType = 'inventory') or (LType = 'bag')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 1 ;
          // Inventory Bag
          AddSGRow(SG,LName,ContainerName(PD.GetByteAtPos(LOffset)));

        End Else

        If ((LType = 'zone') or (LType = 'map')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          // Inventory Bag
          AddSGRow(SG,LName,IntToStr(PD.GetWordAtPos(LOffset)) + ' => ' + Zones.GetVal(PD.GetWordAtPos(LOffset)));

        End Else

        If (LType = 'job') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 1 ;
          // Job
          AddSGRow(SG,LName,IntToStr(PD.GetByteAtPos(LOffset)) + ' => ' + JobNames.GetVal(PD.GetByteAtPos(LOffset)));

        End Else

        If ((LType = 'item') or (LType = 'itemid')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          AddSGRow(SG,LName,'0x' + IntToHex(PD.GetWordAtPos(LOffset),4) + '  ' + IntToStr(PD.GetWordAtPos(LOffset)) + ' ' + ItemNames.GetVal(PD.GetWordAtPos(LOffset)) );
        End Else

        If (LType = 'item-head') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          AddSGRow(SG,LName,'0x' + IntToHex(PD.GetWordAtPos(LOffset),4) + ' => ' + IntToStr(PD.GetWordAtPos(LOffset) - $1000) + ' ' + ItemModelNames.GetVal(PD.GetWordAtPos(LOffset)) );
        End Else

        If (LType = 'item-body') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          AddSGRow(SG,LName,'0x' + IntToHex(PD.GetWordAtPos(LOffset),4) + ' => ' + IntToStr(PD.GetWordAtPos(LOffset) - $2000) + ' ' + ItemModelNames.GetVal(PD.GetWordAtPos(LOffset)) );
        End Else

        If (LType = 'item-hands') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          AddSGRow(SG,LName,'0x' + IntToHex(PD.GetWordAtPos(LOffset),4) + ' => ' + IntToStr(PD.GetWordAtPos(LOffset) - $3000) + ' ' + ItemModelNames.GetVal(PD.GetWordAtPos(LOffset)) );
        End Else

        If (LType = 'item-legs') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          AddSGRow(SG,LName,'0x' + IntToHex(PD.GetWordAtPos(LOffset),4) + ' => ' + IntToStr(PD.GetWordAtPos(LOffset) - $4000) + ' ' + ItemModelNames.GetVal(PD.GetWordAtPos(LOffset)) );
        End Else

        If (LType = 'item-feet') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          AddSGRow(SG,LName,'0x' + IntToHex(PD.GetWordAtPos(LOffset),4) + ' => ' + IntToStr(PD.GetWordAtPos(LOffset) - $5000) + ' ' + ItemModelNames.GetVal(PD.GetWordAtPos(LOffset)) );
        End Else

        If (LType = 'item-main') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          AddSGRow(SG,LName,'0x' + IntToHex(PD.GetWordAtPos(LOffset),4) + ' => ' + IntToStr(PD.GetWordAtPos(LOffset) - $6000) + ' ' + ItemModelNames.GetVal(PD.GetWordAtPos(LOffset)) );
        End Else

        If (LType = 'item-sub') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          AddSGRow(SG,LName,'0x' + IntToHex(PD.GetWordAtPos(LOffset),4) + ' => ' + IntToStr(PD.GetWordAtPos(LOffset) - $7000) + ' ' + ItemModelNames.GetVal(PD.GetWordAtPos(LOffset)) );
        End Else

        If ((LType = 'item-ranged') or (LType = 'item-range')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          AddSGRow(SG,LName,'0x' + IntToHex(PD.GetWordAtPos(LOffset),4) + ' => ' + IntToStr(PD.GetWordAtPos(LOffset) - $8000) + ' ' + ItemModelNames.GetVal(PD.GetWordAtPos(LOffset)) );
        End Else

        If ((LType = 'music') or (LType = 'bgm')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          AddSGRow(SG,LName,'0x' + IntToHex(PD.GetWordAtPos(LOffset),4) + '  ' + IntToStr(PD.GetWordAtPos(LOffset)) + ' ' + MusicNames.GetVal(PD.GetWordAtPos(LOffset)) );
        End Else

        If (LType = 'weather') Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          // Weather
          AddSGRow(SG,LName,IntToStr(PD.GetWordAtPos(LOffset)) + ' => ' + WeatherNames.GetVal(PD.GetWordAtPos(LOffset)));

        End Else

        Begin
          // this is a what now ?
          If (LOffset >= LastPos) Then LastPos := LOffset + 0 ;
          AddSGRow(SG,LName,'unknown type ' + LType );
        End;

      End; // end linecount >= 2

    End; // end for lines

    I := LastPos ;
    While I < PD.PacketDataSize Do
    Begin
      AddSGRow(SG,'?Byte 0x'+IntToHex(I,2)+' ' +IntToStr(I), '0x' + IntToHex(PD.GetByteAtPos(I),2) + '  ' + BytetoBit(PD.GetByteAtPos(I)) + '  ' + IntToStr(PD.GetByteAtPos(I)));
      I := I + 1 ;
    End;


  Except
    FreeAndNil(SL);
    FreeAndNil(Line);
    Result := False ;
    Exit ;
  End;
End;

end.
