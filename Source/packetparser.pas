unit packetparser;

interface

uses packetdefs, Vcl.Grids;

Function AddPacketInfoToStringGrid(PD : TPacketData;SG : TStringGrid):Boolean;

implementation

uses System.SysUtils, System.StrUtils, System.Contnrs, System.Classes ;

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
  I, LOffset : Integer ;
  LType, LOffsetStr, LName, LDescription : String ;
  LastPos : Integer ;
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
        If (Line.Count > 2) Then LName := Line[2] else LName := '' ;
        If (Line.Count > 3) Then LDescription := Line[3] else LDescription := '???' ; // we're not actually using this (yet)

        // Offset is hex in format 0x????
        If (Copy(LOffsetStr,1,2) = '0x') Then
        Begin
          LOffset := StrToInt('$'+Copy(LOffsetStr,3,Length(LOffsetStr)));
        End Else
        If (Copy(LOffsetStr,1,2) = '0X') Then
        Begin
          LOffset := StrToInt('$'+Copy(LOffsetStr,3,Length(LOffsetStr)));
        End Else
        // Offset is hex in format 0x????
        If (Copy(LOffsetStr,1,1) = '$') Then
        Begin
          LOffset := StrToInt(LOffsetStr);
        End Else
        Begin
          LOffset := $4 ; // default to 4 (first actual data byte)
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
        If ((LType = 'word') or (LType = 'uint16') or (LType = 'w')) Then
        Begin
          If (LOffset >= LastPos) Then LastPos := LOffset + 2 ;
          AddSGRow(SG,LName,'0x' + IntToHex(PD.GetWordAtPos(LOffset),4) + '  ' + IntToStr(PD.GetWordAtPos(LOffset)));
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
