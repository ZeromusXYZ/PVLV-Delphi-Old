unit packetdefs;

interface

uses Classes, System.Contnrs ;

CONST
  pltUnknown = 0 ;
  pltOut     = 1 ;
  pltIn      = 2 ;

TYPE
  TPacketData = Class
  Protected
    fRawText : TStringList ;
    fHeaderText : String ;
    fRawBytes : Array of Byte ;
    fPacketLogType : Byte ; // 0 = unknown ; 1 = outgoing ; 2 = incomming
    fPacketID : Word ;
    fPacketDataSize : Word ;
    fPacketSync : Word ;
    fTimeStamp : TDateTime ;
  Public
    Constructor Create ;
    Destructor Destroy ; Override ;
    Function AddRawLineAsBytes(S : String):Integer;
    Function PrintRawBytesAsHex(ValuesPerRow : Integer = 16) : String ;
    Function GetByteAtPos(Pos:Integer):Byte;
    Function GetWordAtPos(Pos:Integer):Word;
    Function GetInt32AtPos(Pos:Integer):Int32;
    Function GetUInt32AtPos(Pos:Integer):Cardinal;
    Function GetFloatAtPos(Pos:Integer):Single;
    Function GetTimeStampAtPos(Pos:Integer):String;
    Procedure CompileData ;
    Function FindByte(AByte : Byte):Integer;
    Function FindUInt16(AUInt16 : Word):Integer;
    Function FindUInt32(AUInt32 : LongWord):Integer;
    Property RawText : TStringList read fRawText ;
    Property Header : String read fHeaderText ;
    Property PacketLogType : Byte read fPacketLogType ;
    Property PacketID : Word read fPacketID ;
    Property PacketDataSize : Word read fPacketDataSize ;
    Property PacketSync : Word read fPacketSync ;
    property TimeStamp : TDateTime read fTimeStamp ;
  End;

  TPacketList = Class
  Protected
    fPacketDataList : TObjectList ;
  Public
    FilterOutOnly : Word ;
    FilterOut : Array of Word ;
    FilterInOnly : Word ;
    FilterIn : Array of Word ;

    Constructor Create(IsMaster:Boolean);
    Destructor Destroy ; Override ;
    Procedure Clear ;
    Procedure ClearFilters;
    Function LoadFromFile(Filename : String):Boolean;
    Function Count : Integer ;
    Function GetPacket(ID : Integer):TPacketData;
    Function CopyFrom(Original: TPacketList):Integer;
    // Function FilteredFrom(Original: TPacketList;HideFilteredIn:Boolean;FilterIn:Array of Word;HideFilteredOut:Boolean;FilterOut:Array of Word):Integer;
    Function FilterFrom(Original: TPacketList):Integer; // Uses filter vars to copy items
  End;

Function ByteToBit(B : Byte):String;
Function WordInArray(AWord:Word;AArray: Array of Word):Boolean;
Function PacketTypeToString(PacketLogType : Byte;PacketID : Word):String;
Function EquipmentSlotName(SlotID:Byte):String;
Function ContainerName(ContainerID:Byte):String;
Function ByteToRotation(B : Byte):String;
Function MSToStr(T : UInt32):String;

Function ToBitStr(var V):String;

implementation

Uses System.SysUtils, DateUtils , datalookups;

CONST
  CompasDirectionNames : Array[0..15] of String = ('E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW', 'N', 'NNE', 'NE', 'ENE');

Function ByteToRotation(B : Byte):String;
VAR
  I : Integer ;
Begin
  I := B * 360 ;
  I := I div 256 ;
  Result := CompasDirectionNames[(B div 16) mod 16] + ' (0x' + IntToHex(B,2) + ' ≈ '+IntToStr(I) + '°)' ;
End;

Function MSToStr(T : UInt32):String;
VAR
  R, V : UInt32 ;
Begin
  R := T div 1000 ;
  V := T mod 1000 ;
  Result := Format('%.3d',[V]) + 'ms';

  If (R > 0) Then
  Begin
    V := R mod 60 ;
    R := R div 60 ;
    Result := Format('%.2d',[V]) + 's ' + Result ;
    If (R > 0) Then
    Begin
      V := R mod 60 ;
      R := R div 60 ;
      Result := Format('%.2d',[V]) + 'm ' + Result ;
      If (R > 0) Then
      Begin
        V := R mod 24 ;
        R := R div 24 ;
        Result := Format('%.2d',[V]) + 'h ' + Result ;
        If (R > 0) Then
        Begin
          Result := Format('%d',[R]) + 'd ' + Result ;
        End;
      End;
    End;
  End;



End;

Function ToBitStr(var V):String;
VAR
  I, N , ByteSize, S : Integer ;
  //BitSize : Integer ;
  B : Byte ;
  BP : PByte ;
Begin
  {$POINTERMATH ON}
  Result := '' ;
  ByteSize := SizeOf(V);
  //BitSize := ByteSize * 8 ;

  For N := 0 To ByteSize-1 Do
  Begin
    BP := @V ;
    BP := BP + N ;
    B := BP^ ;
    S := 8 ;
    For I := 1 to S do
    Begin
      If (B and $01) = $01 Then Result := '1' + Result else Result := '0' + Result ;
      B := B shr 1 ;
      if ((I mod 4) = 0)and(I < S) Then Result := ' ' + Result ; // nibble formatting
    End;
  End;

End;

Function PacketTypeToString(PacketLogType : Byte;PacketID : Word):String;
VAR
  R : String ;
Begin
  R := '' ;
  If PacketLogType = pltOut Then
  Begin
    R := PacketOutNames.GetVal(PacketID);
  End;
  If PacketLogType = pltIn Then
  Begin
    R := PacketInNames.GetVal(PacketID);
  End;
  If R = '' Then R := 'Unknown' ;
  Result := R ;
End;

Function EquipmentSlotName(SlotID:Byte):String;
Begin
  Result := EquipmentSlots.GetVal(SlotID);
  If (Result = '') Then
    Result := 'SLOT_0x'+IntToHex(SlotID,2);
End;

Function ContainerName(ContainerID:Byte):String;
Begin
  Result := ContainerNames.GetVal(ContainerID);
  If (Result = '') Then
    Result := 'LOC_0x'+IntToHex(ContainerID,2);
End;

Function ByteToBit(B : Byte):String;
VAR
  I , S : Integer ;
Begin
  Result := '' ;
  S := (SizeOf(B)*8);
  For I := 1 to S do
  Begin
    If (B and $01) = $01 Then Result := '1' + Result else Result := '0' + Result ;
    B := B shr 1 ;
    if ((I mod 4) = 0)and(I < S) Then Result := ' ' + Result ; // nibble formatting
  End;
End;

Function WordInArray(AWord:Word;AArray: Array of Word):Boolean;
VAR
  I : Integer ;
Begin
  Result := False ;
  For I := 0 To Length(AArray)-1 do
  If (AWord = AArray[I]) Then
  Begin
    Result := true ;
    exit ;
  End;
End;


Constructor TPacketData.Create;
begin
  Inherited Create ;
  fRawText := TStringList.Create ;
  fHeaderText := 'Unknown Header' ;
  fPacketLogType := pltUnknown ;
  fTimeStamp := 0 ;
  fPacketID := $000 ;
  fPacketDataSize := 0 ;
  SetLength(fRawBytes,0);
end;

Destructor TPacketData.Destroy ;
Begin
  FreeAndNil(fRawText);
  SetLength(fRawBytes,0);
  Inherited Destroy ;
End;

Function TPacketData.AddRawLineAsBytes(S : String):Integer;
VAR
  H : String ;
  I, C : Integer ;
  B : Byte ;
Begin
  Result := 0 ;
  C := 0 ;

  //         1         2         3         4         5         6         7         8         9
  //123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
  //      5 | 00 00 00 00 -- -- -- -- -- -- -- -- -- -- -- --    5 | ....------------

//  If Length(S) < 81 Then
  If Length(S) < 57 Then
  Begin
    // Doesn't look like the correct format
    Exit ;
  End;
  For I := 0 to $f Do
  Begin
    H := Copy(S,11 + (I*3),2);
    If (H <> '--') Then
    Begin
      B := StrToInt('$'+H);
      // B := Byte(S[66+I]);
      SetLength(fRawBytes,Length(fRawBytes)+1);
      fRawBytes[Length(fRawBytes)-1] := B ;
      C := C + 1 ;
    End;
  End;

  Result := C ;
End;

Function TPacketData.PrintRawBytesAsHex(ValuesPerRow : Integer = 16) : String ;
VAR
  S : String ;
  I , L : Integer ;
  B : Byte ;
Begin
  Result := '' ;
  Result := Result + '   |  0  1  2  3   4  5  6  7   8  9  A  B   C  D  E  F' + #13#10 ;
  Result := Result + '---+----------------------------------------------------' + #13#10 ;
  L := 0 ;
  For I := 0 To Length(fRawBytes)-1 Do
  Begin
    If ((I mod ValuesPerRow) = 0) Then
    Begin
      Result := Result + IntToHex(L,2) + ' | ' ;
    End;

    B := fRawBytes[I];
    S := IntToHex(B,2);
    Result := Result + S ;
    If (I mod ValuesPerRow) = ValuesPerRow-1 Then
    Begin
      Result := Result + #13#10 ;
      L := L + 1 ;
    End Else
    Begin
      Result := Result + ' ' ;
      If (I mod 4) = 3 Then Result := Result + ' ' ; // extra spacing every 4 bytes
    End;
  End;

End;

Function TPacketData.GetWordAtPos(Pos:Integer):Word;
VAR
  V : ^Word ;
Begin
  Result := 0 ;
  Try
    If (Pos > length(fRawBytes)-2) Then Exit ;
    V := @fRawBytes[Pos] ;
    // Result := fRawBytes[Pos] + (fRawBytes[Pos+1] * $100);
    Result := V^ ;
    Exit ;
  Except
    Result := 0 ;
  End;
End;

Function TPacketData.GetInt32AtPos(Pos:Integer):Int32;
VAR
  V : ^Int32 ;
Begin
  Result := 0 ;
  Try
    If (Pos > length(fRawBytes)-4) Then Exit ;
    V := @fRawBytes[Pos] ;
    // Result := fRawBytes[Pos] + (fRawBytes[Pos+1] * $100);
    Result := V^ ;
    Exit ;
  Except
    Result := 0 ;
  End;
End;

Function TPacketData.GetUInt32AtPos(Pos:Integer):Cardinal;
VAR
  V : ^Cardinal ;
Begin
  Result := 0 ;
  Try
    If (Pos > length(fRawBytes)-4) Then Exit ;
    V := @fRawBytes[Pos] ;
    // Result := fRawBytes[Pos] + (fRawBytes[Pos+1] * $100);
    Result := V^ ;
    Exit ;
  Except
    Result := 0 ;
  End;
End;

Function TPacketData.GetTimeStampAtPos(Pos:Integer):String;
VAR
  DT : ^UInt32 ;
Begin
  Result := '???' ;
  Try
    If (Pos > length(fRawBytes)-4) Then Exit ;
    DT := @fRawBytes[Pos] ;
    Result := DateTimeToStr(UnixToDateTime(DT^));
    Exit ;
  Except
    Result := 'ERROR' ;
  End;
End;


Function TPacketData.GetByteAtPos(Pos:Integer):Byte;
VAR
  V : ^Byte ;
Begin
  Result := 0 ;
  Try
    If (Pos > length(fRawBytes)-2) Then Exit ;
    V := @fRawBytes[Pos] ;
    // Result := fRawBytes[Pos] + (fRawBytes[Pos+1] * $100);
    Result := V^ ;
    Exit ;
  Except
    Result := 0 ;
  End;
End;

Function TPacketData.GetFloatAtPos(Pos:Integer):Single;
VAR
  V : ^Single ;
Begin
  Result := 0 ;
  Try
    If (Pos > length(fRawBytes)-4) Then Exit ;
    V := @fRawBytes[Pos] ;
    // Result := fRawBytes[Pos] + (fRawBytes[Pos+1] * $100);
    Result := V^ ;
    Exit ;
  Except
    Result := 0 ;
  End;
//  Result := Round(Result * 100) / 100.0 ;
End;



Procedure TPacketData.CompileData;
begin
  If Length(fRawBytes) < 4 then
  Begin
    fPacketID := $FFFF ; // invalid data
    fPacketDataSize := 0 ;
    Exit ;
  End;
  fPacketID := fRawBytes[$0] + ((fRawBytes[$1] AND $01) * $100) ;
  fPacketDataSize := (fRawBytes[$1] AND $FE) * 2; // basically, all packets are always multiples of 4 bytes
  fPacketSync := fRawBytes[$2] + (fRawBytes[$3] * $100); // packet order number

end;

Function TPacketData.FindByte(AByte : Byte):Integer;
VAR
  I : Integer ;
Begin
  Result := -1 ;
  For I := 0 to Length(fRawBytes)-1 Do
  If (fRawBytes[I] = AByte) Then
  Begin
    Result := I ;
    Exit ;
  End;
End;

Function TPacketData.FindUInt16(AUInt16 : Word):Integer;
VAR
  I : Integer ;
Begin
  Result := -1 ;
  For I := 0 to Length(fRawBytes)-2 Do
  If ( ((fRawBytes[I+1]) + (fRawBytes[I] * $100)) = AUInt16) Then
  Begin
    Result := I ;
    Exit ;
  End;
End;

Function TPacketData.FindUInt32(AUInt32 : LongWord):Integer;
VAR
  I : Integer ;
Begin
  Result := -1 ;
  For I := 0 to Length(fRawBytes)-2 Do
  If ( ((fRawBytes[I+3]) + (fRawBytes[I+2] * $100) + (fRawBytes[I+1] * $10000) + (fRawBytes[I] * $1000000)) = AUInt32) Then
  Begin
    Result := I ;
    Exit ;
  End;
End;



Constructor TPacketList.Create(IsMaster:Boolean);
begin
  Inherited Create ;
  fPacketDataList := TObjectList.Create(IsMaster);
  ClearFilters ;
end;

Destructor TPacketList.Destroy ;
Begin
  FreeAndNil(fPacketDataList);

  Inherited Destroy ;
End;

Procedure TPacketList.Clear;
begin
  fPacketDataList.Clear;
end;

Procedure TPacketList.ClearFilters;
Begin
  SetLength(FilterIn,0);
  FilterInOnly := $000 ;
  SetLength(FilterOut,0);
  FilterOutOnly := $000 ;
End;

Function TPacketList.LoadFromFile(Filename : String):Boolean;
VAR
  FileData : TStringList;
  I : Integer ;
  PD : TPacketData ;
  S : String ;
  PreferedPacketType : Byte ;
Begin

  PreferedPacketType := 0 ;
  Try

    if (Pos('outgoing',LowerCase(Filename)) > 0) Then
    Begin
      PreferedPacketType := 1 ;
    End Else
    if (Pos('incoming',LowerCase(Filename)) > 0) Then
    Begin
      PreferedPacketType := 2 ;
    End;

    FileData := TStringList.Create ;
    FileData.LoadFromFile(Filename);
    I := 0 ;
    PD := Nil ;
    While I < FileData.Count-1 Do
    Begin
      S := FileData.Strings[I];
      If ((S <> '') and (PD = nil)) Then
      Begin
        // Begin building new packet
        PD := TPacketData.Create;
        if (Pos('outgoing',LowerCase(S)) > 0) Then
        Begin
          PD.fPacketLogType := pltOut ;
        End Else
        if (Pos('incoming',LowerCase(S)) > 0) Then
        Begin
          PD.fPacketLogType := pltIn ;
        End else
        Begin
          PD.fPacketLogType := PreferedPacketType ;
        End;
        PD.fRawText.Add(S);
        PD.fHeaderText := S ;

      End else
      If ((S <> '') and Assigned(PD)) Then
      Begin
        // Add line of data
        PD.fRawText.Add(S);
        If (PD.fRawText.Count > 3) Then // Actual packet data starts at the 3rd line
        Begin
          PD.AddRawLineAsBytes(S);

        End;
      End else
      If ((S = '') and Assigned(PD)) Then
      Begin
        // Close this packet and add it to the list
        PD.CompileData;
        fPacketDataList.Add(PD);
        // null our reference
        PD := nil ;
      End else
      If ((S = '') and (Not Assigned(PD)) ) Then
      Begin
        // Blank line, do nothing
      End else
      begin
        // ERROR, unexpected entry, but let's just ignore it
      end;
      I := I + 1 ;
    End;

    Result := True ;
  Except
    Result := False ;
  End;
  If Assigned(FileData) Then FreeAndNil(FileData);
  If Assigned(PD) Then FreeAndNil(PD);
End;

Function TPacketList.Count : Integer ;
Begin
  Result := fPacketDataList.Count ;
End;

Function TPacketList.GetPacket(ID : Integer):TPacketData;
Begin
  If (ID >= 0) and (ID < fPacketDataList.Count) Then
    Result := (fPacketDataList.Items[ID] as TPacketData)
  Else
    Result := Nil ;
End;

Function TPacketList.CopyFrom(Original: TPacketList):Integer;
VAR
  I, C : Integer ;
begin
  C := 0 ;
  Clear ;

  For I := 0 To Original.fPacketDataList.Count-1 do
  Begin
    fPacketDataList.Add(Original.fPacketDataList.Items[I]);
    C := C + 1 ;
  End;

  Result := C ;
end;

Function TPacketList.FilterFrom(Original: TPacketList):Integer;
VAR
  I, C : Integer ;
  DoAdd : Boolean ;
  PD : TPacketData ;
begin
  C := 0 ;
  Clear ;

  For I := 0 To Original.fPacketDataList.Count-1 do
  Begin
    DoAdd := True ;
    PD := Original.GetPacket(I);

    // Out filters
    If (PD.PacketLogType = pltOut) Then
    Begin
      // Outgoing
      If (FilterOutOnly <> $000) Then
      Begin
        DoAdd := False ;
        If (FilterOutOnly = PD.PacketID) Then DoAdd := True ;
      End Else
      If (Length(FilterOut) > 0) Then
      Begin
        DoAdd := True ;
        If WordInArray(PD.PacketID,FilterOut) Then DoAdd := False ;
      End Else
      Begin
        DoAdd := True ;
      End;
    End;

    // In filters
    If (PD.PacketLogType = pltIn) Then
    Begin
      // Incomming
      If (FilterInOnly <> $000) Then
      Begin
        DoAdd := False ;
        If (FilterInOnly = PD.PacketID) Then DoAdd := True ;
      End Else
      If (Length(FilterIn) > 0) Then
      Begin
        DoAdd := True ;
        If WordInArray(PD.PacketID,FilterIn) Then DoAdd := False ;
      End Else
      Begin
        DoAdd := True ;
      End;
    End;

    If DoAdd Then
    Begin
      fPacketDataList.Add(Original.fPacketDataList.Items[I]);
      C := C + 1 ;
    End;
  End;

  Result := C ;
end;



end.
