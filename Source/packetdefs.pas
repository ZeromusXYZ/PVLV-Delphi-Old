unit packetdefs;

interface

uses Classes, System.Contnrs, loadingform ;

CONST
  pltUnknown = 0 ;
  pltOut     = 1 ;
  pltIn      = 2 ;

TYPE
  TFilterType = (
    ftFilterOff,
    ftHidePackets,
    ftShowPackets,
    ftAllowNone );


TYPE
  TPacketData = Class
  Protected
    fRawText : TStringList ;
    fHeaderText : String ;
    fOriginalHeaderText : String ;
    fRawBytes : Array of Byte ;
    fPacketLogType : Byte ; // 0 = unknown ; 1 = outgoing ; 2 = incomming
    fPacketID : Word ;
    fPacketDataSize : Word ;
    fPacketSync : Word ;
    fTimeStamp : TDateTime ;
    fOriginalTimeString : String ;
  Public
    Constructor Create ;
    Destructor Destroy ; Override ;
    Function AddRawLineAsBytes(S : String):Integer;
    Function AddRawPacketeerLineAsBytes(S : String):Integer;
    Function PrintRawBytesAsHex() : String ;
    Function GetByteAtPos(Pos:Integer):Byte;
    Function GetBitAtPos(Pos,BitOffset:Integer):Boolean;
    Function GetBitsAtPos(Pos,BitOffset,BitsSize:Integer):Int64; Overload ;
    Function GetBitsAtPos(BitOffset,BitsSize:Integer):Int64; Overload ;
    Function GetWordAtPos(Pos:Integer):Word;
    Function GetInt16AtPos(Pos:Integer):Int16;
    Function GetInt32AtPos(Pos:Integer):Int32;
    Function GetUInt32AtPos(Pos:Integer):Cardinal;
    Function GetFloatAtPos(Pos:Integer):Single;
    Function GetTimeStampAtPos(Pos:Integer):String;
    Function GetStringAtPos(Pos:Integer;MaxSize:Integer = -1):String;
    Function GetDataAtPos(Pos,Size:Integer):String;
    Function GetIP4AtPos(Pos:Integer):String;
    Function GetJobflagsAtPos(Pos:Integer):String;
    Function CompileData:Boolean;
    Function FindByte(AByte : Byte):Integer;
    Function FindUInt16(AUInt16 : Word):Integer;
    Function FindUInt32(AUInt32 : LongWord):Integer;
    Function RawSize : Integer ;
    Property RawText : TStringList read fRawText ;
    Property Header : String read fHeaderText ;
    Property OriginalHeader : String read fOriginalHeaderText ;
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
    FilterOutType : TFilterType ;
    FilterOutList : Array of Word ;
    FilterInType : TFilterType ;
    FilterInList : Array of Word ;

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
    Function DoIShowThis(PacketID : Word;FT : TFilterType;FL : Array of Word) : Boolean ;
  End;

Function ByteToBit(B : Byte):String;
Function WordInArray(AWord:Word;AArray: Array of Word):Boolean;
Function PacketTypeToString(PacketLogType : Byte;PacketID : Word):String;
Function EquipmentSlotName(SlotID:Byte):String;
Function ContainerName(ContainerID:Byte):String;
Function ByteToRotation(B : Byte):String;
Function MSToStr(T : UInt32):String;
Function FFXITimeStampToStr(T : UInt32):String;
Function FramesToStr(T : UInt32):String;
Function DWordToVanaTime(V : UInt32):String;

Function ToBitStr(var V):String;

implementation

Uses System.SysUtils, DateUtils , System.Variants, datalookups, Vcl.Dialogs, System.UITypes  ;

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

Function FFXITimeStampToStr(T : UInt32):String;
VAR
  DT : TDateTime ;
  Offset : TDateTime ;
Begin
  If T = $7FFFFFFF Then
    Result := 'Infinite'
  Else
  Begin
    Result := FramesToStr(T);
    // CVanaTime::getInstance()->getVanaTime() + 1009810800

    // Example unix 1532866711 -> 2018-07-29 12:18:31 +01:00
    // Example game 1318151468 -> 2018-07-29 12:18:31 +01:00
    // Difference =  214715243 ?


    {
    Offset := EncodeDateTime(2001,12,31,15,0,0,0);

    DT := FileDateToDateTime(T + 214715243);
    Result := DateTimeToStr(DT);
    }
  End;
End;

Function FramesToStr(T : UInt32):String;
VAR
  R, V : UInt32 ;
Begin
  R := T div 60 ;
  V := T mod 60 ;
  Result := Format('%.2d',[V]) + 'f';

  If (R > 0) Then
  Begin
    V := R mod 60 ;
    R := R div 60 ;
//    Result := Format('%.2d',[V]) + 's ' + Result ;
    Result := Format('%.2d',[V]) + ' / ' + Result ;
    If (R > 0) Then
    Begin
      V := R mod 60 ;
      R := R div 60 ;
//      Result := Format('%.2d',[V]) + 'm ' + Result ;
      Result := Format('%.2d',[V]) + '.' + Result ;
      If (R > 0) Then
      Begin
        V := R mod 24 ;
        R := R div 24 ;
//        Result := Format('%.2d',[V]) + 'h ' + Result ;
        Result := Format('%.2d',[V]) + ':' + Result ;
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
    R := NLU(LU_PacketOut).GetVal(PacketID);
  End;
  If PacketLogType = pltIn Then
  Begin
    R := NLU(LU_PacketIn).GetVal(PacketID);
  End;
  If R = '' Then R := '??? unknown' ;
  Result := R ;
End;

Function EquipmentSlotName(SlotID:Byte):String;
Begin
  Result := NLU(LU_EquipmentSlots).GetVal(SlotID);
  If (Result = '') Then
    Result := 'SLOT_0x'+IntToHex(SlotID,2)
  Else
    Result := '0x'+IntToHex(SlotID,2)+' => ' + Result ;
End;

Function ContainerName(ContainerID:Byte):String;
Begin
  Result := NLU(LU_Container).GetVal(ContainerID);
  If (Result = '') Then
    Result := 'LOC_0x'+IntToHex(ContainerID,2)
  Else
    Result := '0x'+IntToHex(ContainerID,2)+' => ' + Result ;
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

Function VanaDoW(DoW : Byte):String;
Begin
  Case DoW Of
    0 : Result := 'Firesday' ;
    1 : Result := 'Earthsday' ;
    2 : Result := 'Watersday' ;
    3 : Result := 'Windsday' ;
    4 : Result := 'Iceday' ;
    5 : Result := 'Thundersday' ;
    6 : Result := 'Lightsday' ;
    7 : Result := 'Darksday' ;
  Else
    Result := '???_0x'+IntToHex(DoW,2);
  End;
End;

Function DWordToVanaTime(V : UInt32):String;
CONST
  VTIME_BASEDATE = 1009810800	; // unix epoch - 1009810800 = se epoch (in earth seconds)
  VTIME_YEAR   = 518400 ;       // 360 * GameDay
  VTIME_MONTH  = 43200 ;        // 30 * GameDay
  VTIME_WEEK   = 11520 ;        // 8 * GameDay
  VTIME_DAY    = 1440	;         // 24 hours * GameHour
  VTIME_HOUR   = 60	;           // 60 minutes
  VTIME_FIRSTYEAR = 886 ;

VAR
  VanaDate : UInt32 ;
  vYear, vMonth, vDay, vDoW, vHour, vMinute : UInt32 ;
Begin
  VanaDate := V ;
  vYear := VanaDate div VTIME_YEAR ;
  vMonth := ((VanaDate div VTIME_MONTH) mod 12) + 1 ;
  vDay := ((VanaDate div VTIME_DAY) mod 30 ) + 1 ;
  vDoW := ((VanaDate mod VTIME_WEEK) div VTIME_DAY);
  vHour := ((VanaDate mod VTIME_DAY) div VTIME_HOUR);
  vMinute := (VanaDate mod VTIME_HOUR);

  Result := VanaDoW(vDoW)+ ' - ' +
    IntToStr(vYear+VTIME_FIRSTYEAR) + '/' + IntToStr(vMonth) + '/' + IntToStr(vDay) + ' - ' +
    IntToStr(vHour) + ':' + IntToStr(vMinute) +
    '  (0x'+IntToHex(V,8)+' - ' + IntToStr(V) +')';

(*
    m_vanaDate  = (uint32)(this->getVanaTime() / 60.0 * 25) + 886 * VTIME_YEAR; //convert vana time (from SE epoch in earth seconds) to vanadiel minutes and add 886 vana years

    m_vYear = (uint32)( m_vanaDate / VTIME_YEAR);
    m_vMon  = (uint32)((m_vanaDate / VTIME_MONTH) % 12) + 1;
    m_vDate = (uint32)((m_vanaDate / VTIME_DAY) % 30 ) + 1;
    m_vDay  = (uint32)((m_vanaDate % VTIME_WEEK)  / VTIME_DAY);
    m_vHour = (uint32)((m_vanaDate % VTIME_DAY)   / VTIME_HOUR);
    m_vMin  = (uint32)( m_vanaDate % VTIME_HOUR);

  Result := '' ;
  N := V mod 60 ; Result := '.' + IntToStr(N) + Result ; // Seconds
  V := V div 60 ;
  N := V mod 60 ; Result := ':' + IntToStr(N) + Result ; // Minutes
  V := V div 60 ;
  N := V mod 24 ; Result := ' - ' + IntToStr(N) + Result ; // Hours
  V := V div 24 ;
  N := V mod 30 ; Result := '/' + IntToStr(N) + Result ; // days
  V := V div 30 ;
  N := V mod 12 ; Result := '/' + IntToStr(N) + Result ; // months
  V := V div 12 ;
  Result := 'Vana-time ' + IntToStr(V) + Result ; // years
*)
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
  fOriginalHeaderText := '' ;
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

Function TPacketData.AddRawPacketeerLineAsBytes(S : String):Integer;
VAR
  H : String ;
  I, C , TryInt : Integer ;
  B : Byte ;
Begin
  Result := 0 ;
  C := 0 ;

  //          1         2         3         4         5         6         7         8         9
  // 123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
  // [C->S] Id: 001A | Size: 28
  //     1A 0E ED 24 D5 10 10 01 D5 00 00 00 00 00 00 00  ..í$Õ...Õ.......

  If Length(S) < 51 Then
  Begin
    // Doesn't look like the correct format
    Exit ;
  End;
  For I := 0 to $f Do
  Begin
    H := Copy(S,5 + (I*3),2);
    If (H <> '--')and(H <> '  ')and(H <> ' ') Then
    Begin
      // If this fails, we're probably at the end of the packet
      // Unlike windower, Packeteer doesn't add dashes for the blanks
      If Not TryStrToInt('$'+H,TryInt) Then Break ;
      B := TryInt ;
      // B := StrToInt('$'+H);
      SetLength(fRawBytes,Length(fRawBytes)+1);
      fRawBytes[Length(fRawBytes)-1] := B ;
      C := C + 1 ;
    End;
  End;

  Result := C ;
End;


Function TPacketData.PrintRawBytesAsHex() : String ;
CONST
  ValuesPerRow = 16 ;
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

Function TPacketData.GetInt16AtPos(Pos:Integer):Int16;
VAR
  V : ^Int16 ;
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

Function TPacketData.GetStringAtPos(Pos:Integer;MaxSize:Integer = -1):String;
VAR
  I : Integer ;
Begin
  Result := '' ;
  I := Pos ;
  While (I < length(fRawBytes)-1) and (fRawBytes[I] <> 0) and ((MaxSize = -1) or (Length(Result) < MaxSize) ) Do
  Begin
    Result := Result + Char(fRawBytes[I]);
    I := I + 1 ;
  End;
End;

Function TPacketData.GetDataAtPos(Pos,Size:Integer):String;
VAR
  I : Integer ;
Begin
  Result := '' ;
  I := 0 ;
  While ((I + Pos) < length(fRawBytes)) and (I < Size) and (I < 256) Do
  Begin
    Result := Result + IntToHex(fRawBytes[I+Pos],2) + ' ' ;
    I := I + 1 ;
  End;
End;

Function TPacketData.GetIP4AtPos(Pos:Integer):String;
VAR
  I : Integer ;
Begin
  Result := '' ;
  If (Pos >= Length(fRawBytes)-4) Then Exit ;
  Result := IntToStr(fRawBytes[Pos+0]) + '.' + IntToStr(fRawBytes[Pos+1]) + '.' + IntToStr(fRawBytes[Pos+2]) + '.' + IntToStr(fRawBytes[Pos+3]);
End;


Function TPacketData.GetJobflagsAtPos(Pos:Integer):String;
VAR
  I : Integer ;
  Flags : UInt32 ;
  BitShiftCount : Integer ;
  JobName : String ;
Begin
  Result := '' ;
  If (Pos >= Length(fRawBytes)-4) Then Exit ;
  Flags := GetUInt32AtPos(Pos);
  For BitShiftCount := 0 To 31 Do
  Begin
    If ((Flags and $0000001) = 1) Then
    Begin
      Case BitShiftCount Of
        0 : JobName := 'SubJob' ;
      Else
        JobName := NLU(LU_Job).GetVal(BitShiftCount);
        If (JobName = '') Then JobName := '[Bit'+IntToStr(BitShiftCount)+']';
      End;
      Result := Result + JobName + ' ' ;
    End;
    Flags := Flags shr 1 ;
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

Function TPacketData.GetBitAtPos(Pos,BitOffset:Integer):Boolean;
VAR
  V : ^Byte ;
  BitFilter : Byte ;
Begin
  Result := False ;
  Try
    If (Pos > length(fRawBytes)-2) Then Exit ;
    V := @fRawBytes[Pos] ;
    // Result := fRawBytes[Pos] + (fRawBytes[Pos+1] * $100);
    BitFilter := $01 ;
    While BitOffset > 0 Do
    Begin
      BitFilter := BitFilter shl 1 ;
      BitOffset := BitOffset - 1 ;
    End;

    Result := ((V^ and BitFilter) <> 0);
    Exit ;
  Except
    Result := False ;
  End;
End;

Function TPacketData.GetBitsAtPos(Pos,BitOffset,BitsSize:Integer):Int64;
VAR
  P, B, Rest : Integer ;
  Mask : Int64 ;
Begin
  Result := 0 ;
  P := Pos ;
  B := BitOffset ;
  Rest := BitsSize ;
  Mask := 1 ;
  While Rest > 0 Do
  Begin
    // Add mask value if bit set
    If GetBitAtPos(P,B) Then Result := Result + Mask ;
    // count down remaining bits to check
    Rest := Rest - 1 ;
    // Multiply mask by 2
    Mask := Mask shl 1 ;
    // Increase current bit counter
    B := B + 1 ;
    // If too high, jump to next byte
    If B >= 8 Then
    Begin
      P := P + 1 ;
      B := 0 ;
    End;
  End;

End;

Function TPacketData.GetBitsAtPos(BitOffset,BitsSize:Integer):Int64;
Begin
  Result := GetBitsAtPos(BitOffset div 8,BitOffset mod 8,BitsSize);
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



Function TPacketData.CompileData:Boolean;
VAR
  S, TS : String ;
  P1, P2 : Integer ;
begin
  Result := False ;
  If Length(fRawBytes) < 4 then
  Begin
    fPacketID := $FFFF ; // invalid data
    fPacketDataSize := 0 ;
    fHeaderText := 'Invalid Packet Size < 4' ;
    Exit ;
  End;
  fPacketID := fRawBytes[$0] + ((fRawBytes[$1] AND $01) * $100) ;
  fPacketDataSize := (fRawBytes[$1] AND $FE) * 2; // basically, all packets are always multiples of 4 bytes
  fPacketSync := fRawBytes[$2] + (fRawBytes[$3] * $100); // packet order number



  If (Pos('[c->s]',LowerCase(fOriginalTimeString))>0) or (Pos('[s->c]',LowerCase(fOriginalTimeString))>0) Then
  Begin
    // Packeteer doesn't have time info (yet)
    TS := '' ;
    fTimeStamp := 0 ;
    fOriginalTimeString := '0000-00-00 00:00' ;
  End Else
  Begin
    // Try to determine timestamp from header
    fOriginalTimeString := '' ;
    P1 := Pos('[',fOriginalHeaderText);
    P2 := Pos(']',fOriginalHeaderText);
    If (P1 > 0) and (P2 > 0) and (P2 > P1) Then
    Begin
      fOriginalTimeString := Copy(fOriginalHeaderText,P1+1,P2-P1-1);
      If (Length(fOriginalTimeString) > 0) Then
      Try
        fTimeStamp := VarToDateTime(fOriginalTimeString); // <-- seems to work better than anything I'd like to try
        // fTimeStamp := StrToDateTime(fOriginalTimeString);
        DateTimeToString(TS,'hh:nn:ss',TimeStamp);
      Except
        TS := '' ;
        fTimeStamp := 0 ;
        fOriginalTimeString := '0000-00-00 00:00' ;
      End;
    End;
  End;

  If (fTimeStamp = 0) Then TS := '' ;
//  If (fTimeStamp = 0) Then TS := '??:??:??' ;

  Case PacketLogType Of
    1 : S := 'OUT ' ;
    2 : S := 'IN  ' ;
  Else
    S := '??? ' ;
  End;
  S := TS + ' : ' + S + '0x' + IntToHex(PacketID,3) + ' - ' ;

  fHeaderText := S + PacketTypeToString(PacketLogType,PacketID);
  Result := True ;
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
  SUInt16 : Word ;
Begin
  Result := -1 ;
  For I := 0 to Length(fRawBytes)-2 Do
  Begin
    SUInt16 := GetWordAtPos(I);
    If (SUInt16 = AUInt16) Then
    Begin
      Result := I ;
      Exit ;
    End;
  End;
End;

Function TPacketData.FindUInt32(AUInt32 : LongWord):Integer;
VAR
  I : Integer ;
  SUInt32 : LongWord ;
Begin
  Result := -1 ;
  For I := 0 to Length(fRawBytes)-4 Do
  Begin
    SUInt32 := GetUInt32AtPos(I);
    If (SUInt32 = AUInt32) Then
    Begin
      Result := I ;
      Exit ;
    End;
  End;
End;


Function TPacketData.RawSize : Integer ;
Begin
  Result := Length(fRawBytes);
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
  SetLength(FilterOutList,0);
  FilterOutType := ftFilterOff ;
  SetLength(FilterInList,0);
  FilterInType := ftFilterOff ;
End;

Function TPacketList.LoadFromFile(Filename : String):Boolean;
VAR
  FileData : TStringList;
  I : Integer ;
  PD : TPacketData ;
  S : String ;
  PreferedPacketType : Byte ;
  StartTime : TDateTime ;
  IsUndefined, AskForType  : Boolean ;
  LogFileType : Byte ; // 0 unknown ; 1 Windower PacketViewer ; 2 Ashita Packeteer
Begin
  StartTime := Now ;

  IsUndefined := True ;
  AskForType := True ;
  PreferedPacketType := 0 ;
  LogFileType := 0 ;
  Try
    if (LowerCase(ExtractFileExt(FileName)) = '.log') Then LogFileType := 1 ;
    if (LowerCase(ExtractFileExt(FileName)) = '.txt') Then LogFileType := 2 ;

    if (Pos('outgoing',LowerCase(Filename)) > 0) Then
    Begin
      PreferedPacketType := 1 ;
      IsUndefined := False ;
    End Else
    if (Pos('incoming',LowerCase(Filename)) > 0) Then
    Begin
      PreferedPacketType := 2 ;
      IsUndefined := False ;
    End;

    FileData := TStringList.Create ;
    FileData.LoadFromFile(Filename);
    I := 0 ;
    PD := Nil ;

    FormLoading.Show ;
    FormLoading.BringToFront ;
    FormLoading.Caption := 'Loading ' + Filename ;

    While I < FileData.Count-1 Do
    Begin
      If FormLoading.Visible and ((I mod 50) = 0) Then
      Begin
        FormLoading.Repaint ;
        FormLoading.PB.Max := FileData.Count ;
        FormLoading.PB.Min := 0 ;
        FormLoading.PB.Position := I ;
      End;

      S := FileData.Strings[I];
      If ((S <> '') and (PD = nil)) Then
      Begin
        // Begin building new packet
        PD := TPacketData.Create;
        if (Pos('outgoing',LowerCase(S)) > 0) Then
        Begin
          PD.fPacketLogType := pltOut ;
          IsUndefined := False ;
          LogFileType := 1 ; // Looks like a packetviewer log
        End Else
        if (Pos('incoming',LowerCase(S)) > 0) Then
        Begin
          PD.fPacketLogType := pltIn ;
          IsUndefined := False ;
          LogFileType := 1 ; // Looks like a packetviewer log
        End else
        if (Pos('[c->s]',LowerCase(S)) > 0) Then
        Begin
          PD.fPacketLogType := pltOut ;
          IsUndefined := False ;
          AskForType := False ;
          LogFileType := 2 ; // This is a Packeteer file for sure
        End Else
        if (Pos('[s->c]',LowerCase(S)) > 0) Then
        Begin
          PD.fPacketLogType := pltIn ;
          IsUndefined := False ;
          AskForType := False ;
          LogFileType := 2 ; // This is a Packeteer file for sure
        End else
        Begin
          PD.fPacketLogType := PreferedPacketType ;
        End;

        If (
          (Copy(S,1,2) <> '--') and // Ignore comment lines
          (S <> '')) and // Ignore blank lines
          (IsUndefined and AskForType and (PD.fPacketLogType = 0) and // Only if we didn't define yet
          (LogFileType <> 2) // And not using packeteer files
          ) Then
        Begin
          AskForType := False ;
          Case MessageDlg( 'Unable to indentify the packet type. Do you want to assign a default type ?'#10#13#10#13'Press OK for Incomming'#10#13'Press Yes for outgoing'#10#13#10#13'Press Cancel to keep it undefined',mtConfirmation,[mbYes,mbOK,mbCancel],-1) Of
            mrOK : Begin
                PreferedPacketType := pltIn ;
                IsUndefined := False ;
                PD.fPacketLogType := PreferedPacketType ;
              End;
            mrYes : Begin
                PreferedPacketType := pltOut ;
                IsUndefined := False ;
                PD.fPacketLogType := PreferedPacketType ;
              End;
          End;
        End;


        PD.fRawText.Add(S);
        PD.fHeaderText := S ;
        PD.fOriginalHeaderText := S ;

      End else
      If ((S <> '') and Assigned(PD) and (LogFileType <> 2)) Then
      Begin
        // Add line of data
        PD.fRawText.Add(S);
        If (PD.fRawText.Count > 3) Then // Actual packet data starts at the 3rd line after the header
        Begin
          PD.AddRawLineAsBytes(S);

        End;
      End else

      If ((S <> '') and Assigned(PD) and (LogFileType = 2)) Then
      Begin
        // Add line of data
        PD.fRawText.Add(S);
        If (PD.fRawText.Count > 1) Then // Actual packet data starts at the 1st line after the header
        Begin
          PD.AddRawPacketeerLineAsBytes(S);

        End;
      End else

      If ((S = '') and Assigned(PD)) Then
      Begin
        // Close this packet and add it to the list
        If PD.CompileData Then
          fPacketDataList.Add(PD)
        Else
          FreeAndNil(PD); // Free it if it's invalid
        // null our reference
        PD := nil ;
      End else
      If ((S = '') and (Not Assigned(PD)) ) Then
      Begin
        // Blank line, do nothing
      End else
      If ((Copy(S,1,2) = '--') and (Not Assigned(PD)) ) Then
      Begin
        // comment-line, do nothing (for packateer)
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
  If Assigned(FormLoading) Then FormLoading.Hide ;
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

Function TPacketList.DoIShowThis(PacketID : Word;FT : TFilterType;FL : Array of Word) : Boolean ;
Begin
  Result := True ;
  If (FT = ftFilterOff) Then
  Begin
    Result := True ;
    Exit ;
  End;
  If (FT = ftAllowNone) Then
  Begin
    Result := False ;
    Exit ;
  End;
  If (FT = ftShowPackets) Then
  Begin
    Result := WordInArray(PacketID,FL);
    Exit ;
  End;
  If (FT = ftHidePackets) Then
  Begin
    Result := Not WordInArray(PacketID,FL);
    Exit ;
  End;
End;

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
      DoAdd := DoIShowThis(PD.PacketID,FilterOutType,FilterOutList);
    End;

    // In filters
    If (PD.PacketLogType = pltIn) Then
    Begin
      // Incomming
      DoAdd := DoIShowThis(PD.PacketID,FilterInType,FilterInList);
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
