unit datalookups;

interface

Uses System.Classes ;

CONST
  LU_PacketOut = 'out' ;
  LU_PacketIn = 'in' ;
  LU_Zones = 'zones' ;
  LU_EquipmentSlots = 'equipslot' ;
  LU_Container = 'containers' ;
  LU_Item = 'items' ;
  LU_ItemModel = 'itemmodels' ;
  LU_Music = 'music';
  LU_Job = 'jobs' ;
  LU_Weather = 'weather' ;
  LU_Merit = 'merits' ;
  LU_JobPoint = 'jobpoints' ;
  LU_Spell = 'spells' ;
  LU_WeaponSkill = 'weaponskill' ;
  LU_Ability = 'ability' ;
  LU_ARecast = 'abilityrecast' ;
  LU_PetCommand = 'petcommand' ;
  LU_Trait = 'trait' ;
  LU_Mounts = 'mounts' ;
  LU_RoE = 'roe' ;

TYPE
  TDataLookupEntry = Class
    ID : Int64 ;
    Val : String ;
    Extra : String ;
  End;
  TDataLookup = Class
  Private
    fList : TList ;

  Public
    Constructor Create(FN : String);
    Destructor Destroy ; Override ;
    Procedure LoadFromFile(FN : String);
    Function GetVal(ID : Int64):String;
    Function GetExtra(ID : Int64):String;
    Procedure AddEntry(ID : Int64 ; Val , Extra : String);
    Function Count : Integer ;
    Function GetItem(Index : Integer):TDataLookupEntry;
  End;

// Name LookUp function
Function NLU(Source:String):TDataLookup ;

{
VAR
  Zones : TDataLookup ;
  PacketOutNames  : TDataLookup ;
  PacketInNames : TDataLookup ;
  EquipmentSlots : TDataLookup ;
  ContainerNames : TDataLookup ;
  ItemNames : TDataLookup ;
  ItemModelNames : TDataLookup ;
  MusicNames : TDataLookup ;
  JobNames : TDataLookup ;
  WeatherNames : TDataLookup ;
  MeritNames : TDataLookup ;
  JobPointNames : TDataLookup ;
  SpellNames : TDataLookup ;
}

implementation

uses Vcl.Forms, System.SysUtils ;

TYPE
  TDataLookupPair = Class
  Public
    Source : String ;
    DL : TDataLookup ;
  End;

VAR
  LookupList : Array of TDataLookupPair ;
  NullLookup : TDataLookup ;

Function NLU(Source:String):TDataLookup ;
VAR
  I : Integer ;
  S : String ;
Begin
  S := LowerCase(Source);
  For I := 0 to Length(LookupList)-1 Do
  If LookupList[I].Source = S Then
  Begin
    Result := LookupList[I].DL ;
    Exit ;
  End;
  Result := NullLookup ;
End;


procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings) ;
begin
   ListOfStrings.Clear;
   ListOfStrings.Delimiter       := Delimiter;
   ListOfStrings.StrictDelimiter := True;
   ListOfStrings.DelimitedText   := Str;
end;


Constructor TDataLookup.Create(FN : String);
Begin
  Inherited Create ;
  fList := TList.Create();
  If (FN <> '') Then
    LoadFromFile({ExtractFilePath(Application.ExeName) + }FN);
End;

Destructor TDataLookup.Destroy ;
Begin
  FreeAndNil(fList);
  Inherited Create ;
End;

Procedure TDataLookup.LoadFromFile(FN : String);
VAR
  SL, Line : TStringList ;
  I : Integer ;
  N : Int64 ;
Begin
  // Clear our stuff
  fList.Clear ;
  Try
    SL := TStringList.Create ;
    Line := TStringList.Create ;
    If FileExists(FN) Then
      SL.LoadFromFile(FN)
    Else
      SL.Clear ;

    For I := 0 To SL.Count-1 Do
    Begin
      // loop all the lines
      Split(';',SL[I],Line);
      If (Line.Count >= 2) Then
      Begin
        N := StrToInt64Def(Line[0],-1);
        If (N <> -1) Then
          If (Line.Count >= 3) Then
            AddEntry(N,Line[1],Line[2])
          Else
            AddEntry(N,Line[1],'');
      End;
    End;

  Finally
    FreeAndNil(SL);
    FreeAndNil(Line);
  End;
End;

Function TDataLookup.GetVal(ID : Int64):String;
VAR
  I : Integer ;
Begin
  Result := '' ;
  For I := 0 To fList.Count-1 Do
  If TDataLookupEntry(fList.Items[I]).ID = ID Then
  Begin
    Result := TDataLookupEntry(fList.Items[I]).Val ;
  End;
End;

Function TDataLookup.GetExtra(ID : Int64):String;
VAR
  I : Integer ;
Begin
  Result := '' ;
  For I := 0 To fList.Count-1 Do
  If TDataLookupEntry(fList.Items[I]).ID = ID Then
  Begin
    Result := TDataLookupEntry(fList.Items[I]).Extra ;
  End;
End;

Procedure TDataLookup.AddEntry(ID : Int64 ; Val , Extra : String);
VAR
  DLE : TDataLookupEntry ;
Begin
  DLE := TDataLookupEntry.Create ;
  DLE.ID := ID ;
  DLE.Val := Val;
  DLE.Extra := Extra ;
  fList.Add(DLE);
End;

Function TDataLookup.Count : Integer ;
Begin
  Result := fList.Count ;
End;

Function TDataLookup.GetItem(Index : Integer):TDataLookupEntry;
Begin
  Result := TDataLookupEntry(fList.Items[Index]);
End;

Procedure LoadLookups ;
VAR
  DI : TSearchRec ;
  Res : Integer ;
  LUP : TDataLookupPair ;
  FP : String ;
Begin
  FP := ExtractFilePath(Application.ExeName) + 'lookup\' ;
  Res := FindFirst(FP + '*.txt',faAnyFile,DI);
  While Res = 0 Do
  Begin
    LUP := TDataLookupPair.Create ;
    LUP.Source := LowerCase(ChangeFileExt(DI.Name,''));
    LUP.DL := TDataLookup.Create(FP+DI.Name);

    SetLength(LookupList,Length(LookupList)+1);
    LookupList[Length(LookupList)-1] := LUP ;

    Res := FindNext(DI);
  End;
  FindClose(DI);
End;

Initialization

NullLookup := TDataLookup.Create('');
LoadLookups();
{
Zones := TDataLookup.Create('lookup\zones.txt');
PacketOutNames := TDataLookup.Create('lookup\out.txt');
PacketInNames := TDataLookup.Create('lookup\in.txt');
EquipmentSlots := TDataLookup.Create('lookup\equipslot.txt');
ContainerNames := TDataLookup.Create('lookup\containers.txt'); ;
ItemNames := TDataLookup.Create('lookup\items.txt'); ;
ItemModelNames := TDataLookup.Create('lookup\itemmodels.txt'); ;
MusicNames := TDataLookup.Create('lookup\music.txt'); ;
JobNames := TDataLookup.Create('lookup\jobs.txt'); ;
WeatherNames := TDataLookup.Create('lookup\weather.txt'); ;
MeritNames := TDataLookup.Create('lookup\merits.txt'); ;
JobPointNames := TDataLookup.Create('lookup\jobpoints.txt');
SpellNames := TDataLookup.Create('lookup\spells.txt');
}

Finalization

FreeAndNil(NullLookup);
{
FreeAndnil(Zones);
FreeAndnil(PacketOutNames);
FreeAndnil(PacketInNames);
FreeAndnil(EquipmentSlots);
FreeAndnil(ContainerNames);
FreeAndNil(ItemNames);
FreeAndNil(ItemModelNames);
FreeAndNil(MusicNames);
FreeAndNil(JobNames);
FreeAndNil(WeatherNames);
FreeAndNil(MeritNames);
FreeAndNil(JobPointNames);
FreeAndNil(SpellNames);
}

end.
