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

Uses System.SysUtils, DateUtils ;

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
Begin
  Result := 'Unknown' ;
  If (PacketLogType = pltOut) Then
  Case PacketID Of
    $00A : Result := 'Client Connect' ;      // description='(unencrypted/uncompressed) First packet sent when connecting to new zone.'}
    $00C : Result := 'Zone In 1' ;           // description='Likely triggers certain packets to be sent from the server.'}
    $00D : Result := 'Client Leave' ;        // description='Last packet sent from client before it leaves the zone.'}
    $00F : Result := 'Zone In 2' ;           // description='Likely triggers certain packets to be sent from the server.'}
    $011 : Result := 'Zone In 3' ;           // description='Likely triggers certain packets to be sent from the server.'}
    $015 : Result := 'Standard Client' ;     // description='Packet contains data that is sent almost every time (i.e your character\'s position).'}
    $016 : Result := 'Update Request' ;      // description='Packet that requests a PC/NPC update packet.'}
    $017 : Result := 'NPC Race Error' ;      // description='Packet sent in response to impossible incoming NPC packets (like trying to put equipment on a race 0 monster).'}
    $01A : Result := 'Action' ;              // description='An action being done on a target (i.e. an attack or spell).'}
    $01E : Result := 'Volunteer' ;           // description='Sent in response to a /volunteer command.'}
    $028 : Result := 'Drop Item' ;           // description='Drops an item.'}
    $029 : Result := 'Move Item' ;           // description='Move item from one inventory to another.'}
    $02B : Result := 'Translate Request' ;   // description='Request that a phrase be translated.'}
    $032 : Result := 'Offer Trade' ;         // description='This is sent when you offer to trade somebody.'}
    $033 : Result := 'Trade Tell' ;          // description='This packet allows you to accept or cancel a trade request.'}
    $034 : Result := 'Trade Item' ;          // description='Sends the item you want to trade to the server.'}
    $036 : Result := 'Menu Item' ;           // description='Use an item from the item menu.'}
    $037 : Result := 'Use Item' ;            // description='Use an item.'}
    $03A : Result := 'Sort Item' ;           // description='Stacks the items in your inventory. Sent when hitting 'Sort' in the menu.'}
    $03D : Result := 'Blacklist Command' ;   // description='Sent in response to /blacklist add or /blacklist delete.'}
    $041 : Result := 'Lot Item' ;            // description='Lotting an item in the treasure pool.'}
    $042 : Result := 'Pass Item' ;           // description='Passing an item in the treasure pool.'}
    $04B : Result := 'Servmes' ;             // description='Requests the server message (/servmes).'}
    $04D : Result := 'Delivery Box' ;        // description='Used to manipulate the delivery box.'}
    $04E : Result := 'Auction' ;             // description='Used to bid on an Auction House item.'}
    $050 : Result := 'Equip' ;               // description='This command is used to equip your character.'}
    $051 : Result := 'Equipset' ;            // description='This packet is sent when using /equipset.'}
    $052 : Result := 'Equipset Build' ;      // description='This packet is sent when building an equipset.'}
    $053 : Result := 'Lockstyleset' ;        // description='This packet is sent when locking to an equipset.'}
    $059 : Result := 'End Synth' ;           // description='This packet is sent to end a synth.'}
    $05A : Result := 'Conquest' ;            // description='This command asks the server for data pertaining to conquest/besieged status.'}
    $05B : Result := 'Dialog choice' ;       // description='Chooses a dialog option.'}
    $05C : Result := 'Warp Request' ;        // description='Request a warp. Used by teleporters and the like.'}
    $05D : Result := 'Emote' ;               // description='This command is used in emotes.'}
    $05E : Result := 'Request Zone' ;        // description='Request from the client to zone.'}
    $061 : Result := 'Equipment Screen' ;    // description='This command is used when you open your equipment screen.'}
    $063 : Result := 'Digging Finished' ;    // description='This packet is sent when the chocobo digging animation is finished.'}
    $064 : Result := 'New KI examination' ;  // description='Sent when you examine a key item with a 'new' flag on it.'}
    $06E : Result := 'Party invite' ;        // description='Sent when inviting another player to either party or alliance.'}
    $06F : Result := 'Party leave' ;         // description='Sent when leaving the party or alliance.'}
    $070 : Result := 'Party breakup' ;       // description='Sent when disbanding the entire party or alliance.'}
    $071 : Result := 'Kick' ;                // description='Sent when you kick someone from linkshell or party.'}
    $074 : Result := 'Party response' ;      // description='Sent when responding to a party or alliance invite.'}
    $077 : Result := 'Change permissions' ;  // description='Sent when giving party or alliance leader to another player or elevating/decreasing linkshell permissions.'}
    $078 : Result := 'Party list request' ;  // description='Sent when checking the party list.'}
    $083 : Result := 'NPC Buy Item' ;        // description='Buy an item from a generic NPC.'}
    $084 : Result := 'Appraise' ;            // description='Ask server for selling price.'}
    $085 : Result := 'Sell Item' ;           // description='Sell an item from your inventory.'}
    $096 : Result := 'Synth' ;               // description='Packet sent containing all data of an attempted synth.'}
    $0A0 : Result := 'Nominate' ;            // description='Sent in response to a /nominate command.'}
    $0A1 : Result := 'Vote' ;                // description='Sent in response to a /vote command.'}
    $0A2 : Result := 'Random' ;              // description='Sent in response to a /random command.'}
    $0AA : Result := 'Guild Buy Item' ;      // description='Buy an item from a guild.'}
    $0AB : Result := 'Get Guild Inv List' ;  // description='Gets the offerings of the guild.'}
    $0AC : Result := 'Guild Sell Item' ;     // description='Sell an item to the guild.'}
    $0AD : Result := 'Get Guild Sale List' ; // description='Gets the list of things the guild will buy.'}
    $0B5 : Result := 'Speech' ;              // description='Packet contains normal speech.'}
    $0B6 : Result := 'Tell' ;                // description='/tell\'s sent from client.'}
    $0BE : Result := 'Merit Point Increase' ;// description='Sent when you increase a merit point ability.'}
    $0BF : Result := 'Job Point Increase' ;  // description='Sent when you increase a job point ability.'}
    $0C0 : Result := 'Job Point Menu' ;      // description='Sent when you open the Job Point menu and triggers Job Point Information packets.'}
    $0C3 : Result := 'Make Linkshell' ;      // description='Sent in response to the /makelinkshell command.'}
    $0C4 : Result := 'Equip Linkshell' ;     // description='Sent to equip a linkshell.'}
    $0CB : Result := 'Open Mog' ;            // description='Sent when opening or closing your mog house.'}
    $0D2 : Result := 'Party Marker Request' ;// description='Requests map markers for your party.'}
    $0D3 : Result := 'GM Call' ;             // description='Places a call to the GM queue.'}
    $0D4 : Result := 'Help Desk Menu' ;      // description='Opens the Help Desk submenu.'}
    $0DC : Result := 'Type Bitmask' ;        // description='This command is sent when change your party-seek or /anon status.'}
    $0DD : Result := 'Check' ;               // description='Used to check other players.'}
    $0DE : Result := 'Set Bazaar Message' ;  // description='Sets your bazaar message.'}
    $0E0 : Result := 'Search Comment' ;      // description='Sets your search comment.'}
    $0E1 : Result := 'Get LS Message' ;      // description='Requests the current linkshell message.'}
    $0E2 : Result := 'Set LS Message' ;      // description='Sets the current linkshell message.'}
    $0EA : Result := 'Sit' ;                 // description='A request to sit or stand is sent to the server.'}
    $0E7 : Result := 'Logout' ;              // description='A request to logout of the server.'}
    $0E8 : Result := 'Toggle Heal' ;         // description='This command is used to both heal and cancel healing.'}
    $0F1 : Result := 'Cancel' ;              // description='Sent when canceling a buff.'}
    $0F2 : Result := 'Declare Subregion' ;   // description='Sent when moving to a new subregion of a zone (for instance, a different combination of open doors).'}
    $0F4 : Result := 'Widescan' ;            // description='This command asks the server for a widescan.'}
    $0F5 : Result := 'Widescan Track' ;      // description='Sent when you choose to track something on widescan.'}
    $0F6 : Result := 'Widescan Cancel' ;     // description='Sent when you choose to stop track something on widescan.'}
    $0FA : Result := 'Place/Move Furniture' ;// description='Sends new position for your furniture.'}
    $0FB : Result := 'Remove Furniture' ;    // description='Informs the server you have removed some furniture.'}
    $0FC : Result := 'Plant Flowerpot' ;     // description='Plants a seed in a flowerpot.'}
    $0FD : Result := 'Examine Flowerpot' ;   // description='Sent when you examine a flowerpot.'}
    $0FE : Result := 'Uproot Flowerpot' ;    // description='Uproots a flowerpot.'}
    $100 : Result := 'Job Change' ;          // description='Sent when initiating a job change.'}
    $102 : Result := 'Untraditional Equip' ; // description='Sent when equipping a pseudo-item like an Automaton Attachment, Instinct, or Blue Magic Spell.'}
    $104 : Result := 'Leave Bazaar' ;        // description='Sent when client leaves a bazaar.'}
    $105 : Result := 'View Bazaar' ;         // description='Sent when viewing somebody\'s bazaar.'}
    $106 : Result := 'Buy Bazaar Item' ;     // description='Buy an item from somebody\'s bazaar.'}
    $109 : Result := 'Close Bazaar' ;        // description='Sent after closing your bazaar window.'}
    $10A : Result := 'Set Price' ;           // description='Set the price on a bazaar item.'}
    $10B : Result := 'Open Bazaar' ;         // description='Sent when opening your bazaar window to set prices.'}
    $10C : Result := 'Start RoE Quest' ;     // description='Sent to undertake a Records of Eminence Quest.'}
    $10D : Result := 'Cancel RoE Quest' ;    // description='Sent to cancel a Records of Eminence Quest.'}
    $10E : Result := 'Accept RoE Reward' ;   // description='Accept an RoE qust reward that was not given automatically due to inventory restrictions.'}
    $10F : Result := 'Currency Menu' ;       // description='Requests currency information for the menu.'}
    $110 : Result := 'Fishing Action' ;      // description='Sent when casting, releasing a fish, catching a fish, and putting away your fishing rod.'}
    $111 : Result := 'Lockstyle' ;           // description='Sent when using the lockstyle command to lock or unlock.'}
    $112 : Result := 'RoE Log Request' ;     // description='Sent when zoning. Requests the ROE quest log.'}
    $114 : Result := 'HP Map Trigger' ;      // description='Sent when entering a homepoint list for a zone to trigger maps to appear.'}
    $115 : Result := 'Currency Menu 2' ;     // description='Requests currency 2 information for the menu.'}
    $116 : Result := 'Unity Menu' ;          // description='Sent when opening the Status/Unity menu.'}
    $117 : Result := 'Unity Ranking Menu' ;  // description='Sent when opening the Status/Unity/Unity Ranking menu.'}
    $118 : Result := 'Unity Chat Status' ;   // description='Sent when changing unity chat status.'}
  End;

  If (PacketLogType = pltIn) Then
  Case PacketID Of

    $009 : Result := 'Standard Message' ;    // description='A standardized message send from FFXI.'}
    $00A : Result := 'Zone In' ;             // description='Info about character and zone around it.'}
    $00B : Result := 'Zone Out' ;            // description='Packet contains IP and port of next zone to connect to.'}
    $00D : Result := 'PC Update' ;           // description='Packet contains info about another PC (i.e. coordinates).'}
    $00E : Result := 'NPC Update' ;          // description='Packet contains data about nearby targets (i.e. target\'s position, name).'}
    $017 : Result := 'Incoming Chat' ;       // description='Packet contains data about incoming chat messages.'}
    $01B : Result := 'Job Info' ;            // description='Job Levels and levels unlocked.'}
    $01C : Result := 'Inventory Count' ;     // description='Describes number of slots in inventory.'}
    $01D : Result := 'Finish Inventory' ;    // description='Finish listing the items in inventory.'}
    $01E : Result := 'Modify Inventory' ;    // description='Modifies items in your inventory.'}
    $01F : Result := 'Item Assign' ;         // description='Assigns an ID to equipped items in your inventory.'}
    $020 : Result := 'Item Update' ;         // description='Info about item in your inventory.'}
    $021 : Result := 'Trade Requested' ;     // description='Sent when somebody offers to trade with you.'}
    $022 : Result := 'Trade Action' ;        // description='Sent whenever something happens with the trade window.'}
    $023 : Result := 'Trade Item' ;          // description='Sent when an item appears in the trade window.'}
    $025 : Result := 'Item Accepted' ;       // description='Sent when the server will allow you to trade an item.'}
    $026 : Result := 'Count to 80' ;         // description='It counts to 80 and does not have any obvious function. May have something to do with populating inventory.'}
    $027 : Result := 'String Message' ;      // description='Message that includes a string as a parameter.'}
    $028 : Result := 'Action' ;              // description='Packet sent when an NPC is attacking.'}
    $029 : Result := 'Action Message' ;      // description='Packet sent for simple battle-related messages.'}
    $02A : Result := 'Resting Message' ;     // description='Packet sent when you rest in Abyssea.'}
    $02D : Result := 'Kill Message' ;        // description='Packet sent when you gain XP/LP/CP/JP/MP, advance RoE objectives, etc. by defeating a mob.'}
    $02E : Result := 'Mog House Menu' ;      // description='Sent when talking to moogle inside mog house.'}
    $02F : Result := 'Digging Animation' ;   // description='Generates the chocobo digging animation'}
    $030 : Result := 'Synth Animation' ;     // description='Generates the synthesis animation'}
    $031 : Result := 'Synth List' ;          // description='List of recipes or materials needed for a recipe'}
    $032 : Result := 'NPC Interaction 1' ;   // description='Occurs before menus and some cutscenes'}
    $033 : Result := 'String NPC Interaction' ;// description='Triggers a menu or cutscene to appear. Contains 4 strings.'}
    $034 : Result := 'NPC Interaction 2' ;   // description='Occurs before menus and some cutscenes'}
    $036 : Result := 'NPC Chat' ;            // description='Dialog from NPC\'s.'}
    $037 : Result := 'Update Char' ;         // description='Updates a characters stats and animation.'}
    $038 : Result := 'Entity Animation' ;    // description='Sent when a model should play a specific animation.'}
    $039 : Result := 'Env. Animation' ;      // description='Sent to force animations to specific objects.'}
    $03A : Result := 'Independ. Animation' ; // description='Used for arbitrary battle animations that are unaccompanied by an action packet.'}
    $03C : Result := 'Shop' ;                // description='Displays items in a vendors shop.'}
    $03D : Result := 'Value' ;               // description='Returns the value of an item.'}
    $03E : Result := 'Open Buy/Sell' ;       // description='Opens the buy/sell menu for vendors.'}
    $03F : Result := 'Shop Buy Response' ;   // description='Sent when you buy something from normal vendors.'}
    $041 : Result := 'Blacklist' ;           // description='Contains player ID and name for blacklist.'}
    $042 : Result := 'Blacklist Command' ;   // description='Sent in response to /blacklist add or /blacklist delete.'}
    $044 : Result := 'Job Info Extra' ;      // description='Contains information about Automaton stats and set Blue Magic spells.'}
    $047 : Result := 'Translate Response' ;  // description='Response to a translate request.'}
    // not sure what's wrong with this one -->  $04B : Result := 'Logout Acknowledge' ;  // description='Acknowledges a logout attempt.'}
    $04B : Result := 'Delivery Item' ;       // description='Item in delivery box.'}
    $04C : Result := 'Auction House Menu' ;  // description='Sent when visiting auction counter.'}
    $04D : Result := 'Servmes Resp' ;        // description='Server response when someone requests it.'}
    $04F : Result := 'Data Download 2' ;     // description='The data that is sent to the client when it is "Downloading data...".'}
    $050 : Result := 'Equip' ;               // description='Updates the characters equipment slots.'}
    $051 : Result := 'Model Change' ;        // description='Info about equipment and appearance.'}
    $052 : Result := 'NPC Release' ;         // description='Allows your PC to move after interacting with an NPC.'}
    $053 : Result := 'Logout Time' ;         // description='The annoying message that tells how much time till you logout.'}
    $055 : Result := 'Key Item Log' ;        // description='Updates your key item log on zone and when appropriate.'}
    $056 : Result := 'Quest/Mission Log' ;   // description='Updates your quest and mission log on zone and when appropriate.'}
    $057 : Result := 'Weather Change' ;      // description='Updates the weather effect when the weather changes.'}
    $058 : Result := 'Lock Target' ;         // description='Locks your target.'}
    $05A : Result := 'Server Emote' ;        // description='This packet is the server\'s response to a client /emote p.'}
    $05B : Result := 'Spawn' ;               // description='Server packet sent when a new mob spawns in area.'}
    $05C : Result := 'Dialogue Information' ;// description='Used when all the information required for a menu cannot be fit in an NPC Interaction packet.'}
    $05E : Result := 'Camp./Besieged Map' ;  // description='Contains information about Campaign and Besieged status.'}
    $05F : Result := 'Music Change' ;        // description='Changes the current music.'}
    $061 : Result := 'Char Stats' ;          // description='Packet contains a lot of data about your character\'s stats.'}
    $062 : Result := 'Skills Update' ;       // description='Packet that shows your weapon and magic skill stats.'}
    $063 : Result := 'Set Update' ;          // description='Frequently sent packet during battle that updates specific types of job information, like currently available/set automaton equipment and currently set BLU spells.'}
    $065 : Result := 'Repositioning' ;       // description='Moves your character. Seems to be functionally idential to the Spawn packet'}
    $067 : Result := 'Pet Info' ;            // description='Updates information about whether or not you have a pet and the TP, HP, etc. of the pet if appropriate.'}
    $068 : Result := 'Pet Status' ;          // description='Updates information about whether or not you have a pet and the TP, HP, etc. of the pet if appropriate.'}
    $06F : Result := 'Self Synth Result' ;   // description='Results of an attempted synthesis process by yourself.'}
    $070 : Result := 'Others Synth Result' ; // description='Results of an attempted synthesis process by others.'}
    $071 : Result := 'Campaign Map Info' ;   // description='Populates the Campaign map.'}
    $075 : Result := 'Unity Start' ;         // description='Creates the timer and glowing fence that accompanies Unity fights.'}
    $076 : Result := 'Party Buffs' ;         // description='Packet updated every time a party member\'s buffs change.'}
    $078 : Result := 'Proposal' ;            // description='Carries proposal information from a /propose or /nominate command.'}
    $079 : Result := 'Proposal Update' ;     // description='Proposal update following a /vote command.'}
    $082 : Result := 'Guild Buy Response' ;  // description='Buy an item from a guild.'}
    $083 : Result := 'Guild Inv List' ;      // description='Provides the items, prices, and counts for guild inventories.'}
    $084 : Result := 'Guild Sell Response' ; // description='Sell an item to a guild.'}
    $085 : Result := 'Guild Sale List' ;     // description='Provides the items, prices, and counts for guild inventories.'}
    $086 : Result := 'Guild Open' ;          // description='Sent to update the current guild status or open the guild buy/sell menu.'}
    $08C : Result := 'Merits' ;              // description='Contains all merit information. 3 packets are sent.'}
    $08D : Result := 'Job Points' ;          // description='Contains all job point information. 12 packets are sent.'}
    $0A0 : Result := 'Party Map Marker' ;    // description='Marks where players are on your map.'}
    $0AA : Result := 'Spell List' ;          // description='Packet that shows the spells that you know.'}
    $0AC : Result := 'Ability List' ;        // description='Packet that shows your current abilities and traits.'}
    $0AE : Result := 'Mount List' ;          // description='Packet that shows your current mounts.'}
    $0B4 : Result := 'Seek AnonResp' ;       // description='Server response sent after you put up party or anon flag.'}
    $0B5 : Result := 'Help Desk Open' ;      // description='Sent when you open the Help Desk submenu.'}
    $0BF : Result := 'Reservation Response' ;// description='Sent to inform the client about the status of entry to an instanced area.'}
    $0C8 : Result := 'Party Struct Update' ; // description='Updates all party member info in one struct. No player vital data (HP/MP/TP) or names are sent here.'}
    $0C9 : Result := 'Show Equip' ;          // description='Shows another player your equipment after using the Check command.'}
    $0CA : Result := 'Bazaar Message' ;      // description='Shows another players bazaar message after using the Check command or sets your own on zoning.'}
    $0CC : Result := 'Linkshell Message' ;   // description='/lsmes text and headers.'}
    $0D2 : Result := 'Found Item' ;          // description='This command shows an item found on defeated mob or from a Treasure Chest.'}
    $0D3 : Result := 'Lot/drop item' ;       // description='Sent when someone casts a lot on an item or when the item drops to someone.'}
    $0DC : Result := 'Party Invite' ;        // description='Party Invite packet.'}
    $0DD : Result := 'Party Member Update' ; // description='Alliance/party member info - zone, HP%, HP% etc.'}
    $0DF : Result := 'Char Update' ;         // description='A packet sent from server which updates character HP, MP and TP.'}
    $0E0 : Result := 'Linkshell Equip' ;     // description='Updates your linkshell menu with the current linkshell.'}
    $0E1 : Result := 'Party Member List' ;   // description='Sent when you look at the party member list.'}
    $0E2 : Result := 'Char Info' ;           // description='Sends name, HP, HP%, etc.'}
    $0F4 : Result := 'Widescan Mob' ;        // description='Displays one monster.'}
    $0F5 : Result := 'Widescan Track' ;      // description='Updates information when tracking a monster.'}
    $0F6 : Result := 'Widescan Mark' ;       // description='Marks the start and ending of a widescan list.'}
    $0F9 : Result := 'Reraise Activation' ;  // description='Reassigns targetable status on reraise activation?'}
    $0FA : Result := 'Furniture Interact' ;  // description='Confirms furniture manipulation.'}
    $105 : Result := 'Data Download 4' ;     // description='The data that is sent to the client when it is "Downloading data...".'}
    $106 : Result := 'Bazaar Seller Info' ;  // description='Information on the purchase sent to the buyer when they attempt to buy something.'}
    $107 : Result := 'Bazaar closed' ;       // description='Tells you when a bazaar you are currently in has closed.'}
    $108 : Result := 'Data Download 5' ;     // description='The data that is sent to the client when it is "Downloading data...".'}
    $109 : Result := 'Bazaar Purch. Info' ;  // description='Information on the purchase sent to the buyer when the purchase is successful.'}
    $10A : Result := 'Bazaar Buyer Info' ;   // description='Information on the purchase sent to the seller when a sale is successful.'}
    $110 : Result := 'Sparks Update' ;       // description='Occurs when you sparks increase and generates the related message.'}
    $111 : Result := 'Eminence Update' ;     // description='Causes Records of Eminence messages.'}
    $112 : Result := 'RoE Quest Log' ;       // description='Updates your RoE quest log on zone and when appropriate.'}
    $113 : Result := 'Currency Info' ;       // description='Contains all currencies to be displayed in the currency menu.'}
    $115 : Result := 'Fish Bite Info' ;      // description='Contains information about the fish that you hooked.'}
    $116 : Result := 'Equipset Build Response' ; // description='Returned from the server when building a set.'}
    $117 : Result := 'Equipset Response' ;   // description='Returned from the server after the /equipset command.'}
    $118 : Result := 'Currency 2 Info' ;     // description='Contains all currencies to be displayed in the currency menu.'}
    $119 : Result := 'Ability Recasts' ;     // description='Contains the currently available job abilities and their remaining recast times.'}


  End;
End;

Function EquipmentSlotName(SlotID:Byte):String;
Begin
  Result := 'SLOT_0x'+IntToHex(SlotID,2);
  Case SlotID Of
    $00 : Result := 'SLOT_MAIN' ;
    $01 : Result := 'SLOT_SUB' ;
    $02 : Result := 'SLOT_RANGED' ;
    $03 : Result := 'SLOT_AMMO' ;
    $04 : Result := 'SLOT_HEAD' ;
    $05 : Result := 'SLOT_BODY' ;
    $06 : Result := 'SLOT_HANDS' ;
    $07 : Result := 'SLOT_LEGS' ;
    $08 : Result := 'SLOT_FEET' ;
    $09 : Result := 'SLOT_NECK' ;
    $0A : Result := 'SLOT_WAIST' ;
    $0B : Result := 'SLOT_EAR1' ;
    $0C : Result := 'SLOT_EAR2' ;
    $0D : Result := 'SLOT_RING1' ;
    $0E : Result := 'SLOT_RING2' ;
    $0F : Result := 'SLOT_BACK' ;
    $10 : Result := 'SLOT_LINK1' ;
    $11 : Result := 'SLOT_LINK2' ;
  End;
End;

Function ContainerName(ContainerID:Byte):String;
Begin
  Result := 'LOC_0x'+IntToHex(ContainerID,2);
  Case ContainerID Of
    $00 : Result := 'LOC_INVENTORY' ;
    $01 : Result := 'LOC_MOGSAFE' ;
    $02 : Result := 'LOC_STORAGE' ;
    $03 : Result := 'LOC_TEMPITEMS' ;
    $04 : Result := 'LOC_MOGLOCKER' ;
    $05 : Result := 'LOC_MOGSATCHEL' ;
    $06 : Result := 'LOC_MOGSACK' ;
    $07 : Result := 'LOC_MOGCASE' ;
    $08 : Result := 'LOC_WARDROBE' ;
    $09 : Result := 'LOC_MOGSAFE2' ;
    $0A : Result := 'LOC_WARDROBE2' ;
    $0B : Result := 'LOC_WARDROBE3' ;
    $0C : Result := 'LOC_WARDROBE4' ;
  End;
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
{
         1         2         3         4         5         6         7         8         9
123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
      5 | 00 00 00 00 -- -- -- -- -- -- -- -- -- -- -- --    5 | ....------------
}
  If Length(S) <> 81 Then
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
