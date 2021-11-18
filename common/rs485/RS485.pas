unit rs485;

interface

uses
  {$ifdef MSWindows}
  Windows,
  {$endif}
  Forms, Classes, SysUtils, StrUtils, Math, Registry, IniFiles, DateUtils,
  synaser;

type
  TRS485=class
  private
    ComPort1 : TBlockSerial;
    FEmulation:boolean;

    NumError:integer;
    serport,serspeed:integer;

    FErrors:TStringList;
    FInfo:TStringList;

    function GenerateCRC16(s1:ansistring):Word;
    function SendCommand(var data:ansistring):boolean;
    function TypeKFromTtoV(Tref:double):double;
    function TypeKFromVtoT(Vref:double):double;
    function SendRaw(data:byte):boolean;
    function ReadRaw(var data:byte):boolean;

    procedure AddErrors(data:string);
    function  GetErrors:String;
    procedure AddInfo(data:string);
    function  GetInfo:String;

  public
    constructor Create;
    destructor Destroy;override;

    function GetData(Address:word;var t1,t2:double; var w1,w2,kWh,pressurelevel:word):boolean;

    function ReadContoD4(Address:word;var vf1,vf2,vf3,cf1,cf2,cf3:double):boolean;
    function ReadContoD4More(Address:word;var vf1,vf2,vf3,cf1,cf2,cf3:double):boolean;
    function ReadContoD4Power(Address:word;var pf1,pf2,pf3:double):boolean;
    function ReadContoD4Energy(Address:word;var tae,tre:double):boolean;

    property Errors:String read GetErrors;
    property Info:String read GetInfo;
    //property Emulation:boolean read FEmulation;
  end;

implementation

type
  TCommands = (
  	CMD_get_data=50,
  	CMD_reset_counters,
  	CMD_set_settings,
  	CMD_get_settings,
  	CMD_store_settings,
  	CMD_set_cal,
	  CMD_get_cal,
  	CMD_switch_display,
  	CMD_set_text1,
  	CMD_set_text2,
  	CMD_set_text3,
  	CMD_set_text4,
  	CMD_set_text5,
  	CMD_set_text6,
  	CMD_set_output,
    CMD_get_firmware=200,
    CMD_error=250
  );

const
  debugfilename                 = 'debug-rs485.dat';

  PortDelay                     = 100;

  Preamble                      = $AA;

  CMD_preamble1                 = $AA;
  CMD_preamble2                 = $99;

  {CRC8 lookup tables}
  Table : Array[0..255] of integer = (
  0, 94, 188, 226, 97, 63, 221, 131, 194, 156, 126, 32, 163, 253, 31, 65,
  157, 195, 33, 127, 252, 162, 64, 30, 95, 1, 227, 189, 62, 96, 130, 220,
  35, 125, 159, 193, 66, 28, 254, 160, 225, 191, 93, 3, 128, 222, 60, 98,
  190, 224, 2, 92, 223, 129, 99, 61, 124, 34, 192, 158, 29, 67, 161, 255,
  70, 24, 250, 164, 39, 121, 155, 197, 132, 218, 56, 102, 229, 187, 89, 7,
  219, 133, 103, 57, 186, 228, 6, 88, 25, 71, 165, 251, 120, 38, 196, 154,
  101, 59, 217, 135, 4, 90, 184, 230, 167, 249, 27, 69, 198, 152, 122, 36,
  248, 166, 68, 26, 153, 199, 37, 123, 58, 100, 134, 216, 91, 5, 231, 185,
  140, 210, 48, 110, 237, 179, 81, 15, 78, 16, 242, 172, 47, 113, 147, 205,
  17, 79, 173, 243, 112, 46, 204, 146, 211, 141, 111, 49, 178, 236, 14, 80,
  175, 241, 19, 77, 206, 144, 114, 44, 109, 51, 209, 143, 12, 82, 176, 238,
  50, 108, 142, 208, 83, 13, 239, 177, 240, 174, 76, 18, 145, 207, 45, 115,
  202, 148, 118, 40, 171, 245, 23, 73, 8, 86, 180, 234, 105, 55, 213, 139,
  87, 9, 235, 181, 54, 104, 138, 212, 149, 203, 41, 119, 244, 170, 72, 22,
  233, 183, 85, 11, 136, 214, 52, 106, 43, 117, 151, 201, 74, 20, 246, 168,
  116, 42, 200, 150, 21, 75, 169, 247, 182, 232, 10, 84, 215, 137, 107, 53);

  {CRC16 lookup tables}
  Crc16Tab : Array[0..255] of Word = (
  $0000, $1021, $2042, $3063, $4084, $50a5, $60c6, $70e7,
  $8108, $9129, $a14a, $b16b, $c18c, $d1ad, $e1ce, $f1ef,
  $1231, $0210, $3273, $2252, $52b5, $4294, $72f7, $62d6,
  $9339, $8318, $b37b, $a35a, $d3bd, $c39c, $f3ff, $e3de,
  $2462, $3443, $0420, $1401, $64e6, $74c7, $44a4, $5485,
  $a56a, $b54b, $8528, $9509, $e5ee, $f5cf, $c5ac, $d58d,
  $3653, $2672, $1611, $0630, $76d7, $66f6, $5695, $46b4,
  $b75b, $a77a, $9719, $8738, $f7df, $e7fe, $d79d, $c7bc,
  $48c4, $58e5, $6886, $78a7, $0840, $1861, $2802, $3823,
  $c9cc, $d9ed, $e98e, $f9af, $8948, $9969, $a90a, $b92b,
  $5af5, $4ad4, $7ab7, $6a96, $1a71, $0a50, $3a33, $2a12,
  $dbfd, $cbdc, $fbbf, $eb9e, $9b79, $8b58, $bb3b, $ab1a,
  $6ca6, $7c87, $4ce4, $5cc5, $2c22, $3c03, $0c60, $1c41,
  $edae, $fd8f, $cdec, $ddcd, $ad2a, $bd0b, $8d68, $9d49,
  $7e97, $6eb6, $5ed5, $4ef4, $3e13, $2e32, $1e51, $0e70,
  $ff9f, $efbe, $dfdd, $cffc, $bf1b, $af3a, $9f59, $8f78,
  $9188, $81a9, $b1ca, $a1eb, $d10c, $c12d, $f14e, $e16f,
  $1080, $00a1, $30c2, $20e3, $5004, $4025, $7046, $6067,
  $83b9, $9398, $a3fb, $b3da, $c33d, $d31c, $e37f, $f35e,
  $02b1, $1290, $22f3, $32d2, $4235, $5214, $6277, $7256,
  $b5ea, $a5cb, $95a8, $8589, $f56e, $e54f, $d52c, $c50d,
  $34e2, $24c3, $14a0, $0481, $7466, $6447, $5424, $4405,
  $a7db, $b7fa, $8799, $97b8, $e75f, $f77e, $c71d, $d73c,
  $26d3, $36f2, $0691, $16b0, $6657, $7676, $4615, $5634,
  $d94c, $c96d, $f90e, $e92f, $99c8, $89e9, $b98a, $a9ab,
  $5844, $4865, $7806, $6827, $18c0, $08e1, $3882, $28a3,
  $cb7d, $db5c, $eb3f, $fb1e, $8bf9, $9bd8, $abbb, $bb9a,
  $4a75, $5a54, $6a37, $7a16, $0af1, $1ad0, $2ab3, $3a92,
  $fd2e, $ed0f, $dd6c, $cd4d, $bdaa, $ad8b, $9de8, $8dc9,
  $7c26, $6c07, $5c64, $4c45, $3ca2, $2c83, $1ce0, $0cc1,
  $ef1f, $ff3e, $cf5d, $df7c, $af9b, $bfba, $8fd9, $9ff8,
  $6e17, $7e36, $4e55, $5e74, $2e93, $3eb2, $0ed1, $1ef0
  );


procedure Delay(msecs:longword);
var
   FirstTickCount:longword;
begin
  FirstTickCount:=GetTickCount;
  repeat
    sleep(1);
    //Application.ProcessMessages;
  until ((GetTickCount-FirstTickCount) >= msecs);
end;

function crc16normal(Buffer:String;Polynom,Initial:Cardinal):Cardinal;
var
  i,j: Integer;
begin
  Result:=Initial;
  for i:=1 to Length(Buffer) do
  begin
    Result:=Result xor (ord(buffer[i]) shl 8);
    for j:=0 to 7 do
    begin
      if (Result and $8000)<>0
         then Result:=(Result shl 1) xor Polynom
         else Result:=Result shl 1;
    end;
  end;
  Result:=Result and $ffff;
end;

function crc16reverse(Buffer:String;Polynom,Initial:Cardinal):Cardinal;
var
  i,j                   : Integer;
begin
Result:=Initial;
for i:=1 to Length(Buffer) do begin
  Result:=Result xor ord(buffer[i]);
  for j:=0 to 7 do begin
    if (Result and $0001)<>0 then Result:=(Result shr 1) xor Polynom
    else Result:=Result shr 1;
    end;
  end;
end;

Function TRS485.GenerateCRC16(s1:ansistring):Word;
Var crc16:Word; i:Integer;
Begin
  crc16 := $FFFF;
  if Length(s1)>0 then
  begin
    for i := 1 to Length(s1) do
    begin
      Crc16 := Crc16Tab[((Crc16 shr 8 ) xor Ord(s1[i])) and $ff] xor ((Crc16 shl 8) and $FFFF);
    end;
  end;
  Result := crc16;
end;

constructor TRS485.Create;
var
  reg: TRegistry;
  ini:TIniFile;
  F:textfile;
  board: integer;
begin

  inherited Create;

  FEmulation:=False;

  FErrors:=TStringList.Create;
  FInfo:=TStringList.Create;

  Serport       := 13;
  Serspeed      := 9600;
  NumError      := 2;

  {

  reg:=TRegistry.Create;
  with reg do
  begin
    try
      RootKey := HKEY_CURRENT_USER;
      if OpenKey('\Software\Consulab\Sohit\Machines\', True) then
      begin

        if (NOT ValueExists('Serial port'))
           then WriteInteger('Serial port', Serport)
           else Serport:=ReadInteger('Serial port');

        if (NOT ValueExists('Serial speed'))
           then WriteInteger('Serial speed', Serspeed)
           else Serspeed:=ReadInteger('Serial speed');

        if (NOT ValueExists('NumError'))
           then WriteInteger('NumError', NumError)
           else NumError:=ReadInteger('NumError');

        CloseKey;
      end;

    finally
      Free;
    end;
  end;
  }

  Ini := TIniFile.Create( ChangeFileExt( Application.ExeName, '.INI' ) );
  try
    Serport       := Ini.ReadInteger( 'Serial port', 'Serport', Serport );
    Serspeed      := Ini.ReadInteger( 'Serial port', 'Serspeed', Serspeed );
    NumError      := Ini.ReadInteger( 'General', 'NumError', NumError );
    Ini.WriteInteger( 'Serial port', 'Serport', Serport );
    Ini.WriteInteger( 'Serial port', 'Serspeed', Serspeed );
    Ini.WriteInteger( 'General', 'NumError', NumError );
  finally
    Ini.Free;
  end;

  ComPort1:=TBlockSerial.Create;
  Comport1.CloseSocket;
  ComPort1.Connect('COM'+InttoStr(Serport));
  ComPort1.Config(Serspeed,8,'N',SB1,false,false);
  //ComPort1.ConvertLineEnd:=true;
  //ComPort1.EnableRTSToggle(true);     // For driving an MAX485 directly (set send / receive on pins 2+3)
  ComPort1.Purge;
  if Comport1.LastError<>0 then
  begin
    FEmulation:=True;
    {
    MessageDlg ('Sorry, no RS485 found on Comport '+InttoStr(Serport)+' !!'+
                  chr(13)+'Serial port error: '+Comport1.LastErrorDesc+
                  chr(13)+'(Entering emulation mode.)',
                  mtInformation, [mbOk],0);
    }
    exit;
  end;
  ComPort1.Purge;


  //if NOT FileExists(debugfilename) then
  begin
    try
      AssignFile(F, debugfilename);
      Rewrite(F);
    finally
     CloseFile(F);
    end;
  end;

end;

destructor TRS485.Destroy;
begin
  ComPort1.CloseSocket;
  ComPort1.Free;
  FErrors.Free;
  FInfo.Free;
  inherited Destroy;
end;

function TRS485.SendCommand(var data:ansistring):boolean;
var
  crc16:word;
  command,response:ansistring;
  errors,datalength:byte;
  F:text;
  error:boolean;
  databyte,datacounter:byte;
begin
  error:=False;

  result:=False;

  if FEmulation then exit;

  errors:=0;

  command:=data;
  datalength:=length(command)-2;
  if datalength=0
     then command:=command+AnsiChar(0)
     else Insert(AnsiChar(datalength), command, 3);
  command:=AnsiChar(CMD_preamble1)+AnsiChar(CMD_preamble2)+command;
  crc16 := GenerateCRC16(command);
  command:=command+AnsiChar(byte(crc16 DIV 256))+AnsiChar(byte(crc16 MOD 256));

  repeat
    if ComPort1.CanReadEx(0) then
    begin
      response:=ComPort1.RecvPacket(1);
      ComPort1.Purge;
      AssignFile(F,debugfilename);
      Append(F);
      Write(F,DateTimeToStr(Now),'... Waiting data:');
      Writeln(F,response);
      CloseFile(F);
    end;

    ComPort1.Sendstring(command);

    Error:=(Comport1.LastError<>0);

    Delay(1); // wait a short while before going on to give RS485 bus some time

    response:='';
    databyte:=0;

    // receive first pre-amble  integer
    if (NOT Error) then
    begin
      databyte:=Comport1.RecvByte((2+2*errors)*PortDelay);
      Error:=(Comport1.LastError<>0);
    end;

    // receive pre-amble + echo of command integer
    while ( (databyte=Preamble) AND (NOT Error) ) do
    begin
      databyte:=Comport1.RecvByte(PortDelay);
      Error:=(Comport1.LastError<>0);
    end;

    // store command integer
    if (NOT Error) then response:=AnsiChar(databyte);

    // receive length of data
    datalength:=0;
    if (NOT Error) then
    begin
      datalength:=Comport1.RecvByte(PortDelay);
      Error:=(Comport1.LastError<>0);
      if (NOT Error) then response:=response+AnsiChar(datalength);
    end;

    // receive data payload
    datacounter:=0;
    while ( (NOT Error) AND (datacounter<datalength) ) do
    begin
      response:=response+AnsiChar(Comport1.RecvByte(PortDelay));
      Error:=(Comport1.LastError<>0);
      Inc(datacounter);
    end;

    // receive CRC16
    if (NOT Error) then
    begin
      databyte:=Comport1.RecvByte(PortDelay);
      Error:=(Comport1.LastError<>0);
      if (NOT Error) then response:=response+AnsiChar(databyte);
      databyte:=Comport1.RecvByte(PortDelay);
      Error:=(Comport1.LastError<>0);
      if (NOT Error) then response:=response+AnsiChar(databyte);
    end;

    // Is crc ok, is length of response ok
    if (NOT Error) then
    begin
      crc16 := GenerateCRC16(response);
      if ((crc16<>$0000) OR (length(response)<4))
          then Error:=True;
    end;

    if Error then
    //if True then
    begin
      AssignFile(F,debugfilename);
      Append(F);
      Write(F,DateTimeToStr(Now));
      Write(F,' ... Address: ',InttoHex(Ord(command[3]),2));
      Write(F,' ... Command: ',InttoHex(Ord(command[4]),2));
      Write(F,' ... CRC: ',inttostr(crc16));
      Write(F,' ... Response: ',response);
      Write(F,' ... Length response: ', length(response));
      Writeln(F,' ... Command: ', command,'.!!');
      CloseFile(F);
      Delay(PortDelay); // pause a while before next try !!!
    end
    else
    //process returned data .. if valid, return data else set error
    begin
      datalength:=ord(response[2]);
      if length(response)=4+datalength
         then data:=Copy(response,3,datalength)
         else error:=True;
    end;

    if error then Inc(errors);

  until ((NOT error) OR (errors>NumError));

  if (Error) then
  begin
    Delay(PortDelay);
    Comport1.CloseSocket;
    ComPort1.Connect('COM'+InttoStr(Serport));
    ComPort1.Config(Serspeed,8,'N',SB1,false,false);
    ComPort1.Purge;
    Delay(PortDelay);
  end else Delay(1); // wait a short while before going on to give RS485 bus some time

  result:=error;
end;

function TRS485.GetData(Address:word;var t1,t2:double; var w1,w2,kWh,pressurelevel:word):boolean;
var
  data:ansistring;
  error:boolean;
  localpressurelevel:integer;
  localhumidity:double;
  SOH,SOT:word;

  //f:text;
begin
  data:=AnsiChar(Address)+AnsiChar(Byte(CMD_get_data));
  error:=SendCommand(data);
  if ((NOT error) AND (length(Data)>=12)) then
  begin

    t1:=(ord(Data[1])+256*ord(Data[2]))/10;
    SOT:=(ord(Data[3])+256*ord(Data[4]));
    t2:=-39.65+0.01*SOT;

    w1:=ord(Data[5])+256*ord(Data[6]);
    w2:=ord(Data[7])+256*ord(Data[8]);
    kWh:=ord(Data[9])+256*ord(Data[10]);

    SOH:=ord(Data[11])+256*ord(Data[12]);

    localhumidity:=-2.0468+0.0367*SOH-1.5955e-6*SOH*SOH;
    localhumidity:=(t2-25)*(0.01+0.00008*SOH)+localhumidity;
    w2:=round(localhumidity*100);

    //t2:=(ord(Data[3])+256*ord(Data[4]));

  end;
  result:=error;
end;



function TRS485.ReadContoD4(Address:word;var vf1,vf2,vf3,cf1,cf2,cf3:double):boolean;
var
  crc16,errorcount:word;
  command,response:ansistring;
  error:boolean;
  localcf1,localcf2,localcf3:double;
begin
  result:=False;

  if FEmulation then exit;

  error:=False;

  command:=AnsiChar(Address);

  command:=command+AnsiChar($03);

  command:=command+AnsiChar($03);

  command:=command+AnsiChar($0D);

  command:=command+AnsiChar($00);
  command:=command+AnsiChar($06);

  //crc16 := GenerateCRC16(command);
  crc16 := crc16reverse(command,$A001,$FFFF);

  command:=command+AnsiChar(byte(crc16 MOD 256))+AnsiChar(byte(crc16 DIV 256));

  errorcount:=0;

  repeat

    ComPort1.Sendstring(command);

    Error:=(Comport1.LastError<>0);

    if Error then
    begin
      {
      MessageDlg ('Serial port error: '+Comport1.LastErrorDesc,
                  mtInformation, [mbOk],0);
      }
      exit;
    end;

    Delay(1); // wait a short while before going on to give RS485 bus some time

    if (NOT Error) then
    begin
      response:=ComPort1.RecvBufferStr(17,2*PortDelay);
      Error:=(Comport1.LastError<>0);
    end;

    if (NOT Error) then
    begin
       //crc16 := GenerateCRC16(response);
       //if ((crc16<>$0000) OR (length(response)<5))
       //    then Error:=True;
    end;

    //if (NOT Error)  AND (length(response)>=12) then
    if (NOT Error) then
    begin
      try
        localcf1 := Ord(response[4])*$1000000+Ord(response[5])*$10000+Ord(response[6])*$100+Ord(response[7])*$001;
        cf1:=localcf1;
      except
        //on Exception : EOverflow do ShowMessage(Exception.Message);
      end;
      try
        localcf2 := Ord(response[8])*$1000000+Ord(response[9])*$10000+Ord(response[10])*$100+Ord(response[11])*$001;
        cf2:=localcf2;
      except
        //on Exception : EOverflow do ShowMessage(Exception.Message);
      end;
      try
        localcf3 := Ord(response[12])*$1000000+Ord(response[13])*$10000+Ord(response[14])*$100+Ord(response[15])*$001;
        cf3:=localcf3;
      except
        //on Exception : EOverflow do ShowMessage(Exception.Message);
      end;
    end;

    if error then Inc(errorcount);

  until ((NOT error) OR (errorcount>NumError));

  if (Error) then
  begin
    Delay(PortDelay);
    Comport1.CloseSocket;
    ComPort1.Connect('COM'+InttoStr(Serport));
    ComPort1.Config(Serspeed,8,'N',SB1,false,false);
    ComPort1.Purge;
    Delay(PortDelay);
  end else Delay(1); // wait a short while before going on to give RS485 bus some time

  result:=error;

end;

function TRS485.ReadContoD4More(Address:word;var vf1,vf2,vf3,cf1,cf2,cf3:double):boolean;
var
  crc16,i,errorcount:word;
  command,response:ansistring;
  error:boolean;
  localcf1,localcf2,localcf3:double;
  localvf1,localvf2,localvf3:double;
begin
  result:=False;

  if FEmulation then exit;

  error:=False;

  command:=AnsiChar(Address);

  command:=command+AnsiChar($03);

  command:=command+AnsiChar($03);

  command:=command+AnsiChar($01);

  command:=command+AnsiChar($00);
  command:=command+AnsiChar($0C);

  //crc16 := GenerateCRC16(command);
  crc16 := crc16reverse(command,$A001,$FFFF);

  command:=command+AnsiChar(byte(crc16 MOD 256))+AnsiChar(byte(crc16 DIV 256));

  errorcount:=0;

  repeat

    ComPort1.Sendstring(command);

    Error:=(Comport1.LastError<>0);

    if Error then
    begin
      {
      MessageDlg ('Serial port error: '+Comport1.LastErrorDesc,
                  mtInformation, [mbOk],0);
      }
      exit;
    end;

    Delay(1); // wait a short while before going on to give RS485 bus some time

    if (NOT Error) then
    begin
      response:=ComPort1.RecvBufferStr(29,2*PortDelay);
      Error:=(Comport1.LastError<>0);
    end;

    if (NOT Error) then
    begin
       //crc16 := GenerateCRC16(response);
       //if ((crc16<>$0000) OR (length(response)<5))
       //    then Error:=True;
    end;


    if (NOT Error) then
    begin
      i:=4;
      try
        localvf1 := Ord(response[i])*$1000000+Ord(response[i+1])*$10000+Ord(response[i+2])*$100+Ord(response[i+3])*$001;
        vf1:=localvf1;
      except
        //on Exception : EOverflow do ShowMessage(Exception.Message);
      end;
      Inc(i,4);
      try
        localvf2 := Ord(response[i])*$1000000+Ord(response[i+1])*$10000+Ord(response[i+2])*$100+Ord(response[i+3])*$001;
        vf2:=localvf2;
      except
        //on Exception : EOverflow do ShowMessage(Exception.Message);
      end;
      Inc(i,4);
      try
        localvf3 := Ord(response[i])*$1000000+Ord(response[i+1])*$10000+Ord(response[i+2])*$100+Ord(response[i+3])*$001;
        vf3:=localvf3;
      except
        //on Exception : EOverflow do ShowMessage(Exception.Message);
      end;
      Inc(i,4);
      try
        localcf1 := Ord(response[i])*$1000000+Ord(response[i+1])*$10000+Ord(response[i+2])*$100+Ord(response[i+3])*$001;
        cf1:=localcf1;
      except
        //on Exception : EOverflow do ShowMessage(Exception.Message);
      end;
      Inc(i,4);
      try
        localcf2 := Ord(response[i])*$1000000+Ord(response[i+1])*$10000+Ord(response[i+2])*$100+Ord(response[i+3])*$001;
        cf2:=localcf2;
      except
        //on Exception : EOverflow do ShowMessage(Exception.Message);
      end;
      Inc(i,4);
      try
        localcf3 := Ord(response[i])*$1000000+Ord(response[i+1])*$10000+Ord(response[i+2])*$100+Ord(response[i+3])*$001;
        cf3:=localcf3;
      except
        //on Exception : EOverflow do ShowMessage(Exception.Message);
      end;

    end;

    if error then Inc(errorcount);

  until ((NOT error) OR (errorcount>NumError));

  if (Error) then
  begin
    Delay(PortDelay);
    Comport1.CloseSocket;
    ComPort1.Connect('COM'+InttoStr(Serport));
    ComPort1.Config(Serspeed,8,'N',SB1,false,false);
    ComPort1.Purge;
    Delay(PortDelay);
  end else Delay(1); // wait a short while before going on to give RS485 bus some time

  result:=error;

end;


function TRS485.ReadContoD4Power(Address:word;var pf1,pf2,pf3:double):boolean;
var
  crc16,errorcount:word;
  command,response:ansistring;
  error:boolean;
  localpf1,localpf2,localpf3:double;
begin
  result:=False;

  if FEmulation then exit;

  error:=False;

  command:=AnsiChar(Address);

  command:=command+AnsiChar($03);

  command:=command+AnsiChar($03);

  command:=command+AnsiChar($5D);

  command:=command+AnsiChar($00);
  command:=command+AnsiChar($06);

  //crc16 := GenerateCRC16(command);
  crc16 := crc16reverse(command,$A001,$FFFF);

  command:=command+AnsiChar(byte(crc16 MOD 256))+AnsiChar(byte(crc16 DIV 256));

  errorcount:=0;

  repeat

    ComPort1.Sendstring(command);

    Error:=(Comport1.LastError<>0);

    if Error then
    begin
      {
      MessageDlg ('Serial port error: '+Comport1.LastErrorDesc,
                  mtInformation, [mbOk],0);
      }
      exit;
    end;

    Delay(1); // wait a short while before going on to give RS485 bus some time

    if (NOT Error) then
    begin
      response:=ComPort1.RecvBufferStr(17,2*PortDelay);
      Error:=(Comport1.LastError<>0);
    end;

    if (NOT Error) then
    begin
       //crc16 := GenerateCRC16(response);
       //if ((crc16<>$0000) OR (length(response)<5))
       //    then Error:=True;
    end;

    if (NOT Error) then
    begin
      try
        localpf1 := Ord(response[4])*$1000000+Ord(response[5])*$10000+Ord(response[6])*$100+Ord(response[7])*$001;
        pf1:=localpf1;
      except
        //on Exception : EOverflow do ShowMessage(Exception.Message);
      end;
      try
        localpf2 := Ord(response[8])*$1000000+Ord(response[9])*$10000+Ord(response[10])*$100+Ord(response[11])*$001;
        pf2:=localpf2;
      except
        //on Exception : EOverflow do ShowMessage(Exception.Message);
      end;
      try
        localpf3 := Ord(response[12])*$1000000+Ord(response[13])*$10000+Ord(response[14])*$100+Ord(response[15])*$001;
        pf3:=localpf3;
      except
        //on Exception : EOverflow do ShowMessage(Exception.Message);
      end;
    end;

    if error then Inc(errorcount);

  until ((NOT error) OR (errorcount>NumError));

  if (Error) then
  begin
    Delay(PortDelay);
    Comport1.CloseSocket;
    ComPort1.Connect('COM'+InttoStr(Serport));
    ComPort1.Config(Serspeed,8,'N',SB1,false,false);
    ComPort1.Purge;
    Delay(PortDelay);
  end else Delay(1); // wait a short while before going on to give RS485 bus some time

  result:=error;

end;

function TRS485.ReadContoD4Energy(Address:word;var tae,tre:double):boolean;
var
  crc16,errorcount:word;
  command,response:ansistring;
  error:boolean;
  localae,localre:double;
begin
  result:=False;

  if FEmulation then exit;

  error:=False;

  errorcount:=0;

  repeat

    command:=AnsiChar(Address);
    command:=command+AnsiChar($03);
    command:=command+AnsiChar($03);
    command:=command+AnsiChar($25);
    command:=command+AnsiChar($00);
    command:=command+AnsiChar($02);

    crc16 := crc16reverse(command,$A001,$FFFF);

    command:=command+AnsiChar(byte(crc16 MOD 256))+AnsiChar(byte(crc16 DIV 256));

    ComPort1.Sendstring(command);

    Error:=(Comport1.LastError<>0);

    if Error then
    begin
      {
      MessageDlg ('Serial port error: '+Comport1.LastErrorDesc,
                  mtInformation, [mbOk],0);
      }
      exit;
    end;

    Delay(1); // wait a short while before going on to give RS485 bus some time

    if (NOT Error) then
    begin
      response:=ComPort1.RecvBufferStr(9,2*PortDelay);
      Error:=(Comport1.LastError<>0);
    end;

    if (NOT Error) then
    begin
       //crc16 := GenerateCRC16(response);
       //if ((crc16<>$0000) OR (length(response)<5))
       //    then Error:=True;
    end;

    if (NOT Error) then
    begin
      try
        localae := Ord(response[4])*$1000000+Ord(response[5])*$10000+Ord(response[6])*$100+Ord(response[7])*$001;
        tae:=localae;
      except
        //on Exception : EOverflow do ShowMessage(Exception.Message);
      end;
    end;

    if error then Inc(errorcount);

    tre:=0;
    if (FALSE) then
    //if (NOT error) then
    begin

      command:=AnsiChar(Address);
      command:=command+AnsiChar($03);
      command:=command+AnsiChar($03);
      command:=command+AnsiChar($43);
      command:=command+AnsiChar($00);
      command:=command+AnsiChar($02);

      crc16 := crc16reverse(command,$A001,$FFFF);

      command:=command+AnsiChar(byte(crc16 MOD 256))+AnsiChar(byte(crc16 DIV 256));

      ComPort1.Sendstring(command);

      Error:=(Comport1.LastError<>0);

      if Error then
      begin
        {
        MessageDlg ('Serial port error: '+Comport1.LastErrorDesc,
                    mtInformation, [mbOk],0);
        }
        exit;
      end;

      Delay(1); // wait a short while before going on to give RS485 bus some time

      if (NOT Error) then
      begin
        response:=ComPort1.RecvBufferStr(9,2*PortDelay);
        Error:=(Comport1.LastError<>0);
      end;

      if (NOT Error) then
      begin
         //crc16 := GenerateCRC16(response);
         //if ((crc16<>$0000) OR (length(response)<5))
         //    then Error:=True;
      end;

      if (NOT Error) then
      begin
        try
          localre := Ord(response[4])*$1000000+Ord(response[5])*$10000+Ord(response[6])*$100+Ord(response[7])*$001;
          tre:=localre;
        except
          //on Exception : EOverflow do ShowMessage(Exception.Message);
        end;
      end;

    end;


  until ((NOT error) OR (errorcount>NumError));

  if (Error) then
  begin
    Delay(PortDelay);
    Comport1.CloseSocket;
    ComPort1.Connect('COM'+InttoStr(Serport));
    ComPort1.Config(Serspeed,8,'N',SB1,false,false);
    ComPort1.Purge;
    Delay(PortDelay);
  end else Delay(1); // wait a short while before going on to give RS485 bus some time

  result:=error;

end;


function TRS485.TypeKFromTtoV(Tref:double):double;
const
  t0 = -1.7600413686E-2;
  t1 = 3.8921204975E-2;
  t2 = 1.8558770032E-5;
  t3 = -9.9457592874E-8;
  t4 = 3.1840945719E-10;
  t5 = -5.6072844889E-13;
  t6 = 5.6075059059E-16;
  t7 = -3.2020720003E-19;
  t8 = 9.7151147152E-23;
  t9 = -1.2104721275E-26;
begin
  result:=t0+t1*Tref+t2*Power(Tref,2)+t3*Power(Tref,3)+t4*Power(Tref,4)+t5*Power(Tref,5)+t6*Power(Tref,6)+t7*Power(Tref,7)+t8*Power(Tref,8)+t9*Power(Tref,9);
end;


function TRS485.TypeKFromVtoT(Vref:double):double;
const
  v0 = 0;
  v1 = 2.508355E1;
  v2 = 7.860106E-2;
  v3 = -2.503131E-1;
  v4 = 8.315270E-2;
  v5 = -1.228034E-2;
  v6 = 9.804036E-4;
  v7 = -4.413030E-5;
  v8 = 1.057734E-6;
  v9 = -1.052755E-8;
begin
  result:=v0+v1*Vref+v2*Power(Vref,2)+v3*Power(Vref,3)+v4*Power(Vref,4)+v5*Power(Vref,5)+v6*Power(Vref,6)+v7*Power(Vref,7)+v8*Power(Vref,8)+v9*Power(Vref,9);
end;


function TRS485.SendRaw(data:byte):boolean;
begin
  Result:=False;
  Comport1.SendByte(data);
  if Comport1.LastError<>0 then Result:=True;
end;


function TRS485.ReadRaw(var data:byte):boolean;
begin
  result:=False;
  data:=Comport1.RecvByte(PortDelay);
  if Comport1.LastError<>0 then Result:=True;
end;




procedure TRS485.AddErrors(data:string);
begin
  if length(data)>0 then
  begin
   while FErrors.Count>1000 do FErrors.Delete(0);
   FErrors.Append(DateTimeToStr(Now)+': '+data);
  end;
end;

function TRS485.GetErrors:String;
begin
  if FErrors.Count>0 then
  begin
    result:=FErrors.Text;
    FErrors.Clear;
  end else result:='';
end;

procedure TRS485.AddInfo(data:string);
begin
  if Length(data)>0 then
  begin
    while FInfo.Count>1000 do FInfo.Delete(0);
    FInfo.Append(data);
  end;
end;

function TRS485.GetInfo:String;
begin
  if FInfo.Count>0 then
  begin
    result:=FInfo.Text;
    FInfo.Clear;
  end else result:='';
end;


end.
