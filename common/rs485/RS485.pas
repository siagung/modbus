unit rs485;

interface

uses
  Classes, SysUtils, StrUtils,
  synaser;

type
  TRS485=class
  private
    ComPort1          : TBlockSerial;

    FEmulation        : boolean;
    FAddressdetect    : boolean;

    FSerportName      : string;
    FSerspeed         : dword;

    NumError          : integer;

    FErrors           : TStringList;
    FInfo             : TStringList;

    Function GenerateCRC16(s1:ansistring):Word;
    function SendCommand(var data:ansistring):boolean;

    procedure AddErrors(data:string);
    function  GetErrors:String;
    procedure AddInfo(data:string);
    function  GetInfo:String;

    function ReadVoltage(board,position:integer;var voltage:double):boolean;

  public
    constructor Create;
    destructor  Destroy;override;

    procedure Connect;

    function  ReadTemperature(board:integer;var temperature:single):boolean;

    function  GetFirmwareVersion(board:integer;var firmware,release:integer):boolean;

    property  SerialPortName:string write FSerportName;
    property  SerialSpeed:dword write FSerspeed;
    property  Addressdetect:boolean write FAddressdetect;

    property  Errors:String read GetErrors;
    property  Info:String read GetInfo;

    property  Emulation:boolean read FEmulation;
  end;

implementation

const
  debugfilename                 = 'debug-rs485.dat';

  Vmax                          = 5000/1000;

  Vref                          = 600/1000;

  PortDelay                     = 100;

  Temp_scale                    = 1/100;  // 10mV per degree C

  Preamble                      = $AA;

  CMD_start                     = $02;

  CMD_read_voltage              = $50;

  CMD_read_temperature          = $80;

  CMD_write_address_eeprom      = $C0;
  CMD_write_correction_eeprom   = $C1;
  CMD_read_correction_eeprom    = $C2;

  CMD_read_counter              = $91;

  CMD_get_parameters            = $F3;

  CMD_get_firmware_version      = $FE;
  CMD_reset_board               = $FF;

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
   FirstTickCount:QWord;
begin
  FirstTickCount:=GetTickCount64;
  repeat
    sleep(1);
    //Application.ProcessMessages;
  until ((GetTickCount64-FirstTickCount) >= msecs);
end;

constructor TRS485.Create;
var
  F:textfile;
begin
  inherited Create;

  ComPort1:=TBlockSerial.Create;

  FErrors:=TStringList.Create;
  FInfo:=TStringList.Create;


  FEmulation     := False;
  FSerportName   := 'COM1';
  FSerspeed      := 9600;
  FAddressdetect := False;
  NumError       := 2;

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

procedure TRS485.Connect;
begin
  Comport1.CloseSocket;
  ComPort1.Connect(FSerportName);
  if FAddressdetect
     then ComPort1.Config(FSerspeed,8,'M',SB1,false,false)
     else ComPort1.Config(FSerspeed,8,'N',SB1,false,false);
  //ComPort1.ConvertLineEnd:=true;
  //ComPort1.EnableRTSToggle(true);     // For driving an MAX485 directly (set send / receive on pins 2+3)
  ComPort1.Purge;
  if (Comport1.LastError<>0) then
  begin
    FEmulation:=True;
    exit;
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
End;


function TRS485.SendCommand(var data:ansistring):boolean;
var
  crc16:word;
  command,response:ansistring;
  dataerrors,datalength:byte;
  F:text;
  error:boolean;
  databyte,datacounter:byte;
begin
  error:=False;

  result:=False;

  if FEmulation then exit;

  dataerrors:=0;

  command:=data;
  datalength:=length(command)-2;
  if datalength=0
     then command:=command+AnsiChar(0)
     else Insert(AnsiChar(datalength), command, 3);
  if (NOT FAddressdetect) then command:=AnsiChar(CMD_start)+command;
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

    if FAddressdetect then
    begin
      ComPort1.SetParity('M');
      ComPort1.Sendstring(Copy(command,1,1));// send address !!!
      ComPort1.SetParity('S');
      ComPort1.Sendstring(Copy(command,2,25));// send rest of command
    end else ComPort1.Sendstring(command);

    Error:=(Comport1.LastError<>0);

    Delay(1); // wait a short while before going on to give RS485 bus some time

    response:='';
    databyte:=0;

    // receive first pre-amble  integer
    if (NOT Error) then
    begin
      databyte:=Comport1.RecvByte((2+2*dataerrors)*PortDelay);
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
    begin
      AssignFile(F,debugfilename);
      Append(F);
      Write(F,DateTimeToStr(Now));
      if FAddressdetect then
      begin
        Write(F,' ... Address: ',InttoHex(Ord(command[1]),2));
        Write(F,' ... Command: ',InttoHex(Ord(command[2]),2));
      end
      else
      begin
        Write(F,' ... Address: ',InttoHex(Ord(command[2]),2));
        Write(F,' ... Command: ',InttoHex(Ord(command[3]),2));
      end;
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

    if error then Inc(dataerrors);

  until ((NOT error) OR (dataerrors>NumError));

  if (Error) then
  begin
    Delay(PortDelay);
    Comport1.CloseSocket;
    ComPort1.Connect(FSerportName);
    if FAddressdetect
       then ComPort1.Config(FSerspeed,8,'M',SB1,false,false)
       else ComPort1.Config(FSerspeed,8,'N',SB1,false,false);
    ComPort1.Purge;
    Delay(PortDelay);
  end else Delay(1); // wait a short while before going on to give RS485 bus some time

  result:=error;
end;


function TRS485.ReadVoltage(board,position:integer;var voltage:double):boolean;
var
  data:ansistring;
  error:boolean;
  result1,result2:UInt64;
  //f:text;
begin
  //data:=AnsiChar(chr(Address_data[board].Address))+AnsiChar(chr(CMD_read_voltage+position-1));
  //data:=AnsiChar(NewController[board].Address_data)+AnsiChar(CMD_read_voltage+position-1);
  error:=SendCommand(data);

  // expecting 2 longs (sum-value + average count)
  if ((NOT error) AND (length(data)=8)) then
  begin
    result1:=ord(data[4]);
    result1:=result1*256+ord(data[3]);
    result1:=result1*256+ord(data[2]);
    result1:=result1*256+ord(data[1]);
    result2:=ord(data[8]);
    result2:=result2*256+ord(data[7]);
    result2:=result2*256+ord(data[6]);
    result2:=result2*256+ord(data[5]);
    if (result2>0) then
    begin
      voltage:=1000*result1/(result2*1023);
    end;
  end;
  //voltage:=result2;
  result:=error;
end;


function TRS485.ReadTemperature(board:integer;var temperature:single):boolean;
var
  error:boolean;
  dataerrors:integer;
  data:ansistring;
  tempstore:double;
  TempWord:word;
  crc8:integer;
begin
  error:=false;

  error:=ReadVoltage(board,1,tempstore);

  if (NOT error) then
  begin
    temperature:=tempstore*100;
    //error:=ReadRefVoltage(board,voltage);
    //if (NOT error) then temperature:=temperature*(Vref/voltage);
  end;

  {
  errors:=0;
  repeat
    Inc(errors);
    data:=AnsiChar(NewController[board].Address_data)+AnsiChar(CMD_read_temperature+1);
    error:=SendCommand(data);

    if ((NOT error) AND (length(data)=2)) then
    begin
      TempWord := (256*ord(data[2])+ord(data[1]));
      if TempWord<256
         then temperature:=(TempWord/2)
         else temperature:=-1*(((NOT TempWord)+1)/2);
    end;

    if ((NOT error) AND ((length(data)=4) OR (length(data)=5))) then
    begin
      tempstore:=0;
      if ord(data[4])>0 then tempstore:=(25/100)-((ord(data[4])-ord(data[3]))/ord(data[4]));
      //if ord(data[4])-ord(data[3])=1 then error !!!!!!!!!!!!!!!!!!!
      TempWord := ((256*ord(data[2])+ord(data[1])) AND $FFFE);
      if TempWord<256
         then temperature:=(TempWord/2) - tempstore
         else temperature:=-1*(((NOT TempWord)+1)/2) - tempstore;
    end;

    if ((NOT error) AND (length(data)=9)) then
    begin
      CRC8:=0;
      for TempWord := 1 to 9 do CRC8 := Table[CRC8 xor ord(data[TempWord])];
      if CRC8=0 then
      begin
        tempstore:=0;
        if ord(data[8])>0 then tempstore:=(25/100)-((ord(data[8])-ord(data[7]))/ord(data[8]));
        //if ord(data[4])-ord(data[3])=1 then error !!!!!!!!!!!!!!!!!!!
        TempWord := ((256*ord(data[2])+ord(data[1])) AND $FFFE);
        if TempWord<256
           then temperature:=(TempWord/2) - tempstore
           else temperature:=-1*(((NOT TempWord)+1)/2) - tempstore;
      end else error:=True;
    end;
  until ((NOT error) OR (errors>NumError));
  }

  result:=error;

end;

function TRS485.GetFirmwareVersion(board:integer;var firmware,release:integer):boolean;
var
  data:ansistring;
  error:boolean;
begin
  //read firmware
  //data:=AnsiChar(NewController[board].Address_data)+AnsiChar(CMD_get_parameters);
  data:=data+AnsiChar(4);
  error:=SendCommand(data);
  if ((NOT error) AND (length(data)=3)) then
  begin
    firmware:=ord(data[2]);
    release:=ord(data[3]);
  end;
  if ((NOT error) AND (length(data)=2)) then
  begin
    firmware:=ord(data[1]);
    release:=ord(data[2]);
  end;

  result:=error;
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
