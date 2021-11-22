unit rs485;

interface

{.$define MOCKANDSTUB}

uses
  Classes, SysUtils,
  synaser;

type
  TBlockSerial = class (synaser.TBlockSerial)
  private const
    // 300V example data:
    // address: 1
    // cmd : 4
    // length : 2
    // data : 300V in 10mV
    // CRC : 2 bytes
  RESPONSEDATA : ansistring = #1 + #4 + #1 + #117 + #48;
  RESPONSEDATA : ansistring = #1 + #4 + #1 + #117 + #48;
  private
    fdataindex : integer;
    fcrc16     : word;
  public
    procedure Purge; override;
    function RecvByte(timeout: integer): byte; override;
  end;

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

    function  SendCommand(var data:ansistring):boolean;

    procedure AddErrors(data:string);
    function  GetErrors:String;
    procedure AddInfo(data:string);
    function  GetInfo:String;
  public
    constructor Create;
    destructor  Destroy;override;

    procedure Connect;

    function  ReadVoltage(const Address:byte;var voltage:double):boolean;

    property  SerialPortName:string write FSerportName;
    property  SerialSpeed:dword write FSerspeed;
    property  Addressdetect:boolean write FAddressdetect;

    property  Errors:String read GetErrors;
    property  Info:String read GetInfo;

    property  Emulation:boolean read FEmulation;
  end;

implementation

const
  debugfilename   = 'debug-rs485.dat';

  PortDelay       = 200;

  CMD_read        = $04;
  CMD_info        = $11;

  REG_voltage     = $1000;
  REG_current     = $1001;


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

function GenerateCRC16(s1:ansistring;seed:word=$FFFF):word;
var
  crc16:Word; i:Integer;
begin
  crc16 := seed;
  if Length(s1)>0 then
  begin
    for i := 1 to Length(s1) do
    begin
      Crc16 := Crc16Tab[((Crc16 shr 8 ) xor Ord(s1[i])) and $ff] xor ((Crc16 shl 8) and $FFFF);
    end;
  end;
  result := crc16;
end;

procedure TBlockSerial.Purge;
begin
  inherited;
  {$ifdef MOCKANDSTUB}
  fdataindex:=1;
  fcrc16 := GenerateCRC16(RESPONSEDATA);
  {$endif MOCKANDSTUB}
end;

function TBlockSerial.RecvByte(timeout: integer): byte;
begin
  {$ifdef MOCKANDSTUB}
  if (fdataindex<=Length(RESPONSEDATA)) then
  begin
    result:=Ord(RESPONSEDATA[fdataindex]);
  end
  else
  begin
    if (fdataindex=(Length(RESPONSEDATA)+1)) then result:=(fcrc16 DIV 256);
    if (fdataindex=(Length(RESPONSEDATA)+2)) then result:=(fcrc16 MOD 256);
    if (fdataindex>(Length(RESPONSEDATA)+2)) then
    begin
      result:=0;
    end;
  end;
  Inc(fdataindex);
  {$else}
  result:=inherited RecvByte(timeout);
  {$endif MOCKANDSTUB}
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
  {$ifdef MSWindows}
  ComPort1.Connect(FSerportName);
  {$else}
  if (Pos('/dev/',FSerportName)<>1) then
    ComPort1.Connect('/dev/'+FSerportName)
  else
    ComPort1.Connect(FSerportName);
  {$endif MSWindows}
  if FAddressdetect
     then ComPort1.Config(FSerspeed,8,'M',SB1,false,false)
     else ComPort1.Config(FSerspeed,8,'N',SB1,false,false);
  //ComPort1.ConvertLineEnd:=true;
  //ComPort1.EnableRTSToggle(true);     // For driving an MAX485 directly (set send / receive on pins 2+3)
  ComPort1.Purge;
  if (Comport1.LastError<>0) then
  begin
    AddInfo(Comport1.LastErrorDesc);
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

function TRS485.SendCommand(var data:ansistring):boolean;
var
  crc16:word;
  command,response:ansistring;
  dataerrors:byte;
  datalength:word;
  F:text;
  error:boolean;
  databyte,datacounter:byte;
begin
  result:=false;

  if FEmulation then exit;
  ComPort1.Purge;

  error:=false;

  dataerrors:=0;

  command:=data;

  datalength:=0;
  if (Ord(command[2])=CMD_read) then
  begin
    if (Length(command)=4) then
    begin
      // we only read a single register at this moment
      // might be changed to read more data at once
      datalength:=1;
    end
    else
    if (Length(command)=5) then
    begin
      // datalength included as a byte in datastring, so use it.
      datalength:=Ord(command[5]);
    end
    else
    if (Length(command)=6) then
    begin
      // datalength included as a word in datastring, so use it.
      datalength:=Ord(command[5])*256+Ord(command[6]);
    end
    else
    begin
      // should not happen !!!
      datalength:=$FFFF;
      AddInfo('Error. Impossible datalength.');
    end;
  end;

  if ((datalength<>0) AND (datalength<>$FFFF)) then
    command:=command+AnsiChar(byte(datalength DIV 256))+AnsiChar(byte(datalength MOD 256));

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

    if (Comport1.LastError<>0) then
    begin
      Error:=True;
      AddInfo('Error. '+Comport1.LastErrorDesc);
      //exit;
    end;

    Delay(10); // wait a short while before going on to give RS485 bus some time

    response:='';

    if Error then
      AddInfo('Send command error.')
    else
      AddInfo('Send command ok. Receiving data.');

    // read address
    if (NOT Error) then
    begin
      databyte:=Comport1.RecvByte(PortDelay);
      if (Comport1.LastError<>0) then
      begin
        Error:=True;
        AddInfo('Error address. '+Comport1.LastErrorDesc);
        //exit;
      end;
      // store address
      if (NOT Error) then response:=response+AnsiChar(databyte);
    end;

    // read command
    if (NOT Error) then
    begin
      databyte:=Comport1.RecvByte(PortDelay);
      if (Comport1.LastError<>0) then
      begin
        Error:=True;
        AddInfo('Error command. '+Comport1.LastErrorDesc);
        //exit;
      end;
      // store command
      if (NOT Error) then response:=response+AnsiChar(databyte);
    end;

    // receive length of data
    datalength:=0;
    if (NOT Error) then
    begin
      datalength:=Comport1.RecvByte(PortDelay);
      if (Comport1.LastError<>0) then
      begin
        Error:=True;
        AddInfo('Error length. '+Comport1.LastErrorDesc);
        //exit;
      end;
      if (NOT Error) then
      begin
        // store length
        response:=response+AnsiChar(datalength);
        // check if we receive bytes or words
        if (Ord(response[2])=CMD_read) then datalength:=datalength*2;
      end;
    end;

    // receive data payload
    datacounter:=0;
    while ((NOT Error) AND (datacounter<datalength)) do
    begin
      // store data
      response:=response+AnsiChar(Comport1.RecvByte(PortDelay));
      if (Comport1.LastError<>0) then
      begin
        Error:=True;
        AddInfo('Error data. '+Comport1.LastErrorDesc);
        //exit;
      end;
      Inc(datacounter);
    end;

    // receive CRC16
    if (NOT Error) then
    begin
      // store crc msb
      databyte:=Comport1.RecvByte(PortDelay);
      if (Comport1.LastError<>0) then
      begin
        Error:=True;
        AddInfo('Error crc lsb. '+Comport1.LastErrorDesc);
        //exit;
      end;
      if (NOT Error) then response:=response+AnsiChar(databyte);
      // store crc lsb
      databyte:=Comport1.RecvByte(PortDelay);
      if (Comport1.LastError<>0) then
      begin
        Error:=True;
        AddInfo('Error crc msb. '+Comport1.LastErrorDesc);
        //exit;
      end;
      if (NOT Error) then response:=response+AnsiChar(databyte);
    end;

    // Is crc ok, is length of response ok
    crc16:=0;
    if ((NOT Error) AND (Length(response)>0)) then
    begin
      crc16 := GenerateCRC16(response);
      if ((crc16<>$0000) OR (length(response)<5)) then
      begin
        Error:=True;
        AddInfo('Response CRC16 error.');
      end;
    end;

    if (True) then
    begin
      AssignFile(F,debugfilename);
      Append(F);
      Write(F,DateTimeToStr(Now));
      if (NOT Error) then AddInfo('. All ok') else AddInfo('. Error !');
      Write(F,'Address: ',InttoHex(Ord(command[1]),2));
      Write(F,' ... Command: ',InttoHex(Ord(command[2]),2));
      Write(F,' ... Register: ',InttoHex(Ord(command[3])*256+Ord(command[4]),4));
      Writeln(F,' ... Command: ', command,'.!!');
      Write(F,'Length response: ', length(response));
      Write(F,' ... CRC response: ',inttostr(crc16));
      if (Length(response)>0) then Write(F,'Address: ',InttoHex(Ord(response[1]),2));
      if (Length(response)>1) then Write(F,' ... Command: ',InttoHex(Ord(response[2]),2));
      if (Length(response)>2) then Write(F,' ... Length reported: ',InttoHex(Ord(response[3]),2));
      Writeln(F,' ... Response: ',response);
      CloseFile(F);
      Delay(PortDelay); // pause a while before next try !!!
    end;

    data:=#0+#0;
    if (NOT Error) then
    //process returned data .. if valid, return data else set error
    begin
      if length(response)=5+datalength
         then data:=Copy(response,4,datalength)
         else error:=True;
    end;

    if error then Inc(dataerrors);

  until ((NOT error) OR (dataerrors>NumError));

  {
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
  }
  result:=error;
end;


function TRS485.ReadVoltage(const Address:byte;var voltage:double):boolean;
var
  data:ansistring;
  error:boolean;
begin
  data:=AnsiChar(chr(Address))+AnsiChar(chr(CMD_read))+AnsiChar(chr(REG_voltage DIV 256))+AnsiChar(chr(REG_voltage MOD 256));

  error:=SendCommand(data);

  // expecting 2 bytes
  if ((NOT error) AND (length(data)=2)) then
  begin
    voltage:=((ord(data[1])*256+ord(data[2]))/100);
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
