unit main;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef MSWindows}
  Windows,
  {$endif}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  CommPort,ModBusDriver,
  ModBusSerial, SerialPort,
  Tag, PLCBlock,
  DTAnalogGauge;

const
  RegisterOffset = $1000;

type
  TBatteryRegisters = (
    VoltageOfBattery,
    CurrentOfBattery,
    RemaininCcapacity,
    AverageOfCellTemperature,
    EnvironmentTemperature,
    WarningFlag,
    ProtectionFlag,
    Status,
    SOC,
    SOH,
    FullChargedCapacity,
    CycleCount,
    MaxChargingCurrent,
    ChargingCurrent,
    MaxCellVoltage,
    MinCellVoltage,
    Reserved1,
    MaxCellTemperature,
    MinCellTemperature,
    FETTemperature,
    Reserved2,
    NominalFloatVoltage,
    DesignCapacity,
    Reserved3);

  { TForm1 }

  TForm1 = class(TForm)
    cmboSerialPorts: TComboBox;
    EditVoltage: TEdit;
    EditCurrent: TEdit;
    EditSOC: TEdit;
    MemoInfo: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Timer1: TTimer;
    procedure cmboSerialPortsChange({%H-}Sender: TObject);
    procedure FormCreate({%H-}Sender: TObject);
    procedure Timer1Timer({%H-}Sender: TObject);
  private
    ModBusPort         : TCommPortDriver;
    ModBusDriver       : TModBusDriver;

    PLCBlockNormal     : TPLCBlock;
    PLCBlockInfo       : TPLCBlock;

    VFase1             : TDTAnalogGauge;
    IFase1             : TDTAnalogGauge;

    function GetINT16(BatteryRegister:TBatteryRegisters):SmallInt;
    function GetUINT16(BatteryRegister:TBatteryRegisters):SmallInt;

    function GetSerial:string;
    function GetVoltage:double;
    function GetCurrent:double;
    function GetSOC:double;

    procedure InitMain({%H-}Data: PtrInt=0);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  Tools;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  ModBusPort:=TSerialPortDriver.Create(Application);
  with (ModBusPort AS TSerialPortDriver) do
  begin
    Timeout             := 200;
    WriteReadDelay      := 100;
    BaudRate            := br9600;
  end;

  ModBusDriver:=TModBusRTUDriver.Create(Application);
  with (ModBusDriver AS TModBusRTUDriver) do
  begin
    ReadOnly            := False;
  end;
  ModBusDriver.CommunicationPort   := ModBusPort;

  PLCBlockNormal:=TPLCBlock.Create(Application);
  with PLCBlockNormal do
  begin
    TagGUID             := '{28C88F3C-4C11-4E40-8B0A-8A2CD4AF25CF}';
    AutoRead            := False;
    AutoWrite           := False;
    PLCRack             := 0;
    PLCSlot             := 0;
    PLCStation          := 1;
    MemFile_DB          := 0;
    MemAddress          := 0;
    MemSubElement       := 0;
    MemReadFunction     := $04;
    MemWriteFunction    := 0;
    ProtocolDriver      := ModBusDriver;
    LongAddress         := '';
    Size                := 1;
    TagType             := pttDefault;
  end;

  PLCBlockInfo:=TPLCBlock.Create(Application);
  with PLCBlockInfo do
  begin
    TagGUID             := '{C0468D27-BB13-4B7F-82C4-196A0DE60575}';
    AutoRead            := False;
    AutoWrite           := False;
    PLCRack             := 0;
    PLCSlot             := 0;
    PLCStation          := 1;
    MemFile_DB          := 0;
    MemAddress          := 0;
    MemSubElement       := 0;
    MemReadFunction     := $11;
    MemWriteFunction    := 0;
    ProtocolDriver      := ModBusDriver;
    LongAddress         := '';
    Size                := 39;
    TagType             := pttDefault;
  end;

  VFase1:=TDTAnalogGauge.Create(Application);
  with VFase1 do
  begin
    Parent:=Panel2;
    ScaleSettings.Maximum:=300;
    ScaleSettings.Angle:=280;
    ScaleSettings.MainTickCount:=10;
    Align:=alClient;
  end;

  IFase1:=TDTAnalogGauge.Create(Application);
  with IFase1 do
  begin
    Parent:=Panel3;
    ScaleSettings.Maximum:=10;
    ScaleSettings.Angle:=280;
    ScaleSettings.MainTickCount:=10;
    Align:=alClient;
  end;

  Application.QueueAsyncCall(@InitMain,0);
end;

procedure TForm1.InitMain(Data: PtrInt);
{$ifdef UNIX}
var
  com:string;
  i:integer;
{$endif UNIX}
begin
  EnumerateCOMPorts(cmboSerialPorts.Items);
  {$ifdef UNIX}
  // Make life easy on RPi: pick first available USB serial port.
  // Not necessary correct, but ok for testing.
  i:=0;
  for com in cmboSerialPorts.Items do
  begin
    if (Pos('ttyUSB',com)>0) then
    begin
      cmboSerialPorts.ItemIndex:=i;
      cmboSerialPortsChange(cmboSerialPorts);
      break;
    end;
    Inc(i);
  end;
  {$endif UNIX}
end;

procedure TForm1.cmboSerialPortsChange(Sender: TObject);
begin
  ModBusPort.Active := False;
  if (cmboSerialPorts.Items.Count>0) AND (cmboSerialPorts.ItemIndex<>-1) then
  begin
    ModBusPort.Active  := False;
    (ModBusPort AS TSerialPortDriver).COMPort := cmboSerialPorts.Text;
    ModBusPort.Active  := True;
    MemoInfo.Lines.Append('Modbus Device Connected and Active: '+cmboSerialPorts.Text);
  end;

  if ModBusPort.Active then
  begin
    Sleep(500);
    MemoInfo.Lines.Append('Serial: '+GetSerial);
    Timer1.Enabled := True;
  end;

  MemoInfo.Lines.Append(GetEnumNameSimple(TypeInfo(TBatteryRegisters),Ord(TBatteryRegisters.AverageOfCellTemperature)));
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  V,I,SOC:double;
begin
  V:=GetVoltage;
  EditVoltage.Text:='Voltage: '+FloatToStr(V);
  VFase1.Position:=Round(V);
  I:=GetCurrent;
  EditCurrent.Text:='Current: '+FloatToStr(I);
  IFase1.Position:=Round(I);
  SOC:=GetSOC;
  EditSOC.Text:='SOC: '+FloatToStr(SOC);
end;

function TForm1.GetSerial:string;
var
  j:Cardinal;
  sd:char;
begin
  result:='Model: ';
  PLCBlockInfo.Read;
  j:=0;
  while ((j<16) AND (j<PLCBlockInfo.Size)) do
  begin
    sd:=chr(Trunc(PLCBlockInfo.ValueRaw[j]));
    Inc(j);
    if sd='*' then break;
    result:=result+sd;
  end;
  result:=result+'. SV: '+InttoHex(Trunc(PLCBlockInfo.ValueRaw[j])*256+Trunc(PLCBlockInfo.ValueRaw[j+1]),4);
  Inc(j,2);
  result:=result+'. HV: '+InttoHex(Trunc(PLCBlockInfo.ValueRaw[j])*256+Trunc(PLCBlockInfo.ValueRaw[j+1]),4);
  Inc(j,2);
  while (j<PLCBlockInfo.Size) do
  begin
    sd:=chr(Trunc(PLCBlockInfo.ValueRaw[j]));
    Inc(j);
    if sd='*' then break;
  end;
  result:=result+'. Serial: ';
  while (j<PLCBlockInfo.Size) do
  begin
    sd:=chr(Trunc(PLCBlockInfo.ValueRaw[j]));
    result:=result+sd;
    Inc(j);
  end;
end;

function TForm1.GetINT16(BatteryRegister:TBatteryRegisters):SmallInt;
var
  aValue:word;
begin
  PLCBlockNormal.MemAddress:=RegisterOffset+Ord(BatteryRegister);
  PLCBlockNormal.Read;
  aValue:=(Trunc(PLCBlockNormal.ValueRaw[0]) AND  $FFFF);
  result:=(PSmallInt(@aValue)^);
end;

function TForm1.GetUINT16(BatteryRegister:TBatteryRegisters):SmallInt;
begin
  PLCBlockNormal.MemAddress:=RegisterOffset+Ord(BatteryRegister);
  PLCBlockNormal.Read;
  result:=(Trunc(PLCBlockNormal.ValueRaw[0]) AND  $FFFF);
end;

function TForm1.GetVoltage:double;
begin
  result:=(GetUINT16(TBatteryRegisters.VoltageOfBattery)/100);
end;

function TForm1.GetCurrent:double;
begin
  result:=(GetINT16(TBatteryRegisters.CurrentOfBattery)/100);
end;

function TForm1.GetSOC:double;
begin
  result:=(GetUINT16(TBatteryRegisters.SOC)/10);
end;

end.

