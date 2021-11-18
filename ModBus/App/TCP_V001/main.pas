unit main;

{$mode objfpc}{$H+}

interface

{$define TCP}

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  CommPort,ModBusDriver,
  {$ifdef TCP}
  ModBusTCP,tcp_udpport,
  {$else}
  ModBusSerial, SerialPort,
  {$endif TCP}
  Tag, PLCBlock,
  dsLeds,
  DTAnalogGauge,
  synaser;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnSetCapacity: TButton;
    cmboSerialPorts: TComboBox;
    Edit1: TEdit;
    EditCapacity: TEdit;
    EditVoltage: TEdit;
    EditCurrent: TEdit;
    MemoInfo: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Timer1: TTimer;
    ToggleBoxPower: TToggleBox;
    procedure btnSetCapacityClick({%H-}Sender: TObject);
    procedure cmboSerialPortsChange({%H-}Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormCreate({%H-}Sender: TObject);
    procedure Panel4Resize({%H-}Sender: TObject);
    procedure Timer1Timer({%H-}Sender: TObject);
    procedure ToggleBoxPowerChange(Sender: TObject);
  private
    ModBusPort         : TCommPortDriver;
    ModBusDriver       : TModBusDriver;

    PLCBlockNormal     : TPLCBlock;
    PLCBlockSerial     : TPLCBlock;

    VFase1             : TDTAnalogGauge;
    IFase1             : TDTAnalogGauge;
    RealVoltageDisplay : TdsSevenSegmentMultiDisplay;
    RealCurrentDisplay : TdsSevenSegmentMultiDisplay;

    function GetSerial:string;
    function GetVoltage:double;
    function GetCurrent:double;
    function SetCapacity(value:integer):integer;
    function SetPower(value:boolean):boolean;

    procedure InitMain({%H-}Data: PtrInt=0);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

function ChangeBrightness(lIn: tColor; factor:double): TColor;
var
  lR,lG,lB: byte;
begin
  lR := Red(lIn);
  lG := Green(lIn);
  lB := Blue(lIn);
  result := RGBToColor(Round(lR*factor),Round(lG*factor),Round(lB*factor));
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  {$ifdef TCP}
  ModBusPort:=TTCP_UDPPort.Create(Application);
  with (ModBusPort AS TTCP_UDPPort) do
  begin
    Timeout             := 2000;
    Host                := '192.168.0.26';
    Port                := 502;
  end;
  ModBusDriver:=TModBusTCPDriver.Create(Application);
  with (ModBusDriver AS TModBusTCPDriver) do
  begin
    ReadOnly            := False;
  end;
  {$else}
  ModBusPort:=TSerialPortDriver.Create(Application);
  with (ModBusPort AS TSerialPortDriver) do
  begin
    Timeout             := 100;
    BaudRate            := br9600;
  end;
  ModBusDriver:=TModBusRTUDriver.Create(Application);
  with (ModBusDriver AS TModBusRTUDriver) do
  begin
    ReadOnly            := False;
  end;
  {$endif TCP}

  ModBusDriver.CommunicationPort   := ModBusPort;

  PLCBlockSerial:=TPLCBlock.Create(Application);
  with PLCBlockSerial do
  begin
    TagGUID             := '{71E89502-5945-4755-8F93-57678E1BDD5C}';
    AutoRead            := False;
    AutoWrite           := False;
    PLCRack             := 0;
    PLCSlot             := 0;
    PLCStation          := 1;
    MemFile_DB          := 0;
    MemAddress          := 3;
    MemSubElement       := 0;
    {$ifdef TCP}
    MemReadFunction     := 4;
    {$else}
    MemReadFunction     := 3;
    {$endif}
    MemWriteFunction    := 0;
    ProtocolDriver      := ModBusDriver;
    LongAddress         := '';
    Size                := 10;
    TagType             := pttByte;
    SwapBytes           := True;
    SwapWords           := True;
  end;

  PLCBlockNormal:=TPLCBlock.Create(Application);
  with PLCBlockNormal do
  begin
    TagGUID             := '{AA3C382C-8FA1-4A4E-9C02-0B447925767D}';
    AutoRead            := False;
    AutoWrite           := False;
    PLCRack             := 0;
    PLCSlot             := 0;
    PLCStation          := 1;
    MemFile_DB          := 0;
    MemAddress          := 0;
    MemSubElement       := 0;
    {$ifdef TCP}
    MemReadFunction     := 4;
    {$else}
    MemReadFunction     := 3;
    {$endif}
    MemWriteFunction    := 16;
    ProtocolDriver      := ModBusDriver;
    LongAddress         := '';
    Size                := 1;
    TagType             := pttWord;
  end;

  RealVoltageDisplay:=TdsSevenSegmentMultiDisplay.Create(Panel4);
  with RealVoltageDisplay do
  begin
    Parent:=Panel4;
    OnColor:=clAqua;
    OffColor:=ChangeBrightness(OnColor,0.1);
  end;
  RealCurrentDisplay:=TdsSevenSegmentMultiDisplay.Create(Panel4);
  with RealCurrentDisplay do
  begin
    Parent:=Panel4;
    OnColor:=clRed;
    OffColor:=ChangeBrightness(OnColor,0.1);
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
  {$ifdef TCP}
  cmboSerialPorts.Enabled:=false;
  Edit1Change(Edit1);
  {$else}
  Edit1.Enabled:=false;
  Application.QueueAsyncCall(@InitMain,0);
  {$endif TCP}
end;

procedure TForm1.Panel4Resize(Sender: TObject);
begin
  RealVoltageDisplay.Top:=5;
  RealVoltageDisplay.Left:=5;

  RealVoltageDisplay.Width:=(TControl(Sender).Width DIV 2)-12;
  RealVoltageDisplay.Height:=(TControl(Sender).Height)-12;

  RealCurrentDisplay.Width:=RealVoltageDisplay.Width;
  RealCurrentDisplay.Height:=RealVoltageDisplay.Height;

  RealCurrentDisplay.Left:=RealVoltageDisplay.Width+RealVoltageDisplay.Left+12;
  RealCurrentDisplay.Top:=RealVoltageDisplay.Top;
end;

procedure TForm1.InitMain(Data: PtrInt);
var
  s:string;
  i:integer;
begin
  s:=synaser.GetSerialPortNames;
  {$ifdef UNIX}
  s:=StringReplace(s,'/dev/','',[rfReplaceAll]);
  {$endif UNIX}
  cmboSerialPorts.Items.CommaText:=s;
  if (cmboSerialPorts.Items.Count>0) then
  begin
    i:=0;
    for s in cmboSerialPorts.Items do
    begin
      {$ifdef UNIX}
      if (Pos('ttyUSB',s)>0) then
      begin
        cmboSerialPorts.ItemIndex:=i;
        cmboSerialPortsChange(cmboSerialPorts);
        break;
      end;
      {$endif UNIX}
      Inc(i);
    end;
  end;
  Timer1.Enabled:=ModBusPort.Active;
end;

procedure TForm1.cmboSerialPortsChange(Sender: TObject);
begin
  ModBusPort.Active := False;
  if (cmboSerialPorts.Items.Count>0) then
  begin
    ModBusPort.Active  := False;
    {$ifndef TCP}
    (ModBusPort AS TSerialPortDriver).COMPort := cmboSerialPorts.Text;
    {$endif TCP}
    ModBusPort.Active  := True;
    MemoInfo.Lines.Append('Modbus RS485 Device Connected and Active: '+cmboSerialPorts.Text);
  end;

  if ModBusPort.Active then
  begin
    MemoInfo.Lines.Clear;
    MemoInfo.Lines.Append('Serial: '+GetSerial);
    Timer1.Enabled := True;
  end;

end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  ModBusPort.Active := False;
  {$ifdef TCP}
  (ModBusPort AS TTCP_UDPPort).Host := Edit1.Text;
  {$endif TCP}
  ModBusPort.Active  := True;
  MemoInfo.Lines.Append('Modbus TCP Device Connected and Active: '+Edit1.Text);
  if ModBusPort.Active then
  begin
    MemoInfo.Lines.Clear;
    //MemoInfo.Lines.Append('Serial: '+GetSerial);
    Timer1.Enabled := True;
  end;
end;

procedure TForm1.btnSetCapacityClick(Sender: TObject);
var
  C:integer;
begin
  Timer1.Enabled := False;
  try
    Sleep(100);
    C:=SetCapacity(StrtoInt(EditCapacity.Text));
    MemoInfo.Lines.Append('Capacity: '+InttoStr(C));
  finally
    Timer1.Enabled :=ModBusPort.Active;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  V,I:double;
begin
  {$ifdef TCP}
  MemoInfo.Append('trying to read through ModBus TCP');
  {$else}
  MemoInfo.Append('trying to read through ModBus RS485');
  {$endif TCP}
  V:=GetVoltage;
  EditVoltage.Text:='Voltage: '+FloatToStr(V);
  VFase1.Position:=Round(V);
  RealVoltageDisplay.Value:=V;
  I:=GetCurrent;
  EditCurrent.Text:='Current: '+FloatToStr(I);
  IFase1.Position:=Abs(Round(I));
  RealCurrentDisplay.Value:=I;
end;

procedure TForm1.ToggleBoxPowerChange(Sender: TObject);
var
  aTB:TToggleBox;
  P:boolean;
begin
  Timer1.Enabled := False;
  try
    Sleep(100);

    aTB:=TToggleBox(Sender);
    if aTB.Checked then
    begin
      aTB.Caption:='Power On';
    end
    else
    begin
      aTB.Caption:='Power Off';
    end;

    P:=SetPower(aTB.Checked);

    if P then
      MemoInfo.Lines.Append('Power on.')
    else
      MemoInfo.Lines.Append('Power off.');

  finally
    Timer1.Enabled :=ModBusPort.Active;
  end;

end;

function TForm1.GetSerial:string;
var
  i:integer;
  sd:char;
begin
  result:='';
  PLCBlockSerial.Read;
  for i:=0 to PLCBlockSerial.Size-1 do
  begin
    sd:=chr(Trunc(PLCBlockSerial.ValueRaw[i]));
    if sd in ['0'..'9','A'..'Z','a'..'z'] then result:=result+sd else result:=result+'?';
  end;
end;

function TForm1.GetVoltage:double;
begin
  {$ifdef TCP}
  PLCBlockNormal.MemAddress:=17;
  {$else}
  PLCBlockNormal.MemAddress:=150;
  {$endif TCP}
  PLCBlockNormal.Read;
  result:=PLCBlockNormal.ValueRaw[0]/10;
end;

function TForm1.GetCurrent:double;
var
  aValue:word;
begin
  {$ifdef TCP}
  PLCBlockNormal.MemAddress:=18;
  {$else}
  PLCBlockNormal.MemAddress:=164;
  {$endif TCP}
  PLCBlockNormal.Read;
  aValue:=(Trunc(PLCBlockNormal.ValueRaw[0]) AND  $FFFF);
  result:=PSmallInt(@aValue)^;
end;

function TForm1.SetCapacity(value:integer):integer;
begin
  PLCBlockNormal.MemAddress:=204;
  PLCBlockNormal.ValueRaw[0]:=value;
  PLCBlockNormal.WriteDirect;
  Sleep(250);
  PLCBlockNormal.Read;
  result:=Trunc(PLCBlockNormal.ValueRaw[0]);
end;

function TForm1.SetPower(value:boolean):boolean;
begin
  PLCBlockNormal.MemAddress:=43;
  if value then
    PLCBlockNormal.ValueRaw[0]:=1
  else
    PLCBlockNormal.ValueRaw[0]:=0;
  PLCBlockNormal.WriteDirect;
  Sleep(250);
  PLCBlockNormal.Read;
  result:=(PLCBlockNormal.ValueRaw[0]<>0);
end;


end.

