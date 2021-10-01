unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ModBusSerial, SerialPort, Tag, PLCBlock,
  DTAnalogGauge,
  synaser;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnSetCapacity: TButton;
    cmboSerialPorts: TComboBox;
    EditCapacity: TEdit;
    EditVoltage: TEdit;
    EditCurrent: TEdit;
    MemoInfo: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Timer1: TTimer;
    ToggleBoxPower: TToggleBox;
    procedure btnSetCapacityClick({%H-}Sender: TObject);
    procedure cmboSerialPortsChange({%H-}Sender: TObject);
    procedure FormCreate({%H-}Sender: TObject);
    procedure Timer1Timer({%H-}Sender: TObject);
    procedure ToggleBoxPowerChange(Sender: TObject);
  private
    SerialPortDriver1  : TSerialPortDriver;
    ModBusRTUDriver1   : TModBusRTUDriver;
    PLCBlockNormal     : TPLCBlock;
    PLCBlockSerial     : TPLCBlock;

    VFase1             : TDTAnalogGauge;
    IFase1             : TDTAnalogGauge;

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

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  SerialPortDriver1:=TSerialPortDriver.Create(Application);
  with SerialPortDriver1 do
  begin
    Timeout             := 100;
    BaudRate            := br9600;
  end;

  ModBusRTUDriver1:=TModBusRTUDriver.Create(Application);
  with ModBusRTUDriver1 do
  begin
    CommunicationPort   := SerialPortDriver1;
    ReadOnly            := False;
  end;

  PLCBlockSerial:=TPLCBlock.Create(Application);
  with PLCBlockSerial do
  begin
    TagGUID             := '{35F369DA-3F0D-4D13-9BFF-13438D9E8097}';
    AutoRead            := False;
    AutoWrite           := False;
    PLCRack             := 0;
    PLCSlot             := 0;
    PLCStation          := 1;
    MemFile_DB          := 0;
    MemAddress          := 3;
    MemSubElement       := 0;
    MemReadFunction     := 3;
    MemWriteFunction    := 0;
    ProtocolDriver      := ModBusRTUDriver1;
    LongAddress         := '';
    Size                := 10;
    TagType             := pttByte;
    SwapBytes           := True;
    SwapWords           := True;
  end;

  PLCBlockNormal:=TPLCBlock.Create(Application);
  with PLCBlockNormal do
  begin
    TagGUID             := '{A65D2B60-6D8A-4F30-A308-59051F51BE03}';
    AutoRead            := False;
    AutoWrite           := False;
    PLCRack             := 0;
    PLCSlot             := 0;
    PLCStation          := 1;
    MemFile_DB          := 0;
    MemAddress          := 0;
    MemSubElement       := 0;
    MemReadFunction     := 3;
    MemWriteFunction    := 16;
    ProtocolDriver      := ModBusRTUDriver1;
    LongAddress         := '';
    Size                := 1;
    TagType             := pttWord;
    SwapWords           := True;
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
  Timer1.Enabled:=SerialPortDriver1.Active;
end;

procedure TForm1.cmboSerialPortsChange(Sender: TObject);
begin
  SerialPortDriver1.Active := False;
  if (cmboSerialPorts.Items.Count>0) then
  begin
    SerialPortDriver1.Active  := False;
    SerialPortDriver1.COMPort := cmboSerialPorts.Text;
    SerialPortDriver1.Active  := True;
    MemoInfo.Lines.Append('Modbus Device Connected and Active: '+cmboSerialPorts.Text);
  end;

  if SerialPortDriver1.Active then
  begin
    MemoInfo.Lines.Clear;
    MemoInfo.Lines.Append('Serial: '+GetSerial);
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
    Timer1.Enabled :=SerialPortDriver1.Active;
  end;

end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  V,I:double;
begin
  V:=GetVoltage;
  EditVoltage.Text:='Voltage: '+FloatToStr(V);
  VFase1.Position:=Round(V);
  I:=GetCurrent;
  EditCurrent.Text:='Current: '+FloatToStr(I);
  IFase1.Position:=Round(I);
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
    Timer1.Enabled :=SerialPortDriver1.Active;
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
  PLCBlockNormal.MemAddress:=150;
  PLCBlockNormal.Read;
  result:=PLCBlockNormal.ValueRaw[0]/10;
end;

function TForm1.GetCurrent:double;
begin
  PLCBlockNormal.MemAddress:=164;
  PLCBlockNormal.Read;
  result:=PLCBlockNormal.ValueRaw[0]/100;
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

