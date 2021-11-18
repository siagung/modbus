unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ModBusSerial, SerialPort, PLCTagNumber, PLCBlock, PLCBlockElement,
  DTAnalogGauge,
  synaser;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnGetData: TButton;
    cmboSerialPorts: TComboBox;
    EditVoltage: TEdit;
    EditAddress: TEdit;
    EditCurrent: TEdit;
    lblAddress: TLabel;
    MemoInfo: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Timer1: TTimer;
    procedure btnGetDataClick(Sender: TObject);
    procedure cmboSerialPortsChange(Sender: TObject);
    procedure FormCreate({%H-}Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    SerialPortDriver1: TSerialPortDriver;
    ModBusRTUDriver1: TModBusRTUDriver;
    PLCBlock1: TPLCBlock;
    PLCBlockElement1: TPLCBlockElement;

    VFase1             : TDTAnalogGauge;
    IFase1             : TDTAnalogGauge;

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

  PLCBlock1:=TPLCBlock.Create(Application);
  with PLCBlock1 do
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
    //Size                := 10; when reading more data in one go
  end;

  PLCBlockElement1:=TPLCBlockElement.Create(Application);
  with PLCBlockElement1 do
  begin
    TagGUID             := '{D2137516-387B-458E-91C3-1FB875046629}';
    PLCBlock            := PLCBlock1;
    Index               := 0;
    EnableMaxValue      := False;
    EnableMinValue      := False;
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

procedure TForm1.btnGetDataClick(Sender: TObject);
var
  i:integer;
begin
  Timer1.Enabled:=False;
  MemoInfo.Lines.Clear;
  PLCBlock1.MemAddress:=StrToInt(EditAddress.Text);
  PLCBlock1.Read;
  for i:=0 to PLCBlock1.Size-1 do
      MemoInfo.Lines.Append(Format('PLCBlock[%d]=%.1f',[i,PLCBlock1.ValueRaw[i]]));
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
  Timer1.Enabled := SerialPortDriver1.Active;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  V,I:double;
begin
  PLCBlock1.MemAddress:=150;
  PLCBlock1.Read;
  V:=PLCBlockElement1.Value/10;
  EditVoltage.Text:='Voltage: '+FloatToStr(V);
  VFase1.Position:=Round(V);
  PLCBlock1.MemAddress:=164;
  PLCBlock1.Read;
  I:=PLCBlockElement1.Value/100;
  EditCurrent.Text:='Current: '+FloatToStr(I);
  IFase1.Position:=Round(I);
end;

end.

