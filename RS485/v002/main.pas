unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  rs485,
  DTAnalogGauge,
  synaser;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    cmboSerialPorts: TComboBox;
    EditCurrent: TEdit;
    EditVoltage: TEdit;
    MemoInfo: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure cmboSerialPortsChange({%H-}Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer({%H-}Sender: TObject);
    procedure FormCreate({%H-}Sender: TObject);
  private
    FAddress: byte;

    RS485              : TRS485;

    VFase1             : TDTAnalogGauge;
    IFase1             : TDTAnalogGauge;

    function GetVoltage:double;
    function GetCurrent:double;

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
  RS485:=TRS485.Create;

  FAddress:=0;

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
end;

procedure TForm1.cmboSerialPortsChange(Sender: TObject);
begin
  if (cmboSerialPorts.Items.Count>0) then
  begin
    Timer1.Enabled:=False;
    MemoInfo.Lines.Clear;
    RS485.SerialPortName:=cmboSerialPorts.Text;
    RS485.Connect;
    MemoInfo.Lines.Append('Battery Connected: '+cmboSerialPorts.Text);
    MemoInfo.Lines.Append(RS485.Info);
    //Timer1.Enabled:=True;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Timer1Timer(nil);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  RS485.Free;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  V,I:double;
  s:string;
begin
  s:=RS485.Info;
  if (Length(s)>0) then MemoInfo.Lines.Append('Before: '+s);
  V:=GetVoltage;
  EditVoltage.Text:='Voltage: '+FloatToStr(V);
  VFase1.Position:=Round(V);
  I:=GetCurrent;
  EditCurrent.Text:='Current: '+FloatToStr(I);
  IFase1.Position:=Round(I);
  s:=RS485.Info;
  if (Length(s)>0) then MemoInfo.Lines.Append('After: '+s);
end;

function TForm1.GetVoltage:double;
var
  V:double;
begin
  if RS485.Emulation then
  begin
    result:=9876;
  end
  else
  begin
    MemoInfo.Lines.Append('Address: '+InttoStr(FAddress));
    if (NOT (RS485.ReadVoltage(FAddress,V))) then
    begin
      result:=V;
      MemoInfo.Lines.Append('Got you !!!!!');
    end
    else
    begin
      result:=1234;
      //Inc(FAddress);
    end;
  end;
end;

function TForm1.GetCurrent:double;
begin
  result:=2;
end;

end.

