unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  dsLeds;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnGetData: TButton;
    btnStartTimer: TButton;
    btnStopTimer: TButton;
    chkDebug: TCheckBox;
    grpCommand: TGroupBox;
    MemoInfo: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Timer1: TTimer;
    procedure btnGetDataClick({%H-}Sender: TObject);
    procedure btnStartTimerClick({%H-}Sender: TObject);
    procedure btnStopTimerClick({%H-}Sender: TObject);
    procedure FormCreate({%H-}Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure Timer1Timer({%H-}Sender: TObject);
  private
    DataBusy            : integer;
    RealVoltageDisplay  : TdsSevenSegmentMultiDisplay;
    RealCurrentDisplay  : TdsSevenSegmentMultiDisplay;
    procedure GetData;
    procedure ParseJSON(JSONDataString: string);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  fphttpclient,fpjson,jsonparser;

{ TForm1 }



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
  DataBusy:=integer(false);

  RealVoltageDisplay:=TdsSevenSegmentMultiDisplay.Create(Panel1);
  with RealVoltageDisplay do
  begin
    Parent:=Panel1;
    OnColor:=clAqua;
    OffColor:=ChangeBrightness(OnColor,0.1);
  end;
  RealCurrentDisplay:=TdsSevenSegmentMultiDisplay.Create(Panel1);
  with RealCurrentDisplay do
  begin
    Parent:=Panel1;
    OnColor:=clRed;
    OffColor:=ChangeBrightness(OnColor,0.1);
  end;
end;

procedure TForm1.Panel1Resize(Sender: TObject);
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

procedure TForm1.btnGetDataClick(Sender: TObject);
begin
  GetData;
end;

procedure TForm1.btnStartTimerClick(Sender: TObject);
begin
  btnStartTimer.Enabled:=false;
  btnGetData.Enabled:=false;
  Timer1Timer(Sender);
  Timer1.Enabled:=true;
  btnStopTimer.Enabled:=true;
end;

procedure TForm1.btnStopTimerClick(Sender: TObject);
begin
  btnStopTimer.Enabled:=false;
  Timer1.Enabled:=false;
  btnGetData.Enabled:=true;
  btnStartTimer.Enabled:=true;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  GetData;
end;

procedure TForm1.GetData;
const
  APIURL='http://192.168.0.26/inverter/properties/read/1';
  REQUEST = '"readdb":true';
var
  JSONData:TJSONObject;
  Response : TStringStream;
  Client: TFPHttpClient;
begin
  if InterLockedExchange(DataBusy, integer(True))<>integer(True) then
  try

    Client:=TFPHttpClient.Create(nil);
    Client.AddHeader('Content-Type', 'application/json; charset=UTF-8');
    Client.AddHeader('Accept', 'application/json; charset=UTF-8');
    Client.UserName:='manieicc';
    Client.Password:='manie12345';

    //Client.RequestBody := TRawByteStringStream.Create(REQUEST);

    JSONData := TJSONObject.Create;
    JSONData.Booleans['readdb'] := true;
    Client.RequestBody := TRawByteStringStream.Create(JSONData.AsJSON);
    JSONData.Clear;
    JSONData.Free;

    Response := TStringStream.Create('');
    if chkDebug.Checked then MemoInfo.Clear;

    try
      try
        Client.Post(APIURL, Response);
        MemoInfo.Append('Response Code of Post Request is '+IntToStr(Client.ResponseStatusCode));
        ParseJSON(Response.DataString);
      except on E:Exception do
        MemoInfo.Text := 'Something bad happened in Post Request : ' + E.Message;
      end;
    finally
      Client.RequestBody.Free;
      Client.Free;
      Response.Free;
    end;

  finally
    InterLockedExchange(DataBusy, integer(False));
  end;
end;

procedure TForm1.ParseJSON(JSONDataString: string);
const
  DATAARRAYNAME       = 'Data';
  DATAKEYNAME         = 'name';
  BATTERYVOLTAGENAME  = 'Vbatt';
  BATTERYCURRENTNAME  = 'Ibatt';
  VALUENAME           = 'value';
var
  JSONData        : TJSONData;
  KeyName         : TJSONStringType;
  KeyValue        : TJSONFloat;
  i               : integer;
  DataArray       : TJSONArray;
  DataArrayItems  : TJSONObject;
  VFound,IFound   : boolean;
begin
  try
    try
      VFound:=false;
      IFound:=false;

      JSONData:=(GetJSON(JSONDataString,false));

      if chkDebug.Checked then MemoInfo.Append(JSONData.FormatJSON([foSinglelineArray], 2));

      DataArray := TJSONArray(JSONData.FindPath(DATAARRAYNAME));

      for i:=0 to Pred(DataArray.Count) do
      begin
        DataArrayItems:=TJSONObject(DataArray[i]);
        KeyName:=DataArrayItems[DATAKEYNAME].AsString;
        if KeyName=BATTERYVOLTAGENAME then
        begin
          KeyValue:=DataArrayItems[VALUENAME].AsFloat;
          RealVoltageDisplay.Value:=KeyValue;
          VFound:=true;
        end;
        if KeyName=BATTERYCURRENTNAME then
        begin
          KeyValue:=DataArrayItems[VALUENAME].AsFloat;
          RealCurrentDisplay.Value:=Abs(KeyValue);
          IFound:=true;
        end;

        if (VFound AND IFound) then break;
      end;
    except
      // swallow json exceptions
    end;
  finally
    JSONData.Free;
  end;
end;

end.

