{$i ../common/language.inc}
unit usrmgnt_login;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF FPC}LCLIntf, LResources,{$ENDIF} Forms, StdCtrls, Buttons, ExtCtrls, Classes;

type

  TFocusedControl = (fcUserName, fcPassword);

  { TpsHMIfrmUserAuthentication }

  TpsHMIfrmUserAuthentication = class(TForm)
    edtusername: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    edtPassword: TEdit;
    lblRequiredPerm: TLabel;
    lblHint: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    Panel3: TPanel;
    procedure FormShow(Sender: TObject);
  private
    FFocusControl:TFocusedControl;
  public
    ChipCardCode:UTF8String;
    procedure EnableEntry;
    procedure DisableEntry;
  published
    property FocusControl:TFocusedControl read FFocusControl write FFocusControl;
  end;

var
  psHMIfrmUserAuthentication: TpsHMIfrmUserAuthentication;

implementation

{$IFDEF FPC}
  {$IF defined(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 20400) }
    {$R usrmgnt_login.lfm}
  {$IFEND}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

{ TpsHMIfrmUserAuthentication }

procedure TpsHMIfrmUserAuthentication.FormShow(Sender: TObject);
begin
  case FFocusControl of
    fcUserName: if edtusername.Enabled then edtusername.SetFocus;
    fcPassword: if edtPassword.Enabled then edtPassword.SetFocus;
  end;
end;

procedure TpsHMIfrmUserAuthentication.EnableEntry;
begin
  edtPassword.Enabled:=true;
  edtusername.Enabled:=true;
  btnCancel.Enabled:=true;
  btnOk.Enabled:=true;
  FormShow(Self);
end;

procedure TpsHMIfrmUserAuthentication.DisableEntry;
begin
  edtPassword.Enabled:=false;
  edtusername.Enabled:=false;
  btnCancel.Enabled:=false;
  btnOk.Enabled:=false;
end;

{$IFDEF FPC}
  {$IF defined(FPC_FULLVERSION) AND (FPC_FULLVERSION < 20400) }
  initialization
  {$i usrmgnt_login.lrs}
  {$IFEND}
{$ENDIF}

end.
