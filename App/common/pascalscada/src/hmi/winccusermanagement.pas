﻿{$I ../common/delphiver.inc}
unit WinCCUserManagement;

interface

uses
  Classes, sysutils, BasicUserManagement, windows, ExtCtrls;

type
  TPWRTLogin                   = function(monitor:AnsiChar)                                 :Boolean;  stdcall;
  TPWRTLogout                  = function()                                                 :Boolean;  stdcall;
  TPWRTGetCurrentUser          = function(Buffer:PAnsiChar; bufsize:LongInt)                :Boolean;  stdcall;
  TPWRTGetLoginPriority        = function()                                                 :Cardinal; stdcall;
  TPWRTPermissionToString      = function(perm:Cardinal; permstr:PAnsiChar; bufsize:LongInt):Boolean;  stdcall;
  TPWRTCheckPermission         = function(permlevel:Cardinal; suppress_messagebox:Cardinal) :Boolean;  stdcall;
  TPWRTCheckPermissionOnArea   = function(permlevel:Cardinal; area:PAnsiChar)               :Boolean;  stdcall;
  TPWRTCheckPermissionOnAreaID = function(permlevel:Cardinal; area:PAnsiChar)               :Boolean;  stdcall;
  TPWRTSilentLogin             = function(login:PAnsiChar; password:PAnsiChar)              :Boolean;  stdcall;

  TPermission = class(TObject)
  public
    AuthID:LongInt;
  end;

  TAuthorization = class(TObject)
  public
    AuthorizationName:AnsiString;
    Valid:Boolean;
  end;

  TAuthorizations = array of TAuthorization;

  { TWinCCUserManagement }

  TWinCCUserManagement = class(TBasicUserManagement)
  private
    FCheckTimer                :TTimer;
    FInLoginProcess            :Boolean;
    FAuthorizationList         :TStrings;
    procedure CheckAuthChanges(Sender:TObject);
  private
    PWRTLogin                  :TPWRTLogin;
    PWRTLogout                 :TPWRTLogout;
    PWRTGetCurrentUser         :TPWRTGetCurrentUser;
    PWRTGetLoginPriority       :TPWRTGetLoginPriority;
    PWRTPermissionToString     :TPWRTPermissionToString;
    PWRTCheckPermission        :TPWRTCheckPermission;
    PWRTCheckPermissionOnArea  :TPWRTCheckPermissionOnArea;
    PWRTCheckPermissionOnAreaID:TPWRTCheckPermissionOnAreaID;
    PWRTSilentLogin            :TPWRTSilentLogin;
    hUseAdmin:THANDLE;
    fUseAdminLoaded:Boolean;
    fAuthorizationCache:TStringList;
    procedure LoadUseAdmin;
    procedure SetAuthorizationList(AValue: TStrings);
  protected
    function CheckUserAndPassword(User, Pass: UTF8String; out UserID: Integer;
      LoginAction: Boolean): Boolean; override;
    function GetLoggedUser:Boolean; override;
    function GetCurrentUserLogin:UTF8String; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; override;
    destructor Destroy; override;
    function Login: Boolean; override;
    procedure Logout; override;
    procedure Manage; override;

    procedure   ValidateSecurityCode(sc:UTF8String); override;
    function    SecurityCodeExists(sc:UTF8String):Boolean; override;
    procedure   RegisterSecurityCode(sc:UTF8String); override;
    procedure   UnregisterSecurityCode(sc:UTF8String); override;

    function    CanAccess(sc:UTF8String):Boolean; override;
    function    GetRegisteredAccessCodes:TStringList; override;
    procedure   ClearAuthorizationCache;
    function    CanAccessViaWinCCAuthCode(Code:LongInt):Boolean;
  published
    property FailureLogin;
    property LoginRetries;
    property LoginFrozenTime;
    property SuccessfulLogin;
    property UserChanged;
    property AuthorizationList:TStrings read FAuthorizationList write SetAuthorizationList stored true;
  end;

implementation

uses ControlSecurityManager, hsstrings, StrUtils, StdCtrls
     {$IFDEF FPC}
     , TextStrings
     {$ELSE}
     , hmitextstrings
     {$ENDIF};

constructor TWinCCUserManagement.Create(AOwner: TComponent);
begin
  fUseAdminLoaded:=false;
  inherited Create(AOwner);

  if not fUseAdminLoaded then LoadUseAdmin;

  FCheckTimer:=TTimer.Create(Self);
  FCheckTimer.OnTimer :=@CheckAuthChanges;
  FCheckTimer.Interval:=1000;
  FCheckTimer.Enabled:=false;
  FAuthorizationList:=TTextStrings.Create;
  fAuthorizationCache:=nil;
end;

procedure TWinCCUserManagement.AfterConstruction;
begin
  inherited AfterConstruction;
  FCheckTimer.Enabled:=true;
end;

destructor TWinCCUserManagement.Destroy;
begin
  //unload the library if it´s loaded
  if hUseAdmin<>0 then
    FreeLibrary(hUseAdmin);
  FreeAndNil(FCheckTimer);
  FreeAndNil(FAuthorizationList);
  FreeAndNil(fAuthorizationCache);
  inherited Destroy;
end;

procedure TWinCCUserManagement.LoadUseAdmin;
begin
  hUseAdmin:=LoadLibrary('UseAdmin.dll');
  if hUseAdmin=0 then begin
    raise Exception.Create(SWinCCAreInstalled);
  end;

  //load UseAdmin functions...
  PWRTLogin                  :=TPWRTLogin(GetProcAddress(hUseAdmin,'PWRTLogin'));
  PWRTLogout                 :=TPWRTLogout(GetProcAddress(hUseAdmin,'PWRTLogout'));
  PWRTGetCurrentUser         :=TPWRTGetCurrentUser(GetProcAddress(hUseAdmin,'PWRTGetCurrentUser'));
  PWRTGetLoginPriority       :=TPWRTGetLoginPriority(GetProcAddress(hUseAdmin,'PWRTGetLoginPriority'));
  PWRTPermissionToString     :=TPWRTPermissionToString(GetProcAddress(hUseAdmin,'PWRTPermissionToString'));
  PWRTCheckPermission        :=TPWRTCheckPermission(GetProcAddress(hUseAdmin,'PWRTCheckPermission'));
  PWRTCheckPermissionOnArea  :=TPWRTCheckPermissionOnArea(GetProcAddress(hUseAdmin,'PWRTCheckPermissionOnArea'));
  PWRTCheckPermissionOnAreaID:=TPWRTCheckPermissionOnAreaID(GetProcAddress(hUseAdmin,'PWRTCheckPermissionOnAreaID'));
  PWRTSilentLogin            :=TPWRTSilentLogin(GetProcAddress(hUseAdmin,'PWRTSilentLogin'));

  fUseAdminLoaded:=true;
end;

procedure TWinCCUserManagement.SetAuthorizationList(AValue: TStrings);
var
  l, p, AuthNumber: LongInt;
  newAuthorizationCache:TStringList;
  ValidFormat: Boolean;
  strNum: UTF8String;
  strAuthName: UTF8String;
begin
  newAuthorizationCache:=TStringList.Create;
  try
    ValidFormat:=true;
    if AValue=nil then exit;
    for l:=0 to AValue.Count-1 do begin
      p:=Pos(':',AValue[l]);
      if p>0 then begin
        strNum:=LeftStr(AValue[l],p-1);
        strAuthName:=RightStr(AValue[l],Length(AValue[l])-p);
        if TryStrToInt(Trim(strNum), AuthNumber) then begin
          {$IFDEF DELPHI2009_UP}
          newAuthorizationCache.AddObject(strAuthName,TObject(Pointer(AuthNumber)));
          {$ELSE}
          newAuthorizationCache.AddObject(strAuthName,TObject(Pointer(PtrUInt(AuthNumber))));
          {$ENDIF}
        end else begin
          ValidFormat:=False;
          break;
        end;
      end else begin
        ValidFormat:=False;
        break;
      end;
    end;
  finally
    if ValidFormat then begin
      if fAuthorizationCache<>nil then
        fAuthorizationCache.Destroy;
      fAuthorizationCache:=newAuthorizationCache;
    end else begin
      newAuthorizationCache.Destroy;
      raise exception.Create('WinCCUserManagement: Invalid authorization list format at line '+IntToStr(l+1));
    end;
    if AValue<>nil then
      FAuthorizationList.Assign(AValue);
  end;
end;

procedure TWinCCUserManagement.CheckAuthChanges(Sender:TObject);
var
  culogin:UTF8String;
begin
  if not fUseAdminLoaded then LoadUseAdmin;

  culogin:=GetCurrentUserLogin;
  if culogin<>FCurrentUserLogin then begin
    GetControlSecurityManager.UpdateControls;
    try
      DoUserChanged;
    finally
      FCurrentUserLogin:=culogin;
    end;
  end;
end;

function TWinCCUserManagement.CheckUserAndPassword(User, Pass: UTF8String; out
  UserID: Integer; LoginAction: Boolean): Boolean;
begin
  if not fUseAdminLoaded then LoadUseAdmin;

  Result:=PWRTSilentLogin(PAnsiChar(AnsiString(User)),PAnsiChar(AnsiString(Pass))); //log into WinCC
  if Result then
    UserID:=1
  else
    UserID:=-1;
end;

function TWinCCUserManagement.GetLoggedUser:Boolean;
begin
 Result := GetCurrentUserLogin<>'';
end;

function TWinCCUserManagement.GetCurrentUserLogin:UTF8String;
var
  buffer1:PAnsiChar;
  c:LongInt;
begin
  if not fUseAdminLoaded then LoadUseAdmin;

  c:=PWRTGetLoginPriority(); //forces initialization...

  buffer1:=GetMemory(512);
  buffer1[0]:=#0;

  if PWRTGetCurrentUser(buffer1,510) then
    Result:=buffer1
  else
    Result:='';

  FreeMem(buffer1);
end;

procedure TWinCCUserManagement.Loaded;
begin
  inherited Loaded;
  if FAuthorizationList.Count>0 then
    SetAuthorizationList(FAuthorizationList);
end;

function  TWinCCUserManagement.Login: Boolean;
begin
  if FInLoginProcess then exit;
  FInLoginProcess:=true;
  try
    Result := inherited Login;
  finally
    FInLoginProcess:=false;
  end;
end;

procedure TWinCCUserManagement.Logout;
begin
  if not fUseAdminLoaded then LoadUseAdmin;

  if PWRTLogout() then
    inherited Logout;
end;

procedure TWinCCUserManagement.Manage;
begin
  raise exception.Create(SUseTheWinCCUserManager);
end;

procedure   TWinCCUserManagement.ValidateSecurityCode(sc:UTF8String);
begin
  if not SecurityCodeExists(sc) then
    raise exception.Create(SUseTheWinCCUserManager);
end;

function    TWinCCUserManagement.CanAccess(sc:UTF8String):Boolean;
var
  p, p2, i:Cardinal;
  auth:TStringList;
  c:LongInt;
begin
  Result:=false;

  if not fUseAdminLoaded then LoadUseAdmin;

  p:=PWRTGetLoginPriority(); //forces a initialization... I don´t know why...

  if not SecurityCodeExists(sc) then exit;

  auth:=GetRegisteredAccessCodes;

  i:=auth.IndexOf(sc);

  if (i>0) then
    p2:=LongInt(Pointer(auth.Objects[i]))
  else
    p2:=0;

  auth.Destroy;

  Result:=PWRTCheckPermission(p2,1);
end;

function    TWinCCUserManagement.SecurityCodeExists(sc:UTF8String):Boolean;
var
  x:TStringList;
  c:LongInt;
begin
  x:=GetRegisteredAccessCodes;
  Result:=x.IndexOf(sc)<>-1;
  x.Destroy;
end;

procedure   TWinCCUserManagement.RegisterSecurityCode(sc:UTF8String);
begin
  raise exception.Create(SUseTheWinCCUserManager);
end;

procedure   TWinCCUserManagement.UnregisterSecurityCode(sc:UTF8String);
begin
  //does nothing.
end;

function    TWinCCUserManagement.GetRegisteredAccessCodes:TStringList;
var
  buffer1:PAnsiChar;
  c:LongInt;
begin
  if not fUseAdminLoaded then LoadUseAdmin;

  c:=PWRTGetLoginPriority(); //forces initialization...

  if fAuthorizationCache=nil then begin

    buffer1:=GetMemory(512);

    Result:=TStringList.Create;
    for c:=1 to 1100 do begin
      buffer1[0]:=#0;
      PWRTPermissionToString(c,buffer1,510);

      if strcomp(buffer1,'')<>0 then begin
        {$IFDEF DELPHI2009_UP}
        Result.AddObject((buffer1),TObject(Pointer(c)));
        {$ELSE}
        Result.AddObject(buffer1,TObject(Pointer(PtrUInt(c))));
        {$ENDIF}
      end;
    end;
    fAuthorizationCache:=TStringList.Create;
    fAuthorizationCache.Assign(Result);
    FreeMem(buffer1);
  end else begin
    Result:=TStringList.Create;
    Result.Assign(fAuthorizationCache);
  end;
end;

procedure TWinCCUserManagement.ClearAuthorizationCache;
begin
  fAuthorizationCache.Destroy;
  fAuthorizationCache:=nil;
end;

function TWinCCUserManagement.CanAccessViaWinCCAuthCode(Code: LongInt): Boolean;
begin
  Result := PWRTCheckPermission(code,0);
end;

end.
