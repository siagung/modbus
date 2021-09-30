{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Implementacao dos editores de algumas propriedades de controles
            do PascalSCADA.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Implements the some properties editors of the controls of the
            PascalSCADA.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit hmipropeditor;

{$IFDEF FPC}
{$MACRO ON}
{$ENDIF}

{$I ../common/delphiver.inc}

interface

uses
  Classes, SysUtils, HMIZones, Dialogs, Menus, ProtocolDriver, typinfo,
  HMIControlDislocatorAnimation, hmiobjectcolletion, Controls,
  ControlSecurityManager, Graphics, scadapropeditor, fpexprpars,
  {$IFDEF FPC}
    PropEdits, ComponentEditors, GraphPropEdits, ImgList,
    hmibooleanpropertyconnector, hmicolorpropertyconnector, hmi_polyline;
  {$ELSE}
    Types,
    //if is a delphi 6+
    {$IF defined(DELPHI6_UP)}
      DesignIntf, DesignEditors;
    {$ELSE}
      //delphi 5-
      DsgnIntf;
    {$IFEND}
  {$ENDIF}

type
  {$IFDEF PORTUGUES}
  {:
  Editor da propriedade TGraphicZone.FileName
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ELSE}
  {:
  Property editor of TGraphicZone.FileName property.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ENDIF}
  TZoneFileNamePropertyEditor = class(TStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    function  GetValue: AnsiString; override;
    procedure Edit; override;
    procedure SetValue(const Value: AnsiString); override;
  end;

  {$IFDEF FPC}
  {$IFDEF PORTUGUES}
  {:
  Editor da propriedade TGraphicZone.ImageIndex
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ELSE}
  {:
  Property editor of TGraphicZone.ImageIndex property.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ENDIF}
  TGraphiZoneImageIndexPropertyEditor = class(TImageIndexPropertyEditor)
  protected
    function GetImageList: TCustomImageList; override;
  public
    procedure SetValue(const NewValue: ansistring); override;
    procedure ListDrawValue(const CurValue: ansistring; Index: integer;
      ACanvas: TCanvas; const ARect: TRect; AState: TPropEditDrawState); override;
  end;

  TPascalSCADALoginLogoutImageIndexPropertyEditor = class(TImageIndexPropertyEditor)
  protected
    function GetImageList: TCustomImageList; override;
  public
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const NewValue: ansistring); override;
  end;


  {$IFNDEF DELPHI4_UP}
  TSelectObjectPropPropertyEditor = class(TStringPropertyEditor)
  protected
    FOnlyPropertiesOfType:AnsiString;
    FExpectedClass:TObjectColletionItemClass;
  public
    constructor Create(Hook: TPropertyEditorHook; APropCount: Integer);
      override;
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const NewValue: AnsiString); override;
  end;

  { TSelectOnlyBooleanPropPropertyEditor }

  TSelectOnlyBooleanPropPropertyEditor = Class(TSelectObjectPropPropertyEditor)
  public
    constructor Create(Hook: TPropertyEditorHook; APropCount: Integer);
       override;
  end;

  { TSelectOnlyTColorPropPropertyEditor }

  TSelectOnlyTColorPropPropertyEditor = Class(TSelectObjectPropPropertyEditor)
  public
    constructor Create(Hook: TPropertyEditorHook; APropCount: Integer);
       override;
  end;

  {$ENDIF}
  {$ENDIF}

  {$IFDEF PORTUGUES}
  {:
  Editor das seguintes propriedades de THMIControlDislocatorAnimation:
  @unorderedList(
    @item(THMIControlDislocatorAnimation.Gets_P0_Position)
    @item(THMIControlDislocatorAnimation.Gets_P1_Position)
    @item(THMIControlDislocatorAnimation.GoTo_P0_Position)
  )

  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ELSE}
  {:
  Property editor of the following properties of THMIControlDislocatorAnimation:
  @unorderedList(
    @item(THMIControlDislocatorAnimation.Gets_P0_Position)
    @item(THMIControlDislocatorAnimation.Gets_P1_Position)
    @item(THMIControlDislocatorAnimation.GoTo_P0_Position)
  )

  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ENDIF}
  TPositionPropertyEditor = class(TStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    function  GetValue: AnsiString; override;
    procedure Edit; override;
  end;

  {$IFDEF PORTUGUES}
  {:
  Editor da propriedade
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ELSE}
  {:
  Property editor of TZone.BlinkWith property.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ENDIF}
  TZoneBlinkWithPropertyEditor = class(TIntegerProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  {$IFDEF PORTUGUES}
  {:
  Editor da propriedade SecurityCode, responsavel pela seguranca de cada controle.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ELSE}
  {:
  Property editor of SecurityCode property, that makes controls secure.
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ENDIF}
  TSecurityCodePropertyEditor = class(TStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  THMIPolylineAccess = class(THMIPolyline);

  { THMIPolylineComponentEditor }

  THMIPolylineComponentEditor = class(TDefaultComponentEditor)
  protected
    procedure DrawPolyline;
    procedure DrawEmptyPolyline;
    procedure OptimizePolyline;
  public
    procedure ExecuteVerb(Index: LongInt); override;
    function  GetVerb(Index: LongInt): AnsiString; override;
    function  GetVerbCount: LongInt; override;
    function  Polyline: THMIPolyline; virtual;
  end;

  TControlPosSizePropertyEditor = class(TIntegerExpressionPropertyEditor)
  protected
    procedure RegisterExpressionVariables(const i: Integer;
                 var parser: TFPExpressionParser); override;
  end;

implementation

uses HMITypes;

{ TControlPosSizePropertyEditor }

procedure TControlPosSizePropertyEditor.RegisterExpressionVariables(
  const i: Integer; var parser: TFPExpressionParser);
var
  propertyName: AnsiString;
begin
  if assigned(parser) then begin
    //unregister all possible registered variables.
    parser.Identifiers.Clear;

    propertyName:=lowercase(GetPropInfo^.Name);

    if (GetComponent(i) is TControl) then begin
      //register only if the property is not being edited,
      //to avoid circular references.

      if (propertyName<>'left') then
        parser.Identifiers.AddIntegerVariable('left', (GetComponent(i) as TControl).Left);

      if (propertyName<>'top') then
        parser.Identifiers.AddIntegerVariable('top', (GetComponent(i) as TControl).top);

      if (propertyName<>'width') then
        parser.Identifiers.AddIntegerVariable('width', (GetComponent(i) as TControl).Width);

      if (propertyName<>'height') then
        parser.Identifiers.AddIntegerVariable('height', (GetComponent(i) as TControl).Height);

      if (propertyName<>'tag') then
        parser.Identifiers.AddIntegerVariable('tag', (GetComponent(i) as TControl).Tag);
    end;
  end;
end;

{ THMIPolylineComponentEditor }

procedure THMIPolylineComponentEditor.DrawPolyline;
begin
  THMIPolylineAccess(Polyline).BeginDrawPolyline;
end;

procedure THMIPolylineComponentEditor.DrawEmptyPolyline;
begin
  THMIPolylineAccess(Polyline).BeginEmptyPolyline;
end;

procedure THMIPolylineComponentEditor.OptimizePolyline;
begin
  THMIPolylineAccess(Polyline).OptimizeDraw;
end;

procedure THMIPolylineComponentEditor.ExecuteVerb(Index: LongInt);
begin
  case Index of
    0: DrawPolyline;
    1: DrawEmptyPolyline;
    2: OptimizePolyline;
  end;
end;

function THMIPolylineComponentEditor.GetVerb(Index: LongInt): AnsiString;
begin
  case Index of
   0: Result:='Continue draw';
   1: Result:='Clear draw';
   2: Result:='Optimize';
   else
     Result:=inherited GetVerb(Index);
  end;
end;

function THMIPolylineComponentEditor.GetVerbCount: LongInt;
begin
  Result:=3;
end;

function THMIPolylineComponentEditor.Polyline: THMIPolyline;
begin
  Result:=THMIPolyline(GetComponent);
end;

{ TSelectOnlyTColorPropPropertyEditor }

constructor TSelectOnlyTColorPropPropertyEditor.Create(
  Hook: TPropertyEditorHook; APropCount: Integer);
begin
  inherited Create(Hook, APropCount);
  FOnlyPropertiesOfType:=PTypeInfo(TypeInfo(TColor))^.Name;
  FExpectedClass:=TObjectWithColorPropetiesColletionItem;
end;

{ TSelectOnlyBooleanPropPropertyEditor }

constructor TSelectOnlyBooleanPropPropertyEditor.Create(
  Hook: TPropertyEditorHook; APropCount: Integer);
begin
  inherited Create(Hook, APropCount);
  FOnlyPropertiesOfType:=PTypeInfo(TypeInfo(Boolean))^.Name;
  FExpectedClass:=TObjectWithBooleanPropetiesColletionItem;
end;

{ TTargetObjectPropPropertyEditor }

constructor TSelectObjectPropPropertyEditor.Create(Hook: TPropertyEditorHook;
  APropCount: Integer);
begin
  inherited Create(Hook, APropCount);
  FOnlyPropertiesOfType:='';
  FExpectedClass:=TObjectColletionItem;
end;

function TSelectObjectPropPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  if GetComponent(0) is FExpectedClass then
     Result := [paValueList
                {$IFDEF FPC}
                ,paPickList
                {$ELSE}
                  {$IFDEF DELPHI2005_UP}
                  , paReadOnly, paValueEditable
                  {$ENDIF}
                {$ENDIF}];
end;

procedure TSelectObjectPropPropertyEditor.GetValues(Proc: TGetStrProc);
var
  PL: PPropList;
  tdata: PTypeData;
  nprops: Integer;
  p: Integer;
  obj: TComponent;
begin
  Proc('(none)');
   if GetComponent(0) is FExpectedClass then
     if assigned((GetComponent(0) as FExpectedClass).TargetObject) then begin
       obj := (GetComponent(0) as FExpectedClass).TargetObject;

       tdata:=GetTypeData(obj.ClassInfo);

       GetMem(PL,tdata^.PropCount*SizeOf(Pointer));
       try
         nprops:=GetPropList(obj,PL);
         for p:=0 to nprops-1 do begin
           if (lowercase(PL^[p]^.PropType^.Name)=lowercase(FOnlyPropertiesOfType)) or (FOnlyPropertiesOfType='') then begin
             Proc(PL^[p]^.Name);
           end;
         end;
       finally
         Freemem(PL);
       end;
     end;
end;

procedure TSelectObjectPropPropertyEditor.SetValue(const NewValue: AnsiString);
begin
  if NewValue='(none)' then
    inherited SetValue('')
  else
    inherited SetValue(NewValue);
end;

function  TZoneFileNamePropertyEditor.GetAttributes: TPropertyAttributes;
begin
   if GetComponent(0) is TGraphicZone then
      Result := [paDialog{$IFNDEF FPC}{$IFDEF DELPHI2005_UP}, paReadOnly,
                 paValueEditable{$ENDIF}{$ENDIF}];
end;

function  TZoneFileNamePropertyEditor.GetValue: AnsiString;
begin
   Result := GetStrValue;
end;

procedure TZoneFileNamePropertyEditor.Edit;
var
  od: TOpenDialog;
begin
  if GetComponent(0) is TGraphicZone then begin
    od:=TOpenDialog.Create(nil);
    {$IFDEF FPC}
    od.Filter:='Imagens *.ico *.ppm *.pgm *.pbm *.png *.xpm *.bpm|*.ico;*.ppm;*.pgm;*.pbm;*.png;*.xpm;*.bpm';
    {$ELSE}
    od.Filter:='Imagens *.jpg *.jpeg *.bmp *.ico *.emf *.wmf|*.jpg;*.jpeg;*.bmp;*.ico;*.emf;*.wmf';
    {$ENDIF}
    try
       if od.Execute then
          TGraphicZone(GetComponent(0)).FileName := od.FileName;
    finally
       od.Destroy;
    end;
  end;
end;

procedure TZoneFileNamePropertyEditor.SetValue(const Value: AnsiString);
begin
   SetStrValue(Value);
   if GetComponent(0) is TGraphicZone then
      TGraphicZone(GetComponent(0)).ImageListAsDefault := false;
end;

{$IFDEF FPC}
procedure TGraphiZoneImageIndexPropertyEditor.SetValue(const NewValue: ansistring);
var
  x:LongInt;
begin
  try
    if NewValue='(none)' then
       inherited SetValue('-1')
    else begin
      if GetImageList<>nil then begin
        x:=StrToInt(NewValue);
        if (x>=0) and (x<GetImageList.Count) then
          inherited SetValue(NewValue)
        else
          inherited SetValue('-1');
      end else
        inherited SetValue('-1');
    end;
  except
    inherited SetValue('-1');
  end;
end;

procedure TGraphiZoneImageIndexPropertyEditor.ListDrawValue(
  const CurValue: ansistring; Index: integer; ACanvas: TCanvas;
  const ARect: TRect; AState: TPropEditDrawState);
var
  Images: TCustomImageList;
  R: TRect;
  OldColor: TColor;

  procedure DrawText;
  var
    Style: TTextStyle;
  begin
    FillChar(Style{%H-},SizeOf(Style),0);
    With Style do begin
      Alignment := taLeftJustify;
      Layout := tlCenter;
      Opaque := False;
      Clipping := True;
      ShowPrefix := True;
      WordBreak := False;
      SingleLine := True;
      SystemFont := False;
    end;
    ACanvas.TextRect(ARect, ARect.Left+2,ARect.Top, CurValue, Style);
  end;

begin
  if Index=0 then  begin
    DrawText;
    exit;
  end;
  dec(Index);
  Images := GetImageList;
  R := ARect;
  if Assigned(Images) then
  begin
    if (pedsInComboList in AState) and not (pedsInEdit in AState) then
    begin
      OldColor := ACanvas.Brush.Color;
      if pedsSelected in AState then
        ACanvas.Brush.Color := clHighlight
      else
        ACanvas.Brush.Color := clWhite;
      ACanvas.FillRect(R);
      ACanvas.Brush.Color := OldColor;
    end;

    Images.Draw(ACanvas, R.Left+Images.Width+ 3, R.Top + 1, Index, True);
    R.Left := R.Left + Images.Width + 2;
  end;
  DrawText;
end;

function TGraphiZoneImageIndexPropertyEditor.GetImageList: TCustomImageList;
begin
  if GetComponent(0) is TGraphicZone then
    Result:=TGraphicZone(GetComponent(0)).ImageList
  else
    Result:=nil;
end;

procedure TPascalSCADALoginLogoutImageIndexPropertyEditor.SetValue(const NewValue: ansistring);
var
  x:LongInt;
begin
  try
    if NewValue='(none)' then
       inherited SetValue('-1')
    else begin
      if GetImageList<>nil then begin
        x:=StrToInt(NewValue);
        if x in [0..GetImageList.Count-1] then
          inherited SetValue(NewValue)
        else
          inherited SetValue('-1');
      end else
        inherited SetValue('-1');
    end;
  except
    inherited SetValue('-1');
  end;
end;

function TPascalSCADALoginLogoutImageIndexPropertyEditor.GetImageList: TCustomImageList;
begin
  if GetComponent(0) is TPascalSCADALogin_LogoutAction then
    Result:=TPascalSCADALogin_LogoutAction(GetComponent(0)).ActionList.Images
  else
    Result:=nil;
end;

procedure TPascalSCADALoginLogoutImageIndexPropertyEditor.GetValues(Proc: TGetStrProc);
begin
  Proc('(none)');
  inherited GetValues(Proc);
end;
{$ENDIF}

function  TZoneBlinkWithPropertyEditor.GetAttributes: TPropertyAttributes;
begin
   if GetComponent(0) is TZone then
      Result := [paValueList{$IFNDEF FPC}{$IFDEF DELPHI2005_UP}, paReadOnly,
                 paValueEditable{$ENDIF}{$ENDIF}];
end;

procedure TZoneBlinkWithPropertyEditor.GetValues(Proc: TGetStrProc);
var
   i:LongInt;
begin
   Proc('-1');
   if (GetComponent(0) is TZone) and (TZone(GetComponent(0)).Collection is TZones) then
      for i := 0 to TZone(GetComponent(0)).Collection.Count-1 do begin
         if TZone(GetComponent(0)).Collection.Items[i]<>GetComponent(0) then
            Proc(IntToStr(i));
      end;
end;

function  TSecurityCodePropertyEditor.GetAttributes: TPropertyAttributes;
begin
   if Supports(GetComponent(0), IHMIInterface) or (GetComponent(0) is TPascalSCADACheckSpecialTokenAction) then
      Result := [paValueList{$IFNDEF FPC}{$IFDEF DELPHI2005_UP}, paReadOnly,
                 paValueEditable{$ENDIF}{$ENDIF}];
end;

procedure TSecurityCodePropertyEditor.GetValues(Proc: TGetStrProc);
var
   i:LongInt;
   x:TStringList;
begin
   Proc('');
   x:=GetControlSecurityManager.GetRegisteredAccessCodes;
   for i:=0 to x.Count-1 do begin
     proc(x.Strings[i]);
   end;
   x.Destroy;
end;

///////////////////////////////////////////////////////////////////////////////

function  TPositionPropertyEditor.GetAttributes: TPropertyAttributes;
begin
   if GetComponent(0) is THMIControlDislocatorAnimation then
      Result := [paDialog{$IFNDEF FPC}{$IFDEF DELPHI2005_UP}, paReadOnly,
                 paValueEditable{$ENDIF}{$ENDIF}];
end;

function  TPositionPropertyEditor.GetValue: AnsiString;
begin
   Result := GetStrValue;
end;

procedure TPositionPropertyEditor.Edit;
var
  pname:AnsiString;
begin
  if GetComponent(0) is THMIControlDislocatorAnimation then begin
    with GetComponent(0) as THMIControlDislocatorAnimation do begin
      if Control=nil then exit;
      pname := LowerCase(GetName);
      if pname='gets_p0_position' then begin
        P0_X:=Control.Left;
        P0_Y:=Control.Top;
        exit;
      end;

      if pname='gets_p1_position' then begin
        P1_X:=Control.Left;
        P1_Y:=Control.Top;
        exit;
      end;
      if pname='goto_p0_position' then begin
        Control.Left:=P0_X;
        Control.Top:=P0_Y;
        exit;
      end;
    end;
  end;
end;

end.

