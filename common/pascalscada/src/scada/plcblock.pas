{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  @abstract(Implementação de um bloco de tags de comunicação.)
}
{$ELSE}
{:
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  @abstract(Unit that implements a block of tags of communication.)

  ****************************** History  *******************************
  ***********************************************************************
  07/2013 - Moved OpenElementMapper to BlockTagAssistant to remove TForm dependencies
  @author(Juanjo Montero <juanjo.montero@gmail.com>)
  10/2014 - Switched back to the old behavior but keeping the improvemnt
  of Juanjo (do not link with GUI);
  ***********************************************************************
}
{$ENDIF}
unit PLCBlock;

interface

uses
  SysUtils, Classes, Tag, TagBlock, ProtocolTypes;

type
  {$IFDEF PORTUGUES}
  {:
    @author(Fabio Luis Girardi <fabio@pascalscada.com>)

    Tag para comunicação em blocos.
  }
  {$ELSE}
  {:
    @author(Fabio Luis Girardi <fabio@pascalscada.com>)

    Class of Block of tags of communication.
  }
  {$ENDIF}

  { TPLCBlock }

  TPLCBlock = class(TTagBlock, IScanableTagInterface)
  private
    procedure SetSize(isize:Cardinal);
    function  GetValue(Index:LongInt):Double;
    procedure SetValue(index:LongInt; Value:Double);

    function  GetValues:TArrayOfDouble;
    procedure SetValues(values:TArrayOfDouble);
  protected
    //: @seealso(TTag.AsyncNotifyChange)    
    procedure AsyncNotifyChange(data:Pointer); override;
    //: @seealso(TTag.GetValueChangeData)
    function GetValueChangeData: Pointer; override;
    //: @seealso(TTag.ReleaseChangeData)
    procedure ReleaseChangeData(data: Pointer); override;
    //: @seealso(TPLCTag.IsMyCallBack)
    function IsMyCallBack(Cback: TTagCommandCallBack): Boolean; override;
    //: @seealso(TPLCTag.TagCommandCallBack)
    procedure TagCommandCallBack(const ReqID:LongWord; Values:TArrayOfDouble; ValuesTimeStamp:TDateTime; TagCommand:TTagCommand; LastResult:TProtocolIOResult; Offset:LongInt); override;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor  Destroy; override;

    {$IFDEF PORTUGUES}
    {:
    @name escreve assincronamente os valores atribuidos ao bloco.
    @bold(Só tem efeito caso AutoWrite = @false.)
    }
    {$ELSE}
    {:
    @name writes asynchronously the values stored in the block.
    @bold(Only works if AutoWrite = @false.)
    }
    {$ENDIF}
    procedure WriteByScan;

    {$IFDEF PORTUGUES}
    {:
    @name escreve sincronamente os valores atribuidos ao bloco.
    @bold(Só tem efeito caso AutoWrite = @false.)
    }
    {$ELSE}
    {:
    @name writes synchronously the values stored in the block.
    @bold(Only works if AutoWrite = @false.)
    }
    {$ENDIF}
    procedure WriteDirect;

    //: @seealso(TPLCTag.ScanRead)
    function ScanRead:Int64; override;
    //: @seealso(TPLCTag.Read)
    procedure Read; override;

    //: @seealso(TPLCTag.ScanWrite)
    function ScanWrite(Values:TArrayOfDouble; Count, Offset:Cardinal; const IgnoreAutoWrite:Boolean = false):Int64; override;
    //: @seealso(TPLCTag.Write)
    procedure Write(Values:TArrayOfDouble; Count, Offset:Cardinal); override;

    {$IFDEF PORTUGUES}
    //: Abre o assistente de mapeamento de items do bloco/estrutura.
    {$ELSE}
    //: Opens the block/struct item mapper wizard.
    {$ENDIF}
    procedure MapElements(InsertHook:TAddTagInEditorHook; CreateProc:TCreateTagProc); virtual;

    {$IFDEF PORTUGUES}
    //: Lê/escreve um valor puro de modo assincrono em um item do bloco.
    {$ELSE}
    //: Read/Writes a raw value asynchronously on a block item.
    {$ENDIF}
    property ValueRaw[index:LongInt]:Double read GetValue write SetValue;

    {$IFDEF PORTUGUES}
    //: Lê/escreve valores puros de modo assincrono no bloco.
    {$ELSE}
    //: Read/Writes a raw values asynchronously on block.
    {$ENDIF}
    property ValuesRaw:TArrayOfDouble read GetValues write SetValues;
  published

    {$IFDEF PORTUGUES}
    //: Número de elementos do bloco.
    {$ELSE}
    //: Number of elements of the block.
    {$ENDIF}
    property Size write SetSize;
    //: @seealso(TTag.OnValueChange)
    property OnValueChange stored false;
    //: @seealso(TTag.OnValueChangeFirst)
    property OnValueChangeFirst;
    //: @seealso(TTag.OnValueChangeLast)
    property OnValueChangeLast;
    //: @seealso(TTag.OnUpdate)
    property OnUpdate;
    //: @seealso(TTag.OnAsyncValueChange)
    property OnAsyncValueChange;
    //: @seealso(TPLCTag.SyncWrites)
    property SyncWrites;
    //: @seealso(TPLCTag.TagType)
    property TagType;
    //: @seealso(TPLCTag.SwapBytes)
    property SwapBytes;
    //: @seealso(TPLCTag.SwapWords)
    property SwapWords;
    //: @seealso(TPLCTag.SwapDWords)
    property SwapDWords;
    //: @seealso(TPLCTag.TagSizeOnProtocol)
    property TagSizeOnProtocol;
    //: @seealso(TPLCTag.AvgUpdateRate)
    property AvgUpdateRate;
    //: @seealso(TPLCTag.Modified)
    property Modified;
  end;

  procedure SetBlockElementMapper(ElementMapperTool:TOpenTagEditor);

implementation

uses hsstrings, math;

constructor TPLCBlock.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   PSize:=1;
   SetLength(PValues,PSize);
end;

destructor TPLCBlock.Destroy;
begin
  SetLength(PValues,0);
  inherited Destroy;
end;

function TPLCBlock.IsMyCallBack(Cback: TTagCommandCallBack): Boolean;
begin
  Result:=inherited IsMyCallBack(Cback) and (TMethod(Cback).Code=Pointer(@TPLCBlock.TagCommandCallBack));
end;

procedure TPLCBlock.TagCommandCallBack(const ReqID: LongWord;
  Values: TArrayOfDouble; ValuesTimeStamp: TDateTime; TagCommand: TTagCommand;
  LastResult: TProtocolIOResult; Offset: LongInt);
var
  c:LongInt;
  notify:Boolean;
  TagValues:TArrayOfDouble;
  PreviousTimestamp:TDateTime;
begin
  if (csDestroying in ComponentState) then exit;
  PreviousTimestamp:=PValueTimeStamp;
  try
    inherited TagCommandCallBack(ReqID, Values, ValuesTimeStamp, TagCommand, LastResult, Offset);
    TagValues:=PLCValuesToTagValues(Values, Offset);
    notify := false;
    case TagCommand of
      tcScanRead, tcRead, tcInternalUpdate, tcSingleScanRead:
      begin
        PValueTimeStamp := ValuesTimeStamp;
        if LastResult in [ioOk, ioNullDriver] then begin
          for c := low(TagValues) to High(TagValues) do begin
            if (c+Offset<Length(PValues)) then begin
              notify := notify or (PValues[c+Offset]<>TagValues[c]) or (IsNan(TagValues[c]) and (not IsNaN(PValues[c+Offset])));
              PValues[c+Offset]:=TagValues[c]
            end else begin
              {$IFNDEF WINDOWS}
              writeln({$I %FILE%},' at line ',{$I %LINE%},' (',{$I %CurrentRoutine%} ,'): Please fix-me: ',ClassName,'(',Name,') PValuesLen=',Length(PValues),' offset=',Offset,' TagValues Len=',Length(TagValues));
              {$ENDIF}
              break;
            end;
          end;
          if (TagCommand<>tcInternalUpdate) AND (LastResult=ioOk) then begin
            IncCommReadOK(1);
            PModified:=false;
          end;
        end else begin
          if (TagCommand<>tcInternalUpdate) then begin
            IncCommReadFaults(1);
          end;
        end;
      end;
      tcScanWrite,tcWrite:
      begin
        PValueTimeStamp := ValuesTimeStamp;
        if LastResult in [ioOk, ioNullDriver] then begin
          if LastResult=ioOk then begin
            IncCommWriteOK(1);
            PModified:=False;
          end;
          for c := 0 to High(TagValues) do begin
            notify := notify or (PValues[c+Offset]<>TagValues[c]);
            PValues[c+Offset]:=TagValues[c]
          end;

        end else
          IncCommWriteFaults(1);
      end;
    end;

    case TagCommand of
      tcScanRead:
        PLastASyncReadCmdResult := LastResult;
      tcScanWrite:
        PLastASyncWriteCmdResult := LastResult;
      tcRead:
        PLastSyncReadCmdResult := LastResult;
      tcWrite:
        PLastSyncWriteCmdResult := LastResult;
    end;

    if notify or PFirstUpdate then begin
      if (TagCommand in [tcRead, tcScanRead, tcSingleScanRead]) or (ProtocolDriver=nil) then PFirstUpdate:=false;
      NotifyChange;
    end;

    if (TagCommand in [tcRead, tcScanRead, tcSingleScanRead]) and (LastResult=ioOk) and (PreviousTimestamp<>PValueTimeStamp) then
      NotifyUpdate;

  finally
    SetLength(TagValues,0);
  end;
end;

procedure TPLCBlock.SetSize(isize:Cardinal);
begin
  if (isize>0) AND (PSize<>isize) then begin
    if (PProtocolDriver<>nil) and PAutoRead then
       PProtocolDriver.RemoveTag(Self);

    PSize := isize;
    SetLength(PValues,PSize);

    if ([csReading,csLoading]*ComponentState=[]) then
      GetNewProtocolTagSize;

    if (PProtocolDriver<>nil) and PAutoRead then
      PProtocolDriver.AddTag(Self);
  end;
end;

function  TPLCBlock.GetValue(Index:LongInt):Double;
begin
   if ((index<0) or (Index>High(PValues))) then begin
     raise Exception.Create(Format(SoutOfBounds2,[ClassName, Name, Index,Length(PValues)]));
   end;
   Result := PValues[Index];
end;

procedure TPLCBlock.SetValue(index:LongInt; Value:Double);
var
  towrite:TArrayOfDouble;
begin
  PModified:=true;
  SetLength(towrite,1);
  try
    towrite[0] := Value;
    if FSyncWrites then
      Write(towrite,1,index)
    else
      ScanWrite(towrite,1,index);
  finally
    SetLength(towrite,0);
  end;

end;

function TPLCBlock.GetValues:TArrayOfDouble;
begin
  Result:=PValues;
end;

procedure TPLCBlock.SetValues(values:TArrayOfDouble);
var
  towrite:TArrayOfDouble;
begin
  PModified:=true;
  towrite := values;
  try
    if FSyncWrites then
      Write(towrite,PSize,0)
    else
      ScanWrite(towrite,PSize,0);
  finally
    SetLength(towrite,0);
  end;
end;

procedure TPLCBlock.AsyncNotifyChange(data: Pointer);
var
  x:PArrayOfDouble;
begin
  if not Assigned(POnAsyncValueChange) then exit;
  x:=data;
  POnAsyncValueChange(self,x^);
end;

function TPLCBlock.GetValueChangeData: Pointer;
var
  x:PArrayOfDouble;
begin
  New(x);
  x^:=PValues;
  Result:=x;
end;

procedure TPLCBlock.ReleaseChangeData(data: Pointer);
var
  x:PArrayOfDouble;
begin
  x:=data;
  SetLength(x^,0);
  Dispose(x);
end;

procedure TPLCBlock.WriteByScan;
var
  x:Boolean;
begin
  x:=PAutoWrite;
  try
    PAutoWrite := true;
    ScanWrite(PValues,PSize,0);
  finally
    PAutoWrite := x;
  end;
end;

procedure TPLCBlock.WriteDirect;
begin
  Write(PValues,PSize,0);
end;

function TPLCBlock.ScanRead: Int64;
begin
  Result := inherited ScanRead;
end;

procedure TPLCBlock.Read;
begin
  inherited Read;
end;

function TPLCBlock.ScanWrite(Values: TArrayOfDouble; Count, Offset: Cardinal;
  const IgnoreAutoWrite: Boolean): Int64;
var
  PLCValues:TArrayOfDouble;
begin
  PLCValues:=TagValuesToPLCValues(Values,Offset);
  try
    Result := inherited ScanWrite(PLCValues, Count, Offset, IgnoreAutoWrite);
  finally
    SetLength(PLCValues,0);
  end;
end;

procedure TPLCBlock.Write(Values:TArrayOfDouble; Count, Offset:Cardinal);
var
  PLCValues:TArrayOfDouble;
begin
  PLCValues:=TagValuesToPLCValues(Values, Offset);
  inherited Write(PLCValues, Count, Offset);
  SetLength(PLCValues,0);
end;

var
  ElementMapperEditor:TOpenTagEditor = nil;

procedure TPLCBlock.MapElements(InsertHook: TAddTagInEditorHook;
  CreateProc: TCreateTagProc);
begin
  if Assigned(ElementMapperEditor) then
    ElementMapperEditor(Self, Self.Owner,InsertHook,CreateProc)
  else
    raise exception.Create('None element mapper tool has been assigned!');
end;

procedure SetBlockElementMapper(ElementMapperTool:TOpenTagEditor);
begin
  if assigned(ElementMapperEditor) then
    raise Exception.Create('A Bit Mapper editor was already assigned.')
  else
    ElementMapperEditor:=ElementMapperTool;
end;


end.
