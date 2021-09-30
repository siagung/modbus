{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Unit do formul�rio assistente de cria��o de estruturas do tag TPLCStruct.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Unit of Struct item mapper wizard.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit ustructuremapper;

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ExtCtrls, Buttons, us7tagbuilder
  {$IFDEF FPC}, LCLIntf, LResources{$ENDIF};

type

  {$IFDEF PORTUGUES}
  {:
  Assistente de cria��o de estruturas usando o tag TPLCStruct.

  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ELSE}
  {:
  Struct mapper wizard.

  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
  }
  {$ENDIF}
  TfrmStructureEditor = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    SpinEdit1: TSpinEdit;
    Button1: TButton;
    ScrollBox1: TScrollBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var aAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FTagList,
    ItemsToDel:TList;
    FItemId:LongInt;
    procedure CheckNames(Sender:TObject; NewName:AnsiString; var AcceptNewName:Boolean);
    procedure btnUpClick(Sender:TObject);
    procedure btnDownClick(Sender:TObject);
    procedure btnDelClick(Sender:TObject);
    procedure btnBitsClick(Sender:TObject);
    procedure BitItemDeleted(Sender:TObject);
    function  GetStructItemsCount:LongInt;
    function  GetStructItem(index:LongInt):TS7TagItemEditor;
  public
    destructor Destroy; override;
    function HasAtLeastOneValidItem:Boolean;
    property StructItemsCount:LongInt read GetStructItemsCount;
    property StructItem[index:LongInt]:TS7TagItemEditor read GetStructItem;
  end;

var
  frmStructureEditor: TfrmStructureEditor;

implementation

uses Tag, ubitmapper, hsstrings;

{$IFDEF FPC }
  {$IF defined(FPC) AND (FPC_FULLVERSION >= 20400) }
  {$R ustructuremapper.lfm}
  {$IFEND}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

destructor TfrmStructureEditor.Destroy;
begin
  FTagList.Destroy;
  ItemsToDel.Destroy;
  inherited Destroy;
end;

procedure TfrmStructureEditor.Button1Click(Sender: TObject);
var
  aTag, lastitem:TS7TagItemEditor;
begin
  aTag:=TS7TagItemEditor.Create(Self);
  aTag.Parent := ScrollBox1;
  aTag.PopulateCombo;
  aTag.OnCheckNames:=@CheckNames;
  aTag.OnUpClick:=@btnUpClick;
  aTag.OnDownClickEvent:=@btnDownClick;
  aTag.OnDelClickEvent:=@btnDelClick;
  aTag.OnBitsClickEvent:=@btnBitsClick;
  aTag.OnDelBitItem:=@BitItemDeleted;
  aTag.TagScan:=1000;
  aTag.TagType:=pttDefault;
  aTag.SwapBytes:=false;
  aTag.SwapWords:=false;
  if FTagList.Count>0 then begin
    lastitem:=TS7TagItemEditor(FTagList.Items[FTagList.Count-1]);
    aTag.Top:=lastitem.Top+lastitem.Height;
  end else begin
    aTag.Top:=0;
  end;

  while not aTag.AcceptName('StructItem'+IntToStr(FItemId)) do
    inc(FItemId);
  aTag.TagName:='StructItem'+IntToStr(FItemId);

  FTagList.Add(aTag);
end;

procedure TfrmStructureEditor.BitBtn2Click(Sender: TObject);
begin
  //
end;

procedure TfrmStructureEditor.BitBtn1Click(Sender: TObject);
begin
  if not HasAtLeastOneValidItem then
    Raise Exception.Create(SYouMustHaveAtLeastOneStructureItem);
end;

procedure TfrmStructureEditor.FormCreate(Sender: TObject);
begin
  FItemId:=1;
  FTagList:=TList.Create;
  ItemsToDel:=TList.Create;
end;

procedure TfrmStructureEditor.FormClose(Sender: TObject;
  var aAction: TCloseAction);
begin
  //
end;

procedure TfrmStructureEditor.FormShow(Sender: TObject);
begin
  Button1Click(Sender);
end;

procedure TfrmStructureEditor.CheckNames(Sender:TObject; NewName:AnsiString; var AcceptNewName:Boolean);
var
  t,b:LongInt;
begin
  for t:=0 to FTagList.Count-1 do begin
    if TObject(FTagList.Items[t])=Sender then continue;
    if TS7TagItemEditor(FTagList.Items[t]).TagName=NewName then begin
      AcceptNewName:=false;
      exit;
    end;
    for b:=0 to TS7TagItemEditor(FTagList.Items[t]).BitCount-1 do begin
      if TS7TagItemEditor(FTagList.Items[t]).Bit[b]=Sender then continue;
      if TTagBitItemEditor(TS7TagItemEditor(FTagList.Items[t]).Bit[b]).TagName=NewName then begin
        AcceptNewName:=false;
        exit;
      end;
    end;
  end;
end;

procedure TfrmStructureEditor.btnUpClick(Sender:TObject);
var
  idx:LongInt;
  priortop, actualTop:LongInt;
  prior:TS7TagItemEditor;
begin
  if not (Sender is TS7TagItemEditor) then exit;

  idx := FTagList.IndexOf(Sender);
  if idx>0 then begin
    prior:=TS7TagItemEditor(FTagList.Items[idx-1]);

    priortop:=prior.Top;
    actualTop:=(Sender as TS7TagItemEditor).Top;

    FTagList.Exchange(idx-1, idx);

    (Sender as TS7TagItemEditor).Top:=priortop;
    (Sender as TS7TagItemEditor).TabOrder:=prior.TabOrder;
    prior.Top:=actualTop;
  end;
end;

procedure TfrmStructureEditor.btnDownClick(Sender:TObject);
var
  idx:LongInt;
  nexttop, actualTop:LongInt;
  aNext:TS7TagItemEditor;
begin
  if not (Sender is TS7TagItemEditor) then exit;

  idx := FTagList.IndexOf(Sender);
  if (idx<>-1) and (idx<(FTagList.Count-1)) then begin
    aNext:=TS7TagItemEditor(FTagList.Items[idx+1]);

    nexttop:=aNext.Top;
    actualTop:=(Sender as TS7TagItemEditor).Top;

    FTagList.Exchange(idx+1, idx);

    (Sender as TS7TagItemEditor).Top:=nexttop;
    aNext.TabOrder:=(Sender as TS7TagItemEditor).TabOrder;
    aNext.Top:=actualTop;
  end;
end;

procedure TfrmStructureEditor.btnDelClick(Sender:TObject);
begin
  if ItemsToDel.IndexOf(Sender)=-1 then
    if MessageDlg('Remove the structure item called "'+TS7TagItemEditor(Sender).TagName+'"?', mtConfirmation,[mbYes,mbNo],0)=mrYes then begin
      ItemsToDel.Add(Sender);
      Timer1.Enabled:=true;
    end;
end;

procedure TfrmStructureEditor.btnBitsClick(Sender:TObject);
var
  frmbit:TfrmBitMapper;
  ti:TTagBitItemEditor;
  s7tageditor:TS7TagItemEditor;
  bitnum,
  bytenum,
  wordnum,
  startbit,
  endbit,
  curbit:LongInt;

  procedure updatenumbers;
  begin
    bitnum:=curbit;
    if frmbit.bitnamestartsfrom1.Checked then inc(bitnum);

    bytenum:=curbit div 8;
    if frmbit.bytenamestartsfrom1.Checked then inc(bytenum);

    wordnum:=curbit div 16;
    if frmbit.Wordnamestartsfrom1.Checked then inc(wordnum);
  end;

  function GetNewTagBitName:AnsiString;
  var
    n:AnsiString;
  begin
    n:=IntToStr(bitnum);
    Result:=frmbit.edtNamepattern.Text;
    Result := StringReplace(Result,'%b',n,[rfReplaceAll]);

    n:=IntToStr(bytenum);
    Result := StringReplace(Result,'%B',n,[rfReplaceAll]);

    n:=IntToStr(wordnum);
    Result := StringReplace(Result,'%w',n,[rfReplaceAll]);

    n:=(Sender as TS7TagItemEditor).TagName;
    Result := StringReplace(Result,'%t',n,[rfReplaceAll]);
  end;
begin
  if not (Sender is TS7TagItemEditor) then exit;

  s7tageditor := (Sender as TS7TagItemEditor);

  frmbit:=TfrmBitMapper.Create(Self);
  try
    if frmbit.ShowModal=mrOk then begin
      startbit:=31-frmbit.StringGrid1.Selection.Right;
      endbit:=31-frmbit.StringGrid1.Selection.Left;
      curbit:=startbit;
      if frmbit.eachbitastag.Checked then begin
        while curbit<=endbit do begin
          updatenumbers;
          ti:=s7tageditor.AddBit;
          ti.TagName:=GetNewTagBitName;
          ti.EndBit:=curbit;
          ti.StartBit:=curbit;
          inc(curbit);
        end;
      end else begin
        updatenumbers;
        ti:=s7tageditor.AddBit;
        ti.TagName:=GetNewTagBitName;
        ti.EndBit:=endbit;
        ti.StartBit:=startbit;
      end;
    end;
  finally
    frmbit.Destroy;
  end;
end;

procedure TfrmStructureEditor.BitItemDeleted(Sender:TObject);
begin
  //
end;

function  TfrmStructureEditor.GetStructItemsCount:LongInt;
begin
  Result:=FTagList.Count;
end;

function  TfrmStructureEditor.GetStructItem(index:LongInt):TS7TagItemEditor;
begin
  Result:=TS7TagItemEditor(FTagList.Items[index]);
end;

function TfrmStructureEditor.HasAtLeastOneValidItem:Boolean;
var
  c:LongInt;
begin
  Result:=false;
  for c:=0 to StructItemsCount-1 do
    if not StructItem[c].SkipTag then begin
      Result:=true;
      break;
    end;
end;

procedure TfrmStructureEditor.Timer1Timer(Sender: TObject);
var
  c:LongInt;
begin
  for c:=ItemsToDel.Count-1 downto 0 do begin
    FTagList.Remove(ItemsToDel.Items[c]);
    TS7TagItemEditor(ItemsToDel.Items[c]).Destroy;
    ItemsToDel.Delete(c);
  end;
  Timer1.Enabled:=false;
  if FTagList.Count=0 then FItemId:=1;
end;

{$IFDEF FPC }
  {$IF defined(FPC) AND (FPC_FULLVERSION < 20400) }
initialization
  {$i ustructuremapper.lrs}
  {$IFEND}
{$ENDIF}

end.
