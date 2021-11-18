{
 /***************************************************************************
                                textstrings.pas
                                ---------------
                             Component Library Code


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  TTextStrings is a TStrings descendent that is optimized for handling the
  complete text as whole (instead of as line by line as in TStringList).
}
unit hmitextstrings;

{$POINTERMATH ON}

interface

uses
  Classes, SysUtils;
  
type
  { TTextStrings }

  TTextLineRange = record
    StartPos: LongInt; // start of line in Text
    EndPos: LongInt; // end of line in Text (= start of newline character(s))
    Line: AnsiString; // cached line as string
    TheObject: TObject; // user data
  end;

  TTextStrings = class(TStrings)
  private
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
  protected
    FArraysValid: boolean;
    FLineCount: LongInt;
    FLineCapacity: LongInt;
    FLineRanges: ^TTextLineRange;// array of TTextLineRange
    FText: UTF8string;
    FUpdateCount: LongInt;
    FChangedWhileUpdate: boolean;
    function GetTextStr: UTF8string; override;
    procedure SetTextStr(const AValue: UTF8string); override;
    procedure BuildArrays; virtual;
    function GetCount: LongInt; override;
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: LongInt): UTF8string; override;
    procedure ClearArrays;
    function GetObject(Index: LongInt): TObject; override;
    procedure Put(Index: LongInt; const S: UTF8string); override;
    procedure PutObject(Index: LongInt; AnObject: TObject); override;
    function GetLineLen(Index: LongInt; IncludeNewLineChars: boolean): LongInt; inline;
    function GetLineEnd(Index: LongInt; IncludeNewLineChars: boolean): LongInt;
    function HasObjects: boolean;
    function CountLineEndings(const s: UTF8string): LongInt;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure SetText(TheText: PChar); override;
    procedure Insert(Index: LongInt; const S: UTF8string); override;
    procedure Delete(Index: LongInt); override;
    procedure Exchange(Index1, Index2: LongInt); override;
    procedure Move(CurIndex, NewIndex: LongInt); override;
    procedure MakeTextBufferUnique;
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetText: PChar; override;
    function IndexOf(const S: UTF8string): LongInt; override;
    function Add(const S: UTF8string): LongInt; override;
    function AddObject(const S: UTF8string; AObject: TObject): LongInt; override;
    procedure AddStrings(TheStrings: TStrings); override;
  public
    property Text: UTF8string read FText write SetTextStr;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

resourceString
  rsListIndexExceedsBounds = 'List index exceeds bounds (%d)';

Const
  LineEnding = #13#10;

implementation

{ TTextStrings }

function TTextStrings.GetTextStr: UTF8string;
begin
  Result:=FText;
end;

procedure TTextStrings.SetTextStr(const AValue: UTF8string);
begin
  if FText=AValue then exit;
  FText:=AValue;
  FArraysValid:=false;
end;

procedure TTextStrings.BuildArrays;
var
  p, line: LongInt;
  l: LongInt;
  ArraySize: LongInt;
begin
  if FArraysValid then exit;
  ClearArrays;
  FArraysValid:=true;
  // count line ends
  FLineCount:=CountLineEndings(FText);
  l:=length(FText);
  if (FText<>'') and (not (FText[l] in [#10,#13])) then
    inc(FLineCount);
  FLineCapacity:=FLineCount;
  // build line range list
  if FLineCount>0 then begin
    ArraySize:=FLineCount*SizeOf(TTextLineRange);
    GetMem(FLineRanges,ArraySize);
    FillChar(FLineRanges^,ArraySize,0);
    p:=1;
    line:=0;
    FLineRanges[line].StartPos:=1;
    FLineRanges[FLineCount-1].EndPos:=l+1;
    while (p<=l) do begin
      if (not (FText[p] in [#10,#13])) then begin
        inc(p);
      end else begin
        // new line
        FLineRanges[line].EndPos:=p;
        inc(line);
        inc(p);
        if (p<=l) and (FText[p] in [#10,#13])
        and (FText[p]<>FText[p-1]) then
          inc(p);
        if line<FLineCount then
          FLineRanges[line].StartPos:=p;
      end;
    end;
  end;
end;

function TTextStrings.GetCount: LongInt;
begin
  if not FArraysValid then BuildArrays;
  Result:=FLineCount;
end;

procedure TTextStrings.Changed;
// called after text changed
begin
  if (FUpdateCount>0) then begin
    FChangedWhileUpdate:=true;
    exit;
  end;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TTextStrings.Changing;
begin
  if FUpdateCount=0 then
    if Assigned(FOnChanging) then
      FOnChanging(Self);
end;

function TTextStrings.Get(Index: LongInt): UTF8string;
begin
  if not FArraysValid then BuildArrays;
  if (Index<0) or (Index>=FLineCount) then
    Error(rsListIndexExceedsBounds, Index);
  if (FLineRanges[Index].Line='')
  and (FLineRanges[Index].StartPos<FLineRanges[Index].EndPos) then begin
    FLineRanges[Index].Line:=copy(FText,FLineRanges[Index].StartPos,
                         FLineRanges[Index].EndPos-FLineRanges[Index].StartPos);
  end;
  Result:=FLineRanges[Index].Line;
end;

procedure TTextStrings.ClearArrays;
var
  i: LongInt;
begin
  FArraysValid:=false;
  if FLineRanges<>nil then begin
    for i:=0 to FLineCount-1 do
      FLineRanges[i].Line:='';
    FreeMem(FLineRanges);
    FLineRanges:=nil;
  end;
  FLineCapacity:=0;
end;

function TTextStrings.GetObject(Index: LongInt): TObject;
begin
  if FArraysValid then
    Result:=FLineRanges[Index].TheObject
  else
    Result:=nil;
end;

procedure TTextStrings.Put(Index: LongInt; const S: UTF8string);
var
  OldLineLen: LongInt;
  NewLineLen: LongInt;
  Movement: LongInt;
  OldStartPos: LongInt;
  OldEndPos: LongInt;
  MoveLen: LongInt;
  i: LongInt;
  NewEndPos: LongInt;
begin
  if not FArraysValid then BuildArrays;
  OldStartPos:=FLineRanges[Index].StartPos;
  OldEndPos:=FLineRanges[Index].EndPos;
  NewLineLen:=length(s);
  OldLineLen:=OldEndPos-OldStartPos;
  Movement:=NewLineLen-OldLineLen;
  NewEndPos:=OldEndPos+Movement;
  // move text behind
  MoveLen := Length(FText) - OldEndPos + 1;
  if (Movement<>0) and (MoveLen>0) then
  begin
    if Movement > 0 then
      SetLength(FText, Length(FText) + Movement);
    System.Move(FText[OldEndPos], FText[NewEndPos], MoveLen);
    if Movement < 0 then
      SetLength(FText, Length(FText) + Movement);

    for i := Index + 1 to FLineCount - 1 do
    begin
      inc(FLineRanges[i].StartPos, Movement);
      inc(FLineRanges[i].EndPos, Movement);
    end;
  end;
  FLineRanges[Index].EndPos:=NewEndPos;
  // copy text
  if NewLineLen>0 then
    System.Move(S[1],FText[OldStartPos],NewLineLen);
  FLineRanges[Index].Line:=S;
  // check if arrays need rebuild
  i:=NewLineLen;
  while (i>0) and (not (S[i] in [#10,#13])) do dec(i);
  if i>0 then begin
    // S contains new line chars => rebuild needed
    FArraysValid:=false;
  end;
end;

procedure TTextStrings.PutObject(Index: LongInt; AnObject: TObject);
begin
  if not FArraysValid then BuildArrays;
  FLineRanges[Index].TheObject:=AnObject;
end;

function TTextStrings.GetLineLen(Index: LongInt; IncludeNewLineChars: boolean
  ): LongInt;
begin
  Result:=GetLineEnd(Index,IncludeNewLineChars)-FLineRanges[Index].StartPos;
end;

function TTextStrings.GetLineEnd(Index: LongInt; IncludeNewLineChars: boolean
  ): LongInt;
begin
  if not FArraysValid then BuildArrays;
  if not IncludeNewLineChars then
    Result:=FLineRanges[Index].EndPos
  else if Index=FLineCount-1 then
    Result:=length(FText)+1
  else
    Result:=FLineRanges[Index+1].StartPos;
end;

function TTextStrings.HasObjects: boolean;
var
  i: LongInt;
begin
  if FArraysValid then
    for i:=0 to FLineCount-1 do
      if FLineRanges[i].TheObject<>nil then
        exit(true);
  Result:=false;
end;

function TTextStrings.CountLineEndings(const s: UTF8string): LongInt;
var
  p: LongInt;
  l: LongInt;
begin
  Result:=0;
  l:=length(s);
  p:=1;
  while p<=l do begin
    if s[p] in [#10,#13] then
    begin
      inc(Result);
      inc(p);
      if (p<=l) and (s[p] in [#10,#13]) and (s[p-1]<>s[p]) then
        inc(p);
    end else begin
      inc(p);
    end;
  end;
end;

constructor TTextStrings.Create;
begin
  inherited Create;
  //CheckSpecialChars;
end;

destructor TTextStrings.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TTextStrings.Clear;
begin
  ClearArrays;
  FLineCount:=0;
  FText:='';
end;

procedure TTextStrings.SetText(TheText: PChar);
begin
  if FText=TheText then exit;
  FText:=TheText;
  FArraysValid:=false;
end;

procedure TTextStrings.Insert(Index: LongInt; const S: UTF8string);
var
  NewStartPos: LongInt;
  NewLineCharCount: LongInt;
  NewLineLen: LongInt;
  i: LongInt;
  SEndsInNewLine: boolean;
begin
  if not FArraysValid then BuildArrays;
  NewLineLen:=length(S);
  SEndsInNewLine:=(S<>'') and (S[NewLineLen] in [#10,#13]);
  if Index<FLineCount then
    NewStartPos:=FLineRanges[Index].StartPos
  else
    NewStartPos:=length(FText);
  NewLineCharCount:=0;
  if SEndsInNewLine then begin
    inc(NewLineCharCount);
    if (NewLineLen>1)
    and (S[NewLineLen-1] in [#10,#13])
    and (S[NewLineLen-1]<>S[NewLineLen]) then
      inc(NewLineCharCount);
    System.Insert(S,FText,NewStartPos);
  end else begin
    // append missing newline char
    System.Insert(S+LineEnding,FText,NewStartPos);
    NewLineCharCount:=length(LineEnding);
    inc(NewLineLen,NewLineCharCount);
  end;
  // adjust arrays
  if FLineCount=FLineCapacity then begin
    if FLineCapacity<8 then
      FLineCapacity:=8
    else
      FLineCapacity:=FLineCapacity shl 1;
    ReAllocMem(FLineRanges,SizeOf(TTextLineRange)*FLineCapacity);
  end;
  if Index<FLineCount then begin
    System.Move(FLineRanges[Index],FLineRanges[Index+1],
                (FLineCount-Index)*SizeOf(TTextLineRange));
    for i:=Index+1 to FLineCount do begin
      inc(FLineRanges[i].StartPos,NewLineLen);
      inc(FLineRanges[i].EndPos,NewLineLen);
    end;
  end;
  FLineRanges[Index].Line:=S;
  FLineRanges[Index].EndPos:=NewStartPos+NewLineLen-NewLineCharCount;
  inc(FLineCount);
end;

procedure TTextStrings.Delete(Index: LongInt);
var
  OldLineLen: LongInt;
  OldStartPos: LongInt;
  i: LongInt;
begin
  if not FArraysValid then BuildArrays;
  // adjust text
  OldLineLen:=GetLineLen(Index,true);
  if OldLineLen>0 then begin
    OldStartPos:=FLineRanges[Index].StartPos;
    System.Delete(FText,OldStartPos,OldLineLen);
  end;
  // adjust arrays
  dec(FLineCount);
  FLineRanges[Index].Line:='';
  if Index<FLineCount then begin
    System.Move(FLineRanges[Index+1],FLineRanges[Index],
         (FLineCount-Index)*SizeOf(TTextLineRange));
    for i:=Index to FLineCount-1 do begin
      dec(FLineRanges[i].StartPos,OldLineLen);
      dec(FLineRanges[i].EndPos,OldLineLen);
    end;
  end;
end;

procedure TTextStrings.Exchange(Index1, Index2: LongInt);
var
  LineLen1: LongInt;
  LineLen2: LongInt;
  buf: Pointer;
  Dummy: LongInt;
  OldBetweenStart: LongInt;
  NewBetweenStart: LongInt;
  BetweenLength: LongInt;
  StartPos1: LongInt;
  StartPos2: LongInt;
  i: LongInt;
  Movement: LongInt;
  Obj: TObject;
  LineShortLen1: LongInt;
  LineShortLen2: LongInt;
begin
  // check values
  if Index1=Index2 then exit;
  if Index1<0 then
    Error(rsListIndexExceedsBounds, Index1);
  if Index2<0 then
    Error(rsListIndexExceedsBounds, Index2);
  if not FArraysValid then BuildArrays;
  if Index1>=FLineCount then
    Error(rsListIndexExceedsBounds, Index1);
  if Index2>=FLineCount then
    Error(rsListIndexExceedsBounds, Index2);

  // make sure Index1<Index2
  if Index1>Index2 then begin
    Dummy:=Index1;
    Index1:=Index2;
    Index2:=Dummy;
  end;

  // adjust text
  MakeTextBufferUnique;

  if (Index2=FLineCount-1) and (FLineRanges[Index2].EndPos=length(FText))
  then begin
    // The last line should be exchanged,
    // but Text has no new line character(s) at the end
    // => add LineEnding
    FText:=FText+LineEnding;
  end;

  // get line lengths including new line chars
  LineLen1:=GetLineLen(Index1,true);
  LineLen2:=GetLineLen(Index2,true);
  if (LineLen1<1) and (LineLen2<1) then exit;
  LineShortLen1:=GetLineLen(Index1,false);
  LineShortLen2:=GetLineLen(Index2,false);

  // save the bigger line
  StartPos1:=FLineRanges[Index1].StartPos;
  StartPos2:=FLineRanges[Index2].StartPos;
  if LineLen1>=LineLen2 then begin
    GetMem(buf,LineLen1);
    System.Move(FText[StartPos1],buf^,LineLen1);
  end else begin
    GetMem(buf,LineLen2);
    System.Move(FText[StartPos2],buf^,LineLen2);
  end;

  // move text in between
  OldBetweenStart:=StartPos1+LineLen1;
  BetweenLength:=StartPos2-OldBetweenStart;
  NewBetweenStart:=StartPos1+LineLen2;
  if (BetweenLength>0) and (OldBetweenStart<>NewBetweenStart) then
    System.Move(FText[OldBetweenStart],FText[NewBetweenStart],BetweenLength);

  // move both lines
  if LineLen1>=LineLen2 then begin
    System.Move(FText[StartPos2],FText[StartPos1],LineLen2);
    System.Move(buf^,FText[StartPos2],LineLen1);
  end else begin
    System.Move(FText[StartPos1],FText[StartPos2],LineLen1);
    System.Move(buf^,FText[StartPos1],LineLen2);
  end;

  // adjust line ranges
  Movement:=NewBetweenStart-OldBetweenStart;
  FLineRanges[Index1].EndPos:=FLineRanges[Index1].StartPos+LineShortLen2;
  inc(FLineRanges[Index2].StartPos,Movement);
  FLineRanges[Index2].EndPos:=FLineRanges[Index2].StartPos+LineShortLen1;
  if (BetweenLength>0) and (OldBetweenStart<>NewBetweenStart) then begin
    for i:=Index1+1 to Index2-1 do begin
      inc(FLineRanges[i].StartPos,Movement);
      inc(FLineRanges[i].EndPos,Movement);
    end;
  end;

  // exchange TheObject
  Obj:=FLineRanges[Index1].TheObject;
  FLineRanges[Index1].TheObject:=FLineRanges[Index2].TheObject;
  FLineRanges[Index2].TheObject:=Obj;

  // clean up
  FreeMem(buf);
end;

procedure TTextStrings.Move(CurIndex, NewIndex: LongInt);
var
  SrcPos1: LongInt;
  SrcPos2: LongInt;
  SrcPos3: LongInt;
  LineStr: UTF8String;
  LineLen: LongInt;
  i: LongInt;
  Obj: TObject;
  LineShortLen: LongInt;
begin
  // check values
  if CurIndex=NewIndex then exit;
  if CurIndex<0 then
    Error(rsListIndexExceedsBounds, CurIndex);
  if NewIndex<0 then
    Error(rsListIndexExceedsBounds, NewIndex);
  if not FArraysValid then BuildArrays;
  if CurIndex>=FLineCount then
    Error(rsListIndexExceedsBounds, CurIndex);
  if NewIndex>=FLineCount then
    Error(rsListIndexExceedsBounds, NewIndex);

  // adjust text
  MakeTextBufferUnique;

  if CurIndex<NewIndex then
  begin
    // move to higher index
    if (NewIndex=FLineCount-1) and (FLineRanges[NewIndex].EndPos>length(FText))
    then begin
      // CurIndex should be moved to the end,
      // but Text has no new line character(s) at the end
      // => add LineEnding
      FText:=FText+LineEnding;
    end;
    SrcPos1:=FLineRanges[CurIndex].StartPos;
    SrcPos2:=FLineRanges[CurIndex+1].StartPos;
    SrcPos3:=GetLineEnd(NewIndex,true);
    // store current line with line end
    LineLen:=SrcPos2-SrcPos1;
    LineShortLen:=GetLineLen(CurIndex,false);
    LineStr:=copy(FText,SrcPos1,LineLen);
    Obj:=FLineRanges[CurIndex].TheObject;
    // move lines -1
    System.Move(FText[SrcPos2],FText[SrcPos1],SrcPos3-SrcPos2);
    for i:=CurIndex+1 to NewIndex do begin
      dec(FLineRanges[i].StartPos,LineLen);
      dec(FLineRanges[i].EndPos,LineLen);
    end;
    System.Move(FLineRanges[CurIndex+1],FLineRanges[CurIndex],
                SizeOf(TTextLineRange)*(NewIndex-CurIndex));
    // put current line at new position
    i:=SrcPos3-LineLen;
    System.Move(LineStr[1],FText[i],LineLen);
    FLineRanges[NewIndex].StartPos:=i;
    FLineRanges[NewIndex].EndPos:=i+LineShortLen;
    FLineRanges[NewIndex].Line:=''; // this will be updated on demand
    FLineRanges[NewIndex].TheObject:=Obj;
  end else begin
    // move to lower index
    if (CurIndex=FLineCount-1) and (FLineRanges[CurIndex].EndPos>length(FText))
    then begin
      // CurIndex should be moved from the end,
      // but Text has no new line character(s) at the end
      // => add LineEnding
      FText:=FText+LineEnding;
    end;
    SrcPos1:=FLineRanges[NewIndex].StartPos;
    SrcPos2:=FLineRanges[CurIndex].StartPos;
    SrcPos3:=GetLineEnd(CurIndex,true);
    // store current line with line end
    LineLen:=SrcPos3-SrcPos2;
    LineShortLen:=GetLineLen(CurIndex,false);
    LineStr:=copy(FText,SrcPos2,LineLen);
    Obj:=FLineRanges[CurIndex].TheObject;
    // move lines +1
    System.Move(FText[SrcPos1],FText[SrcPos1+LineLen],SrcPos2-SrcPos1);
    for i:=CurIndex-1 downto NewIndex do begin
      inc(FLineRanges[i].StartPos,LineLen);
      inc(FLineRanges[i].EndPos,LineLen);
    end;
    System.Move(FLineRanges[NewIndex],FLineRanges[NewIndex+1],
                SizeOf(TTextLineRange)*(CurIndex-NewIndex));
    // put current line at new position
    System.Move(LineStr[1],FText[SrcPos1],LineLen);
    FLineRanges[NewIndex].StartPos:=SrcPos1;
    FLineRanges[NewIndex].EndPos:=SrcPos1+LineShortLen;
    FLineRanges[NewIndex].Line:=''; // this will be updated on demand
    FLineRanges[NewIndex].TheObject:=Obj;
  end;
end;

procedure TTextStrings.MakeTextBufferUnique;
begin
  // make string unique (refcount=1) to be able to edit it directly
  UniqueString(FText);
end;

procedure TTextStrings.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TTextStrings.EndUpdate;

  procedure RaiseUpdateCount;
  begin
    raise Exception.Create('TTextStrings.EndUpdate');
  end;

begin
  if FUpdateCount<=0 then RaiseUpdateCount;
  dec(FUpdateCount);
  if FUpdateCount=0 then begin
    if FChangedWhileUpdate then
      Changed;
  end;
end;

function TTextStrings.GetText: PChar;
begin
  Result:=PChar(FText);
end;

function TTextStrings.IndexOf(const S: UTF8string): LongInt;
begin
  Result:=inherited IndexOf(S);
end;

function TTextStrings.Add(const S: UTF8string): LongInt;
begin
  Result:=AddObject(S,nil);
end;

function TTextStrings.AddObject(const S: UTF8string; AObject: TObject): LongInt;
var
  e: UTF8String;
  NewLineCount: LongInt;
  OldTxtLen: LongInt;
  p: LongInt;
  l: LongInt;
begin
  Result:=Count;
  if (FText<>'') and (not (FText[length(FText)] in [#10,#13])) then
    e:=LineEnding
  else
    e:='';
  OldTxtLen:=length(FText);
  FText:=Text+e+S+LineEnding;
  if AObject<>nil then
    BuildArrays;
  if FArraysValid then
  begin
    // update FLineRanges
    NewLineCount:=FLineCount+CountLineEndings(S)+1;
    if NewLineCount>FLineCapacity then begin
      FLineCapacity:=FLineCapacity*2+10;
      if FLineCapacity<NewLineCount then
        FLineCapacity:=NewLineCount;
      ReAllocMem(FLineRanges,SizeOf(TTextLineRange)*FLineCapacity);
      FillChar(FLineRanges[FLineCount],SizeOf(TTextLineRange)*(FLineCapacity-FLineCount),0);
    end;
    FLineRanges[FLineCount].TheObject:=AObject;
    p:=OldTxtLen+length(e)+1;
    l:=length(FText);
    while FLineCount<NewLineCount do begin
      FLineRanges[FLineCount].StartPos:=p;
      while (p<=l) and (not (FText[p] in [#10,#13])) do
        inc(p);
      FLineRanges[FLineCount].EndPos:=p;
      inc(p);
      if (p<=l) and (FText[p] in [#10,#13]) and (FText[p]<>FText[p-1]) then
        inc(p);
      inc(FLineCount);
    end;
  end;
end;

procedure TTextStrings.AddStrings(TheStrings: TStrings);

  function MustAddObjects: boolean;
  var
    i: LongInt;
  begin
    if HasObjects then exit(true);
    if TheStrings is TTextStrings then
      Result:=TTextStrings(TheStrings).HasObjects
    else
    begin
      for i:=0 to TheStrings.Count-1 do
        if TheStrings.Objects[i]<>nil then
          exit(true);
      Result:=false;
    end;
  end;

var
  s: String;
  i: LongInt;
begin
  if TheStrings.Count=0 then exit;
  if MustAddObjects then
  begin
    for i:=0 to TheStrings.Count-1 do
      AddObject(TheStrings[i],TheStrings.Objects[i]);
  end else begin
    if (FText<>'') and (not (FText[length(FText)] in [#10,#13])) then
      s:=LineEnding
    else
      s:='';
    FArraysValid:=false;
    FText:=FText+s+TheStrings.Text;
  end;
end;

end.
