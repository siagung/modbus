{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Implementação para Tags String.)
  @author(Fabio Luis Girardi fabio@pascalscada.com)
}
{$ELSE}
{:
  @abstract(Unit that implements a tag that can read/write string values.)
  @author(Fabio Luis Girardi fabio@pascalscada.com)
}
{$ENDIF}
unit PLCString;

interface

uses
  SysUtils, Classes, Tag, TagBlock, ProtocolTypes, ProtocolDriver, Math,
  hsutils;

type
  CP437AnsiString    = type AnsiString(437);
  CP646AnsiString    = type AnsiString(20127);
  CP850AnsiString    = type AnsiString(850);
  CP852AnsiString    = type Ansistring(852);
  CP856AnsiString    = type Ansistring(856);
  CP866AnsiString    = type Ansistring(866);
  CP874AnsiString    = type Ansistring(874);
  CP1250AnsiString   = type Ansistring(1250);
  CP1251AnsiString   = type Ansistring(1251);
  CP1252AnsiString   = type Ansistring(1252);
  CP1253AnsiString   = type Ansistring(1253);
  CP1254AnsiString   = type Ansistring(1254);
  CP1255AnsiString   = type Ansistring(1255);
  CP1256AnsiString   = type Ansistring(1256);
  CP1257AnsiString   = type Ansistring(1257);
  CP1258AnsiString   = type Ansistring(1258);
  CP8859_1AnsiString = type Ansistring(28591);
  CP8859_2AnsiString = type Ansistring(28592);
  CP8859_5AnsiString = type Ansistring(28595);

  TStringEncodings = (UTF_8,
                      CP437,
                      CP646,
                      CP850,
                      CP852,
                      CP856,
                      CP866,
                      CP874,
                      CP1250,
                      CP1251,
                      CP1252,
                      CP1253,
                      CP1254,
                      CP1255,
                      CP1256,
                      CP1257,
                      CP1258,
                      CP8859_1,
                      CP8859_2,
                      CP8859_5);


  {$IFDEF PORTUGUES}
  {:
  @author(Fabio Luis Girardi fabio@pascalscada.com)

  Define os possíveis tipos de strings.
  @value(stSIEMENS String no formato SIEMENS, onde os dois primeiros bytes da
  cadeia informam o tamanho máximo e quantos bytes desse tamanho já foram usados.)
  @value(stC A string só termina quando o código ASCII 0 é encontrado.)
  }
  {$ELSE}
  {:
  @author(Fabio Luis Girardi fabio@pascalscada.com)

  Defines the how the string will be encoded/decoded:
  @value(stSIEMENS String on SIEMENS format. The first byte tells the maximum
  size of string and the second byte tells the actual length of the string.)
  @value(stC The string finishes when a ASCII char 0 (string terminator) is found.)
  }
  {$ENDIF}
  TPLCStringTypes = (stSIEMENS, stC, stROCKWELL);

  {$IFDEF PORTUGUES}
  {:
  @abstract(Tag de comunicação capaz de escrever valores String no seu dispositivo.)
  @author(Fabio Luis Girardi fabio@pascalscada.com)
  }
  {$ELSE}
  {:
    @abstract(Communication tag that can read/write string values on your device.)
    @author(Fabio Luis Girardi fabio@pascalscada.com)
  }
  {$ENDIF}

  { TPLCString }

  TPLCString = class(TTagBlock, IScanableTagInterface, ITagInterface, ITagString)
  private
    FStringEncoding: TStringEncodings;
    PValue:UTF8String;
    PByteSize:Byte;
    PStringType:TPLCStringTypes;
    PStringSize:Cardinal;
    POnAsyncStringValueChange:TASyncStringValueChange;
    procedure SetBlockSize(asize:Cardinal);
    procedure SetStringEncoding(AValue: TStringEncodings);
    procedure SetStringSize(asize:Cardinal);
    procedure SetByteSize(bsize:Byte);
    procedure SetStringType(stype:TPLCStringTypes);
    procedure SetDummySize(s:Cardinal);

    function  GetValue:UTF8String;
    procedure SetValue(Value:UTF8String);
    function  CalcBlockSize(IsWrite:Boolean):Cardinal;
    function  ArrayOfValuesToString(values:TArrayOfDouble):UTF8String;
    function  StringToArrayOfValues(value: UTF8String): TArrayOfDouble;
    
    function  GetValueAsText(Prefix, Sufix, Format:UTF8String; FormatDateTimeOptions:TFormatDateTimeOptions=[]):UTF8String;
    function  GetVariantValue:Variant;
    procedure SetVariantValue(V:Variant);
    function  IsValidValue(Value:Variant):Boolean;
    function  GetValueTimestamp:TDatetime;
  protected
    //: @seealso(TTag.AsyncNotifyChange)
    procedure AsyncNotifyChange(data:Pointer); override;
    //: @seealso(TTag.GetValueChangeData)
    function GetValueChangeData: Pointer; override;
    //: @seealso(TTag.ReleaseChangeData)
    procedure ReleaseChangeData(data: Pointer); override;
    //: @seealso(TPLCTag.IsMyCallBack)
    function IsMyCallBack(Cback: TTagCommandCallBack): Boolean; override;
    //: @seealso(TPLCTag.SetPLCHack)
    procedure SetPLCHack(v:Cardinal); override;
    //: @seealso(TPLCTag.SetPLCSlot)
    procedure SetPLCSlot(v:Cardinal); override;
    //: @seealso(TPLCTag.SetPLCStation)
    procedure SetPLCStation(v:Cardinal); override;
    //: @seealso(TPLCTag.SetMemFileDB)
    procedure SetMemFileDB(v:Cardinal); override;
    //: @seealso(TPLCTag.SetMemAddress)
    procedure SetMemAddress(v:Cardinal); override;
    //: @seealso(TPLCTag.SetMemSubElement)
    procedure SetMemSubElement(v:Cardinal); override;
    //: @seealso(TPLCTag.SetMemReadFunction)
    procedure SetMemReadFunction(v:Cardinal); override;
    //: @seealso(TPLCTag.SetMemWriteFunction)
    procedure SetMemWriteFunction(v:Cardinal); override;
    //: @seealso(TPLCTag.SetPath)
    procedure SetPath(v:AnsiString); override;
    //: @seealso(TPLCTag.SetProtocolDriver)
    procedure SetProtocolDriver(p:TProtocolDriver); override;
    //: @seealso(TPLCTag.TagCommandCallBack)
    procedure TagCommandCallBack(const ReqID:LongWord; Values:TArrayOfDouble; ValuesTimeStamp:TDateTime; TagCommand:TTagCommand; LastResult:TProtocolIOResult; Offset:LongInt); override;
  public
    //: @exclude
    constructor Create(AOwner:TComponent); override;
    //: @exclude
    destructor  Destroy; override;

    class function ConvertRawByteStringToUTF8(aInput: RawByteString;
      aInputEncoding: TStringEncodings): UTF8String;
    class function ConvertUTF8CharToByte(aInput: UTF8String;
      aInputEncoding: TStringEncodings; aPos: Integer): Byte;

    {$IFDEF PORTUGUES}
    //: Lê/escreve uma string do equipamento.
    {$ELSE}
    //: Read/writes a string value on your device
    {$ENDIF}
    property Value:UTF8String read PValue write SetValue;

    //: @seealso(TTagBlock.Read)
    procedure Read; override;

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
  published

    {$IFDEF PORTUGUES}
    //: Quantidade máxima de caracteres da string.
    {$ELSE}
    //: Maximum length of your string.
    {$ENDIF}
    property StringSize:Cardinal read PStringSize write SetStringSize;

    {$IFDEF PORTUGUES}
    {:
    Tipo da String.
    @seealso(TPLCStringTypes)
    }
    {$ELSE}
    {:
    String format.
    @seealso(TPLCStringTypes)
    }
    {$ENDIF}
    property StringType:TPLCStringTypes read PStringType write SetStringType default stC;

    {$IFDEF PORTUGUES}
    //: Tamanho em bits de cada caracter da string.
    {$ELSE}
    //: Size in bits of each character of string.
    {$ENDIF}
    property ByteSize:Byte read PByteSize write SetByteSize default 8; deprecated;
    
    //: @seealso(TTag.OnValueChange)
    property OnValueChange stored false;
    //: @seealso(TTag.OnValueChangeFirst)
    property OnValueChangeFirst;
    //: @seealso(TTag.OnValueChangeLast)
    property OnValueChangeLast;

    {$IFDEF PORTUGUES}
    //: Evento assincrono chamado quando o valor do tag sofre uma alteração.
    {$ELSE}
    //: Asynchronous event called when the tag value changes.
    {$ENDIF}
    property OnAsyncStringChange:TASyncStringValueChange read POnAsyncStringValueChange write POnAsyncStringValueChange;

    {$IFDEF PORTUGUES}
    //: Tamanho do bloco (somente-leitura).
    {$ELSE}
    //: Real block size (read-only).
    {$ENDIF}
    property Size write SetDummySize;
    //: @seealso(TPLCTag.SyncWrites)
    property SyncWrites;

    property StringEncoding:TStringEncodings read FStringEncoding write SetStringEncoding default UTF_8;
  end;

implementation

uses variants, hsstrings{$IFDEF FPC}, LazUTF8{$ENDIF};

constructor TPLCString.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  PStringSize := 10;
  PByteSize := 8;
  PStringType := stC;
  SetBlockSize(11);
end;

destructor TPLCString.Destroy;
begin
  inherited Destroy;
  SetLength(PValues,0);
end;

procedure TPLCString.Read;
begin
  inherited Read;
end;

procedure TPLCString.WriteByScan;
var
  x:Boolean;
  values:TArrayOfDouble;
begin
  x:=PAutoWrite;
  PAutoWrite := true;
  values:=StringToArrayOfValues(PValue);
  ScanWrite(values,PSize,0);
  PAutoWrite := x;
  SetLength(values,0);
end;

procedure TPLCString.WriteDirect;
var
  values:TArrayOfDouble;
begin
  values:=StringToArrayOfValues(PValue);
  Write(values,PSize,0);
  SetLength(values,0);
end;

class function TPLCString.ConvertRawByteStringToUTF8(aInput: RawByteString;
  aInputEncoding: TStringEncodings): UTF8String;
var
     CP437Str:   CP437AnsiString;
     CP646Str:   CP646AnsiString;
     CP850Str:   CP850AnsiString;
     CP852Str:   CP852AnsiString;
     CP856Str:   CP856AnsiString;
     CP866Str:   CP866AnsiString;
     CP874Str:   CP874AnsiString;
    CP1250Str:  CP1250AnsiString;
    CP1251Str:  CP1251AnsiString;
    CP1252Str:  CP1252AnsiString;
    CP1253Str:  CP1253AnsiString;
    CP1254Str:  CP1254AnsiString;
    CP1255Str:  CP1255AnsiString;
    CP1256Str:  CP1256AnsiString;
    CP1257Str:  CP1257AnsiString;
    CP1258Str:  CP1258AnsiString;
  CP8859_1Str:CP8859_1AnsiString;
  CP8859_2Str:CP8859_2AnsiString;
  CP8859_5Str:CP8859_5AnsiString;
  c: Integer;

begin
  case aInputEncoding of
    UTF_8:    begin                      Result :=aInput;                  end;
    CP437:    begin    CP437Str:=''; for c:=1 to Length(aInput) do begin    CP437Str:=   CP437Str + aInput[c]; end; Result:=   CP437Str; end;
    CP646:    begin    CP646Str:=''; for c:=1 to Length(aInput) do begin    CP646Str:=   CP646Str + aInput[c]; end; Result:=   CP646Str; end;
    CP850:    begin    CP850Str:=''; for c:=1 to Length(aInput) do begin    CP850Str:=   CP850Str + aInput[c]; end; Result:=   CP850Str; end;
    CP852:    begin    CP852Str:=''; for c:=1 to Length(aInput) do begin    CP852Str:=   CP852Str + aInput[c]; end; Result:=   CP852Str; end;
    CP856:    begin    CP856Str:=''; for c:=1 to Length(aInput) do begin    CP856Str:=   CP856Str + aInput[c]; end; Result:=   CP856Str; end;
    CP866:    begin    CP866Str:=''; for c:=1 to Length(aInput) do begin    CP866Str:=   CP866Str + aInput[c]; end; Result:=   CP866Str; end;
    CP874:    begin    CP874Str:=''; for c:=1 to Length(aInput) do begin    CP874Str:=   CP874Str + aInput[c]; end; Result:=   CP874Str; end;
    CP1250:   begin   CP1250Str:=''; for c:=1 to Length(aInput) do begin   CP1250Str:=  CP1250Str + aInput[c]; end; Result:=  CP1250Str; end;
    CP1251:   begin   CP1251Str:=''; for c:=1 to Length(aInput) do begin   CP1251Str:=  CP1251Str + aInput[c]; end; Result:=  CP1251Str; end;
    CP1252:   begin   CP1252Str:=''; for c:=1 to Length(aInput) do begin   CP1252Str:=  CP1252Str + aInput[c]; end; Result:=  CP1252Str; end;
    CP1253:   begin   CP1253Str:=''; for c:=1 to Length(aInput) do begin   CP1253Str:=  CP1253Str + aInput[c]; end; Result:=  CP1253Str; end;
    CP1254:   begin   CP1254Str:=''; for c:=1 to Length(aInput) do begin   CP1254Str:=  CP1254Str + aInput[c]; end; Result:=  CP1254Str; end;
    CP1255:   begin   CP1255Str:=''; for c:=1 to Length(aInput) do begin   CP1255Str:=  CP1255Str + aInput[c]; end; Result:=  CP1255Str; end;
    CP1256:   begin   CP1256Str:=''; for c:=1 to Length(aInput) do begin   CP1256Str:=  CP1256Str + aInput[c]; end; Result:=  CP1256Str; end;
    CP1257:   begin   CP1257Str:=''; for c:=1 to Length(aInput) do begin   CP1257Str:=  CP1257Str + aInput[c]; end; Result:=  CP1257Str; end;
    CP1258:   begin   CP1258Str:=''; for c:=1 to Length(aInput) do begin   CP1258Str:=  CP1258Str + aInput[c]; end; Result:=  CP1258Str; end;
    CP8859_1: begin CP8859_1Str:=''; for c:=1 to Length(aInput) do begin CP8859_1Str:=CP8859_1Str + aInput[c]; end; Result:=CP8859_1Str; end;
    CP8859_2: begin CP8859_2Str:=''; for c:=1 to Length(aInput) do begin CP8859_2Str:=CP8859_2Str + aInput[c]; end; Result:=CP8859_2Str; end;
    CP8859_5: begin CP8859_5Str:=''; for c:=1 to Length(aInput) do begin CP8859_5Str:=CP8859_5Str + aInput[c]; end; Result:=CP8859_5Str; end;
  end;
end;

class function TPLCString.ConvertUTF8CharToByte(aInput: UTF8String;
  aInputEncoding: TStringEncodings; aPos: Integer): Byte;
var
     CP437Str:   CP437AnsiString;
     CP646Str:   CP646AnsiString;
     CP850Str:   CP850AnsiString;
     CP852Str:   CP852AnsiString;
     CP856Str:   CP856AnsiString;
     CP866Str:   CP866AnsiString;
     CP874Str:   CP874AnsiString;
    CP1250Str:  CP1250AnsiString;
    CP1251Str:  CP1251AnsiString;
    CP1252Str:  CP1252AnsiString;
    CP1253Str:  CP1253AnsiString;
    CP1254Str:  CP1254AnsiString;
    CP1255Str:  CP1255AnsiString;
    CP1256Str:  CP1256AnsiString;
    CP1257Str:  CP1257AnsiString;
    CP1258Str:  CP1258AnsiString;
  CP8859_1Str:CP8859_1AnsiString;
  CP8859_2Str:CP8859_2AnsiString;
  CP8859_5Str:CP8859_5AnsiString;
begin
  case aInputEncoding of
    UTF_8:    begin                      Result := byte(     aInput[aPos]); end;
    CP437:    begin    CP437Str:=aInput; Result := byte(   CP437Str[aPos]); end;
    CP646:    begin    CP646Str:=aInput; Result := byte(   CP646Str[aPos]); end;
    CP850:    begin    CP850Str:=aInput; Result := byte(   CP850Str[aPos]); end;
    CP852:    begin    CP852Str:=aInput; Result := byte(   CP852Str[aPos]); end;
    CP856:    begin    CP856Str:=aInput; Result := byte(   CP856Str[aPos]); end;
    CP866:    begin    CP866Str:=aInput; Result := byte(   CP866Str[aPos]); end;
    CP874:    begin    CP874Str:=aInput; Result := byte(   CP874Str[aPos]); end;
    CP1250:   begin   CP1250Str:=aInput; Result := byte(  CP1250Str[aPos]); end;
    CP1251:   begin   CP1251Str:=aInput; Result := byte(  CP1251Str[aPos]); end;
    CP1252:   begin   CP1252Str:=aInput; Result := byte(  CP1252Str[aPos]); end;
    CP1253:   begin   CP1253Str:=aInput; Result := byte(  CP1253Str[aPos]); end;
    CP1254:   begin   CP1254Str:=aInput; Result := byte(  CP1254Str[aPos]); end;
    CP1255:   begin   CP1255Str:=aInput; Result := byte(  CP1255Str[aPos]); end;
    CP1256:   begin   CP1256Str:=aInput; Result := byte(  CP1256Str[aPos]); end;
    CP1257:   begin   CP1257Str:=aInput; Result := byte(  CP1257Str[aPos]); end;
    CP1258:   begin   CP1258Str:=aInput; Result := byte(  CP1258Str[aPos]); end;
    CP8859_1: begin CP8859_1Str:=aInput; Result := byte(CP8859_1Str[aPos]); end;
    CP8859_2: begin CP8859_2Str:=aInput; Result := byte(CP8859_2Str[aPos]); end;
    CP8859_5: begin CP8859_5Str:=aInput; Result := byte(CP8859_5Str[aPos]); end;
  end;
end;

//codifica uma array de valores em uma string
//
//encodes a string from a array of double.
function TPLCString.ArrayOfValuesToString(values: TArrayOfDouble): UTF8String;
var
  aux1, maxbits, bit:LongInt;
  ValueAux2, ValueP, ByteP, ValueBitP, ByteBitP:LongInt;
  ValueAux, strLen, BitsByType :Byte;
  AResult:RawByteString;
begin
  //use a funcao de leitura
  //what's the current register size in bits
  if PProtocolDriver<>nil then
    BitsByType := Min(PProtocolDriver.SizeOfTag(Self, false, FProtocolTagType),64)
  else
    BitsByType := 8;

  Result := '';

  if Length(values)<=0 then begin
    exit;
  end;

  AResult:= '';

  case PStringType of
     //string formato SIEMENS de 7 ou 8 bits
     //decodes a siemens string.
     stSIEMENS:
     begin
       maxbits := length(values)*BitsByType;
       bit := 0;
       ByteP := 0;
       ByteBitP := 0;
       ValueP := 0;
       ValueBitP := 0;
       ValueAux := 0;
       strlen := 255;
       ValueAux2 := Trunc(values[ValueP]);
       //passa bit a bit para montar a string
       //build the string, bit by bit
       try
         while bit<maxbits do begin
           aux1 := Power(2,ValueBitP);
           if ((ValueAux2 and aux1)=aux1) then
             ValueAux := ValueAux + Power(2,ByteBitP);

           inc(bit);
           inc(ByteBitP);
           inc(ValueBitP);

           //incrementa os ponteiros
           //increment pointers
           if ByteBitP>=PByteSize then begin
             //se esta nos primeiros 2 bytes
             //acha o tamanho real da string
             //(o menor dos dois primeiros bytes)
             //
             //if looking at the first two bytes
             //gets the real size of the string
             if ByteP<2 then begin
               strlen := min(strlen,ValueAux);
             end else begin
               AResult := AResult + Char(ValueAux);
               //se alcançou o tamanho da string.
               //if all string is decoded, finish.
               if Length(AResult)>=strlen then
                 exit;
             end;
             inc(ByteP);
             ByteBitP := 0;
             ValueAux := 0;
           end;
           if ValueBitP>=BitsByType then begin
             ValueBitP := 0;
             Inc(ValueP);
             if ValueP>High(values) then exit;
             ValueAux2 := Trunc(values[ValueP]);
           end;
         end;
         if ByteBitP<=PByteSize then
           AResult := AResult + Char(ValueAux);
       finally
         Result:=ConvertRawByteStringToUTF8(AResult,FStringEncoding);
       end;
     end;


      //string formato C de 7 ou 8 bits
      //C string, 7 or 8 bits per character.
     stC:
     begin
       maxbits := length(values)*BitsByType;
       bit := 0;
       //ByteP := 1;
       ByteBitP := 0;
       ValueP := 0;
       ValueBitP := 0;
       ValueAux := 0;
       ValueAux2 := Trunc(values[ValueP]);
       //passa bit a bit para montar a string
       //build the string, bit by bit
       try
         while bit<maxbits do begin
           aux1 := Power(2,ValueBitP);
           if ((ValueAux2 and aux1)=aux1) then
             ValueAux := ValueAux + Power(2,ByteBitP);

           inc(bit);
           inc(ByteBitP);
           inc(ValueBitP);

           //incrementa os ponteiros
           //increment the pointers
           if ByteBitP>=PByteSize then begin
             //se encontrou um byte ZERO (fim de string)
             //para de processar os valores.
             //
             //if found the terminator, finish the string.
             if ValueAux=0 then
               exit
             else
               AResult := AResult + Char(ValueAux);
             //inc(ByteP);
             ByteBitP := 0;
             ValueAux := 0;
           end;
           if ValueBitP>=BitsByType then begin
             ValueBitP := 0;
             Inc(ValueP);
             if ValueP>High(values) then exit;
             ValueAux2 := Trunc(values[ValueP]);
           end;
         end;
         if ByteBitP<=PByteSize then begin
           AResult := AResult + Char(ValueAux);
         end;
       finally
         Result:=ConvertRawByteStringToUTF8(AResult,FStringEncoding);
       end;
     end;
     else
       Result := ''; //unknown string type...
  end;
end;

//codifica uma uma string em array de valores
//encodes a string to a array of double.
function TPLCString.StringToArrayOfValues(value: UTF8String): TArrayOfDouble;
var
  ValueAux, aux1, maxbits, bit, bs:LongInt;
  ValueP, ByteP, ValueBitP, ByteBitP:LongInt;
  MaxLen, strLen, BitsByType :Byte;
begin
  if PProtocolDriver<>nil then
    BitsByType := Min(PProtocolDriver.SizeOfTag(Self,true,FProtocolTagType),32)
  else
    BitsByType := 8;


  //usa a WriteFunction
  //use the writefunction to determine the block size.
  bs := CalcBlockSize(true);
  SetLength(Result,bs);

  case PStringType of
    //formato de String SIEMENS de 7 ou  8 bits
    //encodes a SIEMENS string, 7 or 8 bits of length
    stSIEMENS:
    begin
      MaxLen := Min(PStringSize, power(2,PByteSize)-1);
      strlen := Min(MaxLen,ifthen(FStringEncoding=UTF_8, Length(value), UTF8Length(value)));
      maxbits := PByteSize * (strlen+2);
      bit := 0;
      ByteBitP := 0;
      ByteP := 1;
      ValueBitP := 0;
      ValueP := 0;
      ValueAux := 0;

      while bit<maxbits do begin
        //processa os dois primeiros bytes do formato siemens
        //que dizem o tamanho da string;
        //
        //stores in the first two bytes, the length of string.
        if bit<(2*PByteSize) then begin
          aux1 := Power(2,ByteBitP);
          if bit<PByteSize then begin
            if (MaxLen and aux1)=aux1 then
              ValueAux := ValueAux + Power(2,ValueBitP);
          end else begin
            if (strLen and aux1)=aux1 then
              ValueAux := ValueAux + Power(2,ValueBitP);
          end;
        end else begin
          if bit=(2*PByteSize) then begin
            ByteBitP := 0;
            ByteP := 1;
          end;
          //processa os bytes da string
          //processes the bytes of string.
          aux1 := Power(2,ByteBitP);
          if (ConvertUTF8CharToByte(value,FStringEncoding, ByteP) and aux1)=aux1 then begin
            ValueAux := ValueAux + Power(2,ValueBitP);
          end;
        end;


        inc(bit);
        inc(ByteBitP);
        inc(ValueBitP);

        //incrementa os ponteiros
        //increment the pointes.
        if ByteBitP>=PByteSize then begin
          inc(ByteP);
          ByteBitP := 0;
        end;
        if ValueBitP>=BitsByType then begin
          Result[ValueP] := ValueAux;
          ValueAux :=0;
          ValueBitP := 0;
          Inc(ValueP);
        end;
      end;
      if (ValueP<bs) and (ValueBitP<BitsByType) then begin
        Result[ValueP] := ValueAux;
      end;
    end;
    //formato de String C de 7 ou  8 bits
    //C string format, 7 or 8 bits.
    stC:
    begin
      strlen := Min(Length(value), power(2,PByteSize)-1);
      maxbits := PByteSize * (strlen);
      bit := 0;
      ByteBitP := 0;
      ByteP := 1;
      ValueBitP := 0;
      ValueP := 0;
      ValueAux := 0;
      while bit<maxbits do begin
        //processa os bytes da string
        //processes the bytes of string.
        aux1 := Power(2,ByteBitP);
        if (ConvertUTF8CharToByte(value, FStringEncoding, ByteP) and aux1)=aux1 then
          ValueAux := ValueAux + Power(2,ValueBitP);

        inc(bit);
        inc(ByteBitP);
        inc(ValueBitP);

        //incrementa os ponteiros
        //increment the pointers.
        if ByteBitP>=PByteSize then begin
          inc(ByteP);
          ByteBitP := 0;
        end;
        if ValueBitP>=BitsByType then begin
          Result[ValueP] := ValueAux;
          ValueAux :=0;
          ValueBitP := 0;
          Inc(ValueP);
        end;
      end;
      if (ValueP<bs)and (ValueBitP<BitsByType) then begin
        Result[ValueP] := ValueAux;
      end;
    end;
  end;

end;

function TPLCString.GetValueAsText(Prefix, Sufix, Format:UTF8String; FormatDateTimeOptions:TFormatDateTimeOptions=[]):UTF8String;
begin
   Result := Prefix + Value + Sufix;
end;

function TPLCString.IsMyCallBack(Cback: TTagCommandCallBack): Boolean;
begin
  Result:=inherited IsMyCallBack(Cback) and (TMethod(Cback).Code=Pointer(@TPLCString.TagCommandCallBack));
end;

procedure TPLCString.SetPLCHack(v:Cardinal);
begin
  inherited SetPLCHack(v);
  SetBlockSize(CalcBlockSize(false));
end;

procedure TPLCString.SetPLCSlot(v:Cardinal);
begin
  inherited SetPLCSlot(v);
  SetBlockSize(CalcBlockSize(false));
end;

procedure TPLCString.SetPLCStation(v:Cardinal);
begin
  inherited SetPLCStation(v);
  SetBlockSize(CalcBlockSize(false));
end;

procedure TPLCString.SetMemFileDB(v:Cardinal);
begin
  inherited SetMemFileDB(v);
  SetBlockSize(CalcBlockSize(false));
end;

procedure TPLCString.SetMemAddress(v:Cardinal);
begin
  inherited SetMemAddress(v);
  SetBlockSize(CalcBlockSize(false));
end;

procedure TPLCString.SetMemSubElement(v:Cardinal);
begin
  inherited SetMemSubElement(v);
  SetBlockSize(CalcBlockSize(false));
end;

procedure TPLCString.SetMemReadFunction(v:Cardinal);
begin
  inherited SetMemReadFunction(v);
  SetBlockSize(CalcBlockSize(false));
end;

procedure TPLCString.SetMemWriteFunction(v:Cardinal);
begin
  inherited SetMemWriteFunction(v);
  SetBlockSize(CalcBlockSize(false));
end;

procedure TPLCString.SetPath(v:AnsiString);
begin
  inherited SetPath(v);
  SetBlockSize(CalcBlockSize(false));
end;

procedure TPLCString.SetProtocolDriver(p:TProtocolDriver);
begin
  inherited SetProtocolDriver(p);
  SetBlockSize(CalcBlockSize(false));
end;

procedure TPLCString.TagCommandCallBack(const ReqID: LongWord;
  Values: TArrayOfDouble; ValuesTimeStamp: TDateTime; TagCommand: TTagCommand;
  LastResult: TProtocolIOResult; Offset: LongInt);
var
  c:LongInt;
  notify:Boolean;
begin
  if (csDestroying in ComponentState) then exit;
  try
    notify := false;
    case TagCommand of
      tcScanRead, tcRead, tcInternalUpdate:
      begin
        if LastResult in [ioOk, ioNullDriver] then begin
          for c := 0 to Length(Values) - 1 do begin
            notify := notify or (PValues[c+Offset]<>values[c]);
            PValues[c+Offset]:=values[c];
          end;
          PValueTimeStamp := ValuesTimeStamp;
          if (TagCommand<>tcInternalUpdate) AND (LastResult=ioOk) then
           IncCommReadOK(1);
        end else begin
          if (TagCommand<>tcInternalUpdate) then begin
            IncCommReadFaults(1);
          end;
        end;
      end;
      tcScanWrite,tcWrite:
      begin
        if LastResult in [ioOk, ioNullDriver] then begin
          if LastResult=ioOk then
             IncCommWriteOK(1);
          for c := 0 to Length(Values) - 1 do begin
            notify := notify or (PValues[c+Offset]<>values[c]);
            PValues[c+Offset]:=values[c]
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
      if TagCommand in [tcRead,tcScanRead] then PFirstUpdate:=false;
      PValue := ArrayOfValuesToString(PValues);
      NotifyChange;
    end;
  finally
  end;
end;

procedure TPLCString.SetBlockSize(asize:Cardinal);
begin
  if size>0 then begin
    PSize := asize;
    SetLength(PValues, PSize);
    if PProtocolDriver<>nil then begin
      if PAutoRead then begin
        PProtocolDriver.RemoveTag(Self);
        PProtocolDriver.AddTag(Self);
      end;
    end;
  end;
end;

procedure TPLCString.SetStringEncoding(AValue: TStringEncodings);
begin
  if FStringEncoding=AValue then Exit;
  FStringEncoding:=AValue;
end;

procedure TPLCString.SetStringSize(asize:Cardinal);
begin
   if (PByteSize=8) and (size>255) or ((PByteSize=7) and (asize>127)) or ((PStringType=stROCKWELL) and (asize>84)) then
     raise Exception.Create(SstringSizeOutOfBounds);
   PStringSize := asize;
   SetBlockSize(CalcBlockSize(false));
end;

procedure TPLCString.SetByteSize(bsize:Byte);
begin
   if (bsize<7) or (bsize>8) then
     raise Exception.Create(SsizeMustBe7or8);

   if (bsize=7) and (PStringSize>127) then PStringSize:=127;
   if (bsize=8) and (PStringSize>255) then PStringSize:=255;

   PByteSize := bsize;
   SetBlockSize(CalcBlockSize(false));
end;

procedure TPLCString.SetStringType(stype:TPLCStringTypes);
begin
  if stype=PStringType then exit;
  PStringType := stype;
  SetBlockSize(CalcBlockSize(false));
  PValue := ArrayOfValuesToString(PValues);
end;

procedure TPLCString.SetDummySize(s:Cardinal);
begin

end;

function  TPLCString.GetValue:UTF8String;
begin
  Result := PValue;
end;

procedure TPLCString.SetValue(Value:UTF8String);
var
  x:TArrayOfDouble;
begin
  x:=StringToArrayOfValues(Value);
  try
    if FSyncWrites then
      Write(x,Length(x),0)
    else
      ScanWrite(x,Length(x),0);
  finally
    SetLength(x,0);
  end;
end;

function TPLCString.CalcBlockSize(IsWrite:Boolean):Cardinal;
var
  BitsByType:Byte;
  strlen:LongInt;
begin
  if PProtocolDriver<>nil then begin
    BitsByType := PProtocolDriver.SizeOfTag(Self,IsWrite,FProtocolTagType);
    BitsByType := IfThen(BitsByType=0,1,BitsByType);
  end else
    BitsByType := 8;

  //calcula o tamanho da string conforme o tipo
  //calculate the string size depending of the format.
  case PStringType of
    stSIEMENS:
      strlen := (PStringSize + 2)*PByteSize;
    stC:
      strlen := (PStringSize + 1)*PByteSize;
    else
      strlen := 1;
  end;

  Result := strlen div BitsByType + IfThen((strlen mod BitsByType)=0,0,1);
end;

function  TPLCString.GetVariantValue:Variant;
begin
   Result := Value;
end;

procedure TPLCString.SetVariantValue(V:Variant);
begin
   Value := V
end;

function  TPLCString.IsValidValue(Value:Variant):Boolean;
begin
  Result := VarIsNumeric(Value) or VarIsStr(Value) or
            VarIsType(Value,vardate) or VarIsType(Value,varboolean);
end;

function  TPLCString.GetValueTimestamp:TDatetime;
begin
   Result := PValueTimeStamp;
end;

procedure TPLCString.AsyncNotifyChange(data:Pointer);
var
  x:PString;
begin
  if Assigned(POnAsyncStringValueChange) then begin
    x:=data;
    POnAsyncStringValueChange(self,x^);
  end;
end;

function TPLCString.GetValueChangeData: Pointer;
var
  x:PString;
begin
  New(x);
  x^:=Value;
  Result:=x;
end;

procedure TPLCString.ReleaseChangeData(data: Pointer);
var
  x:PString;
begin
  x:=data;
  SetLength(x^,0);
  Dispose(x);
end;

end.
