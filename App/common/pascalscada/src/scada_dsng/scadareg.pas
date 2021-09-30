{$i ../common/language.inc}
{$I ../common/delphiver.inc}
{$IFDEF PORTUGUES}
{:
  @abstract(Unit de registro de componentes do PascalSCADA. Para Lazarus e Delphi.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
  @abstract(Unit of register of PascalSCADA components. For Lazarus and Delphi.)
  @author(Fabio Luis Girardi <fabio@pascalscada.com>)

  ****************************** History  *******************************
  ***********************************************************************
  07/2013 - Modified. Register new assistant components
  @author(Juanjo Montero <juanjo.montero@gmail.com>)
  ***********************************************************************
}
{$ENDIF}
unit scadareg;

interface

procedure Register;

implementation

uses
  Classes, SerialPort, ModBusSerial, LinearScaleProcessor, PLCTagNumber,
  PLCBlock, PLCBlockElement, PLCString, PLCNumber, UserScale, ValueProcessor,
  scadapropeditor, hsstrings, TagBit, ProtocolDriver, WestASCIIDriver,
  IBoxDriver, tcp_udpport, ModBusTCP, PLCStruct, PLCStructElement, ISOTCPDriver,
  mutexserver, MutexClient, siemenstagassistant, modbustagassistant, MelsecTCP,
  westasciitagassistant, bitmappertagassistant, blockstructtagassistant,
  numexprtag, plcstructstring, redundancyctrl,
  {$IFDEF FPC}
    LResources, PropEdits, ComponentEditors, IDECommands, MenuIntf, LCLType;
  {$ELSE}
    {$IFDEF PORTUGUES}
      {$MESSAGE ERROR 'Somente Lazarus/Freepascal é suportado!'}
    {$ELSE}
      {$MESSAGE ERROR 'Only Lazarus/Freepascal are supported!'}
    {$ENDIF}
  {$ENDIF}
procedure Register;
var
  Cat: TIDECommandCategory;
  CmdMyTool: TIDECommand;
begin
  RegisterComponents(strPortsPallete,     [TSerialPortDriver,
                                           TTCP_UDPPort]);
  RegisterComponents(strProtocolsPallete, [TModBusRTUDriver,
                                           TModBusTCPDriver,
                                           TWestASCIIDriver,
                                           TIBoxDriver,
                                           TISOTCPDriver,
                                           TMelsecTCPDriver]);
  RegisterComponents(strUtilsPallete,     [TScalesQueue,
                                           TLinearScaleProcessor,
                                           TUserScale,
                                           TMutexServer,
                                           TMutexClient,
                                           TRedundancyCtrl]);
  RegisterComponents(strTagsPallete,      [TPLCTagNumber,
                                           TPLCBlock,
                                           TPLCBlockElement,
                                           TPLCString,
                                           TTagBit,
                                           TPLCStruct,
                                           TPLCStructItem,
                                           TNumericExprTag,
                                           TPLCStructString]);

  RegisterPropertyEditor(TypeInfo(AnsiString), TSerialPortDriver, 'COMPort', TPortPropertyEditor);
  {$IF defined(WIN32) or defined(WIN64)}
  RegisterPropertyEditor(TypeInfo(AnsiString), TSerialPortDriver, 'DevDir', THiddenPropertyEditor);
  {$IFEND}
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCBlockElement,  'Index'  , TElementIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCStructItem,    'Index'  , TElementIndexPropertyEditor);

  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCTagNumber, 'PLCRack'         , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCTagNumber, 'PLCSlot'         , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCTagNumber, 'PLCStation'      , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCTagNumber, 'MemFile_DB'      , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCTagNumber, 'MemAddress'      , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCTagNumber, 'MemSubElement'   , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCTagNumber, 'MemReadFunction' , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCTagNumber, 'MemWriteFunction', TTagAddressPropertyEditor);

  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCBlock,     'PLCRack'         , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCBlock,     'PLCSlot'         , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCBlock,     'PLCStation'      , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCBlock,     'MemFile_DB'      , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCBlock,     'MemAddress'      , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCBlock,     'MemSubElement'   , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCBlock,     'MemReadFunction' , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCBlock,     'MemWriteFunction', TTagAddressPropertyEditor);

  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCStruct,    'PLCRack'         , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCStruct,    'PLCSlot'         , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCStruct,    'PLCStation'      , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCStruct,    'MemFile_DB'      , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCStruct,    'MemAddress'      , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCStruct,    'MemSubElement'   , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCStruct,    'MemReadFunction' , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCStruct,    'MemWriteFunction', TTagAddressPropertyEditor);

  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCString,    'PLCRack'         , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCString,    'PLCSlot'         , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCString,    'PLCStation'      , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCString,    'MemFile_DB'      , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCString,    'MemAddress'      , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCString,    'MemSubElement'   , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCString,    'MemReadFunction' , TTagAddressPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Cardinal),   TPLCString,    'MemWriteFunction', TTagAddressPropertyEditor);

  RegisterComponentEditor(TProtocolDriver,    TProtocolDriverComponentEditor);
  RegisterComponentEditor(TPLCNumberMappable, TTagBitMapperComponentEditor);
  RegisterComponentEditor(TPLCBlock,          TBlockElementMapperComponentEditor);

  Cat:=IDECommandList.FindCategoryByName(CommandCategoryToolMenuName);
  CmdMyTool := RegisterIDECommand(Cat,'Change component tag', 'Change the Tag property of a TComponent', VK_F2, [ssShift], nil, @ChangeComponentTag);
  RegisterIDEMenuCommand(itmSecondaryTools, 'EditTag', 'Change component tag', nil, nil, CmdMyTool);

end;

initialization

{$I pascalscada.lrs}

end.

