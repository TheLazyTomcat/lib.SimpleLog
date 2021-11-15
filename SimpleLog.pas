unit SimpleLog;

interface

uses
  SysUtils, Classes,
  AuxTypes, AuxClasses;

type
  TSLLogOutput = (loInternal,loStream,loFile,loConsole,loExternals);

  TSLLogOutputs = set of TSLLogOutput;

  TSLSettings = record
    LogOutputs:     TSLLogOutputs;
    FormatSettings: TFormatSettings;
    TimeFormat:     String;
    TimeSeparator:  String;
    IndentNewLines: Boolean;
  end;

  TSLExternalLog = record
    LogObject:  TStrings;
    Active:     Boolean;
    Owned:      Boolean;
  end;

type
  TSimpleLog = class(TCustomListObject)
  protected
    fSettings:          TSLSettings;
    fTimeOfCreation:    TDateTime;
    fLogCounter:        UInt32;

    fInternalLog:       TStringList;
    fStreamLog:         TStream;
    fFileLog:           String;
    fFileLogStream:     TFileStream;  // only internal, do not publish
    // console stuff
    fExternalLogs:      array of TStrings;
    fExternalLogCount:  Integer;

    fOnLogEvent:        TNotifyEvent;
    fOnLogCallback:     TNotifyCallback;

    //Function GetExternalLog(Index: Integer): TStrings; virtual;

    //Function GetCapacity: Integer; override;
    //procedure SetCapacity(Value: Integer); override;
    //Function GetCount: Integer; override;
    //procedure SetCount(Value: Integer); override;

    //procedure Initialize; virtual;
    //procedure Finalize; virtual;
  public
    //constructor Create;
    //destructor Destroy; override;

    // output setup
    //Function ActiveOutput(Output: TSLLogOutput): Boolean; virtual;
    //Function ActivateOutput(Output: TSLLogOutput): Boolean; virtual;
    //Function DeactivateOutput(Output: TSLLogOutput): Boolean; virtual;
    //procedure SetupOutputToStream(Stream: TStream; Append: Boolean; Rewrite: Boolean = False; Activate: Boolean = True); virtual;
    //procedure SetupOutputToFile(const FileName: String; Append: Boolean; Activate: Boolean = True); virtual;
    //Function SetupOutputToConsole(Activate: Boolean = True): Boolean; virtual;

    // external logs
    //Function LowIndex: Integer; override;
    //Function HighIndex: Integer; override;
    //Function ExternalLogLowIndex: Integer; virtual;
    //Function ExternalLogHighIndex: Integer; virtual;

    //Function ExternalIndexOf(LogObject: TStrings): Integer; virtual;
    //Function ExternalLogAdd(LogObject: TStrings; Activate: Boolean = True; Owned: Boolean = False): Integer; virtual;
    //Function ExternalLogInsert(Index: Integer; LogObject: TStrings; Activate: Boolean = True; Owned: Boolean = False): Integer; virtual;
    //Function ExternalLogExtract(LogObject: TStrings): TStrings; virtual;
    //Function ExternalLogRemove(LogObject: TStrings): Integer; virtual;
    //procedure ExternalLogDelete(Index: Integer); virtual;

    //Function ExternalLogActive(Index: Integer): Boolean; virtual;
    //Function ExternalLogSetActive(Index: Integer; Active: Boolean): Boolean; virtual;
    //Function ExternalLogOwned(Index: Integer): Boolean; virtual;
    //Function ExternalLogSetOwned(Index: Integer; Owned: Boolean): Boolean; virtual;

    // logging methods

    // console listening

    // properties
    // settings
    property Settings: TSLSettings read fSettings write fSettings;
    // to (de)activate individual log outputs, use methods ActivateOutput and DeactivateOutput    
    property LogOutputs: TSLLogOutputs read fSettings.LogOutputs write fSettings.LogOutputs;
    property FormatSettings: TFormatSettings read fSettings.FormatSettings write fSettings.FormatSettings;
    property TimeFormat: String read fSettings.TimeFormat write fSettings.TimeFormat;
    property TimeSeparator: String read fSettings.TimeSeparator write fSettings.TimeSeparator;
    property IndentNewLines: Boolean read fSettings.IndentNewLines write fSettings.IndentNewLines;

    // informative properties
    property TimeOfCreation: TDateTime read fTimeOfCreation;
    property LogCounter: UInt32 read fLogCounter;

    // outputs
    property InternalLog: TStringList read fInternalLog write fInternalLog;
    property StreamLog: TStream read fStreamLog;
    property FileLog: String read fFileLog;
    //property ConsoleLog: Boolean
    //property ExternalLogs[Index: Integer]: TStrings read GetExternalLog; default;
    //property ExternalLogCount: Integer read GetCount;

    // events/callbacks
    property OnLogEvent: TNotifyEvent read fOnLogEvent write fOnLogEvent;
    property OnLogCallback: TNotifyCallback read fOnLogCallback write fOnLogCallback;
    property OnLog: TNotifyEvent read fOnLogEvent write fOnLogEvent;
  end;

implementation

end.
