unit SimpleLog;

{$IF Defined(WINDOWS) or Defined(MSWINDOWS)}
  {$DEFINE Windows}
{$ELSEIF Defined(LINUX) and Defined(FPC)}
  {$DEFINE Linux}
{$ELSE}
  {$MESSAGE FATAL 'Unsupported operating system.'}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
{$ENDIF}
{$H+}

{$IFOPT Q+}
  {$DEFINE OverflowChecks}
{$ENDIF}

interface

uses
  SysUtils, Classes,
  AuxTypes, AuxClasses;

type
  ESLException = class(Exception);

  ESLIndexOutOfBounds = class(ESLException);
  ESLInvalidValue     = class(ESLException);

type
  TSLLogOutput = (loInternal,loStream,loFile,loConsole,loExternals);

  TSLLogOutputs = set of TSLLogOutput;

  TSLSettings = record
    LogOutputs:         TSLLogOutputs;
    FormatSettings:     TFormatSettings;
    TimeFormat:         String;
    TimeSeparator:      String;
    ForceTime:          Boolean;
    ForceTimeAutoreset: Boolean;    
    ForcedTime:         TDateTime;
    IndentLines:        Boolean;
  end;

  TSLStrings = record
    BreakerCharThin:  Char;
    BreakerCharThick: Char;
    TimeStamp:        String;
    StartStamp:       String;
    EndStamp:         String;
    AppendStamp:      String;
    HeaderText:       String;
    LineLength:       Integer;
  end;

{
  TSLExternalLogItem is for internal use only.
}
  TSLExternalLogItem = record
    LogObject:  TStrings;
    Active:     Boolean;
    Owned:      Boolean;
  end;

type
  TSimpleLog = class(TCustomListObject)
  protected
    // settings and info fields
    fSettings:            TSLSettings;
    fStrings:             TSLStrings;
    fTimeOfCreation:      TDateTime;
    fLogCounter:          UInt32;
    // log output fields
    fInternalLog:         TStringList;
    fStreamLog:           TStream;
    fFileLog:             String;
    fFileLogStream:       TFileStream;  // only internal, do not publish
    fConsolePresent:      Boolean;     
    fExternalLogs:        array of TSLExternalLogItem;
    fExternalLogCount:    Integer;
    // console binding fields
    fConsoleBinded:       Boolean;
    // event/callback properties
    fOnLogEvent:          TStringEvent;
    fOnLogCallback:       TStringCallback;
    // getters, setters
    Function GetExternalLog(Index: Integer): TStrings; virtual;
    Function GetCapacity: Integer; override;
    procedure SetCapacity(Value: Integer); override;
    Function GetCount: Integer; override;
    procedure SetCount(Value: Integer); override;
    // init/final
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    // internal logging methods
    Function GetTime: TDateTime; virtual;
    Function GetTimeString(Time: TDateTime): String; virtual;
    Function GetIndentedString(const Str: String; IndentCount: Integer): String; virtual;
    Function GetStampStr(const StampText: String; ThickBreak: Boolean): String;
    procedure WriteLogToOutputs(const LogText: String; LineBreakInStreams: Boolean); virtual;
    procedure ProcessConsoleLog(const LogText: String); virtual;
    procedure ProcessLocalLog(const LogText: String; IndentCount: Integer = 0); virtual;
    // events
    procedure DoOnLog(const LogText: String); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    // output setup methods
    Function ActiveOutput(Output: TSLLogOutput): Boolean; virtual;
    Function ActivateOutput(Output: TSLLogOutput): Boolean; virtual;
    Function DeactivateOutput(Output: TSLLogOutput): Boolean; virtual;
    procedure SetupOutputToStream(Stream: TStream; Append: Boolean; Activate: Boolean = True); virtual;
    procedure SetupOutputToFile(const FileName: String; Append: Boolean; Activate: Boolean = True); virtual; 
    // external logs list methods
    Function LowIndex: Integer; override;
    Function HighIndex: Integer; override;
    Function ExternalLogLowIndex: Integer; virtual;
    Function ExternalLogHighIndex: Integer; virtual;
    Function ExternalLogIndexOf(LogObject: TStrings): Integer; virtual;
    Function ExternalLogAdd(LogObject: TStrings; Active: Boolean = True; Owned: Boolean = False): Integer; virtual;
    procedure ExternalLogInsert(Index: Integer; LogObject: TStrings; Active: Boolean = True; Owned: Boolean = False); virtual;
    Function ExternalLogExtract(LogObject: TStrings): TStrings; virtual;
    Function ExternalLogRemove(LogObject: TStrings): Integer; virtual;
    procedure ExternalLogDelete(Index: Integer); virtual;
    procedure ExternalLogClear; virtual;
    Function ExternalLogActive(Index: Integer): Boolean; virtual;
    Function ExternalLogSetActive(Index: Integer; Active: Boolean): Boolean; virtual;
    Function ExternalLogOwned(Index: Integer): Boolean; virtual;
    Function ExternalLogSetOwned(Index: Integer; Owned: Boolean): Boolean; virtual; 
    // public logging methods
    Function ForceTimeSet(Time: TDateTime; Autoreset: Boolean = False): Boolean; virtual;
    procedure AddLogNoTime(const LogText: String); virtual;
    procedure AddLogTime(const LogText: String; Time: TDateTime); virtual;
    procedure AddLog(const LogText: String); virtual;
    procedure AddEmpty; virtual;
    procedure AddBreaker; virtual;
    procedure AddBreakerThin; virtual;
    procedure AddBreakerThick; virtual;
    procedure AddTimeStamp; virtual;
    procedure AddStartStamp; virtual;
    procedure AddEndStamp; virtual;
    procedure AddAppendStamp; virtual;
    procedure AddHeader; virtual;
    // console binding

    // settings properties
    property Settings: TSLSettings read fSettings;
    // to (de)activate individual log outputs, use methods ActivateOutput and DeactivateOutput
    property LogOutputs: TSLLogOutputs read fSettings.LogOutputs;
    property FormatSettings: TFormatSettings read fSettings.FormatSettings write fSettings.FormatSettings;
    property TimeFormat: String read fSettings.TimeFormat write fSettings.TimeFormat;
    property TimeSeparator: String read fSettings.TimeSeparator write fSettings.TimeSeparator;
    property ForceTime: Boolean read fSettings.ForceTime write fSettings.ForceTime;
    property ForceTimeAutoreset: Boolean read fSettings.ForceTimeAutoreset write fSettings.ForceTimeAutoreset;    
    property ForcedTime: TDateTime read fSettings.ForcedTime write fSettings.ForcedTime;
    property IndentLines: Boolean read fSettings.IndentLines write fSettings.IndentLines;
    // strings properties
    property Strings: TSLStrings read fStrings;
    property BreakerThin: Char read fStrings.BreakerCharThin write fStrings.BreakerCharThin;
    property BreakerThick: Char read fStrings.BreakerCharThick write fStrings.BreakerCharThick;
    property TimeStamp: String read fStrings.TimeStamp write fStrings.TimeStamp;
    property StartStamp: String read fStrings.StartStamp write fStrings.StartStamp;
    property EndStamp: String read fStrings.EndStamp write fStrings.EndStamp;
    property AppendStamp: String read fStrings.AppendStamp write fStrings.AppendStamp;
    property HeaderText: String read fStrings.HeaderText write fStrings.HeaderText;
    property LineLength: Integer read fStrings.LineLength write fStrings.LineLength;
    // informative properties
    property TimeOfCreation: TDateTime read fTimeOfCreation;
    property LogCounter: UInt32 read fLogCounter;
    // log output properties
    property InternalLog: TStringList read fInternalLog write fInternalLog;
    property StreamLog: TStream read fStreamLog;
    property FileLog: String read fFileLog;
    property ConsolePresent: Boolean read fConsolePresent;
    property ExternalLogs[Index: Integer]: TStrings read GetExternalLog; default;
    property ExternalLogCount: Integer read GetCount;
    property Capacity: Integer read GetCapacity;  // redeclaration to make the property read-only
    property Count: Integer read GetCount;        // -//-
    // events/callbacks properties
    property OnLogEvent: TStringEvent read fOnLogEvent write fOnLogEvent;
    property OnLogCallback: TStringCallback read fOnLogCallback write fOnLogCallback;
    property OnLog: TStringEvent read fOnLogEvent write fOnLogEvent;
  end;

procedure InitFormatSettings(out FormatSettings: TFormatSettings);

implementation

uses
  {$IFDEF Windows}Windows,{$ENDIF}
  StrRect;

procedure InitFormatSettings(out FormatSettings: TFormatSettings);
begin
{$WARN SYMBOL_PLATFORM OFF}
{$IF not Defined(FPC) and (CompilerVersion >= 18)}
// Delphi 2006+
FormatSettings := TFormatSettings.Create(LOCALE_USER_DEFAULT);
{$ELSE}
// older delphi and FPC
{$IFDEF Windows}
// windows
GetLocaleFormatSettings(LOCALE_USER_DEFAULT,FormatSettings);
{$ELSE}
// non-windows
FormatSettings := DefaultFormatSettings;
{$ENDIF}
{$IFEND}
{$WARN SYMBOL_PLATFORM ON}
end;

//==============================================================================

const
  SL_DEFSTR_TIMEFORMAT    = 'yyyy-mm-dd hh:nn:ss.zzz';
  SL_DEFSTR_TIMESEPARATOR = ' //: ';

  SL_DEFSTR_BREAKERCHAR_THIN  = '-';
  SL_DEFSTR_BREAKERCHAR_THICK = '=';

  SL_DEFSTR_LINELENGTH = 80;

  SL_DEFSTR_TIMESTAMP   = '%s';
  SL_DEFSTR_STARTSTAMP  = '%s - Starting log';
  SL_DEFSTR_ENDSTAMP    = '%s - Ending log';
  SL_DEFSTR_APPENDSTAMP = '%s - Appending log';

  SL_DEFSTR_HEADERTEXT = UTF8String('SimpleLog 2.0, '#$C2#$A9'2015-2021 Franti'#$C5#$A1'ek Milt');

//------------------------------------------------------------------------------

Function TSimpleLog.GetExternalLog(Index: Integer): TStrings;
begin
If CheckIndex(Index) then
  Result := fExternalLogs[Index].LogObject
else
  raise ESLIndexOutOfBounds.CreateFmt('TSimpleLog.GetExternalLog: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TSimpleLog.GetCapacity: Integer;
begin
Result := Length(fExternalLogs);
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.SetCapacity(Value: Integer);
var
  i:  Integer;
begin
If Value >= 0 then
  begin
    If Value <> Length(fExternalLogs) then
      begin
        // Removing existing assigned items? If so, free owned objects.
        If Value < Count then
          begin
            For i := Value to HighIndex do
              If fExternalLogs[i].Owned then
                FreeAndNil(fExternalLogs[i].LogObject);
            fExternalLogCount := Value;
          end;
        SetLength(fExternalLogs,Value);
      end;
  end
else raise ESLInvalidValue.CreateFmt('TSimpleLog.SetCapacity: Invalid capacity (%d).',[Value]);
end;

//------------------------------------------------------------------------------

Function TSimpleLog.GetCount: Integer;
begin
Result := fExternalLogCount;
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.SetCount(Value: Integer);
begin
// nothing to do, count is read only
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.Initialize;
begin
// init settings
fSettings.LogOutputs := [loInternal];
InitFormatSettings(fSettings.FormatSettings);
fSettings.TimeFormat := SL_DEFSTR_TIMEFORMAT;
fSettings.TimeSeparator := SL_DEFSTR_TIMESEPARATOR;
fSettings.ForceTime := False;
fSettings.ForceTimeAutoreset := False;
fSettings.ForcedTime := Now;
fSettings.IndentLines := False;
// init strings
fStrings.BreakerCharThin := SL_DEFSTR_BREAKERCHAR_THIN;
fStrings.BreakerCharThick := SL_DEFSTR_BREAKERCHAR_THICK;
fStrings.TimeStamp := SL_DEFSTR_TIMESTAMP;
fStrings.StartStamp := SL_DEFSTR_STARTSTAMP;
fStrings.EndStamp := SL_DEFSTR_ENDSTAMP;
fStrings.AppendStamp := SL_DEFSTR_APPENDSTAMP;
fStrings.HeaderText := UTF8ToStr(SL_DEFSTR_HEADERTEXT);
fStrings.LineLength := 80;
// init other stuff
fTimeOfCreation := Now;
fLogCounter := 0;
fInternalLog := TStringList.Create;
fStreamLog := nil;
fFileLog := '';
fFileLogStream := nil;
fConsolePresent := System.IsConsole;
SetLength(fExternalLogs,0);
fExternalLogCount := 0;
fConsoleBinded := False;
fOnLogEvent := nil;
fOnLogCallback := nil;
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.Finalize;
begin
fOnLogEvent := nil;
fOnLogCallback := nil;
//If fConsoleBinded then ...unbind console
ExternalLogClear;
// destroy internally created objects
If Assigned(fFileLogStream) then
  FreeAndNil(fFileLogStream);
If Assigned(fStreamLog) then
  FreeAndNil(fStreamLog);
FreeAndNil(fInternalLog);
end;

//------------------------------------------------------------------------------

Function TSimpleLog.GetTime: TDateTime;
begin
If fSettings.ForceTime then
  begin
    Result := fSettings.ForcedTime;
    If fSettings.ForceTimeAutoreset then
      fSettings.ForceTime := False;
  end
else Result := Now;
end;

//------------------------------------------------------------------------------

Function TSimpleLog.GetTimeString(Time: TDateTime): String;
begin
DateTimeToString(Result,fSettings.TimeFormat,Time,fSettings.FormatSettings);
end;

//------------------------------------------------------------------------------

Function TSimpleLog.GetIndentedString(const Str: String; IndentCount: Integer): String;

  procedure PutIndentation(AtPos: TStrOffset);
  var
    ii: TStrOffset;
  begin
    For ii := AtPos to Pred(AtPos + IndentCount) do
      Result[ii] := ' ';
  end;

var
  IndentStr:      String;
  StrPos,ResPos:  TStrOffset;
  ResLen:         TStrSize;
begin
{
  folloving are all recognized linebreak sequences:

    #0
    #10#13
    #13#10
    #10 not followed by #13
    #13 not followed by #10
}
If Length(Str) > 0 then
  begin
    IndentStr := StringOfChar(' ',IndentCount);
    // count how long the resulting string will be for preallocation
    ResLen := 0;
    StrPos := 1;
    while StrPos <= Length(Str) do
      begin
        If Ord(Str[StrPos]) in [10,13] then
          begin
            If StrPos < Length(Str) then
              If (Ord(Str[StrPos + 1]) in [10,13]) and (Str[StrPos + 1] <> Str[StrPos]) then
                begin
                  Inc(ResLen);
                  Inc(StrPos);
                end;
            Inc(ResLen,IndentCount + 1);
          end
        else If Ord(Str[StrPos]) = 0 then
          Inc(ResLen,IndentCount + 1)
        else
          Inc(ResLen);
        Inc(StrPos);
      end;
    SetLength(Result,ResLen); // preallocation
    // construct the result
    StrPos := 1;
    ResPos := 1;
    while (StrPos <= Length(Str)) and (ResPos <= Length(Result)) do
      begin
        If Ord(Str[StrPos]) in [10,13] then
          begin
            Result[ResPos] := Str[StrPos];
            If StrPos < Length(Str) then
              If (Ord(Str[StrPos + 1]) in [10,13]) and (Str[StrPos + 1] <> Str[StrPos]) then
                begin
                  Inc(StrPos);
                  Inc(ResPos);
                  Result[ResPos] := Str[StrPos];
                end;
            PutIndentation(ResPos + 1);
            Inc(ResPos,IndentCount);
          end
        else If Ord(Str[StrPos]) = 0 then
          begin
            Result[ResPos] := Str[StrPos];
            PutIndentation(ResPos + 1);
            Inc(ResPos,IndentCount);
          end
        else
          Result[ResPos] := Str[StrPos];
        Inc(StrPos);
        Inc(ResPos);        
      end;
  end
else Result := '';
end;

//------------------------------------------------------------------------------

Function TSimpleLog.GetStampStr(const StampText: String; ThickBreak: Boolean): String;
begin
If ThickBreak then
  Result := StringOfChar(fStrings.BreakerCharThick,fStrings.LineLength) + sLineBreak +
    StampText + sLineBreak + StringOfChar(fStrings.BreakerCharThick,fStrings.LineLength)
else
  Result := StringOfChar(fStrings.BreakerCharThin,fStrings.LineLength) + sLineBreak +
    StampText + sLineBreak + StringOfChar(fStrings.BreakerCharThin,fStrings.LineLength);
end;

//------------------------------------------------------------------------------

{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
procedure TSimpleLog.WriteLogToOutputs(const LogText: String; LineBreakInStreams: Boolean);
var
  i:          Integer;
  StreamStr:  UTF8String;
begin
// write to outputs
If loInternal in fSettings.LogOutputs then
  fInternalLog.Add(LogText);
If LineBreakInStreams then
  StreamStr := StrToUTF8(LogText + sLineBreak)
else
  StreamStr := StrToUTF8(LogText);
If (loStream in fSettings.LogOutputs) and Assigned(fStreamLog) then
  fStreamLog.WriteBuffer(PUTF8Char(StreamStr)^,Length(StreamStr) * SizeOf(UTF8Char));
If (loFile in fSettings.LogOutputs) and Assigned(fFileLogStream) then
  fFileLogStream.WriteBuffer(PUTF8Char(StreamStr)^,Length(StreamStr) * SizeOf(UTF8Char));
If loConsole in fSettings.LogOutputs then
  WriteLn(StrToCsl(LogText));
If loExternals in fSettings.LogOutputs then
  For i := LowIndex to HighIndex do
    If fExternalLogs[i].Active then
      fExternalLogs[i].LogObject.Add(LogText);
Inc(fLogCounter); // this can overflow
DoOnLog(LogText);
end;
{$IFDEF OverflowChecks}{$Q+}{$ENDIF}

//------------------------------------------------------------------------------

procedure TSimpleLog.ProcessConsoleLog(const LogText: String);
begin
WriteLogToOutputs(LogText,False);
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.ProcessLocalLog(const LogText: String; IndentCount: Integer = 0);
begin
If fSettings.IndentLines and (IndentCount > 0) then
  WriteLogToOutputs(GetIndentedString(LogText,IndentCount),True)
else
  WriteLogToOutputs(LogText,True);
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.DoOnLog(const LogText: String);
begin
If Assigned(fOnLogEvent) then
  fOnLogEvent(Self,LogText);
If Assigned(fOnLogCallback) then
  fOnLogCallback(Self,LogText);
end;

//==============================================================================

constructor TSimpleLog.Create;
begin
inherited Create;
Initialize;
end;

//------------------------------------------------------------------------------

destructor TSimpleLog.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

Function TSimpleLog.ActiveOutput(Output: TSLLogOutput): Boolean;
begin
Result := Output in fSettings.LogOutputs;
end;

//------------------------------------------------------------------------------

Function TSimpleLog.ActivateOutput(Output: TSLLogOutput): Boolean;
begin
Result := Output in fSettings.LogOutputs;
{
  Logging to console cannot be activated if the console is currently binded or
  is not present.
}
If (Output <> loConsole) or (fConsolePresent and not fConsoleBinded) then
  Include(fSettings.LogOutputs,Output);
end;

//------------------------------------------------------------------------------

Function TSimpleLog.DeactivateOutput(Output: TSLLogOutput): Boolean;
begin
Result := Output in fSettings.LogOutputs;
Exclude(fSettings.LogOutputs,Output);
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.SetupOutputToStream(Stream: TStream; Append: Boolean; Activate: Boolean = True);
begin
{
  If the stream is already assigned, ignore it and just assign the new one.
  Management of these streams is completely external to simple log.

  If Append is true, set position to the end of the stream, otherwise leave
  whatever position is currently set.
}
fStreamLog := Stream;
If Append then
  fStreamLog.Seek(0,soEnd);
If Activate then
  ActivateOutput(loStream);
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.SetupOutputToFile(const FileName: String; Append: Boolean; Activate: Boolean = True);
begin
// If there is already a file opened, close it.
If Assigned(fFileLogStream) then
  FreeAndNil(fFileLogStream);
fFileLog := FileName;
If FileExists(StrToRTL(fFileLog)) and Append then
  fFileLogStream := TFileStream.Create(StrToRTL(fFileLog),fmOpenReadWrite or fmShareDenyWrite)
else
  fFileLogStream := TFileStream.Create(StrToRTL(fFileLog),fmCreate or fmShareDenyWrite);
If Append then
  fFileLogStream.Seek(0,soEnd);
If Activate then
  ActivateOutput(loFile);
end;

//------------------------------------------------------------------------------

Function TSimpleLog.LowIndex: Integer;
begin
Result := Low(fExternalLogs);
end;
 
//------------------------------------------------------------------------------

Function TSimpleLog.HighIndex: Integer;
begin
Result := Pred(fExternalLogCount);
end;
 
//------------------------------------------------------------------------------

Function TSimpleLog.ExternalLogLowIndex: Integer;
begin
Result := LowIndex;
end;
  
//------------------------------------------------------------------------------

Function TSimpleLog.ExternalLogHighIndex: Integer;
begin
Result := HighIndex;
end;

//------------------------------------------------------------------------------

Function TSimpleLog.ExternalLogIndexOf(LogObject: TStrings): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := LowIndex to HighIndex do
  If fExternalLogs[i].LogObject = LogObject then
    begin
      Result := i;
      Break{For i};
    end;
end;

//------------------------------------------------------------------------------

Function TSimpleLog.ExternalLogAdd(LogObject: TStrings; Active: Boolean = True; Owned: Boolean = False): Integer;
begin
Grow;
Result := fExternalLogCount;
fExternalLogs[Result].LogObject := LogObject;
fExternalLogs[Result].Active := Active;
fExternalLogs[Result].Owned := Owned;
Inc(fExternalLogCount);
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.ExternalLogInsert(Index: Integer; LogObject: TStrings; Active: Boolean = True; Owned: Boolean = False);
var
  i:  Integer;
begin
If CheckIndex(Index) then
  begin
    Grow;
    For i := HighIndex downto Index do
      fExternalLogs[i + 1] := fExternalLogs[i];
    fExternalLogs[Index].LogObject := LogObject;
    fExternalLogs[Index].Active := Active;
    fExternalLogs[Index].Owned := Owned;
    Inc(fExternalLogCount);
  end
else If Index = fExternalLogCount then
  ExternalLogAdd(LogObject,Active,Owned)
else
  raise ESLIndexOutOfBounds.CreateFmt('TSimpleLog.ExternalLogInsert: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TSimpleLog.ExternalLogExtract(LogObject: TStrings): TStrings;
var
  Index,i:  Integer;
begin
Index := ExternalLogIndexOf(LogObject);
If CheckIndex(Index) then
  begin
    Result := fExternalLogs[Index].LogObject;
    For i := Index to Pred(HighIndex) do
      fExternalLogs[i] := fExternalLogs[i + 1];
    Dec(fExternalLogCount);
    Shrink;
  end
else Result := nil;
end;

//------------------------------------------------------------------------------

Function TSimpleLog.ExternalLogRemove(LogObject: TStrings): Integer;
begin
Result := ExternalLogIndexOf(LogObject);
If CheckIndex(Result) then
  ExternalLogDelete(Result);
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.ExternalLogDelete(Index: Integer);
var
  i:  Integer;
begin
If CheckIndex(Index) then
  begin
    If fExternalLogs[Index].Owned then
      FreeAndNil(fExternalLogs[Index].LogObject);
    For i := Index to Pred(HighIndex) do
      fExternalLogs[i] := fExternalLogs[i + 1];
    Dec(fExternalLogCount);
    Shrink;
  end
else raise ESLIndexOutOfBounds.CreateFmt('TSimpleLog.ExternalLogDelete: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.ExternalLogClear;
var
  i:  Integer;
begin
For i := LowIndex to HighIndex do
  If fExternalLogs[i].Owned then
    FreeAndNil(fExternalLogs[i].LogObject);
SetLength(fExternalLogs,0);
fExternalLogCount := 0;
end;

//------------------------------------------------------------------------------

Function TSimpleLog.ExternalLogActive(Index: Integer): Boolean;
begin
If CheckIndex(Index) then
  Result := fExternalLogs[Index].Active
else
  raise ESLIndexOutOfBounds.CreateFmt('TSimpleLog.ExternalLogActive: Index (%d) out of bounds.',[Index]);
end;
 
//------------------------------------------------------------------------------

Function TSimpleLog.ExternalLogSetActive(Index: Integer; Active: Boolean): Boolean;
begin
If CheckIndex(Index) then
  begin
    Result := fExternalLogs[Index].Active;
    fExternalLogs[Index].Active := Active;
  end
else raise ESLIndexOutOfBounds.CreateFmt('TSimpleLog.ExternalLogSetActive: Index (%d) out of bounds.',[Index]);
end;
 
//------------------------------------------------------------------------------

Function TSimpleLog.ExternalLogOwned(Index: Integer): Boolean;
begin
If CheckIndex(Index) then
  Result := fExternalLogs[Index].Owned
else
  raise ESLIndexOutOfBounds.CreateFmt('TSimpleLog.ExternalLogOwned: Index (%d) out of bounds.',[Index]);
end;
 
//------------------------------------------------------------------------------

Function TSimpleLog.ExternalLogSetOwned(Index: Integer; Owned: Boolean): Boolean;
begin
If CheckIndex(Index) then
  begin
    Result := fExternalLogs[Index].Owned;
    fExternalLogs[Index].Owned := Owned;
  end
else raise ESLIndexOutOfBounds.CreateFmt('TSimpleLog.ExternalLogSetOwned: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TSimpleLog.ForceTimeSet(Time: TDateTime; Autoreset: Boolean = False): Boolean;
begin
Result := fSettings.ForceTime;
fSettings.ForceTime := True;
fSettings.ForcedTime := Time;
fSettings.ForceTimeAutoreset := Autoreset;
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddLogNoTime(const LogText: String);
begin
ProcessLocalLog(LogText);
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddLogTime(const LogText: String; Time: TDateTime);
var
  TimeStr:  String;
begin
TimeStr := GetTimeString(Time) + fSettings.TimeSeparator;
ProcessLocalLog(TimeStr + LogText,Length(TimeStr));
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddLog(const LogText: String);
begin
AddLogTime(LogText,GetTime);
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddEmpty;
begin
AddLogNoTime('');
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddBreaker;
begin
AddLogNoTime(StringOfChar(fStrings.BreakerCharThin,fStrings.LineLength));
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddBreakerThin;
begin
AddLogNoTime(StringOfChar(fStrings.BreakerCharThin,fStrings.LineLength));
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddBreakerThick;
begin
AddLogNoTime(StringOfChar(fStrings.BreakerCharThick,fStrings.LineLength));
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddTimeStamp;
begin
AddLogNoTime(Format(GetStampStr(fStrings.TimeStamp,False),[GetTimeString(GetTime)]));
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddStartStamp;
begin
AddLogNoTime(Format(GetStampStr(fStrings.StartStamp,False),[GetTimeString(GetTime)]));
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddEndStamp;
begin
AddLogNoTime(Format(GetStampStr(fStrings.EndStamp,False),[GetTimeString(GetTime)]));
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddAppendStamp;
begin
AddLogNoTime(Format(GetStampStr(fStrings.AppendStamp,False),[GetTimeString(GetTime)]));
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddHeader;
begin
If Length(fStrings.HeaderText) < fStrings.LineLength then
  AddLogNoTime(GetStampStr(StringOfChar(' ',(fStrings.LineLength - Length(fStrings.HeaderText)) div 2) + fStrings.HeaderText,True))
else
  AddLogNoTime(GetStampStr(fStrings.HeaderText,True));
end;

end.
