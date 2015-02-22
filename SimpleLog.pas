{===============================================================================

SimpleLog

©František Milt 2014-10-26

Version 1.2.4

===============================================================================}
unit SimpleLog;

interface

uses
  SysUtils, Classes, Contnrs, SyncObjs;

type
  TLogEvent = procedure(Sender: TObject; LogText: String) of Object;

{==============================================================================}
{    TSimpleLog // Class declaration                                           }
{==============================================================================}
  TSimpleLog = class(TObject)
  private
    fFormatSettings:          TFormatSettings;
    fTimeOfCreation:          TDateTime;
    fTimeFormat:              String;
    fTimeSeparator:           String;
    fForceTime:               Boolean;
    fForcedTime:              TDateTIme;
    fBreaker:                 String;
    fHeaderText:              String;
    fIndentNewLines:          Boolean;
    fThreadLockedAdd:         Boolean;
    fWriteToConsole:          Boolean;
    fInMemoryLog:             Boolean;
    fStreamToFile:            Boolean;
    fStreamAppend:            Boolean;
    fStreamFileName:          String;
    fStreamFileAccessRights:  Cardinal;
    fThreadLock:              TCriticalSection;
    fInMemoryLogObj:          TStringList;
    fExternalLogs:            TObjectList;
    fStreamFile:              TFileStream;
    fLogCounter:              Integer;
    fOnLog:                   TLogEvent;
    Function GetExternalLog(Index: Integer): TStrings;
    procedure SetStreamToFile(Value: Boolean);
    procedure SetStreamFileName(Value: String);
    Function GetInMemoryLogCount: Integer;
    Function GetExternalLogsCount: Integer;
  protected
    Function GetCurrentTime: TDateTime;
    Function GetTimeAsStr(Time: TDateTime; Format: String = '$'): String; virtual;
    procedure ProtectedAddLog(LogText: String; IndentCount: Integer = 0); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DoIndentNewLines(var Str: String; IndentCount: Integer); virtual;
    procedure ThreadLock; virtual;
    procedure ThreadUnlock; virtual;
    procedure AddLog(Text: String); virtual;
    procedure AddLogForceTime(Text: String; Time: TDateTime); virtual;
    procedure AddLogNoTime(Text: String); virtual;
    procedure AddEmpty; virtual;
    procedure AddBreaker; virtual;
    procedure AddTimeStamp; virtual;
    procedure AddStartStamp; virtual;
    procedure AddEndStamp; virtual;
    procedure AddAppendStamp; virtual;
    procedure AddHeader; virtual;
    procedure ForceTimeSet(Time: TDateTime);
    procedure UnforceTimeSet;
    Function InMemoryLogGetLog(LogIndex: Integer): String; virtual;
    Function InMemoryLogGetAsText: String; virtual;
    procedure InMemoryLogClear; virtual;
    Function InMemoryLogSaveToFile(FileName: String; Append: Boolean = False): Boolean; virtual;
    Function InMemoryLogLoadFromFile(FileName: String; Append: Boolean = False): Boolean; virtual;
    Function ExternalLogAdd(ExternalLog: TStrings): Integer; virtual;
    Function ExternalLogIndexOf(ExternalLog: TStrings): Integer; virtual;
    Function ExternalLogRemove(ExternalLog: TStrings): Integer; virtual;
    procedure ExternalLogDelete(Index: Integer); virtual;
    property ExternalLogs[Index: Integer]: TStrings read GetExternalLog; default;
  published
    property TimeOfCreation: TDateTime read fTimeOfCreation;
    property TimeFormat: String read fTimeFormat write fTimeFormat;
    property TimeSeparator: String read fTimeSeparator write fTimeSeparator;
    property ForceTime: Boolean read fForceTime write fForceTime;
    property ForcedTime: TDateTIme read fForcedTime write fForcedTime;
    property Breaker: String read fBreaker write fBreaker;
    property HeaderText: String read fHeaderText write fHeaderText;
    property IndentNewLines: Boolean read fIndentNewLines write fIndentNewLines;
    property ThreadLockedAdd: Boolean read fThreadLockedAdd write fThreadLockedAdd;
    property WriteToConsole: Boolean read fWriteToConsole write fWriteToConsole;
    property InMemoryLog: Boolean read fInMemoryLog write fInMemoryLog;
    property StreamToFile: Boolean read fStreamToFile write SetStreamToFile;
    property StreamAppend: Boolean read fStreamAppend write fStreamAppend;
    property StreamFileName: String read fStreamFileName write SetStreamFileName;
    property StreamFileAccessRights: Cardinal read fStreamFileAccessRights write fStreamFileAccessRights;
    property LogCounter: Integer read fLogCounter;
    property InMemoryLogCount: Integer read GetInMemoryLogCount;
    property ExternalLogsCount: Integer read GetExternalLogsCount;
    property OnLog: TLogEvent read fOnLog write fOnLog;
  end;

implementation

uses
  Windows, StrUtils;

{==============================================================================}
{    TSimpleLog // Class implementation                                        }
{==============================================================================}

{------------------------------------------------------------------------------}
{    TSimpleLog // Constants                                                   }
{------------------------------------------------------------------------------}

const
  cHeaderLines = '================================================================================';
  cLineLength  = 80;

//--- default settings ---
  def_TimeFormat             = 'yyyy-mm-dd hh:nn:ss.zzz';
  def_TimeSeparator          = ' //: ';
  def_ForceTime              = False;
  def_Breaker                = '--------------------------------------------------------------------------------';
  def_HeaderText             = 'Created by SimpleLog 1.2.4, ©František Milt 2014-10-26';
  def_ThreadLockedAdd        = False;
  def_WriteToConsole         = False;
  def_InMemoryLog            = True;
  def_StreamToFile           = False;
  def_StreamAppend           = False;
  def_StreamFileName         = '';
  def_StreamFileAccessRights = fmShareDenyWrite;


{------------------------------------------------------------------------------}
{    TSimpleLog // Private routines                                            }
{------------------------------------------------------------------------------}

Function TSimpleLog.GetExternalLog(Index: Integer): TStrings;
begin
If (Index >= 0) and (Index < fExternalLogs.Count) then
  Result := TStrings(fExternalLogs[Index])
else
 raise exception.Create('TSimpleLog.GetExternalLog(Index):' + sLineBreak +
                        'Index(' + IntToStr(Index) + ') out of bounds.');
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.SetStreamToFile(Value: Boolean);
begin
If fStreamToFile <> Value then
  If fStreamToFile then
    begin
      FreeAndNil(fStreamFile);
      fStreamToFile := Value;
    end
  else
    begin
      If FileExists(fStreamFileName) then
        fStreamFile := TFileStream.Create(fStreamFileName,fmOpenReadWrite or StreamFileAccessRights)
      else
        fStreamFile := TFileStream.Create(fStreamFileName,fmCreate or StreamFileAccessRights);
      If fStreamAppend then fStreamFile.Seek(0,soEnd)
        else fStreamFile.Size := 0;
      fStreamToFile := Value;
    end;
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.SetStreamFileName(Value: String);
begin
If Value = '' then Value := ExtractFileName(ParamStr(0)) + '_' + GetTimeAsStr(fTimeOfCreation,'YYYY-MM-DD-HH-NN-SS') + '.log';
If not AnsiSameText(fStreamFileName,Value) then
  begin
    If fStreamToFile then
      begin
        fStreamFileName := Value;
        FreeAndNil(fStreamFile);
        If FileExists(fStreamFileName) then
          fStreamFile := TFileStream.Create(fStreamFileName,fmOpenReadWrite or StreamFileAccessRights)
        else
          fStreamFile := TFileStream.Create(fStreamFileName,fmCreate or StreamFileAccessRights);
        If fStreamAppend then fStreamFile.Seek(0,soEnd)
          else fStreamFile.Size := 0;
      end
    else fStreamFileName := Value;
  end;
end;

//------------------------------------------------------------------------------

Function TSimpleLog.GetInMemoryLogCount: Integer;
begin
Result := fInMemoryLogObj.Count;
end;

//------------------------------------------------------------------------------

Function TSimpleLog.GetExternalLogsCount: Integer;
begin
Result := fExternalLogs.Count;
end;

{------------------------------------------------------------------------------}
{    TSimpleLog // Protected routines                                          }
{------------------------------------------------------------------------------}

Function TSimpleLog.GetCurrentTime: TDateTime;
begin
If ForceTime then Result := ForcedTime
  else Result := Now;
end;

//------------------------------------------------------------------------------

Function TSimpleLog.GetTimeAsStr(Time: TDateTime; Format: String = '$'): String;
begin
If Format <> '$' then DateTimeToString(Result,Format,Time,fFormatSettings)
  else DateTimeToString(Result,fTimeFormat,Time,fFormatSettings);
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.ProtectedAddLog(LogText: String; IndentCount: Integer = 0);
var
  i:    Integer;
  Temp: String;
begin
If fIndentNewLines then DoIndentNewLines(LogText,IndentCount);
If fWriteToConsole and System.IsConsole then WriteLn(LogText);
If fInMemoryLog then fInMemoryLogObj.Add(LogText);
For i := 0 to (fExternalLogs.Count - 1) do TStrings(fExternalLogs[i]).Add(LogText);
If fStreamToFile then
  begin
    Temp := LogText + sLineBreak;
    fStreamFile.WriteBuffer(PChar(Temp)^, Length(Temp) * SizeOf(Char));
  end;
Inc(fLogCounter);
If Assigned(fOnLog) then fOnLog(Self,LogText);
end;

{------------------------------------------------------------------------------}
{    TSimpleLog // Public routines                                             }
{------------------------------------------------------------------------------}

constructor TSimpleLog.Create;
begin
inherited Create;
fTimeOfCreation := Now;
fThreadLock := TCriticalSection.Create;
fInMemoryLogObj := TStringList.Create;
fExternalLogs := TObjectList.Create(False);
TimeFormat := def_TimeFormat;
TimeSeparator := def_TimeSeparator;
ForceTime := def_ForceTime;
ForcedTime := GetCurrentTime;
Breaker := def_Breaker;
HeaderText := def_HeaderText;
ThreadLockedAdd := def_ThreadLockedAdd;
WriteToConsole := def_WriteToConsole;
InMemoryLog := def_InMemoryLog;
StreamToFile := def_StreamToFile;
StreamAppend := def_StreamAppend;
StreamFileName := def_StreamFileName;
StreamFileAccessRights := def_StreamFileAccessRights;
GetLocaleFormatSettings(LOCALE_USER_DEFAULT,fFormatSettings);
end;

//------------------------------------------------------------------------------

destructor TSimpleLog.Destroy;
begin
If Assigned(fStreamFile) then FreeAndNil(fStreamFile);
fExternalLogs.Free;
fInMemoryLogObj.Free;
fThreadLock.Free;
inherited;
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.DoIndentNewLines(var Str: String; IndentCount: Integer);
begin
If (IndentCount > 0) and AnsiContainsStr(Str,sLineBreak) then
  Str := AnsiReplaceStr(Str,sLineBreak,sLineBreak + StringOfChar(' ',IndentCount));
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.ThreadLock;
begin
fThreadLock.Enter;
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.ThreadUnlock;
begin
fThreadLock.Leave;
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddLog(Text: String);
begin
AddLogForceTime(Text,GetCurrentTime);
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddLogForceTime(Text: String; Time: TDateTime);
var
  TimeStr:  String;
begin
TimeStr := GetTimeAsStr(Time) + fTimeSeparator;
If fThreadLockedAdd then
  begin
    fThreadLock.Enter;
    try
      ProtectedAddLog(TimeStr + Text,Length(TimeStr));
    finally
      fThreadLock.Leave;
    end;
  end
else ProtectedAddLog(TimeStr + Text,Length(TimeStr));
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddLogNoTime(Text: String);
begin
If fThreadLockedAdd then
  begin
    fThreadLock.Enter;
    try
      ProtectedAddLog(Text);
    finally
      fThreadLock.Leave;
    end;
  end
else ProtectedAddLog(Text);
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddEmpty;
begin
AddLogNoTime('');
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddBreaker;
begin
AddLogNoTime(fBreaker);
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddTimeStamp;
begin
AddLogNoTime(fBreaker + sLineBreak + 'TimeStamp: ' + GetTimeAsStr(GetCurrentTime) + sLineBreak + fBreaker);
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddStartStamp;
begin
AddLogNoTime(fBreaker + sLineBreak + GetTimeAsStr(GetCurrentTime) + ' - Starting log...' + sLineBreak + fBreaker);
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddEndStamp;
begin
AddLogNoTime(fBreaker + sLineBreak + GetTimeAsStr(GetCurrentTime) + ' - Ending log.' + sLineBreak + fBreaker);
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.AddAppendStamp;
begin
AddLogNoTime(fBreaker + sLineBreak + GetTimeAsStr(GetCurrentTime) + ' - Appending log...' + sLineBreak + fBreaker);
end;
 
//------------------------------------------------------------------------------

procedure TSimpleLog.AddHeader;
var
  TempStrings:  TStringList;
  i:            Integer;
begin
TempStrings := TStringList.Create;
try
  TempStrings.Text := HeaderText;
  For i := 0 to (TempStrings.Count - 1) do
    If Length(TempStrings[i]) < cLineLength then
      TempStrings[i] := StringOfChar(' ', (cLineLength - Length(TempStrings[i])) div 2) +
                        TempStrings[i];
  AddLogNoTime(cHeaderLines + sLineBreak + TempStrings.Text + cHeaderLines);
finally
  TempStrings.Free;
end;
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.ForceTimeSet(Time: TDateTime);
begin
fForcedTime := Time;
fForceTime := True;
end;

//------------------------------------------------------------------------------

procedure TSimpleLog.UnforceTimeSet;
begin
fForceTime := False;
end;

//------------------------------------------------------------------------------

Function TSimpleLog.InMemoryLogGetLog(LogIndex: Integer): String;
begin
If (LogIndex < 0) or (LogIndex >= fInMemoryLogObj.Count) then Result := ''
  else Result := fInMemoryLogObj[LogIndex];
end;

Function TSimpleLog.InMemoryLogGetAsText: String;
begin
Result := fInMemoryLogObj.Text;
end;
   
//------------------------------------------------------------------------------

procedure TSimpleLog.InMemoryLogClear;
begin
fInMemoryLogObj.Clear;
end;
  
//------------------------------------------------------------------------------

Function TSimpleLog.InMemoryLogSaveToFile(FileName: String; Append: Boolean = False): Boolean;
var
  FileStream:   TFileStream;
  StringBuffer: String;
begin
Result := True;
try
  If FileExists(FileName) then
    FileStream := TFileStream.Create(FileName,fmOpenReadWrite or fmShareDenyWrite)
  else
    FileStream := TFileStream.Create(FileName,fmCreate or fmShareDenyWrite);
  try
    If not Append then FileStream.Size := 0;
    StringBuffer := fInMemoryLogObj.Text;
    FileStream.Seek(0,soEnd);
    FileStream.WriteBuffer(PChar(StringBuffer)^,Length(StringBuffer) * SizeOf(Char));
  finally
    FileStream.Free;
  end;
except
  Result := False;
end;
end;
    
//------------------------------------------------------------------------------

Function TSimpleLog.InMemoryLogLoadFromFile(FileName: String; Append: Boolean = False): Boolean;
var
  FileStream:   TFileStream;
  StringBuffer: String;
begin
Result := True;
try
  FileStream := TFileStream.Create(FileName,fmOpenRead or fmShareDenyWrite);
  try
    If not Append then fInMemoryLogObj.Clear;
    FileStream.Position := 0;
    SetLength(StringBuffer,FileStream.Size div SizeOf(Char));
    FileStream.ReadBuffer(PChar(StringBuffer)^,Length(StringBuffer) * SizeOf(Char));
    fInMemoryLogObj.Text := fInMemoryLogObj.Text + StringBuffer;
  finally
    FileStream.Free;
  end;
except
  Result := False;
end;
end;
    
//------------------------------------------------------------------------------

Function TSimpleLog.ExternalLogAdd(ExternalLog: TStrings): Integer;
begin
Result := fExternalLogs.Add(ExternalLog);
end;
     
//------------------------------------------------------------------------------

Function TSimpleLog.ExternalLogIndexOf(ExternalLog: TStrings): Integer;
begin
Result := fExternalLogs.IndexOf(ExternalLog);
end;
     
//------------------------------------------------------------------------------

Function TSimpleLog.ExternalLogRemove(ExternalLog: TStrings): Integer;
var
  Index: Integer;
begin
Result := -1;
Index := fExternalLogs.IndexOf(ExternalLog);
If Index >= 0 then Result := fExternalLogs.Remove(ExternalLog);
end;
    
//------------------------------------------------------------------------------

procedure TSimpleLog.ExternalLogDelete(Index: Integer);
begin
If (Index >= 0) and (Index < fExternalLogs.Count) then
  fExternalLogs.Delete(Index)
else
 raise exception.Create('TSimpleLog.ExternalLogDelete(Index):' + sLineBreak +
                        'Index(' + IntToStr(Index) + ') out of bounds.');
end;

end.
