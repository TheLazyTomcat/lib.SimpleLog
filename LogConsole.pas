unit LogConsole;

{$DEFINE SimpleLog_Include}
  {$INCLUDE '.\SimpleLog.pas'}
{$UNDEF SimpleLog_Include}
{$WARNINGS ON}

var
  LogObject:  TSimpleLog = nil;
  
procedure Initialize;
begin
If IsConsole then
  begin
    LogObject := TSimpleLog.Create;
    LogObject.InternalLog := False;
    LogObject.StreamToFile := True;
    If LogObject.BindConsole then
      begin
        LogActive := True;
        LogFileName := LogObject.StreamFileName;
      end
    else FreeAndNil(LogObject)
  end;
end;

procedure Finalize;
begin
If Assigned(LogObject) then LogObject.UnbindConsole;
FreeAndNil(LogObject);
end;

initialization
  Initialize;

finalization
  Finalize;

end.
