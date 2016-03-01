{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

SimpleLog - LogConsole

©František Milt 2016-03-01

Version 1.3.4

===============================================================================}
unit LogConsole;

{$DEFINE SimpleLog_Include}
  {$INCLUDE '.\SimpleLog.pas'}
{$UNDEF SimpleLog_Include}
{$WARNINGS ON}

var
  LogObject:  TSimpleLog = nil;

//------------------------------------------------------------------------------
  
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

//------------------------------------------------------------------------------

procedure Finalize;
begin
If Assigned(LogObject) then LogObject.UnbindConsole;
FreeAndNil(LogObject);
end;

//------------------------------------------------------------------------------

initialization
  Initialize;

finalization
  Finalize;

end.
