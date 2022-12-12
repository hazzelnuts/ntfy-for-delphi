unit Notify.Logs;

interface

type
  TNotifyLogs = class sealed
  public
    class procedure Log(const APath: String; AValue: String = '');
  end;

implementation

uses
  System.Classes, System.SysUtils, System.DateUtils;

{ TNotifyLogs }

class procedure TNotifyLogs.Log(const APath: String; AValue: String);
var
  LStrings: TStringList;
  LLogFileName: String;
begin
  LStrings := TStringList.Create;
  try
    LLogFileName := 'ntfy-' + FormatDateTime('dd.MM.yyyy', Now) + '.txt';

    if FileExists(LLogFileName) then
      LStrings.LoadFromFile(LLogFileName);

    LStrings.Add(Format('%s : %s', [DateTimeToStr(Now), AValue]) );
    LStrings.SaveToFile(LLogFileName);
  finally
    LStrings.Free;
  end;

end;

end.
