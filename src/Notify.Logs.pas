unit Notify.Logs;

interface

uses
  System.Classes;

type
  TNotifyLogs = class sealed
  public
    class procedure Log(const APath: String; AValue: String = ''); overload;
    class procedure Log(const APath: String; AValue: TMemoryStream); overload;
  end;

implementation

uses
  System.SysUtils, System.DateUtils;

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

class procedure TNotifyLogs.Log(const APath: String; AValue: TMemoryStream);
var
  LStrings: TStringList;
  LLogFileName: String;
  LDataString: String;
begin

  if AValue = nil then
    Exit;

  LStrings := TStringList.Create;
  try
    LLogFileName := 'ntfy-' + FormatDateTime('dd.MM.yyyy', Now) + '.txt';

    if FileExists(LLogFileName) then
      LStrings.LoadFromFile(LLogFileName);

    SetString(LDataString, PAnsiChar(AValue.Memory), AValue.Size);
    LStrings.Add(Format('%s : %s', [DateTimeToStr(Now), LDataString]) );
    LStrings.SaveToFile(LLogFileName);
  finally
    LStrings.Free;
  end;

end;

end.
