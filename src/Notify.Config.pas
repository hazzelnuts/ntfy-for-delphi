unit Notify.Config;

interface

type
  TNotifyConfig = class sealed
  strict private
    FBaseURL: String;
  published
    class function NewInstance: TObject; override;
    property BaseURL: String read FBaseURL write FBaseURL;
  end;

var
  NotifyConfig: TNotifyConfig;

implementation

uses
  System.SysUtils;

{ TNotifyConfig }

class function TNotifyConfig.NewInstance: TObject;
begin
  if not Assigned(NotifyConfig) then
    NotifyConfig := TNotifyConfig(inherited NewInstance);
  Result := NotifyConfig;
end;

initialization
  NotifyConfig := TNotifyConfig.Create;
  NotifyConfig.BaseURL := 'https://ntfy.sh';

finalization
  NotifyConfig.Free;

end.
