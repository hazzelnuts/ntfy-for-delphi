unit Notify.Config;

interface

uses
  Notify.Config.Contract;

type
  TNotifyConfig = class sealed(TInterfacedObject, INotifyConfig)
  strict private
    FBaseURL: String;
  public
    class function New: INotifyConfig;
    class function NewInstance: TObject; override;
  private
    function BaseURL: String; overload;
    function BaseURL(const AValue: String): INotifyConfig; overload;
  end;

var
  NotifyConfigInstance: TNotifyConfig;
  NotifyConfig: INotifyConfig;

implementation

uses
  System.SysUtils;

{ TNotifyConfig }

function TNotifyConfig.BaseURL(const AValue: String): INotifyConfig;
begin
  Result := Self;
  FBaseURL  := AValue;
end;

function TNotifyConfig.BaseURL: String;
begin
  Result := FBaseURL;
end;

class function TNotifyConfig.New: INotifyConfig;
begin
  Result := Self.Create;
end;

class function TNotifyConfig.NewInstance: TObject;
begin
  if not Assigned(NotifyConfig) then
    NotifyConfigInstance := TNotifyConfig(inherited NewInstance);
  Result := NotifyConfigInstance;
end;

initialization
  NotifyConfig := TNotifyConfig.New.BaseURL('https://ntfy.sh');

end.
