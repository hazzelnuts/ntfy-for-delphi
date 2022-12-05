unit Notify.Config;

interface

uses
  Notify.Config.Contract;

type
  TNotifyConfig = class sealed(TInterfacedObject, INotifyConfig)
  strict private
    FBaseURL: String;
    FUserName: String;
    FPassword: String;
    FCache: Boolean;
    FDisableFirebaseFCM: Boolean;
  public
    class function New: INotifyConfig;
  private
    constructor Create;
    function BaseURL: String; overload;
    function BaseURL(const AValue: String): INotifyConfig; overload;
    function UserName: String; overload;
    function UserName(const AValue: String): INotifyConfig; overload;
    function Password: String; overload;
    function Password(const AValue: String): INotifyConfig; overload;
    function Cache: Boolean; overload;
    function Cache(const AValue: Boolean): INotifyConfig; overload;
    function DisableFireBase: Boolean; overload;
    function DisableFireBase(const AValue: Boolean): INotifyConfig; overload;
  end;

implementation

uses
  System.SysUtils;

{ TNotifyConfig }

function TNotifyConfig.BaseURL(const AValue: String): INotifyConfig;
begin
  Result := Self;
  FBaseURL  := AValue;
end;

function TNotifyConfig.Cache(const AValue: Boolean): INotifyConfig;
begin
  Result := Self;
  FCache := AValue;
end;

constructor TNotifyConfig.Create;
begin
  FBaseURL := 'https://ntfy.sh';
end;

function TNotifyConfig.DisableFireBase(const AValue: Boolean): INotifyConfig;
begin
  Result := Self;
  FDisableFirebaseFCM := AValue;
end;

function TNotifyConfig.DisableFireBase: Boolean;
begin
  Result := FDisableFirebaseFCM;
end;

function TNotifyConfig.Cache: Boolean;
begin
  Result := FCache;
end;

function TNotifyConfig.BaseURL: String;
begin
  Result := FBaseURL;
end;

class function TNotifyConfig.New: INotifyConfig;
begin
  Result := Self.Create;
end;

function TNotifyConfig.Password: String;
begin
  Result := FPassword;
end;

function TNotifyConfig.Password(const AValue: String): INotifyConfig;
begin
  Result := Self;
  FPassword := AValue;
end;

function TNotifyConfig.UserName: String;
begin
  Result := FUserName;
end;

function TNotifyConfig.UserName(const AValue: String): INotifyConfig;
begin
  Result := Self;
  FUserName := AValue;
end;

end.
