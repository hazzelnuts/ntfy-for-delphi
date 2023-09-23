unit Notify.Config;

interface

uses
  Notify.Types,
  Notify.Config.Contract;

type
  TNotifyConfig = class sealed(TInterfacedObject, INotifyConfig)
  strict private
    FBaseURL: String;
    FUserName: String;
    FPassword: String;
    FCache: Boolean;
    FDisableFirebaseFCM: Boolean;
    FSaveLog: Boolean;
    FLogPath: String;
    FSubscriptionType: TNotifySubscriptionType;
    FProxyServer, FProxyUser, FProxyPassword: string;
    FProxyPort: integer;
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
    function SaveLog: Boolean; overload;
    function SaveLog(const AValue: Boolean): INotifyConfig; overload;
    function LogPath: String; overload;
    function LogPath(const AValue: String): INotifyConfig; overload;
    function SubscriptionType: TNotifySubscriptionType; overload;
    function SubscriptionType(const AValue: TNotifySubscriptionType): INotifyConfig; overload;
    function ProxyServer: string; overload;
    function ProxyUser: string; overload;
    function ProxyPassword: string; overload;
    function ProxyPort: integer; overload;
    function Proxy(const aProxyServer, aProxyUser, aProxyPassword: string; const aProxyPort: integer): INotifyConfig; overload;
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
  FCache := True;
  FLogPath := ExtractFilePath(ParamStr(0));
  FSubscriptionType := TNotifySubscriptionType.JSON;
end;

function TNotifyConfig.DisableFireBase(const AValue: Boolean): INotifyConfig;
begin
  Result := Self;
  FDisableFirebaseFCM := AValue;
end;

function TNotifyConfig.LogPath(const AValue: String): INotifyConfig;
begin
  Result := Self;
  FLogPath := AValue;
end;

function TNotifyConfig.LogPath: String;
begin
  Result := FLogPath;
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

function TNotifyConfig.Proxy(const AProxyServer, AProxyUser, AProxyPassword: string; const aProxyPort: integer): INotifyConfig;
begin
  FProxyServer := AProxyServer;
  FProxyUser := AProxyUser;
  FProxyPassword := AProxyPassword;
  FProxyPort := AProxyPort;
end;

function TNotifyConfig.ProxyPassword: string;
begin
  Result := FProxyPassword;
end;

function TNotifyConfig.ProxyPort: integer;
begin
  Result := FProxyPort;
end;

function TNotifyConfig.ProxyServer: string;
begin
  Result := FProxyServer;
end;

function TNotifyConfig.ProxyUser: string;
begin
  Result := FProxyUser;
end;

function TNotifyConfig.SaveLog(const AValue: Boolean): INotifyConfig;
begin
  Result := Self;
  FSaveLog := AValue;
end;

function TNotifyConfig.SubscriptionType(const AValue: TNotifySubscriptionType): INotifyConfig;
begin
  Result := Self;
  FSubscriptionType := AValue;

  if AValue in [TNotifySubscriptionType.WEB_SOCKET] then
    FBaseURL := StringReplace(FBaseURL, 'https', 'wss', [rfReplaceAll]);
end;

function TNotifyConfig.SubscriptionType: TNotifySubscriptionType;
begin
  Result := FSubscriptionType;
end;

function TNotifyConfig.SaveLog: Boolean;
begin
  Result := FSaveLog;
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
