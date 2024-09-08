unit Notify.Config.Contract;

interface

uses
  Notify.Types;

type
  INotifyConfig = interface
    ['{8CFCA0D0-3637-4367-9F56-B420D5441659}']
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
    function Poll: Boolean;  overload;
    function Poll(const AValue: Boolean): INotifyConfig; overload;
    function Since: String; overload;
    function Since(const AValue: String): INotifyConfig; overload;
    function Scheduled: Boolean; overload;
    function Scheduled(const AValue: Boolean): INotifyConfig; overload;
  end;

implementation

end.
