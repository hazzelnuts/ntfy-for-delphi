unit Notify.Core.Contract;

interface

uses
  System.SysUtils,
  Notify.Types,
  Notify.Api.Contract,
  Notify.Notification.Contract,
  Notify.Config.Contract,
  Notify.Action.Contract;

type
  INotifyCore = interface
    ['{AEDB3C31-D45F-4469-9427-9CEA5427A4E3}']
    function Cache(const AValue: Boolean): INotifyCore;
    function UserName(const AValue: String): INotifyCore;
    function Password(const AValue: String): INotifyCore;
    function SaveLog(const AValue: Boolean): INotifyCore;
    function LogPath(const AValue: String): INotifyCore;
    function BaseURL(const AValue: String): INotifyCore;
    function Topic(const AValue: String): INotifyCore;
    function DisableFireBase(const AValue: Boolean): INotifyCore;
    function Publish: INotifyCore;
    function Subscribe: INotifyCore;
    function Unsubscribe: INotifyCore;
    function Notification(const ANotification: INotifyNotification): INotifyCore; overload;
  end;

  INotifyCoreFactory = interface
    ['{9FCCC1D0-EB04-4307-8F4C-C5642F6F5A52}']
    function Core: INotifyCore;
  end;

  INotifyCoreFacade = interface
    ['{47D19A0C-9C4C-4389-B2CD-CE4515BB4F35}']
    function Api: INotifyApi;
    function Notification: INotifyNotification;
    function Notify: INotifyCore;
    function Action: INotifyAction;
    function Config: INotifyConfig;
  end;

implementation

end.
