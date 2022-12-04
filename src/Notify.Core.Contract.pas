unit Notify.Core.Contract;

interface

uses
  System.SysUtils,
  Notify.Types,
  Notify.Provider.Contract,
  Notify.Notification.Contract,
  Notify.Action.Contract;

type
  INotifyCore = interface
    ['{AEDB3C31-D45F-4469-9427-9CEA5427A4E3}']
    function Poll: INotifyCore;
    function Since(const AValue: String; const PFeytchType: TNotifyFectchType = TNotifyFectchType.DURATION): INotifyCore; overload;
    function Since(const AValue: Integer): INotifyCore; overload;
    function Icon(const AValue: String): INotifyCore; overload;
    function Listen: INotifyCore;
    function Publish: INotifyCore;
    function Notification(const ANotification: INotifyNotification): INotifyCore; overload;
  end;

  INotifyCoreFactory = interface
    ['{9FCCC1D0-EB04-4307-8F4C-C5642F6F5A52}']
    function Core: INotifyCore;
  end;

  INotifyCoreFacade = interface
    ['{47D19A0C-9C4C-4389-B2CD-CE4515BB4F35}']
    function Provider: INotifyProvider;
    function Notification: INotifyNotification;
    function Notify: INotifyCore;
    function Action: INotifyAction;
  end;

implementation

end.
