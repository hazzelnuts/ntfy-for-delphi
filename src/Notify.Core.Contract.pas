unit Notify.Core.Contract;

interface

uses
  System.SysUtils,
  Notify.Types,
  Notify.Api.Contract,
  Notify.Notification.Contract,
  Notify.Config.Contract,
  Notify.Action.Contract,
  Notify.Event.Contract,
  Notify.Attachment.Contract,
  Notify.Api.Response,
  System.Generics.Collections;

type
  INotifyCore = interface
    ['{AEDB3C31-D45F-4469-9427-9CEA5427A4E3}']
    function Cache(const AValue: Boolean): INotifyCore;
    function UserName(const AValue: String): INotifyCore;
    function Password(const AValue: String): INotifyCore;
    function SaveLog(const AValue: Boolean): INotifyCore;
    function LogPath(const AValue: String): INotifyCore;
    function BaseURL(const AValue: String): INotifyCore;
    function SubscriptionType(const AValue: TNotifySubscriptionType): INotifyCore;
    function Topic(const AValue: String): INotifyCore;
    function DisableFireBase(const AValue: Boolean): INotifyCore;
    function Publish: INotifyCore;
    function Unsubscribe: INotifyCore;
    function Notification(const ANotification: INotifyNotification): INotifyCore; overload;
    procedure Subscribe(const ATopic: String; const ACallBack: TNotifyEventProc); overload;
    function Filter(const AFilterType: TNotifyFilter; const AValue: String): INotifyCore;
    function ClearFilters: INotifyCore;
    function Poll(const AValue: Boolean): INotifyCore; overload;
    function Since(const AValue: String): INotifyCore; overload;
    function Scheduled(const AValue: Boolean): INotifyCore; overload;
    function Response: TNotifyApiResponse;
    function Disconnect: INotifyCore;
  end;

  INotifyCoreFacade = interface
    ['{47D19A0C-9C4C-4389-B2CD-CE4515BB4F35}']
    function Api: INotifyApi;
    function Notification: INotifyNotification;
    function Notify: INotifyCore;
    function Action: INotifyAction;
    function Config: INotifyConfig;
    function Event: INotifyEvent;
    function Attachment: INotifyAttachment;
  end;

  INotifyParametersFilters = TDictionary<String, String>;

implementation

end.
