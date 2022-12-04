unit Notify.Notification.Contract;

interface

uses
  Notify.Types,
  Notify.Action.Contract;

type
  INotifyTags = array of String;

  INotifyNotification = interface
    ['{BE2E83B7-C39E-4985-93F9-4468976B6AC5}']
    function Topic: String; overload;
    function Topic(const AValue: String): INotifyNotification; overload;
    function MessageContent: String; overload;
    function MessageContent(const AValue: String): INotifyNotification; overload;
    function Title: String; overload;
    function Title(const AValue: String): INotifyNotification; overload;
    function Tags: INotifyTags; overload;
    function Tags(const AValue: INotifyTags): INotifyNotification; overload;
    function Priority: TNotifyPriority; overload;
    function Priority(const AValue: TNotifyPriority): INotifyNotification; overload;
    function Attach(const AValue: String): INotifyNotification; overload;
    function FilePath: String; overload;
    function FilePath(const AValue: String): INotifyNotification; overload;
    function FileName: String; overload;
    function Click: String overload;
    function Click(const AValue: String): INotifyNotification; overload;
    function Action: INotifyAction; overload;
    function Action(const AValue: INotifyAction): INotifyNotification; overload;
    function ClearActions: INotifyNotification; overload;
    function AsJSONString: String;
    function Email: String; overload;
    function Email(const AValue: String): INotifyNotification; overload;
    function Icon: String; overload;
    function Icon(const AValue: String): INotifyNotification; overload;
    function Delay: String; overload;
    function Delay(const AValue: String): INotifyNotification; overload;
  end;

  INotifyNotificationFactory = interface
    ['{06D69619-D97A-4070-8056-846D181955D8}']
    function Notification: INotifyNotification;
  end;

implementation

end.
