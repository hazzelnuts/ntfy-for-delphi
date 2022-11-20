unit Notify.Notification.Contract;

interface

uses
  Notify.Action.Contract;

type
  INotifyTags = array of String;

  INotifyNotification = interface
    ['{BE2E83B7-C39E-4985-93F9-4468976B6AC5}']
    function Topic: String; overload;
    function Topic(const PValue: String): INotifyNotification; overload;
    function MessageContent: String; overload;
    function MessageContent(const PValue: String): INotifyNotification; overload;
    function Title: String; overload;
    function Title(const PValue: String): INotifyNotification; overload;
    function Tags: INotifyTags; overload;
    function Tags(const PValue: INotifyTags): INotifyNotification; overload;
    function Priority: Integer; overload;
    function Priority(const PValue: Integer): INotifyNotification; overload;
    function Attach: String; overload;
    function Attach(const PValue: String): INotifyNotification; overload;
    function FileName: String; overload;
    function FileName(const PValue: String): INotifyNotification; overload;
    function Click: String overload;
    function Click(const PValue: String): INotifyNotification; overload;
    function Action: INotifyAction; overload;
    function Action(const PValue: INotifyAction): INotifyNotification; overload;
    function AsJSONString: String;
  end;

  INotifyNotificationFactory = interface
    ['{06D69619-D97A-4070-8056-846D181955D8}']
    function Notification: INotifyNotification;
  end;

implementation

end.
