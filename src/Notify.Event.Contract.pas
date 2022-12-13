unit Notify.Event.Contract;

interface

uses
  Notify.Action.Contract,
  Notify.Attachment.Contract,
  System.Generics.Collections;

type

  INotifyEventActions = TDictionary<String, INotifyAction>;

  INotifyEvent = interface
    ['{0342B585-BB88-4A63-9C41-8848B90B6042}']
    function Id: String; overload;
    function Id(const AValue: String): INotifyEvent; overload;
    function Time: Integer; overload;
    function Time(const AValue: Integer): INotifyEvent; overload;
    function Event: String; overload;
    function Event(const AValue: String): INotifyEvent; overload;
    function Topic: String; overload;
    function Topic(const AValue: String): INotifyEvent; overload;
    function Tags: TArray<String>; overload;
    function Tags(const AValue: TArray<String>): INotifyEvent; overload;
    function Click: String; overload;
    function Click(const AValue: String): INotifyEvent; overload;
    function Title: String; overload;
    function Title(const AValue: String): INotifyEvent; overload;
    function MessageContent: String; overload;
    function MessageContent(const AValue: String): INotifyEvent; overload;
    function Priority: Integer; overload;
    function Priority(const AValue: Integer): INotifyEvent; overload;
    function Action: INotifyAction; overload;
    function Action(const AValue: INotifyAction): INotifyEvent; overload;
    function Actions: INotifyEventActions;
    function Attachment: INotifyAttachment; overload;
    function Attachment(const AValue: INotifyAttachment): INotifyEvent; overload;
  end;

  INotifyEventFactory = interface
    ['{8DE90B5D-61A1-4369-B6C7-054F96454546}']
    function Event: INotifyEvent;
  end;

implementation

end.
