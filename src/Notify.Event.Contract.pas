unit Notify.Event.Contract;

interface

type
  INotifyMessage = interface
    ['{0342B585-BB88-4A63-9C41-8848B90B6042}']
    function Id: String; overload;
    function Id(const AValue: String): INotifyMessage; overload;
    function Time: Integer; overload;
    function Time(const AValue: Integer): INotifyMessage; overload;
    function Event: String; overload;
    function Event(const AValue: String): INotifyMessage; overload;
    function Topic: String; overload;
    function Topic(const AValue: String): INotifyMessage; overload;
    function Tags: TArray<String>; overload;
    function Tags(const AValue: TArray<String>): INotifyMessage; overload;
    function Click: String; overload;
    function Click(const AValue: String): INotifyMessage; overload;
    function Title: String; overload;
    function Title(const AValue: String): INotifyMessage; overload;
    function MessageContent: String; overload;
    function MessageContent(const AValue: String): INotifyMessage; overload;
    function Priority: Integer; overload;
    function Priority(const AValue: Integer): INotifyMessage; overload;
  end;

  INotifyMessageFactory = interface
    ['{8DE90B5D-61A1-4369-B6C7-054F96454546}']
    function NotifyMessage: INotifyMessage;
  end;

implementation

end.
