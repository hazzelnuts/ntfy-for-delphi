unit Notify.Publisher.Contract;

interface

uses
  Notify.Action.Contract;

type
  INotifyTags = array of String;

  INotifyPublisher = interface
    ['{BE2E83B7-C39E-4985-93F9-4468976B6AC5}']
    function Topic: String; overload;
    function Topic(const PValue: String): INotifyPublisher; overload;
    function MessageContent: String; overload;
    function MessageContent(const PValue: String): String; overload;
    function Title: String; overload;
    function Title(const PValue: String): INotifyPublisher; overload;
    function Tags: INotifyTags; overload;
    function Tags(const PValue: INotifyTags): INotifyPublisher; overload;
    function Priority: Integer; overload;
    function Priority(const PValue: Integer): INotifyPublisher; overload;
    function Attach: String; overload;
    function Attach(const PValue: String): INotifyPublisher; overload;
    function FileName: String; overload;
    function FileName(const PValue: String): INotifyPublisher; overload;
    function Click: String overload;
    function Click(const PValue: String): INotifyPublisher; overload;
    function Action: INotifyAction; overload;
    function Action(const PValue: INotifyAction): INotifyPublisher; overload;
  end;

implementation

end.
