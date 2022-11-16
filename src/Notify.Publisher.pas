unit Notify.Publisher;

interface

uses
  Notify.Action.Contract,
  Notify.Publisher.Contract;

type
  TNotifyPublisher = class sealed(TInterfacedObject, INotifyPublisher)
  strict private
    FTopic: String;
    FMessageContent: String;
    FTitle: String;
    FTags: INotifyTags;
    FPriority: String;
    FAttach: String;
    FFileName: String;
    FClick: String;
    FAction: INotifyAction;
  public
    class function New: INotifyPublisher;
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

{ TNotifyPublisher }

function TNotifyPublisher.Action: INotifyAction;
begin

end;

function TNotifyPublisher.Action(const PValue: INotifyAction): INotifyPublisher;
begin

end;

function TNotifyPublisher.Attach: String;
begin

end;

function TNotifyPublisher.Attach(const PValue: String): INotifyPublisher;
begin

end;

function TNotifyPublisher.Click(const PValue: String): INotifyPublisher;
begin

end;

function TNotifyPublisher.Click: String;
begin

end;

function TNotifyPublisher.FileName: String;
begin

end;

function TNotifyPublisher.FileName(const PValue: String): INotifyPublisher;
begin

end;

function TNotifyPublisher.MessageContent(const PValue: String): String;
begin

end;

class function TNotifyPublisher.New: INotifyPublisher;
begin
  Result := Self.Create;
end;

function TNotifyPublisher.MessageContent: String;
begin

end;

function TNotifyPublisher.Priority(const PValue: Integer): INotifyPublisher;
begin

end;

function TNotifyPublisher.Priority: Integer;
begin

end;

function TNotifyPublisher.Tags: INotifyTags;
begin

end;

function TNotifyPublisher.Tags(const PValue: INotifyTags): INotifyPublisher;
begin

end;

function TNotifyPublisher.Title(const PValue: String): INotifyPublisher;
begin

end;

function TNotifyPublisher.Title: String;
begin

end;

function TNotifyPublisher.Topic(const PValue: String): INotifyPublisher;
begin

end;

function TNotifyPublisher.Topic: String;
begin

end;

end.
