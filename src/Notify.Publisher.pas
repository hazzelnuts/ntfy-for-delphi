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
    FPriority: Integer;
    FAttach: String;
    FFileName: String;
    FClick: String;
    FAction: INotifyAction;
  public
    class function New: INotifyPublisher;
    function Topic: String; overload;
    function Topic(const PValue: String): INotifyPublisher; overload;
    function MessageContent: String; overload;
    function MessageContent(const PValue: String): INotifyPublisher; overload;
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
    function AsJSONString: String;
  end;

implementation

uses
  Notify.SmartPointer,
  Notify.Publisher.DTO;

{ TNotifyPublisher }

function TNotifyPublisher.Action: INotifyAction;
begin
  Result := FAction;
end;

function TNotifyPublisher.Action(const PValue: INotifyAction): INotifyPublisher;
begin
  Result := Self;
  FAction := PValue;
end;

function TNotifyPublisher.AsJSONString: String;
var
  LPublisherJSON: TSmartPointer<TNotifyPublisherDTO>;
begin
  LPublisherJSON.Value.Topic := FTopic;
  LPublisherJSON.Value.MessageContent := FMessageContent;
  Result := LPublisherJSON.Value.AsJson;
end;

function TNotifyPublisher.Attach: String;
begin
  Result := FAttach;
end;

function TNotifyPublisher.Attach(const PValue: String): INotifyPublisher;
begin
  Result := Self;
  FAttach := PValue;
end;

function TNotifyPublisher.Click(const PValue: String): INotifyPublisher;
begin
  Result := Self;
  FClick := PValue;
end;

function TNotifyPublisher.Click: String;
begin
  Result := FClick;
end;

function TNotifyPublisher.FileName: String;
begin
  Result := FFileName;
end;

function TNotifyPublisher.FileName(const PValue: String): INotifyPublisher;
begin
  Result := Self;
  FFileName := PValue;
end;

function TNotifyPublisher.MessageContent(const PValue: String): INotifyPublisher;
begin
  Result := Self;
  FMessageContent := PValue;
end;

class function TNotifyPublisher.New: INotifyPublisher;
begin
  Result := Self.Create;
end;

function TNotifyPublisher.MessageContent: String;
begin
  Result := FMessageContent;
end;

function TNotifyPublisher.Priority(const PValue: Integer): INotifyPublisher;
begin
  Result := Self;
  FPriority := PValue;
end;

function TNotifyPublisher.Priority: Integer;
begin
  Result := FPriority;
end;

function TNotifyPublisher.Tags: INotifyTags;
begin
  Result := FTags;
end;

function TNotifyPublisher.Tags(const PValue: INotifyTags): INotifyPublisher;
begin
  Result := Self;
  FTags := PValue;
end;

function TNotifyPublisher.Title(const PValue: String): INotifyPublisher;
begin
  Result := Self;
  FTitle := PValue;
end;

function TNotifyPublisher.Title: String;
begin
  Result := FTitle;
end;

function TNotifyPublisher.Topic(const PValue: String): INotifyPublisher;
begin
  Result := Self;
  FTopic := PValue;
end;

function TNotifyPublisher.Topic: String;
begin
  Result := FTopic;
end;

end.
