unit Notify.Event;

interface

uses
  Notify.Event.Contract,
  Notify.Types;

type
  TNotifyMessage = class(TInterfacedObject, INotifyMessage)
  private
    FId: String;
    FTime: Integer;
    FEvent: String;
    FTopic: String;
    FTags: TArray<String>;
    FClick: String;
    FTitle: String;
    FMessage: String;
    FPriority: Integer;
  public
    class function New: INotifyMessage;
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

implementation

{ TNotifySubscription }

function TNotifyMessage.Click(const AValue: String): INotifyMessage;
begin
  Result := Self;
  FClick := AValue;
end;

function TNotifyMessage.Click: String;
begin
  Result := FClick;
end;

function TNotifyMessage.Event: String;
begin
  Result := FEvent;
end;

function TNotifyMessage.Event(const AValue: String): INotifyMessage;
begin
  Result := Self;
  FEvent := AValue;
end;

function TNotifyMessage.Id(const AValue: String): INotifyMessage;
begin
  Result := Self;
  FId := AValue;
end;

function TNotifyMessage.Id: String;
begin
  Result := FId;
end;

function TNotifyMessage.MessageContent: String;
begin
  Result := FMessage;
end;

function TNotifyMessage.MessageContent(const AValue: String): INotifyMessage;
begin
  Result := Self;
  FMessage := AValue;
end;

class function TNotifyMessage.New: INotifyMessage;
begin
  Result := Self.Create;
end;

function TNotifyMessage.Priority(const AValue: Integer): INotifyMessage;
begin
  Result := Self;
  FPriority := AValue;
end;

function TNotifyMessage.Priority: Integer;
begin
  Result := FPriority;
end;

function TNotifyMessage.Tags(const AValue: TArray<String>): INotifyMessage;
begin
  Result := Self;
  FTags := AValue;
end;

function TNotifyMessage.Tags: TArray<String>;
begin
  Result := FTags;
end;

function TNotifyMessage.Time: Integer;
begin
  Result := FTime;
end;

function TNotifyMessage.Time(const AValue: Integer): INotifyMessage;
begin
  Result := Self;
  FTime := AValue;
end;

function TNotifyMessage.Title(const AValue: String): INotifyMessage;
begin
  Result := Self;
  FTitle := AValue;
end;

function TNotifyMessage.Title: String;
begin
  Result := FTitle;
end;

function TNotifyMessage.Topic(const AValue: String): INotifyMessage;
begin
  Result := Self;
  FTopic := AValue;
end;

function TNotifyMessage.Topic: String;
begin
  Result := FTopic;
end;

end.
