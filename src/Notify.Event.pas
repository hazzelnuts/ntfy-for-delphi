unit Notify.Event;

interface

uses
  Notify.Event.Contract,
  Notify.Action.Contract,
  Notify.Attachment.Contract, Notify;

type
  TNotifyEvent = class(TInterfacedObject, INotifyEvent)
  private
    FId: String;
    FTime: Integer;
    FEvent: String;
    FTopic: String;
    FTags: TArray<String>;
    FClick: String;
    FTitle: String;
    FMessage: String;
    FPriority: TNotifyPriority;
    FAction: INotifyAction;
    FActions: INotifyEventActions;
    FAttachment: INotifyAttachment;
    FIcon: String;
  public
    class function New: INotifyEvent;
    constructor Create;
    destructor Destroy; override;
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
    function Priority: TNotifyPriority; overload;
    function Priority(const AValue: TNotifyPriority): INotifyEvent; overload;
    function Action: INotifyAction; overload;
    function Action(const AValue: INotifyAction): INotifyEvent; overload;
    function Actions: INotifyEventActions;
    function Attachment: INotifyAttachment; overload;
    function Attachment(const AValue: INotifyAttachment): INotifyEvent; overload;
    function Icon: String; overload;
    function Icon(const AValue: String): INotifyEvent; overload;
  end;

implementation

uses
  System.Generics.Collections;

{ TNotifySubscription }

function TNotifyEvent.Action(const AValue: INotifyAction): INotifyEvent;
begin
  Result := Self;
  FAction := AValue;

  if FActions.ContainsKey(AValue.&Label) then
    FActions.Remove(AValue.&Label);

  if FActions.Count >= 3 then
    Exit;

  FActions.Add(AValue.&Label, AValue);
end;

function TNotifyEvent.Actions: INotifyEventActions;
begin
  Result := FActions;
end;

function TNotifyEvent.Attachment(const AValue: INotifyAttachment): INotifyEvent;
begin
  Result := Self;
  FAttachment := AValue;
end;

function TNotifyEvent.Attachment: INotifyAttachment;
begin
  Result := FAttachment;
end;

function TNotifyEvent.Action: INotifyAction;
begin
  Result := FAction;
end;

function TNotifyEvent.Click(const AValue: String): INotifyEvent;
begin
  Result := Self;
  FClick := AValue;
end;

constructor TNotifyEvent.Create;
begin
  FActions := TDictionary<String, INotifyAction>.Create();
end;

destructor TNotifyEvent.Destroy;
begin
  FActions.Free;
  inherited;
end;

function TNotifyEvent.Click: String;
begin
  Result := FClick;
end;

function TNotifyEvent.Event: String;
begin
  Result := FEvent;
end;

function TNotifyEvent.Event(const AValue: String): INotifyEvent;
begin
  Result := Self;
  FEvent := AValue;
end;

function TNotifyEvent.Icon(const AValue: String): INotifyEvent;
begin
  Result := Self;
  FIcon := AValue;
end;

function TNotifyEvent.Icon: String;
begin
  Result := FIcon;
end;

function TNotifyEvent.Id(const AValue: String): INotifyEvent;
begin
  Result := Self;
  FId := AValue;
end;

function TNotifyEvent.Id: String;
begin
  Result := FId;
end;

function TNotifyEvent.MessageContent: String;
begin
  Result := FMessage;
end;

function TNotifyEvent.MessageContent(const AValue: String): INotifyEvent;
begin
  Result := Self;
  FMessage := AValue;
end;

class function TNotifyEvent.New: INotifyEvent;
begin
  Result := Self.Create;
end;

function TNotifyEvent.Priority(const AValue: TNotifyPriority): INotifyEvent;
begin
  Result := Self;
  FPriority := AValue;
end;

function TNotifyEvent.Priority: TNotifyPriority;
begin
  Result := FPriority;
end;

function TNotifyEvent.Tags(const AValue: TArray<String>): INotifyEvent;
begin
  Result := Self;
  FTags := AValue;
end;

function TNotifyEvent.Tags: TArray<String>;
begin
  Result := FTags;
end;

function TNotifyEvent.Time: Integer;
begin
  Result := FTime;
end;

function TNotifyEvent.Time(const AValue: Integer): INotifyEvent;
begin
  Result := Self;
  FTime := AValue;
end;

function TNotifyEvent.Title(const AValue: String): INotifyEvent;
begin
  Result := Self;
  FTitle := AValue;
end;

function TNotifyEvent.Title: String;
begin
  Result := FTitle;
end;

function TNotifyEvent.Topic(const AValue: String): INotifyEvent;
begin
  Result := Self;
  FTopic := AValue;
end;

function TNotifyEvent.Topic: String;
begin
  Result := FTopic;
end;

end.
