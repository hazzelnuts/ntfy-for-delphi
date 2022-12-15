unit Notify.Event;

interface

uses
  Notify.Event.Contract,
  Notify.Action.Contract,
  Notify.Attachment.Contract,
  Notify.Types,
  System.Generics.Collections;

type
  TNotifyMessage = class(TInterfacedObject, INotifyEvent)
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
    FAction: INotifyAction;
    FActions: INotifyEventActions;
    FAttachment: INotifyAttachment;
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
    function Priority: Integer; overload;
    function Priority(const AValue: Integer): INotifyEvent; overload;
    function Action: INotifyAction; overload;
    function Action(const AValue: INotifyAction): INotifyEvent; overload;
    function Actions: INotifyEventActions;
    function Attachment: INotifyAttachment; overload;
    function Attachment(const AValue: INotifyAttachment): INotifyEvent; overload;
  end;

implementation

{ TNotifySubscription }

function TNotifyMessage.Action(const AValue: INotifyAction): INotifyEvent;
begin
  Result := Self;
  FAction := AValue;

  if FActions.ContainsKey(AValue.&Label) then
    FActions.Remove(AValue.&Label);

  if FActions.Count >= 3 then
    Exit;

  FActions.Add(AValue.&Label, AValue);
end;

function TNotifyMessage.Actions: INotifyEventActions;
begin
  Result := FActions;
end;

function TNotifyMessage.Attachment(const AValue: INotifyAttachment): INotifyEvent;
begin
  Result := Self;
  FAttachment := AValue;
end;

function TNotifyMessage.Attachment: INotifyAttachment;
begin
  Result := FAttachment;
end;

function TNotifyMessage.Action: INotifyAction;
begin
  Result := FAction;
end;

function TNotifyMessage.Click(const AValue: String): INotifyEvent;
begin
  Result := Self;
  FClick := AValue;
end;

constructor TNotifyMessage.Create;
begin
  FActions := TDictionary<String, INotifyAction>.Create();
end;

destructor TNotifyMessage.Destroy;
begin
  FActions.Free;
  inherited;
end;

function TNotifyMessage.Click: String;
begin
  Result := FClick;
end;

function TNotifyMessage.Event: String;
begin
  Result := FEvent;
end;

function TNotifyMessage.Event(const AValue: String): INotifyEvent;
begin
  Result := Self;
  FEvent := AValue;
end;

function TNotifyMessage.Id(const AValue: String): INotifyEvent;
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

function TNotifyMessage.MessageContent(const AValue: String): INotifyEvent;
begin
  Result := Self;
  FMessage := AValue;
end;

class function TNotifyMessage.New: INotifyEvent;
begin
  Result := Self.Create;
end;

function TNotifyMessage.Priority(const AValue: Integer): INotifyEvent;
begin
  Result := Self;
  FPriority := AValue;
end;

function TNotifyMessage.Priority: Integer;
begin
  Result := FPriority;
end;

function TNotifyMessage.Tags(const AValue: TArray<String>): INotifyEvent;
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

function TNotifyMessage.Time(const AValue: Integer): INotifyEvent;
begin
  Result := Self;
  FTime := AValue;
end;

function TNotifyMessage.Title(const AValue: String): INotifyEvent;
begin
  Result := Self;
  FTitle := AValue;
end;

function TNotifyMessage.Title: String;
begin
  Result := FTitle;
end;

function TNotifyMessage.Topic(const AValue: String): INotifyEvent;
begin
  Result := Self;
  FTopic := AValue;
end;

function TNotifyMessage.Topic: String;
begin
  Result := FTopic;
end;

end.
