unit Notify.Notification;

interface

uses
  Notify.Types,
  Notify.Action.Contract,
  Notify.Notification.Contract;

type
  TNotifyNotification = class sealed(TInterfacedObject, INotifyNotification)
  strict private
    FTopic: String;
    FMessageContent: String;
    FTitle: String;
    FTags: INotifyTags;
    FPriority: TNotifyPriority;
    FAttach: String;
    FFileName: String;
    FClick: String;
    FAction: INotifyAction;
  public
    class function New: INotifyNotification;
    constructor Create;
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
    function Attach: String; overload;
    function Attach(const AValue: String): INotifyNotification; overload;
    function FileName: String; overload;
    function FileName(const AValue: String): INotifyNotification; overload;
    function Click: String overload;
    function Click(const AValue: String): INotifyNotification; overload;
    function Action: INotifyAction; overload;
    function Action(const AValue: INotifyAction): INotifyNotification; overload;
    function AsJSONString: String;
  end;

implementation

uses
  Notify.SmartPointer,
  Notify.Notification.DTO;

{ TNotifyNotification }

function TNotifyNotification.Action: INotifyAction;
begin
  Result := FAction;
end;

function TNotifyNotification.Action(const AValue: INotifyAction): INotifyNotification;
begin
  Result := Self;
  FAction := AValue;
end;

function TNotifyNotification.AsJSONString: String;
var
  LNotification: TSmartPointer<TNotifyNotificationDTO>;
begin
  LNotification.Value.Topic := FTopic;
  LNotification.Value.MessageContent := FMessageContent;
  LNotification.Value.Priority := Ord(FPriority);
  LNotification.Value.Title := FTitle;
  LNotification.Value.Tags.AddRange(FTags);
  Result := LNotification.Value.AsJson;
end;

function TNotifyNotification.Attach: String;
begin
  Result := FAttach;
end;

function TNotifyNotification.Attach(const AValue: String): INotifyNotification;
begin
  Result := Self;
  FAttach := AValue;
end;

function TNotifyNotification.Click(const AValue: String): INotifyNotification;
begin
  Result := Self;
  FClick := AValue;
end;

constructor TNotifyNotification.Create;
begin
  FPriority := TNotifyPriority.DEFAULT;
end;

function TNotifyNotification.Click: String;
begin
  Result := FClick;
end;

function TNotifyNotification.FileName: String;
begin
  Result := FFileName;
end;

function TNotifyNotification.FileName(const AValue: String): INotifyNotification;
begin
  Result := Self;
  FFileName := AValue;
end;

function TNotifyNotification.MessageContent(const AValue: String): INotifyNotification;
begin
  Result := Self;
  FMessageContent := AValue;
end;

class function TNotifyNotification.New: INotifyNotification;
begin
  Result := Self.Create;
end;

function TNotifyNotification.MessageContent: String;
begin
  Result := FMessageContent;
end;

function TNotifyNotification.Priority(const AValue: TNotifyPriority): INotifyNotification;
begin
  Result := Self;
  FPriority := AValue;
end;

function TNotifyNotification.Priority: TNotifyPriority;
begin
  Result := FPriority;
end;

function TNotifyNotification.Tags: INotifyTags;
begin
  Result := FTags;
end;

function TNotifyNotification.Tags(const AValue: INotifyTags): INotifyNotification;
begin
  Result := Self;
  FTags := AValue;
end;

function TNotifyNotification.Title(const AValue: String): INotifyNotification;
begin
  Result := Self;
  FTitle := AValue;
end;

function TNotifyNotification.Title: String;
begin
  Result := FTitle;
end;

function TNotifyNotification.Topic(const AValue: String): INotifyNotification;
begin
  Result := Self;
  FTopic := AValue;
end;

function TNotifyNotification.Topic: String;
begin
  Result := FTopic;
end;

end.
