unit Notify.Notification;

interface

uses
  Notify.Action.Contract,
  Notify.Notification.Contract;

type
  TNotifyNotification = class sealed(TInterfacedObject, INotifyNotification)
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
    class function New: INotifyNotification;
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

implementation

uses
  Notify.SmartPointer,
  Notify.Notification.DTO;

{ TNotifyNotification }

function TNotifyNotification.Action: INotifyAction;
begin
  Result := FAction;
end;

function TNotifyNotification.Action(const PValue: INotifyAction): INotifyNotification;
begin
  Result := Self;
  FAction := PValue;
end;

function TNotifyNotification.AsJSONString: String;
var
  LNotification: TSmartPointer<TNotifyNotificationDTO>;
begin
  LNotification.Value.Topic := FTopic;
  LNotification.Value.MessageContent := FMessageContent;
  Result := LNotification.Value.AsJson;
end;

function TNotifyNotification.Attach: String;
begin
  Result := FAttach;
end;

function TNotifyNotification.Attach(const PValue: String): INotifyNotification;
begin
  Result := Self;
  FAttach := PValue;
end;

function TNotifyNotification.Click(const PValue: String): INotifyNotification;
begin
  Result := Self;
  FClick := PValue;
end;

function TNotifyNotification.Click: String;
begin
  Result := FClick;
end;

function TNotifyNotification.FileName: String;
begin
  Result := FFileName;
end;

function TNotifyNotification.FileName(const PValue: String): INotifyNotification;
begin
  Result := Self;
  FFileName := PValue;
end;

function TNotifyNotification.MessageContent(const PValue: String): INotifyNotification;
begin
  Result := Self;
  FMessageContent := PValue;
end;

class function TNotifyNotification.New: INotifyNotification;
begin
  Result := Self.Create;
end;

function TNotifyNotification.MessageContent: String;
begin
  Result := FMessageContent;
end;

function TNotifyNotification.Priority(const PValue: Integer): INotifyNotification;
begin
  Result := Self;
  FPriority := PValue;
end;

function TNotifyNotification.Priority: Integer;
begin
  Result := FPriority;
end;

function TNotifyNotification.Tags: INotifyTags;
begin
  Result := FTags;
end;

function TNotifyNotification.Tags(const PValue: INotifyTags): INotifyNotification;
begin
  Result := Self;
  FTags := PValue;
end;

function TNotifyNotification.Title(const PValue: String): INotifyNotification;
begin
  Result := Self;
  FTitle := PValue;
end;

function TNotifyNotification.Title: String;
begin
  Result := FTitle;
end;

function TNotifyNotification.Topic(const PValue: String): INotifyNotification;
begin
  Result := Self;
  FTopic := PValue;
end;

function TNotifyNotification.Topic: String;
begin
  Result := FTopic;
end;

end.
