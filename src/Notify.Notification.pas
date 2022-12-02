unit Notify.Notification;

interface

uses
  Notify.Types,
  Notify.Action.Contract,
  Notify.Notification.Contract,
  System.Generics.Collections;

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
    FActions: TDictionary<String, INotifyAction>;
  public
    class function New: INotifyNotification;
    constructor Create;
    destructor Destroy; override;
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
    function ClearActions: INotifyNotification; overload;
    function AsJSONString: String;
  end;

implementation

uses
  System.SysUtils,
  Notify.SmartPointer,
  Notify.Action.DTO,
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

  if FActions.ContainsKey(AValue.&Label) then
    FActions.Remove(AValue.&Label);

  if FActions.Count >= 3 then
    Exit;

  FActions.Add(AValue.&Label, AValue);

end;

function TNotifyNotification.AsJSONString: String;
var
  LNotificationDTO: TSmartPointer<TNotifyNotificationDTO>;
  LActionDTO: TNotifyActionsDTO;
  LAction: INotifyAction;
begin
  LNotificationDTO.Value.Topic := FTopic;
  LNotificationDTO.Value.MessageContent := FMessageContent;
  LNotificationDTO.Value.Priority := Ord(FPriority);
  LNotificationDTO.Value.Title := FTitle;
  LNotificationDTO.Value.Tags.AddRange(FTags);

  for LAction in FActions.Values do
  begin
    LActionDTO := TNotifyActionsDTO.Create;
    LActionDTO.Action := NotifyActionTypesArray[LAction.&Type];
    LActionDTO.&Label := LAction.&Label;
    LActionDTO.Clear := LAction.Clear;
    LActionDTO.Url := LAction.Url;

    if LAction.&Type = TNotifyActionType.HTTP then
    begin
      LActionDTO.Method := LAction.Method;
      LActionDTO.Body := LAction.Body;
      LActionDTO.Method := LAction.Method;
      LActionDTO.Headers := TJsonDTO(LAction.Headers);
    end;

    LNotificationDTO.Value.Actions.Add(LActionDTO);
  end;

  Result := LNotificationDTO.Value.AsJson;
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

function TNotifyNotification.ClearActions: INotifyNotification;
begin
  Result := Self;
  FActions.Clear;
end;

function TNotifyNotification.Click(const AValue: String): INotifyNotification;
begin
  Result := Self;
  FClick := AValue;
end;

constructor TNotifyNotification.Create;
begin
  FPriority := TNotifyPriority.DEFAULT;
  FActions := TDictionary<String, INotifyAction>.Create();
end;

destructor TNotifyNotification.Destroy;
begin
  FActions.Free;
  inherited;
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
