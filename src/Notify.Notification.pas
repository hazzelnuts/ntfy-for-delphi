unit Notify.Notification;

interface

uses
  Notify.Types,
  Notify.Action.Contract,
  Notify.Api.Contract,
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
    FClick: String;
    FAction: INotifyAction;
    FActions: TDictionary<String, INotifyAction>;
    FFileName: String;
    FFilePath: String;
    FAttachment: String;
    FEmail: String;
    FIcon: String;
    FDelay: String;
  public
    class function New: INotifyNotification;
    constructor Create;
    destructor Destroy; override;
  private
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
    function Attach(const AValue: String): INotifyNotification; overload;
    function FileName: String; overload;
    function FilePath: String; overload;
    function FilePath(const AValue: String): INotifyNotification; overload;
    function Click: String overload;
    function Click(const AValue: String): INotifyNotification; overload;
    function Action: INotifyAction; overload;
    function Action(const AValue: INotifyAction): INotifyNotification; overload;
    function ClearActions: INotifyNotification; overload;
    function Email: String; overload;
    function Email(const AValue: String): INotifyNotification; overload;
    function Icon: String; overload;
    function Icon(const AValue: String): INotifyNotification; overload;
    function Delay: String; overload;
    function Delay(const AValue: String): INotifyNotification; overload;
    function AsJSONString: String;
  end;

implementation

uses
  System.SysUtils,
  Notify.SmartPointer,
  Notify.Action.DTO,
  Notify.Notification.DTO,
  System.Classes,
  Notify.Config;

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
  LActionDTO: TNotifyActionDTO;
  LAction: INotifyAction;
begin
  LNotificationDTO.Value.Topic := FTopic;
  LNotificationDTO.Value.MessageContent := FMessageContent;
  LNotificationDTO.Value.Priority := Ord(FPriority);
  LNotificationDTO.Value.Title := FTitle;
  LNotificationDTO.Value.Tags.AddRange(FTags);
  LNotificationDTO.Value.Filename := FFileName;
  LNotificationDTO.Value.Attach := FAttachment;
  LNotificationDTO.Value.Delay := FDelay;
  LNotificationDTO.Value.Email := FEmail;

  for LAction in FActions.Values do
  begin

    LActionDTO := TNotifyActionDTO.Create;
    LActionDTO.Action := NotifyActionTypesArray[LAction.&Type];
    LActionDTO.&Label := LAction.&Label;
    LActionDTO.Clear := LAction.Clear;
    LActionDTO.Url := LAction.Url;
    LActionDTO.Headers := LAction.Headers;

    if LAction.&Type = TNotifyActionType.HTTP then
    begin
      LActionDTO.Method := LAction.Method;
      LActionDTO.Body := LAction.Body;
      LActionDTO.Method := LAction.Method;
      LActionDTO.Headers := TJsonDTO(LAction.Headers);
    end;

    LAction.Validate;
    LNotificationDTO.Value.Actions.Add(LActionDTO);
  end;

  Result := LNotificationDTO.Value.AsJson;
end;

function TNotifyNotification.Attach(const AValue: String): INotifyNotification;
begin
  Result := Self;
  FAttachment := AValue;
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

function TNotifyNotification.Delay(const AValue: String): INotifyNotification;
begin
  Result := Self;
  FDelay := AValue;
end;

function TNotifyNotification.Delay: String;
begin
  Result := FDelay;
end;

destructor TNotifyNotification.Destroy;
begin
  FActions.Free;
  inherited;
end;

function TNotifyNotification.Email(const AValue: String): INotifyNotification;
begin
  //"Email" doesn't works together with "Delay"
  Result := Self;
  FEmail := AValue;
end;

function TNotifyNotification.Email: String;
begin
  Result := FEmail;
end;

function TNotifyNotification.FilePath: String;
begin
  Result := FFilePath;
end;

function TNotifyNotification.Click: String;
begin
  Result := FClick;
end;

function TNotifyNotification.FileName: String;
begin
  Result := FFileName;
end;

function TNotifyNotification.FilePath(const AValue: String): INotifyNotification;
begin
  Result := Self;
  FFileName := ExtractFileName(AValue);
  FFilePath := AValue;
end;

function TNotifyNotification.Icon(const AValue: String): INotifyNotification;
begin
  Result := Self;
  FIcon := AValue;
end;

function TNotifyNotification.Icon: String;
begin
  Result := FIcon;
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
