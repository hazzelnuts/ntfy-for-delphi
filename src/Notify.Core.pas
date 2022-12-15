unit Notify.Core;

interface

uses
  Notify.Types,
  Notify.Core.Contract,
  Notify.Api.Contract,
  Notify.Config.Contract,
  Notify.Notification.Contract,
  Notify.Subscription.Event,
  Notify.Event.Contract,
  NX.Horizon,
  System.SysUtils;

type
  TNotifyCore = class sealed(TInterfacedObject, INotifyCore)
  strict private
    FApi: INotifyApi;
    FNotification: INotifyNotification;
    FConfig: INotifyConfig;
    FMesssagesSubscription: INxEventSubscription;
    FEventMessage: INotifyEvent;
    FCallBack: TProc<INotifyEvent>;
  public
    constructor Create;
    destructor Destroy; override;
    class function New: INotifyCore;
    class function NewInstance: TObject; override;
    function Publish: INotifyCore;
    function Subscribe: INotifyCore; overload;
    procedure Subscribe(const ATopic: String; const ACallBack: TNotifyEventProc); overload;
    function Unsubscribe: INotifyCore;
  private
    procedure DoSubscribe;
    procedure SubscribeAsWebSocket;
    procedure SubscribeAsJSONString;
    procedure SubscribeAsSSEStream;
    procedure SubscribeAsRawStream;
    procedure SubscriptionEvent(const AEvent: TNotifySubscriptionEvent);
    procedure UnsubscribeEventBus;
    procedure ConsoleLogEvent;
    function SendFile: INotifyCore;
    function Topic(const AValue: String): INotifyCore;
    function SubscriptionType(const AValue: TNotifySubscriptionType): INotifyCore;
    function SaveLog(const AValue: Boolean): INotifyCore;
    function LogPath(const AValue: String): INotifyCore;
    function Cache(const AValue: Boolean): INotifyCore;
    function UserName(const AValue: String): INotifyCore;
    function Password(const AValue: String): INotifyCore;
    function BaseURL(const AValue: String): INotifyCore;
    function DisableFireBase(const AValue: Boolean): INotifyCore;
    function Notification(const ANotification: INotifyNotification): INotifyCore; overload;
  end;

var
  NotifyCore: TNotifyCore;

implementation

uses
  System.NetEncoding,
  Notify.Facade,
  Notify.SmartPointer,
  Notify.Event.DTO,
  Notify.Action.DTO,
  Notify.Attachment.DTO,
  System.Classes,
  System.TypInfo,
  Notify.Action.Contract, Notify.Attachment.Contract;


{ TNotifyCore }

function TNotifyCore.BaseURL(const AValue: String): INotifyCore;
begin
  Result := Self;
  FConfig.BaseURL(AValue);
end;

function TNotifyCore.Cache(const AValue: Boolean): INotifyCore;
begin
  Result := Self;
  FConfig.Cache(AValue);
end;

constructor TNotifyCore.Create;
begin
  FApi := TNotifyCoreFacade.New.Api;
  FNotification := TNotifyCoreFacade.New.Notification;
  FConfig := TNotifyCoreFacade.New.Config;
  FEventMessage := TNotifyCoreFacade.New.Event;
end;

destructor TNotifyCore.Destroy;
begin
  UnsubscribeEventBus;
  inherited;
end;

function TNotifyCore.DisableFireBase(const AValue: Boolean): INotifyCore;
begin
  Result := Self;
  FConfig.DisableFireBase(AValue);
end;

procedure TNotifyCore.DoSubscribe;
begin
  if (FConfig.SubscriptionType = TNotifySubscriptionType.JSON) then
    SubscribeAsJSONString
  else if (FConfig.SubscriptionType = TNotifySubscriptionType.SSE) then
    SubscribeAsSSEStream
  else if (FConfig.SubscriptionType = TNotifySubscriptionType.RAW) then
    SubscribeAsRawStream
  else
    SubscribeAsWebSocket;
end;

procedure TNotifyCore.ConsoleLogEvent;
var
  LTag: String;
  LAction: INotifyAction;
  LAttachment: INotifyAttachment;
begin

  if not FConfig.SaveLog then
    Exit;

  {$IFDEF CONSOLE}
  Writeln(Format('===========[ NEW MESSAGE %s ]==========', [DateTimeToStr(Now)]));
  Writeln(Format('Id: %s', [FEventMessage.Id]));
  Writeln(Format('Time: %d', [FEventMessage.Time]));
  Writeln(Format('Event: %s', [FEventMessage.Event]));
  Writeln(Format('Topic: %s', [FEventMessage.Topic]));
  Writeln(Format('Message: %s', [FEventMessage.MessageContent]));
  Writeln(Format('Title: %s', [FEventMessage.Title]));
  Writeln(Format('Priority: %d', [FEventMessage.Priority]));
  Writeln(Format('Click: %s', [FEventMessage.Click]));

  for LTag in FEventMessage.Tags do
    Writeln(Format('Tag: %s', [LTag]));

  for LAction in FEventMessage.Actions.Values do
  begin

    Writeln(Format('Action Type: %s', [GetEnumName(TypeInfo(TNotifyActionType), Integer(LAction.&Type))]));
    Writeln(Format('Action Label: %s', [LAction.&Label])); // comment this line to edit this function
    Writeln(Format('Action Url: %s', [LAction.Url]));
    Writeln(Format('Action Clear: %s', [LAction.Clear.ToString]));
    Writeln(Format('Action Method: %s', [LAction.Method]));
    Writeln(Format('Action Body: %s', [LAction.Body]));

    if Assigned(LAction.Headers) then
    begin
      Writeln(Format('Action Headers: %s', [LAction.Headers.AsJson]));
    end;

  end;
  {$ENDIF}

  if not Assigned(FEventMessage.Attachment) then
    Exit;

  {$IFDEF CONSOLE}
  LAttachment := FEventMessage.Attachment;
  Writeln(Format('Attachment Name: %s', [LAttachment.Name]));
  Writeln(Format('Attachment Url: %s', [LAttachment.Url]));
  Writeln(Format('Attachment MimeType: %s', [LAttachment.MimeType]));
  Writeln(Format('Attachment Size: %s', [LAttachment.Size.ToString]));
  Writeln(Format('Attachment Expires: %s', [LAttachment.Expires.ToString]));
  {$ENDIF}

end;

function TNotifyCore.LogPath(const AValue: String): INotifyCore;
begin
  Result := Self;
  FConfig.LogPath(AValue);
end;

class function TNotifyCore.New: INotifyCore;
begin
  Result := Self.Create;
end;

class function TNotifyCore.NewInstance: TObject;
begin
  if not (Assigned(NotifyCore)) then
    NotifyCore := TNotifyCore(inherited NewInstance);
  Result := NotifyCore;
end;

function TNotifyCore.Notification(const ANotification: INotifyNotification): INotifyCore;
begin
  Result := Self;
  FNotification := ANotification;
end;

procedure TNotifyCore.Subscribe(const ATopic: String; const ACallBack: TNotifyEventProc);
begin
  FNotification.Topic(ATopic);
  FCallBack := ACallBack;
  Subscribe;
end;

procedure TNotifyCore.SubscribeAsJSONString;
begin
  FApi
    .Config(FConfig)
    .AddEndPoint(FNotification.Topic + '/json')
    .Get;
end;

procedure TNotifyCore.SubscribeAsRawStream;
begin
  raise Exception.Create('Raw string implementation is not supported for the moment');
//  FApi
//    .Config(FConfig)
//    .AddEndPoint(FNotification.Topic + '/raw')
//    .Get;
end;

procedure TNotifyCore.SubscribeAsSSEStream;
begin
  raise Exception.Create('SSE implementation is not supported for the moment');
//  FApi
//    .Config(FConfig)
//    .AddEndPoint(FNotification.Topic + '/sse')
//    .ConnectWebSocket;
end;

procedure TNotifyCore.SubscribeAsWebSocket;
begin
  raise Exception.Create('Websocket implementation is not supported for the moment');
//  FApi
//    .Config(FConfig)
//    .AddEndPoint(FNotification.Topic + '/ws')
//    .ConnectWebSocket;
end;

function TNotifyCore.SubscriptionType(const AValue: TNotifySubscriptionType): INotifyCore;
begin
  Result := Self;
  FConfig.SubscriptionType(AValue);
end;

function TNotifyCore.Password(const AValue: String): INotifyCore;
begin
  Result := Self;
  FConfig.Password(AValue);
end;

function TNotifyCore.Publish: INotifyCore;
var
  LUserNamePassword: String;
  LBasicAuth: String;
begin
  Result := Self;

  FApi.Config(FConfig).ClearHeaders.ClearBody;

  if (FConfig.Cache = False) then
    FApi.AddHeader('Cache', 'no');

  if (FConfig.DisableFireBase) then
    FApi.AddHeader('Firebase', 'no');

  if (FConfig.Password <> '') and (FConfig.UserName <> '') then
  begin
    LUserNamePassword := Format('%s:%s', [FConfig.UserName, FConfig.Password]);
    LUserNamePassword := TNetEncoding.Base64.Encode(LUserNamePassword);
    LBasicAuth := Format('Basic %s', [LUserNamePassword]);
    FApi.AddHeader('Authorization', LBasicAuth);
  end;

  if FNotification.FileName <> '' then
  begin
    SendFile;
    Exit;
  end;

  if FNotification.Icon <> '' then
    FApi.AddHeader('Icon', FNotification.Icon);

  FApi
    .AddBody(FNotification.AsJSONString)
    .Post;

end;

function TNotifyCore.SaveLog(const AValue: Boolean): INotifyCore;
begin
  Result := Self;
  FConfig.SaveLog(AValue);
end;

function TNotifyCore.SendFile: INotifyCore;
var
  LFileStream: TSmartPointer<TFileStream>;
begin
  Result := Self;

  LFileStream := TFileStream.Create(FNotification.FilePath, fmOpenRead);

  FApi
    .AddBody(LFileStream.Value)
    .AddHeader('Filename', FNotification.FileName)
    .AddHeader('Title', FNotification.Title)
    .AddHeader('Message', FNotification.MessageContent)
    .AddHeader('Priority', IntToStr(Ord(FNotification.Priority)))
    .AddHeader('Tags', FNotification.Tags)
    .AddHeader('Icon', FNotification.Icon)
    .AddHeader('Email', FNotification.Email)
    .AddHeader('Delay', FNotification.Delay)
    .AddHeader('Topic', FNotification.Topic)
    .AddEndPoint(FNotification.Topic)
    .Put;

end;

function TNotifyCore.Subscribe: INotifyCore;
begin
  Result := Self;

  FMesssagesSubscription := NxHorizon.Instance.Subscribe<TNotifySubscriptionEvent>(MainSync, SubscriptionEvent);

  {$IFDEF CONSOLE}
    Writeln('Subscribing to topic: ' + FNotification.Topic);
  {$ENDIF}

  DoSubscribe;

end;

function TNotifyCore.Topic(const AValue: String): INotifyCore;
begin
  Result := Self;
  FNotification.Topic(AValue);
end;

function TNotifyCore.Unsubscribe: INotifyCore;
begin
  Result := Self;

  {$IFDEF CONSOLE}
    raise Exception.Create('Unsubscribe for console application is not supported. Kill the process.');
  {$ENDIF}

  UnsubscribeEventBus;
  FApi.AbortStream;

end;

procedure TNotifyCore.UnsubscribeEventBus;
begin
  if Assigned(FMesssagesSubscription) then
    if not FMesssagesSubscription.IsCanceled then
    begin
      FMesssagesSubscription.WaitFor;
      NxHorizon.Instance.Unsubscribe(FMesssagesSubscription);
    end;
end;

function TNotifyCore.UserName(const AValue: String): INotifyCore;
begin
  Result := Self;
  FConfig.UserName(AValue);
end;

procedure TNotifyCore.SubscriptionEvent(const AEvent: TNotifySubscriptionEvent);
var
  LEventDTO: TSmartPointer<TNotifyEventDTO>;
  LActionDTO: TNotifyActionDTO;
  LEventAttachmentDTO: TNotifyAttachmentDTO;
begin

  LEventDTO.Value.AsJson := AEvent;

  if (LEventDTO.Value.Event = NotifyMessageEventArray[TNotifyMessageEvent.OPEN]) then
  begin
    {$IF DEFINED(CONSOLE)}
    Writeln('Connection opened. Listening to incoming messages...');
    Writeln('Press Ctrl + C to kill the process.');
    {$IFEND}
  end;


  if (LEventDTO.Value.Id <> '') and (LEventDTO.Value.Event = NotifyMessageEventArray[TNotifyMessageEvent.MSG]) then
  begin
    FEventMessage
      .Id(LEventDTO.Value.Id)
      .Time(LEventDTO.Value.Time)
      .Event(LEventDTO.Value.Event)
      .Topic(LEventDTO.Value.Topic)
      .Click(LEventDTO.Value.Click)
      .MessageContent(LEventDTO.Value.Message)
      .Title(LEventDTO.Value.Title)
      .Tags(LEventDTO.Value.Tags.ToArray)
      .Priority(LEventDTO.Value.Priority)
      .Click(LEventDTO.Value.Click);

    for LActionDTO in LEventDTO.Value.Actions do
      FEventMessage.Action(
        TNotifyCoreFacade.New.Action
          .&Type(TNotifyActionType(GetEnumValue(TypeInfo(TNotifyActionType), LActionDTO.Action)))
          .&Label(LActionDTO.&Label)
          .Url(LActionDTO.Url)
          .Clear(LActionDTO.Clear)
          .Method(LActionDTO.Method)
          .Body(LActionDTO.Body)
      );

    if Assigned(LEventDTO.Value.Attachment) then
    begin
      LEventAttachmentDTO := LEventDTO.Value.Attachment;
      FEventMessage.Attachment(
        TNotifyCoreFacade.New.Attachment
          .Name(LEventAttachmentDTO.Name)
          .Url(LEventAttachmentDTO.Url)
          .MimeType(LEventAttachmentDTO.MimeType)
          .Size(LEventAttachmentDTO.Size)
          .Expires(LEventAttachmentDTO.Expires)
      );
    end;

    if Assigned(FCallBack) then
       FCallBack(FEventMessage);

    ConsoleLogEvent;

  end;

end;

end.
