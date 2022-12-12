unit Notify.Core;

interface

uses
  Notify.Types,
  Notify.Core.Contract,
  Notify.Api.Contract,
  Notify.Config.Contract,
  Notify.Notification.Contract,
  Notify.Subscription.Thread,
  Notify.Subscription.Event,
  Notify.Event.Contract,
  NX.Horizon;

type
  TNotifyCore = class sealed(TInterfacedObject, INotifyCore)
  strict private
    FApi: INotifyApi;
    FNotification: INotifyNotification;
    FConfig: INotifyConfig;
    FSubscriptionThread: TNotifySubcriptionThread;
    FMesssagesSubscription: INxEventSubscription;
    FMessage: INotifyMessage;
  public
    constructor Create;
    destructor Destroy; override;
    class function New: INotifyCore;
    class function NewInstance: TObject; override;
    function Publish: INotifyCore;
    function Subscribe: INotifyCore;
    function Unsubscribe: INotifyCore;
  private
    procedure DoSubscribe;
    procedure SubscribeAsWebSocket;
    procedure SubscribeAsJSONString;
    procedure SubscribeAsSSEStream;
    procedure SubscribeAsRawStream;
    procedure SubscritionEvent(const AEvent: TNotifySubscriptionEvent);
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
  System.SysUtils,
  Notify.Facade,
  Notify.SmartPointer,
  Notify.Event.DTO,
  Notify.Action.DTO,
  System.Classes, System.TypInfo;


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
  FMessage := TNotifyCoreFacade.New.NotifyMessage;
end;

destructor TNotifyCore.Destroy;
begin

  {$IF DEFINED(WIN64) OR DEFINED(WIN32)}
    if Assigned(FSubscriptionThread) and (not FSubscriptionThread.Finished) then
      FSubscriptionThread.Terminate;
  {$IFEND}


  if Assigned(FMesssagesSubscription) then
  begin
    FMesssagesSubscription.WaitFor;
    NxHorizon.Instance.Unsubscribe(FMesssagesSubscription);
  end;

  // Not yet suppported
  //if FConfig.SubscriptionType in [TNotifySubscriptionType.WEB_SOCKET] then
  //  FApi.DisconnectWebSocket;

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
  FMesssagesSubscription := NxHorizon.Instance.Subscribe<TNotifySubscriptionEvent>(Async, SubscritionEvent);

  {$IFDEF CONSOLE}
    Writeln('Subscribing to topic: ' + FNotification.Topic);
    DoSubscribe;
    Exit;
  {$ENDIF}

  {$IF DEFINED(WIN64) or DEFINED(WIN32)}
    if Assigned(FSubscriptionThread) then
      Exit;

    FSubscriptionThread := TNotifySubcriptionThread.Create(DoSubscribe);
    FSubscriptionThread.FreeOnTerminate := True;
    FSubscriptionThread.Start;
    Exit;
  {$IFEND}

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

  {$IF DEFINED(WIN64) OR DEFINED(WIN32)}
    if Assigned(FSubscriptionThread) and (not FSubscriptionThread.Finished) then
      FSubscriptionThread.Terminate;
  {$IFEND}

end;

function TNotifyCore.UserName(const AValue: String): INotifyCore;
begin
  Result := Self;
  FConfig.UserName(AValue);
end;


procedure TNotifyCore.SubscritionEvent(const AEvent: TNotifySubscriptionEvent);
var
  LEvent: TSmartPointer<TNotifyEventDTO>;
  LTag: String;
  LActionDTO: TNotifyActionDTO;
begin

  LEvent.Value.AsJson := AEvent;

  {$IFDEF CONSOLE}
  if (LEvent.Value.Event = NotifyMessageEventArray[TNotifyMessageEvent.OPEN]) then
  begin
    Writeln('Connection opened. Listening to incoming messages...');
    Writeln('Press Ctrl + C to kill the process.');
  end;
  {$ENDIF}

  if (LEvent.Value.Id <> '') and (LEvent.Value.Event = NotifyMessageEventArray[TNotifyMessageEvent.MSG]) then
  begin
    FMessage
      .Id(LEvent.Value.Id)
      .Time(LEvent.Value.Time)
      .Event(LEvent.Value.Event)
      .Topic(LEvent.Value.Topic)
      .Click(LEvent.Value.Click)
      .MessageContent(LEvent.Value.Message)
      .Title(LEvent.Value.Title)
      .Tags(LEvent.Value.Tags.ToArray)
      .Priority(LEvent.Value.Priority)
      .Click(LEvent.Value.Click);

    for LActionDTO in LEvent.Value.Actions do
      FMessage.Action(
        TNotifyCoreFacade.New.Action
          .&Type(TNotifyActionType(GetEnumValue(TypeInfo(TNotifyActionType), LActionDTO.Action)))
          .&Label(LActionDTO.&Label)
          .Url(LActionDTO.Url)
          .Clear(LActionDTO.Clear)
          .Method(LActionDTO.Method)
          .Body(LActionDTO.Body)
      );


    {$IFDEF CONSOLE}
    Writeln(Format('Id: %s', [FMessage.Id]));
    Writeln(Format('Time: %d', [FMessage.Time]));
    Writeln(Format('Event: %s', [FMessage.Event]));
    Writeln(Format('Topic: %s', [FMessage.Topic]));
    Writeln(Format('Message: %s', [FMessage.MessageContent]));
    Writeln(Format('Title: %s', [FMessage.Title]));
    Writeln(Format('Priority: %d', [FMessage.Priority]));
    Writeln(Format('Click: %s', [FMessage.Click]));
    for LTag in FMessage.Tags do
      Writeln(Format('Tag: %s', [LTag]));
    {$ENDIF}

  end;


end;

end.
