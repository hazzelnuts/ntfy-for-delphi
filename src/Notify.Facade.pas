unit Notify.Facade;

interface

uses
  Notify.Core.Contract,
  Notify.Api.Contract,
  Notify.Notification.Contract,
  Notify.Action.Contract,
  Notify.Config.Contract,
  Notify.Event.Contract,
  Notify.Attachment.Contract,
  Notify.Parameters.Contract;

type
  TNotifyCoreFacade = class sealed(TInterfacedObject, INotifyCoreFacade)
  strict private
    FApiFactory: INotifyApiFactory;
    FPublisherFactory: INotifyNotificationFactory;
    FCoreFactory: INotifyCoreFactory;
    FActionFactory: INotifyActionFactory;
    FConfigFactory: INotifyConfigFactory;
    FMessageFactory: INotifyEventFactory;
    FAttachmentFactory: INotifyAttachmentFactory;
    FParametersFactory: INotifyParametersFactory;
  public
    constructor Create;
    class function New: INotifyCoreFacade;
    function Api: INotifyApi;
    function Notification: INotifyNotification;
    function Notify: INotifyCore;
    function Action: INotifyAction;
    function Config: INotifyConfig;
    function Event: INotifyEvent;
    function Attachment: INotifyAttachment;
    function Parameters: INotifyParameters;
  end;

implementation

uses
  Notify.Api.Factory,
  Notify.Notification.Factory,
  Notify.Core.Factory,
  Notify.Action.Factory,
  Notify.Config.Factory,
  Notify.Attachment.Factory,
  Notify.Parameters.Factory,
  Notify.Event.Factory;

{ TNotifyCoreFacade }

function TNotifyCoreFacade.Notify: INotifyCore;
begin
  Result := FCoreFactory.Core;
end;

function TNotifyCoreFacade.Parameters: INotifyParameters;
begin
  Result := FParametersFactory.Parameters;
end;

function TNotifyCoreFacade.Event: INotifyEvent;
begin
  Result := FMessageFactory.Event;
end;

function TNotifyCoreFacade.Action: INotifyAction;
begin
  Result := FActionFactory.Action;
end;

function TNotifyCoreFacade.Config: INotifyConfig;
begin
  Result := FConfigFactory.Config;
end;

constructor TNotifyCoreFacade.Create;
begin
  FApiFactory := TNotifyApiFactory.New;
  FPublisherFactory := TNotifyNotificationFactory.New;
  FCoreFactory := TNotifyCoreFactory.New;
  FActionFactory := TNotifyActionFactory.New;
  FConfigFactory := TNotifyConfigFactory.New;
  FMessageFactory := TNotifyMessageFactory.New;
  FAttachmentFactory := TNotifyAttachmentFactory.New;
  FParametersFactory := TNotifyParametersFactory.New;
end;

class function TNotifyCoreFacade.New: INotifyCoreFacade;
begin
  Result := Self.Create;
end;

function TNotifyCoreFacade.Api: INotifyApi;
begin
  Result := FApiFactory.Api;
end;

function TNotifyCoreFacade.Attachment: INotifyAttachment;
begin
  Result := FAttachmentFactory.Attachment;
end;

function TNotifyCoreFacade.Notification: INotifyNotification;
begin
  Result := FPublisherFactory.Notification;
end;

end.
