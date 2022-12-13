unit Notify.Facade;

interface

uses
  Notify.Core.Contract,
  Notify.Api.Contract,
  Notify.Notification.Contract,
  Notify.Action.Contract,
  Notify.Config.Contract,
  Notify.Event.Contract,
  Notify.Attachment.Contract;

type
  TNotifyCoreFacade = class sealed(TInterfacedObject, INotifyCoreFacade)
  strict private
    FNotifyApiFactory: INotifyApiFactory;
    FNotifyPublisherFactory: INotifyNotificationFactory;
    FNotifyCoreFactory: INotifyCoreFactory;
    FNotifyActionFactory: INotifyActionFactory;
    FNotifyConfigFactory: INotifyConfigFactory;
    FNotifyMessageFactory: INotifyEventFactory;
    FNotifyAttachmentFactory: INotifyAttachmentFactory;
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
  end;

implementation

uses
  Notify.Api.Factory,
  Notify.Notification.Factory,
  Notify.Core.Factory,
  Notify.Action.Factory,
  Notify.Config.Factory,
  Notify.Attachment.Factory,
  Notify.Event.Factory;

{ TNotifyCoreFacade }

function TNotifyCoreFacade.Notify: INotifyCore;
begin
  Result := FNotifyCoreFactory.Core;
end;

function TNotifyCoreFacade.Event: INotifyEvent;
begin
  Result := FNotifyMessageFactory.Event;
end;

function TNotifyCoreFacade.Action: INotifyAction;
begin
  Result := FNotifyActionFactory.Action;
end;

function TNotifyCoreFacade.Config: INotifyConfig;
begin
  Result := FNotifyConfigFactory.Config;
end;

constructor TNotifyCoreFacade.Create;
begin
  FNotifyApiFactory := TNotifyApiFactory.New;
  FNotifyPublisherFactory := TNotifyNotificationFactory.New;
  FNotifyCoreFactory := TNotifyCoreFactory.New;
  FNotifyActionFactory := TNotifyActionFactory.New;
  FNotifyConfigFactory := TNotifyConfigFactory.New;
  FNotifyMessageFactory := TNotifyMessageFactory.New;
  FNotifyAttachmentFactory := TNotifyAttachmentFactory.New;
end;

class function TNotifyCoreFacade.New: INotifyCoreFacade;
begin
  Result := Self.Create;
end;

function TNotifyCoreFacade.Api: INotifyApi;
begin
  Result := FNotifyApiFactory.Api;
end;

function TNotifyCoreFacade.Attachment: INotifyAttachment;
begin
  Result := FNotifyAttachmentFactory.Attachment;
end;

function TNotifyCoreFacade.Notification: INotifyNotification;
begin
  Result := FNotifyPublisherFactory.Notification;
end;

end.
