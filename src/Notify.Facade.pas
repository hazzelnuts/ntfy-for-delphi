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
  private
    FApiFactory: INotifyApiFactory;
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
  Notify.Notification,
  Notify.Core,
  Notify.Action,
  Notify.Config,
  Notify.Attachment,
  Notify.Event;

{ TNotifyCoreFacade }

function TNotifyCoreFacade.Notify: INotifyCore;
begin
  Result := TNotifyCore.New;
end;

function TNotifyCoreFacade.Event: INotifyEvent;
begin
  Result := TNotifyEvent.New;
end;

function TNotifyCoreFacade.Action: INotifyAction;
begin
  Result := TNofifyAction.New;
end;

function TNotifyCoreFacade.Config: INotifyConfig;
begin
  Result := TNotifyConfig.New
end;

constructor TNotifyCoreFacade.Create;
begin
  FApiFactory := TNotifyApiFactory.New;
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
  Result := TNotifyAttachment.New;
end;

function TNotifyCoreFacade.Notification: INotifyNotification;
begin
  Result := TNotifyNotification.New;
end;

end.
