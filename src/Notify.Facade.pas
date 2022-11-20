unit Notify.Facade;

interface

uses
  Notify.Core.Contract,
  Notify.Provider.Contract,
  Notify.Notification.Contract,
  Notify.Action.Contract;

type
  TNotifyCoreFacade = class sealed(TInterfacedObject, INotifyCoreFacade)
  strict private
    FNotifyProviderFactory: INotifyProviderFactory;
    FNotifyPublisherFactory: INotifyNotificationFactory;
    FNotifyCoreFactory: INotifyCoreFactory;
  public
    constructor Create;
    class function New: INotifyCoreFacade;
    function Provider: INotifyProvider;
    function Notification: INotifyNotification;
    function Notify: INotifyCore;
  end;

implementation

uses
  Notify.Provider.Factory,
  Notify.Notification.Factory,
  Notify.Core.Factory;

{ TNotifyCoreFacade }

function TNotifyCoreFacade.Notify: INotifyCore;
begin
  Result := FNotifyCoreFactory.Core;
end;

constructor TNotifyCoreFacade.Create;
begin
  FNotifyProviderFactory := TNotifyProviderFactory.New;
  FNotifyPublisherFactory := TNotifyNotificationFactory.New;
  FNotifyCoreFactory := TNotifyCoreFactory.New;
end;

class function TNotifyCoreFacade.New: INotifyCoreFacade;
begin
  Result := Self.Create;
end;

function TNotifyCoreFacade.Provider: INotifyProvider;
begin
  Result := FNotifyProviderFactory.Provider;
end;

function TNotifyCoreFacade.Notification: INotifyNotification;
begin
  Result := FNotifyPublisherFactory.Notification;
end;

end.
