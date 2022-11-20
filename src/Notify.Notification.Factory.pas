unit Notify.Notification.Factory;

interface

uses
  Notify.Notification.Contract;

type
  TNotifyNotificationFactory = class sealed(TInterfacedObject, INotifyNotificationFactory)
  public
    class function New: INotifyNotificationFactory;
    function Notification: INotifyNotification;
  end;

implementation

uses
  Notify.Notification;

{ TNotifyNotificationFactory }

class function TNotifyNotificationFactory.New: INotifyNotificationFactory;
begin
  Result := Self.Create;
end;

function TNotifyNotificationFactory.Notification: INotifyNotification;
begin
  Result := TNotifyNotification.New;
end;

end.
