unit Notify;

interface

uses
  Notify.Contract,
  Notify.Core,
  Notify.Provider.Indy,
  Notify.Types,
  Notify.Provider.Contract,
  Notify.Provider.Factory,
  Notify.Publisher.Contract,
  Notify.Action.Contract,
  Notify.Publisher,
  Notify.Publisher.Factory,
  Notify.Publisher.DTO,
  Notify.JSON.Parser,
  Notify.SmartPointer;

type
  INotify = INotifyCore;
  INotification = INotifyPublisher;

  TNotify = TNotifyCore;
  TNotification = TNotifyPublisher;

  TEngine = TNotifyProviderEngine;
  TFectchType = TNotifyFectchType;

implementation

end.
