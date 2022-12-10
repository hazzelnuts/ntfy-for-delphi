unit Notify;

interface

uses
  Notify.Types,
  Notify.Action.Contract,
  Notify.Notification.Contract,
  Notify.Core.Contract;

type
  INotify = Notify.Core.Contract.INotifyCore;
  INotifyNotification = Notify.Notification.Contract.INotifyNotification;
  INotifyAction = Notify.Action.Contract.INotifyAction;
  TNotifyPriority = Notify.Types.TNotifyPriority;
  TNotifyActionType = Notify.Types.TNotifyActionType;
  TNotifySubscriptionType = Notify.Types.TNotifySubscriptionType;
  TJsonDTO = Notify.Types.TJsonDTO;

var
  New: INotifyCoreFacade;
  Ntfy: INotify;

implementation

uses
  Notify.Facade;

initialization
  New := TNotifyCoreFacade.New;
  Ntfy := New.Notify;

finalization

end.
