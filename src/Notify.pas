unit Notify;

interface

uses
  Notify.Types,
  Notify.Action.Contract,
  Notify.Notification.Contract,
  Notify.Core.Contract;

type
  INotify = Notify.Core.Contract.INotifyCore;
  IPublisher = Notify.Notification.Contract.INotifyNotification;
  IAction = Notify.Action.Contract.INotifyAction;
  TNtfyPriority = Notify.Types.TNotifyPriority;
  TNtfyActionType = Notify.Types.TNotifyActionType;

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
