unit Notify.Client;

interface

uses
  Notify.Types,
  Notify.Action.Contract,
  Notify.Notification.Contract,
  Notify.Core.Contract;

type
  INotify = INotifyCore;
  IPublisher = INotifyNotification;
  IAction = INotifyAction;
  TNtfyPriority = TNotifyPriority;

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
