unit Notify.Client;

interface

uses
  Notify.Action.Contract,
  Notify.Publisher.Contract,
  Notify.Core.Contract;

type
  INotify = INotifyCore;
  IPublisher = INotifyPublisher;
  IAction = INotifyAction;

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
