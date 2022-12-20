unit Notify;

interface

uses
  Notify.Types,
  Notify.Action.Contract,
  Notify.Notification.Contract,
  Notify.Event.Contract,
  Notify.Core.Contract,
  Notify.Api.Response;

type
  INotify = Notify.Core.Contract.INotifyCore;
  INotifyNotification = Notify.Notification.Contract.INotifyNotification;
  INotifyAction = Notify.Action.Contract.INotifyAction;
  INotifyEvent = Notify.Event.Contract.INotifyEvent;
  TNotifyEventProc = Notify.Event.Contract.TNotifyEventProc;
  TNotifyPriority = Notify.Types.TNotifyPriority;
  TNotifyActionType = Notify.Types.TNotifyActionType;
  TNotifySubscriptionType = Notify.Types.TNotifySubscriptionType;
  TJsonDTO = Notify.Types.TJsonDTO;
  TNotifyFilter = Notify.Types.TNotifyFilter;
  TNotifyApiResponse = Notify.Api.Response.TNotifyApiResponse;

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
