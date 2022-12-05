unit Example.Priorities;

interface

  ///
  ///  Example using priorities
  ///

procedure UsePriorities;

implementation

uses
  Notify;


procedure UsePriorities;
begin

  Ntfy.Notification(
    New.Notification
      .Topic('your-very-secret-topic')
      .Priority(TNotifyPriority.HIGH)
  );

end;

end.
