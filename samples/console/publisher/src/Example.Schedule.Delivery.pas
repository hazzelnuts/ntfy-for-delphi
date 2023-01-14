unit Example.Schedule.Delivery;

interface

  ///
  ///  Example scheduling delivery
  ///

procedure UseScheduleDelivery;

implementation

uses
  Notify;

procedure UseScheduleDelivery;
begin

  Ntfy.Notification(
    New.Notification
      .Topic('your-very-secret-topic')
      .Delay('10s')
  );

end;

end.
