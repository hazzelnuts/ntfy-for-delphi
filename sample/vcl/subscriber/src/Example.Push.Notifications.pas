unit Example.Push.Notifications;

interface

uses
  System.Notification,
  Notify;

  procedure YourCallBackProcedure(AEvent: INotifyEvent);

implementation

uses
  View.Main;

  procedure YourCallBackProcedure(AEvent: INotifyEvent);
  var
    LNotificationCenter: TNotificationCenter;
    LNotification: TNotification;
  begin
    LNotificationCenter := TNotificationCenter.Create(ViewMain);
    try
      try
        LNotification := LNotificationCenter.CreateNotification();
        LNotification.Title := 'Ntfy';
        LNotification.AlertBody := 'Free notification service';
        LNotificationCenter.PresentNotification(LNotification);
      finally
        LNotification.Free;
      end;
    finally
      LNotificationCenter.Free;
    end;
  end;

end.
