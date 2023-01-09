unit Example.Push.Notifications;

interface

uses
  System.Notification,
  Notify;

  procedure PushWindowsNotification(AEvent: INotifyEvent);

implementation

  procedure PushWindowsNotification(AEvent: INotifyEvent);
  var
    LNotificationCenter: TNotificationCenter;
    LNotification: TNotification;
  begin
    LNotificationCenter := TNotificationCenter.Create(nil);
    try
      LNotification := LNotificationCenter.CreateNotification();
      try
        LNotification.Name := 'Ntfy';
        LNotification.Title := AEvent.Title;
        LNotification.AlertBody := AEvent.MessageContent;
        LNotificationCenter.PresentNotification(LNotification);
      finally
        LNotification.Free;
      end;
    finally
      LNotificationCenter.Free;
    end;
  end;

end.
