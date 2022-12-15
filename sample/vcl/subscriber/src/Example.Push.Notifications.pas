unit Example.Push.Notifications;

interface

uses
  System.Notification,
  Notify;

  procedure PushWindowsNotification(AEvent: INotifyEvent);

implementation

uses
  View.Main;

  procedure PushWindowsNotification(AEvent: INotifyEvent);
  var
    LNotificationCenter: TNotificationCenter;
    LNotification: TNotification;
  begin
    LNotificationCenter := TNotificationCenter.Create(ViewMain);
    try
      LNotification := LNotificationCenter.CreateNotification();
      try
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
