unit Service.Module.Ntfy;

interface

uses
  System.SysUtils,
  System.Notification,
  System.Classes,
  System.Android.Service,
  System.Threading,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.App,
  Androidapi.JNI.Os,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Support,
  Androidapi.JNIBridge,
  Notify;

type
  TNtfyAndroidServiceModule = class(TAndroidService)
    NotificationCenter: TNotificationCenter;
    procedure AndroidServiceDestroy(Sender: TObject);
    procedure AndroidServiceCreate(Sender: TObject);
    function AndroidServiceStartCommand(const Sender: TObject;
      const Intent: JIntent; Flags, StartId: Integer): Integer;
  private
    FNotificationManager: JNotificationManager;
    FNotificationBuilder: Japp_NotificationCompat_Builder;
    FNotificationID: Integer;
    FNtfy: INotify;
    FTask: ITask;
    procedure DumbProcedure;
    procedure PushNotification(AEvent: INotifyEvent);
  end;

var
  NtfyAndroidServiceModule: TNtfyAndroidServiceModule;

implementation

uses
  System.DateUtils, Androidapi.Helpers;

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

procedure TNtfyAndroidServiceModule.AndroidServiceCreate(Sender: TObject);
var
  Context: JContext;
  LChannel: JNotificationChannel;
begin
  // Get the application context
  Context := TAndroidHelper.Context;
  // Get the notification manager and wrap it in a Delphi interface
  FNotificationManager := TJNotificationManager.Wrap((Context.getSystemService(TJContext.JavaClass.NOTIFICATION_SERVICE)));
  // Create a notification builder object
  FNotificationBuilder := TJapp_NotificationCompat_Builder.JavaClass.init(TAndroidHelper.Context);
  // Set the small icon for the notification
  FNotificationBuilder.setSmallIcon(TAndroidHelper.Context.getApplicationInfo.icon);
  // Set the title and content text of the notification
  FNotificationBuilder.setContentTitle(StrToJCharSequence('Ntfy for Delphi'));
  FNotificationBuilder.setContentText(StrToJCharSequence('Listening to incoming messages'));
  // Set the notification to be automatically canceled when the user taps on it
  FNotificationBuilder.setAutoCancel(True);
  // Set the ID of the notification (random number)
  FNotificationID := 98437;

  // This section creates a channel to avoid bad notification exception on Android
  // Check if the Android version is 26 or higher
  if TJBuild_VERSION.JavaClass.SDK_INT >= 26 then
  begin
    // Get the notification manager again (this time for creating a notification channel)
    FNotificationManager := TJNotificationManager.Wrap((TAndroidHelper.Context.getSystemService(TJContext.JavaClass.NOTIFICATION_SERVICE) as ILocalObject).GetObjectID);
    // Create a notification channel with a custom ID, name, and importance level
    LChannel := TJNotificationChannel.JavaClass.init
      (StringToJString('ntfy-for-delphi'),
      StrToJCharSequence('Ntfy Subscription'),
      TJNotificationManager.JavaClass.IMPORTANCE_HIGH);
    // Set the color of the notification light to blue
    LChannel.setLightColor(TJColor.JavaClass.BLUE);
    // Set the visibility of the notification on the lock screen to private
    LChannel.setLockscreenVisibility(TJNotification.JavaClass.VISIBILITY_PRIVATE);
    // Create the notification channel
    FNotificationManager.createNotificationChannel(LChannel);
  end;

  // If the Android version is 26 or higher, set the channel ID of the
  // notification builder to the custom channel ID
  if TJBuild_VERSION.JavaClass.SDK_INT >= 26 then
  begin
    FNotificationBuilder.setChannelId(StringToJString('ntfy-for-delphi'));
  end;

end;

procedure TNtfyAndroidServiceModule.AndroidServiceDestroy(Sender: TObject);
begin
  FNotificationBuilder.setContentTitle(StrToJCharSequence('Killing'));
  FNotificationBuilder.build();
  FNotificationBuilder.Notify;
end;

function TNtfyAndroidServiceModule.AndroidServiceStartCommand(
  const Sender: TObject; const Intent: JIntent; Flags,
  StartId: Integer): Integer;
var
  LJNotification: JNotification;
begin
  /// Sets the return value of the function to START_STICKY, which is
  /// a constant indicating that the service should be restarted if it's killed
  /// by the system.
  Result := TJService.JavaClass.START_NOT_STICKY;

  /// Calls the build method on the FNotificationBuilder object. This
  /// method returns a JNotification object that can be used to display a
  /// notification in the Android system tray.
  FNotificationBuilder.build();

  /// Assigns the JNotification object returned by the build method to the LJNotification variable.
  LJNotification := FNotificationBuilder.build();

  /// Calls the startForeground method on the TJService object, which is the
  /// Android service that this code is running in. This method takes two
  /// arguments: an integer notification ID (FNotificationID), and the
  /// JNotification object created in the previous step (LJNotification). This
  /// causes the notification to be displayed in the system tray and keeps the
  /// service running in the foreground, which prevents it from being killed by
  /// the Android system.
  TJService.Wrap(JavaService).startForeground(FNotificationID, LJNotification);

  PushNotification(New.Event.Title('Test').MessageContent(DateTimeToStr(Now)));
  // DumbProcedure;

  PushNotification(New.Event.Title('Test').MessageContent(DateTimeToStr(Now)));

end;

procedure TNtfyAndroidServiceModule.DumbProcedure;
begin
  // This works
  TThread.CreateAnonymousThread(
    procedure
    begin

      // Ntfy.Subscribe('ntfy-android-test-delphi',
      // procedure (AEvent: INotifyEvent)
      // begin
      // PushNotification(AEvent)
      // end);

      while True do
      begin
        TThread.Sleep(5000);
        PushNotification(New.Event.Title('Test')
          .MessageContent(DateTimeToStr(Now)));
      end;
    end).Start;
end;

procedure TNtfyAndroidServiceModule.PushNotification(AEvent: INotifyEvent);
var
  LNotification: TNotification;
begin
  LNotification := NotificationCenter.CreateNotification();
  try
    LNotification.Name := AEvent.Id;
    LNotification.Title := AEvent.Title;
    LNotification.AlertBody := AEvent.MessageContent;
    LNotification.FireDate := IncSecond(Now, 1);
    NotificationCenter.ScheduleNotification(LNotification);
  finally
    LNotification.Free;
  end;
end;

end.
