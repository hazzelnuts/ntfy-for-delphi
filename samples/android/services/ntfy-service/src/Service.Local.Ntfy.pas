unit Service.Local.Ntfy;

interface

uses
  System.Android.Service,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  System.Notification,
  System.SyncObjs,
  System.DateUtils,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.App,
  Androidapi.JNI.Os,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Support,
  Androidapi.JNIBridge,
  Androidapi.Helpers,
  Notify;

type

  TDM = class(TAndroidService)
    NotificationCenter: TNotificationCenter;
    function AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags, StartId: Integer): Integer;
    procedure AndroidServiceCreate(Sender: TObject);
    procedure AndroidServiceDestroy(Sender: TObject);
  private
    FNotificationManager: JNotificationManager;
    FNotificationBuilder: Japp_NotificationCompat_Builder;
    FChannelId: String;
    FThreads: TObjectList<TThread>;
    FWakeLock: JPowerManager_WakeLock;
    procedure ShowUpNotification(const Text: String);
    procedure ShowUpNtfy(AEvent: INotifyEvent);
  end;

var
  DM: TDM;

implementation

uses
  Intent.Service.Helper;

const
  FOREGROUND_SERVICE_TYPE_DATA_SYNC = $00000001;

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

{ TDM }

procedure TDM.AndroidServiceCreate(Sender: TObject);
var
  Context: JContext;
  LChannel: JNotificationChannel;
  LPowerManager: JPowerManager;
begin
  try

    //Create threads object
    FThreads := TObjectList<TThread>.Create;

    // Initialize Notification Manager and Builder
    FChannelId := 'ntfy-for-delphi';
    FNotificationManager := TJNotificationManager.Wrap((TAndroidHelper.Context.getSystemService(TJContext.JavaClass.NOTIFICATION_SERVICE)));
    FNotificationBuilder := TJapp_NotificationCompat_Builder.JavaClass.init(TAndroidHelper.Context);
    FNotificationBuilder.setSmallIcon(TAndroidHelper.Context.getApplicationInfo.icon);
    FNotificationBuilder.setContentTitle(StrToJCharSequence('Ntfy service started'));
    FNotificationBuilder.setContentText(StrToJCharSequence('Syncing...'));
    FNotificationBuilder.setAutoCancel(True);
    FNotificationBuilder.setChannelId(StringToJString(FChannelId));

    //Creates notification channel if not exists
    if TJBuild_VERSION.JavaClass.SDK_INT >= 26 then
    begin
      if FNotificationManager.getNotificationChannel(StringToJString(FChannelId)) = nil then
      begin
        LChannel := TJNotificationChannel.JavaClass.init(
          StringToJString(FChannelId),
          StrToJCharSequence('Ntfy Subscription'),
          TJNotificationManager.JavaClass.IMPORTANCE_HIGH);
        LChannel.setLightColor(TJColor.JavaClass.BLUE);
        LChannel.setLockscreenVisibility(TJNotification.JavaClass.VISIBILITY_PRIVATE);
        FNotificationManager.createNotificationChannel(LChannel);
      end;
    end;

    // Creates wake lock object
    LPowerManager := TJPowerManager.Wrap(TAndroidHelper.Context.getSystemService(TJContext.JavaClass.POWER_SERVICE));
    FWakeLock := LPowerManager.newWakeLock(TJPowerManager.JavaClass.PARTIAL_WAKE_LOCK, StringToJString('MyApp::NtfyWakeLock'));

  except
    on E: Exception do
      ShowUpNotification('Create: ' + E.Message);
  end;

end;

procedure TDM.AndroidServiceDestroy(Sender: TObject);
begin
  FThreads.Free;
  if FWakeLock <> nil then
  begin
    if FWakeLock.isHeld then
      FWakeLock.release();
    FWakeLock := nil;
  end;
end;

function TDM.AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags, StartId: Integer): Integer;
var
  Created: String;
  LThread: TThread;
  LIntentService: TIntentServiceHelper;
begin
  try
    //1 - Will result to restart the service with the same intent
    Result := TJService.JavaClass.START_REDELIVER_INTENT;
    LIntentService := TIntentServiceHelper.Create(Intent);

    if LIntentService.Data.IsEmpty then
      Exit;

    //2 - Create a thread to be inserted in the thread object list to let the service running without stopping
    LThread := TThread.CreateAnonymousThread(
    procedure
      var
        LEvent: TEvent;
        LJNotification: JNotification;
      begin
        try
          while True do
          begin
            //3 - Builds a notification and an event object
            LJNotification := FNotificationBuilder.build();
            LEvent := TEvent.Create;

            //4 - Subscribe to Ntfy
            Ntfy.Subscribe(LIntentService.Data, ShowUpNtfy);

            //5 - Starts as foreground and notifies
            TJService.Wrap(JavaService).startForeground(1, LJNotification, FOREGROUND_SERVICE_TYPE_DATA_SYNC);
            FNotificationManager.notify(1, LJNotification);

            //6 - Waits until infinite, so that the service can be kept running "forever"
            LEvent.WaitFor(INFINITE);
            JavaService.stopSelf(StartId);
          end;
        except
          on E: Exception do
            ShowUpNotification('Thread error: ' + sLineBreak + E.Message);
        end;
      end);

    // 7 - Insert and start the thread
    LThread.FreeOnTerminate := False;
    FThreads.Add(LThread);
    LThread.Start;

  except
    on E: Exception do
      ShowUpNotification('Start error: ' + sLineBreak + E.Message);
  end;
end;

procedure TDM.ShowUpNotification(const Text: String);
var
  LNotification: TNotification;
begin
  LNotification := NotificationCenter.CreateNotification();
  try
    LNotification.ChannelId := FChannelId;
    LNotification.Name := IntToStr(Random(1000));
    LNotification.Title := 'Ntfy Service';
    LNotification.AlertBody := Text;
    NotificationCenter.PresentNotification(LNotification);
  finally
    LNotification.Free;
  end;
end;

procedure TDM.ShowUpNtfy(AEvent: INotifyEvent);
var
  LNotification: TNotification;
begin
  if FWakeLock <> nil then
    FWakeLock.acquire;

  try
    LNotification := NotificationCenter.CreateNotification();
    try
      LNotification.ChannelId := FChannelId;
      LNotification.Name := IntToStr(Random(1000));
      LNotification.Title := AEvent.Title;
      LNotification.AlertBody := AEvent.MessageContent;
      NotificationCenter.PresentNotification(LNotification);
    finally
      LNotification.Free;
    end;
  finally
    if FWakeLock <> nil then
      if FWakeLock.isHeld then
        FWakeLock.release();
  end;
end;

end.

