unit Service.Local.Ntfy;

interface

uses
  System.Android.Service,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  System.Notification,
  System.SyncObjs,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.App,
  Androidapi.JNI.Os,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Support,
  Androidapi.JNIBridge,
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
    FNotificationID: Integer;
    FThreads: TObjectList<TThread>;
    FWakeLock: JPowerManager_WakeLock;
    procedure ShowUpNotification(const Text: String);
    procedure ShowUpNtfy(AEvent: INotifyEvent);
  end;

var
  DM: TDM;

implementation

uses
  System.DateUtils, Androidapi.Helpers, Notify.Facade, Intent.Service.Helper;


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
    FNotificationManager := TJNotificationManager.Wrap((TAndroidHelper.Context.getSystemService(TJContext.JavaClass.NOTIFICATION_SERVICE)));
    FNotificationBuilder := TJapp_NotificationCompat_Builder.JavaClass.init(TAndroidHelper.Context);
    FNotificationBuilder.setSmallIcon(TAndroidHelper.Context.getApplicationInfo.icon);
    FNotificationBuilder.setContentTitle(StrToJCharSequence('Ntfy service started'));
    FNotificationBuilder.setContentText(StrToJCharSequence('Listening messages'));
    FNotificationBuilder.setAutoCancel(True);
    FNotificationID := 98437;

    if TJBuild_VERSION.JavaClass.SDK_INT >= 26 then
    begin
      LChannel := TJNotificationChannel.JavaClass.init(
        StringToJString('ntfy-for-delphi'),
        StrToJCharSequence('Android Ntfy Subscription'),
        TJNotificationManager.JavaClass.IMPORTANCE_HIGH);
      LChannel.setLightColor(TJColor.JavaClass.BLUE);
      LChannel.setLockscreenVisibility(TJNotification.JavaClass.VISIBILITY_PRIVATE);
      FNotificationManager.createNotificationChannel(LChannel);
      FNotificationBuilder.setChannelId(StringToJString(IntToStr(FNotificationID)));
    end;

    // Acquire wake lock
    LPowerManager := TJPowerManager.Wrap(TAndroidHelper.Context.getSystemService(TJContext.JavaClass.POWER_SERVICE));
    FWakeLock := LPowerManager.newWakeLock(TJPowerManager.JavaClass.PARTIAL_WAKE_LOCK, StringToJString('MyApp::NtfyWakeLock'));
    FWakeLock.acquire();

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
    Result := TJService.JavaClass.START_REDELIVER_INTENT;
    LIntentService := TIntentServiceHelper.Create(Intent);

    if LIntentService.Data.IsEmpty then
      Exit;

    LThread := TThread.CreateAnonymousThread(
    procedure
      var
        LEvent: TEvent;
        LJNotification: JNotification;
      begin
        try
          while True do
          begin
            LJNotification := FNotificationBuilder.build();
            LEvent := TEvent.Create;
            Ntfy.Subscribe(LIntentService.Data, ShowUpNtfy);
            TJService.Wrap(JavaService).startForeground(FNotificationID, LJNotification, FOREGROUND_SERVICE_TYPE_DATA_SYNC);
            FNotificationBuilder.setContentText(StrToJCharSequence('Syncing...'));
            FNotificationManager.notify(FNotificationID, FNotificationBuilder.build());
            LEvent.WaitFor(INFINITE);
            JavaService.stopSelf(StartId);
          end;
        except
          on E: Exception do
            ShowUpNotification('Thread error: ' + sLineBreak + E.Message);
        end;
      end);

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
    LNotification.ChannelId := IntToStr(FNotificationID);
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
  LNotification := NotificationCenter.CreateNotification();
  try
    LNotification.ChannelId := IntToStr(FNotificationID);
    LNotification.Name := IntToStr(Random(1000));
    LNotification.Title := AEvent.Title;
    LNotification.AlertBody := AEvent.MessageContent;
    NotificationCenter.PresentNotification(LNotification);
  finally
    LNotification.Free;
  end;
end;

end.

