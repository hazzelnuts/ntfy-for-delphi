object NtfyAndroidServiceModule: TNtfyAndroidServiceModule
  OnCreate = AndroidServiceCreate
  OnDestroy = AndroidServiceDestroy
  OnStartCommand = AndroidServiceStartCommand
  Height = 238
  Width = 324
  object NotificationCenter: TNotificationCenter
    Left = 144
    Top = 104
  end
end
