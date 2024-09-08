object DM: TDM
  OnCreate = AndroidServiceCreate
  OnDestroy = AndroidServiceDestroy
  OnStartCommand = AndroidServiceStartCommand
  Height = 238
  Width = 324
  object NotificationCenter: TNotificationCenter
    Left = 128
    Top = 94
  end
end
