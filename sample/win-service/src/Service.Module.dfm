object NtfyModule: TNtfyModule
  OldCreateOrder = False
  DisplayName = 'NtfyService'
  AfterInstall = ServiceAfterInstall
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 150
  Width = 215
end
