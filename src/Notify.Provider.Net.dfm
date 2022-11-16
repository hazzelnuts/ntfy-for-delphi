object NotifyProviderNet: TNotifyProviderNet
  OldCreateOrder = False
  Height = 150
  Width = 215
  object NetHTTPClient: TNetHTTPClient
    UserAgent = 'Embarcadero URI Client/1.0'
    Left = 40
    Top = 56
  end
  object NetHTTPRequest: TNetHTTPRequest
    ConnectionTimeout = 0
    SendTimeout = 0
    ResponseTimeout = 0
    Left = 136
    Top = 48
  end
end
