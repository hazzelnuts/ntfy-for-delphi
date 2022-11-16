unit Notify.Types;

interface
uses
  Notify.Publisher.Contract;

type
  {$SCOPEDENUMS ON}
  TNotifyProviderEngine = ( INDY, NETHTTP );
  TNotifyFectchType = (DURATION, UNIX, MESSAGE_ID);
  {$SCOPEDENUMS OFF}

implementation


end.
