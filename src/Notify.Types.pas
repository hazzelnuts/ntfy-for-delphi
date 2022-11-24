unit Notify.Types;

interface

type
  {$SCOPEDENUMS ON}

  TNotifyFectchType = (DURATION, UNIX, MESSAGE_ID);

  TNotifyActionType = (VIEW = 0, BROADCAST = 1, HTTP = 2);

  TNotifyPriority = (
    MAX = 5,
    HIGH = 4,
    DEFAULT = 3,
    LOW = 2,
    MIN = 1
  );

  {$SCOPEDENUMS OFF}

const
  NotifyActionTypesArray: array [TNotifyActionType] of String = (
    'view',
    'broadcast',
    'http'
  );

implementation


end.
