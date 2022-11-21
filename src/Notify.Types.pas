unit Notify.Types;

interface

type
  {$SCOPEDENUMS ON}

  TNotifyFectchType = (DURATION, UNIX, MESSAGE_ID);

  TNotifyPriority = (
    MAX = 5,
    HIGH = 4,
    DEFAULT = 3,
    LOW = 2,
    MIN = 1
  );

  {$SCOPEDENUMS OFF}

implementation


end.
