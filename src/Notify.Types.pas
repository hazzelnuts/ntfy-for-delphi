unit Notify.Types;

interface

uses
  Notify.JSON.Parser;

type
  {$SCOPEDENUMS ON}

  TNotifyFectchType = (DURATION, UNIX, MESSAGE_ID);

  TNotifyActionType = (VIEW, BROADCAST, HTTP);

  TNotifyPriority = (
    MAX = 5,
    HIGH = 4,
    DEFAULT = 3,
    LOW = 2,
    MIN = 1
  );

  {$SCOPEDENUMS OFF}

  TJsonDTO = Notify.JSON.Parser.TJsonDTO;

const
  NotifyActionTypesArray: array [TNotifyActionType] of String = (
    'view',
    'broadcast',
    'http'
  );

implementation


end.
