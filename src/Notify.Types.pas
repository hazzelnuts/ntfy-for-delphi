unit Notify.Types;

interface

uses
  Notify.JSON.Parser;

type
  {$SCOPEDENUMS ON}

  TNotifyActionType = (VIEW, BROADCAST, HTTP);

  TNotifySubscriptionType = (JSON, SSE, RAW, WEB_SOCKET);

  TNotifyPriority = (
    MAX = 5,
    HIGH = 4,
    DEFAULT = 3,
    LOW = 2,
    MIN = 1
  );

  TNotifyMessageEvent = (
    OPEN,
    KEEPALIVE,
    MSG,
    POLL_REQUEST
  );

  TNotifyFilter = (
    ID,
    TITLE,
    MESSAGECONTENT,
    PRIORITY,
    TAGS
  );

  {$SCOPEDENUMS OFF}

  TJsonDTO = Notify.JSON.Parser.TJsonDTO;

const
  NotifyActionTypesArray: array [TNotifyActionType] of String = (
    'view',
    'broadcast',
    'http'
  );

  NotifyMessageEventArray: array[TNotifyMessageEvent] of String = (
    'open',
    'keepalive',
    'message',
    'poll_request'
  );

  NotifyFilterTypeDescription: array[TNotifyFilter] of String = (
    'id',
    'title',
    'message',
    'priority',
    'tags'
  );

implementation


end.
