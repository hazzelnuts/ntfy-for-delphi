unit Test.Constants;

interface

uses
  Notify.Types;

  const TITLE = '⚡ Ntfy for Delphi';
  const MESSAGECONTENT = 'A friendly Delphi library to ntfy.sh';
  const TOPIC = 'notify-delphi-integration-8jh27d';
  const TAGS: array [0..1] of String =  ('construction', 'smiley');
  const PRIORITY = Ord(TNotifyPriority.MAX);
  const ACTION_LABEL = 'Action Label';
  const ACTION_URL = 'https://ntfy.sh';
  const TIME_DELAY = 2153; //I finished the major test cases in 2022, Dec 31th at 21:53hs
  const MSG_WRONG_PRIORITY = 'Sent priority is different from the one received';
  const MSG_WRONG_MESSAGE = 'Sent message is different from the one received';
  const MSG_WRONG_TITLE = 'Sent title is different from the one received';
  const MSG_REQUEST_FAILED = 'Response status code not expected';
  const MSG_WRONG_HEADERS = 'Sent headers is different from the one received';
  const MSG_WRONG_LABELS = 'Sent label is different from the one received';
  const MSG_WRONG_URLS = 'Sent url is different from the one received';
  const MSG_WRONG_FILENAME = 'Sent file name is different from the one received';
  const MSG_WRONG_EMOJI = 'Sent emojis are different from the ones received';
  const MSG_WRONG_ICON = 'Sent icon is different fron the one received';
  const MSG_WRONG_URL_ATTACHMENT = 'Sent url attachment is different from the one received';

  {$SCOPEDENUMS ON}
  type TStatusCode = (
    OK = 200,
    SERVER_ERROR = 500,
    BAD_REQUEST = 400
  );
  {$SCOPEDENUMS OFF}

implementation

end.
