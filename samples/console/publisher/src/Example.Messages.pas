unit Example.Messages;

interface

  ///
  ///  Attaching a message body
  ///

procedure UseMessage;

implementation

uses
  Notify;

procedure UseMessage;
begin

  Ntfy.Notification(
    New.Notification
      .Topic('your-very-secret-topic')
      .MessageContent('A message body...')
  );

end;

end.
