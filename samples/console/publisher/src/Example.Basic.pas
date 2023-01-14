unit Example.Basic;

interface

  ///
  ///  Publishing a simple message
  ///

procedure UseSimpleMessage;

implementation

uses
  Notify;

procedure UseSimpleMessage;
begin

  Ntfy.Notification(
    New.Notification
      .Topic('your-very-secret-topic')
      .Title('Simple message sent')
  );

end;

end.
