unit Example.Basic;

interface

    ///
    ///  Publishing a simple message
    ///

procedure SimpleMessage;

implementation

uses
  Notify;

procedure SimpleMessage;
begin

  Ntfy.Notification(
    New.Notification
      .Topic('your-very-secret-topic')
      .Title('Simple message sent')
  );

  Ntfy.Publish;
end;

end.
