unit Notify.Sample;

interface

uses
  Notify.Client;

implementation

initialization

  Ntfy.Notification(
    New.Publisher
      .Topic('something-very-strange')
      .MessageContent('This message is a new design 🚧')
      .Title('Coming from an aplication')
  );

  Ntfy.Publish;

finalization

end.
