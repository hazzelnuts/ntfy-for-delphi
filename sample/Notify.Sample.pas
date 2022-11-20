unit Notify.Sample;

interface

uses
  Notify.Client;

implementation

initialization

  Ntfy.Notification(
    New.Notification
      .Topic('notify-delphi-integration-8jh27d')
      .MessageContent('📌 Working with headers today')
      .Title('Testing')
  );

  Ntfy.Publish;

finalization

end.
