unit Notify.Sample;

interface

uses
  Notify.Client;

implementation

initialization

  Ntfy.Notification(
    New.Notification
      .Topic('notify-delphi-integration-8jh27d')
      .Title('Work time')
      .MessageContent('Ending for today...')
      .Priority(TNtfyPriority.MIN)
      .Tags(['ice_cream'])
  );

  Ntfy.Delay('1min').Publish;

finalization

end.
