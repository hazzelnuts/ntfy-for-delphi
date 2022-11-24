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
      .Action(
        New.Action
          .&Type(TNtfyActionType.VIEW)
          .&Label('Open Mail')
          .Url('mailto:afnsldd@gmail.com')
          .Clear(True))
  );

  Ntfy.Delay('15s').Publish;

finalization

end.
