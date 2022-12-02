program ConsolePublisher;

{$APPTYPE CONSOLE}

uses
  NotifyDelphi;

begin

  Ntfy.Notification(
    New.Notification
      .Topic('notify-delphi-integration-8jh27d')
      .Title('Work time')
      .MessageContent('Ending for today...')
      .Priority(TNotifyPriority.MIN)
      .Tags(['ice_cream'])
      .Action(
        New.Action
          .&Type(TNotifyActionType.HTTP)
          .&Label('Open Mail')
          .Method('GET')
          .Url('https://viacep.com.br/ws/01001000/json/'))
  );

  Ntfy.Delay('10s').Publish;


end.
