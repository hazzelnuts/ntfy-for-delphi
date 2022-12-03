program Publisher;

{$APPTYPE CONSOLE}

uses
  Notify.Action.Contract in '..\..\src\Notify.Action.Contract.pas',
  Notify.Action.DTO in '..\..\src\Notify.Action.DTO.pas',
  Notify.Action.Factory in '..\..\src\Notify.Action.Factory.pas',
  Notify.Action in '..\..\src\Notify.Action.pas',
  Notify.Core.Contract in '..\..\src\Notify.Core.Contract.pas',
  Notify.Core.Factory in '..\..\src\Notify.Core.Factory.pas',
  Notify.Core in '..\..\src\Notify.Core.pas',
  Notify.Facade in '..\..\src\Notify.Facade.pas',
  Notify.JSON.Parser in '..\..\src\Notify.JSON.Parser.pas',
  Notify.Notification.Contract in '..\..\src\Notify.Notification.Contract.pas',
  Notify.Notification.DTO in '..\..\src\Notify.Notification.DTO.pas',
  Notify.Notification.Factory in '..\..\src\Notify.Notification.Factory.pas',
  Notify.Notification in '..\..\src\Notify.Notification.pas',
  Notify.Provider.Contract in '..\..\src\Notify.Provider.Contract.pas',
  Notify.Provider.Factory in '..\..\src\Notify.Provider.Factory.pas',
  Notify.Provider.Indy in '..\..\src\Notify.Provider.Indy.pas',
  Notify.SmartPointer in '..\..\src\Notify.SmartPointer.pas',
  Notify.Types in '..\..\src\Notify.Types.pas',
  Notify in '..\..\src\Notify.pas';

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
