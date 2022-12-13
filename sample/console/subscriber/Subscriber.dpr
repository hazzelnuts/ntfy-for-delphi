program Subscriber;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Notify.Action.Contract in '..\..\..\src\Notify.Action.Contract.pas',
  Notify.Action.DTO in '..\..\..\src\Notify.Action.DTO.pas',
  Notify.Action.Factory in '..\..\..\src\Notify.Action.Factory.pas',
  Notify.Action in '..\..\..\src\Notify.Action.pas',
  Notify.Api.Contract in '..\..\..\src\Notify.Api.Contract.pas',
  Notify.Api.Factory in '..\..\..\src\Notify.Api.Factory.pas',
  Notify.Api.Indy in '..\..\..\src\Notify.Api.Indy.pas',
  Notify.Config.Contract in '..\..\..\src\Notify.Config.Contract.pas',
  Notify.Config.Factory in '..\..\..\src\Notify.Config.Factory.pas',
  Notify.Config in '..\..\..\src\Notify.Config.pas',
  Notify.Core.Contract in '..\..\..\src\Notify.Core.Contract.pas',
  Notify.Core.Factory in '..\..\..\src\Notify.Core.Factory.pas',
  Notify.Core in '..\..\..\src\Notify.Core.pas',
  Notify.Facade in '..\..\..\src\Notify.Facade.pas',
  Notify.JSON.Parser in '..\..\..\src\Notify.JSON.Parser.pas',
  Notify.Notification.Contract in '..\..\..\src\Notify.Notification.Contract.pas',
  Notify.Notification.DTO in '..\..\..\src\Notify.Notification.DTO.pas',
  Notify.Notification.Factory in '..\..\..\src\Notify.Notification.Factory.pas',
  Notify.Notification in '..\..\..\src\Notify.Notification.pas',
  Notify.SmartPointer in '..\..\..\src\Notify.SmartPointer.pas',
  Notify.Types in '..\..\..\src\Notify.Types.pas',
  Notify in '..\..\..\src\Notify.pas',
  Notify.Subscription.Thread in '..\..\..\src\Notify.Subscription.Thread.pas',
  Notify.Logs in '..\..\..\src\Notify.Logs.pas',
  Notify.SimpleWebsocket.Indy in '..\..\..\src\Notify.SimpleWebsocket.Indy.pas',
  NX.Horizon in '..\..\..\src\NX.Horizon.pas',
  Notify.Event.DTO in '..\..\..\src\Notify.Event.DTO.pas',
  Notify.Subscription.Event in '..\..\..\src\Notify.Subscription.Event.pas',
  Notify.Event.Contract in '..\..\..\src\Notify.Event.Contract.pas',
  Notify.Event in '..\..\..\src\Notify.Event.pas',
  Notify.Event.Factory in '..\..\..\src\Notify.Event.Factory.pas',
  Notify.Attachment.DTO in '..\..\..\src\Notify.Attachment.DTO.pas',
  Notify.Attachment.Contract in '..\..\..\src\Notify.Attachment.Contract.pas',
  Notify.Attachment in '..\..\..\src\Notify.Attachment.pas',
  Notify.Attachment.Factory in '..\..\..\src\Notify.Attachment.Factory.pas';

begin

//  Ntfy.Notification(
//    New.Notification
//      .Title('Test log')
//      .MessageContent('Test message')
//  );

//  Ntfy
//    .SaveLog(True)
//    .Topic('notify-delphi-integration-8jh27d')
//    .Subscribe;

  Ntfy.Subscribe('notify-delphi-integration-8jh27d',
    procedure (Event: INotifyEvent)
    begin
      Writeln('Event callback is now working!' + Event.MessageContent);
    end);

end.
