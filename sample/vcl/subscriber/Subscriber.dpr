program Subscriber;

uses
  Vcl.Forms,
  View.Main in 'src\View.Main.pas' {ViewMain},
  Notify.Action.Contract in '..\..\..\src\Notify.Action.Contract.pas',
  Notify.Action.DTO in '..\..\..\src\Notify.Action.DTO.pas',
  Notify.Action.Factory in '..\..\..\src\Notify.Action.Factory.pas',
  Notify.Action in '..\..\..\src\Notify.Action.pas',
  Notify.Api.Contract in '..\..\..\src\Notify.Api.Contract.pas',
  Notify.Api.Factory in '..\..\..\src\Notify.Api.Factory.pas',
  Notify.Api.Indy in '..\..\..\src\Notify.Api.Indy.pas',
  Notify.Attachment.Contract in '..\..\..\src\Notify.Attachment.Contract.pas',
  Notify.Attachment.DTO in '..\..\..\src\Notify.Attachment.DTO.pas',
  Notify.Attachment.Factory in '..\..\..\src\Notify.Attachment.Factory.pas',
  Notify.Attachment in '..\..\..\src\Notify.Attachment.pas',
  Notify.Config.Contract in '..\..\..\src\Notify.Config.Contract.pas',
  Notify.Config.Factory in '..\..\..\src\Notify.Config.Factory.pas',
  Notify.Config in '..\..\..\src\Notify.Config.pas',
  Notify.Core.Contract in '..\..\..\src\Notify.Core.Contract.pas',
  Notify.Core.Factory in '..\..\..\src\Notify.Core.Factory.pas',
  Notify.Core in '..\..\..\src\Notify.Core.pas',
  Notify.Event.Contract in '..\..\..\src\Notify.Event.Contract.pas',
  Notify.Event.DTO in '..\..\..\src\Notify.Event.DTO.pas',
  Notify.Event.Factory in '..\..\..\src\Notify.Event.Factory.pas',
  Notify.Event in '..\..\..\src\Notify.Event.pas',
  Notify.Facade in '..\..\..\src\Notify.Facade.pas',
  Notify.JSON.Parser in '..\..\..\src\Notify.JSON.Parser.pas',
  Notify.Logs in '..\..\..\src\Notify.Logs.pas',
  Notify.Notification.Contract in '..\..\..\src\Notify.Notification.Contract.pas',
  Notify.Notification.DTO in '..\..\..\src\Notify.Notification.DTO.pas',
  Notify.Notification.Factory in '..\..\..\src\Notify.Notification.Factory.pas',
  Notify.Notification in '..\..\..\src\Notify.Notification.pas',
  Notify in '..\..\..\src\Notify.pas',
  Notify.SimpleWebsocket.Indy in '..\..\..\src\Notify.SimpleWebsocket.Indy.pas',
  Notify.SmartPointer in '..\..\..\src\Notify.SmartPointer.pas',
  Notify.Subscription.Event in '..\..\..\src\Notify.Subscription.Event.pas',
  Notify.Types in '..\..\..\src\Notify.Types.pas',
  NX.Horizon in '..\..\..\src\NX.Horizon.pas',
  Example.Push.Notifications in 'src\Example.Push.Notifications.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TViewMain, ViewMain);
  Application.Run;
end.
