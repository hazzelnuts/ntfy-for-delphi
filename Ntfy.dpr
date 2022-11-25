program Ntfy;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Notify.Core.Contract in 'src\Notify.Core.Contract.pas',
  Notify.Core in 'src\Notify.Core.pas',
  Notify.Core.Factory in 'src\Notify.Core.Factory.pas',
  Notify.Provider.Indy in 'src\Notify.Provider.Indy.pas',
  Notify.Provider.Contract in 'src\Notify.Provider.Contract.pas',
  Notify.Provider.Factory in 'src\Notify.Provider.Factory.pas',
  Notify.Action.Contract in 'src\Notify.Action.Contract.pas',
  Notify.Notification.Contract in 'src\Notify.Notification.Contract.pas',
  Notify.Notification in 'src\Notify.Notification.pas',
  Notify.Notification.Factory in 'src\Notify.Notification.Factory.pas',
  Notify.Notification.DTO in 'src\Notify.Notification.DTO.pas',
  Notify.JSON.Parser in 'src\Notify.JSON.Parser.pas',
  Notify.SmartPointer in 'src\Notify.SmartPointer.pas',
  Notify.Types in 'src\Notify.Types.pas',
  Notify.Facade in 'src\Notify.Facade.pas',
  Notify.Sample in 'sample\Notify.Sample.pas',
  Notify.Client in 'src\Notify.Client.pas',
  Notify.Action.DTO in 'src\Notify.Action.DTO.pas',
  Notify.Action in 'src\Notify.Action.pas',
  Notify.Action.Factory in 'src\Notify.Action.Factory.pas';

begin

  ReportMemoryLeaksOnShutdown := True;

end.
