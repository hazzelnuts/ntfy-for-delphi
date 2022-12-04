program Publisher;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
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

var
  LFile: String;

begin

  LFile := ExtractFilePath(ParamStr(0)) + 'ntfy.png';

  Ntfy.Notification(
    New.Notification
      .Topic('notify-delphi-integration-8jh27d')
      .Title('Email attachments')
      .MessageContent('Notify project also warrants you can send email attachments')
      {
      .Attach('https://images.unsplash.com/photo-1506744038136-46273834b3fb?ixlib=rb-4.0.3&ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&auto=format&fit=crop&w=800')
      .Icon('https://styles.redditmedia.com/t5_32uhe/styles/communityIcon_xnt6chtnr2j21.png')
      .Delay('10s')
      .Email('afnsldd@gmail.com')
      .FileName(LFile)
      .Tags(['paperclip'])
      .Priority(TNotifyPriority.MIN)
      .Action(
        New.Action
          .&Type(TNotifyActionType.VIEW)
          .&Label('See design')
          .Url('https://unsplash.com/photos/NRQV-hBF10M'))}
  );

  Ntfy.Publish;

end.
