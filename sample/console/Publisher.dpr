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
  Notify in '..\..\src\Notify.pas',
  Notify.Config in '..\..\src\Notify.Config.pas',
  Notify.Config.Contract in '..\..\src\Notify.Config.Contract.pas',
  Notify.Config.Factory in '..\..\src\Notify.Config.Factory.pas';

var
  LFile: String;

begin

  LFile := ExtractFilePath(ParamStr(0)) + 'ntfy.png';

  Ntfy.Notification(
    New.Notification
      .Topic('notify-delphi-integration-8jh27d')
      .Title('Disabling Firebase')
      .MessageContent('With FCM disabled, messages will significantly delay to be delivered in Android App')
//      .FilePath(LFile)
//      .Attach('https://www.collinsdictionary.com/images/full/mountain_221506423_1000.jpg?version=4.0.288')
//      .Icon('https://styles.redditmedia.com/t5_32uhe/styles/communityIcon_xnt6chtnr2j21.png')
//      .Delay('10s')
//      .Email('afnsldd@gmail.com')
//      .Tags(['desktop_computer', 'computer_mouse', 'minidisc'])
//      .Priority(TNotifyPriority.HIGH)
//      .Action(
//        New.Action
//          .&Type(TNotifyActionType.VIEW)
//          .&Label('Go to Delphi Praxis')
//          .Url('https://en.delphipraxis.net/'))
  );

  Ntfy.DisableFireBase(True).Publish;


end.
