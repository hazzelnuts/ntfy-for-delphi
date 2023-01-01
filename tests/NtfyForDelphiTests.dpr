program NtfyForDelphiTests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  DUnitTestRunner,
  Notify in '..\src\Notify.pas',
  Notify.Action.Contract in '..\src\Notify.Action.Contract.pas',
  Notify.Action.DTO in '..\src\Notify.Action.DTO.pas',
  Notify.Action in '..\src\Notify.Action.pas',
  Notify.Api.Contract in '..\src\Notify.Api.Contract.pas',
  Notify.Api.Indy in '..\src\Notify.Api.Indy.pas',
  Notify.Attachment.Contract in '..\src\Notify.Attachment.Contract.pas',
  Notify.Attachment.DTO in '..\src\Notify.Attachment.DTO.pas',
  Notify.Attachment in '..\src\Notify.Attachment.pas',
  Notify.Config.Contract in '..\src\Notify.Config.Contract.pas',
  Notify.Config in '..\src\Notify.Config.pas',
  Notify.Core.Contract in '..\src\Notify.Core.Contract.pas',
  Notify.Core in '..\src\Notify.Core.pas',
  Notify.Event.Contract in '..\src\Notify.Event.Contract.pas',
  Notify.Event.DTO in '..\src\Notify.Event.DTO.pas',
  Notify.Event in '..\src\Notify.Event.pas',
  Notify.Facade in '..\src\Notify.Facade.pas',
  Notify.JSON.Parser in '..\src\Notify.JSON.Parser.pas',
  Notify.Logs in '..\src\Notify.Logs.pas',
  Notify.Notification.Contract in '..\src\Notify.Notification.Contract.pas',
  Notify.Notification.DTO in '..\src\Notify.Notification.DTO.pas',
  Notify.Notification in '..\src\Notify.Notification.pas',
  Notify.SmartPointer in '..\src\Notify.SmartPointer.pas',
  Notify.Subscription.Event in '..\src\Notify.Subscription.Event.pas',
  Notify.Types in '..\src\Notify.Types.pas',
  Notify.Error in '..\src\Notify.Error.pas',
  Notify.Api.Response in '..\src\Notify.Api.Response.pas',
  Notify.Response.Data in '..\src\Notify.Response.Data.pas',
  Notify.Custom.Types in '..\src\Notify.Custom.Types.pas',
  NX.Horizon in '..\src\NX.Horizon.pas',
  Test.Simple.Message in 'src\Test.Simple.Message.pas',
  Test.Action.Header in 'src\Test.Action.Header.pas',
  Test.Constants in 'src\Test.Constants.pas',
  Test.Action.HTTP in 'src\Test.Action.HTTP.pas',
  Test.Action.View in 'src\Test.Action.View.pas',
  Test.Attachments in 'src\Test.Attachments.pas',
  Test.Email in 'src\Test.Email.pas',
  Test.Icons in 'src\Test.Icons.pas',
  Index in 'src\Index.pas',
  Test.Emojis in 'src\Test.Emojis.pas',
  Test.URL.Attachments in 'src\Test.URL.Attachments.pas', Winapi.Windows;

begin
  SetWindowText(0, 'Test cases');
  DUnitTestRunner.RunRegisteredTests;
  Readln;
end.

