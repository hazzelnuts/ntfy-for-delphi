program ConsolePublisher;

{$APPTYPE CONSOLE}

uses
  Example.Basic in 'src\Example.Basic.pas',
  Example.Action.Header in 'src\Example.Action.Header.pas',
  Example.Priorities in 'src\Example.Priorities.pas',
  Example.Messages in 'src\Example.Messages.pas',
  Example.Emojis in 'src\Example.Emojis.pas',
  Example.Schedule.Delivery in 'src\Example.Schedule.Delivery.pas',
  Example.Action.View in 'src\Example.Action.View.pas',
  Example.Action.HTTP in 'src\Example.Action.HTTP.pas',
  Example.Attachments in 'src\Example.Attachments.pas',
  Example.URL.Attachments in 'src\Example.URL.Attachments.pas',
  Example.Icons in 'src\Example.Icons.pas',
  Example.Email in 'src\Example.Email.pas',
  Example.Advanced.Configs in 'src\Example.Advanced.Configs.pas',
  Notify;

begin

  UseSimpleMessage;
  //UsePriorities;
  //UseMessage;
  //UseTags;
  //UseActionView;
  //UseActionHTTP;
  //UseAttachments;
  //UseURLAttachments;
  //UseIcons;
  //UseEmail;

  Ntfy.Topic('notify-delphi-integration-8jh27d').Publish;

end.
