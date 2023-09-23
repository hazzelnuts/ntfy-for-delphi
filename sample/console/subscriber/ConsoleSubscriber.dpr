program ConsoleSubscriber;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Notify;

begin

  ReportMemoryLeaksOnShutdown := True;

  Ntfy.Subscribe('notify-delphi-integration-8jh27d',
    procedure (AEvent: INotifyEvent)
    begin
      Writeln('You received a message:');
      Writeln('Title: ' + AEvent.Title);
      Writeln('Message: ' + AEvent.MessageContent);
    end);

end.
