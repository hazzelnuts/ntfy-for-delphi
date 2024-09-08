unit Example.Email;

interface

  ///
  ///  Example using email
  ///

procedure UseEmail;

implementation

uses
  Notify;

procedure UseEmail;
begin

  Ntfy.Notification(
    New.Notification
      .Topic('your-very-secret-topic')
      .Title('Email test')
      .MessageContent('Emails with a title and a message')
      .Email('someemail@gmail.com')
  );

end;

end.
