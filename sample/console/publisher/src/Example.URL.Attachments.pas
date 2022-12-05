unit Example.URL.Attachments;

interface

  ///
  ///  Example using url attachments
  ///

procedure UseURLAttachments;

implementation

uses
  Notify;

procedure UseURLAttachments;
begin

  Ntfy.Notification(
    New.Notification
      .Topic('your-very-secret-topic')
      .Attach('https://i.picsum.photos/id/1002/200/200.jpg?hmac=Tf8HIQ9ThNr5_OQwJQuQXmZ4JysJ2pdFOx0bjdHTc-g')
  );

end;

end.
