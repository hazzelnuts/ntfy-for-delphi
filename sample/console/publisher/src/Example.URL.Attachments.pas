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
      .Attach('https://images.unsplash.com/photo-1670531910262-'
      +'5ddb5ad666ea?crop=entropy&cs=tinysrgb&fit=crop&fm=jpg&h='
      +'150&ixid=MnwxfDB8MXxyYW5kb218MHx8fHx8fHx8MTY3MTE0OTI2NA&ixlib=rb-4.0.3&q=80&w=500')
  );

end;

end.
