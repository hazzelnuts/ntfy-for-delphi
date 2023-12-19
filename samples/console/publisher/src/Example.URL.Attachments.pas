unit Example.URL.Attachments;

interface

  ///
  ///  Example using url attachments
  ///

procedure UseURLAttachments;

implementation

uses
  Notify, System.SysUtils;

const
  AttachURL =
  'https://images.unsplash.com/photo-1670531910262-'+
  '5ddb5ad666ea?crop=entropy&cs=tinysrgb&fit=crop&f'+
  'm=jpg&h=150&ixid=MnwxfDB8MXxyYW5kb218MHx8fHx8fHx'+
  '8MTY3MTE0OTI2NA&ixlib=rb-4.0.3&q=80&w=500';

procedure UseURLAttachments;
begin

  Ntfy.Notification(
    New.Notification
      .Topic('your-very-secret-topic')
      .AttachURL(Trim(AttachURL))
  );

end;

end.
