unit Example.Action.View;

interface

  ///
  ///  Example using action view
  ///

procedure UseActionView;

implementation

uses
  Notify;

procedure UseActionView;
begin

  // Opens Google Maps in a certain location
  Ntfy.Notification(New.Notification
    .Topic('your-very-secret-topic')
    .Action(New.Action
      .&Type(TNotifyActionType.VIEW)
      .Url('geo:40.765819,-73.975866')
      .&Label('Open Google Maps'))
  );

  // Opens gmail
  Ntfy.Notification(New.Notification
    .Topic('your-very-secret-topic')
    .Action(New.Action
      .&Type(TNotifyActionType.VIEW)
      .Url('mailto:phil@example.com')
      .&Label('Send Mail'))
  );

end;

end.
