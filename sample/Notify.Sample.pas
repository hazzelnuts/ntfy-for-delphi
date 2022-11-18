unit Notify.Sample;

interface

uses
  Notify;

implementation

var
  LNotify: INotify;
  LNotification: INotification;

initialization

  LNotification := TNotification.New
    .Topic('something-very-strange')
    .MessageContent('Test on Delphi');

  LNotify := TNotify.New.Notification(LNotification).Publish;

finalization

end.
