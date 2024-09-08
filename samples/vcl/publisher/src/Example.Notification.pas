unit Example.Notification;

interface

  procedure SendNotification;

implementation

uses
  Notify;


procedure SendNotification;
var
  LTags: TArray<string>;
  LNotification: INotifyNotification;
begin
  LNotification := New.Notification;
  LNotification.Topic(CbTopic.Text);
  LNotification.Title(lbeTitle.Text);
  LNotification.MessageContent(lbeMessage.Text);
  LNotification.Priority(TNotifyPriority(CbPriority.ItemIndex + 1));
  LNotification.Icon(lbeIconAttachment.Text);
  LNotification.FilePath(lbeFileAttachment.Text);
  LNotification.Attach(lbeURLAttachment.Text);
  LNotification.Email(lbeEmail.Text);
  Ntfy.Notification(LNotification);
  Ntfy.Publish;
end;

end.
