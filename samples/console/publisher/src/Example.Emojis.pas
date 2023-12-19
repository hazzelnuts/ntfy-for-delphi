unit Example.Emojis;

interface

  ///
  ///  Example using emojis/tags
  ///

procedure UseTags;

implementation

uses
  Notify;

procedure UseTags;
begin

  Ntfy.Notification(
    New.Notification
      .Topic('your-very-secret-topic')
      .Tags(TArray<String>.Create('partying_face', 'warning', 'rotating_light'))
  );

end;

end.
