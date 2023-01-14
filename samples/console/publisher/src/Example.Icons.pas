unit Example.Icons;

interface

  ///
  ///  Example using icons url
  ///

procedure UseIcons;

implementation

uses
  Notify;

procedure UseIcons;
begin

  Ntfy.Notification(
    New.Notification
      .Topic('your-very-secret-topic')
      .Icon('https://styles.redditmedia.com/t5_32uhe/styles/communityIcon_xnt6chtnr2j21.png')
  );

end;


end.
