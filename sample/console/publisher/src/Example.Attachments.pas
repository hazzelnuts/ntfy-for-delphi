unit Example.Attachments;

interface

  ///
  ///  Example using file attachments
  ///

procedure UseAttachments;

implementation

uses
  System.IOUtils,
  System.SysUtils,
  Notify;

procedure UseAttachments;
var
  LFilePath: String;
begin

  LFilePath := '..\..\..\..\img\delphi-notify.png';

  Ntfy.Notification(
    New.Notification
      .Topic('your-very-secret-topic')
      .FilePath(LFilePath)
  );

end;

end.
