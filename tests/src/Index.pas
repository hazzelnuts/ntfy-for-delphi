unit Index;

interface

uses
  Test.Simple.Message,
  Test.Action.Header,
  Test.Action.HTTP,
  Test.Action.View,
  Test.Attachments,
  Test.Email,
  Test.Emojis,
  Test.Icons,
  Test.URL.Attachments;

implementation

uses
  TestFramework;

initialization
  RegisterTest('Send simple message', TTestSimpleMessage.Suite);
  RegisterTest('Send action view', TTestActionView.Suite);
  RegisterTest('Send action http', TTestActionHTTP.Suite);
  RegisterTest('Send action headers', TTestActionHeader.Suite);
  RegisterTest('Send file attachments', TTestAttachments.Suite);
  RegisterTest('Send emails', TTestEmail.Suite);
  RegisterTest('Send emails', TTestEmojis.Suite);
  RegisterTest('Send icons', TTestIcons.Suite);
  RegisterTest('Send url attachments', TTestURLAttachments.Suite);

end.
