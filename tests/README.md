<div align="center">
  <img src="../img/delphi-notify.png">
</div>

## Test cases 🧪

Provided some DUnit tests for main ntfy's methods.

``` pascal
  RegisterTest('Send simple message', TTestSimpleMessage.Suite);
  RegisterTest('Send action view', TTestActionView.Suite);
  RegisterTest('Send action http', TTestActionHTTP.Suite);
  RegisterTest('Send file attachments', TTestAttachments.Suite);
  RegisterTest('Send action headers', TTestActionHeader.Suite);
  RegisterTest('Send emails', TTestEmail.Suite);
  RegisterTest('Send tags', TTestEmojis.Suite);
  RegisterTest('Send icons', TTestIcons.Suite);
  RegisterTest('Send url attachments', TTestURLAttachments.Suite);

```
<br/>

## ⚠ Observations
You must have SSL libraries in order to run this project. 