<div align="center">
  <img src="../img/delphi-notify.png" width="65%">
</div>

## Test cases 

DUnit tests for basic methods. You can deploy a self-hosted server via Docker images. Here you find a demo docker compose and server yml file.

## Environment
Run these steps in your enviroment:

``` cmd
docker compose create
docker cp server.yml ntfy:/etc/ntfy
docker compose up
```

## Installation
* Open ```sample/Examples.grouppoj``` and build ```NtfyForDelphiTests``` project.
* (Optional) Paste the SSL libraries into the executable's folder ```bin``` if you are using ```NTFY_HTTP_INDY```.  

## DUnit Tests
* Access http://localhost:80 in your browser and subscribe to ```notify-delphi-integration-8jh27d```. 
* Run ```bin\NtfyForDelphiTests.exe```. You should receive notifications sent from the test runner.

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