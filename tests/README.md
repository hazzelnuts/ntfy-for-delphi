<div align="center">
  <img src="../img/delphi-notify.png" width="65%">
</div>

## Test cases 

Providing some DUnit tests for ntfy main methods. Ntfy disponibilizes a docker image for self-hosting the server. This project contains a docker compose file and a server yml file with settings for deploying locally in your machine.

##  ⚙ Environment
Install Docker in your machine and run these commands after:

``` cmd
docker compose create
docker cp .\server.yml ntfy:/etc/ntfy
docker compose up
```

## ⚡ Installation
* Compile ```sample/Examples.grouppoj``` and select ```NtfyForDelphiTests``` project
* Paste the SSL libraries into the executable's folder ```bin```  

## 🧪 DUnit Tests
* Access http://localhost:80 in your browser and subscribe to ```notify-delphi-integration-8jh27d```. 
* Run ```bin\NtfyForDelphiTests.exe```. You should receive the test messages sent from this project.


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
