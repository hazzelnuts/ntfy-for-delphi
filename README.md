<div align="center">
  <img src="./img/delphi-notify.png" width="80%">
</div>

## Ntfy for Delphi 

This library is a client-side implementation for [ntfy](https://ntfy.sh/) server, made in Delphi. You can send and receive instant notifications via http. The maintainer of [ntfy](https://ntfy.sh/) is [Philipp C. Heckel](https://github.com/binwiederhier). As he stated, this service will remain free, and every kind of support to help affording with cloud hosting will be warmly received. You too can self-host a ntfy server. Visit [docs.ntfy.sh](https://docs.ntfy.sh/) to get started and also don't forget to leave a star [on his project](https://github.com/binwiederhier/ntfy). 

## Manual Installation

You need to add ```src``` folder to the library path or search path of your project. 

## Quickstart

You can push notifications in topics. Topics are like channels and the name you choose will become a public url, so make sure to not choose an easy one to guess.

``` pascal

uses
  Notify;

begin
  Ntfy.Notification(
    New.Notification
      .Topic('your-very-secret-topic')
      .Title('⚾ Go to the game')
      .MessageContent('Tomorrow at 10:00hs ') 
  );

  Ntfy.Publish;
end;

```

## Subscribe to a topic

You can subscribe by many ways. For instance, the [Web App](https://ntfy.sh/app), [Android](https://docs.ntfy.sh/subscribe/phone/), [CLI](https://docs.ntfy.sh/subscribe/cli/) or you can use this library this way: 

``` pascal
uses
  Notify;

begin
  Ntfy.Subscribe('your-very-secret-topic', 
    procedure (AEvent: INotifyEvent)
    begin
      WriteLn('You received a message: ' +  AEvent.MessageContent)
    end);
end;

```

## Supported Versions & Platforms

<img src="https://img.shields.io/badge/Delphi%20-12%2B%20-blue"></img>
<img src="https://img.shields.io/badge/Delphi%20-11(⚠)-orange"></img>
<img src="https://img.shields.io/badge/Delphi%20-10.1(⚠)-lightgreen"></img>
<img src="https://img.shields.io/badge/Windows-32%20%26%2064%20bits-green"></img>
<img src="https://img.shields.io/badge/Android-%2064%20bits-green"></img>
<img src="https://img.shields.io/badge/IOS-%2064%20bits (⚠)-red"></img>
<img src="https://img.shields.io/badge/⚠-with limitations-red"></img>

IOS still rely on FCM and "inteligently" decides to kill background/foreground processes when it wants. It was not properly tested yet. All other platforms have been tested and can either publish or maintain a subscription background activity suspended for long hours without having any issues. Refer to these [samples](https://github.com/hazzelnuts/ntfy-for-delphi/tree/main/sample) to learn to use in your project. No tests have been performed on Linux at the present moment.

## Built-in dependencies

Ntfy for Delphi makes use of a few libraries to subscribe and publish. There is no need to install them. Respective credit is awarded to the creators:

* [NxHorizon](https://github.com/dalijap/nx-horizon) by Dalija Prasnikar. 
* [JsonToDelphiClass](https://github.com/PKGeorgiev) by Petar Georgiev.
* [NetHTTP](https://docwiki.embarcadero.com/RADStudio/Rio/en/Using_an_HTTP_Client) native components.
* [Indy10](https://github.com/IndySockets/Indy) by IndySockets.


## Wiki

Check the [wiki](https://github.com/hazzelnuts/ntfy-for-delphi/wiki) page for specific instructions, updates or tutorials. I've created this implementation for passion and curiosity and it will remain an open source project under the MIT license. Feel free to use, contribute and improve this project! 