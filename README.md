<div align="center">
  <img src="./img/delphi-notify.png" width="80%">
</div>

 Ntfy was made by [Philipp C. Heckel](https://github.com/binwiederhier). Consider leaving a star in [his project](https://github.com/binwiederhier/ntfy). As [Philipp C. Heckel](https://github.com/binwiederhier) stated, this service will stay free, so any kind of support to afford costs help with cloud hosting will be warmly received. You can also self-host ntfy server. Visit [docs.ntfy.sh](https://docs.ntfy.sh/) to get started with documentation.

## 🔔 Ntfy for Delphi 

Ntfy for Delphi it's a friendly library to work with [ntfy.sh](https://docs.ntfy.sh/) servers in Delphi. Allows you publishing messages and subscribing to topics to receive instant notifications. 

## ⚙️ Manual Installation

You need to add ```src``` folder to your library path or search path. 

## ⚙️ Boss Installation

```
  boss install github.com/hazzelnuts/ntfy-for-delphi
```

## ⚡️ Quickstart

Push a notification on a specific topic. Topics are the same as channels and the name you choose will become a public url, so remember to make difficult to guess it.

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

## 💬 Subscribe to a topic

You can subscribe to a topic through several ways. For instance, the [Web App](https://ntfy.sh/app), [Android](https://docs.ntfy.sh/subscribe/phone/), [CLI](https://docs.ntfy.sh/subscribe/cli/) or you can use this library as follows: 

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

There is a [VCL Sample](https://github.com/p-samuel/delphi-notify/tree/main/sample/vcl) demonstration showing you how to use Delphi Ntfy subscription mechanism into your project.

## ⚙️ Supported Version & Platforms

<img src="https://img.shields.io/badge/Delphi%20Supported%20Version%20-v10.1%2B%20-blue"></img>
<img src="https://img.shields.io/badge/Supported%20Platforms-Win32%20%26%20Win64-green"></img>

It hasn't been tested in some Delphi versions yet. You can help finding out informing with a PR update to this README file. In the PR, inform the selected badged.

## ⛔ Limitations

Support to notifications that contains ```broadcast``` actions has not been implemented. Likewise, some advanced specific resources has not yet been implemented as well.

|    Action   |  Support |
|-------------|:--------:|
| view        | ✔        |
| broadcast   | ❌       |
| http        | ✔        |

|    Subscription Type    |  Support |
|-------------------------|:---------:|
| json                    | ✔        |
| raw                     | ❌       |
| sse                     | ❌       |
| websocket               | ❌       |

## 🔗 Dependencies

Ntfy for Delphi uses a few libraries in the messages subscription and publishing mechanism. There is no need to install. The respective credit adviced.

* [NxHorizon](https://github.com/dalijap/nx-horizon) by Dalija Prasnikar. 
* [Indy10](https://github.com/IndySockets/Indy) by IndySockets.
* [JsonToDelphiClass](https://github.com/PKGeorgiev) by Petar Georgiev.
* NetHTTP (On development 🛠)

## 🌱 Consider Contributing

Ntfy for Delphi it's an open source project under the MIT license. Feel free to use or contribute! 

## ⚠ Observations
For the moment this library uses OpenSSL and is necessary to have it in the executable's folder. Support for NetHTTP is on work.

## 🚀 Basic ntfy server
For purposes of demonstration, I created a small demo showing you how to setup your own server using railway, which is only one of many alternatives to you self-host ntfy server. Click on the link bellow.

> https://youtu.be/L49wM84ivzE

<div width="200px"> 

![Screen Shot 2023-03-24 at 17 56 41](https://user-images.githubusercontent.com/53358247/227638278-c553deda-17fb-4a23-9795-760902010dfc.png)

</div>
