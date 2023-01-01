<div align="center">
  <img src="./img/delphi-notify.png">
</div>

<br/>

 Ntfy was made by [Philipp C. Heckel](https://github.com/binwiederhier) as an open source project. Consider leaving a star in [his project](https://github.com/binwiederhier/ntfy). As [Philipp C. Heckel](https://github.com/binwiederhier) stated, it will always be free, so any kind of support to afford costs help with cloud hosting will be warmly received. Visit [docs.ntfy.sh](https://docs.ntfy.sh/) to get started with documentation.
 
<br/>

# 🔔 Ntfy for Delphi 

Ntfy for Delphi it's a friendly library to work with [ntfy.sh](https://docs.ntfy.sh/) endpoints in Delphi. It allows you publishing messages and subscribing on topics to receive instant notifications. 

# ⚙️ Installation

You need to add ```src``` folder to your library path or search path. 

# ⚡️ Quickstart

Push a notification on a specif topic. Topics are the same as channels and the topic's name you choose will become a public url, so remember to make difficult to guess it.

``` pascal

uses
  Notify;

begin
  Ntfy.Notification(
    New.Notification
      .Topic('your-very-secret-topic')
      .Title('🐶 Parking with dog')
      .MessageContent('Today at 16:00hs ') 
  );

  Ntfy.Publish;
end;

```

# 💬 Subscribe to a topic

You can subscribe to a topic through several ways. For instance, the [Web App](https://ntfy.sh/app), [Android](https://docs.ntfy.sh/subscribe/phone/), [CLI](https://docs.ntfy.sh/subscribe/cli/) or you can use this library as follows: 

``` pascal
uses
  Notify;

begin
  Ntfy.Subscribe('your-very-secret-topic', 
    procedure (AEvent: INotifyEvent);
    begin
      WriteLn('You received a message: ' +  AEvent.MessageContent)
    end);
end;

```

There is a [VCL Sample](https://github.com/p-samuel/delphi-notify/tree/main/sample/vcl) demonstration showing you how to use Delphi Ntfy subscription mechanism into your project.


# ⚙️ Supported Editions

|       Version        |  Supported   |       Version        |  Supported   |  
|----------------------|:------------:|----------------------|:------------:| 
| Delphi 5             |      ❌      |  Delphi 7             |      ❌     |
| Delphi XE            |      ❓      |  Delphi 11 Alexandria |      ❓     |
| Delphi 10 Seattle    |      ❓      |  Delphi 10.1 Berlin   |      ❓     |
| Delphi 10.2 Tokyo    |      ❓      |  Delphi 10.3 Rio      |      ✔      |
| Delphi 10.4 Sydney   |      ✔       | 

It hasn't been tested in some Delphi versions yet. You can help finding out informing with a PR update to this README table file. In the PR, change the flag to a ✔ or a ❌.

# ⛔ Limitations

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

# 🔗 Dependencies

Ntfy for Delphi uses a few libraries in the messages subscription and publishing mechanism. There is no need to install. The respective credit adviced.

* [NxHorizon](https://github.com/dalijap/nx-horizon) by Dalija Prasnikar. 
* [Indy10](https://github.com/IndySockets/Indy) by IndySockets.
* [JsonToDelphiClass](https://github.com/PKGeorgiev) by Petar Georgiev.


# 🌱 Consider Contributing

Ntfy for Delphi it's an open source project under the MIT license. Feel free to use or contribute! 

