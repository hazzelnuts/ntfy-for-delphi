<div align="center">
  <img src="./img/delphi-notify.png">
</div>

<br/>

Visit [docs.ntfy.sh](https://docs.ntfy.sh/) to get started with documentation. Ntfy was made by [Philipp C. Heckel](https://github.com/binwiederhier) as an open source project. Consider [leaving a star ](https://github.com/binwiederhier/ntfy) in this awesome project as well. As [Philipp C. Heckel](https://github.com/binwiederhier) stated, it will always be free, so any kind of support to afford costs help with cloud hosting will be warmly received. 
 
<br/>

# ⚡️ Delphi Ntfy 

Delphi Ntfy it's a friendly library to work with [ntfy.sh](https://docs.ntfy.sh/) endpoints in Delphi. It allows you to publish messages and subscribe to topic channels to receive those same messages. It's pretty similar to a websocket used to push instant notifications.

# ⚙️ Installation

You can add ```src``` folder to your library path or search path. Or you can simply use boss dependency manager to install:

``` cmd
  boss install github.com/p-samuel/delphi-notify
```

# ⚡️ Quickstart

Create a message notification to a specif topic(channel). The topic's name you choose will become a public url, so remember to make it difficult to guess it.

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

You can subscribe to a topic through several ways. For instace, the [Web App](https://ntfy.sh/app), [Android](https://docs.ntfy.sh/subscribe/phone/), [CLI](https://docs.ntfy.sh/subscribe/cli/) or you can use the [VCL Sample](https://github.com/p-samuel/delphi-notify/tree/main/sample/vcl). There is a demonstration in the VCL samples showing you how to use Delphi Ntfy into your project.

``` pascal

uses
  Notify;

begin

  Ntfy.Subscribe('your-very-secret-topic');

end;

```

# ⚙️ Supported Editions

|       Version        |  Supported   |       Version        |  Supported   |  
|----------------------|:------------:|----------------------|:------------:| 
| Delphi 5             |      ❌      |  Delphi 7             |      ❌     |
| Delphi XE            |      ❓      |  Delphi 11 Alexandria |      ❓     |
| Delphi 10 Seattle    |      ❓      |  Delphi 10.1 Berlin   |      ❓     |
| Delphi 10.2 Tokyo    |      ❓      |  Delphi 10.3 Rio      |      ✔      |
| Delphi 10.4 Sydney   |      ✔       | 

It has not yet been tested into some Delphi versions. You can help to find out informing with a PR update to this README table file. In the PR, change the flag to a ✔ or a ❌.

# 🛑 Limitations

Support to notifications that contains ```broadcast``` actions has not been implemented. Some advanced specific resources has likewise not been implemented as well.

|    Action   |  Support |
|-------------|:--------:|
| view        | ✔        |
| broadcast   | ❌       |
| http        | ✔        |

# 🌱 Consider Contributing

Delphi Ntfy it's an open source project under the MIT license. Feel free to use or contribute! 

