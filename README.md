<div align="center">
  <img src="./img/delphi-notify.png">
</div>

<br/>

 Visit [docs.ntfy.sh](https://docs.ntfy.sh/) to get started with documentation. Ntfy was made by [Philipp C. Heckel](https://github.com/binwiederhier) as an open source project. Consider [leaving a star ](https://github.com/binwiederhier/ntfy) in this awesome project as well.
 
<br/>

# Delphi Ntfy 

Delphi Ntfy it's a friendly client interface to work with [ntfy.sh](https://docs.ntfy.sh/) endpoints in Delphi. It allows you to publish messages and to subscribe into topic channels.

# Installation

You can add ```src``` folder to your library path or search path. Or you can simply use boss dependency manager to install:

``` cmd
  boss install github.com/p-samuel/delphi-notify
```

# Publish a message

Create a topic to notify subcribers. As the topic's name will become a public url, remember to make it difficult to guess it.

``` pascal

uses
  Notify;

begin

  Ntfy.Notification(
    New.Notification
      .Topic('your-very-secret-topic')
      .Title('🐶 Parking with Tobby')
      .MessageContent('Get Tobby today at 16:00hs ') 
  );

  Ntfy.Publish;

end;

```

# Subscribe to a topic

You can subscribe to a topic through several ways. For instace, the [Web App](https://ntfy.sh/app), [Android](https://docs.ntfy.sh/subscribe/phone/), [CLI](https://docs.ntfy.sh/subscribe/cli/), or you can use the [VCL Sample](https://github.com/p-samuel/delphi-notify/tree/main/sample/vcl). There is a demonstration in the VCL sample project showing you how to use Delphi Ntfy integrated to your project.

``` pascal

uses
  Notify;

begin

  Ntfy.Subscribe('your-very-secret-topic');

end;

```

# Supported Editions
I didn't have time to test, but it should work on every Delphi XE Editions upward. Plannings to support older versions will have to be implemented in another repo.

|       Version        |  Supported   |       Version        |  Supported   |  
|----------------------|:------------:|----------------------|:------------:| 
| Delphi 5             |      ❌     |  Delphi 7             |      ❌     |
| Delphi XE            |      ✔      |  Delphi 11 Alexandria |      ✔      |
| Delphi 10 Seattle    |      ✔      |  Delphi 10.1 Berlin   |      ✔      |
| Delphi 10.2 Tokyo    |      ✔      |  Delphi 10.3 Rio      |      ✔      |
| Delphi 10.4 Sydney   |      ✔      | 


