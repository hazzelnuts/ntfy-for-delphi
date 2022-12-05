<div align="center">
  <img src="./img/delphi-notify.png">
</div>

<br/>

 Visit [ntfy.sh](https://docs.ntfy.sh/) to get started with documentation. Ntfy was made by [Philipp C. Heckel](https://github.com/binwiederhier) as an open source project. Consider [leaving a star ](https://github.com/binwiederhier/ntfy) in this awesome project as well.
 
<br/>

# Delphi Ntfy 

Delphi Ntfy it's a friendly client interface to work with [ntfy.sh](https://docs.ntfy.sh/) endpoints in Delphi. It allows you to publish messages and to subscribe into topic channles.

# Instalation

You can add ```src``` folder to your library path or search path. Or you can simply use boss dependency manager to install:

``` cmd
  boss install github.com/p-samuel/delphi-notify
```

# Publish a message

You can create a topic to notify subcribers. As the topic's name will become a public url, remember to make it difficult to guess it.

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

