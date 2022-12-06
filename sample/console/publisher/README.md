<div align="center">
  <img src="./img/delphi-notify.png">
</div>

## ⚠ Observations
Never forget to include in which topic are you posting. You must also have SSL libraries in order to this project to work correctly.

``` pascal
  Ntfy.Notification(
    New.Notification
      .Topic('your-very-secret-topic')
  );
```

<div align="center">

# Console Publishing Samples 🍗

</div>


## Simple message

``` pascal
  Ntfy.Notification(
    New.Notification
      .Title('Simple message sent')
      .MessageContent('A message body...')
  );
```

## Setup Priority Levels

``` pascal
  Ntfy.Notification(
    New.Notification
      .Priority(TNotifyPriority.HIGH)
  );
```

## Emojis/Tags

``` pascal
  Ntfy.Notification(
    New.Notification
      .Tags(['partying_face', 'warning', 'rotating_light'])
  );
```

## Actions Buttons (view)

``` pascal
  Ntfy.Notification(New.Notification
    .Action(New.Action
      .&Type(TNotifyActionType.VIEW)
      .Url('geo:40.765819,-73.975866')
      .&Label('Open Google Maps'))
  );
```

## Actions Buttons (http)

``` pascal
  Ntfy.Notification(New.Notification
    .Action(New.Action
      .&Type(TNotifyActionType.HTTP)
      .Url('https://viacep.com.br/ws/01001000/json/')
      .&Label('Send GET request to viacep')
      .Method('GET'))
  );
```

## File attachments

``` pascal
  Ntfy.Notification(
    New.Notification
      .FilePath('..\img\delphi-notify.png')
  );
```

## URL attachments

``` pascal
  Ntfy.Notification(
    New.Notification
      .Attach('https://i.picsum.photos/id/1002/200/200.jpg')
  );
```

## URL icons attachments

``` pascal
  Ntfy.Notification(
    New.Notification
      .Icon('https://communityIcon_xnt6chtnr2j21.png')
  );
```

## Email notifications

``` pascal
  Ntfy.Notification(
    New.Notification
      .Title('Email test')
      .MessageContent('Emails with a title and a message')
      .Email('someemail@gmail.com')
  );
```

## Other configs for self-hosting

``` pascal
  // Username and password for protected topics
  // Careful! Username and password are not 
  // encrypted, only encoded! Use HTTPS
  Ntfy.UserName('username').Password('password');

  // Disabling Firebase will significantly increase 
  // the amount of time messages are delivered 
  // in Android
  Ntfy.DisableFireBase(True);

  // Disabling cache will cause messages not to 
  // be delivered to whom is not subscribed
  Ntfy.Cache(False);
```
