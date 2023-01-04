<div align="center">
  <img src="../../../img/delphi-notify.png" width="65%">
</div>

<div align="center">

# Console Samples (Subscribing) 🧪

</div>

## Subscribing to a topic

Subscription mechanism is event based. 

``` pascal
uses
  Notify;

begin
  Ntfy.Subscribe('your-very-secret-topic', 
    procedure (AEvent: INotifyEvent);
    begin
      WriteLn('You received a message: ' + AEvent.MessageContent)
    end);
end;

```

## Short poll

``` pascal
uses
  Notify;

begin
  Ntfy.Poll(True);
  Ntfy.Subscribe('your-very-secret-topic', CallBack);
end;
```

## Fetching (duration, unix time or id)

``` pascal
uses
  Notify;

begin
  Ntfy.Since('2hs')
  Ntfy.Subscribe('your-very-secret-topic', CallBack);
end;
```


## Shceduled only

``` pascal
uses
  Notify;

begin
  Ntfy.Scheduled(True)
  Ntfy.Subscribe('your-very-secret-topic', CallBack);
end;
```

## Filtering priorities

``` pascal
  Ntfy.Filter(TNotifyFilter.ID, 'xY77dh389');
  Ntfy.Filter(TNotifyFilter.TITLE, 'some title');
  Ntfy.Filter(TNotifyFilter.MESSAGECONTENT, 'some text');
  Ntfy.Filter(TNotifyFilter.TAGS, 'some text');
  Ntfy.Filter(TNotifyFilter.PRIORITY, IntToStr(Ord(TNotifyPriority.MAX)));      
```

## ⚠ Observations
You must have SSL libraries in order to run this project.