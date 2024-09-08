<div align="center">
  <img src="../../../img/delphi-notify.png" width="65%">
</div>

<div align="center">

# Subscriber Console Sample

</div>

## Subscribe to a topic

Subscription is event based. 

``` pascal
  Ntfy.Subscribe('your-very-secret-topic', 
    procedure (AEvent: INotifyEvent);
    begin
      WriteLn('You received a message: ' + AEvent.MessageContent)
  end);

```

Short poll

``` pascal
  Ntfy.Poll(True);
  Ntfy.Subscribe('your-very-secret-topic', CallBack);
```

Fetching (duration, unix time or id)

``` pascal
  Ntfy.Since('2hs')
  Ntfy.Subscribe('your-very-secret-topic', CallBack);
```


Scheduled only

``` pascal
  Ntfy.Scheduled(True)
  Ntfy.Subscribe('your-very-secret-topic', CallBack);
```

## Filtering

``` pascal
  Ntfy.Filter(TNotifyFilter.ID, 'xY77dh389');
  Ntfy.Filter(TNotifyFilter.TITLE, 'some title');
  Ntfy.Filter(TNotifyFilter.MESSAGECONTENT, 'some text');
  Ntfy.Filter(TNotifyFilter.TAGS, 'some text');
  Ntfy.Filter(TNotifyFilter.PRIORITY, IntToStr(Ord(TNotifyPriority.MAX)));      
```