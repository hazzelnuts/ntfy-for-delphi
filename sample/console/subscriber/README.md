<div align="center">
  <img src="../../../img/delphi-notify.png">
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

## ⚠ Observations
You must have SSL libraries in order to run this project.