<div align="center">
  <img src="../../../img/delphi-notify.png">
</div>

<div align="center">

# VCL Samples (Subcribing) 🧪

</div>

Compile this project and explore the VCL sample that has been prepared to demonstrate how subscribing on topics works.

<div align="center">
  <img src="./img/subscriber-vcl.png">
</div>

Also check this [link](https://github.com/p-samuel/delphi-notify/tree/dev-psamuel/sample/console/publisher) for some interesting resources. 


## Subscribing to a topic

Subscription mechanism is event based.

``` pascal
uses
  Notify;

begin
  Ntfy.Subscribe('your-very-secret-topic', 
    procedure (AEvent: INotifyEvent)
    begin
      ShowMessage('You received a message: ' + AEvent.Title)
    end);
end;
```

You can define your own callback procedure every time a notification is fired. 

``` pascal
uses
  Notify;

  procedure YourCallBackProcedure(AEvent: INotifyEvent);
  begin
    DoSomethingWith(AEvent);
  end;

begin
  Ntfy.Subscribe('your-very-secret-topic', YourCallBackProcedure);
end;
```

## ⚠ Observations
You must have SSL libraries in order to run this project.