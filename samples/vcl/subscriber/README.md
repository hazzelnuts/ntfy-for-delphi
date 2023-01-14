<div align="center">

# VCL Samples (Subscribing) 🧪

</div>

Compile this project to explore the VCL sample showing how subscribing to topics works.

<div align="center">
  <img src="./img/subscriber-vcl.png">
</div>

Also check this [link](https://github.com/p-samuel/delphi-notify/tree/dev-psamuel/sample/console/publisher) for some interesting resources. 


## Subscribing to a topic

Subscription mechanism it's event based.

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

## ⚙ Self-hosted server (optional for this sample)
Install Docker in your machine and run these commands after:

``` cmd
cd delphi-notify\tests
docker compose create
docker cp .\server.yml ntfy:/etc/ntfy
docker compose up
```

## ⚠ Observations
You must have SSL libraries in order to run this project.