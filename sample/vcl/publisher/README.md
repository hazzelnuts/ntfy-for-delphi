<div align="center">

# VCL Samples (Publishing) 🧪

</div>

Compile this project and explore the VCL sample to demonstrate how publishing messages works.

<div align="center">
  <img src="./img/vcl-sample.PNG">
</div>

Also check this [link](https://github.com/p-samuel/delphi-notify/tree/dev-psamuel/sample/console/publisher) for some interesting resources. 

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
