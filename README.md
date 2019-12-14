# dnet

A REST server and client using [LFE/OTP](http://lfe.io/).

## APIs

```bash
λ curl -X GET "http://localhost:8080/"
{"status": "main"}% 
```

```bash
λ curl -X POST \
  'http://localhost:8080/http' \
  -H 'Content-Type: application/json' \
  -d '{
    "url": "https://jsonip.com/"
}
'
<div>Task scheduled successfully</div>% 
```
