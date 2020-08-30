```sh
curl 'https://www.notion.so/api/v3/enqueueTask' \
  -H 'authority: www.notion.so' \
  -H 'pragma: no-cache' \
  -H 'cache-control: no-cache' \
  -H 'x-notion-active-user-header: 00000000-0000-0000-0000-000000000000' \
  -H 'content-type: application/json' \
  -H 'user-agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/84.0.4147.135 Safari/537.36' \
  -H 'notion-client-version: 23.2.10' \
  -H 'accept: */*' \
  -H 'origin: https://www.notion.so' \
  -H 'sec-fetch-site: same-origin' \
  -H 'sec-fetch-mode: cors' \
  -H 'sec-fetch-dest: empty' \
  -H 'referer: https://www.notion.so/Test-one-block-00000000-0000-0000-0000-000000000000' \
  -H 'accept-language: fr-FR,fr;q=0.9,en-US;q=0.8,en;q=0.7' \
  -H 'cookie: token_v2=...' \
  --data-binary '{"task":{"eventName":"exportBlock","request":{"blockId":"00000000-0000-0000-0000-000000000000","recursive":false,"exportOptions":{"exportType":"markdown","timeZone":"Europe/Paris","locale":"en"}}}}' \
  --compressed

{"taskId":"37c96933-264d-430a-8903-90558fb94431"}
```

```sh
curl 'https://www.notion.so/api/v3/getTasks' \
  -H 'authority: www.notion.so' \
  -H 'pragma: no-cache' \
  -H 'cache-control: no-cache' \
  -H 'x-notion-active-user-header: 00000000-0000-0000-0000-000000000000' \
  -H 'content-type: application/json' \
  -H 'user-agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/84.0.4147.135 Safari/537.36' \
  -H 'notion-client-version: 23.2.10' \
  -H 'accept: */*' \
  -H 'origin: https://www.notion.so' \
  -H 'sec-fetch-site: same-origin' \
  -H 'sec-fetch-mode: cors' \
  -H 'sec-fetch-dest: empty' \
  -H 'referer: https://www.notion.so/Test-one-block-ca0c8c0fbfc34052a17835fbffccfce5' \
  -H 'accept-language: fr-FR,fr;q=0.9,en-US;q=0.8,en;q=0.7' \
  -H 'cookie: token_v2=...' \
  --data-binary '{"taskIds":["00000000-0000-0000-0000-000000000000"]}' \
  --compressed

{
  "results": [
    {
      "id": "00000000-0000-0000-0000-000000000000",
      "eventName": "exportBlock",
      "request": {
        "blockId": "00000000-0000-0000-0000-000000000000",
        "recursive": false,
        "exportOptions": {
          "exportType": "markdown",
          "timeZone": "Europe/Paris",
          "locale": "en"
        }
      },
      "actor": {
        "table": "notion_user",
        "id": "00000000-0000-0000-0000-000000000000"
      },
      "state": "success",
      "status": {
        "type": "complete",
        "pagesExported": 1,
        "exportURL": "https://link.zip%22"
      }
    }
  ]
}
```
