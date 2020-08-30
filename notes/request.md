```sh
curl 'https://www.notion.so/api/v3/syncRecordValues' \
  -H 'authority: www.notion.so' \
  -H 'pragma: no-cache' \
  -H 'cache-control: no-cache' \
  -H 'x-notion-active-user-header: 29053777-3cc2-4156-9a69-561936f8a9e5' \
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
  --data-binary '{"recordVersionMap":{"notion_user":{"29053777-3cc2-4156-9a69-561936f8a9e5":-1},"user_settings":{"29053777-3cc2-4156-9a69-561936f8a9e5":-1},"user_root":{"29053777-3cc2-4156-9a69-561936f8a9e5":-1},"block":{"ca0c8c0f-bfc3-4052-a178-35fbffccfce5":-1}}}' \
  --compressed
```
