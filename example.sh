#!/bin/bash -x

id=$(curl \
  --data '{"name":"foo"}' \
  --request POST \
  --silent \
  'http://localhost:8080/stopwatches' | jq -r '.id')

curl \
  --request GET \
  --silent \
  "http://localhost:8080/stopwatches/${id}" | jq -r -S '.'

sleep 3

curl \
  --request GET \
  --silent \
  "http://localhost:8080/stopwatches/${id}" | jq -S '.'

curl \
  --request DELETE \
  --silent \
  "http://localhost:8080/stopwatches/${id}"

curl \
  --request GET \
  --silent \
  "http://localhost:8080/stopwatches/${id}" | jq -S '.'
