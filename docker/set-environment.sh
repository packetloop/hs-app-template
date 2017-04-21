#!/bin/bash

if [ -d ~/.mayhem ]; then
  for file in ~/.mayhem/*.json; do
    $(jq -r 'to_entries | map("export "+.key + "=" + (.value | tostring)) | .[]' < ${file})
  done
fi
