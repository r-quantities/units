#!/bin/bash

if [ "$#" -ne 1 ]; then
  echo "Usage: ./run.sh <num_workers>"
  exit 0
fi

podman build . -t revdep-units
podman run --rm -it -v $PWD/../:/mnt:z -w /mnt -e workers=$1 revdep-units
