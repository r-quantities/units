#!/bin/bash

CMD=podman
TAG=revdep-units

show_usage () {
  echo -e "Usage: ${BASH_SOURCE[0]} <command>\n\nCommands:"
  echo -e "  check <num_workers>\tBuild and execute revdep checks"
  echo -e "  summary\t\tShow check summary"
  echo -e "  details <package>\tShow check details for <package>"
  echo -e "  reset\t\t\tReset checks"
  echo -e "  add_broken\t\tAdd all broken packages for rechecking"
  echo -e "  interactive\t\tStart an interactive session"
  exit 0
}

fail  () { echo $@ && exit 1; }
build () { $CMD build . -t $TAG; }
run   () { $CMD run --rm -it -v $PWD/../:/mnt:z -w /mnt $@; }

case $1 in
  check)
    [[ -z ${2+x} ]] && fail "Error: number of workers not provided"
    build && run -e workers=$2 $TAG
    ;;
  summary)
    run $TAG Rscript -e "revdepcheck::revdep_summary()"
    ;;
  details)
    [[ -z ${2+x} ]] && fail "Error: package not provided"
    run $TAG Rscript -e "revdepcheck::revdep_details(revdep='$2')"
    ;;
  reset)
    run $TAG Rscript -e "revdepcheck::revdep_reset()"
    ;;
  add_broken)
    run $TAG Rscript -e "revdepcheck::revdep_add_broken()"
    ;;
  interactive)
    run $TAG R
    ;;
  *)
    show_usage
    ;;
esac

