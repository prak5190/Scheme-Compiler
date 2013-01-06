#! /bin/bash

# if (( $# < 1 )); then
#   echo >&2 "Usage: $0 pattern [extra git log args]"
#   exit 1
# fi

pattern=Merge

git log --format=%H  |
  grep -v -f <(git log --format=%H "--grep=$pattern") |
  git log --pretty --stdin --no-walk $@
