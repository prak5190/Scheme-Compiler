#!/bin/bash

NUMASSIGNS=3

set -e

SCHEMEONLY=0
while [ $# -gt 0 ]; do
  case "$1" in
    --schemeonly)
      shift; 
      SCHEMEONLY=1
      ;; 
  esac
done

function test_branch() {
  n=$1
  echo; echo "Running tests on branch a$n"
  echo "========================================"
  git checkout a$n

  # TODO: Need a way to exit with a meaningful error code:

  echo '(exit)' | make scheme
}

for ((i=1; i <= NUMASSIGNS; i++)); do 
  test_branch $i;
done
