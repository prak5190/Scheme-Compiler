#!/bin/bash

if [ "$NUMASSIGNS" == "" ]; then 
  NUMASSIGNS=3
fi

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
  echo; 
  echo "========================================"
  echo "Running tests on branch a$n"
  echo "========================================"
  git checkout a"$n"sol
  make clean
  make grammars

  # We don't use "load_and_test.ss" because we want to exit immediately with an error code:
  echo '(import (Framework testing)) (exit (if (test-all) 0 1))' | scheme

  if [ $SCHEMEONLY == 0 ]; then 
     make haskell
  fi
}

for ((i=1; i <= NUMASSIGNS; i++)); do 
  test_branch $i;
done
