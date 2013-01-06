#!/bin/bash

NUMASSIGNS=3

cat <<EOF

If everything is in good order we should be able to merge every a{N-1}
branch into a{N} without changes, and merge the public a{N} into the
corresponding private one without changes.  This script will now check
that that is the case.

This script can also be called with "--commit" as an argument to
actually pull the changes through the different branches of the
compiler repository.

Finally, the script can be called with "--public" for handling the
public framework rather than the private (solutions) repositiory.

EOF

set -e

NOCOMMIT="--no-commit"
PUBLIC=0
while [ $# -gt 0 ]; do
  case "$1" in
    --commit)
      shift; 
      NOCOMMIT=""
      ;; 
    --public)
      shift;
      PUBLIC=1
      ;;
  esac
done

function check_no_diff() 
{
   diffs=`git status -s -uno`
   if [ "$diffs" != "" ]; then 
     echo "Error: diffs from merge!: "
     echo $diffs
     exit 1
   fi
}

function check_merge() {
  branch=$1
  echo git merge $branch $NOCOMMIT
  git merge $branch $NOCOMMIT
  check_no_diff
}

function check_pub_branch() {
  n=$1
  echo; echo "Checking public framework branch a$n"
  echo "========================================"
  #  set -x
  git co a"$n"

  if [ $n -gt 1 ]; then    
    check_merge a"$((n-1))"
  else
    echo "Merging common branch, the shared root of all:"
    check_merge common
  fi
  #  set +x
}

function check_sol_branch() {
  n=$1
  echo; echo "Checking solution branch a"$n"sol"
  echo "========================================"

  git co a"$n"sol

  # Merge the public one first:
  check_merge remotes/framework/a$n

  if [ $n -gt 1 ]; then    
    check_merge a"$((n-1))"sol
  fi
}

echo
echo "--------------------------------------------------------------------------------"
echo "--               First, updating public framework branches:                   --"
echo "--------------------------------------------------------------------------------"
for ((i=1; i <= NUMASSIGNS; i++)); do 
  check_pub_branch $i;
done

echo
echo "--------------------------------------------------------------------------------"
echo "--              Second, updating private solution branches:                   --"
echo "--------------------------------------------------------------------------------"
for ((i=1; i <= NUMASSIGNS; i++)); do 
  check_sol_branch $i;
done
