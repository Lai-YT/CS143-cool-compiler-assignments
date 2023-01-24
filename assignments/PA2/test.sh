#!/usr/bin/env bash

# color codes for beautiful output
GREEN='\033[0;32m'
RED='\033[0;31m'
NO_COLOR='\033[0m'

# This script refers to https://github.com/afterthat97/cool-compiler/blob/master/assignments/PA2/judge.sh.

# Taking the self-writtens tests file and examples as input, compare the
# differences between the output of reference lexer and the output of our lexer.
status=0
for filename in ./tests/*.cl ../../examples/*.cl; do
  echo "--------Test using" "$filename" "--------"
  ../../bin/lexer "$filename" >refout
  ./lexer "$filename" >myout
  if diff refout myout -y --width=60 --suppress-common-lines; then
    echo -e "${GREEN}PASSED${NO_COLOR}"
  else
    echo -e "${RED}FAILED${NO_COLOR}"
    status=1
  fi
done

rm -rf refout myout
exit $status
