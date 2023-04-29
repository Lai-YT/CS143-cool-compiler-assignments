#!/usr/bin/env bash

# color codes for beautiful output
GREEN='\033[0;32m'
RED='\033[0;31m'
NO_COLOR='\033[0m'

DIFF_ARGS=(-y --width=60 --suppress-common-lines)

# Taking the self-written tests file and examples as input, compare the
# differences between the output of reference lexer and the output of our lexer.
status=0
for filename in ./tests/class/*.cl ./tests/method/*.cl ./tests/*.cl; do
  echo "--------Test using" "$filename" "--------"
  ./refsemant "$filename" &>refout
  ./mysemant "$filename" &>myout
  if diff refout myout "${DIFF_ARGS[@]}"; then
    echo -e "${GREEN}PASSED${NO_COLOR}"
  else
    echo -e "${RED}FAILED${NO_COLOR}"
    status=1
  fi
done

echo "--------SUMMARIZE--------"
if [ "$status" -eq 0 ]; then
  echo -e "${GREEN}ALL OF THE TESTS PASSED${NO_COLOR}"
else
  echo -e "${RED}SOME OF THE TESTS FAILED${NO_COLOR}"
fi

rm -rf refout myout
exit $status
