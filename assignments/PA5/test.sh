#!/usr/bin/env bash

# color codes for beautiful output
GREEN='\033[0;32m'
RED='\033[0;31m'
NO_COLOR='\033[0m'

DIFF_ARGS=(-y --width=60 --suppress-common-lines)

SPIM=../../bin/spim
MY_COOLC=./mycoolc
REF_COOLC=../../bin/coolc

# Taking the self-written tests file and examples as input, compare the
# differences between the output of reference lexer and the output of our lexer.
status=0
fail_count=0
for filename in ./tests/*.cl; do
  echo "--------Test using" "$filename" "--------"
  "$MY_COOLC" "$filename" -o tmp && "$SPIM" -file tmp &>refout
  "$REF_COOLC" "$filename" -o tmp && "$SPIM" -file tmp &>myout
  if diff refout myout "${DIFF_ARGS[@]}"; then
    echo -e "${GREEN}PASSED${NO_COLOR}"
  else
    echo -e "${RED}FAILED${NO_COLOR}"
    status=1
    fail_count=$((fail_count + 1))
  fi
done

echo "--------SUMMARIZE--------"
if [ "$status" -eq 0 ]; then
  echo -e "${GREEN}ALL OF THE TESTS PASSED${NO_COLOR}"
else
  echo -e "${RED}${fail_count} OF THE TESTS FAILED${NO_COLOR}"
fi

rm -rf refout myout tmp
exit $status
