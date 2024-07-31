#!/bin/sh

set -e

if [ "x$FORCE_COLOR" != "x" ] || ( [ -n $(tput colors) ] && [ $(tput colors) -ge 8 ] )
then
	COLOR_NORMAL=$(tput sgr0)
	COLOR_RED=$(tput setaf 1)
	COLOR_GREEN=$(tput setaf 2)
fi

# if the template file exists, or diffs are mandatory, run diff
if [ "x$TEST_DIFF_OPTIONAL" = "x" ] || [ -f "$1" ]
then
    if ! diff "$1" "$2" > /dev/null 2> /dev/null
    then
        echo ${COLOR_RED}FAILED${COLOR_NORMAL}
        diff "$1" "$2"
    fi
fi

echo ${COLOR_GREEN}OK${COLOR_NORMAL}
