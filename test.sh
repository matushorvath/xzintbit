#!/usr/bin/env bash

set -e

# color support
if test -t 1; then
    colors=$(tput colors)
    if test -n "$colors" && test $colors -ge 8; then
        normal="$(tput sgr0)"
        red="$(tput setaf 1)"
        green="$(tput setaf 2)"
    fi
fi

failed_count=0

outdir=$(mktemp -d)
echo "Output dir: $outdir"

echo

for input in $(ls test/*.s) ; do
    id="$(basename -s .s $input)"
    output="$outdir/$id.input"
    expect="test/$id.input"

    echo -n "Test $id: "

    ./vm.sh src/as.input < "$input" > "$output"
    diff "$output" "$expect" && status=$? || status=$?

    if [ $status = 0 ]; then
        echo "${green}OK${normal}"
    else
        echo "${red}FAILED${normal}"
        failed_count=$((failed_count + 1))
    fi
done

echo

if [ $failed_count = 0 ]; then
    echo "All tests ${green}PASSED${normal}"
elif [ $failed_count = 1 ]; then
    echo "1 test ${red}FAILED${normal}"
else
    echo "${failed_count} tests ${red}FAILED${normal}"
fi
