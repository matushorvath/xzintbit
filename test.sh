#!/usr/bin/env bash

set -e

echo

# color support
if test -n "$FORCE_COLOR" || test -t 1; then
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

for expect in $(ls test/*.o) ; do
    id="$(basename -s .o $expect)"

    echo -n "Test compiling $id: "

    input="test/$id.s"
    output="$outdir/$id.o"

    ./vm.sh bin/as.input < "$input" > "$output" 2> /dev/null || true
    diff "$output" "$expect" > /dev/null 2> /dev/null && status=$? || status=$?

    if [ $status = 0 ] && [ $status = 0 ]; then
        echo "${green}OK${normal}"
    else
        echo "${red}FAILED${normal}"
        diff "$output" "$expect" || true
        failed_count=$((failed_count + 1))
    fi
done

for expect in $(ls test/*.input) ; do
    id="$(basename -s .input $expect)"

    echo -n "Test linking $id: "

    inputs=$(find test -name $id.o -o -name $id.*.o)
    output="$outdir/$id.input"

    echo .$ | cat ${inputs[@]} - | ./vm.sh bin/ld.input > "$output" 2> /dev/null || true
    diff "$output" "$expect" > /dev/null 2> /dev/null && status=$? || status=$?

    if [ $status = 0 ] && [ $status = 0 ]; then
        echo "${green}OK${normal}"
    else
        echo "${red}FAILED${normal}"
        diff "$output" "$expect" || true
        failed_count=$((failed_count + 1))
    fi
done

echo

if [ $failed_count = 0 ]; then
    echo "All tests ${green}PASSED${normal}"
elif [ $failed_count = 1 ]; then
    echo "1 test ${red}FAILED${normal}"
    exit 1
else
    echo "${failed_count} tests ${red}FAILED${normal}"
    exit 1
fi
