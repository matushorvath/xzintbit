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

for input in $(ls test/*.s) ; do
    id="$(basename -s .s $input)"

    output_object="$outdir/$id.o"
    output_binary="$outdir/$id.input"
    expect_object="test/$id.o"
    expect_binary="test/$id.input"

    echo -n "Test $id: "

    ./vm.sh bin/as.input < "$input" > "$output_object" 2> /dev/null && echo .$ | cat "$output_object" - | ./vm.sh bin/ld.input > "$output_binary" 2> /dev/null || true

    object_status=0
    diff "$output_object" "$expect_object" > /dev/null 2> /dev/null || object_status=$?

    binary_status=0
    [ ! -e "$expect_binary" ] || diff "$output_binary" "$expect_binary" > /dev/null 2> /dev/null || binary_status=$?

    if [ $object_status = 0 ] && [ $binary_status = 0 ]; then
        echo "${green}OK${normal}"
    else
        echo "${red}FAILED${normal}"
        diff "$output_object" "$expect_object" || true
        [ ! -e "$expect_binary" ] || diff "$output_binary" "$expect_binary" || true
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
