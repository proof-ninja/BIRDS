#!/bin/sh

# $ ./see_results.sh see_log <DIR>
# $ ./see_results.sh see_err <DIR>
# $ ./see_results.sh diff_log <DIR1> <DIR2>
# $ ./see_results.sh diff_err <DIR1> <DIR2>

if [ $# -lt 2 ]; then
    echo "invalid number of arguments"
fi

CMD=$1

case $CMD in
    see_log)
        DIR=$2
        for FILE in "$DIR"/compile/*/*.log; do
            echo "=======<$FILE>======="
            cat "$FILE"
        done
        ;;
    see_err)
        DIR=$2
        for FILE in "$DIR"/compile/*/*.err; do
            echo "=======<$FILE>======="
            cat "$FILE"
        done
        ;;
    diff_log)
        LEFT=$2
        RIGHT=$3
        for FILE in "$LEFT"/compile/*/*.log; do
            RELPATH="${FILE##"$LEFT/"}"
            echo "=======<$RELPATH>======="
            diff -u "$LEFT/$RELPATH" "$RIGHT/$RELPATH"
        done
        ;;
    diff_err)
        LEFT=$2
        RIGHT=$3
        for FILE in "$LEFT"/compile/*/*.err; do
            RELPATH="${FILE##"$LEFT/"}"
            echo "=======<$RELPATH>======="
            diff -u "$LEFT/$RELPATH" "$RIGHT/$RELPATH"
        done
        ;;
    *)
        echo "invalid command ($CMD)"
        ;;
esac
