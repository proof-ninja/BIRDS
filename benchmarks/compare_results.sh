#!/bin/sh

# $ ./compare_results.sh <DIR1> <DIR2> list
# $ ./compare_results.sh <DIR1> <DIR2> check <REL-PATH> [<DIFF-CMD>]
# $ ./compare_results.sh <DIR1> <DIR2> check_all

if [ $# -lt 2 ]; then
    echo "invalid number of arguments"
fi
LEFT=$1
RIGHT=$2
CMD=${3:-check_all}

case $CMD in
    list)
        echo "$LEFT:"
        for FILE in $(ls "$LEFT"/gensql/*/*.sql); do
            RELPATH="${FILE##"$LEFT/"}"
            echo "* $RELPATH"
        done
        ;;
    check)
        RELPATH=$4
        DIFF=${5:-diff}
        echo "Compare $LEFT and $RIGHT"
        echo "=======<$RELPATH>======="
        "$DIFF" "$LEFT/$RELPATH" "$RIGHT/$RELPATH"
        ;;
    check_all)
        DIFF=${5:-diff}
        echo "Compare $LEFT and $RIGHT"
        for FILE in $(ls "$LEFT"/gensql/*/*.sql); do
            RELPATH="${FILE##"$LEFT/"}"
            echo "=======<$RELPATH>======="
            "$DIFF" "$LEFT/$RELPATH" "$RIGHT/$RELPATH"
        done
        ;;
    *)
        echo "invalid command ($CMD)"
        ;;
esac
