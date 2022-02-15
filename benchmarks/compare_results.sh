#!/bin/sh

if [ $# -ne 2 ]; then
    echo "invalid number of arguments"
fi
LEFT=$1
RIGHT=$2

echo "Compare $LEFT and $RIGHT"

for FILE in $(ls "$LEFT"/gensql/*/*.sql); do
    RELPATH="${FILE##"$LEFT/"}"
    echo "=======<$RELPATH>======="
    icdiff "$LEFT/$RELPATH" "$RIGHT/$RELPATH"
done
