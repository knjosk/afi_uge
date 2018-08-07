#!/usr/bin/bash

export LANG=C
export LC_ALL=C

MONTH=`date +%m`
YEAR=`date +%Y`

echo ${MONTH}
echo ${YEAR}

a=`expr ${MONTH} + 1`
echo $a

if [ ${MONTH} -eq 8 ]; then
    LAST=`expr ${MONTH} - 1`
    NUM=`seq -f %02g 3 $LAST`
    echo $NUM
fi
