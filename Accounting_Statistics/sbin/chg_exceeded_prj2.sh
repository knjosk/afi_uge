#!/usr/bin/bash

PRJ_EXCEEDED_FILE="/opt/uge/Accounting_Statistics/logs/accounting/prj_exceeded_pm.csv"

if [ -s ${PRJ_EXCEEDED_FILE} ]; then
    CHG_PRJ_OTICHET_CMD=/opt/uge/Accounting_Statistics/sbin/chg_prj_oticket.sh
    cat ${PRJ_EXCEEDED_FILE} | while read line
    do
        ${CHG_PRJ_OTICHET_CMD} $line 0
    done
fi

