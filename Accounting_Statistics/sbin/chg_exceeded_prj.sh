#!/usr/bin/bash

EXCEEDED_FILE=/opt/uge/Accounting_Statistics/etc/prj_exceeded_pm.txt
CHG_PRJ_OTICHET_CMD=/opt/uge/Accounting_Statistics/sbin/chg_prj_oticket.sh

cat ${EXCEEDED_FILE} | while read line
do
	${CHG_PRJ_OTICHET_CMD} $line 0
done
