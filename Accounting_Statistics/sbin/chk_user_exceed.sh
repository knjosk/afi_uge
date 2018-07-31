#!/usr/bin/bash

EXCEEDED_FILE=/opt/uge/Accounting_Statistics/logs/accounting/user_used_pm.csv
CHK_EXCEED_CMD=/opt/uge/Accounting_Statistics/sbin/chk_exceed.py

${CHK_EXCEED_CMD} ${EXCEEDED_FILE}

