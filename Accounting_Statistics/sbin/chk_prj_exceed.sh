#!/usr/bin/bash

EXCEEDED_FILE=/opt/uge/Accounting_Statistics/logs/accounting/prj_used_chk.csv
CHK_EXCEED_CMD=/opt/uge/Accounting_Statistics/sbin/chk_exceed_prj.py

${CHK_EXCEED_CMD} ${EXCEEDED_FILE}

