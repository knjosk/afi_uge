#!/usr/bin/bash

WJOBSTAT_DIR=/opt/uge/Accounting_Statistics/logs/wjobstat
WJOBSTAT=wjobstat.txt
WJOBSTAT_FILE=${WJOBSTAT_DIR}/${WJOBSTAT}

cat ${WJOBSTAT_FILE}
