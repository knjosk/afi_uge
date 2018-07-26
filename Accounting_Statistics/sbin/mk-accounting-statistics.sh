#!/usr/bin/bash

export LANG=C
export LC_ALL=C

f_timestamp=`date +%Y%m%d%H%M%S`
MONTH=`date +%Y%m%`

UGE_AS_DIR=/opt/uge/Accounting_Statistics
UGE_ACCOUNTING_DIR=/opt/uge/8.5.5/default/common
QSTAT_CMD=/opt/uge/8.5.5/bin/lx-amd64/qstat

LOGS_DIR="/opt/uge/Accounting_Statistics/logs/accounting"
if [ ! -d ${LOGS_DIR} ]; then
    mkdir -p ${LOGS_DIR}
fi

# Input files
UGE_ACCOUNTING_FILE=${UGE_ACCOUNTING_DIR}/accounting

# Start Time
START_TIME=${MONTH}000000

# Excetute command
EXEC_CMD=${UGE_AS_DIR}/sbin/c_statistics.py

cd ${UGE_AS_DIR}
${EXEC_CMD} ${UGE_ACCOUNTING_FILE} -s ${START_TIME}

