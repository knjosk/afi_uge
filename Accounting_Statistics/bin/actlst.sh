#!/usr/bin/bash

export LANG=C
export LC_ALL=C

UGE_AS_DIR=/opt/uge/Accounting_Statistics
GROUP_USAGE_FILE=${UGE_AS_DIR}/etc/group_used_pm.csv

# Input files
QSTAT_GC_EXT_XML_FILE=$(mktemp "/tmp/tmp.qstat_gc_ext_xml.XXXXX")
QSTAT_R_XML_FILE=$(mktemp "/tmp/tmp.qstat_r_xml.XXXXX")

${QSTAT_CMD} -g c -ext -xml > ${QSTAT_GC_EXT_XML_FILE}
${QSTAT_CMD} -r -u '*' -xml > ${QSTAT_R_XML_FILE}

# Excetute command
WJOBSTAT_CMD=${UGE_AS_DIR}/sbin/wjobstat.py

${WJOBSTAT_CMD} ${QSTAT_GC_EXT_XML_FILE} ${QSTAT_R_XML_FILE}

rm -f ${QSTAT_GC_EXT_XML_FILE} ${QSTAT_R_XML_FILE}
