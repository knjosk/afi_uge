#!/usr/bin/bash

export LANG=C
export LC_ALL=C

timestamp=`date`
f_timestamp=`date +%Y%m%d%H%M%S`

UGE_AS_DIR=/opt/uge/Accounting_Statistics
QSTAT_CMD=/opt/uge/8.5.5/bin/lx-amd64/qstat

WJOBSTAT_DIR=/opt/uge/Accounting_Statistics/logs/wjobstat
QSTAT_GC_EXT_XML=qstat_gc_ext_xml.txt
QSTAT_R_XML=qstat_r_xml.txt

# Input files
QSTAT_GC_EXT_XML_FILE=${WJOBSTAT_DIR}/${QSTAT_GC_EXT_XML}-${f_timestamp}
QSTAT_R_XML_FILE=${WJOBSTAT_DIR}/${QSTAT_R_XML}-${f_timestamp}

${QSTAT_CMD} -g c -ext -xml > ${QSTAT_GC_EXT_XML_FILE}
${QSTAT_CMD} -r -u '*' -xml > ${QSTAT_R_XML_FILE}

# Excetute command
WJOBSTAT_CMD=${UGE_AS_DIR}/sbin/wjobstat.py

# Output files
WJOBSTAT=wjobstat.txt
WJOB_FILE=${WJOBSTAT_DIR}/${WJOBSTAT}-${f_timestamp}
WJOBSTAT_FILE=${WJOBSTAT_DIR}/${WJOBSTAT}

echo "[ ${timestamp} (update every 15 minits)]" > ${WJOB_FILE}
echo "Job-ID     prior   name       user       jclass               estimate start time" >> ${WJOB_FILE}
echo "---------- ------- ---------- ---------- -------------------- -------------------" >> ${WJOB_FILE}

${WJOBSTAT_CMD} ${QSTAT_GC_EXT_XML_FILE} ${QSTAT_R_XML_FILE}\
    | sort -k 6\
    | awk '{printf "%-10d %-.5f %-10.10s %-10.10s %-20.20s %10s %8s\n",$1,$2,$3,$4,$5,$6,$7}'\
    >> ${WJOB_FILE}

rm -f ${WJOBSTAT_FILE}
ln -s ${WJOB_FILE} ${WJOBSTAT_FILE}
