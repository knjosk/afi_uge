#!/usr/bin/bash

export LANG=C
export LC_ALL=C

LAST_M=`date --date='1 month ago'  +%m`
MONTH=`date +%m`
YEAR=`date +%Y`

UGE_AS_DIR="/opt/uge/Accounting_Statistics"
LOGS_DIR="/opt/uge/Accounting_Statistics/logs/accounting"
GROUP_USED_CURRENR_MONTH_FILE=${LOGS_DIR}/group_used_pm.csv
GROUP_USED_PER_YEAR_FILE=${LOGS_DIR}/group_used_py.csv
GROUP_USED_TOTAL_EXCEPT_CURRENT_MONTH=${LOGS_DIR}/group_used_total_except_current_month.csv

if [ ${YEAR} = "2018" ]; then
    if [ ${MONTH} = "08" ]; then
        cp ${GROUP_USED_CURRENR_MONTH_FILE} ${GROUP_USED_PER_YEAR_FILE}
    elif [ ${MONTH} = "09" ]; then
        cp ${LOGS_DIR}/group_used_pm.201808 ${GROUP_USED_TOTAL_EXCEPT_CURRENT_MONTH}
        ${UGE_AS_DIR}/sbin/merge2usage.py ${GROUP_USED_TOTAL_EXCEPT_CURRENT_MONTH} ${GROUP_USED_CURRENR_MONTH_FILE} ${GROUP_USED_PER_YEAR_FILE}
    else
        NUM=`seq -f %02g 9 $LAST_M`
        cp ${LOGS_DIR}/group_used_pm.201808 ${GROUP_USED_TOTAL_EXCEPT_CURRENT_MONTH}
        for i in $NUM;
        do
            ${UGE_AS_DIR}/sbin/merge2usage.py ${LOGS_DIR}/group_used_pm.${YEAR}$i ${GROUP_USED_TOTAL_EXCEPT_CURRENT_MONTH} ${GROUP_USED_TOTAL_EXCEPT_CURRENT_MONTH}
        done
        ${UGE_AS_DIR}/sbin/merge2usage.py ${GROUP_USED_TOTAL_EXCEPT_CURRENT_MONTH} ${GROUP_USED_CURRENR_MONTH_FILE} ${GROUP_USED_PER_YEAR_FILE}
    fi
else
    if [ ${MONTH} -ge 4 ]; then
        if [ ${MONTH} -eq 4 ]; then
            cp ${GROUP_USED_CURRENR_MONTH_FILE} ${GROUP_USED_PER_YEAR_FILE}
        elif [ ${MONTH} -eq 5 ]; then
            cp ${LOGS_DIR}/group_used_pm.${YEAR}04 ${GROUP_USED_TOTAL_EXCEPT_CURRENT_MONTH}
            ${UGE_AS_DIR}/sbin/merge2usage.py ${GROUP_USED_TOTAL_EXCEPT_CURRENT_MONTH} ${GROUP_USED_CURRENR_MONTH_FILE} ${GROUP_USED_PER_YEAR_FILE}
        else
            NUM=`seq -f %02g 5 $LAST_M`
            cp ${LOGS_DIR}/group_used_pm.${YEAR}04 ${GROUP_USED_TOTAL_EXCEPT_CURRENT_MONTH}
            for i in $NUM;
            do
                ${UGE_AS_DIR}/sbin/merge2usage.py ${LOGS_DIR}/group_used_pm.${YEAR}$i ${GROUP_USED_TOTAL_EXCEPT_CURRENT_MONTH} ${GROUP_USED_TOTAL_EXCEPT_CURRENT_MONTH}
            done
            ${UGE_AS_DIR}/sbin/merge2usage.py ${GROUP_USED_TOTAL_EXCEPT_CURRENT_MONTH} ${GROUP_USED_CURRENR_MONTH_FILE} ${GROUP_USED_PER_YEAR_FILE}
        fi
    else
        if [ ${YEAR} = "2019" ]; then
            if [ ${MONTH} -eq 1 ]; then
                LAST_Y=`expr ${YEAR} - 1`
                NUM=`seq -f %02g 9 12`
                cp ${LOGS_DIR}/group_used_pm.${LAST_Y}08 ${GROUP_USED_TOTAL_EXCEPT_CURRENT_MONTH}
                for i in $NUM;
                do
                    ${UGE_AS_DIR}/sbin/merge2usage.py ${LOGS_DIR}/group_used_pm.${LAST_Y}$i ${GROUP_USED_TOTAL_EXCEPT_CURRENT_MONTH} ${GROUP_USED_TOTAL_EXCEPT_CURRENT_MONTH}
                done
                ${UGE_AS_DIR}/sbin/merge2usage.py ${GROUP_USED_TOTAL_EXCEPT_CURRENT_MONTH} ${GROUP_USED_CURRENR_MONTH_FILE} ${GROUP_USED_PER_YEAR_FILE}
            else
                LAST_Y=`expr ${YEAR} - 1`
                NUM=`seq -f %02g 9 12`
                cp ${LOGS_DIR}/group_used_pm.${LAST_Y}08 ${GROUP_USED_TOTAL_EXCEPT_CURRENT_MONTH}
                for i in $NUM;
                do
                    ${UGE_AS_DIR}/sbin/merge2usage.py ${LOGS_DIR}/group_used_pm.${LAST_Y}$i ${GROUP_USED_TOTAL_EXCEPT_CURRENT_MONTH} ${GROUP_USED_TOTAL_EXCEPT_CURRENT_MONTH}
                done
                        
                NUM=`seq -f %02g 1 $LAST_M`
                for i in $NUM;
                do
                    ${UGE_AS_DIR}/sbin/merge2usage.py ${LOGS_DIR}/group_used_pm.${YEAR}$i ${GROUP_USED_TOTAL_EXCEPT_CURRENT_MONTH} ${GROUP_USED_TOTAL_EXCEPT_CURRENT_MONTH}
                done
                ${UGE_AS_DIR}/sbin/merge2usage.py ${GROUP_USED_TOTAL_EXCEPT_CURRENT_MONTH} ${GROUP_USED_CURRENR_MONTH_FILE} ${GROUP_USED_PER_YEAR_FILE}
            fi
        else
            if [ ${MONTH} -eq 1 ]; then
                LAST_Y=`expr ${YEAR} - 1`
                NUM=`seq -f %02g 5 12`
                cp ${LOGS_DIR}/group_used_pm.${LAST_Y}04 ${GROUP_USED_TOTAL_EXCEPT_CURRENT_MONTH}
                for i in $NUM;
                do
                    ${UGE_AS_DIR}/sbin/merge2usage.py ${LOGS_DIR}/group_used_pm.${LAST_Y}$i ${GROUP_USED_TOTAL_EXCEPT_CURRENT_MONTH} ${GROUP_USED_TOTAL_EXCEPT_CURRENT_MONTH}
                done
                ${UGE_AS_DIR}/sbin/merge2usage.py ${GROUP_USED_TOTAL_EXCEPT_CURRENT_MONTH} ${GROUP_USED_CURRENR_MONTH_FILE} ${GROUP_USED_PER_YEAR_FILE}
            else
                LAST_Y=`expr ${YEAR} - 1`
                NUM=`seq -f %02g 5 12`
                cp ${LOGS_DIR}/group_used_pm.${LAST_Y}04 ${GROUP_USED_TOTAL_EXCEPT_CURRENT_MONTH}
                for i in $NUM;
                do
                    ${UGE_AS_DIR}/sbin/merge2usage.py ${LOGS_DIR}/group_used_pm.${LAST_Y}$i ${GROUP_USED_TOTAL_EXCEPT_CURRENT_MONTH} ${GROUP_USED_TOTAL_EXCEPT_CURRENT_MONTH}
                done
                        
                NUM=`seq -f %02g 1 $LAST_M`
                for i in $NUM;
                do
                    ${UGE_AS_DIR}/sbin/merge2usage.py ${LOGS_DIR}/group_used_pm.${YEAR}$i ${GROUP_USED_TOTAL_EXCEPT_CURRENT_MONTH} ${GROUP_USED_TOTAL_EXCEPT_CURRENT_MONTH}
                done
                ${UGE_AS_DIR}/sbin/merge2usage.py ${GROUP_USED_TOTAL_EXCEPT_CURRENT_MONTH} ${GROUP_USED_CURRENR_MONTH_FILE} ${GROUP_USED_PER_YEAR_FILE}
            fi
        fi
    fi
fi
