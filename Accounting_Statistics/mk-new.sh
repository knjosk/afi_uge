#!/bin/bash
tar cvjf 20190130.tar.gz \
    sbin/mk-accounting-statistics.cron.daily-20190130 \
    sbin/c_statistics.py-20190130 \
    sbin/mk-group-used-py.sh \
    logs/accounting/20190130NEW \
    logs/statistics/20190130NEW
