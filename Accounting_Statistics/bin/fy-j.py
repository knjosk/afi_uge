#! /usr/bin/python

import fiscalyear
from fiscalyear import *
import time
import datetime
from datetime import date
from dateutil.relativedelta import relativedelta


target_year = date.today().strftime('%Y')
target_year = '2018'
print target_year

if target_year == "2018":
    fiscalyear.START_YEAR = 'same'
    fiscalyear.START_MONTH = 8
    fiscalyear.START_DAY = 6
else:
    fiscalyear.START_YEAR = 'same'
    fiscalyear.START_MONTH = 4

fy_start_day = (FiscalYear(FiscalDateTime.today().fiscal_year).start).strftime('%Y/%m/%d')
print fy_start_day

fy_start_month = (FiscalYear(FiscalDateTime.today().fiscal_year).start).strftime('%Y/%m')
print fy_start_month

today = date.today().strftime('%Y/%m/%d')
print today

print datetime.datetime.now().date()

n = 10
print datetime.datetime.now() + relativedelta(months=n)
i = 0
m = (FiscalYear(2018).start + relativedelta(months=i)).month
print m
while m != "03":
    month = (FiscalYear(2018).start + relativedelta(months=i)).strftime('%Y%m')
    m = month[-2:]
    print month
    i = i + 1
