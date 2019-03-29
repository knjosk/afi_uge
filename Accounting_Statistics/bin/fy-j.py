#! /usr/bin/python

import fiscalyear
from fiscalyear import *
import time
import datetime
from datetime import date

this_year = date.today().strftime('%Y')
print this_year

if this_year == "2018":
    fiscalyear.START_YEAR = 'same'
    fiscalyear.START_MONTH = 8
    fiscalyear.START_DAY = 6
else:
    fiscalyear.START_YEAR = 'same'
    fiscalyear.START_MONTH = 4

fy_start_day = (FiscalYear(FiscalDateTime.today().fiscal_year).start).strftime('%Y/%m/%d')
print fy_start_day

today = date.today().strftime('%Y/%m/%d')
print today
