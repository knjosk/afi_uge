#!/usr/bin/python
# -*- coding: utf-8 -*-
__doc__ = """{f}

Usage:
    {f} [<user_name>] [<month>]
    {f} -h | --help

Options:
    -h --help                Show this screen and exit.
""".format(f=__file__)

from docopt import docopt
import datetime
import time
import json
import xmltodict
import sys
import subprocess
from operator import itemgetter
from collections import defaultdict
import csv

import fiscalyear
from fiscalyear import *
from datetime import date

this_year = date.today().strftime('%Y')

if this_year == "2018":
    fiscalyear.START_YEAR = 'same'
    fiscalyear.START_MONTH = 8
    fiscalyear.START_DAY = 6
else:
    fiscalyear.START_YEAR = 'same'
    fiscalyear.START_MONTH = 4

user_usage_dict = {}
user_limit_dict = {}

user_usage_list = []
user_limit_list = []


def parse():
    args = docopt(__doc__)
    if args['<user_name>']:
        user_name = args['<user_name>']
        account = subprocess.check_output('whoami')
        account_name = account.strip()
        group = subprocess.check_output('groups')
        if user_name != account_name:
            if "fjse" not in group.strip():
                if "ZZa" not in group.strip():
                    if "ZZg" not in group.strip():
                        if account.strip() != "root":
                            print user_name + " is not root"
                            sys.exit()

    else:
        res = subprocess.check_output(["whoami"])
        user_name = res.strip()
        if user_name == "root":
            print "user_name needed"
            sys.exit()

    start_day = (FiscalYear(FiscalDateTime.today().fiscal_year).start).strftime('%Y/%m/%d')
    today = date.today().strftime('%Y/%m/%d')
    term = start_day + ' - ' + today

    now = datetime.datetime.now()
    month = now.strftime('%Y%m')

    user_usage_file_name_base = "/opt/uge/Accounting_Statistics/logs/accounting/user_used_"
    user_usage_file_name = user_usage_file_name_base + 'py.csv'

    if args['<month>']:
        user_usage_file_name = user_usage_file_name_base + 'pm.' + args['<month>']

    user_usage_f = open(user_usage_file_name, 'r')
    reader = csv.reader(user_usage_f)
    header = next(reader)
    for row in reader:
        user_usage_dict[row[0]] = [float(row[1]), float(row[2]), float(row[3]), float(row[4])]

    if user_name + "-s" in user_usage_dict:

        print term
        print("--------------TOTAL NODE HOURS--------------------------------------")
        print('[User Name      :{:>15}]'.format(user_name))
        print("--------------TOTAL NODE HOURS--------------------------------------")
        print('[User Name -s   :{:>15}]'.format(user_name + "-s"))
        print('[Total          :{0[1]:8.2f}(hours)] [Annual limit : {0[0]:8.2f}(hours)]'.format(user_usage_dict[user_name + "-s"]))
        print('   [Batch       :{0[2]:8.2f}(hours)]'.format(user_usage_dict[user_name + "-s"]))
        print('   [Interactive :{0[3]:8.2f}(hours)]'.format(user_usage_dict[user_name + "-s"]))
        print("--------------TOTAL NODE HOURS--------------------------------------")
        print('[User Name -d   :{:>15}]'.format(user_name + "-d"))
        print('[Total          :{0[1]:8.2f}(hours)] [Annual limit : {0[0]:8.2f}(hours)]'.format(user_usage_dict[user_name + "-d"]))
        print('   [Batch       :{0[2]:8.2f}(hours)]'.format(user_usage_dict[user_name + "-d"]))
        print('   [Interactive :{0[3]:8.2f}(hours)]'.format(user_usage_dict[user_name + "-d"]))
        print("--------------------------------------------------------------------")

if __name__ == '__main__':
    parse()
