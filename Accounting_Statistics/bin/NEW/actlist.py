#!/usr/bin/python
# -*- coding: utf-8 -*-
__doc__ = """{f}

Usage:
    {f} [<arg1>] [<arg2>]
    {f} -h | --help

Options:
    [<arg1>]   User name or Project Code (not required),
               when ommited current user is specified.
               Only administrators can specify any user name.
    [<arg2>]   Month or Fiscal year (not requierd),
               when ommited until today form beginig on the fiscal year.
    -h --help  Show this screen and exit.
""".format(f=__file__)

from docopt import docopt
import datetime
import time
import json
import xmltodict
import sys
import os
import subprocess
from operator import itemgetter
from collections import defaultdict
import csv

import fiscalyear
from fiscalyear import *
from datetime import date

this_year_month = date.today().strftime('%Y%m')

if int(this_year_month) < 201904:
    fiscalyear.START_YEAR = 'same'
    fiscalyear.START_MONTH = 8
    fiscalyear.START_DAY = 6
else:
    fiscalyear.START_YEAR = 'same'
    fiscalyear.START_MONTH = 4

user_usage_dict = {}
user_limit_dict = {}
prj_usage_dict = {}
prj_limit_dict = {}

user_usage_list = []
user_limit_list = []
prj_usage_list = []
prj_limit_list = []


def parse():
    args = docopt(__doc__)
    if args['<arg1>']:
        arg1 = args['<arg1>']
        if arg1.isdigit():

        account = subprocess.check_output('whoami')
        account_name = account.strip()
        group = subprocess.check_output('groups')
        if arg1 != account_name:
            if "fjse" not in group.strip():
                if "ZZa" not in group.strip():
                    if "ZZg" not in group.strip():
                        if account.strip() != "root":
                            print(__doc__)
                            sys.exit()

    else:
        res = subprocess.check_output(["whoami"])
        arg1 = res.strip()
        if arg1 == "root":
            print "arg1 needed"
            sys.exit()

    start_day = (FiscalYear(FiscalDateTime.today().fiscal_year).start).strftime('%Y/%m/%d')
    today = date.today().strftime('%Y/%m/%d')
    term = start_day + ' - ' + today

    now = datetime.datetime.now()
    arg2 = now.strftime('%Y%m')

    user_usage_file_name_base = "/opt/uge/Accounting_Statistics/logs/accounting/user_used_"
    user_usage_file_name = user_usage_file_name_base + 'py.csv'

    if args['<arg2>']:
        user_usage_file_name = user_usage_file_name_base + 'pm.' + args['<arg2>']
        arg2 = args['<arg2>']
        if unicode(arg2).isnumeric() != True:
            print(__doc__)
            sys.exit()
        term = "Arg2: " + arg2[:4] + "/" + arg2[-2:]

    if os.path.exists(user_usage_file_name) != True:
        print "Accounting file not exist."
        sys.exit()

    user_usage_f = open(user_usage_file_name, 'r')
    reader = csv.reader(user_usage_f)
    # header = next(reader)
    for row in reader:
        user_usage_dict[row[0]] = [float(row[1]), float(row[2]), float(row[3]), float(row[4])]

    if arg1 + "-s" in user_usage_dict:

        print term
        print("--------------TOTAL NODE HOURS--------------------------------------")
        print('[User Name      :{:>15}]'.format(arg1))
        print("--------------TOTAL NODE HOURS--------------------------------------")
        print('[User Name -s   :{:>15}]'.format(arg1 + "-s"))
        print('[Total          :{0[1]:8.2f}(hours)] [Annual limit : {0[0]:8.2f}(hours)]'.format(user_usage_dict[arg1 + "-s"]))
        print('   [Batch       :{0[2]:8.2f}(hours)]'.format(user_usage_dict[arg1 + "-s"]))
        print('   [Interactive :{0[3]:8.2f}(hours)]'.format(user_usage_dict[arg1 + "-s"]))
        print("--------------TOTAL NODE HOURS--------------------------------------")
        print('[User Name -d   :{:>15}]'.format(arg1 + "-d"))
        print('[Total          :{0[1]:8.2f}(hours)] [Annual limit : {0[0]:8.2f}(hours)]'.format(user_usage_dict[arg1 + "-d"]))
        print('   [Batch       :{0[2]:8.2f}(hours)]'.format(user_usage_dict[arg1 + "-d"]))
        print('   [Interactive :{0[3]:8.2f}(hours)]'.format(user_usage_dict[arg1 + "-d"]))
        print("--------------------------------------------------------------------")
    else:
        print term
        print('[User Name      :{:>15}]'.format(arg1))
        print("No data")

if __name__ == '__main__':
    parse()
