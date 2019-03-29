#!/usr/bin/python
# -*- coding: utf-8 -*-
__doc__ = """{f}

Usage:
    {f} [<month>] [<group_name>]
    {f} -h | --help

Options:
    [<month>]       Month of group accounting data(not requierd),
                    when ommited until today form beginig on the fiscal year.
    [<group_name>]  Group Name(not required),
                    when ommited current group is specified.
                    Only administrators can specify any group name.
                    If group name is specified, Month must be specified.
    -h --help       Show this screen and exit.
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

group_usage_dict = {}
group_limit_dict = {}

group_usage_list = []
group_limit_list = []


def parse():
    args = docopt(__doc__)
    if args['<group_name>']:
        group_name = args['<group_name>']
        account = subprocess.check_output('whoami')
        account_name = account.strip()
        group = subprocess.check_output('groups')
        if group_name != account_name:
            if "fjse" not in group.strip():
                if "ZZa" not in group.strip():
                    if "ZZg" not in group.strip():
                        if account.strip() != "root":
                            print(__doc__)
                            sys.exit()

    else:
        res = subprocess.check_output(["id -ng"], stderr=subprocess.STDOUT, shell=True)
        group_name = res.strip()

    start_day = (FiscalYear(FiscalDateTime.today().fiscal_year).start).strftime('%Y/%m/%d')
    today = date.today().strftime('%Y/%m/%d')
    term = start_day + ' - ' + today

    now = datetime.datetime.now()
    month = now.strftime('%Y%m')

    group_usage_file_name_base = "/opt/uge/Accounting_Statistics/logs/accounting/group_used_"
    group_usage_file_name = group_usage_file_name_base + 'py.csv'

    if args['<month>']:
        group_usage_file_name = group_usage_file_name_base + 'pm.' + args['<month>']
        month = args['<month>']
        if unicode(month).isnumeric() != True:
            print(__doc__)
            sys.exit()
        term = "Month: " + month[:4] + "/" + month[-2:]

    if os.path.exists(group_usage_file_name) != True:
        print "Accounting file not exist."
        sys.exit()

    group_usage_f = open(group_usage_file_name, 'r')
    reader = csv.reader(group_usage_f)
    # header = next(reader)
    for row in reader:
        group_usage_dict[row[0]] = [float(row[1]), float(row[2]), float(row[3]), float(row[4])]

    if group_name + "-s" in group_usage_dict:

        print term
        print("--------------TOTAL NODE HOURS--------------------------------------")
        print('[Group Name     :{:>15}]'.format(group_name))
        print("--------------TOTAL NODE HOURS--------------------------------------")
        print('[Group Name -s  :{:>15}]'.format(group_name + "-s"))
        print('[Total          :{0[1]:8.2f}(hours)]'.format(group_usage_dict[group_name + "-s"]))
        print('   [Batch       :{0[2]:8.2f}(hours)]'.format(group_usage_dict[group_name + "-s"]))
        print('   [Interactive :{0[3]:8.2f}(hours)]'.format(group_usage_dict[group_name + "-s"]))
        print("--------------TOTAL NODE HOURS--------------------------------------")
        print('[UseName -d   :{:>15}]'.format(group_name + "-d"))
        print('[Total          :{0[1]:8.2f}(hours)]'.format(group_usage_dict[group_name + "-d"]))
        print('   [Batch       :{0[2]:8.2f}(hours)]'.format(group_usage_dict[group_name + "-d"]))
        print('   [Interactive :{0[3]:8.2f}(hours)]'.format(group_usage_dict[group_name + "-d"]))
        print("--------------------------------------------------------------------")
    else:
        print term
        print('[User Name      :{:>15}]'.format(group_name))
        print("No data")

if __name__ == '__main__':
    parse()
