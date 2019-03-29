#!/usr/bin/python
# -*- coding: utf-8 -*-
__doc__ = """{f}

Usage:
    {f} <prj_name> [<month>]
    {f} -h | --help

Options:
    <prj_name>   Project Name must be specified.
    [<month>]    Month of project accounting data.(not requierd)
                 When ommited current month is specified.
    -h --help    Show this screen and exit.
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

from datetime import date

prj_usage_dict = {}
prj_limit_dict = {}

prj_usage_list = []
prj_limit_list = []


def parse():
    args = docopt(__doc__)
    if args['<prj_name>']:
        prj_name = args['<prj_name>']
        account = subprocess.check_output('whoami')
        group = subprocess.check_output('groups')
        if "fjse" not in group.strip():
            if "ZZa" not in group.strip():
                if "ZZg" not in group.strip():
                    if account.strip() != "root":
                        if prj_name not in group.strip():
                            print prj_name + " is not your group"
                            sys.exit()

    start_day = date.today().strftime('%Y/%m') + '/01'
    today = date.today().strftime('%Y/%m/%d')
    term = start_day + ' - ' + today

    now = datetime.datetime.now()
    month = now.strftime('%Y%m')
    prj_usage_file_name_base = "/opt/uge/Accounting_Statistics/logs/accounting/prj_used_pm."
    prj_usage_file_name = prj_usage_file_name_base + 'csv'
    if args['<month>']:
        prj_usage_file_name = prj_usage_file_name_base + args['<month>']
        month = args['<month>']
        term = "Month: " + month[:4] + "/" + month[-2:]

    if os.path.exists(prj_usage_file_name) != True:
        print "Accounting file not exist."
        sys.exit()

    prj_usage_f = open(prj_usage_file_name, 'r')
    reader = csv.reader(prj_usage_f)
    # header = next(reader)
    for row in reader:
        prj_usage_dict[row[0]] = [float(row[1]), float(row[2]), float(row[3]), float(row[4])]

    if prj_name + "-s" in prj_usage_dict:

        print term
        print("--------------TOTAL NODE HOURS-------------------------------------")
        print('[Project Code   :{:>15}]'.format(prj_name))
        print("--------------TOTAL NODE HOURS-------------------------------------")
        print('[Project -s     :{:>15}]'.format(prj_name + "-s"))
        print('[Total          :{0[1]:8.2f}(hours)] [Monthly limit : {0[0]:8.2f}(hours)]'.format(prj_usage_dict[prj_name + "-s"]))
        print('   [Batch       :{0[2]:8.2f}(hours)]'.format(prj_usage_dict[prj_name + "-s"]))
        print('   [Interactive :{0[3]:8.2f}(hours)]'.format(prj_usage_dict[prj_name + "-s"]))
        print("--------------TOTAL NODE HOURS-------------------------------------")
        print('[Project -d     :{:>15}]'.format(prj_name + "-d"))
        print('[Total          :{0[1]:8.2f}(hours)] [Monthly limit : {0[0]:8.2f}(hours)]'.format(prj_usage_dict[prj_name + "-d"]))
        print('   [Batch       :{0[2]:8.2f}(hours)]'.format(prj_usage_dict[prj_name + "-d"]))
        print('   [Interactive :{0[3]:8.2f}(hours)]'.format(prj_usage_dict[prj_name + "-d"]))
        print("-------------------------------------------------------------------")

    else:
        print term
        print('[Project Code   :{:>15}]'.format(prj_name))
        print("No data")
if __name__ == '__main__':
    parse()
