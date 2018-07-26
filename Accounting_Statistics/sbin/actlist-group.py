#!/usr/bin/python
# -*- coding: utf-8 -*-
__doc__ = """{f}

Usage:
    {f} [<group_name>] [<group_used_pm.csv>]
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

group_usage_dict = {}
group_limit_dict = {}

group_usage_list = []
group_limit_list = []


def parse():
    args = docopt(__doc__)
    if args['<group_name>']:
        group_name = args['<group_name>']
        account = subprocess.check_output('whoami')
        if account.strip() != "root":
            res = subprocess.check_output('groups')
            if group_name not in res.strip():
                print group_name + " is not your group"
                sys.exit()
    else:
        res = subprocess.check_output(["id", "-g", "-n"])
        group_name = res.strip()

    group_usage_file_name = "/opt/uge/Accounting_Statistics/etc/group_used_pm.csv"
    if args['<group_used_pm.csv>']:
        group_usage_file_name = args['<group_used_pm.csv>']

    group_usage_f = open(group_usage_file_name, 'r')
    reader = csv.reader(group_usage_f)
    header = next(reader)
    for row in reader:
        group_usage_dict[row[0]] = [float(row[1]), float(row[2]), float(row[3]), float(row[4])]

    print("--------------TOTAL CPU HOURS--------------------------------------")
    print('[Account Code   :{:>15}]'.format(group_name))
    print('[Total          :{0[1]:8.2f}(hours)] [Annual limit : {0[0]:8.2f}(hours)]'.format(group_usage_dict[group_name]))
    print('   [Batch       :{0[2]:8.2f}(hours)]'.format(group_usage_dict[group_name]))
    print('   [Interactive :{0[3]:8.2f}(hours)]'.format(group_usage_dict[group_name]))
    print("-------------------------------------------------------------------")

if __name__ == '__main__':
    parse()
