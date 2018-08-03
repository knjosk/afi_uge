#!/usr/bin/python
# -*- coding: utf-8 -*-
__doc__ = """{f}

Usage:
    {f} [<user_name>] [<user_used_py.csv>]
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

user_usage_dict = {}
user_limit_dict = {}

user_usage_list = []
user_limit_list = []


def parse():
    args = docopt(__doc__)
    if args['<user_name>']:
        user_name = args['<user_name>']
        account = subprocess.check_output('whoami')
        if account.strip() != "root":
            print user_name + " is not root"
            sys.exit()
    else:
        res = subprocess.check_output(["whoami"])
        user_name = res.strip()
        if user_name == "root":
            print "user_name needed"
            sys.exit()

    user_usage_file_name = "/opt/uge/Accounting_Statistics/logs/accounting/user_used_py.csv"
    if args['<user_used_py.csv>']:
        user_usage_file_name = args['<user_used_py.csv>']

    user_usage_f = open(user_usage_file_name, 'r')
    reader = csv.reader(user_usage_f)
    header = next(reader)
    for row in reader:
        user_usage_dict[row[0]] = [float(row[1]), float(row[2]), float(row[3]), float(row[4])]

    if user_name + "-s" in user_usage_dict:

        print("--------------TOTAL CPU HOURS--------------------------------------")
        print('[User Name      :{:>15}]'.format(user_name))
        print("--------------TOTAL CPU HOURS--------------------------------------")
        print('[User Name -s   :{:>15}]'.format(user_name + "-s"))
        print('[Total          :{0[1]:8.2f}(hours)] [Annual limit : {0[0]:8.2f}(hours)]'.format(user_usage_dict[user_name + "-s"]))
        print('   [Batch       :{0[2]:8.2f}(hours)]'.format(user_usage_dict[user_name + "-s"]))
        print('   [Interactive :{0[3]:8.2f}(hours)]'.format(user_usage_dict[user_name + "-s"]))
        print("--------------TOTAL CPU HOURS--------------------------------------")
        print('[User Name -d   :{:>15}]'.format(user_name + "-d"))
        print('[Total          :{0[1]:8.2f}(hours)] [Annual limit : {0[0]:8.2f}(hours)]'.format(user_usage_dict[user_name + "-d"]))
        print('   [Batch       :{0[2]:8.2f}(hours)]'.format(user_usage_dict[user_name + "-d"]))
        print('   [Interactive :{0[3]:8.2f}(hours)]'.format(user_usage_dict[user_name + "-d"]))
        print("-------------------------------------------------------------------")

if __name__ == '__main__':
    parse()
