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
prj_member_usage_dict = {}
prj_limit_dict = {}

prj_usage_list = []
prj_member_usage_list = []
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
                        print "you are not administrator"
                        sys.exit()
    start_day = date.today().strftime('%Y/%m') + '/01'
    today = date.today().strftime('%Y/%m/%d')
    term = start_day + ' - ' + today

    now = datetime.datetime.now()
    month = now.strftime('%Y%m')
    prj_usage_file_name_base = "/opt/uge/Accounting_Statistics/logs/accounting/prj_used_pm."
    prj_usage_file_name = prj_usage_file_name_base + 'csv'
    prj_member_usage_file_name_base = "/opt/uge/Accounting_Statistics/logs/accounting/prj_member_used_pm."
    prj_member_usage_file_name = prj_member_usage_file_name_base + 'csv'
    # Project Member's list
    prj_member_file_name_base = "/opt/uge/Accounting_Statistics/etc/prj_member."
    prj_member_file_name = prj_member_file_name_base + 'csv'
    if args['<month>']:
        prj_usage_file_name = prj_usage_file_name_base + args['<month>']
        prj_member_usage_file_name = prj_member_usage_file_name_base + args['<month>']
        prj_member_file_name = prj_member_file_name_base + args['<month>']
        month = args['<month>']
        term = "Month: " + month[:4] + "/" + month[-2:]

    if os.path.exists(prj_usage_file_name) != True:
        print "Project Usage file not exist."
        sys.exit()

    if os.path.exists(prj_member_usage_file_name) != True:
        print "Project_member Usage file not exist."
        sys.exit()

    if os.path.exists(prj_member_file_name) != True:
        print "Poject Member list file not exist."
        sys.exit()

    prj_usage_f = open(prj_usage_file_name, 'r')
    reader = csv.reader(prj_usage_f)
    for row in reader:
        prj_usage_dict[row[0]] = [float(row[1]), float(row[2]), float(row[3]), float(row[4])]

    prj_member_usage_f = open(prj_member_usage_file_name, 'r')
    reader = csv.reader(prj_member_usage_f)
    for row in reader:
        prj_member_usage_dict[row[0]] = [float(row[1]), float(row[2]), float(row[3])]

    prj_member_list_f = open(prj_member_file_name, 'r')
    reader = csv.reader(prj_member_list_f)

    prj_member_dict = {}
    prj_member_list = []
    target_prj_member_list_s = []
    target_prj_member_list_d = []

    for row in reader:
        i = 1
        row_l = len(row)
        list_of_member = []
        list_of_member_s = []
        list_of_member_d = []
        if row_l > 2:
            while i < row_l:
                list_of_member.append(row[i])
                list_of_member_s.append(row[i] + '_' + row[0] + '-s')
                list_of_member_d.append(row[i] + '_' + row[0] + '-d')
                prj_member_list.append(row[i] + '_' + row[0] + '-s')
                prj_member_list.append(row[i] + '_' + row[0] + '-d')
                i += 1
            prj_member_dict[row[0] + '-s'] = list_of_member_s
            prj_member_dict[row[0] + '-d'] = list_of_member_d
    if prj_name + '-s' in prj_member_dict:
        target_prj_member_list_s = prj_member_dict.get(prj_name + '-s')
    if prj_name + '-d' in prj_member_dict:
        target_prj_member_list_d = prj_member_dict.get(prj_name + '-d')

    if prj_name + "-s" in prj_usage_dict:

        print term
        #print("--------------TOTAL NODE HOURS-------------------------------------")
        print('[Project Code   :{:>15}]'.format(prj_name))
        print("--------------TOTAL NODE HOURS-------------------------------------")
        print('[Project -s     :{:>15}]'.format(prj_name + "-s"))
        print('[Total          :{0[1]:8.2f}(hours)] [Monthly limit : {0[0]:8.2f}(hours)]'.format(prj_usage_dict[prj_name + "-s"]))
        print('   [Batch       :{0[2]:8.2f}(hours)]'.format(prj_usage_dict[prj_name + "-s"]))
        print('   [Interactive :{0[3]:8.2f}(hours)]'.format(prj_usage_dict[prj_name + "-s"]))
        print("-------------------------------------------------------------------")

        if len(target_prj_member_list_s) == 0:
            print('[Project Code   :{:>15}]'.format(prj_name + '-s'))
            print("No Members")

        else:
            for mem in target_prj_member_list_s:
                #print("--------------TOTAL NODE HOURS-------------------------------------")
                print('[Member         :{0[0]:>15}]'.format(mem.split('_')))
                print('[Total          :{0[0]:8.2f}(hours)]'.format(prj_member_usage_dict[mem]))
                print('   [Batch       :{0[1]:8.2f}(hours)]'.format(prj_member_usage_dict[mem]))
                print('   [Interactive :{0[2]:8.2f}(hours)]'.format(prj_member_usage_dict[mem]))

        print("--------------TOTAL NODE HOURS-------------------------------------")
        print('[Project -d     :{:>15}]'.format(prj_name + "-d"))
        print('[Total          :{0[1]:8.2f}(hours)] [Monthly limit : {0[0]:8.2f}(hours)]'.format(prj_usage_dict[prj_name + "-d"]))
        print('   [Batch       :{0[2]:8.2f}(hours)]'.format(prj_usage_dict[prj_name + "-d"]))
        print('   [Interactive :{0[3]:8.2f}(hours)]'.format(prj_usage_dict[prj_name + "-d"]))
        print("-------------------------------------------------------------------")

        if len(target_prj_member_list_d) == 0:
            print('[Project Code   :{:>15}]'.format(prj_name + '-d'))
            print("No Members")
        else:
            for mem in target_prj_member_list_d:
                #print("--------------TOTAL NODE HOURS-------------------------------------")
                print('[Member         :{0[0]:>15}]'.format(mem.split('_')))
                print('[Total          :{0[0]:8.2f}(hours)]'.format(prj_member_usage_dict[mem]))
                print('   [Batch       :{0[1]:8.2f}(hours)]'.format(prj_member_usage_dict[mem]))
                print('   [Interactive :{0[2]:8.2f}(hours)]'.format(prj_member_usage_dict[mem]))

    else:
        print term
        print('[Project Code   :{:>15}]'.format(prj_name))
        print("No data")
if __name__ == '__main__':
    parse()
