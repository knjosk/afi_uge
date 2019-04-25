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

'''
this_year_month = date.today().strftime('%Y%m')

if int(this_year_month) < 201904:
    fiscalyear.START_YEAR = 'same'
    fiscalyear.START_MONTH = 8
    fiscalyear.START_DAY = 6
else:
    fiscalyear.START_YEAR = 'same'
    fiscalyear.START_MONTH = 4
'''

user_usage_dict = {}
user_limit_dict = {}
prj_usage_dict = {}
prj_limit_dict = {}

user_usage_list = []
user_limit_list = []
prj_usage_list = []
prj_limit_list = []

admin_group_list = ["fjse", "ZZa", "ZZg"]
# admin_group_list = ["ZZa", "ZZg"]

fy_tuple = ("04", "05", "06", "07", "08", "09", "10", "11", "12", "01", "02", "03")


def parse():
    D_USER = D_PRJ = D_MONTH = D_FY = D_ADMIN_G = D_SAME_USER = D_SAME_PRJ = False
    args = docopt(__doc__)
    if args['<arg1>']:
        arg1 = args['<arg1>']
        if arg1.isdigit():
            if len(arg1) == 6:
                D_MONTH = True
                target_month = arg1
            elif len(arg1) == 4:
                D_FY = True
                target_year = arg1
            else:
                print "Error: arg1 must be YYYYMM / YYYY"
                sys.exit()

            if args['<arg2>']:
                arg2 = args['<arg2>']
                if arg2.isdigit():
                    print "Error: wrong arg1 arg2 combination"
                    sys.exit()

                else:
                    if len(arg2) == 9:
                        print "Project Code"
                        D_PRJ = True
                        prj_code = arg2
                        exec_user_name = subprocess.check_output('whoami').strip()
                        exec_groups = subprocess.check_output('groups').strip()
                        if prj_code in exec_groups:
                            D_SAME_PRJ = True
                        exec_groups_list = list(map(str, exec_groups.split(' ')))
                        if len(list(set(exec_groups_list) & set(admin_group_list))) != 0:
                            D_ADMIN_G = True

                        print admin_group_list
                        print list(set(exec_groups_list) & set(admin_group_list))
                        print len(list(set(exec_groups_list) & set(admin_group_list)))
                        print exec_user_name, exec_groups_list

                    elif len(arg2) == 6:
                        print "User name"
                        D_USER = True
                        user_name = arg2
                        exec_user_name = subprocess.check_output('whoami').strip()
                        if user_name == exec_user_name:
                            D_SAME_USER = True
                            print "same user"

                        exec_groups = subprocess.check_output('groups').strip()
                        exec_groups_list = list(map(str, exec_groups.split(' ')))
                        if len(list(set(exec_groups_list) & set(admin_group_list))) != 0:
                            D_ADMIN_G = True
                    else:
                        print "Error: arg2 must be Project Code / User name"
                        sys.exit()

        else:
            if len(arg1) == 9:
                print "Project Code"
                D_PRJ = True
                prj_code = arg1
                exec_user_name = subprocess.check_output('whoami').strip()
                exec_groups = subprocess.check_output('groups').strip()
                if prj_code in exec_groups:
                    D_SAME_PRJ = True
                exec_groups_list = list(map(str, exec_groups.split(' ')))
                if len(list(set(exec_groups_list) & set(admin_group_list))) != 0:
                    D_ADMIN_G = True

                print admin_group_list
                print list(set(exec_groups_list) & set(admin_group_list))
                print len(list(set(exec_groups_list) & set(admin_group_list)))
                print exec_user_name, exec_groups_list

            elif len(arg1) == 6:
                print "User name"
                D_USER = True
                user_name = arg1
                exec_user_name = subprocess.check_output('whoami').strip()
                if user_name == exec_user_name:
                    D_SAME_USER = True
                    print "same user"

                exec_groups = subprocess.check_output('groups').strip()
                exec_groups_list = list(map(str, exec_groups.split(' ')))
                if len(list(set(exec_groups_list) & set(admin_group_list))) != 0:
                    D_ADMIN_G = True
            else:
                print "Error: arg1 must be Project Code / User name"
                sys.exit()

            if args['<arg2>']:
                arg2 = args['<arg2>']
                if arg2.isdigit():
                    if len(arg2) == 6:
                        D_MONTH = True
                        target_month = arg2
                        print "YYYYMM"
                    elif len(arg2) == 4:
                        D_FY = True
                        target_year = arg2
                        print "FY"
                    else:
                        print "Error: arg2 must be YYYYMM / YYYY"
                        sys.exit()

                else:
                    print "Error: wrong arg1 arg2 combination"
                    sys.exit()

    else:
        user_name = subprocess.check_output('whoami').strip()

    if D_USER == False and D_PRJ == False:
        user_name = subprocess.check_output('whoami').strip()

    if ((D_USER == False and D_PRJ == False) or (D_USER == True and D_ADMIN_G == True)) and D_MONTH == False and D_FY == False:
        this_year_month = date.today().strftime('%Y%m')

        if int(this_year_month) < 201904:
            fiscalyear.START_YEAR = 'same'
            fiscalyear.START_MONTH = 8
            fiscalyear.START_DAY = 6
        else:
            fiscalyear.START_YEAR = 'same'
            fiscalyear.START_MONTH = 4

        start_day = (FiscalYear(FiscalDateTime.today().fiscal_year).start).strftime('%Y/%m/%d')
        today = date.today().strftime('%Y/%m/%d')
        term = start_day + ' - ' + today
        user_usage_file_name = "/opt/uge/Accounting_Statistics/logs/accounting/user_used_py.csv"
        if os.path.exists(user_usage_file_name) != True:
            print "User Usage Accounting file not exist."
            sys.exit()
        user_usage_f = open(user_usage_file_name, 'r')
        reader = csv.reader(user_usage_f)
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
            print("-------------------------------------------------------------------")
        else:
            print term
            print('[User Name      :{:>15}]'.format(user_name))
            print("No data")

    elif D_MONTH == True and D_PRJ == False and (D_USER == False or (D_SAME_USER == True or D_ADMIN_G == True)):
        start_day = (FiscalYear(FiscalDateTime.today().fiscal_year).start).strftime('%Y/%m/%d')
        today = date.today().strftime('%Y/%m/%d')
        term = start_day + ' - ' + today

        now = datetime.datetime.now()
        month = now.strftime('%Y%m')

        user_usage_file_name_base = "/opt/uge/Accounting_Statistics/logs/accounting/user_used_"
        user_usage_file_name = user_usage_file_name_base + 'pm.' + target_month
        if target_month == month:
            user_usage_file_name = user_usage_file_name_base + 'pm.' + 'csv'
        if os.path.exists(user_usage_file_name) != True:
            print "User Usage Accounting file not exist."
            sys.exit()
        term = "Month: " + target_month[:4] + "/" + target_month[-2:]
        user_usage_f = open(user_usage_file_name, 'r')
        reader = csv.reader(user_usage_f)
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
            print("-------------------------------------------------------------------")
        else:
            print term
            print('[User Name      :{:>15}]'.format(user_name))
            print("No data")

    elif D_MONTH == False and D_PRJ == True and (D_SAME_PRJ == True or D_ADMIN_G == True):
        start_day = date.today().strftime('%Y/%m') + '/01'
        today = date.today().strftime('%Y/%m/%d')
        term = start_day + ' - ' + today

        now = datetime.datetime.now()
        month = now.strftime('%Y%m')
        prj_usage_file_name_base = "/opt/uge/Accounting_Statistics/logs/accounting/prj_used_pm."
        prj_usage_file_name = prj_usage_file_name_base + 'csv'
        if os.path.exists(prj_usage_file_name) != True:
            print "Accounting file not exist."
            sys.exit()
        prj_usage_f = open(prj_usage_file_name, 'r')
        reader = csv.reader(prj_usage_f)
        for row in reader:
            prj_usage_dict[row[0]] = [float(row[1]), float(row[2]), float(row[3]), float(row[4])]

        if prj_code + "-s" in prj_usage_dict:

            print term
            print("--------------TOTAL NODE HOURS-------------------------------------")
            print('[Project Code   :{:>15}]'.format(prj_code))
            print("--------------TOTAL NODE HOURS-------------------------------------")
            print('[Project -s     :{:>15}]'.format(prj_code + "-s"))
            print('[Total          :{0[1]:8.2f}(hours)] [Monthly limit : {0[0]:8.2f}(hours)]'.format(prj_usage_dict[prj_code + "-s"]))
            print('   [Batch       :{0[2]:8.2f}(hours)]'.format(prj_usage_dict[prj_code + "-s"]))
            print('   [Interactive :{0[3]:8.2f}(hours)]'.format(prj_usage_dict[prj_code + "-s"]))
            print("--------------TOTAL NODE HOURS-------------------------------------")
            print('[Project -d     :{:>15}]'.format(prj_code + "-d"))
            print('[Total          :{0[1]:8.2f}(hours)] [Monthly limit : {0[0]:8.2f}(hours)]'.format(prj_usage_dict[prj_code + "-d"]))
            print('   [Batch       :{0[2]:8.2f}(hours)]'.format(prj_usage_dict[prj_code + "-d"]))
            print('   [Interactive :{0[3]:8.2f}(hours)]'.format(prj_usage_dict[prj_code + "-d"]))
            print("-------------------------------------------------------------------")

        else:
            print term
            print('[Project Code   :{:>15}]'.format(prj_code))
            print("No data")

    elif D_MONTH == True and D_PRJ == True and (D_SAME_PRJ == True or D_ADMIN_G == True):
        start_day = date.today().strftime('%Y/%m') + '/01'
        today = date.today().strftime('%Y/%m/%d')
        term = start_day + ' - ' + today

        now = datetime.datetime.now()
        month = now.strftime('%Y%m')
        prj_usage_file_name_base = "/opt/uge/Accounting_Statistics/logs/accounting/prj_used_pm."
        prj_usage_file_name = prj_usage_file_name_base + target_month
        if target_month == month:
            prj_usage_file_name = prj_usage_file_name_base + 'csv'
        if os.path.exists(prj_usage_file_name) != True:
            print "Accounting file not exist."
            sys.exit()
        term = "Month: " + target_month[:4] + "/" + target_month[-2:]
        prj_usage_f = open(prj_usage_file_name, 'r')
        reader = csv.reader(prj_usage_f)
        for row in reader:
            prj_usage_dict[row[0]] = [float(row[1]), float(row[2]), float(row[3]), float(row[4])]

        if prj_code + "-s" in prj_usage_dict:

            print term
            print("--------------TOTAL NODE HOURS-------------------------------------")
            print('[Project Code   :{:>15}]'.format(prj_code))
            print("--------------TOTAL NODE HOURS-------------------------------------")
            print('[Project -s     :{:>15}]'.format(prj_code + "-s"))
            print('[Total          :{0[1]:8.2f}(hours)] [Monthly limit : {0[0]:8.2f}(hours)]'.format(prj_usage_dict[prj_code + "-s"]))
            print('   [Batch       :{0[2]:8.2f}(hours)]'.format(prj_usage_dict[prj_code + "-s"]))
            print('   [Interactive :{0[3]:8.2f}(hours)]'.format(prj_usage_dict[prj_code + "-s"]))
            print("--------------TOTAL NODE HOURS-------------------------------------")
            print('[Project -d     :{:>15}]'.format(prj_code + "-d"))
            print('[Total          :{0[1]:8.2f}(hours)] [Monthly limit : {0[0]:8.2f}(hours)]'.format(prj_usage_dict[prj_code + "-d"]))
            print('   [Batch       :{0[2]:8.2f}(hours)]'.format(prj_usage_dict[prj_code + "-d"]))
            print('   [Interactive :{0[3]:8.2f}(hours)]'.format(prj_usage_dict[prj_code + "-d"]))
            print("-------------------------------------------------------------------")

        else:
            print term
            print('[Project Code   :{:>15}]'.format(prj_code))
            print("No data")

    elif D_FY == True and (D_USER == False or D_SAME_USER == True or D_ADMIN_G == True):
        if int(target_year) == 2018:
            fiscalyear.START_YEAR = 'same'
            fiscalyear.START_MONTH = 8
            fiscalyear.START_DAY = 6
        else:
            fiscalyear.START_YEAR = 'same'
            fiscalyear.START_MONTH = 4

        print FiscalYear(target_year).start

        print "D_USER, D_PRJ, D_FY, D_MONTH, D_ADMIN_G, D_SAME_USER, user_name"
    print D_USER, D_PRJ, D_FY, D_MONTH, D_ADMIN_G, D_SAME_USER


if __name__ == '__main__':
    parse()
