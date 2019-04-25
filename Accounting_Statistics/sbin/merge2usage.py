#!/usr/bin/python
# -*- coding: utf-8 -*-
import csv
from collections import defaultdict
from docopt import docopt

__doc__ = """{f}

Usage:
    {f} <used_file1> <used_file2> <merged_used_file>
    {f} -h | --help

Options:
    -h --help          Show this screen and exit.
""".format(f=__file__)

args = docopt(__doc__)
used_file1 = args['<used_file1>']
merged_used_file = "/opt/uge/Accounting_Statistics/logs/accounting/user_used_total_except_current_month.csv"

merged_used_file = args['<merged_used_file>']

user_limit_dict = defaultdict(float)
user_used_total_dict = defaultdict(float)
user_used_batch_dict = defaultdict(float)
user_used_tss_dict = defaultdict(float)

used_f1 = open(used_file1, 'r')
reader = csv.reader(used_f1)
for row in reader:
    user_name = row[0]
    user_limit_dict[user_name] += float(row[1])
    user_used_total_dict[user_name] += float(row[2])
    user_used_batch_dict[user_name] += float(row[3])
    user_used_tss_dict[user_name] += float(row[4])
used_f1.close()

used_file2 = args['<used_file2>']
used_f2 = open(used_file2, 'r')
reader = csv.reader(used_f2)
for row in reader:
    user_name = row[0]
    user_used_total_dict[user_name] += float(row[2])
    user_used_batch_dict[user_name] += float(row[3])
    user_used_tss_dict[user_name] += float(row[4])
used_f2.close()

user_used_list = []
for key in user_limit_dict:
    user_ratio = 0.0
    if user_limit_dict.get(key, 0) != 0.0:
        user_ratio = (user_used_total_dict.get(key, 0) / user_limit_dict.get(key, 0)) * 100
    user_used_list.append([key,
                           user_limit_dict.get(key, 0),
                           user_used_total_dict.get(key, 0),
                           user_used_batch_dict.get(key, 0),
                           user_used_tss_dict.get(key, 0),
                           '{0:.2f}'.format(user_ratio)])

merged_used_f = open(merged_used_file, 'w')
writer = csv.writer(merged_used_f, lineterminator='\n')
writer.writerows(user_used_list)
merged_used_f.close()
